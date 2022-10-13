# Code for creation of sourcedata object
#   which contains both the (vector) data for the field and the relevant metadata

# -----------------------------------------------------------------------------
#' Constructor for individual datafields within sourcedata object
#'
#' @param x vector of cleaned values for datafield
#' @param field_type field_type object specified for the datafield
#' @param validation_warnings data.table containing any parser/package-specific
#'   warnings
#' @noRd
#' @return A `datafield` object
# TODO: not sure if better to store the entire field_type or just its name or even as a separate list in the sourcedata
datafield <- function(x, field_type, validation_warnings = NULL) {
  structure(
    list(
      values = x,
      field_type = field_type,
      columnname = names(x[1]),
      validation_warnings = validation_warnings
    ),
    class = c(paste0(
      "datafield_", get_field_type_name(field_type)
    ), "datafield")
  )
}

is.datafield <- function(x) inherits(x, "datafield")


# -----------------------------------------------------------------------------
#' Prepare source data
#'
#' Validate a data frame against a [field_types()] specification, and
#' prepare for aggregation.
#'
#' @param df A data frame
#' @param field_types [field_types()] object specifying names and types
#'   of fields (columns) in the supplied `df`. See also
#'   [field_types_available].
#' @param override_column_names If `FALSE`, column names in the supplied `df`
#'   must match the names specified in `field_types` exactly. If `TRUE`, column
#'   names in the supplied `df` will be replaced with the names specified in `field_types`.
#'   The specification must therefore contain the columns in the correct order.
#'   Default = `FALSE`
#' @param na vector containing strings that should be interpreted as missing
#'   values, Default = `c("","NA","NULL")`.
#' @param dataset_shortdesc Short description of the dataset being checked. This
#'   will appear on the report. If blank, the name of the data frame object will
#'   be used
#' @param showprogress Print progress to console. Default = `TRUE`
#' @return A `sourcedata` object
#' @examples
#' # load example data into a data.frame
#' rawdata <- read_data(
#'   system.file("extdata", "example_prescriptions.csv", package = "daiquiri"),
#'   delim = ",",
#'   col_names = TRUE
#' )
#'
#' # validate and prepare the data for aggregation
#' sourcedataobj <- prepare_data(
#'   rawdata,
#'   field_types = field_types(
#'     PrescriptionID = ft_uniqueidentifier(),
#'     PrescriptionDate = ft_timepoint(),
#'     AdmissionDate = ft_datetime(includes_time = FALSE),
#'     Drug = ft_freetext(),
#'     Dose = ft_numeric(),
#'     DoseUnit = ft_categorical(),
#'     PatientID = ft_ignore(),
#'     Location = ft_categorical(aggregate_by_each_category = TRUE)
#'   ),
#'   override_column_names = FALSE,
#'   na = c("", "NULL"),
#'   dataset_shortdesc = "Example data provided with package"
#' )
#' @seealso [field_types()], [field_types_available()],
#'   [aggregate_data()], [report_data()],
#'   [create_report()]
#' @export
#' @importFrom data.table .N .SD
prepare_data <- function(df,
                         field_types,
                         override_column_names = FALSE,
                         na = c("", "NA", "NULL"),
                         dataset_shortdesc = NULL,
                         showprogress = TRUE) {
  log_function_start(match.call()[[1]])

  # initialise known column names to prevent R CMD check notes
  colindex <- rowindex <- fieldname <- NULL

  validate_params_required(match.call())
  validate_params_type(match.call(),
    df = df,
    field_types = field_types,
    override_column_names = override_column_names,
    na = na,
    dataset_shortdesc = dataset_shortdesc,
    showprogress = showprogress
  )

  # use dataset_shortdesc if present, otherwise get from call
  if (is.null(dataset_shortdesc)) {
    # look for create_report() in the call stack and if present, use the latest one
    # can't just use sys.function(-1) as that doesn't work inside testthat
    matchedcalls <- grep("create_report", as.character(sys.calls()))
    if (length(matchedcalls) > 0) {
      dataset_shortdesc <-
        as.character(enquote(
          as.list(
            match.call(
              definition = sys.function(rev(matchedcalls)[1]),
              call = sys.call(sys.parent())
            )
          )$df
        ))[2]
    } else {
      dataset_shortdesc <- as.character(enquote(as.list(match.call())$df))[2]
    }
  }

  log_message(
    paste0("field_types supplied:\n", field_types_to_string(field_types)),
    showprogress
  )

  # validate inputs
  log_message(
    paste0("Checking column names against field_types..."),
    showprogress
  )
  validate_columnnames(names(df),
    names(field_types),
    check_length_only = override_column_names
  )

  log_message(
    paste0("Importing source data [", dataset_shortdesc, "]..."),
    showprogress
  )

  # number of rows in source
  rows_source_n <- nrow(df)
  # number of columns in source
  cols_source_n <- length(df)

  # take a copy of the df so that their original doesn't get updated unknowingly
  # and convert df to data.table to ensure consistency hereonwards whether user
  # passes in a data.frame or a data.table
  # TODO: consider removing df at this point, to release memory
  dt <- data.table::as.data.table(df)

  if (override_column_names == TRUE) {
    names(dt) <- names(field_types)
  }

  # Validate data against specification, store warnings instead of printing them
  # use readr::type_convert for now.  Ideally want to store original values and describe action taken too

  # ensure all columns are character type because readr::type_convert won't skip numeric columns
  dt_datatypes <- vapply(dt, typeof, character(1))
  dt_nonchar_warnings <- data.table::data.table()
  if (any(dt_datatypes != "character")) {
    # Report presence of any non-char columns in source data frame (except ignored ones)
    dt_nonchar_warnings <-
      data.table::data.table(
        colindex = which(
          dt_datatypes != "character" &
            !vapply(field_types, is.field_type_ignore, logical(1))
        ),
        rowindex = NA,
        message = paste0(
          "Data supplied as ",
          dt_datatypes[which(dt_datatypes != "character" &
            !vapply(field_types, is.field_type_ignore, logical(1)))],
          " instead of character, non-conformant values will not be identified"
        )
      )
    # update the dt
    changecols <- names(field_types)[dt_datatypes != "character"]
    dt[, (changecols) := lapply(.SD, as.character), .SDcols = changecols]
  }

  log_message(paste0("Checking data against field_types..."), showprogress)
  raw_warnings <- NULL
  dt <-
    withCallingHandlers(
      readr::type_convert(dt,
        field_types_to_cols(field_types),
        na = na
      ),
      warning = function(w) {
        raw_warnings <<- append(raw_warnings, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    )
  log_message(paste0("  Selecting relevant warnings..."), showprogress)
  # extract items of interest from warnings
  # NOTE: column indexes for readr::type_convert warnings correspond to original data file and are 1-based
  # NOTE: row indexes for readr::type_convert warnings are zero-based (confusingly)
  relevant_warnings <- grep("\\[[0-9]*?, [0-9]*?\\]:", raw_warnings, value = TRUE)
  # list of warnings each with character vector containing row, column, message
  warningslist <-
    lapply(
      strsplit(relevant_warnings, ": "),
      function(x) {
        c(gsub("[^0-9]", "", unlist(strsplit(x[1], ","))), x[2])
      }
    )
  warningsdt <-
    data.table::data.table(
      colindex = as.integer(vapply(warningslist, function(x) x[2], character(1))),
      rowindex = as.integer(vapply(warningslist, function(x) x[1], character(1))) + 1,
      message = vapply(warningslist, function(x) x[3], character(1))
    )

  log_message(paste0("  Identifying nonconformant values..."), showprogress)
  # readr::type_convert replaces nonconformant values with NA. Set them to NaN
  # instead to distinguish them from missing
  # This seems much harder than it should be
  warningcols <- unique(warningsdt[, colindex])
  for (c in warningcols) {
    warningcolname <- names(field_types)[c]
    warningrows <- warningsdt[colindex == c, rowindex]
    dt[warningrows, (warningcolname) := NaN]
  }

  log_message(
    paste0("  Checking and removing missing timepoints..."),
    showprogress
  )
  # check and remove rows where timepoint field is null
  # TODO: should I remove them here or when aggregating?  Summary doesn't look
  # right if remove them here. Rownumbers in warnings no longer matches either
  # TODO: check don't duplicate any messages from above
  timepoint_index <- which(vapply(field_types, is.field_type_timepoint, logical(1)))
  timepoint_fieldname <- names(timepoint_index)
  if (anyNA(dt[[(timepoint_fieldname)]])) {
    navector <- is.na(dt[[(timepoint_fieldname)]])
    # stop if there are no valid timepoint values
    if (sum(navector) == nrow(dt)) {
      stop_custom(
        .subclass = "invalid_param_type",
        message = "Timepoint field does not contain any valid values. Check the correct date format has been used."
      )
    }
    timepointwarnings <-
      data.table::data.table(
        colindex = which(names(field_types) == timepoint_fieldname),
        rowindex = which(navector == TRUE),
        message = "Missing or invalid value in Timepoint field"
      )
    warningsdt <- rbind(warningsdt, timepointwarnings)
    dt <- remove_rows(dt, navector)
    timepoint_missing_n <- sum(navector)
  } else {
    timepoint_missing_n <- 0
  }

  # tidy up warnings
  warningsdt <- rbind(warningsdt, dt_nonchar_warnings)
  data.table::setorder(warningsdt, colindex, rowindex)
  warningsdt <- cbind(
    data.table::data.table(fieldname = names(field_types)[warningsdt[, colindex]]),
    warningsdt[, list(colindex, rowindex, message)]
  )
  warnings_summary <-
    warningsdt[,
      list(instances = data.table::fifelse(anyNA(rowindex), NA_integer_, .N)),
      by = list(fieldname, message)
    ]

  # check for duplicate rows
  duprowsvector <-
    identify_duplicaterows(dt,
      batchby_fieldname = timepoint_fieldname,
      showprogress = showprogress
    )
  # find the index row for each duplicate (i.e. the row immediately before any string of dups since we have already sorted the data)...
  duprowsindex <- c(duprowsvector[-1], FALSE)
  duprowsindex <- duprowsindex & !duprowsvector
  # ...and record the no. of duplicates on it
  dpruns <- rle(duprowsvector)
  duprowsindex[which(duprowsindex == TRUE)] <- dpruns$lengths[which(dpruns$values == TRUE)]
  duprowsindex <- data.table::data.table("[DUPLICATES]" = duprowsindex[!duprowsvector])
  # lastly remove the duplicates from the final clean dataset
  dt <- remove_rows(dt, duprowsvector)

  # basic summary info
  # number of rows imported
  rows_imported_n <- nrow(dt)
  # number of columns imported
  cols_imported_n <- length(dt)
  # number of duplicate rows removed
  rows_duplicates_n <- sum(duprowsvector, na.rm = TRUE)

  log_message(paste0("Loading into sourcedata structure..."), showprogress)
  # load data into datafield classes
  dfs <- vector("list", cols_source_n + 1)
  cols_imported_indexes <- vector("integer")

  for (i in 1:cols_source_n) {
    currentfield <- names(field_types[i])
    log_message(paste0("  ", currentfield), showprogress)
    if (is.field_type_ignore(field_types[[i]])) {
      dfs[[i]] <- datafield(as.vector("ignored"), field_types[[i]])
    } else {
      dfs[[i]] <- datafield(
        dt[, currentfield, with = FALSE],
        field_types[[i]],
        warningsdt[
          fieldname == currentfield,
          c("rowindex", "message")
        ]
      )
      cols_imported_indexes <- c(cols_imported_indexes, i)
      names(cols_imported_indexes)[length(cols_imported_indexes)] <- currentfield
    }
  }
  # Create new datafield to store numbers of dups.
  dfs[[cols_source_n + 1]] <- datafield(
    duprowsindex,
    ft_duplicates(),
    warningsdt[
      colindex == 0,
      c("rowindex", "message")
    ]
  )
  names(dfs) <- c(names(field_types), "[DUPLICATES]")

  log_message(paste0("Finished"), showprogress)

  log_function_end(match.call()[[1]])

  structure(
    list(
      datafields = dfs,
      timepoint_fieldname = timepoint_fieldname,
      timepoint_missing_n = timepoint_missing_n,
      rows_source_n = rows_source_n,
      rows_imported_n = rows_imported_n,
      rows_duplicates_n = rows_duplicates_n,
      cols_source_n = cols_source_n,
      cols_imported_n = cols_imported_n,
      cols_imported_indexes = cols_imported_indexes,
      validation_warnings = warnings_summary,
      dataset_shortdesc = dataset_shortdesc,
      na_values = na
    ),
    class = "sourcedata"
  )
}
#' Test if object is a sourcedata object
#'
#' @param x object to test
#' @return Logical
#' @noRd
is.sourcedata <- function(x) inherits(x, "sourcedata")

#' @export
print.sourcedata <- function(x, ...) {
  sourcesummary <- summarise_source_data(x, showprogress = FALSE)
  cat("Class: sourcedata\n")
  cat("Dataset:", x$dataset_shortdesc, "\n")
  cat("\n")
  cat("Overall:\n")
  cat("Columns in source:", sourcesummary$overall["cols_source_n"], "\n")
  cat("Columns imported:", sourcesummary$overall["cols_imported_n"], "\n")
  cat("Rows in source:", sourcesummary$overall["rows_source_n"], "\n")
  cat("Duplicate rows removed:", sourcesummary$overall["rows_duplicates_n"], "\n")
  cat("Rows imported:", sourcesummary$overall["rows_imported_n"], "\n")
  cat("Column used for timepoint:", sourcesummary$overall["timepoint_fieldname"], "\n")
  cat("Min timepoint value:", sourcesummary$overall["timepoint_min"], "\n")
  cat("Max timepoint value:", sourcesummary$overall["timepoint_max"], "\n")
  cat("Rows missing timepoint values removed:", sourcesummary$overall["timepoint_missing_n"], "\n")
  cat("Strings interpreted as missing values:", sourcesummary$overall["na_values"], "\n")
  cat("Total validation warnings:", sum(sourcesummary$validation_warnings$instances), "\n")
  cat("\n")
  cat("Datafields:\n")
  print(sourcesummary$datafields)
  cat("\n")
  cat("Validation warnings:\n")
  cat("\n")
  if (nrow(sourcesummary$validation_warnings) > 0) {
    print(sourcesummary$validation_warnings)
  } else {
    cat("None")
  }
}

#' Create an object containing a high-level summary of a sourcedata object
#'
#' This can be used by other functions later for displaying info to user
#'
#' @param sourcedata sourcedata object
#' @param showprogress Print progress to console. Default = TRUE
#' @return A list of 1. overall dataset properties, 2. properties of each
#'   datafield, 3. any validation warnings
#' @noRd
# TODO: consider making this a generic summary() method instead.
#       Help file says summary() is for models but there are a bunch of other objects implementing it too
# TODO: Consider adding a warning if a categorical field has "too many" different values
summarise_source_data <- function(sourcedata, showprogress = TRUE) {
  log_function_start(match.call()[[1]])
  log_message(paste0("Creating summary of source data..."), showprogress)

  log_message(paste0("  For overall dataset..."), showprogress)
  overall <- c(
    cols_source_n = format(sourcedata$cols_source_n),
    cols_imported_n = format(sourcedata$cols_imported_n),
    rows_source_n = format(sourcedata$rows_source_n),
    rows_duplicates_n = format(sourcedata$rows_duplicates_n),
    rows_imported_n = format(sourcedata$rows_imported_n),
    timepoint_fieldname = sourcedata$timepoint_fieldname,
    timepoint_min = format(get_datafield_min(sourcedata$datafields[[sourcedata$timepoint_fieldname]])),
    timepoint_max = format(get_datafield_max(sourcedata$datafields[[sourcedata$timepoint_fieldname]])),
    timepoint_missing_n = format(sourcedata$timepoint_missing_n),
    na_values = paste(dQuote(sourcedata$na_values, q = FALSE), collapse = ",")
  )

  log_message(paste0("  For each column in dataset..."), showprogress)
  datafields <-
    data.frame(
      fieldname = format(names(sourcedata$datafields[1:sourcedata$cols_source_n])),
      field_type = format(vapply(
        sourcedata$datafields[1:sourcedata$cols_source_n],
        get_datafield_field_type_name, character(1)
      )),
      datatype = format(vapply(
        sourcedata$datafields[1:sourcedata$cols_source_n],
        get_datafield_basetype, character(1)
      )),
      count = format(vapply(
        sourcedata$datafields[1:sourcedata$cols_source_n],
        get_datafield_count, integer(1)
      )),
      missing = format(vapply(
        sourcedata$datafields[1:sourcedata$cols_source_n],
        function(x) {
          gdm <- get_datafield_missing(x)
          if (is.na(gdm$frequency)) {
            NA_character_
          } else {
            paste0(gdm$frequency, " (", format(gdm$percentage, digits = 1), "%)")
          }
        },
        character(1)
      )),
      min = vapply(
        sourcedata$datafields[1:sourcedata$cols_source_n],
        function(x) format(get_datafield_min(x)),
        character(1)
      ),
      max = vapply(
        sourcedata$datafields[1:sourcedata$cols_source_n],
        function(x) format(get_datafield_max(x)),
        character(1)
      ),
      validation_warnings = format(vapply(
        sourcedata$datafields[1:sourcedata$cols_source_n],
        get_datafield_validation_warnings_n, integer(1)
      )),
      stringsAsFactors = FALSE,
      row.names = NULL
    )

  log_message(paste0("  Validation errors on loading dataset..."), showprogress)
  validation_warnings <- sourcedata$validation_warnings

  log_function_end(match.call()[[1]])

  list(overall = overall, datafields = datafields, validation_warnings = validation_warnings)
}

#####################################################################
# functions to get info about each individual datafield

#' Get field_type (short string) of datafield
#'
#' @param datafield datafield object
#' @return string denoting field_type
#' @noRd
get_datafield_field_type_name <- function(datafield) {
  datafield$field_type$type
}

#' Get data vector of datafield
#'
#' @param datafield datafield object
#' @return vector of data values
#' @noRd
get_datafield_vector <- function(datafield) {
  if (is.field_type_ignore(datafield$field_type)) {
    NA
  } else {
    datafield$values[[1]]
  }
}

#' Get data storage type of datafield
#'
#' @param datafield datafield object
#' @return string denoting storage type
#' @noRd
get_datafield_basetype <- function(datafield) {
  if (is.field_type_ignore(datafield$field_type)) {
    NA_character_
  } else {
    typeof(datafield$values[[1]])
  }
}

#' Get minimum data value of datafield
#'
#' @param datafield datafield object
#' @return minimum data value, excluding NAs
#' @noRd
get_datafield_min <- function(datafield) {
  if (is.field_type_ignore(datafield$field_type) ||
    all(is.na(datafield$values[[1]]))) {
    NA_real_
  } else {
    min(datafield$values[[1]], na.rm = TRUE)
  }
}

#' Get maximum data value of datafield
#'
#' @param datafield datafield object
#' @return maximum data value, excluding NAs
#' @noRd
get_datafield_max <- function(datafield) {
  if (is.field_type_ignore(datafield$field_type) ||
    all(is.na(datafield$values[[1]]))) {
    NA_real_
  } else {
    max(datafield$values[[1]], na.rm = TRUE)
  }
}

#' Get number/percentage of missing values in datafield
#'
#' @param datafield datafield object
#' @return numeric list of 1. frequency, 2. percentage
#' @noRd
get_datafield_missing <- function(datafield) {
  if (is.field_type_ignore(datafield$field_type)) {
    list("frequency" = NA_integer_, "percentage" = NA_real_)
  } else {
    list(
      "frequency" = sum(is.na(datafield$values[[1]])),
      "percentage" = 100 * sum(is.na(datafield$values[[1]])) / length(datafield$values[[1]])
    )
  }
}

#' Get number of validation warnings for datafield
#'
#' @param datafield datafield object
#' @return number of validation warnings
#' @noRd
get_datafield_validation_warnings_n <- function(datafield) {
  if (is.field_type_ignore(datafield$field_type) ||
    is.field_type_calculated(datafield$field_type)) {
    NA_integer_
  } else {
    nrow(datafield$validation_warnings)
  }
}

#' Get number of values present in datafield
#'
#' @param datafield datafield object
#' @return number of non-missing values
#' @noRd
get_datafield_count <- function(datafield) {
  if (is.field_type_ignore(datafield$field_type) ||
    all(is.na(datafield$values[[1]]))) {
    NA_integer_
  } else {
    sum(!is.na(datafield$values[[1]]))
  }
}


# -----------------------------------------------------------------------------
#' Compare column names to field_types specification
#'
#' If there are any validation errors, these are all compiled before calling a
#' single stop()
#'
#' @param source_names vector of column names in dataset
#' @param spec_names vector of column names that should be there
#' @param check_length_only logical denoting that we only want to check the
#'   number of names and not the actual names (since we plan to override the
#'   names anyway)
#' @noRd
validate_columnnames <- function(source_names,
                                 spec_names,
                                 check_length_only = FALSE) {

  # validate - collect all errors together and return only once
  err_validation <- character()

  if (check_length_only == TRUE) {
    if (length(source_names) != length(spec_names)) {
      err_validation <-
        append(
          err_validation,
          paste0(
            "Different number of columns in data vs field_types specification: ",
            length(source_names),
            " in source, ",
            length(spec_names),
            " in specification"
          )
        )
    }
  } else {
    # check for duplicates (spec_names should already have been checked in field_types constructor)
    if (anyDuplicated(source_names) > 0) {
      err_validation <-
        append(
          err_validation,
          paste(
            "Duplicate column names in data: [",
            paste(source_names[duplicated(source_names)], collapse = ", "),
            "]"
          )
        )
    }
    # names must be identical
    # TODO: do we want to allow names to be in a different order? Need to consider downstream effects.
    if (length(setdiff(source_names, spec_names)) > 0) {
      err_validation <-
        append(
          err_validation,
          paste(
            "Column names in data but not in field_types specification: [",
            paste(setdiff(source_names, spec_names), collapse = ", "),
            "]"
          )
        )
    }
    if (length(setdiff(spec_names, source_names)) > 0) {
      err_validation <-
        append(
          err_validation,
          paste(
            "Column names in field_types specification but not in data: [",
            paste(setdiff(spec_names, source_names), collapse = ", "),
            "]"
          )
        )
    }
  }

  if (length(err_validation) > 0) {
    stop_custom(
      .subclass = "invalid_columnnames",
      message = paste0(
        "Invalid column names.\n",
        paste(err_validation, collapse = "\n")
      )
    )
  }
}

#############################################################################
#' Identify any duplicate rows in a memory-efficient way
#'
#' @param dt data.table potentially containing duplicate rows
#' @param batchby_fieldname should be a field with well-spread data in order to
#'   get evenly-sized batches (i.e. the timepoint field)
#' @param batchsize_mb approximate size in Mb over which dt will be split into
#'   batches
#' @param showprogress Print progress to console
#' @return logical vector indicating which rows are duplicates
#' @noRd
identify_duplicaterows <- function(dt,
                                   batchby_fieldname,
                                   batchsize_mb = 200,
                                   showprogress = TRUE) {
  log_message(paste0("Checking for duplicates..."), showprogress)
  # sort by batchby_fieldname then by everything else, so that we can batch the data
  # TODO: try using setkey as well to see if it makes a difference
  log_message(paste0("  Sorting data..."), showprogress)
  data.table::setorderv(
    dt,
    c(
      batchby_fieldname,
      names(dt)[-which(names(dt) == batchby_fieldname)]
    )
  )

  # Need to chunk up large datasets
  # estimate total size and limit size of each chunk
  dtsize <- utils::object.size(dt) / 1000000
  if (dtsize > batchsize_mb) {
    numrows <- nrow(dt)
    numchunks <- as.numeric(ceiling(dtsize / batchsize_mb))
    chunkrows <- ceiling(numrows / numchunks)
    log_message(
      paste0("  Running ", numchunks, " batches of roughly ", chunkrows, " rows each..."),
      showprogress
    )
    batchby_vector <- dt[[(batchby_fieldname)]]
    duprowsvector <- logical(numrows)
    for (chunk in 1:numchunks) {
      log_message(paste0("  Batch ", chunk), showprogress)
      chunkstart <- which.max(batchby_vector >= batchby_vector[((chunk - 1) * chunkrows) + 1])
      if (chunk < numchunks) {
        # end on the previous (unique) field value that the chunk lands on
        chunkend <- which.max(batchby_vector >= batchby_vector[chunk * chunkrows]) - 1
      } else {
        # or else to the end of the dataset
        chunkend <- numrows
      }
      duprowsvector[chunkstart:chunkend] <- duplicated(dt[chunkstart:chunkend, ])
    }
  } else {
    duprowsvector <- duplicated(dt)
  }

  duprowsvector
}

#' Remove rows from data.table in a memory-efficient way
#'
#' Row deletion by reference doesn't exist in data.table yet. Interim
#' memory-efficient solution
#'
#' @param dt data.table
#' @param rowindicator logical vector indicating which rows should be removed
#' @return data.table with rows removed
#' @noRd
remove_rows <- function(dt, rowindicator) {
  if (any(rowindicator)) {
    # NOTE: Need copy() because otherwise when using cols <- names(dt), cols updates when columns are removed from dt
    cols <- data.table::copy(names(dt))
    dt_temp <- data.table::data.table("Col1" = dt[[1]][!rowindicator])
    names(dt_temp)[1] <- cols[1]
    dt[, (cols[1]) := NULL]
    for (col in cols[2:length(cols)]) {
      dt_temp[, (col) := dt[[col]][!rowindicator]]
      dt[, (col) := NULL]
    }
    dt <- dt_temp
  }
  dt
}
