# Code for creation of source_data object
#   which contains both the (vector) data for the field and the relevant metadata

# -----------------------------------------------------------------------------
#' Prepare source data
#'
#' Validate a data frame against a [field_types()] specification, and prepare
#' for aggregation.
#'
#' @param df A data frame
#' @param field_types [field_types()] object specifying names and types of
#'   fields (columns) in the supplied `df`. See also [field_types_available].
#' @param override_column_names If `FALSE`, column names in the supplied `df`
#'   must match the names specified in `field_types` exactly. If `TRUE`, column
#'   names in the supplied `df` will be replaced with the names specified in
#'   `field_types`. The specification must therefore contain the columns in the
#'   correct order. Default = `FALSE`
#' @param na vector containing strings that should be interpreted as missing
#'   values, Default = `c("","NA","NULL")`.
#' @param dataset_description Short description of the dataset being checked.
#'   This will appear on the report. If blank, the name of the data frame object
#'   will be used
#' @param show_progress Print progress to console. Default = `TRUE`
#' @return A `daiquiri_source_data` object
#' @examples
#' # load example data into a data.frame
#' raw_data <- read_data(
#'   system.file("extdata", "example_prescriptions.csv", package = "daiquiri"),
#'   delim = ",",
#'   col_names = TRUE
#' )
#'
#' # validate and prepare the data for aggregation
#' source_data <- prepare_data(
#'   raw_data,
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
#'   dataset_description = "Example data provided with package"
#' )
#'
#' source_data
#' @seealso [field_types()], [field_types_available()],
#'   [aggregate_data()], [report_data()],
#'   [create_report()]
#' @export
#' @importFrom data.table .N .SD
prepare_data <- function(df,
                         field_types,
                         override_column_names = FALSE,
                         na = c("", "NA", "NULL"),
                         dataset_description = NULL,
                         show_progress = TRUE) {
  log_function_start(match.call()[[1]])

  # initialise known column names to prevent R CMD check notes
  col_index <- row_index <- field_name <- NULL

  validate_params_required(match.call())
  validate_params_type(match.call(),
    df = df,
    field_types = field_types,
    override_column_names = override_column_names,
    na = na,
    dataset_description = dataset_description,
    show_progress = show_progress
  )

  # use dataset_description if present, otherwise get from call
  if (is.null(dataset_description)) {
    # look for create_report() in the call stack and if present, use the latest one
    # can't just use sys.function(-1) as that doesn't work inside testthat
    matched_calls <- grep("create_report", as.character(sys.calls()))
    if (length(matched_calls) > 0) {
      dataset_description <-
        as.character(enquote(
          as.list(
            match.call(
              definition = sys.function(rev(matched_calls)[1]),
              call = sys.call(sys.parent())
            )
          )$df
        ))[2]
    } else {
      dataset_description <- as.character(enquote(as.list(match.call())$df))[2]
    }
  }

  log_message(
    paste0("field_types supplied:\n", field_types_to_string(field_types)),
    show_progress
  )

  # validate inputs
  log_message(
    paste0("Checking column names against field_types..."),
    show_progress
  )
  validate_column_names(names(df),
    names(field_types),
    check_length_only = override_column_names
  )

  log_message(
    paste0("Importing source data [", dataset_description, "]..."),
    show_progress
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
  # use readr::type_convert for now. Ideally want to store original values and describe action taken too

  # ensure all columns are character type because readr::type_convert won't skip numeric columns
  dt_datatypes <- vapply(dt, typeof, character(1))
  dt_nonchar_warnings <- data.table::data.table()
  if (any(dt_datatypes != "character")) {
    # Report presence of any non-char columns in source data frame (except ignored ones)
    dt_nonchar_warnings <-
      data.table::data.table(
        col_index = which(
          dt_datatypes != "character" &
            !vapply(field_types, is_ft_ignore, logical(1))
        ),
        row_index = NA,
        message = paste0(
          "Data supplied as ",
          dt_datatypes[which(dt_datatypes != "character" &
            !vapply(field_types, is_ft_ignore, logical(1)))],
          " instead of character, non-conformant values will not be identified"
        )
      )
    # update the dt
    changecols <- names(field_types)[dt_datatypes != "character"]
    dt[, (changecols) := lapply(.SD, as.character), .SDcols = changecols]
  }

  log_message(paste0("Checking data against field_types..."), show_progress)
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
  log_message(paste0("  Selecting relevant warnings..."), show_progress)
  # extract items of interest from warnings
  # NOTE: column indexes for readr::type_convert warnings correspond to original data file and are 1-based
  # NOTE: row indexes for readr::type_convert warnings are zero-based (confusingly)
  relevant_warnings <- grep("\\[[0-9]*?, [0-9]*?\\]:", raw_warnings, value = TRUE)
  # list of warnings each with character vector containing row, column, message
  warnings_list <-
    lapply(
      strsplit(relevant_warnings, ": "),
      function(x) {
        c(gsub("[^0-9]", "", unlist(strsplit(x[1], ","))), x[2])
      }
    )
  warnings_dt <-
    data.table::data.table(
      col_index = as.integer(vapply(warnings_list, function(x) x[2], character(1))),
      row_index = as.integer(vapply(warnings_list, function(x) x[1], character(1))) + 1,
      message = vapply(warnings_list, function(x) x[3], character(1))
    )

  log_message(paste0("  Identifying nonconformant values..."), show_progress)
  # readr::type_convert replaces nonconformant values with NA. Set them to NaN
  # instead to distinguish them from missing
  # This seems much harder than it should be
  warning_cols <- unique(warnings_dt[, col_index])
  for (c in warning_cols) {
    warning_colname <- names(field_types)[c]
    warning_rows <- warnings_dt[col_index == c, row_index]
    dt[warning_rows, (warning_colname) := NaN]
  }

  log_message(
    paste0("  Checking and removing missing timepoints..."),
    show_progress
  )
  # check and remove rows where timepoint field is null
  # TODO: should I remove them here or when aggregating?  Summary doesn't look
  # right if remove them here. Rownumbers in warnings no longer matches either
  # TODO: check don't duplicate any messages from above
  timepoint_index <- which(vapply(field_types, is_ft_timepoint, logical(1)))
  timepoint_field_name <- names(timepoint_index)
  if (anyNA(dt[[(timepoint_field_name)]])) {
    na_vector <- is.na(dt[[(timepoint_field_name)]])
    # stop if there are no valid timepoint values
    if (sum(na_vector) == nrow(dt)) {
      stop_custom(
        .subclass = "invalid_param_type",
        message = "Timepoint field does not contain any valid values. Check the correct date format has been used."
      )
    }
    timepoint_warnings <-
      data.table::data.table(
        col_index = which(names(field_types) == timepoint_field_name),
        row_index = which(na_vector == TRUE),
        message = "Missing or invalid value in Timepoint field"
      )
    warnings_dt <- rbind(warnings_dt, timepoint_warnings)
    dt <- remove_rows(dt, na_vector)
    timepoint_missing_n <- sum(na_vector)
  } else {
    timepoint_missing_n <- 0
  }

  # tidy up warnings
  warnings_dt <- rbind(warnings_dt, dt_nonchar_warnings)
  data.table::setorder(warnings_dt, col_index, row_index)
  warnings_dt <- cbind(
    data.table::data.table(field_name = names(field_types)[warnings_dt[, col_index]]),
    warnings_dt[, list(col_index, row_index, message)]
  )
  warnings_summary <-
    warnings_dt[,
      list(instances = data.table::fifelse(anyNA(row_index), NA_integer_, .N)),
      by = list(field_name, message)
    ]

  # check for duplicate rows
  duprows_vector <-
    identify_duplicate_rows(dt,
      sort_field_name = timepoint_field_name,
      show_progress = show_progress
    )
  # find the index row for each duplicate
  # (i.e. the row immediately before any string of dups since we have already sorted the data)...
  duprows_index <- c(duprows_vector[-1], FALSE)
  duprows_index <- duprows_index & !duprows_vector
  # ...and record the no. of duplicates on it
  dpruns <- rle(duprows_vector)
  duprows_index[which(duprows_index == TRUE)] <- dpruns$lengths[which(dpruns$values == TRUE)]
  duprows_index <- data.table::data.table("[DUPLICATES]" = duprows_index[!duprows_vector])
  # lastly remove the duplicates from the final clean dataset
  dt <- remove_rows(dt, duprows_vector)

  # basic summary info
  # number of rows imported
  rows_imported_n <- nrow(dt)
  # number of columns imported
  cols_imported_n <- length(dt)
  # number of duplicate rows removed
  rows_duplicates_n <- sum(duprows_vector, na.rm = TRUE)

  log_message(paste0("Loading into source_data structure..."), show_progress)
  # load data into data_field classes
  dfs <- vector("list", cols_source_n + 1)
  cols_imported_indexes <- vector("integer")

  for (i in 1:cols_source_n) {
    current_field <- names(field_types[i])
    log_message(paste0("  ", current_field), show_progress)
    if (is_ft_ignore(field_types[[i]])) {
      dfs[[i]] <- data_field(as.vector("ignored"), field_types[[i]])
    } else {
      dfs[[i]] <- data_field(
        dt[, current_field, with = FALSE],
        field_types[[i]],
        warnings_dt[
          field_name == current_field,
          c("row_index", "message")
        ]
      )
      cols_imported_indexes <- c(cols_imported_indexes, i)
      names(cols_imported_indexes)[length(cols_imported_indexes)] <- current_field
    }
  }
  # Create new data_field to store numbers of dups.
  dfs[[cols_source_n + 1]] <- data_field(
    duprows_index,
    ft_duplicates(),
    warnings_dt[
      col_index == 0,
      c("row_index", "message")
    ]
  )
  names(dfs) <- c(names(field_types), "[DUPLICATES]")

  log_message(paste0("Finished"), show_progress)

  log_function_end(match.call()[[1]])

  structure(
    list(
      data_fields = dfs,
      timepoint_field_name = timepoint_field_name,
      timepoint_missing_n = timepoint_missing_n,
      rows_source_n = rows_source_n,
      rows_imported_n = rows_imported_n,
      rows_duplicates_n = rows_duplicates_n,
      cols_source_n = cols_source_n,
      cols_imported_n = cols_imported_n,
      cols_imported_indexes = cols_imported_indexes,
      validation_warnings = warnings_summary,
      dataset_description = dataset_description,
      na_values = na
    ),
    class = "daiquiri_source_data"
  )
}


# -----------------------------------------------------------------------------
#' @export
print.daiquiri_source_data <- function(x, ...) {
  summary <- summarise_source_data(x, show_progress = FALSE)
  cat("Dataset:", x$dataset_description, "\n")
  cat("\n")
  cat("Overall:\n")
  cat("Columns in source:", summary$overall["cols_source_n"], "\n")
  cat("Columns imported:", summary$overall["cols_imported_n"], "\n")
  cat("Rows in source:", summary$overall["rows_source_n"], "\n")
  cat("Duplicate rows removed:", summary$overall["rows_duplicates_n"], "\n")
  cat("Rows imported:", summary$overall["rows_imported_n"], "\n")
  cat("Column used for timepoint:", summary$overall["timepoint_field_name"], "\n")
  cat("Min timepoint value:", summary$overall["timepoint_min"], "\n")
  cat("Max timepoint value:", summary$overall["timepoint_max"], "\n")
  cat("Rows missing timepoint values removed:", summary$overall["timepoint_missing_n"], "\n")
  cat("Strings interpreted as missing values:", summary$overall["na_values"], "\n")
  cat("Total validation warnings:", sum(summary$validation_warnings$instances), "\n")
  cat("\n")
  cat("Datafields:\n")
  print(summary$data_fields)
  cat("\n")
  cat("Validation warnings:\n")
  cat("\n")
  if (nrow(summary$validation_warnings) > 0) {
    print(summary$validation_warnings)
  } else {
    cat("None")
  }
}


# -----------------------------------------------------------------------------
#' Test if object is a source_data object
#'
#' @param x object to test
#' @return Logical
#' @noRd
is_source_data <- function(x) inherits(x, "daiquiri_source_data")


# -----------------------------------------------------------------------------
#' Create an object containing a high-level summary of a source_data object
#'
#' This can be used by other functions later for displaying info to user
#'
#' @param source_data source_data object
#' @param show_progress Print progress to console. Default = TRUE
#' @return A list of 1. overall dataset properties, 2. properties of each
#'   data_field, 3. any validation warnings
#' @noRd
# TODO: consider making this a generic summary() method instead.
# Help file says summary() is for models but there are a bunch of other objects implementing it too
# TODO: Consider adding a warning if a categorical field has "too many" different values
summarise_source_data <- function(source_data, show_progress = TRUE) {
  log_function_start(match.call()[[1]])
  log_message(paste0("Creating summary of source data..."), show_progress)

  timepoint_field <- source_data$data_fields[[source_data$timepoint_field_name]]

  log_message(paste0("  For overall dataset..."), show_progress)

  overall <- c(
    cols_source_n = format(source_data$cols_source_n),
    cols_imported_n = format(source_data$cols_imported_n),
    rows_source_n = format(source_data$rows_source_n),
    rows_duplicates_n = format(source_data$rows_duplicates_n),
    rows_imported_n = format(source_data$rows_imported_n),
    timepoint_field_name = source_data$timepoint_field_name,
    timepoint_min = format(data_field_min(timepoint_field)),
    timepoint_max = format(data_field_max(timepoint_field)),
    timepoint_missing_n = format(source_data$timepoint_missing_n),
    na_values = paste(dQuote(source_data$na_values, q = FALSE), collapse = ",")
  )

  log_message(paste0("  For each column in dataset..."), show_progress)
  # Exclude calculated fields
  imported_fields <- source_data$data_fields[1:source_data$cols_source_n]

  data_fields <-
    data.frame(
      field_name = format(names(imported_fields)),
      field_type = format(vapply(
        imported_fields,
        data_field_type, character(1)
      )),
      datatype = format(vapply(
        imported_fields,
        data_field_basetype, character(1)
      )),
      count = format(vapply(
        imported_fields,
        data_field_count, integer(1)
      )),
      missing = format(vapply(
        imported_fields,
        function(x) {
          gdm <- data_field_missing(x)
          if (is.na(gdm$frequency)) {
            NA_character_
          } else {
            paste0(gdm$frequency, " (", format(gdm$percentage, digits = 1), "%)")
          }
        },
        character(1)
      )),
      min = vapply(
        imported_fields,
        function(x) format(data_field_min(x)),
        character(1)
      ),
      max = vapply(
        imported_fields,
        function(x) format(data_field_max(x)),
        character(1)
      ),
      validation_warnings = format(vapply(
        imported_fields,
        data_field_validation_warnings_n, integer(1)
      )),
      stringsAsFactors = FALSE,
      row.names = NULL
    )

  log_message(paste0("  Validation errors on loading dataset..."), show_progress)
  validation_warnings <- source_data$validation_warnings

  log_function_end(match.call()[[1]])

  list(
    overall = overall,
    data_fields = data_fields,
    validation_warnings = validation_warnings
  )
}


# -----------------------------------------------------------------------------
#' Constructor for individual data_fields within source_data object
#'
#' @param x vector of cleaned values for data_field
#' @param field_type field_type object specified for the data_field
#' @param validation_warnings data.table containing any parser/package-specific
#'   warnings
#' @noRd
#' @return A `data_field` object
# TODO: not sure if better to store the entire field_type
# or just its name or even as a separate list in the source_data
data_field <- function(x, field_type, validation_warnings = NULL) {
  structure(
    list(
      values = x,
      field_type = field_type,
      column_name = names(x[1]),
      validation_warnings = validation_warnings
    ),
    class = c(paste0(
      "daiquiri_data_field_", field_type_type(field_type)
    ), "daiquiri_data_field")
  )
}


# -----------------------------------------------------------------------------
#' Test if object is a data_field object
#'
#' @param x object to test
#' @return Logical
#' @noRd
is_data_field <- function(x) inherits(x, "daiquiri_data_field")


# -----------------------------------------------------------------------------
# PROPERTIES OF INDIVIDUAL data_field OBJECTS

#' Get field_type (short string) of data_field
#'
#' @param data_field data_field object
#' @return string denoting field_type
#' @noRd
data_field_type <- function(data_field) {
  data_field$field_type$type
}

#' Get data vector of data_field
#'
#' @param data_field data_field object
#' @return vector of data values
#' @noRd
data_field_vector <- function(data_field) {
  if (is_ft_ignore(data_field$field_type)) {
    NA
  } else {
    data_field$values[[1]]
  }
}

#' Get data storage type of data_field
#'
#' @param data_field data_field object
#' @return string denoting storage type
#' @noRd
data_field_basetype <- function(data_field) {
  if (is_ft_ignore(data_field$field_type)) {
    NA_character_
  } else {
    typeof(data_field$values[[1]])
  }
}

#' Get minimum data value of data_field
#'
#' @param data_field data_field object
#' @return minimum data value, excluding NAs
#' @noRd
data_field_min <- function(data_field) {
  data_vals <- data_field$values[[1]]
  if (is_ft_ignore(data_field$field_type) || all(is.na(data_vals))) {
    NA_real_
  } else {
    min(data_vals, na.rm = TRUE)
  }
}

#' Get maximum data value of data_field
#'
#' @param data_field data_field object
#' @return maximum data value, excluding NAs
#' @noRd
data_field_max <- function(data_field) {
  data_vals <- data_field$values[[1]]
  if (is_ft_ignore(data_field$field_type) || all(is.na(data_field$values[[1]]))) {
    NA_real_
  } else {
    max(data_field$values[[1]], na.rm = TRUE)
  }
}

#' Get number/percentage of missing values in data_field
#'
#' @param data_field data_field object
#' @return numeric list of 1. frequency, 2. percentage
#' @noRd
data_field_missing <- function(data_field) {
  if (is_ft_ignore(data_field$field_type)) {
    list("frequency" = NA_integer_, "percentage" = NA_real_)
  } else {
    data_vals <- data_field$values[[1]]
    list(
      "frequency" = sum(is.na(data_vals)),
      "percentage" = 100 * sum(is.na(data_vals)) / length(data_vals)
    )
  }
}

#' Get number of validation warnings for data_field
#'
#' @param data_field data_field object
#' @return number of validation warnings
#' @noRd
data_field_validation_warnings_n <- function(data_field) {
  if (is_ft_ignore(data_field$field_type) || is_ft_calculated(data_field$field_type)) {
    NA_integer_
  } else {
    nrow(data_field$validation_warnings)
  }
}

#' Get number of values present in data_field
#'
#' @param data_field data_field object
#' @return number of non-missing values
#' @noRd
data_field_count <- function(data_field) {
  data_vals <- data_field$values[[1]]

  if (is_ft_ignore(data_field$field_type) || all(is.na(data_vals))) {
    NA_integer_
  } else {
    sum(!is.na(data_vals))
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
validate_column_names <- function(source_names,
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
      .subclass = "invalid_column_names",
      message = paste0(
        "Invalid column names.\n",
        paste(err_validation, collapse = "\n")
      )
    )
  }
}



# -----------------------------------------------------------------------------
#' Identify any duplicate rows in a memory-efficient way
#'
#' @param dt data.table potentially containing duplicate rows
#' @param sort_field_name should be a field with well-spread data in order to
#'   get evenly-sized batches (i.e. the timepoint field)
#' @param batch_size_mb approximate size in Mb over which dt will be split into
#'   batches
#' @param show_progress Print progress to console
#' @return logical vector indicating which rows are duplicates
#' @noRd
identify_duplicate_rows <- function(dt,
                                    sort_field_name,
                                    batch_size_mb = 200,
                                    show_progress = TRUE) {
  log_message(paste0("Checking for duplicates..."), show_progress)
  # sort by sort_field_name then by everything else, so that we can batch the data
  # TODO: try using setkey as well to see if it makes a difference
  log_message(paste0("  Sorting data..."), show_progress)
  data.table::setorderv(
    dt,
    c(
      sort_field_name,
      names(dt)[-which(names(dt) == sort_field_name)]
    )
  )

  # Need to chunk up large datasets
  # estimate total size and limit size of each chunk
  dt_size <- utils::object.size(dt) / 1000000
  if (dt_size > batch_size_mb) {
    num_rows <- nrow(dt)
    num_chunks <- as.numeric(ceiling(dt_size / batch_size_mb))
    rows_per_chunk <- ceiling(num_rows / num_chunks)
    log_message(
      paste0("  Running ", num_chunks, " batches of roughly ", rows_per_chunk, " rows each..."),
      show_progress
    )
    batchby_vector <- dt[[(sort_field_name)]]
    duprows_vector <- logical(num_rows)
    for (chunk in 1:num_chunks) {
      log_message(paste0("  Batch ", chunk), show_progress)
      chunk_start <- which.max(batchby_vector >= batchby_vector[((chunk - 1) * rows_per_chunk) + 1])
      if (chunk < num_chunks) {
        # end on the previous (unique) field value that the chunk lands on
        chunk_end <- which.max(batchby_vector >= batchby_vector[chunk * rows_per_chunk]) - 1
      } else {
        # or else to the end of the dataset
        chunk_end <- num_rows
      }
      duprows_vector[chunk_start:chunk_end] <- duplicated(dt[chunk_start:chunk_end, ])
    }
  } else {
    duprows_vector <- duplicated(dt)
  }

  duprows_vector
}


# -----------------------------------------------------------------------------
#' Remove rows from data.table in a memory-efficient way
#'
#' Row deletion by reference doesn't exist in data.table yet. Interim
#' memory-efficient solution
#'
#' @param dt data.table
#' @param row_indicator logical vector indicating which rows should be removed
#' @return data.table with rows removed
#' @noRd
remove_rows <- function(dt, row_indicator) {
  if (any(row_indicator)) {
    # NOTE: Need copy() because otherwise when using cols <- names(dt), cols updates when columns are removed from dt
    cols <- data.table::copy(names(dt))
    dt_temp <- data.table::data.table("Col1" = dt[[1]][!row_indicator])
    names(dt_temp)[1] <- cols[1]
    dt[, (cols[1]) := NULL]
    for (col in cols[2:length(cols)]) {
      dt_temp[, (col) := dt[[col]][!row_indicator]]
      dt[, (col) := NULL]
    }
    dt <- dt_temp
  }
  dt
}
