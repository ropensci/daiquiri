# Code for creation of aggregatedata object
# These contain both the (vector) data for the aggregated field and the relevant metadata
# TODO: decide whether better to store values as a dataframe or a list of ts/zoo objects

# -----------------------------------------------------------------------------
#' Constructor for an individual aggregatefield object
#'
#' @param datafield datafield object
#' @param timepointfieldvalues all values in timepoint field
#' @return aggregatefield object, including a data.table where the first column
#'   is the timepoint group, then one column per aggregationfunction
#' @noRd
#' @importFrom data.table ':=' .EACHI
aggregatefield <- function(datafield,
                           timepointfieldvalues,
                           alltimepoints,
                           aggregation_timeunit,
                           showprogress = TRUE) {

  # initialise known column names to prevent R CMD check notes
  n <- value <- values <- timepointgroup <- NULL

  log_message(paste0("Preparing..."), showprogress)
  functionlist <- datafield$fieldtype$aggfunctions

  log_message(
    paste0("Aggregating ", get_datafield_basetype(datafield), " field..."),
    showprogress
  )

  # TODO: consider doing this by reference
  # this contains all values present in the original datafield, alongside their timepointgroup
  datafield_dt <-
    data.table::data.table(
      "timepointgroup" = timepoint_as_aggregationunit(timepointfieldvalues,
        aggregation_timeunit = aggregation_timeunit
      ),
      "values" = datafield[["values"]][[1]],
      key = "timepointgroup"
    )
  # this contains the aggfn values after aggregating (one column per aggfn)
  groupedvals <- data.table::as.data.table(alltimepoints)
  data.table::setkey(groupedvals)

  for (i in seq_along(functionlist)) {
    f <- functionlist[i]
    log_message(paste0("  By ", f), showprogress)
    if (f == "n") {
      # number of values present (including non-conformant ones)
      groupedvals[datafield_dt[, list("value" = sum(!(is.na(values) &
        !is.nan(values)))),
      by = list(timepointgroup)
      ],
      (f) := value,
      by = .EACHI
      ]
      # for timepoints with no records, set n to 0 (but all other aggfunctions should show NA)
      groupedvals[is.na(n), "n" := 0]
    } else if (all(datafield_dt[, is.na(values)])) {
      # when all datafield values are NA, just return NAs
      groupedvals[, (f) := NA_real_]
    } else if (f == "missing_n") {
      # number of values missing (excludes non-conformant ones)
      groupedvals[datafield_dt[, list("value" = sum(is.na(values) &
        !is.nan(values))),
      by = list(timepointgroup)
      ],
      (f) := value,
      by = .EACHI
      ]
    } else if (f == "missing_perc") {
      # percentage of values missing (excludes non-conformant ones) out of number of records
      groupedvals[datafield_dt[, list("value" = 100 * sum(is.na(values) &
        !is.nan(values)) / length(values)),
      by = list(timepointgroup)
      ],
      (f) := value,
      by = .EACHI
      ]
    } else if (f == "nonconformant_n") {
      # number of nonconformant values
      groupedvals[datafield_dt[, list("value" = sum(is.nan(values))),
        by = list(timepointgroup)
      ],
      (f) := value,
      by = .EACHI
      ]
    } else if (f == "nonconformant_perc") {
      # percentage of nonconformant values out of number of records
      # TODO: should the denominator be all rows or only conformant/nonmissing rows?
      groupedvals[datafield_dt[, list("value" = 100 * sum(is.nan(values)) /
        length(values)),
      by = list(timepointgroup)
      ],
      (f) := value,
      by = .EACHI
      ]
    } else if (f == "sum") {
      # sum of values (used to indicate number of duplicate rows removed)
      groupedvals[datafield_dt[, list("value" = sum(values, na.rm = TRUE)),
        by = list(timepointgroup)
      ],
      (f) := value,
      by = .EACHI
      ]
    } else if (f == "nonzero_perc") {
      # percentage of values which are non-zero out of number of values present
      #   (used to indicate percentage of remaining records that were duplicated)
      groupedvals[datafield_dt[, list("value" = 100 * length(which(values >
        0)) / length(values[!is.na(values)])),
      by = list(timepointgroup)
      ],
      (f) := value,
      by = .EACHI
      ]
    } else if (f == "distinct") {
      # number of distinct values (excluding NAs)
      groupedvals[datafield_dt[, list("value" = length(unique(values[!is.na(values)]))),
        by = list(timepointgroup)
      ],
      (f) := value,
      by = .EACHI
      ]
    } else if (f %in% c("subcat_n", "subcat_perc")) {
      # create a separate column per category value
      distinctcategories <-
        sort(datafield_dt[is.na(values) == FALSE, unique(values)])
      log_message(paste0("    ", length(distinctcategories), " categories found"), showprogress)
      # If there is only one category, don't bother
      if (length(distinctcategories) > 1) {
        # TODO: consider setting a max number of categories
        for (j in seq_along(distinctcategories)) {
          log_message(paste0("    ", j, ": ", distinctcategories[j]), showprogress)
          catval <- distinctcategories[j]
          catname <-
            paste0(f, "_", j, "_", gsub("([[:punct:]])|\\s+", "_", catval))
          if (f == "subcat_n") {
            # number of times this particular category value appears
            groupedvals[datafield_dt[, list("value" = sum(values == catval, na.rm = TRUE)),
              by = list(timepointgroup)
            ],
            (catname) := value,
            by = .EACHI
            ]
          } else if (f == "subcat_perc") {
            # percentage this particular category value appears out of number of records
            # include all values in denominator, including NA and NaN
            groupedvals[datafield_dt[, list("value" = 100 * sum(values == catval, na.rm = TRUE) /
              length(values)),
            by = list(timepointgroup)
            ],
            (catname) := value,
            by = .EACHI
            ]
          }
        }
      }
    } else if (f == "midnight_n") {
      # number of values whose time portion is midnight (used to check for missing time portions)
      # TODO: if n is zero, should this be zero or NA?
      groupedvals[datafield_dt[, list("value" = sum(format(values, format = "%T") == "00:00:00", na.rm = TRUE)),
        by = list(timepointgroup)
      ],
      (f) := value,
      by = .EACHI
      ]
    } else if (f == "midnight_perc") {
      # percentage of values whose time portion is midnight (used to check for missing time portions)
      #   out of number of values present
      groupedvals[datafield_dt[, list("value" = 100 * sum(format(values, format = "%T") == "00:00:00", na.rm = TRUE) /
        length(values[!is.na(values)])),
      by = list(timepointgroup)
      ],
      (f) := value,
      by = .EACHI
      ]
      # NOTE: if n is zero, the above returns NaN. Update to NA instead
      groupedvals[is.nan(get(f)), (f) := NA_real_]
    } else if (f == "min") {
      # minimum value, whether numeric or datetime. Excludes NAs
      # NOTE: min/max return warnings when all values are NA, so need to suppress them
      groupedvals[datafield_dt[, list("value" = suppressWarnings(min(values, na.rm = TRUE))),
        by = list(timepointgroup)
      ],
      (f) := value,
      by = .EACHI
      ]
      # min/max return Inf when all values are NA. Update to NA instead
      groupedvals[is.infinite(get(f)), (f) := NA_real_]
      # min/max also drops datetime class so preserve datatypes
      if (inherits(datafield[["values"]][[1]], "POSIXct")) {
        # TODO: make sure this is consistent with data format on loading
        groupedvals[, (f) := as.POSIXct(get(f), tz = "UTC", origin = "1970-01-01")]
      }
    } else if (f == "max") {
      # maximum value, whether numeric or datetime. Excludes NAs
      # NOTE: min/max return warnings when all values are NA, so need to suppress them
      groupedvals[datafield_dt[, list("value" = suppressWarnings(max(values, na.rm = TRUE))),
        by = list(timepointgroup)
      ],
      (f) := value,
      by = .EACHI
      ]
      # min/max return Inf when all values are NA. Update to NA instead
      groupedvals[is.infinite(get(f)), (f) := NA_real_]
      # min/max also drops datetime class so preserve datatypes
      if (inherits(datafield[["values"]][[1]], "POSIXct")) {
        # TODO: make sure this is consistent with data format on loading
        groupedvals[, (f) := as.POSIXct(get(f), tz = "UTC", origin = "1970-01-01")]
      }
    } else if (f == "mean") {
      # mean value. Excludes NAs
      groupedvals[datafield_dt[, list("value" = mean(values, na.rm = TRUE)),
        by = list(timepointgroup)
      ],
      (f) := value,
      by = .EACHI
      ]
      # NOTE: mean returns NaN when all values are NA. Update to NA instead
      groupedvals[is.nan(get(f)), (f) := NA_real_]
    } else if (f == "median") {
      # median value. Excludes NAs
      groupedvals[datafield_dt[, list("value" = stats::median(values, na.rm = TRUE)),
        by = list(timepointgroup)
      ],
      (f) := value,
      by = .EACHI
      ]
    } else if (f == "minlength") {
      # minimum character length
      # NOTE: min/max return warnings when all values are NA, so need to suppress them
      # NOTE: need the as.double() because min/max returns integer if all values
      # are NA (in the group), and if a mixture of doubles and integers are
      # returned, data.table doesn't like it (though the error only seems to appear
      # when using the package and not when testing inside the package itself)
      groupedvals[
        datafield_dt[,
          list("value" = suppressWarnings(as.double(
            min(nchar(as.character(values),
              keepNA = TRUE
            ),
            na.rm = TRUE
            )
          ))),
          by = list(timepointgroup)
        ],
        (f) := value,
        by = .EACHI
      ]
      # min/max return Inf when all values are NA. Update to NA instead
      groupedvals[is.infinite(get(f)), (f) := NA_real_]
    } else if (f == "maxlength") {
      # maximum character length
      # NOTE: min/max return warnings when all values are NA, so need to suppress them
      # NOTE: need the as.double() because min/max returns integer if all values
      # are NA (in the group), and if a mixture of doubles and integers are
      # returned, data.table doesn't like it (though the error only seems to appear
      # when using the package and not when testing inside the package itself)
      groupedvals[
        datafield_dt[,
          list("value" = suppressWarnings(as.double(
            max(nchar(as.character(values),
              keepNA = TRUE
            ),
            na.rm = TRUE
            )
          ))),
          by = list(timepointgroup)
        ],
        (f) := value,
        by = .EACHI
      ]
      # min/max return Inf when all values are NA. Update to NA instead
      groupedvals[is.infinite(get(f)), (f) := NA_real_]
    } else if (f == "meanlength") {
      # mean character length
      groupedvals[datafield_dt[, list("value" = mean(nchar(as.character(values),
        keepNA = TRUE
      ),
      na.rm = TRUE
      )),
      by = list(timepointgroup)
      ],
      (f) := value,
      by = .EACHI
      ]
      # NOTE: mean returns NaN when all values are NA. Update to NA instead
      groupedvals[is.nan(get(f)), (f) := NA_real_]
    } else {
      # TODO: Decide if this should stop everything or just raise a warning
      # TODO: Putting it here means it doesn't get called if all values are NA
      stop(paste("Unrecognised aggregation type:", f),
        call. = FALSE
      )
    }
  }

  log_message(paste0("Finished"), showprogress)

  structure(
    list(
      values = groupedvals,
      functionlist = functionlist,
      fieldtype = datafield$fieldtype,
      columnname = datafield$columnname
    ),
    class = "aggregatefield"
  )
}

#' Test if object is an aggregatefield object
#'
#' @param x object to test
#' @return Logical
#' @noRd
is.aggregatefield <- function(x) inherits(x, "aggregatefield")

# -----------------------------------------------------------------------------
#' Constructor for the [ALLFIELDSCOMBINED] aggregatefield object
#'
#' Uses results from already-aggregated individual fields rather than doing it
#' all again
#'
#' @param aggfields all aggregatefield objects from data (i.e. excluding
#'   calculated aggfields)
#' @param showprogress Print progress to console
#' @return aggregatefield object, including a data.table where the first column
#'   is the timepoint group, then one column per aggregationfunction
#' @noRd
# TODO: do we want to include duplicates in here too?
# TODO: this field has a numeric datatype whereas individual fields have an int
# datatype, decide if need to make them all the same
aggregateallfields <- function(aggfields,
                               showprogress = TRUE) {

  # initialise known column names to prevent R CMD check notes
  n <- missing_n <- nonconformant_n <- NULL

  ft <- ft_allfields()
  functionlist <- ft$aggfunctions

  groupedvals <- aggfields[[1]][["values"]][, 1]

  for (i in seq_along(aggfields)) {
    for (j in 2:length(names(aggfields[[i]][["values"]]))) {
      # TODO: ideally want to use the aggfunctions list from the allfields
      # fieldtype rather than hard code
      if (names(aggfields[[i]][["values"]])[j] %in% c("n", "missing_n", "nonconformant_n")) {
        f <- names(aggfields[[i]][["values"]])[j]
        if (f %in% names(groupedvals)) {
          # If all values are NA then leave as NA, but if any values are not NA then ignore the NAs (per timepoint)
          groupedvals[
            ,
            (f) := data.table::fifelse(
              is.na(get(f)) & is.na(aggfields[[i]][["values"]][, get(f)]),
              NA_integer_,
              rowSums(cbind(get(f), aggfields[[i]][["values"]][, get(f)]),
                na.rm = TRUE
              )
            )
          ]
        } else {
          groupedvals[, (f) := aggfields[[i]][["values"]][, get(f)]]
        }
      }
    }
  }
  # if there are no datetime or numeric fields, nonconformant_n field needs to be
  # created explicitly
  if (!("nonconformant_n" %in% names(groupedvals))) {
    groupedvals[, "nonconformant_n" := data.table::fifelse(n == 0, NA_integer_, 0)]
  }

  groupedvals[, "missing_perc" := 100 * missing_n / (n + missing_n + nonconformant_n)]
  groupedvals[, "nonconformant_perc" := 100 * nonconformant_n / (n + missing_n + nonconformant_n)]

  log_message(paste0("Finished"), showprogress)

  structure(
    list(
      values = groupedvals,
      functionlist = functionlist,
      fieldtype = ft,
      columnname = "[ALLFIELDSCOMBINED]"
    ),
    class = "aggregatefield"
  )
}

# -----------------------------------------------------------------------------
#' Aggregate source data
#'
#' Aggregates a `sourcedata` object based on the [fieldtypes()] specified at load time.
#' Default time period for aggregation is a calendar day
#'
#' @param sourcedata A `sourcedata` object returned from
#'   [prepare_data()] function
#' @param aggregation_timeunit Unit of time to aggregate over. Specify one of
#'   `"day"`, `"week"`, `"month"`, `"quarter"`, `"year"`. The `"week"` option is
#'   Monday-based. Default = `"day"`
#' @param showprogress Print progress to console. Default = `TRUE`
#' @return An `aggregatedata` object
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
#'   fieldtypes = fieldtypes(
#'     PrescriptionID = ft_uniqueidentifier(),
#'     PrescriptionDate = ft_timepoint(),
#'     AdmissionDate = ft_datetime(includes_time = FALSE),
#'     Drug = ft_freetext(),
#'     Dose = ft_numeric(),
#'     DoseUnit = ft_categorical(),
#'     PatientID = ft_ignore(),
#'     Location = ft_categorical(aggregate_by_each_category = TRUE)
#'   ),
#'   override_columnnames = FALSE,
#'   na = c("", "NULL")
#' )
#'
#' # aggregate the data
#' aggregatedataobj <- aggregate_data(
#'   sourcedataobj,
#'   aggregation_timeunit = "day"
#' )
#'
#' @seealso [prepare_data()], [report_data()]
#' @export
aggregate_data <- function(sourcedata,
                           aggregation_timeunit = "day",
                           showprogress = TRUE) {
  # TODO: allow user to override existing aggfunctions?
  # TODO: Use something better than seq() to calculate weeks and months, so that
  # it works when the first date is not the first of the month

  log_function_start(match.call()[[1]])

  validate_params_required(match.call())
  validate_params_type(match.call(),
    sourcedata = sourcedata,
    aggregation_timeunit = aggregation_timeunit,
    showprogress = showprogress
  )

  # create column to group by
  # TODO: raise an error/warning if data is less granular than aggregation_timeunit
  log_message(
    paste0("Aggregating [", sourcedata$sourcename, "] by [", aggregation_timeunit, "]..."),
    showprogress
  )
  # need to ensure have all possible timepoint values, even if they are missing in the dataset
  alltimepoints_min <-
    timepoint_as_aggregationunit(
      get_datafield_min(sourcedata$datafields[[sourcedata$timepoint_fieldname]]),
      aggregation_timeunit
    )
  alltimepoints_max <-
    timepoint_as_aggregationunit(
      get_datafield_max(sourcedata$datafields[[sourcedata$timepoint_fieldname]]),
      aggregation_timeunit
    )
  alltimepoints <-
    data.table::data.table(seq(alltimepoints_min, alltimepoints_max, by = aggregation_timeunit))

  names(alltimepoints) <-
    paste0(
      gsub("[^a-zA-Z0-9_]", "_", sourcedata$timepoint_fieldname),
      "_by",
      aggregation_timeunit
    )

  ### AGGREGATE OVERALL DATASET
  log_message(paste0("Aggregating overall dataset..."), showprogress)
  # load aggregated data into new vector
  log_message(paste0("Aggregating each datafield in turn..."), showprogress)
  agg <- vector("list", sourcedata$cols_imported_n + 2)
  for (i in 1:sourcedata$cols_imported_n) {
    log_message(paste0(i, ": ", names(sourcedata$cols_imported_indexes)[i]), showprogress)
    fieldindex <- sourcedata$cols_imported_indexes[[i]]
    agg[[i]] <-
      aggregatefield(
        sourcedata$datafields[[fieldindex]],
        get_datafield_vector(sourcedata$datafields[[sourcedata$timepoint_fieldname]]),
        alltimepoints,
        aggregation_timeunit,
        showprogress = showprogress
      )
  }
  log_message(paste0("Aggregating calculated fields..."), showprogress)
  log_message(paste0("[DUPLICATES]:"), showprogress)
  agg[[sourcedata$cols_imported_n + 1]] <-
    aggregatefield(
      sourcedata$datafields[[sourcedata$cols_source_n + 1]],
      get_datafield_vector(sourcedata$datafields[[sourcedata$timepoint_fieldname]]),
      alltimepoints,
      aggregation_timeunit,
      showprogress = showprogress
    )
  log_message(paste0("[ALLFIELDSCOMBINED]:"), showprogress)
  agg[[sourcedata$cols_imported_n + 2]] <-
    aggregateallfields(
      agg[1:sourcedata$cols_imported_n],
      showprogress = showprogress
    )
  names(agg) <-
    c(
      names(sourcedata$cols_imported_indexes),
      "[DUPLICATES]",
      "[ALLFIELDSCOMBINED]"
    )


  log_function_end(match.call()[[1]])

  structure(
    list(
      aggregatefields = agg,
      timepoint_fieldname = sourcedata$timepoint_fieldname,
      # not sure if this should be set at overall object level or allow it to
      # differ per aggregatefield
      aggregation_timeunit = aggregation_timeunit
    ),
    class = "aggregatedata"
  )
}

#' Test if object is an aggregatedata object
#'
#' @param x object to test
#' @return Logical
#' @noRd
is.aggregatedata <- function(x) inherits(x, "aggregatedata")

# -----------------------------------------------------------------------------
#' Export aggregated data
#'
#' Export aggregated data to disk.  Creates a separate file for each aggregated
#' field in dataset.
#'
#' @param aggregatedata A `aggregatedata` object
#' @param save_directory String. Full or relative path for save folder
#' @param save_fileprefix String. Optional prefix for the exported filenames
#' @param save_filetype String. Filetype extension supported by `readr`,
#'   currently only csv allowed
#' @return (invisibly) The `aggregatedata` object that was passed in
#' @examples rawdata <- read_data(
#'   system.file("extdata", "example_prescriptions.csv", package = "daiquiri"),
#'   delim = ",",
#'   col_names = TRUE
#' )
#'
#' sourcedataobj <- prepare_data(
#'   rawdata,
#'   fieldtypes = fieldtypes(
#'     PrescriptionID = ft_uniqueidentifier(),
#'     PrescriptionDate = ft_timepoint(),
#'     AdmissionDate = ft_datetime(includes_time = FALSE),
#'     Drug = ft_freetext(),
#'     Dose = ft_numeric(),
#'     DoseUnit = ft_categorical(),
#'     PatientID = ft_ignore(),
#'     Location = ft_categorical(aggregate_by_each_category = TRUE)
#'   ),
#'   override_columnnames = FALSE,
#'   na = c("", "NULL")
#' )
#'
#' aggregatedataobj <- aggregate_data(
#'   sourcedataobj,
#'   aggregation_timeunit = "day"
#' )
#'
#' export_aggregated_data(
#'   aggregatedataobj,
#'   save_directory = ".",
#'   save_fileprefix = "ex_"
#' )
#'
#' \dontshow{
#' f <- list.files(".", "^ex_.*csv$")
#' file.remove(f)
#' }
#'
#' @export
export_aggregated_data <- function(aggregatedata,
                                   save_directory,
                                   save_fileprefix = "",
                                   save_filetype = "csv") {

  # validation checks on params
  validate_params_required(match.call())
  validate_params_type(match.call(),
    aggregatedata = aggregatedata,
    save_directory = save_directory,
    save_fileprefix = save_fileprefix,
    save_filetype = save_filetype
  )

  if (!(save_filetype %in% c("csv"))) {
    stop(paste(
      "Invalid save_filetype: ",
      save_filetype,
      ". Only csv format is currently supported"
    ))
  }

  # export a file for each field in dataset
  for (i in seq_along(aggregatedata$aggregatefields)) {
    readr::write_csv(
      aggregatedata$aggregatefields[[i]]$values,
      file.path(save_directory, paste0(
        save_fileprefix,
        names(aggregatedata$aggregatefields[i]),
        ".csv"
      ))
    )
  }

  invisible(aggregatedata)
}

#' @export
print.aggregatedata <- function(x, ...) {
  aggsummary <- summarise_aggregated_data(x)
  cat("Class: aggregatedata\n")
  cat("\n")
  cat("Overall:\n")
  cat("Number of data fields:", aggsummary$overall["n_fields"], "\n")
  cat("Column used for timepoint:", aggsummary$overall["timepoint_fieldname"], "\n")
  cat("Timepoint aggregation unit:", aggsummary$overall["aggregation_timeunit"], "\n")
  cat("Min timepoint value:", aggsummary$overall["timepoint_min"], "\n")
  cat("Max timepoint value:", aggsummary$overall["timepoint_max"], "\n")
  cat("Total number of timepoints:", aggsummary$overall["n_timepoints"], "\n")
  cat("Number of empty timepoints:", aggsummary$overall["n_empty_timepoints"], "\n")
  cat("\n")
}

#' Create an object containing a high-level summary of an aggregatedata object
#'
#' This can be used by other functions later for displaying info to user
#'
#' @param aggregatedata aggregatedata object
#' @return A list of 1. overall dataset properties
#' @noRd
# TODO: consider making this a generic summary() method instead. Help file says
# summary() is for models but there are a bunch of other objects implementing it
# too
summarise_aggregated_data <- function(aggregatedata) {
  aggfields <- aggregatedata$aggregatefields

  # summary info for overall dataset
  # use timepoint column to illustrate overall counts
  overall <- c(
    n_fields = length(aggfields),
    timepoint_fieldname = aggregatedata$timepoint_fieldname,
    aggregation_timeunit = aggregatedata$aggregation_timeunit,
    timepoint_min = format(
      min(aggfields[[aggregatedata$timepoint_fieldname]]$values[[1]])
    ),
    timepoint_max = format(
      max(aggfields[[aggregatedata$timepoint_fieldname]]$values[[1]])
    ),
    n_timepoints = length(aggfields[[aggregatedata$timepoint_fieldname]]$values[[1]]),
    n_empty_timepoints = sum(aggfields[[aggregatedata$timepoint_fieldname]]$values[["n"]] == 0)
  )

  structure(
    list(
      overall = overall
    ),
    class = "summary_aggregated_data"
  )
}

#' Convert vector of timepoint values to desired aggregation unit
#'
#' Allocate timepoint values to appropriate day/week/month etc. for later grouping
#'
#' @param x vector of original timepoint values
#' @param aggregation_timeunit desired aggregation granularity
#' @return vector of new timepoint values, same length as before but discretized values
#' @noRd
timepoint_as_aggregationunit <- function(x, aggregation_timeunit) {
  if (aggregation_timeunit == "day") {
    as.Date(x)
  } else if (aggregation_timeunit == "week") {
    # use the prior (or current) Monday
    as.Date(x) - (as.integer(format(x, format = "%u")) - 1)
  } else if (aggregation_timeunit == "month") {
    as.Date(format(x, format = "%Y-%m-01"))
  } else if (aggregation_timeunit == "quarter") {
    as.Date(vapply(x, function(x) {
      format(
        x,
        paste0(
          "%Y-",
          switch(format(x, format = "%m"),
            "01" = "01",
            "02" = "01",
            "03" = "01",
            "04" = "04",
            "05" = "04",
            "06" = "04",
            "07" = "07",
            "08" = "07",
            "09" = "07",
            "10" = "10",
            "11" = "10",
            "12" = "10"
          ),
          "-01"
        )
      )
    }, character(1)))
  } else if (aggregation_timeunit == "year") {
    as.Date(format(x, format = "%Y-01-01"))
  }
}


# -----------------------------------------------------------------------------
# TODO: Define set of allowed aggregation functions similarly to fieldtypes,
# with each object containing formula for aggregation as well as friendly names

#' Set user-friendly names for aggtypes
#'
#' This uses the aggfield columnnames rather than the original
#' aggregationfunction (relevant for subcats)
#'
#' @param aggtype string name of aggtype (from aggfield columnname)
#' @param type "short" or "long"
#' @return string containing friendly name
#' @noRd
# TODO: come up with some friendlier short names, currently just the aggtype itself
aggtype_friendlyname <- function(aggtype, type) {
  if (startsWith(aggtype, "subcat_")) {
    catval <- substring(gsub("^(?:[^_]*_){3}", "_", aggtype), 2)
    if (startsWith(aggtype, "subcat_n")) {
      switch(type,
        short = aggtype,
        long = paste0("No. of values in the category: ", catval)
      )
    } else if (startsWith(aggtype, "subcat_perc")) {
      switch(type,
        short = aggtype,
        long = paste0("Percentage of values in the category: ", catval)
      )
    }
  } else {
    switch(aggtype,
      n = {
        switch(type,
          short = "n",
          long = "No. of values present"
        )
      },
      missing_n = {
        switch(type,
          short = "missing_n",
          long = "No. of missing values"
        )
      },
      missing_perc = {
        switch(type,
          short = "missing_perc",
          long = "Percentage of missing values"
        )
      },
      nonconformant_n = {
        switch(type,
          short = "nonconformant_n",
          long = "No. of non-conformant values"
        )
      },
      nonconformant_perc = {
        switch(type,
          short = "nonconformant_perc",
          long = "Percentage of non-conformant values"
        )
      },
      sum = {
        switch(type,
          short = "sum",
          long = "No. of duplicate records removed"
        )
      },
      nonzero_perc = {
        switch(type,
          short = "nonzero_perc",
          long = "Percentage of (remaining) records that were duplicated"
        )
      },
      distinct = {
        switch(type,
          short = "distinct",
          long = "No. of distinct values"
        )
      },
      midnight_n = {
        switch(type,
          short = "midnight_n",
          long = "No. of values with no time element"
        )
      },
      midnight_perc = {
        switch(type,
          short = "midnight_perc",
          long = "Percentage of values with no time element"
        )
      },
      min = {
        switch(type,
          short = "min",
          long = "Minimum value"
        )
      },
      max = {
        switch(type,
          short = "max",
          long = "Maximum value"
        )
      },
      mean = {
        switch(type,
          short = "mean",
          long = "Mean value"
        )
      },
      median = {
        switch(type,
          short = "median",
          long = "Median value"
        )
      },
      minlength = {
        switch(type,
          short = "minlength",
          long = "Minimum string length"
        )
      },
      maxlength = {
        switch(type,
          short = "maxlength",
          long = "Maximum string length"
        )
      },
      meanlength = {
        switch(type,
          short = "meanlength",
          long = "Mean string length"
        )
      }
    )
  }
}
