# Code for creation of aggregated_data object
# These contain both the (vector) data for the aggregated field and the relevant metadata

# -----------------------------------------------------------------------------
#' Aggregate source data
#'
#' Aggregates a `daiquiri_source_data` object based on the [field_types()] specified at load time.
#' Default time period for aggregation is a calendar day
#'
#' @param source_data A `daiquiri_source_data` object returned from
#'   [prepare_data()] function
#' @param aggregation_timeunit Unit of time to aggregate over. Specify one of
#'   `"day"`, `"week"`, `"month"`, `"quarter"`, `"year"`. The `"week"` option is
#'   Monday-based. Default = `"day"`
#' @param show_progress Print progress to console. Default = `TRUE`
#' @return A `daiquiri_aggregated_data` object
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
#'   na = c("", "NULL")
#' )
#'
#' # aggregate the data
#' aggregated_data <- aggregate_data(
#'   source_data,
#'   aggregation_timeunit = "day"
#' )
#'
#' aggregated_data
#' @seealso [prepare_data()], [report_data()]
#' @export
aggregate_data <- function(source_data,
                           aggregation_timeunit = "day",
                           show_progress = TRUE) {
  # TODO: allow user to override existing aggregation_functions?
  # TODO: raise an error/warning if data is less granular than aggregation_timeunit

  log_function_start(match.call()[[1]])

  validate_params_required(match.call())
  validate_params_type(match.call(),
    source_data = source_data,
    aggregation_timeunit = aggregation_timeunit,
    show_progress = show_progress
  )

  log_message(
    paste0("Aggregating [", source_data$sourcename, "] by [", aggregation_timeunit, "]..."),
    show_progress
  )

  # create column to group by, which will become the x-axes
  timepoint_group_sequence <- create_timepoint_groups(
    timepoint_field = source_data$data_fields[[source_data$timepoint_field_name]],
    aggregation_timeunit = aggregation_timeunit
  )

  # get timepoint field values as they will be used repeatedly later
  timepoint_field_values <- data_field_vector(
    source_data$data_fields[[source_data$timepoint_field_name]]
  )

  ### AGGREGATE OVERALL DATASET
  log_message(paste0("Aggregating overall dataset..."), show_progress)
  # load aggregated data into new vector
  log_message(paste0("Aggregating each data_field in turn..."), show_progress)
  agg_fields <- vector("list", source_data$cols_imported_n + 2)
  for (i in 1:source_data$cols_imported_n) {
    log_message(paste0(i, ": ", names(source_data$cols_imported_indexes)[i]), show_progress)
    fieldindex <- source_data$cols_imported_indexes[[i]]
    agg_fields[[i]] <-
      aggregate_field(
        source_data$data_fields[[fieldindex]],
        timepoint_field_values,
        timepoint_group_sequence,
        aggregation_timeunit,
        show_progress = show_progress
      )
  }
  log_message(paste0("Aggregating calculated fields..."), show_progress)
  log_message(paste0("[DUPLICATES]:"), show_progress)
  agg_fields[[source_data$cols_imported_n + 1]] <-
    aggregate_field(
      source_data$data_fields[[source_data$cols_source_n + 1]],
      timepoint_field_values,
      timepoint_group_sequence,
      aggregation_timeunit,
      show_progress = show_progress
    )
  log_message(paste0("[ALL_FIELDS_COMBINED]:"), show_progress)
  agg_fields[[source_data$cols_imported_n + 2]] <-
    aggregate_combined_fields(
      agg_fields[1:source_data$cols_imported_n],
      show_progress = show_progress
    )
  names(agg_fields) <-
    c(
      names(source_data$cols_imported_indexes),
      "[DUPLICATES]",
      "[ALL_FIELDS_COMBINED]"
    )


  log_function_end(match.call()[[1]])

  structure(
    list(
      aggregated_fields = agg_fields,
      timepoint_field_name = source_data$timepoint_field_name,
      # not sure if this should be set at overall object level or allow it to
      # differ per aggregated_field
      aggregation_timeunit = aggregation_timeunit,
      dataset_description = source_data$dataset_description
    ),
    class = "daiquiri_aggregated_data"
  )
}


# -----------------------------------------------------------------------------
#' @export
print.daiquiri_aggregated_data <- function(x, ...) {
  agg_summary <- summarise_aggregated_data(x)
  cat("Dataset:", x$dataset_description, "\n")
  cat("\n")
  cat("Overall:\n")
  cat("Number of data fields:", agg_summary$overall["n_fields"], "\n")
  cat("Column used for timepoint:", agg_summary$overall["timepoint_field_name"], "\n")
  cat("Timepoint aggregation unit:", agg_summary$overall["aggregation_timeunit"], "\n")
  cat("Min timepoint value:", agg_summary$overall["timepoint_min"], "\n")
  cat("Max timepoint value:", agg_summary$overall["timepoint_max"], "\n")
  cat("Total number of timepoints:", agg_summary$overall["n_timepoints"], "\n")
  cat("Number of empty timepoints:", agg_summary$overall["n_empty_timepoints"], "\n")
  cat("\n")
}


# -----------------------------------------------------------------------------
#' Export aggregated data
#'
#' Export aggregated data to disk.  Creates a separate file for each aggregated
#' field in dataset.
#'
#' @param aggregated_data A `daiquiri_aggregated_data` object
#' @param save_directory String. Full or relative path for save folder
#' @param save_file_prefix String. Optional prefix for the exported filenames
#' @param save_file_type String. Filetype extension supported by `readr`,
#'   currently only csv allowed
#' @return (invisibly) The `daiquiri_aggregated_data` object that was passed in
#' @examples raw_data <- read_data(
#'   system.file("extdata", "example_prescriptions.csv", package = "daiquiri"),
#'   delim = ",",
#'   col_names = TRUE
#' )
#'
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
#'   na = c("", "NULL")
#' )
#'
#' aggregated_data <- aggregate_data(
#'   source_data,
#'   aggregation_timeunit = "day"
#' )
#'
#' export_aggregated_data(
#'   aggregated_data,
#'   save_directory = ".",
#'   save_file_prefix = "ex_"
#' )
#'
#' \dontshow{
#' f <- list.files(".", "^ex_.*csv$")
#' file.remove(f)
#' }
#'
#' @export
export_aggregated_data <- function(aggregated_data,
                                   save_directory,
                                   save_file_prefix = "",
                                   save_file_type = "csv") {
  # validation checks on params
  validate_params_required(match.call())
  validate_params_type(match.call(),
    aggregated_data = aggregated_data,
    save_directory = save_directory,
    save_file_prefix = save_file_prefix,
    save_file_type = save_file_type
  )

  if (!(save_file_type %in% c("csv"))) {
    stop(paste(
      "Invalid save_file_type: ",
      save_file_type,
      ". Only csv format is currently supported"
    ))
  }

  # export a file for each field in dataset
  for (i in seq_along(aggregated_data$aggregated_fields)) {
    readr::write_csv(
      aggregated_data$aggregated_fields[[i]]$values,
      file.path(save_directory, paste0(
        save_file_prefix,
        names(aggregated_data$aggregated_fields[i]),
        ".csv"
      ))
    )
  }

  invisible(aggregated_data)
}


# -----------------------------------------------------------------------------
#' Test if object is an aggregated_data object
#'
#' @param x object to test
#' @return Logical
#' @noRd
is_aggregated_data <- function(x) inherits(x, "daiquiri_aggregated_data")


# -----------------------------------------------------------------------------
#' Create an object containing a high-level summary of an aggregated_data object
#'
#' This can be used by other functions later for displaying info to user
#'
#' @param aggregated_data aggregated_data object
#' @return A list of 1. overall dataset properties
#' @noRd
# TODO: consider making this a generic summary() method instead. Help file says
# summary() is for models but there are a bunch of other objects implementing it
# too
summarise_aggregated_data <- function(aggregated_data) {
  agg_fields <- aggregated_data$aggregated_fields
  timepoint_field <- agg_fields[[aggregated_data$timepoint_field_name]]

  # summary info for overall dataset
  # use timepoint column to illustrate overall counts
  overall <- c(
    n_fields = length(agg_fields),
    timepoint_field_name = aggregated_data$timepoint_field_name,
    aggregation_timeunit = aggregated_data$aggregation_timeunit,
    timepoint_min = format(
      min(timepoint_field$values[[1]])
    ),
    timepoint_max = format(
      max(timepoint_field$values[[1]])
    ),
    n_timepoints = length(timepoint_field$values[[1]]),
    n_empty_timepoints = sum(timepoint_field$values[["n"]] == 0)
  )

  list(overall = overall)
}


# -----------------------------------------------------------------------------
#' Constructor for an individual aggregated_field object
#'
#' @param data_field data_field object
#' @param timepoint_field_values all values in timepoint field
#' @return aggregated_field object, including a data.table where the first column
#'   is the timepoint group, then one column per aggregation function
#' @noRd
#' @importFrom data.table ':=' .EACHI
aggregate_field <- function(data_field,
                            timepoint_field_values,
                            timepoint_group_sequence,
                            aggregation_timeunit,
                            show_progress = TRUE) {
  # initialise known column names to prevent R CMD check notes
  n <- value <- values <- timepoint_group <- NULL

  log_message(paste0("Preparing..."), show_progress)
  function_list <- data_field$field_type$aggregation_functions

  log_message(
    paste0("Aggregating ", data_field_basetype(data_field), " field..."),
    show_progress
  )

  # this contains all values present in the original data_field, alongside their timepoint_group
  data_field_dt <-
    data.table::data.table(
      "timepoint_group" = timepoint_as_timepoint_group(
        timepoint_field_values,
        aggregation_timeunit = aggregation_timeunit
      ),
      "values" = data_field[["values"]][[1]],
      key = "timepoint_group"
    )
  # this contains the agg_fun values after aggregating (one column per agg_fun)
  grouped_values <- data.table::as.data.table(timepoint_group_sequence)
  data.table::setkey(grouped_values)

  for (i in seq_along(function_list)) {
    f <- function_list[i]
    log_message(paste0("  By ", f), show_progress)
    if (f == "n") {
      # number of values present (including non-conformant ones)
      grouped_values[
        data_field_dt[
          ,
          list("value" = sum(!(is.na(values) & !is.nan(values)))),
          by = list(timepoint_group)
        ],
        (f) := value,
        by = .EACHI
      ]
      # for timepoints with no records, set n to 0 (but all other aggregation_functions should show NA)
      grouped_values[is.na(n), "n" := 0]
    } else if (all(data_field_dt[, is.na(values)])) {
      # when all data_field values are NA, just return NAs
      grouped_values[, (f) := NA_real_]
    } else if (f == "missing_n") {
      # number of values missing (excludes non-conformant ones)
      grouped_values[
        data_field_dt[
          ,
          list("value" = sum(is.na(values) & !is.nan(values))),
          by = list(timepoint_group)
        ],
        (f) := value,
        by = .EACHI
      ]
    } else if (f == "missing_perc") {
      # percentage of values missing (excludes non-conformant ones) out of number of records
      grouped_values[
        data_field_dt[
          ,
          list("value" = 100 * sum(is.na(values) & !is.nan(values)) / length(values)),
          by = list(timepoint_group)
        ],
        (f) := value,
        by = .EACHI
      ]
    } else if (f == "nonconformant_n") {
      # number of nonconformant values
      grouped_values[
        data_field_dt[
          ,
          list("value" = sum(is.nan(values))),
          by = list(timepoint_group)
        ],
        (f) := value,
        by = .EACHI
      ]
    } else if (f == "nonconformant_perc") {
      # percentage of nonconformant values out of number of records
      # TODO: should the denominator be all rows or only conformant/nonmissing rows?
      grouped_values[
        data_field_dt[
          ,
          list("value" = 100 * sum(is.nan(values)) / length(values)),
          by = list(timepoint_group)
        ],
        (f) := value,
        by = .EACHI
      ]
    } else if (f == "sum") {
      # sum of values (used to indicate number of duplicate rows removed)
      grouped_values[
        data_field_dt[
          ,
          list("value" = sum(values, na.rm = TRUE)),
          by = list(timepoint_group)
        ],
        (f) := value,
        by = .EACHI
      ]
    } else if (f == "nonzero_perc") {
      # percentage of values which are non-zero out of number of values present
      #   (used to indicate percentage of remaining records that were duplicated)
      grouped_values[
        data_field_dt[
          ,
          list("value" = 100 * length(which(values > 0)) / length(values[!is.na(values)])),
          by = list(timepoint_group)
        ],
        (f) := value,
        by = .EACHI
      ]
    } else if (f == "distinct") {
      # number of distinct values (excluding NAs)
      grouped_values[
        data_field_dt[
          ,
          list("value" = length(unique(values[!is.na(values)]))),
          by = list(timepoint_group)
        ],
        (f) := value,
        by = .EACHI
      ]
    } else if (f %in% c("subcat_n", "subcat_perc")) {
      # create a separate column per category value
      distinct_categories <-
        sort(data_field_dt[is.na(values) == FALSE, unique(values)])
      log_message(paste0("    ", length(distinct_categories), " categories found"), show_progress)
      # If there is only one category, don't bother
      if (length(distinct_categories) > 1) {
        # TODO: consider setting a max number of categories
        for (j in seq_along(distinct_categories)) {
          log_message(paste0("    ", j, ": ", distinct_categories[j]), show_progress)
          catval <- distinct_categories[j]
          catname <-
            paste0(f, "_", j, "_", gsub("([[:punct:]])|\\s+", "_", catval))
          if (f == "subcat_n") {
            # number of times this particular category value appears
            grouped_values[
              data_field_dt[
                ,
                list("value" = sum(values == catval, na.rm = TRUE)),
                by = list(timepoint_group)
              ],
              (catname) := value,
              by = .EACHI
            ]
          } else if (f == "subcat_perc") {
            # percentage this particular category value appears out of number of records
            # include all values in denominator, including NA and NaN
            grouped_values[
              data_field_dt[
                ,
                list("value" = 100 * sum(values == catval, na.rm = TRUE) / length(values)),
                by = list(timepoint_group)
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
      grouped_values[
        data_field_dt[
          ,
          list("value" = sum(format(values, format = "%T") == "00:00:00", na.rm = TRUE)),
          by = list(timepoint_group)
        ],
        (f) := value,
        by = .EACHI
      ]
    } else if (f == "midnight_perc") {
      # percentage of values whose time portion is midnight (used to check for missing time portions)
      #   out of number of values present
      grouped_values[
        data_field_dt[
          ,
          list("value" = 100 * sum(format(values, format = "%T") == "00:00:00", na.rm = TRUE) / length(values[!is.na(values)])),
          by = list(timepoint_group)
        ],
        (f) := value,
        by = .EACHI
      ]
      # NOTE: if n is zero, the above returns NaN. Update to NA instead
      grouped_values[is.nan(get(f)), (f) := NA_real_]
    } else if (f == "min") {
      # minimum value, whether numeric or datetime. Excludes NAs
      # NOTE: min/max return warnings when all values are NA, so need to suppress them
      grouped_values[
        data_field_dt[
          ,
          list("value" = suppressWarnings(min(values, na.rm = TRUE))),
          by = list(timepoint_group)
        ],
        (f) := value,
        by = .EACHI
      ]
      # min/max return Inf when all values are NA. Update to NA instead
      grouped_values[is.infinite(get(f)), (f) := NA_real_]
      # min/max also drops datetime class so preserve datatypes
      if (inherits(data_field[["values"]][[1]], "POSIXct")) {
        # TODO: make sure this is consistent with data format on loading
        grouped_values[, (f) := as.POSIXct(get(f), tz = "UTC", origin = "1970-01-01")]
      }
    } else if (f == "max") {
      # maximum value, whether numeric or datetime. Excludes NAs
      # NOTE: min/max return warnings when all values are NA, so need to suppress them
      grouped_values[
        data_field_dt[
          ,
          list("value" = suppressWarnings(max(values, na.rm = TRUE))),
          by = list(timepoint_group)
        ],
        (f) := value,
        by = .EACHI
      ]
      # min/max return Inf when all values are NA. Update to NA instead
      grouped_values[is.infinite(get(f)), (f) := NA_real_]
      # min/max also drops datetime class so preserve datatypes
      if (inherits(data_field[["values"]][[1]], "POSIXct")) {
        # TODO: make sure this is consistent with data format on loading
        grouped_values[, (f) := as.POSIXct(get(f), tz = "UTC", origin = "1970-01-01")]
      }
    } else if (f == "mean") {
      # mean value. Excludes NAs
      grouped_values[
        data_field_dt[
          ,
          list("value" = mean(values, na.rm = TRUE)),
          by = list(timepoint_group)
        ],
        (f) := value,
        by = .EACHI
      ]
      # NOTE: mean returns NaN when all values are NA. Update to NA instead
      grouped_values[is.nan(get(f)), (f) := NA_real_]
    } else if (f == "median") {
      # median value. Excludes NAs
      grouped_values[
        data_field_dt[
          ,
          list("value" = stats::median(values, na.rm = TRUE)),
          by = list(timepoint_group)
        ],
        (f) := value,
        by = .EACHI
      ]
    } else if (f == "min_length") {
      # minimum character length
      # NOTE: min/max return warnings when all values are NA, so need to suppress them
      # NOTE: need the as.double() because min/max returns integer if all values
      # are NA (in the group), and if a mixture of doubles and integers are
      # returned, data.table doesn't like it (though the error only seems to appear
      # when using the package and not when testing inside the package itself)
      grouped_values[
        data_field_dt[
          ,
          list("value" = suppressWarnings(as.double(
            min(
              nchar(as.character(values),
                keepNA = TRUE
              ),
              na.rm = TRUE
            )
          ))),
          by = list(timepoint_group)
        ],
        (f) := value,
        by = .EACHI
      ]
      # min/max return Inf when all values are NA. Update to NA instead
      grouped_values[is.infinite(get(f)), (f) := NA_real_]
    } else if (f == "max_length") {
      # maximum character length
      # NOTE: min/max return warnings when all values are NA, so need to suppress them
      # NOTE: need the as.double() because min/max returns integer if all values
      # are NA (in the group), and if a mixture of doubles and integers are
      # returned, data.table doesn't like it (though the error only seems to appear
      # when using the package and not when testing inside the package itself)
      grouped_values[
        data_field_dt[
          ,
          list("value" = suppressWarnings(as.double(
            max(
              nchar(as.character(values),
                keepNA = TRUE
              ),
              na.rm = TRUE
            )
          ))),
          by = list(timepoint_group)
        ],
        (f) := value,
        by = .EACHI
      ]
      # min/max return Inf when all values are NA. Update to NA instead
      grouped_values[is.infinite(get(f)), (f) := NA_real_]
    } else if (f == "mean_length") {
      # mean character length
      grouped_values[
        data_field_dt[
          ,
          list("value" = mean(nchar(as.character(values), keepNA = TRUE),
            na.rm = TRUE
          )),
          by = list(timepoint_group)
        ],
        (f) := value,
        by = .EACHI
      ]
      # NOTE: mean returns NaN when all values are NA. Update to NA instead
      grouped_values[is.nan(get(f)), (f) := NA_real_]
    } else {
      # TODO: Decide if this should stop everything or just raise a warning
      # TODO: Putting it here means it doesn't get called if all values are NA
      stop(paste("Unrecognised aggregation type:", f),
        call. = FALSE
      )
    }
  }

  log_message(paste0("Finished"), show_progress)

  structure(
    list(
      values = grouped_values,
      function_list = function_list,
      field_type = data_field$field_type,
      column_name = data_field$column_name
    ),
    class = "daiquiri_aggregated_field"
  )
}


# -----------------------------------------------------------------------------
#' Test if object is an aggregated_field object
#'
#' @param x object to test
#' @return Logical
#' @noRd
is_aggregated_field <- function(x) inherits(x, "daiquiri_aggregated_field")


# -----------------------------------------------------------------------------
#' Constructor for the [ALL_FIELDS_COMBINED] aggregated_field object
#'
#' Uses results from already-aggregated individual fields rather than doing it
#' all again
#'
#' @param agg_fields all aggregated_field objects from data (i.e. excluding
#'   calculated agg_fields)
#' @param show_progress Print progress to console
#' @return aggregated_field object, including a data.table where the first column
#'   is the timepoint group, then one column per aggregationfunction
#' @noRd
# TODO: do we want to include duplicates in here too?
# TODO: this field has a numeric datatype whereas individual fields have an int
# datatype, decide if need to make them all the same
aggregate_combined_fields <- function(agg_fields,
                                      show_progress = TRUE) {
  # initialise known column names to prevent R CMD check notes
  n <- missing_n <- nonconformant_n <- NULL

  ft <- ft_allfields()
  function_list <- ft$aggregation_functions

  grouped_values <- agg_fields[[1]][["values"]][, 1]

  for (i in seq_along(agg_fields)) {
    for (j in 2:length(names(agg_fields[[i]][["values"]]))) {
      # TODO: ideally want to use the aggregation_functions list from the allfields
      # field_type rather than hard code
      if (names(agg_fields[[i]][["values"]])[j] %in% c("n", "missing_n", "nonconformant_n")) {
        f <- names(agg_fields[[i]][["values"]])[j]
        if (f %in% names(grouped_values)) {
          # If all values are NA then leave as NA,
          # but if any values are not NA then ignore the NAs (per timepoint)
          grouped_values[
            ,
            (f) := data.table::fifelse(
              is.na(get(f)) & is.na(agg_fields[[i]][["values"]][, get(f)]),
              NA_integer_,
              rowSums(cbind(get(f), agg_fields[[i]][["values"]][, get(f)]),
                na.rm = TRUE
              )
            )
          ]
        } else {
          grouped_values[, (f) := agg_fields[[i]][["values"]][, get(f)]]
        }
      }
    }
  }
  # if there are no datetime or numeric fields, nonconformant_n field needs to be
  # created explicitly
  if (!("nonconformant_n" %in% names(grouped_values))) {
    grouped_values[, "nonconformant_n" := data.table::fifelse(n == 0, NA_integer_, 0)]
  }

  grouped_values[, "missing_perc" := 100 * missing_n / (n + missing_n + nonconformant_n)]
  grouped_values[, "nonconformant_perc" := 100 * nonconformant_n / (n + missing_n + nonconformant_n)]

  log_message(paste0("Finished"), show_progress)

  structure(
    list(
      values = grouped_values,
      function_list = function_list,
      field_type = ft,
      column_name = "[ALL_FIELDS_COMBINED]"
    ),
    class = "daiquiri_aggregated_field"
  )
}


# -----------------------------------------------------------------------------
#' Allocate timepoint values to appropriate day/week/month etc. for later grouping
#'
#' @param x vector of original timepoint values
#' @param aggregation_timeunit desired aggregation granularity
#' @return vector of new timepoint values, same length as before but discretized values
#' @noRd
timepoint_as_timepoint_group <- function(x, aggregation_timeunit) {
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
#' Create sequence of timepoint values that will form the x-axes
#'
#' Need to ensure have all possible timepoint values, even if they are missing in the dataset
#'
#' @param timepoint_field data_field specified as the timepoint
#' @param aggregation_timeunit day/week/month/quarter/year
#'
#' @return single column data.table containing one row per timepoint_group
#' @noRd
create_timepoint_groups <- function(timepoint_field, aggregation_timeunit) {
  min_timepoint <-
    timepoint_as_timepoint_group(
      data_field_min(timepoint_field),
      aggregation_timeunit
    )
  max_timepoint <-
    timepoint_as_timepoint_group(
      data_field_max(timepoint_field),
      aggregation_timeunit
    )
  timepoint_groups <-
    data.table::data.table(seq(min_timepoint, max_timepoint, by = aggregation_timeunit))

  names(timepoint_groups) <-
    paste0(
      gsub("[^a-zA-Z0-9_]", "_", timepoint_field$column_name),
      "_by",
      aggregation_timeunit
    )

  timepoint_groups
}


# -----------------------------------------------------------------------------
# TODO: Define set of allowed aggregation functions similarly to field_types,
# with each object containing formula for aggregation as well as friendly names

#' Set user-friendly names for agg_funs
#'
#' This uses the agg_field column_names rather than the original
#' aggregationfunction (relevant for subcats)
#'
#' @param agg_fun string name of agg_fun (from agg_field column_name)
#' @param type "short" or "long"
#' @return string containing friendly name
#' @noRd
# TODO: come up with some friendlier short names, currently just the agg_fun itself
agg_fun_friendly_name <- function(agg_fun, type) {
  if (startsWith(agg_fun, "subcat_")) {
    catval <- substring(gsub("^(?:[^_]*_){3}", "_", agg_fun), 2)
    if (startsWith(agg_fun, "subcat_n")) {
      switch(type,
        short = agg_fun,
        long = paste0("No. of values in the category: ", catval)
      )
    } else if (startsWith(agg_fun, "subcat_perc")) {
      switch(type,
        short = agg_fun,
        long = paste0("Percentage of values in the category: ", catval)
      )
    }
  } else {
    switch(agg_fun,
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
      min_length = {
        switch(type,
          short = "min_length",
          long = "Minimum string length"
        )
      },
      max_length = {
        switch(type,
          short = "max_length",
          long = "Maximum string length"
        )
      },
      mean_length = {
        switch(type,
          short = "mean_length",
          long = "Mean string length"
        )
      }
    )
  }
}
