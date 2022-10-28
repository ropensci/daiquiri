#' Create a data quality report from a data frame
#'
#' Accepts record-level data from a data frame, validates it against the
#' expected type of content of each column, generates a collection of time
#' series plots for visual inspection, and saves a report to disk.
#'
#' @param df A data frame. Rectangular data can be read from file using
#'   [read_data()]. See Details.
#' @param field_types [field_types()] object specifying names and types of
#'   fields (columns) in the supplied `df`. See also [field_types_available].
#' @param override_column_names If `FALSE`, column names in the supplied `df`
#'   must match the names specified in `field_types` exactly. If `TRUE`, column
#'   names in the supplied `df` will be replaced with the names specified in
#'   `field_types`. The specification must therefore contain the columns in the
#'   correct order. Default = `FALSE`
#' @param na vector containing strings that should be interpreted as missing
#'   values, Default = `c("","NA","NULL")`.
#' @param report_title Title to appear on the report
#' @param dataset_description Short description of the dataset being checked.
#'   This will appear on the report. If blank, the name of the data frame object
#'   will be used
#' @param aggregation_timeunit Unit of time to aggregate over. Specify one of
#'   `"day"`, `"week"`, `"month"`, `"quarter"`, `"year"`. The `"week"` option is
#'   Monday-based. Default = `"day"`
#' @param save_directory String specifying directory in which to save the
#'   report. Default is current directory.
#' @param save_filename String specifying filename for the report, excluding any
#'   file extension. If no filename is supplied, one will be automatically
#'   generated with the format `daiquiri_report_YYMMDD_HHMMSS`.
#' @param show_progress Print progress to console. Default = `TRUE`
#' @param log_directory String specifying directory in which to save log file.
#'   If no directory is supplied, progress is not logged.
#' @return A list containing information relating to the supplied parameters as
#'   well as the resulting `daiquiri_source_data` and `daiquiri_aggregated_data`
#'   objects.
#' @section Details: In order for the package to detect any non-conformant
#'   values in numeric or datetime fields, these should be present in the data
#'   frame in their raw character format. Rectangular data from a text file will
#'   automatically be read in as character type if you use the [read_data()]
#'   function. Data frame columns that are not of class character will still be
#'   processed according to the `field_types` specified.
#' @examples
#' \donttest{
#' # load example data into a data.frame
#' raw_data <- read_data(
#'   system.file("extdata", "example_prescriptions.csv", package = "daiquiri"),
#'   delim = ",",
#'   col_names = TRUE
#' )
#'
#' # create a report in the current directory
#' daiq_obj <- create_report(
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
#'   dataset_description = "Example data provided with package",
#'   aggregation_timeunit = "day",
#'   report_title = "daiquiri data quality report",
#'   save_directory = ".",
#'   save_filename = "example_data_report",
#'   show_progress = TRUE,
#'   log_directory = NULL
#' )
#' \dontshow{file.remove("./example_data_report.html")}
#' }
#'
#' @seealso [read_data()], [field_types()],
#'   [field_types_available()]
#' @export
create_report <- function(df,
                          field_types,
                          override_column_names = FALSE,
                          na = c("", "NA", "NULL"),
                          dataset_description = NULL,
                          aggregation_timeunit = "day",
                          report_title = "daiquiri data quality report",
                          save_directory = ".",
                          save_filename = NULL,
                          show_progress = TRUE,
                          log_directory = NULL) {

  # if a log directory is supplied, start a new log. Otherwise, close any existing log
  if (!is.null(log_directory)) {
    log_filename <- initialise_log(log_directory)
  } else {
    close_log()
    log_filename <- NULL
  }

  log_function_start(match.call()[[1]])

  # check params before running anything so that it fails sooner rather than later
  validate_params_required(match.call())
  validate_params_type(
    match.call(),
    df = df,
    field_types = field_types,
    override_column_names = override_column_names,
    na = na,
    dataset_description = dataset_description,
    aggregation_timeunit = aggregation_timeunit,
    report_title = report_title,
    save_directory = save_directory,
    save_filename = save_filename,
    show_progress = show_progress,
    log_directory = log_directory
  )

  source_data <-
    prepare_data(
      df,
      field_types,
      override_column_names = override_column_names,
      dataset_description = dataset_description,
      na = na,
      show_progress = show_progress
    )

  aggregated_data <-
    aggregate_data(source_data,
      aggregation_timeunit = aggregation_timeunit,
      show_progress = show_progress
    )

  reportfilename <-
    report_data(
      source_data,
      aggregated_data,
      report_title = report_title,
      save_directory = save_directory,
      save_filename = save_filename,
      show_progress = show_progress
    )

  log_function_end(match.call()[[1]])

  close_log()

  structure(
    list(
      dataset_description = source_data$dataset_description,
      field_types = field_types,
      override_column_names = override_column_names,
      na_values = na,
      aggregation_timeunit = aggregation_timeunit,
      report_filename = reportfilename,
      source_data = source_data,
      aggregated_data = aggregated_data,
      log_filename = log_filename
    ),
    class = "daiquiri_object"
  )
}


# -----------------------------------------------------------------------------
#' @export
print.daiquiri_object <- function(x, ...) {
  cat("Class: daiquiri_object\n")
  cat("Dataset:", x$source_data$dataset_description, "\n")
  cat("\n")
  cat("Columns in source:", x$source_data$cols_source_n, "\n")
  cat("Columns imported:", x$source_data$cols_imported_n, "\n")
  cat("Rows in source:", x$source_data$rows_source_n, "\n")
  cat("Duplicate rows removed:", x$source_data$rows_duplicates_n, "\n")
  cat("Rows imported:", x$source_data$rows_imported_n, "\n")
  cat("Column used for timepoint:", x$source_data$timepoint_field_name, "\n")
  cat("Rows missing timepoint values removed:", x$source_data$timepoint_missing_n, "\n")
  cat("Total validation warnings:", nrow(x$source_data$validation_warnings), "\n")
  cat("\n")

  agg_fields <- x$aggregated_data$aggregated_fields
  cat("Min timepoint value:", format(agg_fields[[1]]$values[[1]][1]), "\n")
  cat("Max timepoint value:", format(rev(agg_fields[[1]]$values[[1]])[1]), "\n")
  cat("Timepoint aggregation unit:", x$aggregated_data$aggregation_timeunit, "\n")
  cat(
    "Total number of timepoints:",
    length(agg_fields[[x$aggregated_data$timepoint_field_name]]$values[[1]]), "\n"
  )
  cat(
    "Number of empty timepoints:",
    sum(agg_fields[[x$aggregated_data$timepoint_field_name]]$values[["n"]] == 0), "\n"
  )
}


# -----------------------------------------------------------------------------
#' Read delimited data for optimal use with daiquiri
#'
#' Popular file readers such as `readr::read_delim()` perform datatype
#' conversion by default, which can interfere with daiquiri's ability to detect
#' non-conformant values. Use this function instead to ensure optimal
#' compatibility with daiquiri's features.
#'
#' Operates as a restricted implementation of [readr::read_delim()].
#'
#' @param file A string containing path of file containing data to load, or a
#'   URL starting `http://`, `file://`, etc. Compressed files with extension
#'   `.gz`, `.bz2`, `.xz` and `.zip` are supported.
#' @param delim Single character used to separate fields within a record. E.g.
#'   `","` or `"\t"`
#' @param col_names Either `TRUE`, `FALSE` or a character vector of column
#'   names. If `TRUE`, the first row of the input will be used as the column
#'   names, and will not be included in the data frame. If `FALSE`, column names
#'   will be generated automatically. Default = `TRUE`
#' @param quote Single character used to quote strings.
#' @param trim_ws Should leading and trailing whitespace be trimmed from each
#'   field?
#' @param comment A string used to identify comments. Any text after the comment
#'   characters will be silently ignored
#' @param skip Number of lines to skip before reading data. If `comment` is
#'   supplied any commented lines are ignored after skipping
#' @param n_max Maximum number of lines to read.
#' @param show_progress Display a progress bar? Default = `TRUE`
#' @return A data frame
#' @examples raw_data <- read_data(
#'   system.file("extdata", "example_prescriptions.csv", package = "daiquiri"),
#'   delim = ",",
#'   col_names = TRUE
#' )
#'
#' head(raw_data)
#' @seealso [field_types()], [field_types_available()],
#'   [aggregate_data()], [report_data()],
#'   [create_report()]
#' @export
read_data <- function(file,
                      delim = NULL,
                      col_names = TRUE,
                      quote = "\"",
                      trim_ws = TRUE,
                      comment = "",
                      skip = 0,
                      n_max = Inf,
                      show_progress = TRUE) {
  validate_params_required(match.call())
  # NOTE: let readr do its own param validation

  readr::read_delim(
    file,
    delim = delim,
    quote = quote,
    col_names = col_names,
    col_types = readr::cols(.default = "c"),
    col_select = NULL,
    na = character(),
    comment = comment,
    trim_ws = trim_ws,
    skip = skip,
    n_max = n_max,
    name_repair = "unique",
    progress = show_progress,
    skip_empty_rows = TRUE,
    lazy = TRUE
  )
}
