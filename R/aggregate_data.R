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
                           show_progress = TRUE,
                           stratify_by = NULL) {
  # TODO: allow user to override existing aggregation_functions?
  # TODO: raise an error/warning if data is less granular than aggregation_timeunit

  log_function_start(match.call()[[1]])

  validate_params_required(match.call())
  validate_params_type(match.call(),
    source_data = source_data,
    aggregation_timeunit = aggregation_timeunit,
    show_progress = show_progress,
    stratify_by = stratify_by
  )

  # TODO: validate stratify_by

  log_message(
    paste0("Aggregating [", source_data$sourcename, "] by [", aggregation_timeunit, "]..."),
    show_progress
  )

  # create column to group by, which will become the x-axes
  timepoint_group_sequence <- create_timepoint_groups(
    timepoint_field = source_data$data_fields[[source_data$timepoint_field_name]],
    aggregation_timeunit = aggregation_timeunit
  )

  # map timepoint field values to timepoint_group_sequence values.
  # these will be used repeatedly later
  timepoint_field_as_timepoint_group <- timepoint_as_timepoint_group(
    data_field_vector(source_data$data_fields[[source_data$timepoint_field_name]]),
    aggregation_timeunit = aggregation_timeunit
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
        timepoint_field_as_timepoint_group,
        timepoint_group_sequence,
        show_progress = show_progress
      )
  }
  log_message(paste0("Aggregating calculated fields..."), show_progress)
  log_message(paste0("[DUPLICATES]:"), show_progress)
  agg_fields[[source_data$cols_imported_n + 1]] <-
    aggregate_field(
      source_data$data_fields[[source_data$cols_source_n + 1]],
      timepoint_field_as_timepoint_group,
      timepoint_group_sequence,
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

  ### AGGREGATE STRATIFIED DATASET
  if (!is.null(stratify_by)) {
    log_message(paste0("Stratifying dataset by [", stratify_by, "] field..."), show_progress)
    stratify_by_field_values <- source_data$data_fields[[stratify_by]]$values
    # load aggregated data into new vector
    log_message(paste0("Aggregating each data_field in turn..."), show_progress)
    agg_fields_stratified <- vector("list", source_data$cols_imported_n - 1)
    for (i in 1:source_data$cols_imported_n) {
      fieldname <- names(source_data$cols_imported_indexes)[i]
      if( fieldname != stratify_by){
        log_message(paste0(i, ": ", names(source_data$cols_imported_indexes)[i]), show_progress)
        fieldindex <- source_data$cols_imported_indexes[[i]]
        agg_fields_stratified[[i]] <-
          aggregate_field_stratified(
            data_field = source_data$data_fields[[fieldindex]],
            stratify_by_field_values,
            timepoint_field_as_timepoint_group,
            timepoint_group_sequence,
            show_progress = show_progress
          )
          names(agg_fields_stratified)[i] <- fieldname
        }
    }
  }

  log_function_end(match.call()[[1]])

  structure(
    list(
      aggregated_fields = agg_fields,
      timepoint_field_name = source_data$timepoint_field_name,
      # not sure if this should be set at overall object level or allow it to
      # differ per aggregated_field
      aggregation_timeunit = aggregation_timeunit,
      dataset_description = source_data$dataset_description,
      stratified_by = stratify_by,
      aggregated_fields_stratified = agg_fields_stratified
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
#' @examples
#' \donttest{
#' raw_data <- read_data(
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
#' @param timepoint_field_as_timepoint_group all values in timepoint field
#'   transformed to correct timepoint group
#' @param timepoint_group_sequence complete set of x-axis values
#' @param show_progress Print progress to console
#' @return aggregated_field object, including a data.table where the first column
#'   is the timepoint group, then one column per aggregation function
#' @noRd
aggregate_field <- function(data_field,
                            timepoint_field_as_timepoint_group,
                            timepoint_group_sequence,
                            show_progress = TRUE) {

  log_message(paste0("Preparing..."), show_progress)
  function_list <- data_field$field_type$aggregation_functions

  log_message(
    paste0("Aggregating ", data_field_basetype(data_field), " field..."),
    show_progress
  )

  # this contains all values present in the original data_field, alongside their timepoint_group
  data_field_dt <-
    data.table::data.table(
      "timepoint_group" = timepoint_field_as_timepoint_group,
      "values" = data_field[["values"]][[1]],
      key = "timepoint_group"
    )
  # this will contain the agg_fun values after aggregating (one column per agg_fun)
  grouped_values <- data.table::as.data.table(timepoint_group_sequence)
  data.table::setkey(grouped_values)

  # append values for each aggregation function in turn
  for (i in seq_along(function_list)) {
    f <- function_list[i]
    log_message(paste0("  By ", f), show_progress)
    aggregate_and_append_values(aggregation_function = f,
                                data_field_dt = data_field_dt,
                                grouped_values = grouped_values,
                                show_progress = show_progress)
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
#' Constructor for an individual aggregated_field_stratified object
#'
#' @param data_field data_field object
#' @param timepoint_field_as_timepoint_group all values in timepoint field
#'   transformed to correct timepoint group
#' @param timepoint_group_sequence complete set of x-axis values
#' @param show_progress Print progress to console
#' @return aggregated_field object, including a data.table where the first column
#'   is the timepoint group, then one column per aggregation function
#' @noRd
aggregate_field_stratified <- function(data_field,
                                        stratify_by_field_values,
                                        timepoint_field_as_timepoint_group,
                                        timepoint_group_sequence,
                                        show_progress = TRUE) {

  log_message(paste0("Preparing..."), show_progress)
  function_list <- data_field$field_type$aggregation_functions

  log_message(
    paste0("Aggregating ", data_field_basetype(data_field), " field..."),
    show_progress
  )

  # this contains all values present in the original data_field, alongside their timepoint_group and stratify_by group
  data_field_dt <-
    data.table::data.table(
      "timepoint_group" = timepoint_field_as_timepoint_group,
      "stratify_by_group" = stratify_by_field_values[[1]],
      "values" = data_field[["values"]][[1]],
      key = "timepoint_group"
    )
  # this will contain the agg_fun values after aggregating (one column per agg_fun)
  grouped_values <- data.table::CJ(timepoint_group_sequence[[1]],
                                   stratify_by_field_values[[1]],
                                   sorted = TRUE,
                                   unique = TRUE)
  data.table::setnames(grouped_values,
                       new = c(names(timepoint_group_sequence),
                               names(stratify_by_field_values))
                       )

  # append values for each aggregation function in turn
  for (i in seq_along(function_list)) {
    f <- function_list[i]
    log_message(paste0("  By ", f), show_progress)
    aggregate_and_append_values(aggregation_function = f,
                                data_field_dt = data_field_dt,
                                grouped_values = grouped_values,
                                stratify = TRUE,
                                show_progress = show_progress)
  }

  log_message(paste0("Finished"), show_progress)

  structure(
    list(
      values = grouped_values,
      function_list = function_list,
      field_type = data_field$field_type,
      column_name = data_field$column_name,
      stratify_by_field_name = names(stratify_by_field_values)
    ),
    class = "daiquiri_aggregated_field_stratified"
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
#' Get user-friendly names for agg_funs
#'
#' This uses the agg_field column_names rather than the original
#' aggregationfunction (relevant for subcats)
#'
#' @param agg_fun_colname string name of agg_fun from agg_field column_name
#' @param type "short" or "long"
#' @return string containing friendly name
#' @noRd
# TODO: come up with some friendlier short names, currently just the agg_fun itself
agg_fun_friendly_name <- function(agg_fun_colname, type) {
  if (startsWith(agg_fun_colname, "subcat_")) {
    catval <- agg_fun_subcat_value(agg_fun_colname)
    agg_fun_type <- agg_fun_subcat_type(agg_fun_colname)
    agg_fun <- agg_fun_from_type(type = agg_fun_type)
    switch(type,
      short = agg_fun$friendly_name_short,
      long = paste0(agg_fun$friendly_name_long, ": ", catval)
      )
  } else {
    agg_fun <- agg_fun_from_type(type = agg_fun_colname)
    switch(type,
      short = agg_fun$friendly_name_short,
      long = agg_fun$friendly_name_long
      )
  }
}

#' Get the subcategory value from the agg_fun_colname
#'
#' This uses the agg_field column_names rather than the original
#' aggregationfunction
#'
#' @param agg_fun_colname (vector of) string name of agg_fun (from agg_field column_name)
#' @return string containing subcat value
#' @noRd
agg_fun_subcat_value <- function(agg_fun_colname) {
  substring(gsub("^(?:[^_]*_){3}", "_", agg_fun_colname), 2)
}

#' Get the subcat type from the agg_fun_colname
#'
#' This uses the agg_field column_names rather than the original
#' aggregationfunction. Bit ugly but works so long as there are only two subcat types
#'
#' @param agg_fun_colname (vector of) string name of agg_fun (from agg_field column_name)
#' @return string containing the subcat type
#' @noRd
agg_fun_subcat_type <- function(agg_fun_colname) {
  ifelse(grepl("subcat_n", agg_fun_colname),
         "subcat_n",
         "subcat_perc")
}
