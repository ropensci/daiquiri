# =============================================================================
# FUNCTIONS FOR LOGGING

# -----------------------------------------------------------------------------
#' Initialise a log file
#'
#' Choose a directory in which to save the log file. If this is not called, no
#' log file is created.
#'
#' @param log_directory String containing directory to save log file
#' @return Character string containing the full path to the newly-created log
#'   file
#' @examples
#' log_name <- initialise_log(".")
#'
#' log_name
#' \dontshow{
#' close_log()
#' file.remove(log_name)
#' }
#' @export
initialise_log <- function(log_directory) {
  validate_params_required(match.call())
  validate_params_type(match.call(),
    log_directory = log_directory
  )

  file_and_path <-
    file.path(
      log_directory,
      paste0(utils::packageName(), "_", format(Sys.time(), "%Y%m%d%_%H%M%S"), ".log")
    )

  if (file.create(file_and_path)) {
    # need to save absolute path so that logging continues correctly from report_htmldoc.Rmd
    package_environment$log_name <- normalizePath(file_and_path)

    log_message(paste(
      "Log file initialised.",
      "Package version",
      utils::packageVersion(utils::packageName()), ";",
      R.Version()$version.string
    ))
  } else {
    stop("Log file [", file_and_path, "] could not be created")
  }

  package_environment$log_name
}


# -----------------------------------------------------------------------------
#' Close any active log file
#'
#' @return If a log file was found, the path to the log file that was closed,
#'   otherwise an empty string
#' @examples close_log()
#' @export
close_log <- function() {
  if (exists("log_name", envir = package_environment)) {
    log_message("Log file closed")
    log_name <- package_environment$log_name
    rm("log_name", envir = package_environment)
  } else {
    log_name <- ""
  }
  log_name
}


# -----------------------------------------------------------------------------
#' Write message to log file (if it exists) and/or print to console
#'
#' @param message message to write
#' @param show_progress Print message to console
#' @noRd
# TODO: decide if show_progress should be a global setting e.g. package option ( options("mypkg-myval"=3) )
log_message <- function(message, show_progress = FALSE) {
  if (exists("log_name", envir = package_environment)) {
    if (file.access(package_environment$log_name, mode = 2) == -1) {
      stop(paste0("Cannot write to log file [", package_environment$log_name, "].
									Message not logged: ", message))
    } else {
      log_con <- file(package_environment$log_name, open = "a")
      writeLines(paste(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), ":", message),
        con = log_con
      )
      close(log_con)
    }
  }
  if (show_progress) cat(message, "\n")
}


# -----------------------------------------------------------------------------
#' Log entry to function
#'
#' @param function_name function_name to be written
#' @noRd
log_function_start <- function(function_name) {
  log_message(paste("[START FUNCTION]", function_name), FALSE)
}

#' Log exit from function
#'
#' @param function_name function_name to be written
#' @noRd
log_function_end <- function(function_name) {
  log_message(paste("[END FUNCTION]", function_name), FALSE)
}


# =============================================================================
# FUNCTIONS FOR PARAMETER VALIDATION

# -----------------------------------------------------------------------------
#' Check all required params have been passed in to the calling function
#'
#' Should be called at start of all exported functions to check user has
#' supplied all required arguments If any are missing, execution is stopped
#'
#' @param call call from the function being checked
#' @noRd
validate_params_required <- function(call) {
  # get the required arguments from function definition
  params_defined <-
    formals(utils::tail(as.character(call[[1]]), n = 1))
  params_required <- names(which(vapply(params_defined, is.symbol, logical(1))))
  # exclude ... param
  params_required <- params_required[which(params_required != "...")]
  # get the arguments passed into the parent call
  params_passed <- names(as.list(call)[-1])

  if (any(!params_required %in% params_passed)) {
    stop_custom(
      .subclass = "invalid_param_missing",
      message = paste(
        "Required argument(s) missing:",
        paste(setdiff(params_required, params_passed),
          collapse = ", "
        )
      )
    )
  }
}


# -----------------------------------------------------------------------------
#' Check all params that have been passed in to the calling function are of
#' correct type/class
#'
#' Should be called at start of all exported functions to check user has
#' supplied all arguments correctly Any that are invalid are collated and then
#' execution is stopped
#'
#' @param call call from the function being checked
#' @param ... the parameters that were actually passed into the function being
#'   checked, with names
#' @noRd
validate_params_type <- function(call, ...) {
  params_defined <-
    names(formals(utils::tail(as.character(call[[1]]), n = 1)))
  # exclude ... param
  params_defined <- params_defined[which(params_defined != "...")]
  params_passed <- list(...)
  params_names <- names(params_passed)

  # check internal usage is correct
  if (length(which(params_names != "")) != length(params_passed)) {
    stop_custom(
      .subclass = "invalid_call",
      message = "Invalid call for function. Params must be passed in with names"
    )
  }
  if (!setequal(params_defined, params_names)) {
    stop_custom(
      .subclass = "invalid_call",
      message = paste0(
        "Invalid call for function. Different set of params in parent function
        definition than were passed in to validate_params_type().",
        "\n",
        "In validate_params_type() but not in parent function: ",
        paste(setdiff(params_names, params_defined), collapse = ", "),
        "\n",
        "In parent function but not in validate_params_type(): ",
        paste(setdiff(params_defined, params_names), collapse = ", ")
      )
    )
  }

  # validate user-supplied params - collect all errors together and return only once
  err_validation <- character()
  for (i in seq_along(params_names)) {
    if (params_names[i] == "df") {
      if (!is.data.frame(params_passed[[i]])) {
        err_validation <- append(
          err_validation,
          paste0(
            params_names[i],
            ": Expected a data frame but found: [ class = ",
            class(params_passed[[i]]),
            "; contents = ",
            substr(toString(params_passed[[i]]), 1, 100),
            "]."
          )
        )
      }
    } else if (params_names[i] == "field_types") {
      if (!is_field_types(params_passed[[i]])) {
        err_validation <- append(
          err_validation,
          paste0(
            params_names[i],
            ": Expected a field_types specification but found: [ class = ",
            class(params_passed[[i]]),
            "; contents = ",
            substr(toString(params_passed[[i]]), 1, 100),
            "]"
          )
        )
      }
    } else if (params_names[i] == "default_field_type") {
      if (!is_field_type(params_passed[[i]])) {
        err_validation <- append(
          err_validation,
          paste0(
            params_names[i],
            ": Expected a field_type but found: [ class = ",
            class(params_passed[[i]]),
            "; contents = ",
            substr(toString(params_passed[[i]]), 1, 100),
            "]"
          )
        )
      }
    } else if (params_names[i] == "source_data") {
      if (!is_source_data(params_passed[[i]])) {
        err_validation <- append(
          err_validation,
          paste0(
            params_names[i],
            ": Expected a source_data object but found: [ class = ",
            class(params_passed[[i]]),
            "; contents = ",
            substr(toString(params_passed[[i]]), 1, 100),
            "]"
          )
        )
      }
    } else if (params_names[i] == "aggregated_data") {
      if (!is_aggregated_data(params_passed[[i]])) {
        err_validation <- append(
          err_validation,
          paste0(
            params_names[i],
            ": Expected a aggregated_data object but found: [ class = ",
            class(params_passed[[i]]),
            "; contents = ",
            substr(toString(params_passed[[i]]), 1, 100),
            "]"
          )
        )
      }
    } else if (params_names[i] %in% c("override_column_names", "show_progress")) {
      if (!is.logical(params_passed[[i]])) {
        err_validation <- append(
          err_validation,
          paste0(
            params_names[i],
            ": Expected TRUE/FALSE but found: [ class = ",
            class(params_passed[[i]]),
            "; contents = ",
            substr(toString(params_passed[[i]]), 1, 100),
            "]"
          )
        )
      }
    } else if (params_names[i] %in% c("save_directory")) {
      if (!is.character(params_passed[[i]]) ||
        !dir.exists(params_passed[[i]])) {
        err_validation <- append(
          err_validation,
          paste0(
            params_names[i],
            ": Directory not found: [ class = ",
            class(params_passed[[i]]),
            "; contents = ",
            substr(toString(params_passed[[i]]), 1, 255),
            "]"
          )
        )
      }
    } else if (params_names[i] %in% c("log_directory")) {
      if (!is.null(params_passed[[i]]) &&
        (!is.character(params_passed[[i]]) ||
          !dir.exists(params_passed[[i]]))) {
        err_validation <- append(
          err_validation,
          paste0(
            params_names[i],
            ": Directory not found: [ class = ",
            class(params_passed[[i]]),
            "; contents = ",
            substr(toString(params_passed[[i]]), 1, 255),
            "]"
          )
        )
      }
    } else if (params_names[i] %in% c("save_filename")) {
      if (!is.null(params_passed[[i]]) &&
        (grepl("[^a-zA-Z0-9_-]", params_passed[[i]]) ||
          nchar(params_passed[[i]]) == 0)) {
        # NOTE: this is very restrictive and I'm not sure how it works in different locales
        err_validation <- append(
          err_validation,
          paste0(
            params_names[i],
            ": Invalid filename: ",
            params_passed[[i]],
            ". Filename can only contain alphanumeric, '-', and '_' characters, and should not include the file extension."
          )
        )
      }
    } else if (params_names[i] %in% c("save_file_prefix")) {
      if (grepl("[^a-zA-Z0-9_-]", params_passed[[i]])) {
        # NOTE: this is very restrictive and I'm not sure how it works in different locales
        err_validation <- append(
          err_validation,
          paste0(
            params_names[i],
            ": Invalid file_prefix: ",
            params_passed[[i]],
            ". file_prefix can only contain alphanumeric, '-', and '_' characters"
          )
        )
      }
    } else if (params_names[i] %in% c("aggregation_timeunit")) {
      if (length(params_passed[[i]]) != 1 ||
        !(params_passed[[i]] %in% c("day", "week", "month", "quarter", "year"))) {
        err_validation <- append(
          err_validation,
          paste0(
            params_names[i],
            ": Values allowed are: day, week, month, quarter, year but found: [ class = ",
            class(params_passed[[i]]),
            "; contents = ",
            substr(toString(params_passed[[i]]), 1, 100),
            "]"
          )
        )
      }
    } else if (params_names[i] %in% c("dataset_description", "report_title")) {
      if (!is.null(params_passed[[i]]) &&
        (!is.character(params_passed[[i]]) ||
          length(params_passed[[i]]) != 1)) {
        err_validation <- append(
          err_validation,
          paste0(
            params_names[i],
            ": Expected a character string but found: [ class = ",
            class(params_passed[[i]]),
            "; contents = ",
            substr(toString(params_passed[[i]]), 1, 500),
            "]"
          )
        )
      }
    } else if (params_names[i] %in% c("na")) {
      if (any(!is.character(params_passed[[i]]))) {
        err_validation <- append(
          err_validation,
          paste0(
            params_names[i],
            ": Expected a vector of character strings but found: [ class = ",
            class(params_passed[[i]]),
            "; contents = ",
            substr(toString(params_passed[[i]]), 1, 500),
            "]"
          )
        )
      }
    } else if (params_names[i] %in% c("format", "save_file_type")) {
      # ignore for now as currently dealt with inside function
    }
  }

  if (length(err_validation) > 0) {
    stop_custom(
      .subclass = "invalid_param_type",
      message = paste0(
        "Invalid argument(s) supplied.\n",
        paste(err_validation, collapse = "\n")
      )
    )
  }
}


# =============================================================================
# MISCELLANEOUS

# -----------------------------------------------------------------------------
#' Raise a custom error with a class that can be tested for
#'
#' Copied from https://adv-r.hadley.nz/conditions.html#custom-conditions
#'
#' @param .subclass category of error
#' @param message error message
#' @param call calling function
#' @param ... other items to pass to condition object
#' @noRd
stop_custom <- function(.subclass, message, call = NULL, ...) {
  err <- structure(
    list(
      message = message,
      call = call,
      ...
    ),
    class = c(.subclass, "error", "condition")
  )
  stop(err)
}


# -----------------------------------------------------------------------------
# DUMMY FUNCTIONS SET UP PURELY FOR UNIT TESTING
# testthat can't find them when they are defined in the test-xxx files

#' Dummy function to assist unit testing of validate_params_required()
#'
#' @param p1 required param
#' @param p2 required param
#' @param p3 optional param
#' @param ... optional param
#' @noRd
testfn_params_required <- function(p1, p2, p3 = NULL, ...) {
  validate_params_required(match.call())
}

#' Dummy function to assist unit testing of validate_params_type()
#'
#' This should contain every parameter defined in an exported function
#'
#' @noRd
testfn_params_type <- function(df,
                               field_types,
                               source_data,
                               aggregated_data,
                               override_column_names = FALSE,
                               na = c("", "NA", "NULL"),
                               dataset_description = "shortdesc",
                               aggregation_timeunit = "day",
                               report_title = "title",
                               save_directory = ".",
                               save_filename = "filename",
                               show_progress = TRUE,
                               log_directory = NULL,
                               format = "html",
                               save_file_type = "csv",
                               save_file_prefix = "",
                               default_field_type = ft_ignore()) {
  if (missing(df)) {
    df <- data.frame("Fieldname" = 123)
  }
  if (missing(field_types)) {
    field_types <- daiquiri::field_types(Col_tp = ft_timepoint())
  }
  if (missing(source_data)) {
    source_data <- structure(list(data_fields = NA), class = "daiquiri_source_data")
  }
  if (missing(aggregated_data)) {
    aggregated_data <-
      structure(list(data_fields = NA), class = "daiquiri_aggregated_data")
  }

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
    log_directory = log_directory,
    source_data = source_data,
    aggregated_data = aggregated_data,
    format = format,
    save_file_type = save_file_type,
    save_file_prefix = save_file_prefix,
    default_field_type = default_field_type
  )
}

#' Helper function for preparing test data()
#'
#' @param testvalues vector containing alternate values for timepoint and field value
#' @param field_type field_type of field value
#' @param add_uid_field add a uid field so that the rows don't get deduped
#'
#' @return source_data
#' @noRd
prepare_data_testhelper <- function(
    testvalues,
    field_type,
    add_uid_field = FALSE
){

  df <-
    data.table::data.table(
      "col_timepoint" = testvalues[seq(from = 1, to = length(testvalues) - 1, by = 2)],
      "col_values" = testvalues[seq(from = 2, to = length(testvalues), by = 2)]
    )
  fts <- field_types(
    col_timepoint = ft_timepoint(),
    col_values = field_type
  )

  if(add_uid_field){
    df[, "col_uid" := seq_along(df$col_timepoint)]
    fts <- field_types(
      col_timepoint = ft_timepoint(),
      col_uid = ft_simple(),
      col_values = field_type
    )
  }

  prepare_data(
    df,
    field_types = fts,
    show_progress = FALSE
  )
}

#' Helper function for testing aggregate_and_append_values()
#'
#' @param testvalues vector containing alternate values for timepoint and field value
#' @param field_type field_type of field value
#' @param aggregation_function aggregation_function being tested
#' @param aggregation_timeunit aggregation_timeunit
#' @param field_offset add 1/2 to return values from duplicates/allfields calculated fields
#' @param add_uid_field add a uid field so that the rows don't get deduped
#'
#' @return grouped_values data.table
#' @noRd
aggregate_and_append_values_testhelper <- function(
    testvalues,
    field_type,
    aggregation_function,
    aggregation_timeunit,
    field_offset = 0,
    add_uid_field = FALSE
){

  source_data <-
    prepare_data_testhelper(
      testvalues = testvalues,
      field_type = field_type,
      add_uid_field = add_uid_field
    )

  #--- this part copied from aggregate_data() ---#
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
  #--- ---#

  if(add_uid_field){
    field_offset <- field_offset + 1
  }
  data_field <- source_data$data_fields[[2 + field_offset]]

  #--- this part copied from aggregate_field() ---#
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
  #--- ---#

  aggregate_and_append_values(aggregation_function,
                              data_field_dt = data_field_dt,
                              grouped_values = grouped_values,
                              show_progress = FALSE)

  grouped_values
}

# -----------------------------------------------------------------------------
#' Dummy functions set up purely for R CMD Check Namespace bug
#'
#' Addresses R CMD Check NOTE: Namespace in Imports field not imported from: ‘package’
#' In some situations the check misses the fact that the 'package' is used in the Rmd file in ./inst/rmd
#' Call it here to stop the NOTE
#' @noRd
dummy_package_call <- function() {
  reactable::colDef
  xfun::base64_uri
}
