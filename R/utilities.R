# Generic functions

#' Check all required params have been passed in to the calling function
#'
#' Should be called at start of all exported functions to check user has
#' supplied all required arguments If any are missing, execution is stopped
#'
#' @param call call from the function being checked
#' @noRd
validate_params_required <- function(call) {
  # get the required arguments from function definition
  params_defined <- formals(as.character(call[[1]]))
  params_required <- names(which(vapply(params_defined, is.symbol, logical(1))))
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
  params_defined <- names(formals(as.character(call[[1]])))
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
    } else if (params_names[i] == "fieldtypes") {
      if (!is.fieldtypes(params_passed[[i]])) {
        err_validation <- append(
          err_validation,
          paste0(
            params_names[i],
            ": Expected a fieldtypes specification but found: [ class = ",
            class(params_passed[[i]]),
            "; contents = ",
            substr(toString(params_passed[[i]]), 1, 100),
            "]"
          )
        )
      }
    } else if (params_names[i] == "default_fieldtype") {
      if (!is.fieldtype(params_passed[[i]])) {
        err_validation <- append(
          err_validation,
          paste0(
            params_names[i],
            ": Expected a fieldtype but found: [ class = ",
            class(params_passed[[i]]),
            "; contents = ",
            substr(toString(params_passed[[i]]), 1, 100),
            "]"
          )
        )
      }
    } else if (params_names[i] == "sourcedata") {
      if (!is.sourcedata(params_passed[[i]])) {
        err_validation <- append(
          err_validation,
          paste0(
            params_names[i],
            ": Expected a sourcedata object but found: [ class = ",
            class(params_passed[[i]]),
            "; contents = ",
            substr(toString(params_passed[[i]]), 1, 100),
            "]"
          )
        )
      }
    } else if (params_names[i] == "aggregatedata") {
      if (!is.aggregatedata(params_passed[[i]])) {
        err_validation <- append(
          err_validation,
          paste0(
            params_names[i],
            ": Expected a aggregatedata object but found: [ class = ",
            class(params_passed[[i]]),
            "; contents = ",
            substr(toString(params_passed[[i]]), 1, 100),
            "]"
          )
        )
      }
    } else if (params_names[i] %in% c("override_columnnames", "showprogress")) {
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
    } else if (params_names[i] %in% c("save_fileprefix")) {
      if (grepl("[^a-zA-Z0-9_-]", params_passed[[i]])) {
        # NOTE: this is very restrictive and I'm not sure how it works in different locales
        err_validation <- append(
          err_validation,
          paste0(
            params_names[i],
            ": Invalid fileprefix: ",
            params_passed[[i]],
            ". Fileprefix can only contain alphanumeric, '-', and '_' characters"
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
    } else if (params_names[i] %in% c("dataset_shortdesc")) {
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
    } else if (params_names[i] %in% c("format", "save_filetype")) {
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


# -----------------------------------------------------------------------------
# logging functions
#' Initialise log file
#'
#' Choose a directory in which to save the log file. If this is not called, no
#' log file is created.
#'
#' @param log_directory String containing directory to save log file
#' @return Character string containing the full path to the newly-created log file
#' @examples
#' logname <- initialise_log(".")
#' \dontshow{
#' close_log()
#' file.remove(logname)
#' }
#' @export
initialise_log <- function(log_directory) {
  validate_params_required(match.call())
  validate_params_type(match.call(),
    log_directory = log_directory
  )

  filenameandpath <-
    file.path(
      log_directory,
      paste0(utils::packageName(), "_", format(Sys.time(), "%Y%m%d%_%H%M%S"), ".log")
    )

  if (file.create(filenameandpath)) {
    # need to save absolute path so that logging continues correctly from report_htmldoc.Rmd
    packageenvironment$logname <- normalizePath(filenameandpath)

    log_message(paste(
      "Log file initialised.",
      "Package version",
      utils::packageVersion(utils::packageName()), ";",
      R.Version()$version.string
    ))
  } else {
    stop("Log file [", filenameandpath, "] could not be created")
  }

  packageenvironment$logname
}


#' Closes any active log file
#'
#' @return If a log file was found, the path to the log file that was closed,
#'   otherwise an empty string
#' @examples close_log()
#' @export
close_log <- function() {
  if (exists("logname", envir = packageenvironment)) {
    log_message("Log file closed")
    logname <- packageenvironment$logname
    rm("logname", envir = packageenvironment)
  } else {
    logname <- ""
  }
  logname
}

#' Write message to log file (if it exists) and/or print to console
#'
#' @param message message to write
#' @param showprogress Print message to console
#' @noRd
# TODO: decide if showprogress should be a global setting e.g. package option ( options("mypkg-myval"=3) )
log_message <- function(message, showprogress = FALSE) {
  if (exists("logname", envir = packageenvironment)) {
    if (file.access(packageenvironment$logname, mode = 2) == -1) {
      stop(paste0("Cannot write to log file [", packageenvironment$logname, "].
									Message not logged: ", message))
    } else {
      log_con <- file(packageenvironment$logname, open = "a")
      writeLines(paste(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), ":", message),
        con = log_con
      )
      close(log_con)
    }
  }
  if (showprogress) cat(message, "\n")
}

#' Log entry to function
#'
#' @param functionname functionname to be written
#' @noRd
log_function_start <- function(functionname) {
  log_message(paste("[START FUNCTION]", functionname), FALSE)
}

#' Log exit from function
#'
#' @param functionname functionname to be written
#' @noRd
log_function_end <- function(functionname) {
  log_message(paste("[END FUNCTION]", functionname), FALSE)
}

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
  err <- structure(list(
    message = message,
    call = call,
    ...
  ),
  class = c(.subclass, "error", "condition")
  )
  stop(err)
}

######################################################
# dummy functions set up purely for unit testing.
# testthat can't find them when they are defined in the test_xxx files

#' Dummy function to assist unit testing of validate_params_required()
#'
#' @param p1 required param
#' @param p2 required param
#' @param p3 optional param
#' @noRd
testfn_params_required <- function(p1, p2, p3 = NULL) {
  validate_params_required(match.call())
}

#' Dummy function to assist unit testing of validate_params_type()
#'
#' This should contain every parameter defined in an exported function
#'
#' @noRd
testfn_params_type <- function(df,
                               fieldtypes,
                               sourcedata,
                               aggregatedata,
                               override_columnnames = FALSE,
                               na = c("", "NA", "NULL"),
                               dataset_shortdesc = "shortdesc",
                               aggregation_timeunit = "day",
                               save_directory = ".",
                               save_filename = "filename",
                               showprogress = TRUE,
                               log_directory = NULL,
                               format = "html",
                               save_filetype = "csv",
                               save_fileprefix = "",
                               default_fieldtype = ft_ignore()) {
  if (missing(df)) {
    df <- data.frame("Fieldname" = 123)
  }
  if (missing(fieldtypes)) {
    fieldtypes <- daiquiri::fieldtypes(Col_tp = ft_timepoint())
  }
  if (missing(sourcedata)) {
    sourcedata <- structure(list(datafields = NA), class = "sourcedata")
  }
  if (missing(aggregatedata)) {
    aggregatedata <-
      structure(list(datafields = NA), class = "aggregatedata")
  }

  validate_params_type(
    match.call(),
    df = df,
    fieldtypes = fieldtypes,
    override_columnnames = override_columnnames,
    na = na,
    dataset_shortdesc = dataset_shortdesc,
    aggregation_timeunit = aggregation_timeunit,
    save_directory = save_directory,
    save_filename = save_filename,
    showprogress = showprogress,
    log_directory = log_directory,
    sourcedata = sourcedata,
    aggregatedata = aggregatedata,
    format = format,
    save_filetype = save_filetype,
    save_fileprefix = save_fileprefix,
    default_fieldtype = default_fieldtype
  )
}


######################################################

#' Dummy function set up purely for R CMD Check Namespace bug
#'
#' Addresses R CMD Check NOTE: Namespace in Imports field not imported from: ‘reactable’
#' In some situations the check misses the fact that reactable is used in the Rmd file in ./inst/rmd
#' Call it here to stop the NOTE
#' @noRd
dummy_reactable_call <- function() {
  x <- reactable::colDef()
}
