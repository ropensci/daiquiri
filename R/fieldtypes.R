# Create field specification (don't know whether to call them fields or columns)
#
# NOTE: ideally want classes to inherit, e.g. all field_types (except "ignored"
# ones should implement a function for checking missing data)
#
# copying structure used in readr

# -----------------------------------------------------------------------------
# SPECIFY ALLOWABLE TYPES

#' Constructor for individual field_type object
#'
#' @param type string denoting the field_type
#' @param collector readr `collector' to use when parsing the data
#' @param dataclass type of data, e.g. character or POSIXct
#' @param aggfunctions aggfunctions to apply to the data in this field
#' @param options additional options for certain aggfunctions
#' @return field_type object
#' @noRd
# TODO: decide whether to require all aggfunctions to be supplied or automatically include the basic ones
field_type <- function(type,
                       collector,
                       dataclass,
                       aggfunctions = c("n", "missing_n", "missing_perc"),
                       options = NULL) {
  structure(
    list(
      type = type,
      collector = collector,
      dataclass = dataclass,
      aggfunctions = aggfunctions,
      options = options
    ),
    class = c(paste0("field_type_", type), "field_type")
  )
}

#' Test if object is a field_type object
#'
#' @param x object to test
#' @return Logical
#' @noRd
is.field_type <- function(x) inherits(x, "field_type")

#' Test if object is a timepoint field_type
#'
#' @param x object to test
#' @return Logical
#' @noRd
is.field_type_timepoint <- function(x) inherits(x, "field_type_timepoint")

#' Test if object is a ignore field_type
#'
#' @param x object to test
#' @return Logical
#' @noRd
is.field_type_ignore <- function(x) inherits(x, "field_type_ignore")

#' Test if object is a datetime field_type
#'
#' @param x object to test
#' @return Logical
#' @noRd
is.field_type_datetime <- function(x) inherits(x, "field_type_datetime")

#' Test if object is a numeric field_type
#'
#' @param x object to test
#' @return Logical
#' @noRd
is.field_type_numeric <- function(x) inherits(x, "field_type_numeric")

#' Test if object is a calculated field_type
#'
#' @param x object to test
#' @return Logical
#' @noRd
is.field_type_calculated <- function(x) inherits(x, c("field_type_allfields", "field_type_duplicates"))


#' Types of data fields available for specification
#'
#' Each column in the source dataset must be assigned to a particular `ft_xx`
#' depending on the type of data that it contains. This is done through a
#' [field_types()] specification.
#' @seealso [field_types()]
#' @name field_types_available
#' @return A `field_type` object denoting the type of data in the column
#' @examples fts <- field_types(
#'   PatientID = ft_uniqueidentifier(),
#'   TestID = ft_ignore(),
#'   TestDate = ft_timepoint(),
#'   TestName = ft_categorical(aggregate_by_each_category = FALSE),
#'   TestResult = ft_numeric(),
#'   ResultDate = ft_datetime(),
#'   ResultComment = ft_freetext(),
#'   Location = ft_categorical()
#' )
NULL

#' @section Details: `ft_timepoint()` - identifies the data field which
#'   should be used as the independent time variable. There should be one and
#'   only one of these specified.
#' @param includes_time If `TRUE`, additional aggregated values will be generated
#'   using the time portion (and if no time portion is present then midnight
#'   will be assumed). If `FALSE`, aggregated values will ignore any time portion.
#'   Default = `TRUE`
#' @param format Where datetime values are not in the format `YYYY-MM-DD` or
#'   `YYYY-MM-DD HH:MM:SS`, an alternative format can be specified at the per
#'   field level, using [readr::col_datetime()] format
#'   specifications, e.g. `format = "%d/%m/%Y"`. When a format is
#'   supplied, it must match the complete string.
#' @rdname field_types_available
#' @export
ft_timepoint <- function(includes_time = TRUE,
                         format = "") {
  # NOTE: nonconformant values appear in validation warnings
  aggfn <- c("n")
  options <- NULL
  if (includes_time) {
    aggfn <- c(aggfn, "midnight_n", "midnight_perc")
    # TODO: can probably do something more sophisticated here with match.call()
    options <- "includes_time"
  }
  field_type(
    type = "timepoint",
    collector = readr::col_datetime(format = format),
    dataclass = "POSIXct",
    aggfunctions = aggfn,
    options = options
  )
}

#' @section Details: `ft_uniqueidentifier()` - identifies data fields which
#'   contain a (usually computer-generated) identifier for an entity, e.g. a
#'   patient. It does not need to be unique within the dataset.
#' @rdname field_types_available
#' @export
ft_uniqueidentifier <- function() {
  # TODO: potential additional aggfunctions: proportion numeric; distinct first chars
  field_type(
    type = "uniqueidentifier",
    collector = readr::col_character(),
    dataclass = "character",
    aggfunctions = c(
      "n",
      "missing_n",
      "missing_perc",
      "minlength",
      "maxlength",
      "meanlength"
    )
  )
}

#' @section Details: `ft_categorical()` - identifies data fields which should
#'   be treated as categorical.
#' @param aggregate_by_each_category If `TRUE`, aggregated values will be
#'   generated for each distinct subcategory as well as for the field overall.
#'   If `FALSE`, aggregated values will only be generated for the field overall.
#'   Default = `FALSE`
#' @rdname field_types_available
#' @export
ft_categorical <- function(aggregate_by_each_category = FALSE) {
  # TODO: allow more options for aggregate_by_each_category, e.g. topx (bysize), or accept a vector of values
  aggfn <- c("n", "missing_n", "missing_perc", "distinct")
  options <- NULL
  if (aggregate_by_each_category) {
    aggfn <- c(aggfn, "subcat_n", "subcat_perc")
    # TODO: can probably do something more sophisticated here with match.call()
    options <- "aggregate_by_each_category"
  }
  field_type(
    type = "categorical",
    collector = readr::col_character(),
    dataclass = "character",
    aggfunctions = aggfn,
    options = options
  )
}

#' @section Details:
#' `ft_numeric()` - identifies data fields which contain numeric values that should be treated as continuous.
#' Any values which contain non-numeric characters (including grouping marks) will be classed as non-conformant
#' @rdname field_types_available
#' @export
ft_numeric <- function() {
  field_type(
    type = "numeric",
    collector = readr::col_double(),
    dataclass = "numeric",
    aggfunctions = c(
      "n",
      "missing_n",
      "missing_perc",
      "nonconformant_n",
      "nonconformant_perc",
      "min",
      "max",
      "mean",
      "median"
    )
  )
}

#' @section Details: `ft_datetime()` - identifies data fields which contain
#'   date values that should be treated as continuous.
#' @param includes_time If `TRUE`, additional aggregated values will be generated
#'   using the time portion (and if no time portion is present then midnight
#'   will be assumed). If `FALSE`, aggregated values will ignore any time portion.
#'   Default = `TRUE`
#' @param format Where datetime values are not in the format `YYYY-MM-DD` or
#'   `YYYY-MM-DD HH:MM:SS`, an alternative format can be specified at the per
#'   field level, using [readr::col_datetime()] format
#'   specifications, e.g. `format = "%d/%m/%Y"`. When a format is
#'   supplied, it must match the complete string.
#' @rdname field_types_available
#' @export
ft_datetime <- function(includes_time = TRUE,
                        format = "") {
  aggfn <-
    c(
      "n",
      "missing_n",
      "missing_perc",
      "nonconformant_n",
      "nonconformant_perc",
      "min",
      "max"
    )
  options <- NULL
  if (includes_time) {
    aggfn <- c(aggfn, "midnight_n", "midnight_perc")
    # TODO: can probably do something more sophisticated here with match.call()
    options <- "includes_time"
  }
  field_type(
    type = "datetime",
    collector = readr::col_datetime(format = format),
    dataclass = "POSIXct",
    aggfunctions = aggfn,
    options = options
  )
}

#' @section Details: `ft_freetext()` - identifies data fields which contain
#'   free text values. Only presence/missingness will be evaluated.
#' @rdname field_types_available
#' @export
ft_freetext <- function() {
  field_type(
    type = "freetext",
    collector = readr::col_character(),
    dataclass = "character",
    aggfunctions = c("n", "missing_n", "missing_perc")
  )
}

#' @section Details: `ft_simple()` - identifies data fields where you only
#'   want presence/missingness to be evaluated (but which are not necessarily
#'   free text).
#' @rdname field_types_available
#' @export
ft_simple <- function() {
  field_type(
    type = "simple",
    collector = readr::col_character(),
    dataclass = "character",
    aggfunctions = c("n", "missing_n", "missing_perc")
  )
}

#' @section Details: `ft_ignore()` - identifies data fields which should be
#'   ignored.  These will not be loaded.
#' @rdname field_types_available
#' @export
ft_ignore <- function() {
  field_type(
    type = "ignore",
    collector = readr::col_skip(),
    dataclass = "NULL"
  )
}

# this is an internal field_type for calculating stats across all fields combined and should not be set explicitly by user
ft_allfields <- function() {
  field_type(
    type = "allfields",
    collector = readr::col_skip(),
    dataclass = "NULL",
    aggfunctions = c(
      "n",
      "missing_n",
      "missing_perc",
      "nonconformant_n",
      "nonconformant_perc"
    )
  )
}

# this is an internal field_type for calculating duplicates and should not be set explicitly by user
ft_duplicates <- function() {
  field_type(
    type = "duplicates",
    collector = readr::col_skip(),
    dataclass = "NULL",
    aggfunctions = c("sum", "nonzero_perc")
  )
}

# -----------------------------------------------------------------------------
# OVERALL COLLECTION OF TYPES TODO: consider allowing simple string
# specifications, cf `concise' specification from readr need to think about how
# that would work if want to allow user to define more complicated
# specifications though
#' Create field_types specification
#'
#' Specify the names and types of fields in the source data frame. This is
#' important because the data in each field will be aggregated in different
#' ways, depending on its `field_type`.  See [field_types_available]
#' @param ... names and types of fields (columns) in source data.
#' @return A `field_types` object
#' @examples fts <- field_types(
#'   PatientID = ft_uniqueidentifier(),
#'   TestID = ft_ignore(),
#'   TestDate = ft_timepoint(),
#'   TestName = ft_categorical(aggregate_by_each_category = FALSE),
#'   TestResult = ft_numeric(),
#'   ResultDate = ft_datetime(),
#'   ResultComment = ft_freetext(),
#'   Location = ft_categorical()
#' )
#' @seealso [field_types_available()]
#' @export
field_types <- function(...) {
  fts <- list(...)

  # validate - collect all errors together and return only once
  err_validation <- character()
  is_field_type <- vapply(fts, is.field_type, logical(1))
  if (any(!is_field_type)) {
    err_validation <-
      append(
        err_validation,
        paste(
          "Unrecognised field_type(s) in positions: [",
          paste(which(!is_field_type), collapse = ", "),
          "]",
          "names: [",
          paste(names(fts)[which(!is_field_type)], collapse = ", "),
          "]"
        )
      )
  }
  is_timepoint <- vapply(fts, is.field_type_timepoint, logical(1))
  if (sum(is_timepoint) != 1) {
    err_validation <-
      append(
        err_validation,
        paste(
          "Must specify one and only one timepoint field. Timepoints currently in positions: [",
          paste(which(is_timepoint), collapse = ", "),
          "]",
          "names: [",
          paste(names(fts)[which(is_timepoint)], collapse = ", "),
          "]"
        )
      )
  }
  if (anyDuplicated(names(fts)) > 0) {
    err_validation <-
      append(
        err_validation,
        paste(
          "Duplicate column names not allowed: [",
          paste(names(fts)[duplicated(names(fts))], collapse = ", "),
          "]"
        )
      )
  }
  # check for reserved names
  if (any(names(fts) %in% c("[DUPLICATES]", "[ALLFIELDSCOMBINED]"))) {
    err_validation <-
      append(
        err_validation,
        paste(
          "'[DUPLICATES]' and '[ALLFIELDSCOMBINED]' are names reserved for calculated columns.
					Please rename these columns in your data."
        )
      )
  }
  if (length(err_validation) > 0) {
    stop_custom(
      .subclass = "invalid_field_types",
      message = paste0(
        "Invalid `field_types' specification.\n",
        paste(err_validation, collapse = "\n")
      )
    )
  }

  structure(fts, class = "field_types")
}

is.field_types <- function(x) inherits(x, "field_types")

field_types_to_string <- function(field_types) {
  s <- ""
  for (ft in seq_along(field_types)) {
    s <-
      paste0(s, names(field_types[ft]), "\t", "<", class(field_types[[ft]])[1], ">")
    if (!is.null(field_types[[ft]]$options)) {
      s <- paste0(s, "\t", "options: ", field_types[[ft]]$options)
    }
    s <- paste0(s, "\n")
  }
  s
}

#' @export
print.field_types <- function(x, ...) {
  cat(field_types_to_string(x))
}

#' Print a template field_types() specification to console
#'
#' Helper function to generate template code for a [field_types()] specification,
#' based on the supplied data frame. All fields (columns) in the specification
#' will be defined using the `default_field_type`, and the console output can be
#' copied and edited before being used as input to [create_report()]()
#' or [prepare_data()]().
#'
#' @param df data frame including the column names for the template
#'   specification
#' @param default_field_type `field_type` to be used for each column. Default =
#'   [ft_ignore()]. See  [field_types_available()]
#' @return (invisibly) Character string containing the template code
#' @examples
#' df <- data.frame(col1 = rep("2022-01-01", 5), col2 = rep(1, 5), col3 = 1:5, col4 = rnorm(5))
#'
#' print_field_types_template(df, default_field_type = ft_numeric())
#' @seealso [field_types()]
#' @export
print_field_types_template <- function(df, default_field_type = ft_ignore()) {
  validate_params_required(match.call())
  validate_params_type(match.call(),
    df = df,
    default_field_type = default_field_type
  )

  fieldnames <- names(df)
  template_string <-
    paste(
      "field_types(",
      paste0(
        "\"",
        fieldnames,
        "\"",
        " = ft_",
        default_field_type$type,
        "()",
        ifelse(fieldnames == rev(fieldnames)[1], "", ","),
        collapse = "\n\t"
      ),
      ")"
    )
  cat(template_string)
  invisible(template_string)
}

# -----------------------------------------------------------------------------
# HELPER FUNCTIONS

#' Map field_types to parser's coltypes
#'
#' Parser used is readr. See readr::col_types
#'
#' @param field_types field_types object
#' @param alltostring Set to TRUE if want parser to read everything as character
#' @return list of coltypes
#' @noRd
field_types_to_cols <-
  function(field_types, alltostring = FALSE) {
    # validate
    if (missing(field_types) || !is.field_types(field_types)) {
      stop("Invalid parameter(s) supplied:",
        "`field_types'",
        call. = FALSE
      )
    }
    if (alltostring == TRUE) {
      do.call(readr::cols, lapply(field_types, function(x) {
        readr::col_character()
      }))
    } else {
      do.call(readr::cols, lapply(field_types, get_collector))
    }
  }

#' Get the field_type's collector
#'
#' @param field_type field_type object
#' @return coltype as appropriate to the chosen parser (readr)
#' @noRd
get_collector <- function(field_type) {
  field_type$collector
}

#' Get the field_type's dataclass
#'
#' @param field_type field_type object
#' @return type of data e.g. character or POSIXct
#' @noRd
get_dataclass <- function(field_type) {
  field_type$dataclass
}

#' Get the field_type's type as a string
#'
#' @param field_type field_type object
#' @return string
#' @noRd
get_field_type_name <- function(field_type) {
  field_type$type
}

#' Get the field_type's aggfunctions
#'
#' @param field_type field_type object
#' @return vector of aggfunctions
#' @noRd
get_aggfunctions <- function(field_type) {
  field_type$aggfunctions
}
