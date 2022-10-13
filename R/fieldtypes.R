# Create field specification (don't know whether to call them fields or columns)
#
# Three options:
# user passes in a specification for the entire dataset, every field is required
# User passes in a specification for only the fields they explicitly want to include (by name), ignoring all others
# we try to guess the field types (based on all or only a subset of the rows?)
#
# NOTE: want classes to inherit, e.g. all fieldtypes (except "ignored" ones should implement a function for checking missing data)
#
# copying structure used in readr

# -----------------------------------------------------------------------------
# SPECIFY ALLOWABLE TYPES

#' Constructor for individual fieldtype object
#'
#' @param type string denoting the fieldtype
#' @param collector readr `collector' to use when parsing the data
#' @param dataclass type of data, e.g. character or POSIXct
#' @param aggfunctions aggfunctions to apply to the data in this field
#' @param options additional options for certain aggfunctions
#' @return fieldtype object
#' @noRd
# TODO: decide whether to require all aggfunctions to be supplied or automatically include the basic ones
fieldtype <- function(type,
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
		class = c(paste0("fieldtype_", type), "fieldtype")
	)
}

#' Test if object is a fieldtype object
#'
#' @param x object to test
#' @return Logical
#' @noRd
is.fieldtype <- function(x) inherits(x, "fieldtype")

#' Test if object is a timepoint fieldtype
#'
#' @param x object to test
#' @return Logical
#' @noRd
is.fieldtype_timepoint <- function(x) inherits(x, "fieldtype_timepoint")

#' Test if object is a ignore fieldtype
#'
#' @param x object to test
#' @return Logical
#' @noRd
is.fieldtype_ignore <- function(x) inherits(x, "fieldtype_ignore")

#' Test if object is a datetime fieldtype
#'
#' @param x object to test
#' @return Logical
#' @noRd
is.fieldtype_datetime <- function(x) inherits(x, "fieldtype_datetime")

#' Test if object is a numeric fieldtype
#'
#' @param x object to test
#' @return Logical
#' @noRd
is.fieldtype_numeric <- function(x) inherits(x, "fieldtype_numeric")

#' Test if object is a calculated fieldtype
#'
#' @param x object to test
#' @return Logical
#' @noRd
is.fieldtype_calculated <- function(x) inherits(x, c("fieldtype_allfields", "fieldtype_duplicates"))


#' Available fieldtypes
#'
#' Each column in the source dataset must be assigned to a particular `ft_xx`
#' depending on the type of data that it contains. This is done through a
#' [fieldtypes()] specification.
#' @seealso [fieldtypes()]
#' @name availablefieldtypes
#' @return A `fieldtype` object denoting the type of data in the column
#' @examples fts <- fieldtypes(PatientID = ft_uniqueidentifier(),
#'                             TestID = ft_ignore(),
#'                             TestDate = ft_timepoint(),
#'                             TestName = ft_categorical(aggregate_by_each_category = FALSE),
#'                             TestResult = ft_numeric(),
#'                             ResultDate = ft_datetime(),
#'                             ResultComment = ft_freetext(),
#'                             Location = ft_categorical())
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
#' @rdname availablefieldtypes
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
	fieldtype(
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
#' @rdname availablefieldtypes
#' @export
ft_uniqueidentifier <- function() {
	# TODO: potential additional aggfunctions: proportion numeric; distinct first chars
  fieldtype(type = "uniqueidentifier",
  					collector = readr::col_character(),
  					dataclass = "character",
  					aggfunctions = c("n",
  													 "missing_n",
  													 "missing_perc",
  													 "minlength",
  													 "maxlength",
  													 "meanlength")
            )

}

#' @section Details: `ft_categorical()` - identifies data fields which should
#'   be treated as categorical.
#' @param aggregate_by_each_category If `TRUE`, aggregated values will be
#'   generated for each distinct subcategory as well as for the field overall.
#'   If `FALSE`, aggregated values will only be generated for the field overall.
#'   Default = `FALSE`
#' @rdname availablefieldtypes
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
	fieldtype(type = "categorical",
  					collector = readr::col_character(),
  					dataclass = "character",
  					aggfunctions = aggfn,
  					options = options
  )
}

#' @section Details:
#' `ft_numeric()` - identifies data fields which contain numeric values that should be treated as continuous.
#' Any values which contain non-numeric characters (including grouping marks) will be classed as non-conformant
#' @rdname availablefieldtypes
#' @export
ft_numeric <- function() {
  fieldtype(type = "numeric",
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
#' @rdname availablefieldtypes
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
	fieldtype(
		type = "datetime",
		collector = readr::col_datetime(format = format),
		dataclass = "POSIXct",
		aggfunctions = aggfn,
		options = options
	)
}

#' @section Details: `ft_freetext()` - identifies data fields which contain
#'   free text values. Only presence/missingness will be evaluated.
#' @rdname availablefieldtypes
#' @export
ft_freetext <- function() {
	fieldtype(
		type = "freetext",
		collector = readr::col_character(),
		dataclass = "character",
		aggfunctions = c("n", "missing_n", "missing_perc")
	)
}

#' @section Details: `ft_simple()` - identifies data fields where you only
#'   want presence/missingness to be evaluated (but which are not necessarily
#'   free text).
#' @rdname availablefieldtypes
#' @export
ft_simple <- function() {
	fieldtype(
		type = "simple",
		collector = readr::col_character(),
		dataclass = "character",
		aggfunctions = c("n", "missing_n", "missing_perc")
	)
}

#' @section Details: `ft_ignore()` - identifies data fields which should be
#'   ignored.  These will not be loaded.
#' @rdname availablefieldtypes
#' @export
ft_ignore <- function() {
	fieldtype(
		type = "ignore",
		collector = readr::col_skip(),
		dataclass = "NULL"
	)
}

# this is an internal fieldtype for calculating stats across all fields combined and should not be set explicitly by user
ft_allfields <- function() {
	fieldtype(
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

# this is an internal fieldtype for calculating duplicates and should not be set explicitly by user
ft_duplicates <- function() {
	fieldtype(
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
#' Create fieldtypes specification
#'
#' Specify the names and types of fields in the source data frame. This is
#' important because the data in each field will be aggregated in different
#' ways, depending on its `fieldtype`.  See [availablefieldtypes]
#' @param ... names and types of fields (columns) in source data.
#' @return A `fieldtypes` object
#' @examples fts <- fieldtypes(PatientID = ft_uniqueidentifier(),
#'                             TestID = ft_ignore(),
#'                             TestDate = ft_timepoint(),
#'                             TestName = ft_categorical(aggregate_by_each_category = FALSE),
#'                             TestResult = ft_numeric(),
#'                             ResultDate = ft_datetime(),
#'                             ResultComment = ft_freetext(),
#'                             Location = ft_categorical())
#' @seealso [availablefieldtypes()]
#' @export
fieldtypes <- function(...) {
	fts <- list(...)

	# validate - collect all errors together and return only once
	err_validation <- character()
	is_fieldtype <- vapply(fts, is.fieldtype, logical(1))
	if (any(!is_fieldtype)) {
		err_validation <-
			append(
				err_validation,
				paste(
					"Unrecognised fieldtype(s) in positions: [",
					paste(which(!is_fieldtype), collapse = ", "),
					"]",
					"names: [",
					paste(names(fts)[which(!is_fieldtype)], collapse = ", "),
					"]"
				)
			)
	}
	is_timepoint <- vapply(fts, is.fieldtype_timepoint, logical(1))
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
			append(err_validation,
						 paste(
						 	"Duplicate column names not allowed: [",
						 	paste(names(fts)[duplicated(names(fts))], collapse = ", "),
						 	"]"
						 ))
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
			.subclass = "invalid_fieldtypes",
			message = paste0(
				"Invalid `fieldtypes' specification.\n",
				paste(err_validation, collapse = "\n")
			)
		)
	}

	structure(fts, class = "fieldtypes")
}

is.fieldtypes <- function(x) inherits(x, "fieldtypes")

fieldtypes_to_string <- function(fieldtypes) {
	s <- ""
	for (ft in seq_along(fieldtypes)) {
		s <-
			paste0(s, names(fieldtypes[ft]), "\t", "<", class(fieldtypes[[ft]])[1], ">")
		if (!is.null(fieldtypes[[ft]]$options)) {
			s <- paste0(s, "\t", "options: ", fieldtypes[[ft]]$options)
		}
		s <- paste0(s, "\n")
	}
	s
}

#' @export
print.fieldtypes <- function(x, ...) {
	cat(fieldtypes_to_string(x))
}

#' Print a template fieldtypes() specification to console
#'
#' Helper function to generate template code for a [fieldtypes()] specification,
#' based on the supplied data frame. All fields (columns) in the specification
#' will be defined using the `default_fieldtype`, and the console output can be
#' copied and edited before being used as input to [create_report()]()
#' or [prepare_data()]().
#'
#' @param df data frame including the column names for the template
#'   specification
#' @param default_fieldtype `fieldtype` to be used for each column. Default =
#'   [ft_ignore()]. See  [availablefieldtypes()]
#' @return (invisibly) Character string containing the template code
#' @examples
#' df <- data.frame(col1 = rep("2022-01-01", 5), col2 = rep(1, 5), col3 = 1:5, col4 = rnorm(5))
#'
#' print_fieldtypes_template(df, default_fieldtype = ft_numeric())
#' @seealso [fieldtypes()]
#' @export
print_fieldtypes_template <- function(df, default_fieldtype = ft_ignore()) {
	validate_params_required(match.call())
	validate_params_type(match.call(),
											 df = df,
											 default_fieldtype = default_fieldtype)

	fieldnames <- names(df)
	template_string <-
		paste(
			"fieldtypes(",
			paste0(
				"\"",
				fieldnames,
				"\"",
				" = ft_",
				default_fieldtype$type,
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

#' Map fieldtypes to parser's coltypes
#'
#' Parser used is readr. See readr::col_types
#'
#' @param fieldtypes fieldtypes object
#' @param alltostring Set to TRUE if want parser to read everything as character
#' @return list of coltypes
#' @noRd
fieldtypes_to_cols <-
	function(fieldtypes, alltostring = FALSE) {
		# validate
		if (missing(fieldtypes) || !is.fieldtypes(fieldtypes)) {
			stop("Invalid parameter(s) supplied:",
					 "`fieldtypes'", call. = FALSE)
		}
		if (alltostring == TRUE) {
			do.call(readr::cols, lapply(fieldtypes, function(x) {
				readr::col_character()
			}))
		}
		else{
			do.call(readr::cols, lapply(fieldtypes, get_collector))
		}
	}

#' Get the fieldtype's collector
#'
#' @param fieldtype fieldtype object
#' @return coltype as appropriate to the chosen parser (readr)
#' @noRd
get_collector <- function(fieldtype) {
	fieldtype$collector
}

#' Get the fieldtype's dataclass
#'
#' @param fieldtype fieldtype object
#' @return type of data e.g. character or POSIXct
#' @noRd
get_dataclass <- function(fieldtype) {
	fieldtype$dataclass
}

#' Get the fieldtype's type as a string
#'
#' @param fieldtype fieldtype object
#' @return string
#' @noRd
get_fieldtype_name <- function(fieldtype) {
	fieldtype$type
}

#' Get the fieldtype's aggfunctions
#'
#' @param fieldtype fieldtype object
#' @return vector of aggfunctions
#' @noRd
get_aggfunctions <- function(fieldtype) {
	fieldtype$aggfunctions
}
