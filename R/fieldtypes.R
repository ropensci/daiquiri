# Create field specification (don't know whether to call them fields or columns)
#
# Three options:
# user passes in a specification for the entire dataset, every field is required
# User passes in a specification for only the fields they explicitly want to include (by name), ignoring all others
# we try to guess the field types (based on all or only a subset of the rows?)
#
# NOTE: want classes to inherit, e.g. all fieldtypes (except "ignored" ones should implement a function for checking missing data)
# if user specifies a factor type should we load it up as a factor or load it as a string and do factor-checking ourselves?
#
# copying structure used in readr

# -----------------------------------------------------------------------------
# SPECIFY ALLOWABLE TYPES

# `collector' is readr `collector'
# TODO: decide whether to require all aggfunctions to be supplied or automatically include the basic ones
fieldtype <- function(type, collector, dataclass, aggfunctions = c("n", "missing_n", "missing_perc"), options = NULL) {
	structure(list(type = type,
								 collector = collector,
								 dataclass = dataclass,
								 aggfunctions = aggfunctions,
  							 options = options),
            class = c(paste0("fieldtype_", type), "fieldtype")
            )
}

is.fieldtype <- function(x) inherits(x, "fieldtype")

is.fieldtype_timepoint <- function(x) inherits(x, "fieldtype_timepoint")

is.fieldtype_ignore <- function(x) inherits(x, "fieldtype_ignore")

is.fieldtype_datetime <- function(x) inherits(x, "fieldtype_datetime")

is.fieldtype_numeric <- function(x) inherits(x, "fieldtype_numeric")

# NOTE: Partitionfield functionality disabled until we work out how to present it
# is.fieldtype_partition <- function(x) inherits(x, "fieldtype_partition")

is.fieldtype_calculated <- function(x) inherits(x, c("fieldtype_allfields", "fieldtype_duplicates"))


#' Available fieldtypes
#'
#' Specify the type of data contained in each field of your dataset using a combination of the following
#' @seealso \code{\link{fieldtypes}}
#' @name availablefieldtypes
NULL

#' @section Details:
#' \code{ft_timepoint} - identifies the data field which should be used as the independent time variable.
#'   There should be one and only one of these specified.
#' @param includes_time If TRUE, additional aggregated values will be generated using the time portion (and if no time portion is present then midnight will be assumed). If FALSE, aggregated values will ignore any time portion. Default = TRUE
#' @param format Where datetime values are not in the format `YYYY-MM-DD` or `YYYY-MM-DD HH:MM:SS`, an alternative format can be specified at the per field level, using readr's \code{\link[readr]{col_datetime}} format specifications, e.g. \code{format = "\%d/\%m/\%Y"}.
#'  When a format is supplied, it must match the complete string.
#' @rdname availablefieldtypes
#' @export
ft_timepoint <- function( includes_time = TRUE, format = "" ) {
	# NOTE: nonconformant values appear in validation warnings
	aggfn <- c("n")
	options <- NULL
	if( includes_time ){
		aggfn <- c(aggfn, "midnight_n", "midnight_perc")
		# TODO: can probably do something more sophisticated here with match.call()
		options <- "includes_time"
	}
	fieldtype(type = "timepoint",
            collector = readr::col_datetime(format = format),
  					dataclass = "POSIXct",
						aggfunctions = aggfn,
						options = options
	)
}

#' @section Details:
#' \code{ft_uniqueidentifier} - identifies data fields which contain a (usually computer-generated) identifier for an entity, e.g. a patient.
#' It does not need to be unique within the dataset.
#' @rdname availablefieldtypes
#' @export
ft_uniqueidentifier <- function() {
	# TODO: potential additional aggfunctions: proportion numeric; distinct first chars
  fieldtype(type = "uniqueidentifier",
  					collector = readr::col_character(),
  					dataclass = "character",
  					aggfunctions = c("n", "missing_n", "missing_perc", "minlength", "maxlength", "meanlength")
            )

}

# NOTE: Partitionfield functionality disabled until we work out how to present it
# #' @section Details:
# #' \code{ft_partition} - identifies data fields which should be used to partition the data, such that each partition
# #'   should be considered separately, e.g. a field that designates that different data rows originated from distinct source systems.
# #' The data will be aggregated overall as well as by each partition.
# #' @rdname availablefieldtypes
# #' @export
#
# ft_partition <- function() {
#   fieldtype(type = "partition",
#   					collector = readr::col_character(),
#   					dataclass = "character",
#   					aggfunctions = c("n", "missing_n", "missing_perc", "distinct", "subcat_n", "subcat_perc")
#             )
# }

#' @section Details:
#' \code{ft_categorical} - identifies data fields which should be treated as categorical.
#' @param aggregate_by_each_category If TRUE, aggregated values will be generated for each distinct subcategory as well as for the field overall. If FALSE, aggregated values will only be generated for the field overall. Default = FALSE
#' @rdname availablefieldtypes
#' @export
ft_categorical <- function( aggregate_by_each_category = FALSE ) {
	# TODO: allow more options for aggregate_by_each_category, e.g. topx (bysize), or accept a vector of values
	aggfn <- c("n", "missing_n", "missing_perc", "distinct")
	options <- NULL
	if( aggregate_by_each_category ){
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
#' \code{ft_numeric} - identifies data fields which contain numeric values that should be treated as continuous.
#' Any values which contain non-numeric characters (including grouping marks) will be classed as non-conformant
#' @rdname availablefieldtypes
#' @export
ft_numeric <- function() {
  fieldtype(type = "numeric",
  					collector = readr::col_double(),
  					dataclass = "numeric",
  					aggfunctions = c("n", "missing_n", "missing_perc", "nonconformant_n", "nonconformant_perc", "min", "max", "mean", "median")
  )
}

#' @section Details:
#' \code{ft_datetime} - identifies data fields which contain date values that should be treated as continuous.
#' @param includes_time If TRUE, additional aggregated values will be generated using the time portion (and if
#' no time portion is present then midnight will be assumed). If FALSE, aggregated values will ignore any time portion. Default = TRUE
#' @param format Where datetime values are not in the format `YYYY-MM-DD` or `YYYY-MM-DD HH:MM:SS`,
#' an alternative format can be specified at the per field level, using readr's \code{\link[readr]{col_datetime}}
#' format specifications, e.g. \code{format = "\%d/\%m/\%Y"}. When a format is supplied, it must match the complete string.
#' @rdname availablefieldtypes
#' @export
ft_datetime <- function( includes_time = TRUE, format = "" ) {
	aggfn <- c("n", "missing_n", "missing_perc", "nonconformant_n", "nonconformant_perc", "min", "max")
	options <- NULL
	if( includes_time ){
		aggfn <- c(aggfn, "midnight_n", "midnight_perc")
		# TODO: can probably do something more sophisticated here with match.call()
		options <- "includes_time"
	}
	fieldtype(type = "datetime",
						collector = readr::col_datetime(format = format),
  					dataclass = "POSIXct",
						aggfunctions = aggfn,
						options = options
	)
}

#' @section Details:
#' \code{ft_freetext} - identifies data fields which contain free text values. Only presence/missingness will be evaluated.
#' @rdname availablefieldtypes
#' @export
ft_freetext <- function() {
  fieldtype(type = "freetext",
  					collector = readr::col_character(),
  					dataclass = "character",
  					aggfunctions = c("n", "missing_n", "missing_perc")
  )
}

# # try to guess the type
# ft_guess <- function() {
#   fieldtype("guess", readr::col_guess())
# }

#' @section Details:
#' \code{ft_simple} - identifies data fields where you only want presence/missingness to be evaluated (but which are not necessarily free text).
#' @rdname availablefieldtypes
#' @export
ft_simple <- function() {
  fieldtype(type = "simple",
  					collector = readr::col_character(),
  					dataclass = "character",
  					aggfunctions = c("n", "missing_n", "missing_perc")
  )
}

#' @section Details:
#' \code{ft_ignore} - identifies data fields which should be ignored.  These will not be loaded.
#' @rdname availablefieldtypes
#' @export
ft_ignore <- function() {
  fieldtype(type = "ignore",
  					collector = readr::col_skip(),
  					dataclass = "NULL"
  )
}

# this is an internal fieldtype for calculating stats across all fields combined and should not be set explicitly by user
ft_allfields <- function() {
	fieldtype(type = "allfields",
						collector = readr::col_skip(),
						dataclass = "NULL",
						aggfunctions = c("n", "missing_n", "missing_perc", "nonconformant_n", "nonconformant_perc")
	)
}

# this is an internal fieldtype for calculating duplicates and should not be set explicitly by user
ft_duplicates <- function() {
	fieldtype(type = "duplicates",
						collector = readr::col_skip(),
						dataclass = "NULL",
						aggfunctions = c("sum", "nonzero_perc")
	)
}

# -----------------------------------------------------------------------------
# OVERALL COLLECTION OF TYPES
# TODO: consider allowing simple string specifications, cf `concise' specification from readr
# need to think about how that would work if want to allow user to define more complicated specifications though
#' Create fieldtypes specification
#'
#' Specify the names and types of fields in the source data frame.
#' This is important because the data in each field will be aggregated in different ways,
#'   depending on its fieldtype.  See \link{availablefieldtypes}
#' @param ... names and types of fields (columns) in source data.
#' @return A \code{fieldtypes} object
#' @examples fts <- fieldtypes(PatientID = ft_uniqueidentifier(),
#'                             TestID = ft_ignore(),
#'                             TestDate = ft_timepoint(),
#'                             TestName = ft_categorical(aggregate_by_each_category = FALSE),
#'                             TestResult = ft_numeric(),
#'                             ResultDate = ft_datetime(),
#'                             ResultComment = ft_freetext(),
#'                             Location = ft_categorical())
#' @seealso \code{\link{availablefieldtypes}}
#' @export
fieldtypes <- function(...) {
  fts <- list(...)

  # validate - collect all errors together and return only once
  # TODO: replace spaces and special chars with _
  err_validation <- character()
  is_fieldtype <- vapply(fts, is.fieldtype, logical(1))
  if (any(!is_fieldtype)) {
  	err_validation <- append(err_validation, paste("Unrecognised fieldtype(s) in positions: [", paste(which(!is_fieldtype), collapse = ", "), "]", "names: [", paste(names(fts)[which(!is_fieldtype)], collapse = ", "), "]"))
  }
  is_timepoint <- vapply(fts, is.fieldtype_timepoint, logical(1))
  if (sum(is_timepoint) != 1) {
  	err_validation <- append(err_validation, paste("Must specify one and only one timepoint field. Timepoints currently in positions: [", paste(which(is_timepoint), collapse = ", "), "]", "names: [", paste(names(fts)[which(is_timepoint)], collapse = ", "), "]"))
  }
  if (anyDuplicated(names(fts)) > 0){
  	err_validation <- append(err_validation, paste("Duplicate column names not allowed: [", paste(names(fts)[duplicated(names(fts))], collapse = ", "), "]"))
  }
  if (length(err_validation) > 0) {
    stop_custom(.subclass = "invalid_fieldtypes",
    						message = paste0("Invalid `fieldtypes' specification.\n",
    														 paste(err_validation, collapse = "\n")))
  }

  structure(fts, class = "fieldtypes")
}

is.fieldtypes <- function(x) inherits(x, "fieldtypes")

fieldtypes_to_string <- function(fieldtypes){
	s <- ""
	for( ft in seq_along(fieldtypes) ){
		s <- paste0(s, names(fieldtypes[ft]), "\t", "<", class(fieldtypes[[ft]])[1], ">")
		if( !is.null(fieldtypes[[ft]]$options) ){
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

#' Print a template fieldtypes specification to console
#'
#' Helper function to generate template code for a fieldtypes() specification, based on
#' the supplied data frame. All fields (columns) in the specification will be defined using the default_fieldtype,
#' and the console output can be copied and edited before being used as input to \code{\link{create_report}}() or \code{\link{prepare_data}}().
#' @param df data frame including the column names for the template specification
#' @param default_fieldtype fieldtype to be used for each column. Default = \code{ft_ignore()}. See  \code{\link{availablefieldtypes}}
#' @examples
#' df <- data.frame(col1 = rep("2022-01-01", 5), col2 = rep(1, 5), col3 = 1:5, col4 = rnorm(5))
#'
#' fieldtypes_template(df, default_fieldtype = ft_numeric())
#' @seealso \code{\link{fieldtypes}}
#' @export
fieldtypes_template <- function(df, default_fieldtype = ft_ignore()){
	fieldnames <- names(df)
	cat("fieldtypes(",
			paste0("\"", fieldnames, "\"", " = ft_", default_fieldtype$type, "()",
						 ifelse(fieldnames == rev(fieldnames)[1], "", ","),
						 collapse = "\n\t" ),
			")")
}

# -----------------------------------------------------------------------------
# MAP ALLOWABLE TYPES TO:
# 	readr::col_types
#		read.table::colClasses
fieldtypes_to_cols <- function(fieldtypes, readfunction, alltostring = FALSE){
  # validate
  if (missing(fieldtypes) || !is.fieldtypes(fieldtypes)) {
    stop("Invalid parameter(s) supplied:",
         "`fieldtypes'", call. = FALSE)
  }
	if (readfunction == "readr"){
		if (alltostring == TRUE){
			do.call(readr::cols,lapply(fieldtypes, function(x){readr::col_character()}))
		}
		else{
			do.call(readr::cols,lapply(fieldtypes, get_collector))
		}
	} else if (readfunction %in% c("read.table", "data.table")){
		if (alltostring == TRUE){
			unlist(lapply(fieldtypes, function(x){"character"}))
		}
		else{
			unlist(lapply(fieldtypes, get_dataclass))
		}
	}
}

get_collector <- function(fieldtype){
  fieldtype$collector
}

get_dataclass <- function(fieldtype){
	fieldtype$dataclass
}

get_fieldtype_name <- function(fieldtype){
  fieldtype$type
}


# -----------------------------------------------------------------------------
get_aggfunctions <- function(fieldtype){
  fieldtype$aggfunctions
}

