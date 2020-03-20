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

is.fieldtype_number <- function(x) inherits(x, "fieldtype_number")

is.fieldtype_partition <- function(x) inherits(x, "fieldtype_partition")

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
#' @rdname availablefieldtypes
#' @export
ft_timepoint <- function() {
  fieldtype("timepoint",
            collector = readr::col_datetime(),
  					dataclass = "POSIXct",
            aggfunctions = c("n", "midnight_n", "midnight_perc")
            )
}

#' @section Details:
#' \code{ft_uniqueidentifier} - identifies data fields which contain a (usually computer-generated) identifier for an entity, e.g. a patient.
#' It does not need to be unique within the dataset.
#' @rdname availablefieldtypes
#' @export
ft_uniqueidentifier <- function() {
	# TODO: potential additional aggfunctions: proportion numeric; distinct first chars
  fieldtype("uniqueidentifier",
            readr::col_character(),
  					dataclass = "character",
  					aggfunctions = c("n", "missing_n", "missing_perc", "minlength", "maxlength", "meanlength")
            )

}

#' @section Details:
#' \code{ft_partition} - identifies data fields which should be used to partition the data, such that each partition
#'   should be considered separately, e.g. a field that designates that different data rows originated from distinct source systems.
#' The data will be aggregated overall as well as by each partition.
#' @rdname availablefieldtypes
#' @export
ft_partition <- function() {
  fieldtype("partition",
            readr::col_character(),
  					dataclass = "character",
  					aggfunctions = c("n", "missing_n", "missing_perc", "distinct", "subcat_n", "subcat_perc")
            )
}

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
  fieldtype("categorical",
            readr::col_character(),
  					dataclass = "character",
  					aggfunctions = aggfn,
  					options = options
  )
}

#' @section Details:
#' \code{ft_number} - identifies data fields which contain numeric values that should be treated as continuous.
#' @rdname availablefieldtypes
#' @export
ft_number <- function() {
  fieldtype("number",
            readr::col_number(),
  					dataclass = "numeric",
  					aggfunctions = c("n", "missing_n", "missing_perc", "nonconformant_n", "nonconformant_perc", "min", "max", "mean", "median")
  )
}

#' @section Details:
#' \code{ft_datetime} - identifies data fields which contain date values that should be treated as continuous.
#' @param includes_time If TRUE, additional aggregated values will be generated using the time portion (and if no time portion is present then midnight will be assumed). If FALSE, aggregated values will ignore any time portion. Default = TRUE
#' @rdname availablefieldtypes
#' @export
ft_datetime <- function( includes_time = TRUE ) {
	aggfn <- c("n", "missing_n", "missing_perc", "nonconformant_n", "nonconformant_perc", "min", "max")
	options <- NULL
	if( includes_time ){
		aggfn <- c(aggfn, "midnight_n", "midnight_perc")
		# TODO: can probably do something more sophisticated here with match.call()
		options <- "includes_time"
	}
	fieldtype("datetime",
            readr::col_datetime(),
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
  fieldtype("freetext",
            readr::col_character(),
  					dataclass = "character",
  					aggfunctions = c("n", "missing_n", "missing_perc")
  )
}

# # try to guess the type
# ft_guess <- function() {
#   fieldtype("guess", readr::col_guess())
# }

#' @section Details:
#' \code{ft_simple} - identifies data fields where you only want presence/missingness to be evaluated.
#' @rdname availablefieldtypes
#' @export
ft_simple <- function() {
  fieldtype("simple",
            readr::col_character(),
  					dataclass = "character",
  					aggfunctions = c("n", "missing_n", "missing_perc")
  )
}

#' @section Details:
#' \code{ft_ignore} - identifies data fields which should be ignored.  These will not be loaded.
#' @rdname availablefieldtypes
#' @export
ft_ignore <- function() {
  fieldtype("ignore",
  					readr::col_skip(),
  					dataclass = "NULL"
  )
}

# this is an internal fieldtype for calculating stats across all fields combined and should not be set explicitly by user
ft_allfields <- function() {
	fieldtype("allfields",
						readr::col_skip(),
						dataclass = "NULL",
						aggfunctions = c("n", "missing_n", "missing_perc", "nonconformant_n", "nonconformant_perc")
	)
}

# this is an internal fieldtype for calculating duplicates and should not be set explicitly by user
ft_duplicates <- function() {
	fieldtype("duplicates",
						readr::col_skip(),
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
#' Helper function for users to specify the types of fields in the source data.  This is important both for reading
#'   in the data correctly, and because the data in each field will be tested for changepoints in different ways,
#'   depending on its fieldtype.  See \link{availablefieldtypes}
#' @param ... names and types of fields (columns) in source data, in the correct order.
#'   CHECK IF READR DEALS WITH CORRECTLY NAMED COLUMNS IN WRONG ORDER
#' @return A \code{fieldtypes} object
#' @examples fts <- fieldtypes(PatientID = ft_uniqueidentifier()
#'   ,TestDate = ft_timepoint()
#'   ,TestName = ft_categorical(aggregate_by_each_category = FALSE)
#'   ,TestResult = ft_number()
#'   ,TestComment = ft_ignore()
#'   ,Site = ft_partition())
#' @export
fieldtypes <- function(...) {
  fts <- list(...)

  # validate - collect all errors together and return only once
  # TODO: can't have multiple items with same column name (or else we could append a numeric suffix to duplicated names)
  # TODO: replace spaces and special chars with _
  err_validation <- character()
  is_fieldtype <- vapply(fts, is.fieldtype, logical(1))
  if (any(!is_fieldtype)) {
    err_validation[length(err_validation)+1] <- paste("Unrecognised fieldtype(s): [", paste(which(!is_fieldtype), collapse = ", "), "]")
  }
  is_timepoint <- vapply(fts, is.fieldtype_timepoint, logical(1))
  if (sum(is_timepoint) != 1) {
    # TODO: better to return names rather than indices
    err_validation[length(err_validation)+1] <- paste("Must contain one and only one timepoint field. Relevant fields [", paste(which(is_timepoint), collapse = ", "), "]")
  }
  is_partition <- vapply(fts, is.fieldtype_partition, logical(1))
  # if (sum(is_partition) > 1) {
  # 	# TODO: better to return names rather than indices
  # 	err_validation[length(err_validation)+1] <- paste("Must contain a maximum of one partition field. Relevant fields [", paste(which(is_partition), collapse = ", "), "]")
  # }
  if (length(err_validation) > 0) {
    stop("Invalid `fieldtypes' specification. ",
       paste(err_validation, collapse = "; "),
       call. = FALSE)
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



# -----------------------------------------------------------------------------
# MAP ALLOWABLE TYPES TO:
# 	readr::col_types
#		read.table:colClasses
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

