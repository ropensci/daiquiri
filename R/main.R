#' ehrchangepoints: Automatic Change-point Detection for Electronic Health Records
#'
#' This is the help page for the package as a whole
#'
#' @section Section 1:
#' Classes are S3
#'
#' @section Section 2:
#' The functions ...
#'
#' @docType package
#' @name ehrchangepoints
NULL

# main set of external functions
# TODO: make example/test data available as a dataset in the package
# TODO: add a top-level function that will do everything in one go?



#' Load source data
#'
#' Load source data into sourcedata object
#'
#' @param x Either a data frame or a string containing full or relative path of file containing data to load
#' @param fieldtypes \code{\link{fieldtypes}} object specifying names and types of fields (columns) in source data.
#' @param override_columnnames If FALSE, column names must exist in data frame or header row of file and must match
#' the names specified in fieldtypes exactly. If TRUE, column names in source will be replaced with names in fieldtypes
#' specification. The specification must therefore contain the columns in the correct order. Default = FALSE
#' @param na vector containing strings that should be interpreted as \code{NA}, e.g. \code{c("","NULL")}
#' @param showprogress Print progress to console. Default = FALSE
#' @param log_directory String specifying directory in which to save log file. If no directory is supplied, progress is not logged.
#' @return A \code{sourcedata} object
#' @export
load_dataset <- function(x, fieldtypes, override_columnnames = FALSE, na = NULL, showprogress = FALSE, log_directory = NULL){
	# TODO: other versions that load from a data frame etc, use @describeIn for help file
	# TODO: locales and trimming
	# TODO: add option to suppress output to console (rather than just via param)
	# keep individual steps external for now (instead of bundling into a single function call) as
	#   may be useful for user to be able to inspect the summary before proceeding with aggregation, or to do multiple different aggregations
	# TODO: return a recommendation(s) for timepoint granularity?
	# TODO: distinguish between NULLs and empty strings? Probably useful to do so, though user could always treat empty strings as values (in na param)
	# temp assignments
	# x <- testfile
	# fieldtypes <- testfile_fieldtypes
	# override_columnnames = FALSE
	# na = na=c("","NULL")
	# showprogress=TRUE

	# if a log directory is supplied, start a new log. Otherwise, it will append to an existing log if one is still in memory
	# TODO: should we start a new log every time?
	if( !is.null(log_directory) ){
		log_initialise(log_directory)
	}

	log_function_start(match.call()[[1]])

	log_message(paste0("Fieldtypes supplied:\n", fieldtypes_to_string(fieldtypes)), showprogress)

	if( is.data.frame(x) ){
		source_name <- as.list(match.call())$x
		log_message(paste0("Identified data frame [", source_name, "]"), showprogress)
		# check for mismatch between fieldtypes names and column names
		validate_columnnames(names(x), names(fieldtypes), check_length_only = override_columnnames)
		source_df <- x
	} else if( is.character(x) & length(x) == 1 ){
		# assume all strings are file paths for now
		validate_param_file(x)
		source_name <- normalizePath(x)
		ext <- tolower(substring(x, regexpr("\\.[^\\.]*$", x)))
		# load data
		if( ext == ".csv"){
			log_message(paste0("Identified csv file [", source_name, "]"), showprogress)
			log_message(paste0("Checking column names against fieldtypes..."), showprogress)
			# check for mismatch between fieldtypes names and column names before reading in whole file
			# assumes first row has column names
			check_df <- data.table::fread(file = x, nrows = 1, sep = "auto", na.strings = na, data.table = FALSE)
			validate_columnnames(names(check_df), names(fieldtypes), check_length_only = override_columnnames)

			# TODO: doesn't deal with embedded quotes perfectly
			# TODO: doesn't recognise that a string that starts with a number is not a number (DurationEnteredByPrescriber)
			# TODO: deal with parsing errors appropriately
			# read all values as string, then check datatypes after
			log_message(paste0("Reading file from disk..."), showprogress)
			source_df <- readr::read_csv(x, col_types = fieldtypes_to_cols(fieldtypes, readfunction = "readr", alltostring = TRUE), na=na)

			# return a dataframe instead of a data.table
			# doesn't seem to recognise POSIXct type
			# also stops reading when there is an embedded comma (even if it is surrounded by quotes) even though the help says it can deal with this. Could this be a Windows thing?
			#source_df <- data.table::fread(file = x, sep = "auto", na.strings = na, colClasses = fieldtypes_to_cols(fieldtypes, readfunction = "data.table"), data.table = FALSE)
			# read all values as string
			#source_df <- data.table::fread(file = x, sep = "auto", na.strings = na, colClasses = fieldtypes_to_cols(fieldtypes, readfunction = "data.table", alltostring = TRUE), data.table = FALSE)
		} else {
			#TODO: read in rdata files
			stop(paste("Unsupported file extension:", ext, ". Files can be: csv"))
		}
	} else{
		stop(paste("Unsupported data source: [ class = ", class(x), "; contents = ", substr(toString(x),1,100), "]. Source can be data frame or string file path"))
	}

	if (override_columnnames == TRUE){
		names(source_df) <- names(fieldtypes)
	}


  # create sourcedata object which includes "ignored" columns
  sourcedata <- sourcedata(data.table::setDT(source_df), fieldtypes, source_name, showprogress)
  # print summary to console
  if(showprogress){ print(summarise_source_data(sourcedata, showprogress)) }

  log_function_end(match.call()[[1]])

  # return sourcedata object
  sourcedata

}

# -----------------------------------------------------------------------------
# Validate column names against specification
validate_columnnames <- function(source_names, spec_names, check_length_only = FALSE){
	# source_names <- c("nonsense","set","of","nonsense","names")
	# spec_names <- c("nonsense","set","of","stuff")

	# validate - collect all errors together and return only once
	err_validation <- character()

	if (check_length_only == TRUE){
		if (length(source_names) != length(spec_names)){
			err_validation <- append(err_validation, paste0("Different number of columns in data vs fieldtypes specification: ", length(source_names), " in source, ", length(spec_names), " in specification"))
		}
	} else{
		# check for duplicates (spec_names should already have been checked in fieldtypes constructor)
		if (anyDuplicated(source_names) > 0){
			err_validation <- append(err_validation, paste("Duplicate column names in data: [", paste(source_names[duplicated(source_names)], collapse = ", "), "]"))
		}
		# names must be identical
		# TODO: do we want to allow names to be in a different order? Need to consider downstream effects.
		if (length(setdiff(source_names, spec_names)) > 0) {
			err_validation <- append(err_validation, paste("Column names in data but not in fieldtypes specification: [", paste(setdiff(source_names, spec_names), collapse = ", "), "]"))
		}
		if (length(setdiff(spec_names, source_names)) > 0) {
			err_validation <- append(err_validation, paste("Column names in fieldtypes specification but not in data: [", paste(setdiff(spec_names, source_names), collapse = ", "), "]"))
		}
	}

	if (length(err_validation) > 0) {
		stop("Invalid column names.\n",
				 paste(err_validation, collapse = "\n"),
				 call. = FALSE)
	}



}

#
#
# # -----------------------------------------------------------------------------
# # Validate data against specification
# # takes in a data frame, fieldtypes specification, na strings
# # use type_convert function from readr until have reason not to
# # TODO: call this from sourcedata
# validate_dataframe <- function(df, fieldtypes, na = NULL){
# 	# temp assignments
# 	# df <- source_df
# 	# fieldtypes <- testfile_fieldtypes
# 	# na <- c("","NULL")
#
# 	warn <- NULL
# 	clean_df <- withCallingHandlers(
# 		readr::type_convert(df, fieldtypes_to_cols(fieldtypes, readfunction = "readr"), na = na),
# 		warning = function(w) {
# 			warn <<- append(warn, conditionMessage(w))
# 			invokeRestart("muffleWarning")
# 			}
# 	)
# 	# process any warnings
# 	if (length(warn)>0){
#
# 	}
#
# 	# return data frame with correct(ed) datatypes
#
# }
#
# # check contents of column against desired datatype and convert if necessary
# # note any conversion failures
# validate_column <- function(x, datatype, na = NULL){
# 	x <- clean_df["Dose"]
# 	x <- clean_df["Clusterid"]
# 	x <- source_df["Dose"]
# #	x <- source_df["AdmissionDate"]
#
#
# 	if (datatype == "character"){
# 		if (!is.character(x)){
# 			colclean <- sapply(x, as.character)
# 		} else{
# 			colclean <- x
# 		}
# 		is.na(colclean) <- which(colclean %in% na)
# #		colfail <- c(NA, length(x))
# 	} else if (datatype == "numeric"){
# 		if (!is.numeric(x)){
# 			colclean <- lapply(x, function(i){as.double(i)}
# 			)
# 		} else{
# 			colclean <- x
# 		}
# 	} else if (datatype == "POSIXct"){
#
# 	}
# 	cbind(colclean, colfail)
# }

