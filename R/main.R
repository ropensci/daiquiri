#' ehrchangepoints: Data quality reporting for temporal datasets
#'
#' Generate reports that enable quick visual review of
#' temporal shifts in record-level data. Time series plots showing aggregated
#' values are automatically created for each column depending on its
#' datatype (e.g. min/max/mean values for numeric columns, no. of distinct
#' values for categorical columns), as well as overviews for missing values,
#' non-conformant values, and duplicated rows. It is designed with Electronic
#' Health Records in mind, but can be used for any type of record-level temporal data.
#'
#' Classes are S3
#'
#' The best place to start is the \code{\link{check_dataset}} function, and the walkthrough vignette: \href{../doc/walkthrough.html}{\code{vignette("walkthrough", package = "ehrchangepoints")}}.
#'
#'
#' @docType package
#' @name ehrchangepoints
NULL

# main set of external functions
# TODO: try to reduce dependencies installed by readr


#' Check dataset for potential data quality issues
#'
#' Accepts record-level data from a csv file or dataframe, generates a collection of time series for each column, and saves a report to disk.
#'
#' @param x Either a data frame or a string containing full or relative path of file containing data to load
#' @param fieldtypes \code{\link{fieldtypes}} object specifying names and types of fields (columns) in source data. See also \link{availablefieldtypes}.
#' @param textfile_contains_columnnames If the data to be loaded is a text file, does the first row contain the column names? Default = TRUE
#' @param override_columnnames If FALSE, column names must exist in data frame or header row of file and must match
#' the names specified in fieldtypes exactly. If TRUE, column names in source will be replaced with names in fieldtypes
#' specification. The specification must therefore contain the columns in the correct order. Default = FALSE
#' @param na vector containing strings that should be interpreted as missing values, Default = \code{c("","NULL")}.
#' @param aggregation_timeunit Unit of time to aggregate over. Specify one of "day", "week", "month", "quarter", "year". The "week" option is Monday-based. Default = "day"
#' @param save_directory String specifying directory in which to save the report. Default is current directory.
#' @param save_filename String specifying filename for the report, excluding any file extension.
#' If no filename is supplied (i.e. filename = NULL), one will be automatically generated with the format ehrchangepoints_report_YYMMDD_HHMMSS.
#' @param showprogress Print progress to console. Default = TRUE
#' @param log_directory String specifying directory in which to save log file. If no directory is supplied, progress is not logged.
#' @return A list containing information relating to the supplied parameters as well as the resulting \code{sourcedata} and \code{aggregatedata} objects.
#' @examples checkobj <- check_dataset(
#'   system.file("extdata", "abx2014.csv", package = "ehrchangepoints"),
#'   fieldtypes = fieldtypes(PrescriptionID = ft_uniqueidentifier(),
#'     PrescriptionDate = ft_timepoint(),
#'     AdmissionDate = ft_datetime(includes_time = FALSE),
#'     Drug = ft_freetext(),
#'     Dose = ft_numeric(),
#'     DoseUnit = ft_categorical(),
#'     PatientID = ft_ignore(),
#'     SourceSystem = ft_categorical(aggregate_by_each_category=TRUE)),
#'   textfile_contains_columnnames = TRUE,
#'   override_columnnames = FALSE,
#'   na = c("","NULL"),
#'   aggregation_timeunit = "day",
#'   save_directory = ".",
#'   save_filename = "abx2014report",
#'   showprogress = TRUE,
#'   log_directory = NULL
#' )
#' @seealso \code{\link{fieldtypes}}, \code{\link{availablefieldtypes}}
#' @export
check_dataset <- function(x, fieldtypes, textfile_contains_columnnames = TRUE, override_columnnames = FALSE, na = c("","NULL"), aggregation_timeunit = "day", save_directory = ".", save_filename = NULL, showprogress = TRUE, log_directory = NULL){
	# temp assignments
	# x <- testfile
	# fieldtypes <- testfile_fieldtypes
	# textfile_contains_columnnames = TRUE
	# override_columnnames = FALSE
	# na = na=c("","NULL")
	# aggregation_timeunit = "month"
	# showprogress=TRUE

	# if a log directory is supplied, start a new log. Otherwise, close any existing log
	if( !is.null(log_directory) ){
		log_initialise(log_directory)
	} else{
		log_close()
	}

	log_function_start(match.call()[[1]])

	# check params before running anything so that it fails sooner rather than later
	validate_aggregation_unit(aggregation_timeunit)
	validate_param_dir(save_directory)
	validate_param_savefilename(save_filename, allownull = TRUE)

	sourcedata <- load_dataset(x, fieldtypes, textfile_contains_columnnames = textfile_contains_columnnames, override_columnnames = override_columnnames, na = na, showprogress = showprogress)

	aggregatedata <- aggregate_data(sourcedata, aggregation_timeunit = aggregation_timeunit, showprogress = showprogress)

	reportfilename <- generate_report(sourcedata, aggregatedata, save_directory = save_directory, save_filename = save_filename, showprogress = showprogress)

	log_function_end(match.call()[[1]])

	log_close()

	structure(
		list(
			source_name = sourcedata$sourcename,
			fieldtypes = fieldtypes,
			textfile_contains_columnnames = textfile_contains_columnnames,
			override_columnnames = override_columnnames,
			na_values = na,
			aggregation_timeunit = aggregation_timeunit,
			report_filename = reportfilename,
			sourcedata = sourcedata,
			aggregatedata = aggregatedata
		),
		class = "check_dataset"
	)
}


#' Load source data
#'
#' Load source data into sourcedata object
#'
#' @param x Either a data frame or a string containing full or relative path of file containing data to load
#' @param fieldtypes \code{\link{fieldtypes}} object specifying names and types of fields (columns) in source data. See also \link{availablefieldtypes}.
#' @param textfile_contains_columnnames If the data to be loaded is a text file, does the first row contain the column names? Default = TRUE
#' @param override_columnnames If FALSE, column names must exist in data frame or header row of file and must match
#' the names specified in fieldtypes exactly. If TRUE, column names in source will be replaced with names in fieldtypes
#' specification. The specification must therefore contain the columns in the correct order. Default = FALSE
#' @param na vector containing strings that should be interpreted as missing values, Default = \code{c("","NULL")}.
#' @param showprogress Print progress to console. Default = TRUE
#' @return A \code{sourcedata} object
#' @examples sourcedataobj <- load_dataset(
#'   system.file("extdata", "abx2014.csv", package = "ehrchangepoints"),
#'   fieldtypes = fieldtypes(PrescriptionID = ft_uniqueidentifier(),
#'     PrescriptionDate = ft_timepoint(),
#'     AdmissionDate = ft_datetime(includes_time = FALSE),
#'     Drug = ft_freetext(),
#'     Dose = ft_numeric(),
#'     DoseUnit = ft_categorical(),
#'     PatientID = ft_ignore(),
#'     SourceSystem = ft_categorical(aggregate_by_each_category=TRUE)),
#'   textfile_contains_columnnames = TRUE,
#'   override_columnnames = FALSE,
#'   na = c("","NULL"),
#'   showprogress = TRUE
#' )
#' @seealso \code{\link{fieldtypes}}, \code{\link{availablefieldtypes}}, \code{\link{aggregate_data}}, \code{\link{generate_report}}, \code{\link{check_dataset}}
#' @export
load_dataset <- function(x, fieldtypes, textfile_contains_columnnames = TRUE, override_columnnames = FALSE, na = c("","NULL"), showprogress = TRUE){
	# TODO: rename?
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
	# textfile_contains_columnnames = TRUE
	# override_columnnames = FALSE
	# na = na=c("","NULL")
	# showprogress=TRUE

	log_function_start(match.call()[[1]])

	log_message(paste0("Fieldtypes supplied:\n", fieldtypes_to_string(fieldtypes)), showprogress)

	if( is.data.frame(x) ){
		# When load_dataset is called from check_dataset, passing in a dataframe, we lose the name of the original object
		#   that was passed in, so need to check the grandparent call instead
		if( match.call(definition = sys.function(-1), call = sys.call(sys.parent()))[[1]] == "check_dataset"){
			source_name <- as.list(match.call(definition = sys.function(-1), call = sys.call(sys.parent())))$x
		} else{
			source_name <- as.list(match.call())$x
		}
		log_message(paste0("Identified data frame [", source_name, "]"), showprogress)
		# check for mismatch between fieldtypes names and column names
		validate_columnnames(names(x), names(fieldtypes), check_length_only = override_columnnames)
		# ensure all columns are character type because readr::type_convert (in sourcedata()) won't skip numeric columns
		# TODO: can probably make more efficient by only converting non-char columns and/or converting to data.table first
		x[] <- lapply(x, as.character)
		source_df <- x
	} else if( is.character(x) && length(x) == 1 ){
		# assume all strings are file paths for now
		validate_param_file(x)
		source_name <- normalizePath(x)
		ext <- tolower(substring(x, regexpr("\\.[^\\.]*$", x)))
		# load data
		if( ext == ".csv"){
			log_message(paste0("Identified csv file [", source_name, "]"), showprogress)
			log_message(paste0("Checking column names against fieldtypes..."), showprogress)
			if( textfile_contains_columnnames == FALSE && override_columnnames == FALSE ){
				stop("Bad parameters supplied.\n", "If textfile_contains_columnnames is set to FALSE then override_columnnames must be set to TRUE", call. = FALSE)
			}
			# check for mismatch between fieldtypes names and column names before reading in whole file
			# assumes first row has column names
			check_df <- data.table::fread(file = x, nrows = 1, sep = "auto", na.strings = na, data.table = FALSE)
			validate_columnnames(names(check_df), names(fieldtypes), check_length_only = override_columnnames)

			# read all values as string, then check datatypes after
			log_message(paste0("Reading file from disk..."), showprogress)
			source_df <- readr::read_csv(x, col_names = textfile_contains_columnnames, col_types = fieldtypes_to_cols(fieldtypes, readfunction = "readr", alltostring = TRUE), na=na)

			# return a dataframe instead of a data.table
			# TODO: this may be useful if/when try to implement locales that use ; as separator and , as decimal point
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
  sourcedata <- sourcedata(data.table::setDT(source_df), fieldtypes, source_name, na = na, showprogress)
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

