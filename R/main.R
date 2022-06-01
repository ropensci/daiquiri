#' Data quality reporting for temporal datasets
#'
#' Generate reports that enable quick visual review of temporal shifts in
#' record-level data. Time series plots showing aggregated values are
#' automatically created for each data field (column) depending on its contents
#' (e.g. min/max/mean values for numeric data, no. of distinct values for
#' categorical data), as well as overviews for missing values, non-conformant
#' values, and duplicated rows. The resulting reports are sharable and can
#' contribute to forming a transparent record of the entire analysis process. It
#' is designed with electronic health records in mind, but can be used for any
#' type of record-level temporal data (i.e. tabular data where each row
#' represents a single “event”, one column contains the "event date", and other
#' columns contain any associated values for the event).
#'
#' Classes are S3
#'
#' The best place to start is the \code{\link{create_report}} function, and the
#' walkthrough vignette:
#' \href{../doc/walkthrough.html}{\code{vignette("walkthrough", package =
#' "daiquiri")}}.
#'
#'
#' @docType package
#' @name daiquiri
NULL


#' Create a data quality report from a data frame
#'
#' Accepts record-level data from a data frame, validates it against the
#' expected type of content of each column, generates a collection of time
#' series plots for visual inspection, and saves a report to disk.
#'
#' @param df A data frame. Rectangular data can be read from file using
#'   \code{\link{read_data}}. See details.
#' @param fieldtypes \code{\link{fieldtypes}} object specifying names and types
#'   of fields (columns) in source data. See also \link{availablefieldtypes}.
#' @param override_columnnames If FALSE, column names must exist in data frame
#'   and must match the names specified in fieldtypes exactly. If TRUE, column
#'   names in source will be replaced with names in fieldtypes specification.
#'   The specification must therefore contain the columns in the correct order.
#'   Default = FALSE
#' @param na vector containing strings that should be interpreted as missing
#'   values, Default = \code{c("","NA","NULL")}.
#' @param dataset_shortdesc Short description of the dataset being checked. This
#'   will appear on the report. If blank, the name of the data frame object will
#'   be used
#' @param aggregation_timeunit Unit of time to aggregate over. Specify one of
#'   "day", "week", "month", "quarter", "year". The "week" option is
#'   Monday-based. Default = "day"
#' @param save_directory String specifying directory in which to save the
#'   report. Default is current directory.
#' @param save_filename String specifying filename for the report, excluding any
#'   file extension. If no filename is supplied (i.e. filename = NULL), one will
#'   be automatically generated with the format daiquiri_report_YYMMDD_HHMMSS.
#' @param showprogress Print progress to console. Default = TRUE
#' @param log_directory String specifying directory in which to save log file.
#'   If no directory is supplied, progress is not logged.
#' @return A list containing information relating to the supplied parameters as
#'   well as the resulting \code{sourcedata} and \code{aggregatedata} objects.
#' @section Details: In order for the package to detect any non-conformant
#'   values in numeric or datetime fields, these should be present in the data
#'   frame in their raw character format. Rectangular data from a text file will
#'   automatically be read in as character type if you use the
#'   \code{\link{read_data}} function. Data frame columns that are not of class
#'   character will still be processed according to the fieldtypes
#'   specification.
#' @examples
#' rawdata <- read_data(
#'   system.file("extdata", "example_data.csv", package = "daiquiri"),
#'   delim = ",",
#'   col_names = TRUE
#' )
#'
#' daiqobj <- create_report(
#'   rawdata,
#'   fieldtypes = fieldtypes(PrescriptionID = ft_uniqueidentifier(),
#'     PrescriptionDate = ft_timepoint(),
#'     AdmissionDate = ft_datetime(includes_time = FALSE),
#'     Drug = ft_freetext(),
#'     Dose = ft_numeric(),
#'     DoseUnit = ft_categorical(),
#'     PatientID = ft_ignore(),
#'     Location = ft_categorical(aggregate_by_each_category=TRUE)),
#'   override_columnnames = FALSE,
#'   na = c("","NULL"),
#'   aggregation_timeunit = "day",
#'   save_directory = ".",
#'   save_filename = "example_data_report",
#'   showprogress = TRUE,
#'   log_directory = NULL
#' )
#'
#' \dontshow{file.remove("./example_data_report.html")}
#'
#' @seealso \code{\link{read_data}}, \code{\link{fieldtypes}},
#'   \code{\link{availablefieldtypes}}
#' @export
create_report <- function(df,
													fieldtypes,
													override_columnnames = FALSE,
													na = c("", "NA", "NULL"),
													dataset_shortdesc = NULL,
													aggregation_timeunit = "day",
													save_directory = ".",
													save_filename = NULL,
													showprogress = TRUE,
													log_directory = NULL) {

	# if a log directory is supplied, start a new log. Otherwise, close any existing log
	if (!is.null(log_directory)) {
		log_filename <- log_initialise(log_directory)
	} else{
		log_close()
		log_filename <- NULL
	}

	log_function_start(match.call()[[1]])

	# check params before running anything so that it fails sooner rather than later
	validate_params_required(match.call())
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
		log_directory = log_directory
	)

	sourcedata <-
		prepare_data(
			df,
			fieldtypes,
			override_columnnames = override_columnnames,
			dataset_shortdesc = dataset_shortdesc,
			na = na,
			showprogress = showprogress
		)

	aggregatedata <-
		aggregate_data(sourcedata,
									 aggregation_timeunit = aggregation_timeunit,
									 showprogress = showprogress)

	reportfilename <-
		report_data(
			sourcedata,
			aggregatedata,
			save_directory = save_directory,
			save_filename = save_filename,
			showprogress = showprogress
		)

	log_function_end(match.call()[[1]])

	log_close()

	structure(
		list(
			dataset_shortdesc = sourcedata$dataset_shortdesc,
			fieldtypes = fieldtypes,
			override_columnnames = override_columnnames,
			na_values = na,
			aggregation_timeunit = aggregation_timeunit,
			report_filename = reportfilename,
			sourcedata = sourcedata,
			aggregatedata = aggregatedata,
			log_filename = log_filename
		),
		class = "daiquiri_object"
	)
}

#' @export
print.daiquiri_object <- function(x, ...) {
	cat("Class: daiquiri_object\n")
	cat("Dataset:", x$sourcedata$dataset_shortdesc, "\n")
	cat("\n")
	cat("Columns in source:", x$sourcedata$cols_source_n, "\n")
	cat("Columns imported:", x$sourcedata$cols_imported_n, "\n")
	cat("Rows in source:", x$sourcedata$rows_source_n, "\n")
	cat("Duplicate rows removed:", x$sourcedata$rows_duplicates_n, "\n")
	cat("Rows imported:", x$sourcedata$rows_imported_n, "\n")
	cat("Column used for timepoint:", x$sourcedata$timepoint_fieldname, "\n")
	cat("Rows missing timepoint values removed:", x$sourcedata$timepoint_missing_n, "\n")
	cat("Total validation warnings:", nrow(x$sourcedata$validation_warnings), "\n")
	cat("\n")

	aggfields <- x$aggregatedata$aggregatefields
	cat("Min timepoint value:", format(aggfields[[1]]$values[[1]][1]), "\n")
	cat("Max timepoint value:", format(rev(aggfields[[1]]$values[[1]])[1]), "\n")
	cat("Timepoint aggregation unit:", x$aggregatedata$aggregation_timeunit, "\n")
	cat("Total number of timepoints:",
			length(aggfields[[x$aggregatedata$timepoint_fieldname]]$values[[1]]),	"\n")
	cat("Number of empty timepoints:",
			sum(aggfields[[x$aggregatedata$timepoint_fieldname]]$values[["n"]] == 0),	"\n")
}


#' Read data from delimited file
#'
#' Read rectangular data from a delimited file, with all columns read in as
#' character type so that the package can later detect any non-conformant
#' values. Operates as a restricted implementation of \code{readr::read_delim}.
#'
#' @param file A string containing path of file containing data to load, or a
#'   URL starting http://, file://, etc. Compressed files with extension ‘.gz’,
#'   ‘.bz2’, ".xz" and ".zip" are supported.
#' @param delim Single character used to separate fields within a record. E.g.
#'   \code{","} or \code{"\t"}
#' @param col_names Either TRUE, FALSE or a character vector of column names. If
#'   TRUE, the first row of the input will be used as the column names, and will
#'   not be included in the data frame. If FALSE, column names will be generated
#'   automatically: X1, X2, X3 etc. Default = TRUE
#' @param quote Single character used to quote strings.
#' @param trim_ws Should leading and trailing whitespace be trimmed from each
#'   field?
#' @param comment A string used to identify comments. Any text after the comment
#'   characters will be silently ignored
#' @param skip Number of lines to skip before reading data. If \code{comment} is
#'   supplied any commented lines are ignored after skipping
#' @param n_max Maximum number of lines to read.
#' @param showprogress Display a progress bar? Default = TRUE
#' @return A data frame
#' @examples rawdata <- read_data(
#'   system.file("extdata", "example_data.csv", package = "daiquiri"),
#'   delim = ",",
#'   col_names = TRUE
#' )
#' @seealso \code{\link{fieldtypes}}, \code{\link{availablefieldtypes}},
#'   \code{\link{aggregate_data}}, \code{\link{report_data}},
#'   \code{\link{create_report}}
#' @export
read_data <- function(file,
											delim = NULL,
											col_names = TRUE,
											quote = "\"",
											trim_ws = TRUE,
											comment = "",
											skip = 0,
											n_max = Inf,
											showprogress = TRUE) {

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
		progress = showprogress,
		skip_empty_rows = TRUE,
		lazy = TRUE
	)

}

# Alternative read_data using fread instead
# #' Read data from delimited file (data.table)
# #'
# #' Read rectangular data from a delimited file, with all columns read in as character type so that the package can later detect any non-conformant values.
# #' Operates as a restricted implementation of \code{data.table::fread}, but returns a data.frame by default.
# #'
# #' @param file A string containing path of file containing data to load,
# #' or a URL starting http://, file://, etc. Compressed files with extension ‘.gz’ and ‘.bz2’ are supported.
# #' @param sep The separator between columns. Defaults to the character in the set
# #' [,\t |;:] that separates the sample of rows into the most number of lines with the same number of fields
# #' @param header Does the first row contain the column names?
# #' @param skip Header rows to skip
# #' @param nrows	The maximum number of rows to read
# #' @param select A vector of column names or numbers to keep, drop the rest
# #' @param drop A vector of column names or numbers to drop, keep the rest
# #' @param quote Single character used to quote strings
# #' @param strip.white Strip leading and trailing whitespaces of unquoted fields. Default = TRUE. If FALSE, only header trailing spaces are removed.
# #' @param showprogress Print progress to console. Default = TRUE
# #' @return A data.frame by default, or a \code{data.table} object if \code{data.table} = TRUE
# #' @examples sourcedataobj <- read_data(
# #'   system.file("extdata", "example_data.csv", package = "daiquiri"),
# #'   sep = ","
# #'   header = TRUE
# #' )
# #' @seealso \code{\link{fieldtypes}}, \code{\link{availablefieldtypes}}, \code{\link{aggregate_data}}, \code{\link{report_data}}, \code{\link{create_report}}
# #' @export
# fread_data <- function(file, sep = "auto", header = "auto", select = NULL, drop = NULL,
# 												 skip = 0, nrows = Inf, quote = "\"",
# 												 strip.white = TRUE, data.table = FALSE, showprogress = TRUE){
# 	#	NOTE: col.names param has no default for consistency with read.table and fread
# 	# 	call_args = names(match.call())
# 	#	(!'col.names' %chin% call_args)
# 		data.table::fread(file, sep = sep, nrows = nrows, header = header, skip = skip, select = select,
# 																	 drop = drop, check.names = FALSE,
# 																	 quote = quote, strip.white = strip.white,
# 																	 blank.lines.skip = TRUE, colClasses = "character",
# 											data.table = data.table, showProgress = showprogress)
# 	}
#
#
