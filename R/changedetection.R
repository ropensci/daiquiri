#' changedetection: Automatic Change-point Detection for Large Datasets
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
#' @name changedetection
NULL

# main set of external functions
# TODO: make example/test data available as a dataset in the package
# TODO: Roxygenise internal functions too?

# TODO: probably want as few dependencies as possible so maybe ditch the tibble



#' Load source data
#'
#' Load source data into cpdsourcedata object
#'
#' @param file Full or relative path of file containing data to load
#' @param fieldtypes \code{\link{fieldtypes}} object specifying names and types of fields (columns) in source data.
#' @param na vector containing strings that should be interpreted as \code{NA}, e.g. \code{c("","NULL")}
#' @return A \code{cpdsourcedata} object
#' @export
load_dataset <- function(file, fieldtypes = NULL, na = NULL){
	# TODO: other versions that load from a data frame etc, use @describeIn for help file
	# TODO: locales and trimming
	# TODO: add option to suppress output to console
	# keep individual steps external for now (instead of bundling into a single function call) as
	#   may be useful for user to be able to inspect the summary before proceeding with aggregation, or to do multiple different aggregations
	#   maybe create a single top-level function too
	# TODO: return a recommendation(s) for timepoint granularity?

  # load data
  source_tibble <- readr::read_csv(file, col_types = fieldtypes_to_cols(fieldtypes), na=na)

  # TODO: validate specification against data loaded - not sure how much of this is already done by readr since we specify the coltypes
  # e.g. timepoint field must be not null; readr warning for 2 string values in Dose field

  # create cpdsourcedata object which includes "ignored" columns
  cpdsourcedata <- cpdsourcedata(source_tibble, fieldtypes, file)
  # print summary to console
  print(summarise_source_data(cpdsourcedata))

  # return cpdsourcedata object
  cpdsourcedata

}





