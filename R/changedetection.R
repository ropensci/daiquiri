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

#'
#' # -----------------------------------------------------------------------------
#' #' Process source data
#' #'
#' #' Aggregates cpdsourcedata object based on fieldtypes specified at load time.
#' #' Creates additional cpdaggregate objects if source fieldtypes are specified
#' #' Time period for aggregation is a calendar day.  Further options to be added at a later date.
#' #'
#' #' @param data A \code{cpdsourcedata} object
#' #' @param showprogress Print progress to console. Default = FALSE
#' #' @return A list of \code{cpdaggregate} objects
#' #' @export
#' process_source_data <- function(data, showprogress = FALSE){
#' 	#temp assignment
#' 	# data<-testcpdsourcedata
#' 	# showprogress = TRUE
#'
#' 	## create overall cpdaggregate
#' 	cpdaggoverall <- aggregate_data(data, showprogress = showprogress)
#'
#' 	## aggregate by each sourcefield subgroup
#' 	# set up object of correct size first
#' 	sourcefield_indexes <- numeric()
#' 	# NOTE: could probably just loop through all datafields here, not sure of value of only checking the imported ones
#' 	for (i in 1:data$ncols_imported){
#' 		fieldindex = data$cols_imported_indexes[[i]]
#' 		if (is.fieldtype_source(data$datafields[[fieldindex]]$fieldtype)){
#' 			sourcefield_indexes <- c(sourcefield_indexes, fieldindex)
#' 		}
#' 	}
#'
#' 	# filter cpdsourcedata by each sourcefield level then send to generic aggregate_data function
#' 	if (length(sourcefield_indexes) > 0){
#' 		subaggregate <- vector("list", length(sourcefield_indexes))
#' 		for (i in 1:length(sourcefield_indexes)){
#' 			if(showprogress) cat("Aggregating by ", data$datafields[[sourcefield_indexes[[i]]]]$columnname, "...","\n", sep = "")
#' 			# create factor of sourcefield subgroups
#' 			sourcefield_levels <- unlist(unique(data$datafields[[sourcefield_indexes[[i]]]]$values))
#' 			for (j in 1:length(sourcefield_levels)){
#' 				sourcefield_levelindicator <- which(data$datafields[[sourcefield_indexes[[i]]]]$values == sourcefield_levels[j])
#' 				if(showprogress) cat("Aggregating each datafield in turn...","\n")
#' 				subaggregate[[i]]$agg <- vector("list", data$ncols_imported - 1)
#' 				for (k in 1:data$ncols_imported){
#' 					if(showprogress) cat(k, ":", names(data$cols_imported_indexes)[k],"\n")
#' 					fieldindex = data$cols_imported_indexes[[k]]
#' 					if (fieldindex != sourcefield_indexes[[i]]){
#' 						subaggregate[[i]]$agg[[k]] <- aggregatefield(data$datafields[[fieldindex]], timepoint_byday, groupby_day, showprogress)
#' 					}
#' 				}
#' 			}
#' 		}
#' 	}
#' }

