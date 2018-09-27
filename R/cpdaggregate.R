# Code for creation of cpdaggregate object
# these contain both the (vector) data for the aggregated field and the relevant metadata
# TODO: decide whether better to store values as a dataframe or a list of ts/zoo objects

# -----------------------------------------------------------------------------
# individual aggregatefields
# uses stats::aggregate function, consider trying the aggregate.ts function instead, or group_by tibble function
aggregatefield <- function(datafield, alltimepoints, groupbylist, showprogress = FALSE) {
  #temp assignment
   #datafield = testcpddata$datafields[[3]]
   # groupbylist = list(groupby_day[[1]])
   # alltimepoints = timepoint_byday
  #showprogress = TRUE

  #functionlist = c("n", "missing_n", "missing_perc")
  if(showprogress) cat("Preparing...")
  functionlist = datafield$fieldtype$aggfunctions
  x = get_datafield_vector(datafield)


  # TODO: let each fieldtype determine what aggregate functions to use
  # TODO: sourcetypes need to be dealt with differently

  # # TODO: having trouble vectorising
  # do.call(data.frame, aggregate(x, by = groupbylist, FUN = function(x){c(n=length(x), min = min(x, na.rm = TRUE), missing_n = sum(is.na(x)), missing_perc = sum(is.na(x))/length(x))}, drop = FALSE))
  # agglist <-  function(x){
  #   c(n=length(x), min = min(x, na.rm = TRUE), missing_n = sum(is.na(x)), missing_perc = sum(is.na(x))/length(x))
  # }
  # agglist2 <-  function(functionlist){
  #   sapply(functionlist, aggfunction_lookupfn)
  # }
  #
  # agglist3 <-  function(functionlist){
  #   x <- substitute(x)
  #   sapply(functionlist, aggfunction_lookupfn)
  # }
  #
  # agglist4 <-  function(functionlist){
  #   sapply(functionlist, aggfunction_lookupfn2)
  # }
  #
  # agglist5 <- function(functionlist){
  #   quote(c("n" = length(x),
  #          "missing_n" = sum(is.na(x)),
  #          "missing_perc" = sum(is.na(x))/length(x),
  #          "distinct" = length(unique(x)),
  #          "min" = min(x, na.rm = TRUE),
  #          "max" = max(x, na.rm = TRUE))[functionlist])
  # }
  #
  # agglist6 <- quote(c("n" = length(x),
  #               "missing_n" = sum(is.na(x)),
  #               "missing_perc" = sum(is.na(x))/length(x),
  #               "distinct" = length(unique(x)),
  #               "min" = min(x, na.rm = TRUE),
  #               "max" = max(x, na.rm = TRUE))[functionlist])
  #
  # agglist7 <- function(functionlist){
  #   quote( v <- c("n" = length(x),
  #           "missing_n" = sum(is.na(x)),
  #           "missing_perc" = sum(is.na(x))/length(x),
  #           "distinct" = length(unique(x)),
  #           "min" = min(x, na.rm = TRUE),
  #           "max" = max(x, na.rm = TRUE))[functionlist])
  #   quote(v)
  # }

  #
  #
  # c("n" = length(x),
  #   "missing_n" = sum(is.na(x)),
  #   "missing_perc" = sum(is.na(x))/length(x),
  #   "distinct" = length(unique(x)),
  #   "min" = min(x, na.rm = TRUE),
  #   "max" = max(x, na.rm = TRUE))[functionlist]
  #
  # do.call(data.frame, aggregate(x, by = groupbylist, FUN = agglist, drop = FALSE))
  # do.call(data.frame, aggregate(x, by = groupbylist, FUN = agglist2, drop = FALSE))
  # do.call(data.frame, aggregate(x, by = groupbylist, FUN = agglist3, drop = FALSE))
   # do.call(data.frame, aggregate(x, by = groupbylist, FUN = function(x){eval(agglist4(functionlist))}, drop = FALSE))
  # do.call(data.frame, aggregate(x, by = groupbylist, FUN = function(x){aggfunction_lookup[functionlist]}, drop = FALSE))
   # do.call(data.frame, aggregate(x, by = groupbylist, FUN = function(x){eval(agglist5(functionlist))}, drop = FALSE))
   # do.call(data.frame, aggregate(x, by = groupbylist, FUN = function(x){eval(agglist6)}, drop = FALSE))
   # do.call(data.frame, aggregate(x, by = groupbylist, FUN = function(x){eval(agglist7)}, drop = FALSE))
   # do.call(data.frame, aggregate(x, by = groupbylist, FUN = function(x){eval(sapply(functionlist, aggfunction_lookupfn2))}, drop = FALSE))
  # do.call(data.frame, aggregate(x, by = groupbylist, FUN = function(x){c("n" = length(x),
  #                                                                        "missing_n" = sum(is.na(x)),
  #                                                                        "missing_perc" = sum(is.na(x))/length(x),
  #                                                                        "distinct" = length(unique(x)),
  #                                                                        "min" = min(x, na.rm = TRUE),
  #                                                                        "max" = max(x, na.rm = TRUE))[functionlist]}, drop = FALSE))
  #

  # TODO: ideally want to move FUN function into separate function but when try to do this it keeps evaluating early
  # TODO: for some reason, aggregate function coerces my character vectors into factors and tries to call min() on them (causing an error) even though I've supposedly subsetted min() out
  if(showprogress) cat("Aggregating", typeof(x),"field...")
  if (typeof(x) == "character") {
    groupedvals <- do.call(data.frame, stats::aggregate(x, by = groupbylist, FUN = function(x){c("n" = sum(!is.na(x)),
                                                                                                    "missing_n" = sum(is.na(x)),
                                                                                                    "missing_perc" = 100*sum(is.na(x))/length(x),
                                                                                                    "distinct" = length(unique(x)))[functionlist]}, drop = FALSE))
  } else {
    # suppress warnings for min/max as they complain when all values are NA. TODO: Ideally suppress just that particular warning and not others
    groupedvals <- do.call(data.frame, stats::aggregate(x, by = groupbylist, FUN = function(x){c("n" = sum(!is.na(x)),
                                                                                                "missing_n" = sum(is.na(x)),
                                                                                                "missing_perc" = 100*sum(is.na(x))/length(x),
                                                                                                "distinct" = length(unique(x)),
                                                                                                "min" = suppressWarnings(min(x, na.rm = TRUE)),
                                                                                                "max" = suppressWarnings(max(x, na.rm = TRUE)))[functionlist]}, drop = FALSE))
    # tidy up
    # min/max return Inf when all values are NA. Replace them with NA as don't want to differentiate these from timepoints where there are no records
    # min/max also drops datetime class
    for( c in c("min","max") ){
      if (c %in% functionlist){
        xc <- paste0("x.", c)
        groupedvals[[xc]][is.infinite(groupedvals[[xc]])] <- NA
        # preserve datatypes
        if( inherits(x,"POSIXct") ){
          groupedvals[[xc]] <- as.POSIXct(groupedvals[[xc]], tz = "UTC", origin = "1970-01-01")
        }
      }
    }
  }
  colnames(groupedvals) <- c(names(alltimepoints), functionlist)


  if(showprogress) cat("Tidying up...")
  aggfield <- merge(alltimepoints, groupedvals, by = names(alltimepoints), all = TRUE)
  # for dates with no records, replace n with 0, but leave other aggvalues as na
  aggfield$n[is.na(aggfield$n)] <- 0
  # TODO: set individual min/max dates per aggfield? Probably shouldn't calculate changepoints in sections where there are no records for that aggfield

  if(showprogress) cat("Calculating changepoints...")
  cpts <- find_changepoints(aggfield)[[1]]

  if(showprogress) cat("Finished","\n")

  structure(list(values = aggfield, functionlist = functionlist, changepoints = cpts, fieldtype = datafield$fieldtype, columnname = datafield$columnname),
            class = "aggregatefield")
}

is.aggregatefield <- function(x) inherits(x, "aggregatefield")


# probably not necessary
# get_aggregatefield_min <- function(aggregatefield){
#     min(aggregatefield$values[[1]], na.rm = TRUE)
# }
#
# get_aggregatefield_max <- function(aggregatefield){
#     max(aggregatefield$values[[1]], na.rm = TRUE)
# }

# aggfunction_lookup <- c("n" = length(x),
#                          "missing_n" = sum(is.na(x)),
#                          "missing_perc" = sum(is.na(x))/length(x),
#                          "distinct" = length(unique(x)),
#                          "min" = min(x, na.rm = TRUE),
#                          "max" = max(x, na.rm = TRUE)
#   )
#
#
# aggfunction_lookupfn <- function(function_name){
#   switch(function_name,
#          "n" = length(x),
#          "missing_n" = sum(is.na(x)),
#          "missing_perc" = sum(is.na(x))/length(x),
#          "distinct" = length(unique(x)),
#          "min" = min(x, na.rm = TRUE),
#          "max" = max(x, na.rm = TRUE),
#          "mean" = mean(x, na.rm = TRUE)
#   )
# }
#
# aggfunction_lookupfn2 <- function(function_name){
#   switch(function_name,
#          "n" = quote(length(x)),
#          "missing_n" = quote(sum(is.na(x))),
#          "missing_perc" = quote(sum(is.na(x))/length(x)),
#          "distinct" = quote(length(unique(x))),
#          "min" = quote(min(x, na.rm = TRUE)),
#          "max" = quote(max(x, na.rm = TRUE))
#          )
# }
#  aggfunction_lookup <- function(function_name){
    # switch(function_name,
  #        "n" = length(x),
  #        "missing_n" = function(x){ sum(is.na(x)) },
  #        "missing_perc" = function(x){ sum(is.na(x))/length(x) },
  #        "distinct" = function(x){ length(unique(x)) },
  #        "min" = min(x, na.rm = TRUE),
  #        "max" = max(x, na.rm = TRUE),
  #        "mean" = mean(x, na.rm = TRUE)
  # )
#}


# -----------------------------------------------------------------------------
#' Aggregate source data
#'
#' Aggregates cpdsourcedata object based on fieldtypes specified at load time.
#' Time period for aggregation is a calendar day.  Further options to be added at a later date.
#'
#' @param data A \code{cpdsourcedata} object
#' @param showprogress Print progress to console. Default = FALSE
#' @return A \code{cpdaggregate} object
#' @export
aggregate_data <- function(data, showprogress = FALSE){
  #temp assignment
  # data<-testcpddata
  #showprogress = TRUE

  # create column to group by - default to per Day, allow option to change later
  if(showprogress) cat("Preparing grouping column...")
  # need to ensure have all possible timepoint values, even if they are missing in the dataset
  timepoint_byday = list(seq(as.Date(get_datafield_min(data$datafields[[data$timepoint_fieldname]])), as.Date(get_datafield_max(data$datafields[[data$timepoint_fieldname]])), by="days"))
  names(timepoint_byday) <- paste0(data$timepoint_fieldname, "_byday")
  # stats::aggregate function requires the grouping vector to be passed in as a list so do this once first
  # TODO: if data is e.g. monthly then need to take this into account
  groupby_day = list(as.Date(get_datafield_vector(data$datafields[[data$timepoint_fieldname]])))

  # load aggregated data into new vector
  if(showprogress) cat("Aggregating each datafield in turn...","\n")
  agg <- vector("list", data$ncols_imported)
  for (i in 1:data$ncols_imported){
    if(showprogress) cat(i, ":", names(data$cols_imported_indexes)[i],"\n")
    fieldindex = data$cols_imported_indexes[[i]]
    agg[[i]] <- aggregatefield(data$datafields[[fieldindex]], timepoint_byday, groupby_day, showprogress)
  }
  names(agg) <- names(data$cols_imported_indexes)

  # TODO: create flat dataframe for all changepoints
  if(showprogress) cat("Creating changepoint dataframe...","\n")
  changepoints_df <- all_changepoints(agg)

  structure(
    list(
      aggregatefields = agg,
      changepoints_df = changepoints_df,
      timepoint_fieldname = data$timepoint_fieldname,
      timepoint_unit = "DAY" # not sure if this should be set at overall object level or allow it to differ per aggregatefield
    ),
    class = "cpdaggregate"
  )
}

is.cpdaggregate <- function(x) inherits(x, "cpdaggregate")


# -----------------------------------------------------------------------------
#' Export aggregated data
#'
#' Export aggregated data to disk.  Creates a separate file for each aggregated field in dataset
#'
#' @param cpdaggregate A \code{cpdaggregate} object
#' @param save_directory String. Full or relative path for save folder, use double blackslashes for nested folders and end with double backslash
#' @param save_filetype String. Filetype extension supported by \code{readr}, currently only csv allowed
#' @export
export_aggregated_data <- function(cpdaggregate, save_directory, save_filetype = "csv"){
	#temp assignment
	# cpdaggregate<-testcpddata_byday
	# save_directory = ".\\testoutput\\"
	# save_filetype = "csv"

	# TODO: validation checks on params

	# export a file for each source field in dataset
	for( i in 1:length(cpdaggregate$aggregatefields) ){
		readr::write_csv(cpdaggregate$aggregatefields[[i]]$values, paste0(save_directory, names(cpdaggregate$aggregatefields[i]), ".csv") )
	}
}

#' @export
print.cpdaggregate <- function(x, ...){
	# TODO: to finish
	aggsummary <- summarise_aggregated_data(x)
	cat("Class: cpdaggregate\n")
	cat("\n")
	cat("Overall:\n")
	cat("Number of data fields:", aggsummary$overall["n_fields"], "\n")
	cat("Column used for timepoint:", aggsummary$overall["timepoint_fieldname"], "\n")
	cat("Timepoint aggregation unit:", aggsummary$overall["timepoint_unit"], "\n")
	cat("Min timepoint value:", aggsummary$overall["timepoint_min"], "\n")
	cat("Max timepoint value:", aggsummary$overall["timepoint_max"], "\n")
	cat("Total number of timepoints:", aggsummary$overall["n_timepoints"], "\n")
	cat("Number of empty timepoints:", aggsummary$overall["n_empty_timepoints"], "\n")
	cat("\n")
	cat("By field:\n")
	print(aggsummary$byfield)
}

# summarise aggregated data
# TODO: consider making this a generic summary() method instead.
#       Help file says summary() is for models but there are a bunch of other objects implementing it too
summarise_aggregated_data <- function(cpdaggregate){
	#temp assignment
	# cpdaggregate<-testcpddata_byday

	aggfields <- cpdaggregate$aggregatefields

	# summary info for overall dataset
	# use timepoint column to illustrate overall counts
	overall <- c(n_fields = length(aggfields),
							 timepoint_fieldname = cpdaggregate$timepoint_fieldname,
							 timepoint_unit = cpdaggregate$timepoint_unit,
							 timepoint_min = format(min(aggfields[[cpdaggregate$timepoint_fieldname]]$values[[1]])),
							 timepoint_max = format(max(aggfields[[cpdaggregate$timepoint_fieldname]]$values[[1]])),
							 n_timepoints = length(aggfields[[cpdaggregate$timepoint_fieldname]]$values[[1]]),
							 n_empty_timepoints = length(aggfields[[cpdaggregate$timepoint_fieldname]]$changepoints[["n"]][["is_zero"]]$istrue_indexes)
	)

	# summary info for each column in dataset
	byfield <- data.frame(fieldname=character(),
												aggregatetype=character(),
												changepoint_method=character(),
												# changepoint_method_params=character(),
												n_changepoints=integer(),
												n_outliers=integer(),
												n_istrues=integer(),
												stringsAsFactors = FALSE
	)
	for(i in 1:length(aggfields)){
		for(j in 1:length(aggfields[[i]]$changepoints)){
			cpfield <- aggfields[[i]]$changepoints[[j]]
			for(k in 1:length(cpfield)){
				byfield <- rbind(byfield,
												 data.frame(fieldname=aggfields[[i]]$columnname,
												 					 aggregatetype=names(aggfields[[i]]$changepoints[j]),
												 					 changepoint_method=cpfield[[k]]$changepoint_method,
												 					 # changepoint_method_params=cpfield[[k]]$changepoint_method_params,
												 					 n_changepoints=ifelse(is.null(cpfield[[k]]$changepoint_indexes), NA, length(cpfield[[k]]$changepoint_indexes)),
												 					 n_outliers=ifelse(is.null(cpfield[[k]]$outlier_indexes), NA, length(cpfield[[k]]$outlier_indexes)),
												 					 n_istrues=ifelse(is.null(cpfield[[k]]$istrue_indexes), NA, length(cpfield[[k]]$istrue_indexes)),
												 					 stringsAsFactors = FALSE
												 )
				)
			}
		}
	}
	# byfield2 <- aggregate(cbind(n_changepoints=cpdaggregate$changepoints_df$changepointtype == "edge", n_outliers=cpdaggregate$changepoints_df$changepointtype == "outlier"),
	#                       by = list(fieldname=cpdaggregate$changepoints_df$fieldname, aggregatetype=cpdaggregate$changepoints_df$aggregatetype, changepointmethod=cpdaggregate$changepoints_df$changepointmethod),
	#                       FUN = sum,
	#                       drop = TRUE)

	structure(
		list(
			overall = overall, byfield = byfield
		),
		class = "summary_aggregated_data"
	)

}


