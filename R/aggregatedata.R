# Code for creation of aggregatedata object
# these contain both the (vector) data for the aggregated field and the relevant metadata
# TODO: decide whether better to store values as a dataframe or a list of ts/zoo objects

# -----------------------------------------------------------------------------
# individual aggregatefields
# uses stats::aggregate function, consider trying the aggregate.ts function instead, or group_by tibble function
aggregatefield <- function(datafield, alltimepoints, groupbylist, changepointmethods = "all", sourcefieldname = NULL, sourcefieldvalue = NULL, showprogress = FALSE) {
  #temp assignment
   #datafield = testcpdsourcedata$datafields[[6]]
    # groupbylist = list(groupby_day[[1]])
     #alltimepoints = timepoint_byday
  #showprogress = TRUE

  #functionlist = c("n", "missing_n", "missing_perc")
  log_message(paste0("Preparing..."), showprogress)
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
  log_message(paste0("Aggregating ", typeof(x)," field..."), showprogress)
  # if (typeof(x) == "character") {
  #   groupedvals <- do.call(data.frame, stats::aggregate(x, by = groupbylist, FUN = function(x){c("n" = sum(!is.na(x)),
  #                                                                                                   "missing_n" = sum(is.na(x)),
  #                                                                                                   "missing_perc" = 100*sum(is.na(x))/length(x),
  #                                                                                                   "distinct" = length(unique(x[!is.na(x)])))[functionlist]}, drop = FALSE))
  # } else {
  #   # suppress warnings for min/max as they complain when all values are NA. TODO: Ideally suppress just that particular warning and not others
  #   groupedvals <- do.call(data.frame, stats::aggregate(x, by = groupbylist, FUN = function(x){c("n" = sum(!is.na(x)),
  #                                                                                               "missing_n" = sum(is.na(x)),
  #                                                                                               "missing_perc" = 100*sum(is.na(x))/length(x),
  #                                                                                               "distinct" = length(unique(x[!is.na(x)])),
  #                                                                                               "min" = suppressWarnings(min(x, na.rm = TRUE)),
  #                                                                                               "max" = suppressWarnings(max(x, na.rm = TRUE)))[functionlist]}, drop = FALSE))
  #   # tidy up
  #   # min/max return Inf when all values are NA. Replace them with NA as don't want to differentiate these from timepoints where there are no records
  #   # min/max also drops datetime class
  #   for( c in c("min","max") ){
  #     if (c %in% functionlist){
  #       xc <- paste0("x.", c)
  #       groupedvals[[xc]][is.infinite(groupedvals[[xc]])] <- NA
  #       # preserve datatypes
  #       if( inherits(x,"POSIXct") ){
  #         groupedvals[[xc]] <- as.POSIXct(groupedvals[[xc]], tz = "UTC", origin = "1970-01-01")
  #       }
  #     }
  #   }
  # }


  groupedvals <- alltimepoints
  for( i in seq_along(functionlist) ){
  	f <- functionlist[i]
  	log_message(paste0("  By ", f), showprogress)
  	if( all(is.na(x)) ){
  		groupedvals <- data.frame(groupedvals, x = NA)
  	} else {
  		if( f == "n" ){
	  		groupedvals <- merge(groupedvals, stats::aggregate(x, by = groupbylist, FUN = function(x){sum(!(is.na(x) & !is.nan(x)))}, drop = FALSE), by.x = names(alltimepoints), by.y = "Group.1", all = TRUE)
	  	} else if( f == "missing_n" ){
	  		groupedvals <- merge(groupedvals, stats::aggregate(x, by = groupbylist, FUN = function(x){sum(is.na(x) & !is.nan(x))}, drop = FALSE), by.x = names(alltimepoints), by.y = "Group.1", all = TRUE)
	  	} else if( f == "missing_perc" ){
	  		groupedvals <- merge(groupedvals, stats::aggregate(x, by = groupbylist, FUN = function(x){100*sum(is.na(x) & !is.nan(x))/length(x)}, drop = FALSE), by.x = names(alltimepoints), by.y = "Group.1", all = TRUE)
	  	} else if( f == "nonconformant_n" ){
	  		groupedvals <- merge(groupedvals, stats::aggregate(x, by = groupbylist, FUN = function(x){sum(is.nan(x))}, drop = FALSE), by.x = names(alltimepoints), by.y = "Group.1", all = TRUE)
	  	} else if( f == "nonconformant_perc" ){
	  		groupedvals <- merge(groupedvals, stats::aggregate(x, by = groupbylist, FUN = function(x){100*sum(is.nan(x))/length(x)}, drop = FALSE), by.x = names(alltimepoints), by.y = "Group.1", all = TRUE)
	  	} else if( f == "nonzero_n" ){
	  		groupedvals <- merge(groupedvals, stats::aggregate(x, by = groupbylist, FUN = function(x){sum(x)}, drop = FALSE), by.x = names(alltimepoints), by.y = "Group.1", all = TRUE)
	  	} else if( f == "nonzero_perc" ){
	  		groupedvals <- merge(groupedvals, stats::aggregate(x, by = groupbylist, FUN = function(x){100*sum(x)/length(x)}, drop = FALSE), by.x = names(alltimepoints), by.y = "Group.1", all = TRUE)
	  	} else if( f == "distinct" ){
	  		groupedvals <- merge(groupedvals, stats::aggregate(x, by = groupbylist, FUN = function(x){length(unique(x[!is.na(x)]))}, drop = FALSE), by.x = names(alltimepoints), by.y = "Group.1", all = TRUE)
	  	} else if( f %in% c("min","max","mean","median") ){
	  		# min/max return Inf when all values are NA. Use NA instead as don't want to differentiate these from timepoints where there are no records
	  		if( all(is.na(x)) ){
	  			groupedvals <- cbind(groupedvals, NA)
	  		} else{
	  			if( f == "min" ){
	  				groupedvals <- merge(groupedvals, stats::aggregate(x, by = groupbylist, FUN = function(x){min(x, na.rm = TRUE)}, drop = FALSE), by.x = names(alltimepoints), by.y = "Group.1", all = TRUE)
	  			} else if( f == "max" ){
	  				groupedvals <- merge(groupedvals, stats::aggregate(x, by = groupbylist, FUN = function(x){max(x, na.rm = TRUE)}, drop = FALSE), by.x = names(alltimepoints), by.y = "Group.1", all = TRUE)
	  			} else if( f == "mean" ){
	  				groupedvals <- merge(groupedvals, stats::aggregate(x, by = groupbylist, FUN = function(x){mean(x, na.rm = TRUE)}, drop = FALSE), by.x = names(alltimepoints), by.y = "Group.1", all = TRUE)
	  			} else if( f == "median" ){
	  				groupedvals <- merge(groupedvals, stats::aggregate(x, by = groupbylist, FUN = function(x){stats::median(x, na.rm = TRUE)}, drop = FALSE), by.x = names(alltimepoints), by.y = "Group.1", all = TRUE)
	  			}
	  		}
  			# min/max also drops datetime class
  			# preserve datatypes
  			if( inherits(x,"POSIXct") ){
  				groupedvals[[i+1]] <- as.POSIXct(groupedvals[[i+1]], tz = "UTC", origin = "1970-01-01")
  			}
  		}
  	}
  	colnames(groupedvals)[i+1] <- f
  }

  log_message(paste0("Tidying up..."), showprogress)
  aggfield <- groupedvals
  # for dates with no records, replace n with 0, but leave other aggvalues as na
  if( "n" %in% functionlist ){
  	aggfield[["n"]][is.na(aggfield[["n"]])] <- 0
  }
  # TODO: set individual min/max dates per aggfield? Probably shouldn't calculate changepoints in sections where there are no records for that aggfield

  cpts <- find_changepoints(aggfield, method = changepointmethods, showprogress = showprogress)[[1]]

  log_message(paste0("Finished"), showprogress)

  structure(list(values = aggfield,
  							 functionlist = functionlist,
  							 changepoints = cpts,
  							 fieldtype = datafield$fieldtype,
  							 columnname = datafield$columnname,
  							 sourcefieldname = sourcefieldname,
  							 sourcefieldvalue = sourcefieldvalue),
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
# aggregatefield for all fields combined
# uses results from already-aggregated individual fields
# TODO: do we want to include duplicates in here too?
aggregateallfields <- function(aggfields, alltimepoints, groupbylist, changepointmethods = "all", sourcefieldname = NULL, sourcefieldvalue = NULL, showprogress = FALSE) {
	#temp assignment
	#aggfields = agg[1:(length(agg)-1)]
	#groupbylist = list(groupby_day[[1]])
	#alltimepoints = timepoint_byday
	#showprogress = TRUE

	ft <- ft_allfields()
	functionlist <- ft$aggfunctions

	# TODO: ideally want to use the aggfunctions list from the allfields fieldtype rather than hard code
	groupedvals <- data.frame(alltimepoints, "n" = 0, "missing_n" = 0, "nonconformant_n" = 0)
	for (i in seq_along(aggfields)){
		for( j in 2:length(names(aggfields[[i]][["values"]])) ){
			if (names(aggfields[[i]][["values"]])[j] %in% names(groupedvals)){
				f <- names(aggfields[[i]][["values"]])[j]
				groupedvals[f] <- groupedvals[f] + aggfields[[i]][["values"]][j]
			}
		}
	}
	aggfield <- cbind(groupedvals, "missing_perc" = groupedvals[["missing_n"]]/length(groupbylist[[1]])*100, "nonconformant_perc" = groupedvals[["nonconformant_n"]]/length(groupbylist[[1]])*100)

	# duplicates
	# need to recreate clean_df
	# x < data.frame()
	# groupedvals <- merge(groupedvals, stats::aggregate(x, by = groupbylist, FUN = function(x){sum(duplicated(x))}, drop = FALSE), by.x = names(alltimepoints), by.y = "Group.1", all = TRUE)

	cpts <- find_changepoints(aggfield, method = changepointmethods, showprogress = showprogress)[[1]]

	log_message(paste0("Finished"), showprogress)

	structure(list(values = aggfield,
								 functionlist = functionlist,
								 changepoints = cpts,
								 fieldtype = ft,
								 columnname = "ALLFIELDSCOMBINED",
								 sourcefieldname = sourcefieldname,
								 sourcefieldvalue = sourcefieldvalue),
						class = "aggregatefield")
}

# -----------------------------------------------------------------------------
#' Aggregate source data
#'
#' Aggregates sourcedata object based on fieldtypes specified at load time.
#' Time period for aggregation is a calendar day.  Further options to be added at a later date.
#'
#' @param data A \code{sourcedata} object
#' @param changepointmethods String vector of changepoint methods to apply, or "all" or "none". Defaults to "all".
#' @param showprogress Print progress to console. Default = FALSE
#' @return A \code{aggregatedata} object
#' @export
aggregate_data <- function(data, changepointmethods = "all", showprogress = FALSE){
	# TODO: move calculation of changepoints into separate function or add param to choose cpt methods here
  #temp assignment
  #data<-testcpdsourcedata
	#changepointmethods = "none"
  #showprogress = TRUE

	log_function_start(match.call()[[1]])
	# create column to group by - default to per Day, allow option to change later
	timepoint_unit <- "DAY"
	log_message("Preparing grouping column...", showprogress)
  # need to ensure have all possible timepoint values, even if they are missing in the dataset
  timepoint_byday <- list(seq(as.Date(get_datafield_min(data$datafields[[data$timepoint_fieldname]])), as.Date(get_datafield_max(data$datafields[[data$timepoint_fieldname]])), by="days"))
  names(timepoint_byday) <- paste0(data$timepoint_fieldname, "_byday")
  # stats::aggregate function requires the grouping vector to be passed in as a list so do this once first
  # TODO: if data is e.g. monthly then need to take this into account
  groupby_day <- list(as.Date(get_datafield_vector(data$datafields[[data$timepoint_fieldname]])))

  ### AGGREGATE OVERALL DATASET
  log_message(paste0("Aggregating overall dataset..."), showprogress)
  # load aggregated data into new vector
  log_message(paste0("Aggregating each datafield in turn..."), showprogress)
  agg <- vector("list", data$cols_imported_n + 2)
  for (i in 1:data$cols_imported_n){
    log_message(paste0(i, ": ", names(data$cols_imported_indexes)[i]), showprogress)
    fieldindex <- data$cols_imported_indexes[[i]]
#  Preparation for placing subaggregates as children on each overall aggfield
#    agg[[i]] <- c(aggregatefield(data$datafields[[fieldindex]], timepoint_byday, groupby_day, showprogress = showprogress), subaggregates=vector("list", 0))
#    agg[[i]] <- c(aggregatefield(data$datafields[[fieldindex]], timepoint_byday, groupby_day, showprogress = showprogress), subaggregates=NA)
    agg[[i]] <- aggregatefield(data$datafields[[fieldindex]], timepoint_byday, groupby_day, changepointmethods = changepointmethods, showprogress = showprogress)
  }
  log_message(paste0("Aggregating calculated datafields..."), showprogress)
  agg[[data$cols_imported_n+1]] <- aggregatefield(data$datafields[[data$cols_source_n+1]], timepoint_byday, groupby_day, changepointmethods = changepointmethods, showprogress = showprogress)
  agg[[data$cols_imported_n+2]] <- aggregateallfields(agg[1:data$cols_imported_n+1], timepoint_byday, groupby_day, changepointmethods = changepointmethods, showprogress = showprogress)
  names(agg) <- c(names(data$cols_imported_indexes), "DUPLICATES", "ALLFIELDSCOMBINED")

  log_message(paste0("Creating changepoint dataframe..."), showprogress)
  changepoints_df <- all_changepoints(agg)


  ### AGGREGATE BY EACH SOURCEFIELD SUBGROUP
  # set up lists of correct size first
  log_message(paste0("Checking for fields of type 'source'..."), showprogress)
  sourcefield_fieldnames <- character()
  sourcefield_indexes <- numeric()
  # NOTE: could probably just loop through all datafields here, not sure of value of only checking the imported ones
  for (i in 1:data$cols_imported_n){
  	fieldindex = data$cols_imported_indexes[[i]]
  	if (is.fieldtype_source(data$datafields[[fieldindex]]$fieldtype)){
  		sourcefield_indexes <- c(sourcefield_indexes, fieldindex)
  		sourcefield_fieldnames <- c(sourcefield_fieldnames, data$datafields[[fieldindex]]$columnname)
  	}
  }
  subaggregate <- vector("list", length(sourcefield_indexes))
  if( length(sourcefield_indexes)==0 ){
  	log_message(paste0("None found"), showprogress)
  } else{
	  for (i in seq_along(sourcefield_indexes)){
	  	log_message(paste0("Aggregating by ", data$datafields[[sourcefield_indexes[[i]]]]$columnname, "..."), showprogress)
	  	# create factor of sourcefield subgroups
	  	sourcefield_levels <- unlist(unique(data$datafields[[sourcefield_indexes[[i]]]]$values))
	  	subaggregate[[i]] <- vector("list", length(sourcefield_levels))
	  	for (j in seq_along(sourcefield_levels)){
	  		log_message(paste0("  Filter on ", data$datafields[[sourcefield_indexes[[i]]]]$columnname, "=", sourcefield_levels[j], ":"), showprogress)
	  		# set groupby vector to ignore other levels
	  		sourcefield_levelindicator <- which(data$datafields[[sourcefield_indexes[[i]]]]$values != sourcefield_levels[j])
	  		sourcefield_groupby_day <- groupby_day
	  		sourcefield_groupby_day[[1]][sourcefield_levelindicator] <- NA
	  		log_message(paste0("    Aggregating each datafield in turn..."), showprogress)
	  		subaggregate[[i]][[j]]$aggregatefields <- vector("list", data$cols_imported_n + 1)
	  		nextindex <- 1
	  		for (k in 1:data$cols_imported_n){
	  			log_message(paste0("      ", k, ": ", names(data$cols_imported_indexes)[k]), showprogress)
	  			fieldindex = data$cols_imported_indexes[[k]]
	  			if (fieldindex != sourcefield_indexes[[i]]){
	  				subaggregate[[i]][[j]]$aggregatefields[[nextindex]] <- aggregatefield(data$datafields[[fieldindex]], timepoint_byday, sourcefield_groupby_day, changepointmethods = changepointmethods, sourcefieldname = names(data$cols_imported_indexes)[which(data$cols_imported_indexes==sourcefield_indexes[[i]])], sourcefieldvalue = sourcefield_levels[[j]], showprogress = showprogress)
	  				nextindex <- nextindex + 1
	  			}
	  		}
	  		log_message(paste0("Aggregating calculated datafields..."), showprogress)
	  		subaggregate[[i]][[j]]$aggregatefields[[data$cols_imported_n]] <- aggregatefield(data$datafields[[data$cols_source_n + 1]], timepoint_byday, groupby_day, changepointmethods = changepointmethods, sourcefieldname = names(data$cols_imported_indexes)[which(data$cols_imported_indexes==sourcefield_indexes[[i]])], sourcefieldvalue = sourcefield_levels[[j]], showprogress = showprogress)
	  		subaggregate[[i]][[j]]$aggregatefields[[data$cols_imported_n+1]] <- aggregateallfields(subaggregate[[i]][[j]]$aggregatefields[1:data$cols_imported_n - 1], timepoint_byday, sourcefield_groupby_day, changepointmethods = changepointmethods, sourcefieldname = names(data$cols_imported_indexes)[which(data$cols_imported_indexes==sourcefield_indexes[[i]])], sourcefieldvalue = sourcefield_levels[[j]], showprogress = showprogress)
	  		names(subaggregate[[i]][[j]]$aggregatefields) <- c(names(data$cols_imported_indexes)[-which(data$cols_imported_indexes==sourcefield_indexes[[i]])], "DUPLICATES", "ALLFIELDSCOMBINED")

	  		log_message(paste0("Creating changepoint dataframe..."), showprogress)
	  		subaggregate[[i]][[j]]$changepoints_df <- all_changepoints(subaggregate[[i]][[j]]$aggregatefields)
	  		subaggregate[[i]][[j]]$timepoint_fieldname <- data$timepoint_fieldname
	  		subaggregate[[i]][[j]]$timepoint_unit <- timepoint_unit
	  	}
	  	names(subaggregate[[i]]) <- sourcefield_levels
	  }
	  names(subaggregate) <- names(data$cols_imported_indexes)[which(data$cols_imported_indexes %in% sourcefield_indexes)]


#   # Place subaggregates as children on each overall aggfield
#   # TODO: This doesn't yet work if you have >1 source fieldtype
#   for (k in seq_along(agg)){
#   	if (names(agg[k]) != names(data$cols_imported_indexes)[which(data$cols_imported_indexes==sourcefield_indexes[[1]])]){
#   		agg[[k]]$subaggregates <- vector("list", length(sourcefield_indexes))
# 	  	for (i in seq_along(sourcefield_indexes)){
# 	  			agg[[k]]$subaggregates[[i]] <- vector("list", length(sourcefield_levels))
# 	  			for (j in seq_along(sourcefield_levels)){
# 	  				agg[[k]]$subaggregates[[i]][[j]] <- subaggregate[[i]][[j]][[1]][[k]]
# 	  			}
# 	  			names(agg[[k]]$subaggregates[[i]]) <- paste0(names(data$cols_imported_indexes)[which(data$cols_imported_indexes==sourcefield_indexes[[i]])], "_", sourcefield_levels)
# 	  	}
# 	  	names(agg[[k]]$subaggregates) <- paste0(names(agg[k]), "_by_", names(data$cols_imported_indexes)[which(data$cols_imported_indexes %in% sourcefield_indexes)])
#   	}
#   }
  }

  log_function_end(match.call()[[1]])

    structure(
    list(
      aggregatefields = agg,
      changepoints_df = changepoints_df,
      timepoint_fieldname = data$timepoint_fieldname,
      timepoint_unit = timepoint_unit, # not sure if this should be set at overall object level or allow it to differ per aggregatefield
      sourcefield_fieldnames = sourcefield_fieldnames,
      subaggregates = subaggregate
    ),
    class = "aggregatedata"
  )
}

is.aggregatedata <- function(x) inherits(x, "aggregatedata")


# -----------------------------------------------------------------------------
#' Export aggregated data
#'
#' Export aggregated data to disk.  Creates a separate file for each aggregated field in dataset
#'
#' @param aggregatedata A \code{aggregatedata} object
#' @param save_directory String. Full or relative path for save folder, use double blackslashes for nested folders and end with double backslash
#' @param save_filetype String. Filetype extension supported by \code{readr}, currently only csv allowed
#' @export
export_aggregated_data <- function(aggregatedata, save_directory, save_filetype = "csv"){
	#temp assignment
	# aggregatedata<-testcpddata_byday
	# save_directory = ".\\devtesting\\testoutput\\"
	# save_filetype = "csv"

	# TODO: validation checks on params

	# export a file for each source field in dataset
	for( i in seq_along(aggregatedata$aggregatefields) ){
		readr::write_csv(aggregatedata$aggregatefields[[i]]$values,
										 paste0(save_directory, names(aggregatedata$aggregatefields[i]), ".csv") )
	}

	for(p in seq_along(aggregatedata$subaggregates)){
		for( q in seq_along(aggregatedata$subaggregates[[p]]) ){
			for( i in seq_along(aggregatedata$subaggregates[[p]][[q]]$aggregatefields) ){
				readr::write_csv(aggregatedata$subaggregates[[p]][[q]]$aggregatefields[[i]]$values,
												 paste0(save_directory,
												 			 names(aggregatedata$aggregatefields[i]),
												 			 "_by_", names(aggregatedata$subaggregates),
												 			 "_", names(aggregatedata$subaggregates[[p]][q]),
												 			 ".csv"))
			}
		}
	}

}

#' @export
print.aggregatedata <- function(x, ...){
	# TODO: to finish
	aggsummary <- summarise_aggregated_data(x)
	cat("Class: aggregatedata\n")
	cat("\n")
	cat("Overall:\n")
	cat("Number of data fields:", aggsummary$overall["n_fields"], "\n")
	cat("Column used for timepoint:", aggsummary$overall["timepoint_fieldname"], "\n")
	cat("Timepoint aggregation unit:", aggsummary$overall["timepoint_unit"], "\n")
	cat("Min timepoint value:", aggsummary$overall["timepoint_min"], "\n")
	cat("Max timepoint value:", aggsummary$overall["timepoint_max"], "\n")
	cat("Total number of timepoints:", aggsummary$overall["n_timepoints"], "\n")
	cat("Number of empty timepoints:", aggsummary$overall["n_empty_timepoints"], "\n")
	cat("Column(s) used as sourcefield:", aggsummary$overall["sourcefield_fieldnames"], "\n")
	cat("\n")
	cat("By field:\n")
	print(aggsummary$byfield)
}

# summarise aggregated data
# TODO: consider making this a generic summary() method instead.
#       Help file says summary() is for models but there are a bunch of other objects implementing it too
summarise_aggregated_data <- function(aggregatedata){
	#temp assignment
#	 aggregatedata<-testcpddata_byday

	aggfields <- aggregatedata$aggregatefields

	# summary info for overall dataset
	# use timepoint column to illustrate overall counts
	overall <- c(n_fields = length(aggfields),
							 timepoint_fieldname = aggregatedata$timepoint_fieldname,
							 timepoint_unit = aggregatedata$timepoint_unit,
							 timepoint_min = format(min(aggfields[[aggregatedata$timepoint_fieldname]]$values[[1]])),
							 timepoint_max = format(max(aggfields[[aggregatedata$timepoint_fieldname]]$values[[1]])),
							 n_timepoints = length(aggfields[[aggregatedata$timepoint_fieldname]]$values[[1]]),
							 n_empty_timepoints = length(aggfields[[aggregatedata$timepoint_fieldname]]$changepoints[["n"]][["is_zero"]]$istrue_indexes),
							 sourcefield_fieldnames = toString(aggregatedata$sourcefield_fieldnames)
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
	for(i in seq_along(aggfields)){
		for(j in seq_along(aggfields[[i]]$changepoints)){
			cpfield <- aggfields[[i]]$changepoints[[j]]
			for(k in seq_along(cpfield)){
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
	# byfield2 <- aggregate(cbind(n_changepoints=aggregatedata$changepoints_df$changepointtype == "edge", n_outliers=aggregatedata$changepoints_df$changepointtype == "outlier"),
	#                       by = list(fieldname=aggregatedata$changepoints_df$fieldname, aggregatetype=aggregatedata$changepoints_df$aggregatetype, changepointmethod=aggregatedata$changepoints_df$changepointmethod),
	#                       FUN = sum,
	#                       drop = TRUE)

	if( length(aggregatedata$subaggregates) > 0 ){
		subaggs <- vector("list", length(aggregatedata$subaggregates))
		for(p in seq_along(aggregatedata$subaggregates)){
			numlevels <- length(aggregatedata$subaggregates[[p]])
			subaggs[[p]] <- vector("list", numlevels)
			for(q in 1:numlevels){
				subaggs[[p]][[q]] <- summarise_aggregated_data(aggregatedata$subaggregates[[p]][[q]])
				subaggs[[p]][[q]]$overall <- c(subaggs[[q]]$overall, sourcefield_fieldname=aggregatedata$sourcefield_fieldnames[[p]], sourcefield_fieldvalue=names(aggregatedata$subaggregates[[p]][q]))
			}
		}
	}
	else{
		subaggs <- NA
	}

	structure(
		list(
			overall = overall, byfield = byfield, bysourcefield = subaggs
		),
		class = "summary_aggregated_data"
	)

}


