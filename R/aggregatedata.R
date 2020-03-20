# Code for creation of aggregatedata object
# these contain both the (vector) data for the aggregated field and the relevant metadata
# TODO: decide whether better to store values as a dataframe or a list of ts/zoo objects

# -----------------------------------------------------------------------------
# individual aggregatefields
# NOTE: keep datafield and timepointfieldvalues separate as timepointfieldvalues are updated for subaggregates before being passed in
aggregatefield <- function(datafield, timepointfieldvalues, alltimepoints, aggregation_timeunit, changepointmethods = "all", partitionfieldname = NULL, partitionfieldvalue = NULL, showprogress = FALSE) {
	#temp assignment
	#datafield = testcpdsourcedata$datafields[[2]]
	#timepointfieldvalues = testcpdsourcedata$datafields[[testcpdsourcedata$timepoint_fieldname]][["values"]]
	#timepointfieldvalues = timepointsubvalues
	#showprogress = TRUE
	#aggregation_timeunit = "day"

	#functionlist = c("n", "missing_n", "missing_perc")
	log_message(paste0("Preparing..."), showprogress)
	functionlist = datafield$fieldtype$aggfunctions

	# TODO: validate functionlist at the start
	#stop(paste("Unrecognised aggregation type:", f), call. = FALSE)

	log_message(paste0("Aggregating ", get_datafield_basetype(datafield)," field..."), showprogress)

	# TODO: consider doing this by reference
	datafield_dt <- data.table::data.table("timepoint" = unname(timepointfieldvalues), "values" = datafield[["values"]][[1]], key = "timepoint")
	groupedvals <- data.table::as.data.table(alltimepoints)
	data.table::setkey(groupedvals)

	# need a counter to allow for functions that create multiple columns
	c <- 1
	for( i in seq_along(functionlist) ){
		f <- functionlist[i]
		log_message(paste0("  By ", f), showprogress)
		if( all(datafield_dt[, is.na(values)]) ){
			# NOTE: when all values are NA, class defaults to logical
			groupedvals[, (f) := as.numeric(NA)]
			c <- c + 1
		} else if( f %in% c("subcat_n","subcat_perc") ){
			# create a separate column per category value. Missing values are already covered below
			distinctcategories <- sort(datafield_dt[ is.na(values) == FALSE, unique(values)])
			log_message(paste0("    ", length(distinctcategories), " categories found"), showprogress)
			# If there is only one category, don't bother
			if( length(distinctcategories) > 1 ){
				# TODO: consider setting a max number of categories
				for( j in seq_along(distinctcategories) ){
					log_message(paste0("    ", j, ": ", distinctcategories[j]), showprogress)
					catval <- distinctcategories[j]
					catname <- paste0(f, "_", j, "_", gsub('([[:punct:]])|\\s+', "_", catval))
					if( f == "subcat_n" ){
						groupedvals[datafield_dt[, .("value" = sum(values==catval, na.rm = TRUE))
																		 , .("timepoint" = timepoint_as_aggregationunit(timepoint, aggregation_timeunit = aggregation_timeunit))]
												, (catname) := value, by = .EACHI]
					} else if( f == "subcat_perc" ){
						# include all values in denominator, including NA and NaN
						groupedvals[datafield_dt[, .("value" = 100*sum(values==catval, na.rm = TRUE)/length(values))
																		 , .("timepoint" = timepoint_as_aggregationunit(timepoint, aggregation_timeunit = aggregation_timeunit))]
												, (catname) := value, by = .EACHI]
					}
					c <- c + 1
				}
			}
		} else {
			c <- c + 1
			if( f == "n" ){
				groupedvals[datafield_dt[, .("value" = sum(!(is.na(values) & !is.nan(values))))
																 , .("timepoint" = timepoint_as_aggregationunit(timepoint, aggregation_timeunit = aggregation_timeunit))]
						, (f) := value, by = .EACHI]
			} else if( f == "missing_n" ){
				groupedvals[datafield_dt[, .("value" = sum(is.na(values) & !is.nan(values)))
																 , .("timepoint" = timepoint_as_aggregationunit(timepoint, aggregation_timeunit = aggregation_timeunit))]
										, (f) := value, by = .EACHI]
			} else if( f == "missing_perc" ){
				groupedvals[datafield_dt[, .("value" = 100*sum(is.na(values) & !is.nan(values))/length(values))
																 , .("timepoint" = timepoint_as_aggregationunit(timepoint, aggregation_timeunit = aggregation_timeunit))]
										, (f) := value, by = .EACHI]
			} else if( f == "nonconformant_n" ){
				groupedvals[datafield_dt[, .("value" = sum(is.nan(values)))
																 , .("timepoint" = timepoint_as_aggregationunit(timepoint, aggregation_timeunit = aggregation_timeunit))]
										, (f) := value, by = .EACHI]
			} else if( f == "nonconformant_perc" ){
				# TODO: should the denominator be all rows or only conformant/nonmissing rows?
				groupedvals[datafield_dt[, .("value" = 100*sum(is.nan(values))/length(values))
																 , .("timepoint" = timepoint_as_aggregationunit(timepoint, aggregation_timeunit = aggregation_timeunit))]
										, (f) := value, by = .EACHI]

			} else if( f == "sum" ){
				groupedvals[datafield_dt[, .("value" = sum(values, na.rm = TRUE))
																 , .("timepoint" = timepoint_as_aggregationunit(timepoint, aggregation_timeunit = aggregation_timeunit))]
										, (f) := value, by = .EACHI]
			} else if( f == "nonzero_perc" ){
				groupedvals[datafield_dt[, .("value" = 100*length(which(values>0))/length(values[!is.na(values)]))
																 , .("timepoint" = timepoint_as_aggregationunit(timepoint, aggregation_timeunit = aggregation_timeunit))]
										, (f) := value, by = .EACHI]
			} else if( f == "distinct" ){
				groupedvals[datafield_dt[, .("value" = length(unique(values[!is.na(values)])))
																 , .("timepoint" = timepoint_as_aggregationunit(timepoint, aggregation_timeunit = aggregation_timeunit))]
										, (f) := value, by = .EACHI]
			} else if( f == "midnight_n" ){
				# TODO: if n is zero, should this be zero or NA?
				groupedvals[datafield_dt[, .("value" = sum(format(values, format = "%T") == "00:00:00", na.rm = TRUE))
																 , .("timepoint" = timepoint_as_aggregationunit(timepoint, aggregation_timeunit = aggregation_timeunit))]
										, (f) := value, by = .EACHI]
			} else if( f == "midnight_perc" ){
				# TODO: if n is zero, should this be zero or NA?
				groupedvals[datafield_dt[, .("value" = 100*sum(format(values, format = "%T") == "00:00:00", na.rm = TRUE)/length(values[!is.na(values)]))
																 , .("timepoint" = timepoint_as_aggregationunit(timepoint, aggregation_timeunit = aggregation_timeunit))]
										, (f) := value, by = .EACHI]
			} else if( f == "min" ){
				# NOTE: need the as.double() because min/max returns integer if all values are NA (in the group), and if a mixture of doubles and integers are returned, data.table doesn't like it (though the error only seems to appear when using the package and not when testing inside the package itself)
				groupedvals[datafield_dt[, .("value" = suppressWarnings(as.double(min(values, na.rm = TRUE))))
																 , .("timepoint" = timepoint_as_aggregationunit(timepoint, aggregation_timeunit = aggregation_timeunit))]
										, (f) := value, by = .EACHI]
				groupedvals[is.infinite(f), (f) := NA]
			} else if( f == "max" ){
				# NOTE: need the as.double() because min/max returns integer if all values are NA (in the group), and if a mixture of doubles and integers are returned, data.table doesn't like it (though the error only seems to appear when using the package and not when testing inside the package itself)
				groupedvals[datafield_dt[, .("value" = suppressWarnings(as.double(max(values, na.rm = TRUE))))
																 , .("timepoint" = timepoint_as_aggregationunit(timepoint, aggregation_timeunit = aggregation_timeunit))]
										, (f) := value, by = .EACHI]
				groupedvals[is.infinite(f), (f) := NA]
			} else if( f == "mean" ){
				groupedvals[datafield_dt[, .("value" = mean(values, na.rm = TRUE))
																 , .("timepoint" = timepoint_as_aggregationunit(timepoint, aggregation_timeunit = aggregation_timeunit))]
										, (f) := value, by = .EACHI]
			} else if( f == "median" ){
				groupedvals[datafield_dt[, .("value" = stats::median(values, na.rm = TRUE))
																 , .("timepoint" = timepoint_as_aggregationunit(timepoint, aggregation_timeunit = aggregation_timeunit))]
										, (f) := value, by = .EACHI]
			} else if( f == "minlength" ){
				# NOTE: need the as.double() because min/max returns integer if all values are NA (in the group), and if a mixture of doubles and integers are returned, data.table doesn't like it (though the error only seems to appear when using the package and not when testing inside the package itself)
				groupedvals[datafield_dt[, .("value" = suppressWarnings(as.double(min(nchar(as.character(values), keepNA = TRUE), na.rm = TRUE))))
																 , .("timepoint" = timepoint_as_aggregationunit(timepoint, aggregation_timeunit = aggregation_timeunit))]
										, (f) := value, by = .EACHI]
				groupedvals[is.infinite(f), (f) := NA]
			} else if( f == "maxlength" ){
				# NOTE: need the as.double() because min/max returns integer if all values are NA (in the group), and if a mixture of doubles and integers are returned, data.table doesn't like it (though the error only seems to appear when using the package and not when testing inside the package itself)
				groupedvals[datafield_dt[, .("value" = suppressWarnings(as.double(max(nchar(as.character(values), keepNA = TRUE), na.rm = TRUE))))
																 , .("timepoint" = timepoint_as_aggregationunit(timepoint, aggregation_timeunit = aggregation_timeunit))]
										, (f) := value, by = .EACHI]
				groupedvals[is.infinite(f), (f) := NA]
			} else if( f == "meanlength" ){
				groupedvals[datafield_dt[, .("value" = mean(nchar(as.character(values), keepNA = TRUE), na.rm = TRUE))
																 , .("timepoint" = timepoint_as_aggregationunit(timepoint, aggregation_timeunit = aggregation_timeunit))]
										, (f) := value, by = .EACHI]
			} else{
				# TODO: Decide if this should stop everything or just raise a warning
				# TODO: Putting it here means it doesn't get called if all values are NA
				stop(paste("Unrecognised aggregation type:", f),
						 call. = FALSE)
			}
		}
	}

	log_message(paste0("Tidying up..."), showprogress)
	# for timepoints with no records, replace n with 0, but leave other aggvalues as na
	if( "n" %in% functionlist ){
		groupedvals[is.na(n), "n" := 0]
	}

	# TODO: set individual min/max dates per aggfield? Probably shouldn't calculate changepoints in sections where there are no records for that aggfield
	cpts <- find_changepoints(groupedvals, method = changepointmethods, showprogress = showprogress)[[1]]

	log_message(paste0("Finished"), showprogress)

	structure(list(values = groupedvals,
								 functionlist = functionlist,
								 changepoints = cpts,
								 fieldtype = datafield$fieldtype,
								 columnname = datafield$columnname,
								 partitionfieldname = partitionfieldname,
								 partitionfieldvalue = partitionfieldvalue),
						class = "aggregatefield")
}

is.aggregatefield <- function(x) inherits(x, "aggregatefield")

# -----------------------------------------------------------------------------
# aggregatefield for all fields combined
# uses results from already-aggregated individual fields rather than doing it all again
# TODO: do we want to include duplicates in here too?
aggregateallfields <- function(aggfields, timepointfieldvalues, alltimepoints, changepointmethods = "all", partitionfieldname = NULL, partitionfieldvalue = NULL, showprogress = FALSE) {
	#temp assignment
	# aggfields = agg[1:data$cols_imported_n]
	# timepointfieldvalues = get_datafield_vector(data$datafields[[data$timepoint_fieldname]])
	#showprogress = TRUE

	ft <- ft_allfields()
	functionlist <- ft$aggfunctions

	groupedvals <- data.table::data.table(alltimepoints, key = names(alltimepoints))

	for (i in seq_along(aggfields)){
		for( j in 2:length(names(aggfields[[i]][["values"]])) ){
			# TODO: ideally want to use the aggfunctions list from the allfields fieldtype rather than hard code
			if (names(aggfields[[i]][["values"]])[j] %in% c("n","missing_n","nonconformant_n")){
				f <- names(aggfields[[i]][["values"]])[j]
				if( f %in% names(groupedvals) ){
					# If all values are NA then leave as NA, but if any values are not NA then ignore the NAs (per timepoint)
					groupedvals[, (f) := data.table::fifelse(is.na(aggfields[[i]][["values"]][, get(f)]), rowSums(cbind(get(f), aggfields[[i]][["values"]][, get(f)]), na.rm = FALSE), rowSums(cbind(get(f), aggfields[[i]][["values"]][, get(f)]), na.rm = TRUE))]
				} else{
					groupedvals[, (f) := aggfields[[i]][["values"]][, get(f)]]
				}
			}
		}
	}
	# TODO: Should timepoint field be included too?
	groupedvals[, "missing_perc" := 100*missing_n/(n + missing_n + nonconformant_n)]
	groupedvals[, "nonconformant_perc" := 100*nonconformant_n/(n + missing_n + nonconformant_n)]

	# duplicates
	# need to recreate clean_df
	# x < data.frame()
	# groupedvals <- merge(groupedvals, stats::aggregate(x, by = groupbylist, FUN = function(x){sum(duplicated(x))}, drop = FALSE), by.x = names(alltimepointslist), by.y = "Group.1", all = TRUE)

	cpts <- find_changepoints(groupedvals, method = changepointmethods, showprogress = showprogress)[[1]]

	log_message(paste0("Finished"), showprogress)

	structure(list(values = groupedvals,
								 functionlist = functionlist,
								 changepoints = cpts,
								 fieldtype = ft,
								 columnname = "ALLFIELDSCOMBINED",
								 partitionfieldname = partitionfieldname,
								 partitionfieldvalue = partitionfieldvalue),
						class = "aggregatefield")
}

# -----------------------------------------------------------------------------
#' Aggregate source data
#'
#' Aggregates sourcedata object based on fieldtypes specified at load time.
#' Default time period for aggregation is a calendar day
#'
#' @param data A \code{sourcedata} object
#' @param aggregation_timeunit Unit of time to aggregate over. Specify one of "day", "week", "month", "quarter", "year". The "week" option is Monday-based. Default = "day"
#' @param changepointmethods String vector of changepoint methods to apply, or "all" or "none". Defaults to "all".
#' @param showprogress Print progress to console. Default = FALSE
#' @return A \code{aggregatedata} object
#' @export
aggregate_data <- function(data, aggregation_timeunit = "day", changepointmethods = "all", showprogress = FALSE){
	# TODO: move calculation of changepoints into separate function and allow them to be calculated afterwards
	# TODO: allow user to override existing aggfunctions?
	#temp assignment
	# data<-testcpdsourcedata
	# aggregation_timeunit = "day"
	# changepointmethods = "none"
	# showprogress = TRUE

	log_function_start(match.call()[[1]])

	# create column to group by
	# TODO: raise an error/warning if data is less granular than aggregation_timeunit
	if( !(aggregation_timeunit %in% c("day","week","month","quarter","year")) ) {
		stop("Invalid aggregation_timeunit [", aggregation_timeunit, "]. Values allowed are: day, week, month, quarter, year", call. = FALSE)
	}
	log_message(paste0("Aggregating by [", aggregation_timeunit, "]..."), showprogress)
	# need to ensure have all possible timepoint values, even if they are missing in the dataset
	alltimepoints_min <- timepoint_as_aggregationunit(get_datafield_min(data$datafields[[data$timepoint_fieldname]]), aggregation_timeunit)
	alltimepoints_max <- timepoint_as_aggregationunit(get_datafield_max(data$datafields[[data$timepoint_fieldname]]), aggregation_timeunit)
	alltimepoints <- data.table::data.table(seq(alltimepoints_min, alltimepoints_max, by = aggregation_timeunit))
	names(alltimepoints) <- paste0(data$timepoint_fieldname, "_by", aggregation_timeunit)

	### AGGREGATE OVERALL DATASET
	log_message(paste0("Aggregating overall dataset..."), showprogress)
	# load aggregated data into new vector
	log_message(paste0("Aggregating each datafield in turn..."), showprogress)
	agg <- vector("list", data$cols_imported_n + 2)
	for (i in 1:data$cols_imported_n){
		log_message(paste0(i, ": ", names(data$cols_imported_indexes)[i]), showprogress)
		fieldindex <- data$cols_imported_indexes[[i]]
		#  Preparation for placing subaggregates as children on each overall aggfield
		#    agg[[i]] <- c(aggregatefield(data$datafields[[fieldindex]], alltimepointslist, groupbylist, showprogress = showprogress), subaggregates=vector("list", 0))
		#    agg[[i]] <- c(aggregatefield(data$datafields[[fieldindex]], alltimepointslist, groupbylist, showprogress = showprogress), subaggregates=NA)
		agg[[i]] <- aggregatefield(data$datafields[[fieldindex]], get_datafield_vector(data$datafields[[data$timepoint_fieldname]]), alltimepoints, aggregation_timeunit, changepointmethods = changepointmethods, showprogress = showprogress)
	}
	agg[[data$cols_imported_n+1]] <- aggregatefield(data$datafields[[data$cols_source_n+1]], get_datafield_vector(data$datafields[[data$timepoint_fieldname]]), alltimepoints, aggregation_timeunit, changepointmethods = changepointmethods, showprogress = showprogress)
	agg[[data$cols_imported_n+2]] <- aggregateallfields(agg[1:data$cols_imported_n], get_datafield_vector(data$datafields[[data$timepoint_fieldname]]), alltimepoints, aggregation_timeunit, changepointmethods = changepointmethods, showprogress = showprogress)
	names(agg) <- c(names(data$cols_imported_indexes), "DUPLICATES", "ALLFIELDSCOMBINED")

	log_message(paste0("Creating changepoint dataframe..."), showprogress)
	changepoints_df <- all_changepoints(agg)


	### AGGREGATE BY EACH PARTITIONFIELD SUBGROUP
	# set up lists of correct size first
	log_message(paste0("Checking for fields of type 'partition'..."), showprogress)
	partitionfield_fieldnames <- character()
	partitionfield_indexes <- numeric()
	# NOTE: could probably just loop through all datafields here, not sure of value of only checking the imported ones
	for (i in 1:data$cols_imported_n){
		fieldindex = data$cols_imported_indexes[[i]]
		if (is.fieldtype_partition(data$datafields[[fieldindex]]$fieldtype)){
			partitionfield_indexes <- c(partitionfield_indexes, fieldindex)
			partitionfield_fieldnames <- c(partitionfield_fieldnames, data$datafields[[fieldindex]]$columnname)
		}
	}
	subaggregate <- vector("list", length(partitionfield_indexes))
	if( length(partitionfield_indexes)==0 ){
		log_message(paste0("None found"), showprogress)
	} else{
		for (i in seq_along(partitionfield_indexes)){
			partitionfield_name <- data$datafields[[partitionfield_indexes[[i]]]]$columnname
			log_message(paste0("Aggregating by ", partitionfield_name, "..."), showprogress)
			# create factor of partitionfield subgroups
			# TODO: not sure what's going to happen if NA values are present
			partitionfield_levels <- unlist(unique(data$datafields[[partitionfield_indexes[[i]]]]$values))
			# if there is only one value then don't bother
			if( length(partitionfield_levels) <= 1 ){
				log_message(paste0(length(partitionfield_levels), " unique value(s) found. Skip."), showprogress)
			} else{
				subaggregate[[i]] <- vector("list", length(partitionfield_levels))
				for (j in seq_along(partitionfield_levels)){
					log_message(paste0("  Filter on ", partitionfield_name, "=", partitionfield_levels[j], ":"), showprogress)
					# set timepoint vector to ignore other levels
					partitionfield_levelindicator <- which(data$datafields[[partitionfield_indexes[[i]]]]$values != partitionfield_levels[j])
					timepointsubvalues <- get_datafield_vector(data$datafields[[data$timepoint_fieldname]])
					timepointsubvalues[partitionfield_levelindicator] <- NA
					log_message(paste0("    Aggregating each datafield in turn..."), showprogress)
					subaggregate[[i]][[j]]$aggregatefields <- vector("list", data$cols_imported_n + 1)
					nextindex <- 1
					for (k in 1:data$cols_imported_n){
						log_message(paste0("      ", k, ": ", names(data$cols_imported_indexes)[k]), showprogress)
						fieldindex = data$cols_imported_indexes[[k]]
						if (fieldindex != partitionfield_indexes[[i]]){
							subaggregate[[i]][[j]]$aggregatefields[[nextindex]] <- aggregatefield(data$datafields[[fieldindex]], timepointfieldvalues = timepointsubvalues, alltimepoints = alltimepoints, aggregation_timeunit = aggregation_timeunit, changepointmethods = changepointmethods, partitionfieldname = partitionfield_name, partitionfieldvalue = partitionfield_levels[[j]], showprogress = showprogress)
							nextindex <- nextindex + 1
						}
					}
					log_message(paste0("Aggregating calculated datafields..."), showprogress)
					subaggregate[[i]][[j]]$aggregatefields[[data$cols_imported_n]] <- aggregatefield(data$datafields[[data$cols_source_n]], timepointfieldvalues = timepointsubvalues, alltimepoints = alltimepoints, aggregation_timeunit, changepointmethods = changepointmethods, partitionfieldname = partitionfield_name, partitionfieldvalue = partitionfield_levels[[j]], showprogress = showprogress)
					subaggregate[[i]][[j]]$aggregatefields[[data$cols_imported_n+1]] <- aggregateallfields(subaggregate[[i]][[j]]$aggregatefields[1:data$cols_imported_n - 1], timepointfieldvalues = timepointsubvalues, alltimepoints = alltimepoints, changepointmethods = changepointmethods, partitionfieldname = partitionfield_name, partitionfieldvalue = partitionfield_levels[[j]], showprogress = showprogress)
					names(subaggregate[[i]][[j]]$aggregatefields) <- c(names(data$cols_imported_indexes)[-which(data$cols_imported_indexes==partitionfield_indexes[[i]])], "DUPLICATES", "ALLFIELDSCOMBINED")

					log_message(paste0("Creating changepoint dataframe..."), showprogress)
					subaggregate[[i]][[j]]$changepoints_df <- all_changepoints(subaggregate[[i]][[j]]$aggregatefields)
					subaggregate[[i]][[j]]$timepoint_fieldname <- data$timepoint_fieldname
					subaggregate[[i]][[j]]$aggregation_timeunit <- aggregation_timeunit
				}
				names(subaggregate[[i]]) <- partitionfield_levels
			}
		}
		names(subaggregate) <- names(data$cols_imported_indexes)[which(data$cols_imported_indexes %in% partitionfield_indexes)]


		#   # Place subaggregates as children on each overall aggfield
		#   # TODO: This doesn't yet work if you have >1 partition fieldtype
		#   for (k in seq_along(agg)){
		#   	if (names(agg[k]) != names(data$cols_imported_indexes)[which(data$cols_imported_indexes==partitionfield_indexes[[1]])]){
		#   		agg[[k]]$subaggregates <- vector("list", length(partitionfield_indexes))
		# 	  	for (i in seq_along(partitionfield_indexes)){
		# 	  			agg[[k]]$subaggregates[[i]] <- vector("list", length(partitionfield_levels))
		# 	  			for (j in seq_along(partitionfield_levels)){
		# 	  				agg[[k]]$subaggregates[[i]][[j]] <- subaggregate[[i]][[j]][[1]][[k]]
		# 	  			}
		# 	  			names(agg[[k]]$subaggregates[[i]]) <- paste0(names(data$cols_imported_indexes)[which(data$cols_imported_indexes==partitionfield_indexes[[i]])], "_", partitionfield_levels)
		# 	  	}
		# 	  	names(agg[[k]]$subaggregates) <- paste0(names(agg[k]), "_by_", names(data$cols_imported_indexes)[which(data$cols_imported_indexes %in% partitionfield_indexes)])
		#   	}
		#   }
	}

	log_function_end(match.call()[[1]])

	structure(
		list(
			aggregatefields = agg,
			changepoints_df = changepoints_df,
			timepoint_fieldname = data$timepoint_fieldname,
			aggregation_timeunit = aggregation_timeunit, # not sure if this should be set at overall object level or allow it to differ per aggregatefield
			partitionfield_fieldnames = partitionfield_fieldnames,
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

	# export a file for each partition field in dataset
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
	cat("Timepoint aggregation unit:", aggsummary$overall["aggregation_timeunit"], "\n")
	cat("Min timepoint value:", aggsummary$overall["timepoint_min"], "\n")
	cat("Max timepoint value:", aggsummary$overall["timepoint_max"], "\n")
	cat("Total number of timepoints:", aggsummary$overall["n_timepoints"], "\n")
	cat("Number of empty timepoints:", aggsummary$overall["n_empty_timepoints"], "\n")
	cat("Column(s) used as partitionfield:", aggsummary$overall["partitionfield_fieldnames"], "\n")
	cat("\n")
	cat("Change points by field:\n")
	if( nrow(aggsummary$byfield) > 0){
		print(aggsummary$byfield)
	} else{
		cat("Change points not calculated.\n")
	}
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
							 aggregation_timeunit = aggregatedata$aggregation_timeunit,
							 timepoint_min = format(min(aggfields[[aggregatedata$timepoint_fieldname]]$values[[1]])),
							 timepoint_max = format(max(aggfields[[aggregatedata$timepoint_fieldname]]$values[[1]])),
							 n_timepoints = length(aggfields[[aggregatedata$timepoint_fieldname]]$values[[1]]),
							 n_empty_timepoints = length(aggfields[[aggregatedata$timepoint_fieldname]]$changepoints[["n"]][["is_zero"]]$istrue_indexes),
							 partitionfield_fieldnames = toString(aggregatedata$partitionfield_fieldnames)
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
				subaggs[[p]][[q]]$overall <- c(subaggs[[p]][[q]]$overall, partitionfield_fieldname=aggregatedata$partitionfield_fieldnames[[p]], partitionfield_fieldvalue=names(aggregatedata$subaggregates[[p]][q]))
			}
		}
	}
	else{
		subaggs <- NA
	}

	structure(
		list(
			overall = overall, byfield = byfield, bypartitionfield = subaggs
		),
		class = "summary_aggregated_data"
	)

}

# convert timepoint value to desired aggregation unit
timepoint_as_aggregationunit <- function(x, aggregation_timeunit){
	if( aggregation_timeunit == "day" ){
		as.Date(x)
	} else if( aggregation_timeunit == "week" ){
		# use the prior (or current) Monday
		as.Date(x) - (as.integer(format(x, format = "%u")) - 1)
	} else if( aggregation_timeunit == "month" ){
		as.Date(format(x, format = "%Y-%m-01"))
	} else if( aggregation_timeunit == "quarter" ){
		as.Date(sapply(x, function(x){ format(x, paste0("%Y-"
																						, switch(format(x, format = "%m")
																										 , "01" = "01", "02" = "01", "03" = "01"
																										 , "04" = "04", "05" = "04", "06" = "04"
																										 , "07" = "07", "08" = "07", "09" = "07"
																										 , "10" = "10", "11" = "10", "12" = "10"
																										)
																						, "-01")) }))
	} else if( aggregation_timeunit == "year" ){
		as.Date(format(x, format = "%Y-01-01"))
	}
}
