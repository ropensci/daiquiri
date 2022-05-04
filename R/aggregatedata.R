# Code for creation of aggregatedata object
# these contain both the (vector) data for the aggregated field and the relevant metadata
# TODO: decide whether better to store values as a dataframe or a list of ts/zoo objects
# NOTE: Changepoints functionality disabled until we find a method that works (have not removed params from internal functions but instead have
# set them to default to "none")
# NOTE: Partitionfield functionality disabled until we work out how to present it (creating aligned plots only works if small no. of partitions)

# -----------------------------------------------------------------------------
# individual aggregatefields
# NOTE: keep datafield and timepointfieldvalues separate as timepointfieldvalues are updated for subaggregates before being passed in
#' @importFrom data.table ':=' .EACHI
aggregatefield <- function(datafield, timepointfieldvalues, alltimepoints, aggregation_timeunit, changepointmethods = "none", partitionfieldname = NULL, partitionfieldvalue = NULL, showprogress = TRUE) {
	#temp assignment
	#datafield = outpatsourcedata$datafields[[24]]
	#timepointfieldvalues = get_datafield_vector(outpatsourcedata$datafields[[outpatsourcedata$timepoint_fieldname]])
	#timepointfieldvalues = timepointsubvalues
	#showprogress = TRUE
	#aggregation_timeunit = "day"
	#functionlist = c("n", "missing_n", "missing_perc")

	# initialise known column names to prevent R CMD check notes
	n <- value <- values <- timepointgroup <- NULL

	log_message(paste0("Preparing..."), showprogress)
	functionlist = datafield$fieldtype$aggfunctions

	# TODO: validate functionlist at the start
	#stop(paste("Unrecognised aggregation type:", f), call. = FALSE)

	log_message(paste0("Aggregating ", get_datafield_basetype(datafield)," field..."), showprogress)

	# TODO: consider doing this by reference
	# this contains all values present in the original datafield, alongside their timepointgroup
	datafield_dt <- data.table::data.table("timepointgroup" = timepoint_as_aggregationunit(timepointfieldvalues, aggregation_timeunit = aggregation_timeunit), "values" = datafield[["values"]][[1]], key = "timepointgroup")
	# this contains the aggfn values after aggregating (one column per aggfn)
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
						groupedvals[datafield_dt[, list("value" = sum(values==catval, na.rm = TRUE))
																		 , by = list(timepointgroup)]
												, (catname) := value, by = .EACHI]
					} else if( f == "subcat_perc" ){
						# include all values in denominator, including NA and NaN
						groupedvals[datafield_dt[, list("value" = 100*sum(values==catval, na.rm = TRUE)/length(values))
																		 , by = list(timepointgroup)]
												, (catname) := value, by = .EACHI]
					}
					c <- c + 1
				}
			}
		} else {
			c <- c + 1
			if( f == "n" ){
				groupedvals[datafield_dt[, list("value" = sum(!(is.na(values) & !is.nan(values))))
																 , by = list(timepointgroup)]
						, (f) := value, by = .EACHI]
			} else if( f == "missing_n" ){
				groupedvals[datafield_dt[, list("value" = sum(is.na(values) & !is.nan(values)))
																 , by = list(timepointgroup)]
										, (f) := value, by = .EACHI]
			} else if( f == "missing_perc" ){
				groupedvals[datafield_dt[, list("value" = 100*sum(is.na(values) & !is.nan(values))/length(values))
																 , by = list(timepointgroup)]
										, (f) := value, by = .EACHI]
			} else if( f == "nonconformant_n" ){
				groupedvals[datafield_dt[, list("value" = sum(is.nan(values)))
																 , by = list(timepointgroup)]
										, (f) := value, by = .EACHI]
			} else if( f == "nonconformant_perc" ){
				# TODO: should the denominator be all rows or only conformant/nonmissing rows?
				groupedvals[datafield_dt[, list("value" = 100*sum(is.nan(values))/length(values))
																 , by = list(timepointgroup)]
										, (f) := value, by = .EACHI]

			} else if( f == "sum" ){
				groupedvals[datafield_dt[, list("value" = sum(values, na.rm = TRUE))
																 , by = list(timepointgroup)]
										, (f) := value, by = .EACHI]
			} else if( f == "nonzero_perc" ){
				groupedvals[datafield_dt[, list("value" = 100*length(which(values>0))/length(values[!is.na(values)]))
																 , by = list(timepointgroup)]
										, (f) := value, by = .EACHI]
			} else if( f == "distinct" ){
				groupedvals[datafield_dt[, list("value" = length(unique(values[!is.na(values)])))
																 , by = list(timepointgroup)]
										, (f) := value, by = .EACHI]
			} else if( f == "midnight_n" ){
				# TODO: if n is zero, should this be zero or NA?
				groupedvals[datafield_dt[, list("value" = sum(format(values, format = "%T") == "00:00:00", na.rm = TRUE))
																 , by = list(timepointgroup)]
										, (f) := value, by = .EACHI]
			} else if( f == "midnight_perc" ){
				# NOTE: if n is zero, this returns NaN. Update to NA when tidying up
				groupedvals[datafield_dt[, list("value" = 100*sum(format(values, format = "%T") == "00:00:00", na.rm = TRUE)/length(values[!is.na(values)]))
																 , by = list(timepointgroup)]
										, (f) := value, by = .EACHI]
			} else if( f == "min" ){
				# NOTE: need the as.double() because min/max returns integer if all values are NA (in the group), and if a mixture of doubles and integers are returned, data.table doesn't like it (though the error only seems to appear when using the package and not when testing inside the package itself)
				groupedvals[datafield_dt[, list("value" = suppressWarnings(as.double(min(values, na.rm = TRUE))))
																 , by = list(timepointgroup)]
										, (f) := value, by = .EACHI]
			} else if( f == "max" ){
				# NOTE: need the as.double() because min/max returns integer if all values are NA (in the group), and if a mixture of doubles and integers are returned, data.table doesn't like it (though the error only seems to appear when using the package and not when testing inside the package itself)
				groupedvals[datafield_dt[, list("value" = suppressWarnings(as.double(max(values, na.rm = TRUE))))
																 , by = list(timepointgroup)]
										, (f) := value, by = .EACHI]
			} else if( f == "mean" ){
				groupedvals[datafield_dt[, list("value" = mean(values, na.rm = TRUE))
																 , by = list(timepointgroup)]
										, (f) := value, by = .EACHI]
			} else if( f == "median" ){
				groupedvals[datafield_dt[, list("value" = stats::median(values, na.rm = TRUE))
																 , by = list(timepointgroup)]
										, (f) := value, by = .EACHI]
			} else if( f == "minlength" ){
				# NOTE: need the as.double() because min/max returns integer if all values are NA (in the group), and if a mixture of doubles and integers are returned, data.table doesn't like it (though the error only seems to appear when using the package and not when testing inside the package itself)
				groupedvals[datafield_dt[, list("value" = suppressWarnings(as.double(min(nchar(as.character(values), keepNA = TRUE), na.rm = TRUE))))
																 , by = list(timepointgroup)]
										, (f) := value, by = .EACHI]
			} else if( f == "maxlength" ){
				# NOTE: need the as.double() because min/max returns integer if all values are NA (in the group), and if a mixture of doubles and integers are returned, data.table doesn't like it (though the error only seems to appear when using the package and not when testing inside the package itself)
				groupedvals[datafield_dt[, list("value" = suppressWarnings(as.double(max(nchar(as.character(values), keepNA = TRUE), na.rm = TRUE))))
																 , by = list(timepointgroup)]
										, (f) := value, by = .EACHI]
			} else if( f == "meanlength" ){
				groupedvals[datafield_dt[, list("value" = mean(nchar(as.character(values), keepNA = TRUE), na.rm = TRUE))
																 , by = list(timepointgroup)]
										, (f) := value, by = .EACHI]
			} else{
				# TODO: Decide if this should stop everything or just raise a warning
				# TODO: Putting it here means it doesn't get called if all values are NA
				stop(paste("Unrecognised aggregation type:", f),
						 call. = FALSE)
			}
			# min/max return Inf when all values are NA. Use NA instead as don't want to differentiate these from timepoints where there are no records
			if( f %in% c("min","max","mean","minlength","maxlength","midnight_perc") ){
				groupedvals[is.infinite(get(f)) | is.nan(get(f)), (f) := NA]
				# min/max also drops datetime class
				# preserve datatypes
				if( inherits(datafield[["values"]][[1]],"POSIXct") && f %in% c("min","max") ){
					# TODO: make sure this is consistent with data format on loading
					groupedvals[, (f) := as.POSIXct(get(f), tz = "UTC", origin = "1970-01-01")]
				}
			}
		}
	}

	log_message(paste0("Tidying up..."), showprogress)
	# for timepoints with no records, replace n with 0, but leave other aggvalues as na
	if( "n" %in% functionlist ){
		groupedvals[is.na(n), "n" := 0]
	}

	# TODO: set individual min/max dates per aggfield? Probably shouldn't calculate changepoints in sections where there are no records for that aggfield
	# NOTE: Changepoints functionality disabled until we find a method that works
	#cpts <- find_changepoints(groupedvals, method = changepointmethods, showprogress = showprogress)[[1]]

	log_message(paste0("Finished"), showprogress)

	structure(list(values = groupedvals,
								 functionlist = functionlist,
								 # NOTE: Changepoints functionality disabled until we find a method that works
								 # changepoints = cpts,
								 fieldtype = datafield$fieldtype,
								 columnname = datafield$columnname
								 # NOTE: Partitionfield functionality disabled until we work out how to present it
								 # partitionfieldname = partitionfieldname,
								 # partitionfieldvalue = partitionfieldvalue
								 ),
						class = "aggregatefield")
}

is.aggregatefield <- function(x) inherits(x, "aggregatefield")

# -----------------------------------------------------------------------------
# aggregatefield for all fields combined
# uses results from already-aggregated individual fields rather than doing it all again
# TODO: do we want to include duplicates in here too?
# TODO: this field has a numeric datatype whereas individual fields have an int datatype, decide if need to make them all the same
aggregateallfields <- function(aggfields, changepointmethods = "none", partitionfieldname = NULL, partitionfieldvalue = NULL, showprogress = TRUE) {
	#temp assignment
	#showprogress = TRUE

	# initialise known column names to prevent R CMD check notes
	n <- missing_n <- nonconformant_n <- NULL

	ft <- ft_allfields()
	functionlist <- ft$aggfunctions

	groupedvals <- aggfields[[1]][["values"]][,1]

	for (i in seq_along(aggfields)){
		for( j in 2:length(names(aggfields[[i]][["values"]])) ){
			# TODO: ideally want to use the aggfunctions list from the allfields fieldtype rather than hard code
			if (names(aggfields[[i]][["values"]])[j] %in% c("n","missing_n","nonconformant_n")){
				f <- names(aggfields[[i]][["values"]])[j]
				if( f %in% names(groupedvals) ){
					# If all values are NA then leave as NA, but if any values are not NA then ignore the NAs (per timepoint)
					groupedvals[, (f) := data.table::fifelse(is.na(get(f)) & is.na(aggfields[[i]][["values"]][, get(f)]), NA_integer_, rowSums(cbind(get(f), aggfields[[i]][["values"]][, get(f)]), na.rm = TRUE))]
				} else{
					groupedvals[, (f) := aggfields[[i]][["values"]][, get(f)]]
				}
			}
		}
	}
	# if there are no datetime or numeric fields, nonconformant_n field needs to be created explicitly
	if( !("nonconformant_n" %in% names(groupedvals)) ){
		groupedvals[, "nonconformant_n" := data.table::fifelse(n == 0, NA_integer_, 0)]
	}

	groupedvals[, "missing_perc" := 100*missing_n/(n + missing_n + nonconformant_n)]
	groupedvals[, "nonconformant_perc" := 100*nonconformant_n/(n + missing_n + nonconformant_n)]

	# duplicates
	# need to recreate clean_df
	# x < data.frame()
	# groupedvals <- merge(groupedvals, stats::aggregate(x, by = groupbylist, FUN = function(x){sum(duplicated(x))}, drop = FALSE), by.x = names(alltimepointslist), by.y = "Group.1", all = TRUE)

	# NOTE: Changepoints functionality disabled until we find a method that works
#	cpts <- find_changepoints(groupedvals, method = changepointmethods, showprogress = showprogress)[[1]]

	log_message(paste0("Finished"), showprogress)

	structure(list(values = groupedvals,
								 functionlist = functionlist,
								 # NOTE: Changepoints functionality disabled until we find a method that works
								 # changepoints = cpts,
								 fieldtype = ft,
								 columnname = "[ALLFIELDSCOMBINED]"
								 # NOTE: Partitionfield functionality disabled until we work out how to present it
								 # partitionfieldname = partitionfieldname,
								 # partitionfieldvalue = partitionfieldvalue
								 ),
						class = "aggregatefield")
}

# -----------------------------------------------------------------------------
#' Aggregate source data
#'
#' Aggregates sourcedata object based on fieldtypes specified at load time.
#' Default time period for aggregation is a calendar day
#'
#' @param sourcedata A \code{sourcedata} object returned from \code{\link{prepare_data}} function
#' @param aggregation_timeunit Unit of time to aggregate over. Specify one of "day", "week", "month", "quarter", "year". The "week" option is Monday-based. Default = "day"
#' @param showprogress Print progress to console. Default = TRUE
#' @return An \code{aggregatedata} object
#' @seealso \code{\link{prepare_data}}, \code{\link{report_data}}
#' @export
aggregate_data <- function(sourcedata, aggregation_timeunit = "day", showprogress = TRUE){
	# TODO: move calculation of changepoints into separate function and allow them to be calculated afterwards
	# TODO: allow user to override existing aggfunctions?
	# TODO: Use something better than seq() to calculate weeks and months, so that it works when the first date is not the first of the month
	#temp assignment
	# sourcedata<-outpatsourcedata
	# aggregation_timeunit = "day"
	# NOTE: Changepoints functionality disabled until we find a method that works
	changepointmethods = "none"
	# showprogress = TRUE

	log_function_start(match.call()[[1]])

	validate_params_required(match.call())
	validate_params_type(match.call(),
											 sourcedata = sourcedata,
											 aggregation_timeunit = aggregation_timeunit,
											 showprogress = showprogress)

	# create column to group by
	# TODO: raise an error/warning if data is less granular than aggregation_timeunit
	log_message(paste0("Aggregating [", sourcedata$sourcename, "] by [", aggregation_timeunit, "]..."), showprogress)
	# need to ensure have all possible timepoint values, even if they are missing in the dataset
	alltimepoints_min <- timepoint_as_aggregationunit(get_datafield_min(sourcedata$datafields[[sourcedata$timepoint_fieldname]]), aggregation_timeunit)
	alltimepoints_max <- timepoint_as_aggregationunit(get_datafield_max(sourcedata$datafields[[sourcedata$timepoint_fieldname]]), aggregation_timeunit)
	alltimepoints <- data.table::data.table(seq(alltimepoints_min, alltimepoints_max, by = aggregation_timeunit))
	names(alltimepoints) <- paste0(gsub("[^a-zA-Z0-9_]", "_", sourcedata$timepoint_fieldname), "_by", aggregation_timeunit)

	### AGGREGATE OVERALL DATASET
	log_message(paste0("Aggregating overall dataset..."), showprogress)
	# load aggregated data into new vector
	log_message(paste0("Aggregating each datafield in turn..."), showprogress)
	agg <- vector("list", sourcedata$cols_imported_n + 2)
	for (i in 1:sourcedata$cols_imported_n){
		log_message(paste0(i, ": ", names(sourcedata$cols_imported_indexes)[i]), showprogress)
		fieldindex <- sourcedata$cols_imported_indexes[[i]]
		#  Preparation for placing subaggregates as children on each overall aggfield
		#    agg[[i]] <- c(aggregatefield(sourcedata$datafields[[fieldindex]], alltimepointslist, groupbylist, showprogress = showprogress), subaggregates=vector("list", 0))
		#    agg[[i]] <- c(aggregatefield(sourcedata$datafields[[fieldindex]], alltimepointslist, groupbylist, showprogress = showprogress), subaggregates=NA)
		agg[[i]] <- aggregatefield(sourcedata$datafields[[fieldindex]], get_datafield_vector(sourcedata$datafields[[sourcedata$timepoint_fieldname]]), alltimepoints, aggregation_timeunit, changepointmethods = changepointmethods, showprogress = showprogress)
	}
	log_message(paste0("Aggregating calculated fields..."), showprogress)
	log_message(paste0("[DUPLICATES]:"), showprogress)
	agg[[sourcedata$cols_imported_n+1]] <- aggregatefield(sourcedata$datafields[[sourcedata$cols_source_n+1]], get_datafield_vector(sourcedata$datafields[[sourcedata$timepoint_fieldname]]), alltimepoints, aggregation_timeunit, changepointmethods = changepointmethods, showprogress = showprogress)
	log_message(paste0("[ALLFIELDSCOMBINED]:"), showprogress)
	agg[[sourcedata$cols_imported_n+2]] <- aggregateallfields(agg[1:sourcedata$cols_imported_n], aggregation_timeunit, changepointmethods = changepointmethods, showprogress = showprogress)
	names(agg) <- c(names(sourcedata$cols_imported_indexes), "[DUPLICATES]", "[ALLFIELDSCOMBINED]")

	# NOTE: Changepoints functionality disabled until we find a method that works
#	log_message(paste0("Creating changepoint dataframe..."), showprogress)
#	changepoints_df <- all_changepoints(agg)


	# NOTE: Partitionfield functionality disabled until we work out how to present it
	# ### AGGREGATE BY EACH PARTITIONFIELD SUBGROUP
	# # set up lists of correct size first
	# log_message(paste0("Checking for fields of type 'partition'..."), showprogress)
	# partitionfield_fieldnames <- character()
	# partitionfield_indexes <- numeric()
	# # NOTE: could probably just loop through all datafields here, not sure of value of only checking the imported ones
	# for (i in 1:sourcedata$cols_imported_n){
	# 	fieldindex = sourcedata$cols_imported_indexes[[i]]
	# 	if (is.fieldtype_partition(sourcedata$datafields[[fieldindex]]$fieldtype)){
	# 		partitionfield_indexes <- c(partitionfield_indexes, fieldindex)
	# 		partitionfield_fieldnames <- c(partitionfield_fieldnames, sourcedata$datafields[[fieldindex]]$columnname)
	# 	}
	# }
	# subaggregate <- vector("list", length(partitionfield_indexes))
	# if( length(partitionfield_indexes)==0 ){
	# 	log_message(paste0("None found"), showprogress)
	# } else{
	# 	for (i in seq_along(partitionfield_indexes)){
	# 		partitionfield_name <- sourcedata$datafields[[partitionfield_indexes[[i]]]]$columnname
	# 		log_message(paste0("Aggregating by ", partitionfield_name, "..."), showprogress)
	# 		# create factor of partitionfield subgroups
	# 		# TODO: not sure what's going to happen if NA values are present
	# 		partitionfield_levels <- unlist(unique(sourcedata$datafields[[partitionfield_indexes[[i]]]]$values))
	# 		# if there is only one value then don't bother
	# 		if( length(partitionfield_levels) <= 1 ){
	# 			log_message(paste0(length(partitionfield_levels), " unique value(s) found. Skip."), showprogress)
	# 		} else{
	# 			subaggregate[[i]] <- vector("list", length(partitionfield_levels))
	# 			for (j in seq_along(partitionfield_levels)){
	# 				log_message(paste0("  Filter on ", partitionfield_name, "=", partitionfield_levels[j], ":"), showprogress)
	# 				# set timepoint vector to ignore other levels
	# 				partitionfield_levelindicator <- which(sourcedata$datafields[[partitionfield_indexes[[i]]]]$values != partitionfield_levels[j])
	# 				timepointsubvalues <- get_datafield_vector(sourcedata$datafields[[sourcedata$timepoint_fieldname]])
	# 				timepointsubvalues[partitionfield_levelindicator] <- NA
	# 				log_message(paste0("    Aggregating each datafield in turn..."), showprogress)
	# 				subaggregate[[i]][[j]]$aggregatefields <- vector("list", sourcedata$cols_imported_n + 1)
	# 				nextindex <- 1
	# 				for (k in 1:sourcedata$cols_imported_n){
	# 					log_message(paste0("      ", k, ": ", names(sourcedata$cols_imported_indexes)[k]), showprogress)
	# 					fieldindex = sourcedata$cols_imported_indexes[[k]]
	# 					if (fieldindex != partitionfield_indexes[[i]]){
	# 						subaggregate[[i]][[j]]$aggregatefields[[nextindex]] <- aggregatefield(sourcedata$datafields[[fieldindex]], timepointfieldvalues = timepointsubvalues, alltimepoints = alltimepoints, aggregation_timeunit = aggregation_timeunit, changepointmethods = changepointmethods, partitionfieldname = partitionfield_name, partitionfieldvalue = partitionfield_levels[[j]], showprogress = showprogress)
	# 						nextindex <- nextindex + 1
	# 					}
	# 				}
	# 				log_message(paste0("Aggregating calculated datafields..."), showprogress)
	# 				log_message(paste0("DUPLICATES:"), showprogress)
	# 				subaggregate[[i]][[j]]$aggregatefields[[sourcedata$cols_imported_n]] <- aggregatefield(sourcedata$datafields[[sourcedata$cols_source_n]], timepointfieldvalues = timepointsubvalues, alltimepoints = alltimepoints, aggregation_timeunit, changepointmethods = changepointmethods, partitionfieldname = partitionfield_name, partitionfieldvalue = partitionfield_levels[[j]], showprogress = showprogress)
	# 				log_message(paste0("ALLFIELDSCOMBINED:"), showprogress)
	# 				subaggregate[[i]][[j]]$aggregatefields[[sourcedata$cols_imported_n+1]] <- aggregateallfields(subaggregate[[i]][[j]]$aggregatefields[1:sourcedata$cols_imported_n - 1], timepointfieldvalues = timepointsubvalues, alltimepoints = alltimepoints, changepointmethods = changepointmethods, partitionfieldname = partitionfield_name, partitionfieldvalue = partitionfield_levels[[j]], showprogress = showprogress)
	# 				names(subaggregate[[i]][[j]]$aggregatefields) <- c(names(sourcedata$cols_imported_indexes)[-which(sourcedata$cols_imported_indexes==partitionfield_indexes[[i]])], "DUPLICATES", "ALLFIELDSCOMBINED")
	#
	#
	# 				# NOTE: Changepoints functionality disabled until we find a method that works
	# 				# log_message(paste0("Creating changepoint dataframe..."), showprogress)
	# 				# subaggregate[[i]][[j]]$changepoints_df <- all_changepoints(subaggregate[[i]][[j]]$aggregatefields)
	# 				subaggregate[[i]][[j]]$timepoint_fieldname <- sourcedata$timepoint_fieldname
	# 				subaggregate[[i]][[j]]$aggregation_timeunit <- aggregation_timeunit
	# 			}
	# 			names(subaggregate[[i]]) <- partitionfield_levels
	# 		}
	# 	}
	# 	names(subaggregate) <- names(sourcedata$cols_imported_indexes)[which(sourcedata$cols_imported_indexes %in% partitionfield_indexes)]
	#
	#
	# 	#   # Place subaggregates as children on each overall aggfield
	# 	#   # TODO: This doesn't yet work if you have >1 partition fieldtype
	# 	#   for (k in seq_along(agg)){
	# 	#   	if (names(agg[k]) != names(sourcedata$cols_imported_indexes)[which(sourcedata$cols_imported_indexes==partitionfield_indexes[[1]])]){
	# 	#   		agg[[k]]$subaggregates <- vector("list", length(partitionfield_indexes))
	# 	# 	  	for (i in seq_along(partitionfield_indexes)){
	# 	# 	  			agg[[k]]$subaggregates[[i]] <- vector("list", length(partitionfield_levels))
	# 	# 	  			for (j in seq_along(partitionfield_levels)){
	# 	# 	  				agg[[k]]$subaggregates[[i]][[j]] <- subaggregate[[i]][[j]][[1]][[k]]
	# 	# 	  			}
	# 	# 	  			names(agg[[k]]$subaggregates[[i]]) <- paste0(names(sourcedata$cols_imported_indexes)[which(sourcedata$cols_imported_indexes==partitionfield_indexes[[i]])], "_", partitionfield_levels)
	# 	# 	  	}
	# 	# 	  	names(agg[[k]]$subaggregates) <- paste0(names(agg[k]), "_by_", names(sourcedata$cols_imported_indexes)[which(sourcedata$cols_imported_indexes %in% partitionfield_indexes)])
	# 	#   	}
	# 	#   }
	# }

	log_function_end(match.call()[[1]])

	structure(
		list(
			aggregatefields = agg,
			# NOTE: Changepoints functionality disabled until we find a method that works
			# changepoints_df = changepoints_df,
			timepoint_fieldname = sourcedata$timepoint_fieldname,
			aggregation_timeunit = aggregation_timeunit # not sure if this should be set at overall object level or allow it to differ per aggregatefield
			# NOTE: Partitionfield functionality disabled until we work out how to present it
			# partitionfield_fieldnames = partitionfield_fieldnames,
			# subaggregates = subaggregate
		),
		class = "aggregatedata"
	)
}

is.aggregatedata <- function(x) inherits(x, "aggregatedata")

# -----------------------------------------------------------------------------
#' Export aggregated data
#'
#' Export aggregated data to disk.  Creates a separate file for each aggregated field in dataset.
#'
#' @param aggregatedata A \code{aggregatedata} object
#' @param save_directory String. Full or relative path for save folder
#' @param save_fileprefix String. Optional prefix for the exported filenames
#' @param save_filetype String. Filetype extension supported by \code{readr}, currently only csv allowed
#' @export
export_aggregated_data <- function(aggregatedata, save_directory, save_fileprefix = "", save_filetype = "csv"){
	#temp assignment
	# aggregatedata<-testcpddata_byday
	# save_directory = ".\\devtesting\\testoutput\\"
	# save_filetype = "csv"

	# validation checks on params
	validate_params_required(match.call())
	validate_params_type(match.call(),
											 aggregatedata = aggregatedata,
											 save_directory = save_directory,
											 save_fileprefix = save_fileprefix,
											 save_filetype = save_filetype)

	if( !(save_filetype %in% c("csv")) ){
		stop(paste("Invalid save_filetype: ", save_filetype, ". Only csv format is currently supported"))
	}

	# export a file for each field in dataset
	for( i in seq_along(aggregatedata$aggregatefields) ){
		readr::write_csv(aggregatedata$aggregatefields[[i]]$values,
										 file.path(save_directory, paste0(save_fileprefix, names(aggregatedata$aggregatefields[i]), ".csv"))
										 )
	}

	# NOTE: Partitionfield functionality disabled until we work out how to present it
	# for(p in seq_along(aggregatedata$subaggregates)){
	# 	for( q in seq_along(aggregatedata$subaggregates[[p]]) ){
	# 		for( i in seq_along(aggregatedata$subaggregates[[p]][[q]]$aggregatefields) ){
	# 			readr::write_csv(aggregatedata$subaggregates[[p]][[q]]$aggregatefields[[i]]$values,
	# 											 file.path(save_directory, paste0(
	# 											 			 names(aggregatedata$aggregatefields[i]),
	# 											 			 "_by_", names(aggregatedata$subaggregates),
	# 											 			 "_", names(aggregatedata$subaggregates[[p]][q]),
	# 											 			 ".csv")))
	# 		}
	# 	}
	# }

}

#' @export
print.aggregatedata <- function(x, ...){
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
	# NOTE: Partitionfield functionality disabled until we work out how to present it
	# cat("Column(s) used as partitionfield:", aggsummary$overall["partitionfield_fieldnames"], "\n")
	cat("\n")
	# NOTE: Changepoints functionality disabled until we find a method that works
	# cat("Change points by field:\n")
	# if( nrow(aggsummary$byfield) > 0){
	# 	print(aggsummary$byfield)
	# } else{
	# 	cat("Change points not calculated.\n")
	# }
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
							 n_empty_timepoints = sum(aggfields[[aggregatedata$timepoint_fieldname]]$values[["n"]] == 0)
							 # NOTE: Partitionfield functionality disabled until we work out how to present it
							 # partitionfield_fieldnames = toString(aggregatedata$partitionfield_fieldnames)
	)

	# NOTE: Changepoints functionality disabled until we find a method that works
	# # summary info for each column in dataset
	# byfield <- data.frame(fieldname=character(),
	# 											aggregatetype=character(),
	# 											changepoint_method=character(),
	# 											# changepoint_method_params=character(),
	# 											n_changepoints=integer(),
	# 											n_outliers=integer(),
	# 											n_istrues=integer(),
	# 											stringsAsFactors = FALSE
	# )
	# for(i in seq_along(aggfields)){
	# 	for(j in seq_along(aggfields[[i]]$changepoints)){
	# 		cpfield <- aggfields[[i]]$changepoints[[j]]
	# 		for(k in seq_along(cpfield)){
	# 			byfield <- rbind(byfield,
	# 											 data.frame(fieldname=aggfields[[i]]$columnname,
	# 											 					 aggregatetype=names(aggfields[[i]]$changepoints[j]),
	# 											 					 changepoint_method=cpfield[[k]]$changepoint_method,
	# 											 					 # changepoint_method_params=cpfield[[k]]$changepoint_method_params,
	# 											 					 n_changepoints=ifelse(is.null(cpfield[[k]]$changepoint_indexes), NA, length(cpfield[[k]]$changepoint_indexes)),
	# 											 					 n_outliers=ifelse(is.null(cpfield[[k]]$outlier_indexes), NA, length(cpfield[[k]]$outlier_indexes)),
	# 											 					 n_istrues=ifelse(is.null(cpfield[[k]]$istrue_indexes), NA, length(cpfield[[k]]$istrue_indexes)),
	# 											 					 stringsAsFactors = FALSE
	# 											 )
	# 			)
	# 		}
	# 	}
	# }

	# byfield2 <- aggregate(cbind(n_changepoints=aggregatedata$changepoints_df$changepointtype == "edge", n_outliers=aggregatedata$changepoints_df$changepointtype == "outlier"),
	#                       by = list(fieldname=aggregatedata$changepoints_df$fieldname, aggregatetype=aggregatedata$changepoints_df$aggregatetype, changepointmethod=aggregatedata$changepoints_df$changepointmethod),
	#                       FUN = sum,
	#                       drop = TRUE)

	# NOTE: Partitionfield functionality disabled until we work out how to present it
	# if( length(aggregatedata$subaggregates) > 0 ){
	# 	subaggs <- vector("list", length(aggregatedata$subaggregates))
	# 	for(p in seq_along(aggregatedata$subaggregates)){
	# 		numlevels <- length(aggregatedata$subaggregates[[p]])
	# 		subaggs[[p]] <- vector("list", numlevels)
	# 		for(q in 1:numlevels){
	# 			subaggs[[p]][[q]] <- summarise_aggregated_data(aggregatedata$subaggregates[[p]][[q]])
	# 			subaggs[[p]][[q]]$overall <- c(subaggs[[p]][[q]]$overall, partitionfield_fieldname=aggregatedata$partitionfield_fieldnames[[p]], partitionfield_fieldvalue=names(aggregatedata$subaggregates[[p]][q]))
	# 		}
	# 	}
	# }
	# else{
	# 	subaggs <- NA
	# }

	structure(
		list(
			overall = overall
			# NOTE: Changepoints functionality disabled until we find a method that works
			#	byfield = byfield,
			# NOTE: Partitionfield functionality disabled until we work out how to present it
			# bypartitionfield = subaggs
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
		as.Date(vapply(x, function(x){ format(x, paste0("%Y-"
																						, switch(format(x, format = "%m")
																										 , "01" = "01", "02" = "01", "03" = "01"
																										 , "04" = "04", "05" = "04", "06" = "04"
																										 , "07" = "07", "08" = "07", "09" = "07"
																										 , "10" = "10", "11" = "10", "12" = "10"
																										)
																						, "-01")) }, character(1)))
	} else if( aggregation_timeunit == "year" ){
		as.Date(format(x, format = "%Y-01-01"))
	}
}


# -----------------------------------------------------------------------------
# TODO: Define set of allowed aggregation functions similarly to fieldtypes, with each object containing formula for aggregation as well as friendly names

# Set user-friendly names
# this works using the resulting columnname rather than the original aggregationfunction (relevant for subcats)
aggtype_friendlyname <- function(aggtype, type){
	if( startsWith(aggtype, "subcat_" ) ){
		catval <- substring(gsub('^(?:[^_]*_){3}','_', aggtype), 2)
		if( startsWith(aggtype, "subcat_n") ){
			switch(type,
						 short = aggtype,
						 long = paste0("No. of values in the category: ", catval)
			)
		} else if( startsWith(aggtype, "subcat_perc") ){
			switch(type,
						 short = aggtype,
						 long = paste0("Percentage of values in the category: ", catval)
			)
		}
	} else{
		switch(aggtype,
					 n = {switch(type,
					 						short = "n",
					 						long = "No. of values present"
					 )},
					 missing_n = {switch(type,
					 										short = "missing_n",
					 										long = "No. of missing values"
					 )},
					 missing_perc = {switch(type,
					 											 short = "missing_perc",
					 											 long = "Percentage of missing values"
					 )},
					 nonconformant_n = {switch(type,
					 													short = "nonconformant_n",
					 													long = "No. of non-conformant values"
					 )},
					 nonconformant_perc = {switch(type,
					 														 short = "nonconformant_perc",
					 														 long = "Percentage of non-conformant values"
					 )},
					 sum = {switch(type,
					 							short = "sum",
					 							long = "No. of duplicate records removed"
					 )},
					 nonzero_perc = {switch(type,
					 											 short = "nonzero_perc",
					 											 long = "Percentage of (remaining) records that were duplicated"
					 )},
					 distinct = {switch(type,
					 									 short = "distinct",
					 									 long = "No. of distinct values"
					 )},
					 midnight_n = {switch(type,
					 										 short = "midnight_n",
					 										 long = "No. of values with no time element"
					 )},
					 midnight_perc = {switch(type,
					 												short = "midnight_perc",
					 												long = "Percentage of values with no time element"
					 )},
					 min = {switch(type,
					 							short = "min",
					 							long = "Minimum value"
					 )},
					 max = {switch(type,
					 							short = "max",
					 							long = "Maximum value"
					 )},
					 mean = {switch(type,
					 							 short = "mean",
					 							 long = "Mean value"
					 )},
					 median = {switch(type,
					 								 short = "median",
					 								 long = "Median value"
					 )},
					 minlength = {switch(type,
					 										short = "minlength",
					 										long = "Minimum string length"
					 )},
					 maxlength = {switch(type,
					 										short = "maxlength",
					 										long = "Maximum string length"
					 )},
					 meanlength = {switch(type,
					 										 short = "meanlength",
					 										 long = "Mean string length"
					 )}
		)

	}
}
