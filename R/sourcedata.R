# Code for creation of sourcedata object
#   which contains both the (vector) data for the field and the relevant metadata

# -----------------------------------------------------------------------------
# individual datafields
# allowable datafield types match field_types

# NOTE: not sure if better for object to be the data vector or a list
# TODO: not sure if better to store the entire fieldtype or just its name or even as a separate list in the sourcedata
datafield <- function(x, fieldtype, validation_warnings = NULL) {
  structure(list(values = x,
  							 fieldtype = fieldtype,
  							 columnname = names(x[1]),
  							 validation_warnings = validation_warnings),
            class = c(paste0("datafield_", get_fieldtype_name(fieldtype)), "datafield"))
}

is.datafield <- function(x) inherits(x, "datafield")

# -----------------------------------------------------------------------------
# constructor for sourcedata object
# takes in a data.table, fieldtypes specification, and (string) sourcename
sourcedata <- function(dt, fieldtypes, sourcename, showprogress = FALSE) {
# temp assignments
# dt<-data.table::setDT(source_df)
#	dt<-data.table(source_df)
# fieldtypes<-testfile_fieldtypes
#	showprogress = TRUE

	log_function_start(match.call()[[1]])
	log_message(paste0("Processing source data [", sourcename, "]..."), showprogress)

	# number of rows in source
	rows_source_n <- nrow(dt)
	# number of columns in source
	cols_source_n <- length(dt)
	timepoint_index <- which(vapply(fieldtypes, is.fieldtype_timepoint, logical(1)))
	timepoint_fieldname <- names(timepoint_index)

	# Validate data against specification, store warnings instead of printing them
	# use readr::type_convert for now.  Ideally want to store original values and describe action taken too
	# TODO: deal with warnings about embedded quotes
	# TODO: don't know what is causing the "length of NULL cannot be changed" warning
	# NOTE: row/column indexes for warnings appear to be for original data file, and count the header as row 1
	log_message(paste0("Checking data against fieldtypes..."), showprogress)
	raw_warnings <- NULL
	clean_dt <- withCallingHandlers(
		readr::type_convert(dt, fieldtypes_to_cols(fieldtypes, readfunction = "readr")),
		warning = function(w) {
			raw_warnings <<- append(raw_warnings, conditionMessage(w))
			invokeRestart("muffleWarning")
		}
	)
	log_message(paste0("  Selecting relevant warnings..."), showprogress)
	# TODO: consider removing dt at this point, to release memory
	# extract items of interest from warnings
	relevant_warnings <- grep("\\[[0-9]*?, [0-9]*?\\]:", raw_warnings, value = TRUE)
	# list of warnings each with character vector containing row, column, message
	warningslist <- lapply(strsplit(relevant_warnings, ": "), function(x){c(gsub("[^0-9]", "", unlist(strsplit(x[1], ","))), x[2])})
	warningsdt <- data.table::data.table(colindex = as.integer(sapply(warningslist, function(x){x[2]})),
													 rowindex = as.integer(sapply(warningslist, function(x){x[1]})) + 1,
													 message = as.character(sapply(warningslist, function(x){x[3]})))

	log_message(paste0("  Identifying nonconformant values..."), showprogress)
	# readr::type_convert replaces nonconformant values with NA. Set them to NaN instead to distinguish them from missing
	# this seems much harder than it should be
	warningcols <- unique(warningsdt[, colindex])
	for(c in warningcols){
		warningcolname <- names(fieldtypes)[c]
		warningrows <- warningsdt[colindex == c, rowindex]
		clean_dt[warningrows, (warningcolname) := NaN]
	}

	log_message(paste0("  Checking and removing missing timepoints..."), showprogress)
	# check and remove rows where timepoint field is null
	# TODO: should I remove them here or when aggregating?  Summary doesn't look right if remove them here. Rownumbers in warnings no longer matches either
	# TODO: check don't duplicate any messages from above
	if (anyNA(clean_dt[[(timepoint_fieldname)]])){
		navector <- is.na(clean_dt[[(timepoint_fieldname)]])
		timepointwarnings <- data.table::data.table(colindex = timepoint_index,
																		rowindex = which(navector == TRUE),
																		message = "Missing or invalid value in Timepoint field"
																		)
		warningsdt <- rbind(warningsdt, timepointwarnings)
		# NOTE: Row deletion by reference doesn't exist in data.table yet. Interim memory-efficient solution
		# NOTE: Need copy() because otherwise when using cols <- names(clean_dt), cols updates when columns are removed from clean_dt
		cols <- data.table::copy(names(clean_dt))
		clean_dt_temp <- data.table::data.table("Col1" = clean_dt[[1]][!navector])
		names(clean_dt_temp)[1] <- cols[1]
		clean_dt[, (cols[1]) := NULL]
		for (col in cols[2:length(cols)]){
			clean_dt_temp[, (col) := clean_dt[[col]][!navector]]
			clean_dt[, (col) := NULL]
		}
		clean_dt <- clean_dt_temp
		rm(clean_dt_temp)
		timepoint_missing_n <- sum(navector)
	} else{
		timepoint_missing_n <- 0
	}

	log_message(paste0("Checking for duplicates..."), showprogress)
	# sort by timepoint field then by everything else, so that we can batch the data
	# TODO: try using setkey as well to see if it makes a difference
	log_message(paste0("  Sorting data..."), showprogress)
	data.table::setorderv(clean_dt, c(timepoint_fieldname, names(clean_dt)[-timepoint_index]))

	# check for duplicate rows and remove them here
	# They may skew other agg stats if left in, but would still be useful to see if dups change over time
	# Need to chunk up large datasets
	# estimate total size and limit size of each chunk
	dtsize <- utils::object.size(clean_dt)
	if (dtsize > 200000000){
		numrows <- nrow(clean_dt)
		numchunks <- as.numeric(ceiling(dtsize/200000000))
		chunkrows <- ceiling(numrows/numchunks)
		log_message(paste0("  Running ", numchunks, " batches of roughly ", chunkrows, " rows each..."), showprogress)
		timepoint_vector <- clean_dt[[(timepoint_fieldname)]]
		duprowsvector <- logical(numrows)
		for (chunk in 1:numchunks){
			log_message(paste0("  Batch ", chunk), showprogress)
			chunkstart <- which.max(timepoint_vector >= timepoint_vector[((chunk-1)*chunkrows) + 1])
			if( chunk < numchunks){
				# end on the previous (unique) date that the chunk lands on
				chunkend <- which.max(timepoint_vector >= timepoint_vector[chunk*chunkrows]) - 1
			} else{
				# or else to the end of the dataset
				chunkend <- numrows
			}
			duprowsvector[chunkstart:chunkend] <- duplicated(clean_dt[chunkstart:chunkend, ])
		}
	}	else{
		duprowsvector <- duplicated(clean_dt)
	}
	# find the index row for each duplicate (i.e. the row immediately before any string of dups since we have already sorted the data)...
	duprowsindex <- c(duprowsvector[-1], FALSE)
	duprowsindex <- duprowsindex & !duprowsvector
	# ...and record the no. of duplicates on it
	dpruns <- rle(duprowsvector)
	duprowsindex[which(duprowsindex==TRUE)] <- dpruns$lengths[which(dpruns$values==TRUE)]
	duprowsindex <- data.table::data.table("DUPLICATES" = duprowsindex[!duprowsvector])

	# and remove the duplicates from the final clean dataset
	# TODO: see if can consolidate this with navector removal so only do it once
	#clean_dt <- clean_dt[which(!duprowsvector),]
	if( any(duprowsvector) ){
		# NOTE: Need copy() because otherwise when using cols <- names(clean_dt), cols updates when columns are removed from clean_dt
		cols <- data.table::copy(names(clean_dt))
		clean_dt_temp <- data.table::data.table("Col1" = clean_dt[[1]][!duprowsvector])
		names(clean_dt_temp)[1] <- cols[1]
		clean_dt[, (cols[1]) := NULL]
		for (col in cols[2:length(cols)]){
			clean_dt_temp[, (col) := clean_dt[[col]][!duprowsvector]]
			clean_dt[, (col) := NULL]
		}
		clean_dt <- clean_dt_temp
		rm(clean_dt_temp)
	}

	# basic summary info
	rows_imported_n <- nrow(clean_dt)
	# number of columns imported
	cols_imported_n <- length(clean_dt)
	# number of duplicate rows removed
	rows_duplicates_n <- sum(duprowsvector, na.rm = TRUE)

	log_message(paste0("Loading into sourcedata struture..."), showprogress)
  # load data into datafield classes
  dfs <- vector("list", cols_source_n + 1)
  cols_imported_indexes <- vector("integer")

  # TODO: can't add NULL to class structure and can't seem to add an empty vector either

  # not sure this vectorisation is working correctly, do I need to rewrite datafield() in a vectorised way too?
  # is_fieldtype_ignore <- vapply(fieldtypes, is.fieldtype_ignore, logical(1))
  # dfs[[is_fieldtype_ignore]] <- datafield(as.vector("ignored"), fieldtypes[is_fieldtype_ignore])
  # dfs[[!is_fieldtype_ignore]] <- datafield(clean_dt[names(fieldtypes[!is_fieldtype_ignore])], fieldtypes[[!is_fieldtype_ignore]])

  for (i in 1:cols_source_n){
  	fieldname <- names(fieldtypes[i])
  	log_message(paste0("  ", fieldname), showprogress)
  	if (is.fieldtype_ignore(fieldtypes[[i]])){
      dfs[[i]] <- datafield(as.vector("ignored"), fieldtypes[[i]])
    }
    else{
  		dfs[[i]] <- datafield(clean_dt[, ..fieldname],
  													fieldtypes[[i]],
  													warningsdt[colindex == i, c("rowindex","message")])
      cols_imported_indexes <- c(cols_imported_indexes, i)
      names(cols_imported_indexes)[length(cols_imported_indexes)] <- fieldname
    }
  }
  # Create new datafield to store numbers of dups.
  dfs[[cols_source_n + 1]] <- datafield(duprowsindex,
  											ft_duplicates(),
  											warningsdt[colindex == 0, c("rowindex","message")])
	# TODO: Need to use a reserved word to distinguish it from imported fields
  names(dfs) <- c(names(fieldtypes), "DUPLICATES")


  log_function_end(match.call()[[1]])

  structure(list(
      datafields = dfs,
      timepoint_fieldname = timepoint_fieldname,
      timepoint_missing_n = timepoint_missing_n,
      rows_source_n = rows_source_n,
      rows_imported_n = rows_imported_n,
      rows_duplicates_n = rows_duplicates_n,
      cols_source_n = cols_source_n,
      cols_imported_n = cols_imported_n,
      cols_imported_indexes = cols_imported_indexes,
      validation_warnings = warningsdt, # TODO: not sure whether to store all warnings here or hive them off to each datafield
      sourcename = sourcename
    ),
    class = "sourcedata"
  )
}

is.sourcedata <- function(x) inherits(x, "sourcedata")

#' @export
print.sourcedata <- function(x, ...){
	# TODO: to finish
	sourcesummary <- summarise_source_data(x)
	cat("Class: sourcedata\n")
  cat("Source:", x$sourcename, "\n")
  cat("\n")
  cat("Overall:\n")
  cat("Columns in source:", sourcesummary$overall["cols_source_n"], "\n")
  cat("Columns imported:", sourcesummary$overall["cols_imported_n"], "\n")
  cat("Rows in source:", sourcesummary$overall["rows_source_n"], "\n")
  cat("Duplicate rows removed:", sourcesummary$overall["rows_duplicates_n"], "\n")
  cat("Rows imported:", sourcesummary$overall["rows_imported_n"], "\n")
  cat("Column used for timepoint:", sourcesummary$overall["timepoint_fieldname"], "\n")
  cat("Min timepoint value:", sourcesummary$overall["timepoint_min"], "\n")
  cat("Max timepoint value:", sourcesummary$overall["timepoint_max"], "\n")
  cat("Rows missing timepoint values removed:", sourcesummary$overall["timepoint_missing_n"], "\n")
  cat("Total validation warnings:", nrow(sourcesummary$validation_warnings), "\n")
  cat("\n")
  cat("Datafields:\n")
  print(sourcesummary$datafields)
  cat("\n")
  cat("Validation warnings:\n")
  cat("\n")
  if (nrow(sourcesummary$validation_warnings) > 0){
  	print(sourcesummary$validation_warnings)
  } else{
  	cat("None")
  }

}

# summarise data
# TODO: consider making this a generic summary() method instead.
#       Help file says summary() is for models but there are a bunch of other objects implementing it too
# TODO: Consider adding a warning if a categorical field has "too many" different values
summarise_source_data <- function(sourcedata, showprogress = FALSE){
	#temp assignment
	#  sourcedata<-testcpddata
	# str(sourcedata)

	log_function_start(match.call()[[1]])
	log_message(paste0("Creating summary of source data..."), showprogress)

	log_message(paste0("  For overall dataset..."), showprogress)
	overall <- c(cols_source_n = format(sourcedata$cols_source_n),
							 cols_imported_n = format(sourcedata$cols_imported_n),
							 rows_source_n = format(sourcedata$rows_source_n),
							 rows_duplicates_n = format(sourcedata$rows_duplicates_n),
							 rows_imported_n = format(sourcedata$rows_imported_n),
							 timepoint_fieldname = sourcedata$timepoint_fieldname,
							 timepoint_min = get_datafield_min(sourcedata$datafields[[sourcedata$timepoint_fieldname]], format_as_string = TRUE),
							 timepoint_max = get_datafield_max(sourcedata$datafields[[sourcedata$timepoint_fieldname]], format_as_string = TRUE),
							 timepoint_missing_n = format(sourcedata$timepoint_missing_n)
	)

	log_message(paste0("  For each column in dataset..."), showprogress)
	datafields <- data.frame(fieldname = names(sourcedata$datafields[1:sourcedata$cols_source_n]),
													 fieldtype = sapply(sourcedata$datafields[1:sourcedata$cols_source_n], get_fieldtype_name.datafield),
													 datatype = sapply(sourcedata$datafields[1:sourcedata$cols_source_n],get_datafield_basetype, format_as_string = TRUE),
													 count = sapply(sourcedata$datafields[1:sourcedata$cols_source_n],get_datafield_count, format_as_string = TRUE),
													 missing = sapply(sourcedata$datafields[1:sourcedata$cols_source_n], get_datafield_missing, format_as_string = TRUE),
													 min = sapply(sourcedata$datafields[1:sourcedata$cols_source_n], get_datafield_min, format_as_string = TRUE),
													 max = sapply(sourcedata$datafields[1:sourcedata$cols_source_n], get_datafield_max, format_as_string = TRUE),
													 validation_warnings = sapply(sourcedata$datafields[1:sourcedata$cols_source_n], get_datafield_validation_warnings_n, format_as_string = TRUE),
													 stringsAsFactors = FALSE)

	log_message(paste0("  Validation errors on loading dataset..."), showprogress)
	validation_warnings <- do.call(rbind, c(sapply(sourcedata$datafields[1:sourcedata$cols_source_n],
																								 function(x){
																								 	cbind("Datafield"= rep(x$columnname, ifelse(is.null(nrow(x$validation_warnings)), 0, nrow(x$validation_warnings))),
																								 				x$validation_warnings) }),
																					make.row.names = FALSE))
	# strangely, when all validation_warnings data frames are empty the above creates a matrix with single make.row.names row instead of an empty data frame
	# set to empty data frame here until find better way of doing it
	if (!is.data.frame(validation_warnings)){
		validation_warnings <- cbind("Datafield"=character(0), sourcedata$datafields[[1]]$validation_warnings)
	}

	# do.call(rbind, sapply(sourcedata$datafields,
	# 												function(x){
	# 													cbind("Datafield"= rep(x$columnname, nrow(x$validation_warnings)),
	# 																x$validation_warnings) }))

	log_function_end(match.call()[[1]])

	list(overall = overall, datafields = datafields, validation_warnings = validation_warnings)

}

# TODO: really not sure about naming convention used here. Not sure if worth setting up a generic
get_fieldtype_name.datafield <- function(datafield){
    datafield$fieldtype$type
}

#####################################################################
# functions to get info about each individual datafield

get_datafield_vector <- function(datafield){
  if (is.fieldtype_ignore(datafield$fieldtype)){
    NA
  }
  else{
    datafield$values[[1]]
  }
}

get_datafield_basetype <- function(datafield, format_as_string = FALSE){
  if (format_as_string){
    format(get_datafield_basetype(datafield))
  }
  else{
    if (is.fieldtype_ignore(datafield$fieldtype)){
      NA
    }
    else{
      typeof(datafield$values[[1]])
    }
  }
}

get_datafield_min <- function(datafield, format_as_string = FALSE){
  if (format_as_string){
    format(get_datafield_min(datafield))
  }
  else{
    if (is.fieldtype_ignore(datafield$fieldtype) || all(is.na(datafield$values[[1]]))){
      NA
    }
    else{
      min(datafield$values[[1]], na.rm = TRUE)
    }
  }
}

get_datafield_max <- function(datafield, format_as_string = FALSE){
  if (format_as_string){
    format(get_datafield_max(datafield))
  }
  else{
    if (is.fieldtype_ignore(datafield$fieldtype) || all(is.na(datafield$values[[1]]))){
      NA
    }
    else{
      max(datafield$values[[1]], na.rm = TRUE)
    }
  }
}

get_datafield_missing <- function(datafield, format_as_string = FALSE){
  if (format_as_string){
    if (is.fieldtype_ignore(datafield$fieldtype) || is.fieldtype_calculated(datafield$fieldtype)){
      format(NA)
    }
    else{
      paste0(sum(is.na(datafield$values[[1]])), " (", format(sum(is.na(datafield$values[[1]]))/length(datafield$values[[1]])*100, digits = 3), "%)")
    }
  }
  else{
    if (is.fieldtype_ignore(datafield$fieldtype)){
      NA
    }
    else{
      list("frequency" = sum(is.na(datafield$values[[1]])), "percentage" = sum(is.na(datafield$values[[1]]))/length(datafield$values[[1]]))
    }
  }
}

get_datafield_validation_warnings_n <- function(datafield, format_as_string = FALSE){
	if (format_as_string){
		format(get_datafield_validation_warnings_n(datafield))
	}
	else{
		if (is.fieldtype_ignore(datafield$fieldtype) | is.fieldtype_calculated(datafield$fieldtype)){
			NA
		}
		else{
			nrow(datafield$validation_warnings)
		}
	}
}

get_datafield_count <- function(datafield, format_as_string = FALSE){
	if (format_as_string){
		format(get_datafield_count(datafield))
	}
	else{
		if (is.fieldtype_ignore(datafield$fieldtype) || all(is.na(datafield$values[[1]]))){
			NA
		}
		else{
			sum(!is.na(datafield$values[[1]]))
		}
	}
}
