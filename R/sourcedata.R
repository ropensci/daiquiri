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
#' Prepare source data
#'
#' Validate a data frame against a \code{\link{fieldtypes}} specification, and prepare for aggregation.
#'
#' @param df A data frame
#' @param fieldtypes \code{\link{fieldtypes}} object specifying names and types of fields (columns) in the source data. See also \link{availablefieldtypes}.
#' @param override_columnnames If FALSE, column names must exist in data frame and must match
#' the names specified in fieldtypes exactly. If TRUE, column names in source will be replaced with names in fieldtypes
#' specification. The specification must therefore contain the columns in the correct order. Default = FALSE
#' @param na vector containing strings that should be interpreted as missing values, Default = \code{c("","NA","NULL")}.
#' @param dataset_shortdesc Short description of the dataset being checked. This will appear on the report. If blank, the name of the data frame object will be used
#' @param showprogress Print progress to console. Default = TRUE
#' @return A \code{sourcedata} object
#' @examples rawdata <- read_data(
#'   system.file("extdata", "example_data.csv", package = "daiquiri"),
#'   delim = ",",
#'   col_names = TRUE
#' )
#'
#' sourcedataobj <- prepare_data(
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
#'   na = c("","NULL")
#' )
#' @seealso \code{\link{fieldtypes}}, \code{\link{availablefieldtypes}}, \code{\link{aggregate_data}}, \code{\link{report_data}}, \code{\link{create_report}}
#' @export
#' @importFrom data.table .N
prepare_data <- function(df, fieldtypes, override_columnnames = FALSE, na = c("","NA","NULL"), dataset_shortdesc = NULL, showprogress = TRUE) {
	# temp assignments
	# dt<-data.table::setDT(source_df)
	#	dt<-data.table(source_df)
	# fieldtypes<-testfile_fieldtypes
	#	showprogress = TRUE

	log_function_start(match.call()[[1]])

	# initialise known column names to prevent R CMD check notes
	colindex = rowindex = fieldname = NULL

	validate_params_required(match.call())
	validate_params_type(match.call(),
											 df = df,
											 fieldtypes = fieldtypes,
											 override_columnnames = override_columnnames,
											 na = na,
											 dataset_shortdesc = dataset_shortdesc,
											 showprogress = showprogress)

	# use dataset_shortdesc if present, otherwise get from call
	if( is.null(dataset_shortdesc) ){
		if( match.call(definition = sys.function(-1), call = sys.call(sys.parent()))[[1]] == "create_report"){
			dataset_shortdesc <- as.list(match.call(definition = sys.function(-1), call = sys.call(sys.parent())))$df
		} else{
			dataset_shortdesc <- as.list(match.call())$df
		}
	}

	log_message(paste0("Fieldtypes supplied:\n", fieldtypes_to_string(fieldtypes)), showprogress)

	# validate inputs
	log_message(paste0("Checking column names against fieldtypes..."), showprogress)
	validate_columnnames(names(df), names(fieldtypes), check_length_only = override_columnnames)
	if (override_columnnames == TRUE){
		names(df) <- names(fieldtypes)
	}

	# ensure all columns are character type because readr::type_convert won't skip numeric columns
	df_datatypes <- sapply(df, typeof)
	df_nonchar_warnings <- data.table::data.table()
	if( any(df_datatypes != "character") ){
		# Report presence of any non-char columns in source data frame (except ignored ones)
		df_nonchar_warnings <- data.table::data.table(
			colindex = which(df_datatypes != "character" & !sapply(fieldtypes, is.fieldtype_ignore)),
			rowindex = NA,
			message = paste0("Data supplied as ",
											 df_datatypes[which(df_datatypes != "character" & !sapply(fieldtypes, is.fieldtype_ignore))],
											 " instead of character, non-conformant values will not be identified")
			)
		# update the df
		df[df_datatypes != "character"] <- lapply(df[df_datatypes != "character"], as.character)
	}

	log_message(paste0("Importing source data [", dataset_shortdesc, "]..."), showprogress)

	# number of rows in source
	rows_source_n <- nrow(df)
	# number of columns in source
	cols_source_n <- length(df)
	timepoint_index <- which(vapply(fieldtypes, is.fieldtype_timepoint, logical(1)))
	timepoint_fieldname <- names(timepoint_index)

	# Validate data against specification, store warnings instead of printing them
	# use readr::type_convert for now.  Ideally want to store original values and describe action taken too
	# convert to data.table at this point
	log_message(paste0("Checking data against fieldtypes..."), showprogress)
	raw_warnings <- NULL
	clean_dt <- data.table::as.data.table(withCallingHandlers(
		readr::type_convert(df, fieldtypes_to_cols(fieldtypes, readfunction = "readr"), na = na),
		warning = function(w) {
			raw_warnings <<- append(raw_warnings, conditionMessage(w))
			invokeRestart("muffleWarning")
		}
	))
	log_message(paste0("  Selecting relevant warnings..."), showprogress)
	# TODO: consider removing df at this point, to release memory
	# extract items of interest from warnings
	# NOTE: column indexes for readr::type_convert warnings correspond to original data file and are 1-based
	# NOTE: row indexes for readr::type_convert warnings are zero-based (confusingly)
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
		warningcolname <- names(df)[c]
		warningrows <- warningsdt[colindex == c, rowindex]
		clean_dt[warningrows, (warningcolname) := NaN]
	}

	log_message(paste0("  Checking and removing missing timepoints..."), showprogress)
	# check and remove rows where timepoint field is null
	# TODO: should I remove them here or when aggregating?  Summary doesn't look right if remove them here. Rownumbers in warnings no longer matches either
	# TODO: check don't duplicate any messages from above
	if (anyNA(clean_dt[[(timepoint_fieldname)]])){
		navector <- is.na(clean_dt[[(timepoint_fieldname)]])
		# stop if there are no valid timepoint values
		if(sum(navector) == nrow(df)){
			stop_custom(.subclass = "invalid_param_type",
									message = "Timepoint field does not contain any valid values. Check the correct date format has been used.")
		}
		timepointwarnings <- data.table::data.table(colindex = which(names(df) == timepoint_fieldname),
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

	# tidy up warnings
	warningsdt <- rbind(warningsdt, df_nonchar_warnings)
	data.table::setorder(warningsdt, colindex, rowindex)
	warningsdt <- cbind(data.table::data.table(fieldname = names(df)[warningsdt[, colindex]]),
											warningsdt[, list(colindex, rowindex, message)])
	warnings_summary <- warningsdt[, list(instances = data.table::fifelse(anyNA(rowindex), NA_integer_, .N)), by = list(fieldname, message)]

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

	log_message(paste0("Loading into sourcedata structure..."), showprogress)
	# load data into datafield classes
	dfs <- vector("list", cols_source_n + 1)
	cols_imported_indexes <- vector("integer")

	# TODO: can't add NULL to class structure and can't seem to add an empty vector either

	# not sure this vectorisation is working correctly, do I need to rewrite datafield() in a vectorised way too?
	# is_fieldtype_ignore <- vapply(fieldtypes, is.fieldtype_ignore, logical(1))
	# dfs[[is_fieldtype_ignore]] <- datafield(as.vector("ignored"), fieldtypes[is_fieldtype_ignore])
	# dfs[[!is_fieldtype_ignore]] <- datafield(clean_dt[names(fieldtypes[!is_fieldtype_ignore])], fieldtypes[[!is_fieldtype_ignore]])

	for (i in 1:cols_source_n){
		currentfield <- names(fieldtypes[i])
		log_message(paste0("  ", currentfield), showprogress)
		if (is.fieldtype_ignore(fieldtypes[[i]])){
			dfs[[i]] <- datafield(as.vector("ignored"), fieldtypes[[i]])
		}
		else{
			dfs[[i]] <- datafield(clean_dt[, currentfield, with = FALSE],
														fieldtypes[[i]],
														warningsdt[fieldname == currentfield, c("rowindex","message")])
			cols_imported_indexes <- c(cols_imported_indexes, i)
			names(cols_imported_indexes)[length(cols_imported_indexes)] <- currentfield
		}
	}
	# Create new datafield to store numbers of dups.
	dfs[[cols_source_n + 1]] <- datafield(duprowsindex,
																				ft_duplicates(),
																				warningsdt[colindex == 0, c("rowindex","message")])
	# TODO: Need to use a reserved word to distinguish it from imported fields
	names(dfs) <- c(names(fieldtypes), "DUPLICATES")

	log_message(paste0("Finished"), showprogress)

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
		validation_warnings = warnings_summary,
		dataset_shortdesc = dataset_shortdesc,
		na_values = na
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
  cat("Source:", x$dataset_shortdesc, "\n")
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
  cat("Strings interpreted as missing values:", sourcesummary$overall["na_values"], "\n")
  cat("Total validation warnings:", sum(sourcesummary$validation_warnings$instances), "\n")
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
summarise_source_data <- function(sourcedata, showprogress = TRUE){
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
							 timepoint_missing_n = format(sourcedata$timepoint_missing_n),
							 na_values = paste(dQuote(sourcedata$na_values, q = FALSE), collapse = ",")
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
	validation_warnings <- sourcedata$validation_warnings

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
		if (is.fieldtype_ignore(datafield$fieldtype) || is.fieldtype_calculated(datafield$fieldtype)){
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
    stop_custom(.subclass = "invalid_columnnames",
    						message = paste0("Invalid column names.\n",
    														 paste(err_validation, collapse = "\n")))
	}

}

