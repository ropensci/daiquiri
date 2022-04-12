# Generic functions

# check required params have been passed in
# param_names is a vector
validate_params_required <- function(call){
	# get the required arguments from function definition
	params_defined <- formals(as.character(call[[1]]))
	params_required <- names(which(sapply(params_defined, is.symbol)))
	# get the arguments passed into the parent call
	params_passed <- names(as.list(call)[-1])

	if (any(!params_required %in% params_passed)) {
		stop_custom(.subclass = "invalid_param_missing",
								message = paste("Required argument(s) missing:",
																paste(setdiff(params_required, params_passed),
																			collapse=", ")))
	}
}

validate_params_type <- function(call, ...){
	params_defined <- names(formals(as.character(call[[1]])))
	params_passed <- list(...)
	params_names <- names(params_passed)

	# check internal usage is correct
	if(length(which(params_names != "")) != length(params_passed)){
		stop_custom(.subclass = "invalid_call",
								message = "Invalid call for function. Params must be passed in with names")
	}
	if(!setequal(params_defined, params_names)){
		stop_custom(.subclass = "invalid_call",
								message = paste0("Invalid call for function. Different set of params in parent function definition than were passed in to validate_params_type(). In validate_params_type() but not in parent function: ",
																 paste(setdiff(params_names, params_defined), collapse=", "),
																 "; In parent function but not in validate_params_type(): ",
																 paste(setdiff(params_defined, params_names), collapse=", ")))
	}

	# validate user-supplied params - collect all errors together and return only once
	err_validation <- character()
	for(i in seq_along(params_names)){
		if(params_names[i] == "df"){
			if( !is.data.frame(params_passed[[i]]) ){
			err_validation <- append(err_validation,
															 paste0(params_names[i], ": Expected a data frame but found: [ class = ", class(params_passed[[i]]),
																		 "; contents = ", substr(toString(params_passed[[i]]),1,100), "]."))
			}
		} else if(params_names[i] == "fieldtypes"){
			if( !is.fieldtypes(params_passed[[i]]) ){
			err_validation <- append(err_validation,
															 paste0(params_names[i], ": Expected a fieldtypes specification but found: [ class = ", class(params_passed[[i]]), "; contents = ", substr(toString(params_passed[[i]]),1,100), "]"))
			}
		} else if(params_names[i] == "default_fieldtype"){
			if( !is.fieldtype(params_passed[[i]]) ){
			err_validation <- append(err_validation,
															 paste0(params_names[i], ": Expected a fieldtype but found: [ class = ", class(params_passed[[i]]), "; contents = ", substr(toString(params_passed[[i]]),1,100), "]"))
			}
		} else if(params_names[i] == "sourcedata"){
			if( !is.sourcedata(params_passed[[i]]) ){
			err_validation <- append(err_validation,
															 paste0(params_names[i], ": Expected a sourcedata object but found: [ class = ", class(params_passed[[i]]), "; contents = ", substr(toString(params_passed[[i]]),1,100), "]"))
			}
		} else if(params_names[i] == "aggregatedata"){
			if( !is.aggregatedata(params_passed[[i]]) ){
			err_validation <- append(err_validation,
															 paste0(params_names[i], ": Expected a aggregatedata object but found: [ class = ", class(params_passed[[i]]), "; contents = ", substr(toString(params_passed[[i]]),1,100), "]"))
			}
		} else if(params_names[i] %in% c("override_columnnames", "showprogress")){
			if( !is.logical(params_passed[[i]]) ){
			err_validation <- append(err_validation,
															 paste0(params_names[i], ": Expected TRUE/FALSE but found: [ class = ", class(params_passed[[i]]), "; contents = ", substr(toString(params_passed[[i]]),1,100), "]"))
			}
		} else if(params_names[i] %in% c("save_directory")){
			if(!is.character(params_passed[[i]]) || !dir.exists(params_passed[[i]])){
				err_validation <- append(err_validation,
																 paste0(params_names[i], ": Directory not found: [ class = ", class(params_passed[[i]]), "; contents = ", substr(toString(params_passed[[i]]),1,255), "]"))
			}
		} else if(params_names[i] %in% c("log_directory")){
			if(!is.null(params_passed[[i]]) && ( !is.character(params_passed[[i]]) || !dir.exists(params_passed[[i]]) )){
				err_validation <- append(err_validation,
																 paste0(params_names[i], ": Directory not found: [ class = ", class(params_passed[[i]]), "; contents = ", substr(toString(params_passed[[i]]),1,255), "]"))
			}
		} else if(params_names[i] %in% c("save_filename")){
			if( !is.null(params_passed[[i]]) && ( grepl("[^a-zA-Z0-9_-]", params_passed[[i]]) || nchar(params_passed[[i]]) == 0 ) ){
					# NOTE: this is very restrictive and I'm not sure how it works in different locales
				err_validation <- append(err_validation,
																 paste0(params_names[i], ": Invalid filename: ",
																			 params_passed[[i]],
																			 ". Filename can only contain alphanumeric, '-', and '_' characters, and should not include the file extension."))
			}
		} else if(params_names[i] %in% c("save_fileprefix")){
			if( grepl("[^a-zA-Z0-9_-]", params_passed[[i]]) ){
				# NOTE: this is very restrictive and I'm not sure how it works in different locales
				err_validation <- append(err_validation,
																 paste0(params_names[i], ": Invalid fileprefix: ",
																 			 params_passed[[i]],
																 			 ". Fileprefix can only contain alphanumeric, '-', and '_' characters"))
			}
		} else if(params_names[i] %in% c("aggregation_timeunit")){
			if( length(params_passed[[i]]) != 1 || !(params_passed[[i]] %in% c("day","week","month","quarter","year")) ) {
				err_validation <- append(err_validation,
																 paste0(params_names[i], ": Values allowed are: day, week, month, quarter, year but found: [ class = ", class(params_passed[[i]]), "; contents = ", substr(toString(params_passed[[i]]),1,100), "]"))
			}

		} else if(params_names[i] %in% c("dataset_shortdesc")){
			if( !is.null(params_passed[[i]]) && ( !is.character(params_passed[[i]]) || length(params_passed[[i]]) != 1 ) ){
			err_validation <- append(err_validation,
															 paste0(params_names[i], ": Expected a character string but found: [ class = ", class(params_passed[[i]]), "; contents = ", substr(toString(params_passed[[i]]),1,500), "]"))
			}
		} else if(params_names[i] %in% c("na")){
			if( any(!is.character(params_passed[[i]])) ){
			err_validation <- append(err_validation,
															 paste0(params_names[i], ": Expected a vector of character strings but found: [ class = ", class(params_passed[[i]]), "; contents = ", substr(toString(params_passed[[i]]),1,500), "]"))
			}
		} else if(params_names[i] %in% c("format", "save_filetype")){
			# ignore for now as currently dealt with inside function
		}
	}

	if (length(err_validation) > 0) {
  stop_custom(.subclass = "invalid_param_type",
  						message = paste0("Invalid argument(s) supplied.\n",
  														 paste(err_validation, collapse = "\n")))
	}

}


# -----------------------------------------------------------------------------
# logging functions
#' Initialise log file
#'
#' Choose a directory in which to save the log file.
#' If this is not called, no log file is created.
#'
#' @param dirpath String containing directory to save log file
#' @export
log_initialise <- function(dirpath){

	validate_params_required(match.call())
	validate_params_type(match.call(),
											 dirpath = dirpath)
	packageenvironment$logname <- file.path(dirpath, paste0(utils::packageName(), "_", format(Sys.time(), "%Y%m%d%_%H%M%S"), ".log"))
	log_message(paste("Log file initialised.", "Package version", utils::packageVersion(utils::packageName()), ";", R.Version()$version.string))
}

#' Closes any active log file
#'
#' @export
log_close <- function(){
	if( exists("logname", envir = packageenvironment) ){
		rm("logname", envir = packageenvironment)
	}
}

# TODO: decide if showprogress should be a global setting e.g. package option ( options("mypkg-myval"=3) )
log_message <- function(message, showprogress = FALSE){
	if( exists("logname", envir = packageenvironment) ){
		log_con <- file(packageenvironment$logname, open="a")
		writeLines(paste(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), ":", message), con = log_con)
		close(log_con)
	}
	if(showprogress) cat(message, "\n")
}

log_function_start <- function(functionname){
	log_message(paste("[START FUNCTION]", functionname), FALSE)
}

log_function_end <- function(functionname){
	log_message(paste("[END FUNCTION]", functionname), FALSE)
}

# custom errors with classes that can be tested for
# copied from https://adv-r.hadley.nz/conditions.html#custom-conditions
stop_custom <- function(.subclass, message, call = NULL, ...) {
  err <- structure(
    list(
      message = message,
      call = call,
      ...
    ),
    class = c(.subclass, "error", "condition")
  )
  stop(err)
}

######################################################
# dummy functions set up for unit testing. testthat can't find them when they are defined in the test_xxx files

# test_utilities
testfn_params_required <- function(p1, p2, p3 = NULL){
	validate_params_required(match.call())
}

testfn_params_type <- function(df, fieldtypes, sourcedata, aggregatedata, override_columnnames = FALSE, na = c("","NA","NULL"), dataset_shortdesc = "shortdesc", aggregation_timeunit = "day", save_directory = ".", save_filename = "filename", showprogress = TRUE, log_directory = NULL, format = "html", save_filetype = "csv", save_fileprefix = "", default_fieldtype = ft_ignore()){
		if(missing(df)){
			df <- data.frame("Fieldname" = 123)
		}
		if(missing(fieldtypes)){
			fieldtypes <- daiquiri::fieldtypes(Col_tp = ft_timepoint())
		}
		if(missing(sourcedata)){
			sourcedata <- structure(list(datafields = NA), class = "sourcedata")
		}
		if(missing(aggregatedata)){
			aggregatedata <- structure(list(datafields = NA), class = "aggregatedata")
		}

		validate_params_type(match.call(),
												 df = df,
												 fieldtypes = fieldtypes,
												 override_columnnames = override_columnnames,
												 na = na,
												 dataset_shortdesc = dataset_shortdesc,
												 aggregation_timeunit = aggregation_timeunit,
												 save_directory = save_directory,
												 save_filename = save_filename,
												 showprogress = showprogress,
												 log_directory = log_directory,
												 sourcedata = sourcedata,
												 aggregatedata = aggregatedata,
												 format = format,
												 save_filetype = save_filetype,
												 save_fileprefix = save_fileprefix,
												 default_fieldtype = default_fieldtype)
	}


