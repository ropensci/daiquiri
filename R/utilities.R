# Generic functions

# check required params have been passed in
# param_names is a vector
validate_params_required <- function(param_names){
	# get the arguments passed into the parent call
	params_passed <- names(as.list(match.call(definition = sys.function(-1), call = sys.call(sys.parent())))[-1])

	if (any(!param_names %in% params_passed)) {
		stop_custom(.subclass = "invalid_param_missing",
								message = paste("Required argument(s) missing:",
																paste(setdiff(param_names, params_passed),
																			collapse=", ")))
	}

}

validate_param_file <- function(filepath){
	if(!file.exists(filepath)){
		stop_custom(.subclass = "invalid_file_or_path",
								message = paste0("File not found: ", filepath))
	}
}

validate_param_dir <- function(dirpath){
	if(!dir.exists(dirpath)){
		stop_custom(.subclass = "invalid_file_or_path",
								message = paste0("Directory not found: ", dirpath))
	}
}

validate_param_savefilename <- function(filename, allownull = FALSE){
	if( is.null(filename) ){
		if( allownull == FALSE ){
			stop_custom(.subclass = "invalid_file_or_path",
									message = "Filename is NULL. You must supply a filename.")
		}
	} else if( grepl("[^a-zA-Z0-9_-]", filename) || nchar(filename) == 0 ){
		# NOTE: this is very restrictive and I'm not sure how it works in different locales
		stop_custom(.subclass = "invalid_file_or_path",
								message = paste0("Invalid filename: ",
																 filename,
																 ". Filename can only contain alphanumeric, '-', and '_' characters, and should not include the file extension."))
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

	validate_params_required(param_names = c("dirpath"))
	validate_param_dir(dirpath)
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
