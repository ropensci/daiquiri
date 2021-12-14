# Generic functions
# TODO: Check file paths use slashes consistently

validate_param_file <- function(filepath){
	if(!file.exists(filepath)){
		stop(paste("Invalid file: ", filepath))
	}
}

validate_param_dir <- function(dirpath){
	# remove (up to two) trailing slashes (file.path and normalizePath don't do this for you)
	cleanpath <- paste0(substring(dirpath, 1, nchar(dirpath) - 2), gsub("[/\\]", "", substring(dirpath, nchar(dirpath) - 1)))

	if(!file.exists(cleanpath)){
		stop(paste("Invalid directory: ", cleanpath))
	}
	normalizePath(cleanpath, winslash = "/")
}

validate_param_filetype_plot <- function(filetype){
	# based on current ggsave options
	if(!filetype %in% c("eps", "ps", "tex", "pdf", "jpeg", "tiff", "png", "bmp", "svg", "wmf")){
		stop(paste("Invalid filetype: ", filetype))
	}
}

# TODO: Add unit tests
validate_param_savefilename <- function(filename, allownull = FALSE){
	if( is.null(filename) ){
		if( allownull == FALSE ){
			stop(paste0("Filename is NULL. You must supply a filename."), call. = FALSE)
		}
	} else if( grepl("[^a-zA-Z0-9_-]", filename) || nchar(filename) == 0 ){
		# NOTE: this is very restrictive and I'm not sure how it works in different locales
		stop(paste0("Invalid filename: ", filename, ". Filename can only contain alphanumeric, '-', and '_' characters, and should not include the file extension."),
				 call. = FALSE)
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
	save_dir <- validate_param_dir(dirpath)
	packageenvironment$logname <- file.path(save_dir, paste0(packageName(), "_", format(Sys.time(), "%Y%m%d%_%H%M%S"), ".log"))
	log_message(paste("Log file initialised.", "Package version", utils::packageVersion(packageName()), ";", R.Version()$version.string))
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
