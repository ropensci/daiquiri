# Generic functions

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
	normalizePath(cleanpath)
}

validate_param_filetype_plot <- function(filetype){
	# based on current ggsave options
	if(!filetype %in% c("eps", "ps", "tex", "pdf", "jpeg", "tiff", "png", "bmp", "svg", "wmf")){
		stop(paste("Invalid filetype: ", filetype))
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
	# TODO: allow user to specify a file name?
	save_dir <- validate_param_dir(dirpath)
	packageenvironment$logname <- file.path(save_dir, paste0("ehrchangepoints_", format(Sys.time(), "%Y%m%d%_%H%M%S"), ".log"))
	log_message(paste("Log file initialised.", "Package version", utils::packageVersion("ehrchangepoints"), ";", R.Version()$version.string))
}

log_newfilename <- function(dirpath){
	save_dir <- validate_param_dir(dirpath)
	file.path(save_dir, paste0("ehrchangepoints_", format(Sys.time(), "%Y%m%d%_%H%M%S"), ".log"))
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
