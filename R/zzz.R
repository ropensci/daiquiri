# initialise package environment (doesn't work if put in .onLoad)
# TODO: consider using actual package name
packageenvironment <- new.env(parent = emptyenv())

# TODO: move this main file?  Or maybe just drop.  Don't like how the message prints in red
# display startup message to console when package is attached
.onAttach <- function(libname, pkgname) {
	packageStartupMessage("Automatic Change-point Detection for Electronic Health Records")
}
