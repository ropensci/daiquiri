# initialise package environment (doesn't work if put in .onLoad)
# TODO: consider using actual package name
packageenvironment <- new.env(parent = emptyenv())
# tell data.table that you as a package developer have designed your code to intentionally
#  rely on data.table functionality even though it is not mentioned in NAMESPACE file.
#  This is to ensure the "[" function works even though it can't be prefixed with data.table::
.datatable.aware = TRUE

# TODO: move this main file?  Or maybe just drop.  Don't like how the message prints in red
# display startup message to console when package is attached
.onAttach <- function(libname, pkgname) {
	packageStartupMessage("Automatic Change-point Detection for Electronic Health Records")
}
