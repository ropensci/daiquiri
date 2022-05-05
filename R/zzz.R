# initialise package environment (doesn't work if put in .onLoad)
packageenvironment <- new.env(parent = emptyenv())
# tell data.table that you as a package developer have designed your code to intentionally
#  rely on data.table functionality even though it is not mentioned in NAMESPACE file.
#  This is to ensure the "[" function works even though it can't be prefixed with data.table::
.datatable.aware <- TRUE
