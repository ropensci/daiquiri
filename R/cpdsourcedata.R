# Code for creation of cpdsourcedata object
#   which contains both the (vector) data for the field and the relevant metadata

# -----------------------------------------------------------------------------
# individual datafields
# allowable datafield types match field_types

# datafield <- function(x, fieldtype) {
#   structure(x, fieldtypename = get_fieldtype_name(fieldtype), class = c(paste0("datafield_", get_fieldtype_name(fieldtype)), "datafield"))
# }

# NOTE: not sure if better for object to be the data vector or a list
# TODO: not sure if better to store the entire fieldtype or just its name or even as a separate list in the cpdsourcedata
datafield <- function(x, fieldtype) {
  structure(list(values = x, fieldtype = fieldtype, columnname = names(x[1])),
            class = c(paste0("datafield_", get_fieldtype_name(fieldtype)), "datafield"))
}

is.datafield <- function(x) inherits(x, "datafield")

# -----------------------------------------------------------------------------
# constructor for cpdsourcedata object
# takes in a data frame, fieldtypes specification, and (string) sourcename
# TODO: consider changing class name to cpdsourcedata or cpdimport
cpdsourcedata <- function(data, fieldtypes, sourcename) {
# temp assignments
# data<-testdata
# fieldtypes<-testfile_fieldtypes

  # basic summary info - don't know if appropriate to call directly in structure statement
  # number of columns in source
  numcols_source <- length(fieldtypes)
  # number of columns imported
  numcols_imported <- length(data)
  # total number of rows
  numrows <- nrow(data)
  timepoint_fieldname <- names(which(vapply(fieldtypes, is.fieldtype_timepoint, logical(1))))

  # load data into datafield classes
  dfs <- vector("list", numcols_source)
  cols_imported_indexes <- vector("integer")

  # TODO: can't add NULL to class structure and can't seem to add an empty vector either

  # not sure this vectorisation is working correctly, do I need to rewrite datafield() in a vectorised way too?
  # is_fieldtype_ignore <- vapply(fieldtypes, is.fieldtype_ignore, logical(1))
  # dfs[[is_fieldtype_ignore]] <- datafield(as.vector("ignored"), fieldtypes[is_fieldtype_ignore])
  # dfs[[!is_fieldtype_ignore]] <- datafield(data[names(fieldtypes[!is_fieldtype_ignore])], fieldtypes[[!is_fieldtype_ignore]])

  for (i in 1:numcols_source){
    if (is.fieldtype_ignore(fieldtypes[[i]])){
      dfs[[i]] <- datafield(as.vector("ignored"), fieldtypes[[i]])
    }
    else{
      dfs[[i]] <- datafield(data[names(fieldtypes[i])], fieldtypes[[i]])
      cols_imported_indexes <- c(cols_imported_indexes, i)
      names(cols_imported_indexes)[length(cols_imported_indexes)] <- names(fieldtypes[i])
    }
  }
  names(dfs) <- names(fieldtypes)

  structure(
    list(
      datafields = dfs,
      timepoint_fieldname = timepoint_fieldname,
      nrows = numrows,
      ncols_source = numcols_source,
      ncols_imported = numcols_imported,
      cols_imported_indexes = cols_imported_indexes,
      sourcename = sourcename
    ),
    class = "cpdsourcedata"
  )
}

is.cpdsourcedata <- function(x) inherits(x, "cpdsourcedata")

#' @export
print.cpdsourcedata <- function(x, ...){
	# TODO: to finish
	sourcesummary <- summarise_source_data(x)
	cat("Class: cpdsourcedata\n")
  cat("Source:", x$sourcename, "\n")
  cat("\n")
  cat("Overall:\n")
  cat("Columns in source:", sourcesummary$overall["ncols_source"], "\n")
  cat("Columns imported:", sourcesummary$overall["ncols_imported"], "\n")
  cat("Rows imported:", sourcesummary$overall["nrows"], "\n")
  cat("Column used for timepoint:", sourcesummary$overall["timepoint_fieldname"], "\n")
  cat("Min timepoint value:", sourcesummary$overall["timepoint_min"], "\n")
  cat("Max timepoint value:", sourcesummary$overall["timepoint_max"], "\n")
  cat("\n")
  cat("Datafields:\n")
  print(sourcesummary$datafields)
}

# summarise data
# TODO: consider making this a generic summary() method instead.
#       Help file says summary() is for models but there are a bunch of other objects implementing it too
summarise_source_data <- function(cpdsourcedata){
	#temp assignment
	#  cpdsourcedata<-testcpddata
	# str(cpdsourcedata)

	# summary info for overall dataset
	overall <- c(ncols_source = format(cpdsourcedata$ncols_source),
							 ncols_imported = format(cpdsourcedata$ncols_imported),
							 nrows = format(cpdsourcedata$nrows),
							 timepoint_fieldname = cpdsourcedata$timepoint_fieldname,
							 timepoint_min = get_datafield_min(cpdsourcedata$datafields[[cpdsourcedata$timepoint_fieldname]], format_as_string = TRUE),
							 timepoint_max = get_datafield_max(cpdsourcedata$datafields[[cpdsourcedata$timepoint_fieldname]], format_as_string = TRUE)
	)

	# summary info for each column in dataset
	datafields <- tibble::tibble(fieldname = names(cpdsourcedata$datafields), fieldtype = unlist(lapply(cpdsourcedata$datafields, get_fieldtype_name.datafield)), datatype = unlist(lapply(cpdsourcedata$datafields,get_datafield_basetype, format_as_string = TRUE)), missing = unlist(lapply(cpdsourcedata$datafields, get_datafield_missing, format_as_string = TRUE)), min = unlist(lapply(cpdsourcedata$datafields, get_datafield_min, format_as_string = TRUE)), max = unlist(lapply(cpdsourcedata$datafields, get_datafield_max, format_as_string = TRUE)))

	list(overall = overall, datafields = datafields)
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
    if (is.fieldtype_ignore(datafield$fieldtype)){
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
    if (is.fieldtype_ignore(datafield$fieldtype)){
      NA
    }
    else{
      max(datafield$values[[1]], na.rm = TRUE)
    }
  }
}


get_datafield_missing <- function(datafield, format_as_string = FALSE){
  if (format_as_string){
    if (is.fieldtype_ignore(datafield$fieldtype)){
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
