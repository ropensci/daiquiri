## TEST SOURCEDATA FUNCTIONS ##

# set up conditions for testing
# Restart R: Ctrl-Shift-F10
rm(list = ls())
devtools::load_all(".", reset = TRUE)



# test dataset
filename <- ".\\devtesting\\testdata\\antibiotics_example.csv"
# specify column types
# 12 columns
fieldtypes <- fieldtypes(PrescriptionID = ft_uniqueidentifier()
																	,PatientID = ft_uniqueidentifier()
																	,PrescriptionDate = ft_timepoint()
																	,PrescriptionType = ft_categorical(aggregate_by_each_category=TRUE)
																	,AdmissionDate = ft_datetime(includes_time = FALSE)
																	,Drug = ft_categorical()
																	,Formulation = ft_freetext()
																	,Dose = ft_numeric()
																	,DoseUnit = ft_ignore()
																	,FirstAdministrationDateTime = ft_datetime()
																 ,NumberOfDosesAdministered = ft_numeric()
																 ,AntibioticsSource = ft_categorical())

source_df <- readr::read_csv(filename, col_types = fieldtypes_to_cols(fieldtypes, readfunction = "readr", alltostring = TRUE), na=c("","NULL"))

# add empty timepoints
source_df[1:5,"PrescriptionDate"] <- NA

# add nonconformant values
source_df[11,"AdmissionDate"] <- "2014-06-31"
source_df[21:23,"Dose"] <- "See instructions"
source_df[24:25,"Dose"] <- "10.1.2"
source_df[26,"Dose"] <- "10tablets"
source_df[27,"Dose"] <- "10 "

# add duplicates
source_df <- rbind(source_df, source_df[100:200,])

saveRDS(source_df, ".\\devtesting\\testdata\\antibiotics_exceptions.rds")

source_df <- readRDS(".\\devtesting\\testdata\\antibiotics_exceptions.rds")


sourcedata <- sourcedata(data.table(source_df), fieldtypes, sourcename = "antibiotics_exceptions", showprogress=TRUE)

# check warnings against what was added above
sourcedata$validation_warnings
sourcedata$rows_duplicates_n

summarise_source_data(sourcedata, showprogress = FALSE)
print(sourcedata)


###########################################
# non-standard date formats
# test dataset
filename <- "./devtesting/testdata/abx2014ukdates.csv"
# specify column types
fieldtypes <- fieldtypes(PrescriptionID = ft_uniqueidentifier()
												 ,PrescriptionDate = ft_timepoint(format = "%d/%m/%Y %H:%M")
												 ,AdmissionDate = ft_datetime(includes_time = FALSE, format = "%d/%m/%Y")
												 ,Drug = ft_categorical()
												 ,Dose = ft_numeric()
												 ,DoseUnit = ft_ignore()
												 ,PatientID = ft_ignore()
												 ,SourceSystem = ft_categorical())

daiqobj <- create_report(filename,
													fieldtypes = fieldtypes,
													textfile_contains_columnnames = TRUE,
													override_columnnames = FALSE,
													na = c("","NULL"),
													aggregation_timeunit = "week",
													save_directory = ".",
													save_filename = NULL,
													showprogress = TRUE,
													log_directory = "./")


###########################################
# supply data frame where not all columns are char. readr::type_convert does not skip columns that are not char
testfile <- "./inst/extdata/abx2014.csv"
testfile_fieldtypes <- fieldtypes(PrescriptionID = ft_uniqueidentifier()
																	,PrescriptionDate = ft_timepoint()
																	,AdmissionDate = ft_datetime()
																	,Drug = ft_freetext()
																	,Dose = ft_ignore()
																	,DoseUnit = ft_categorical()
																	,PatientID = ft_ignore()
																	,SourceSystem = ft_categorical())
testdf <- readr::read_csv(testfile, col_names = TRUE
													,na = c("","NULL")
)
create_report(testdf,testfile_fieldtypes,
							aggregation_timeunit = "day",
							na = c("","NULL"),
							save_directory = ".",
							save_filename = NULL,
							showprogress = TRUE)


###########################################
# special chars
# TODO: TO COMPLETE
filename <- "./devtesting/testdata/specialchars.csv"
# specify column types
fieldtypes <- fieldtypes(date = ft_timepoint()
												 ,col1 = ft_freetext()
												 ,col2 = ft_freetext()
												 ,col3 = ft_numeric()
)

sourceobj <- load_data(filename, fieldtypes = fieldtypes)

# look at all warnings and pre/post cleaned data
#source("./R/fieldtypes.R")
source_df <- readr::read_csv(filename, col_types = fieldtypes_to_cols(fieldtypes, readfunction = "readr", alltostring = TRUE), na=c("","NULL"))
raw_warnings <- NULL
clean_dt <- withCallingHandlers(
	readr::type_convert(source_df, fieldtypes_to_cols(fieldtypes, readfunction = "readr"), na = na),
	warning = function(w) {
		raw_warnings <<- append(raw_warnings, conditionMessage(w))
		invokeRestart("muffleWarning")
	}
)
raw_warnings
