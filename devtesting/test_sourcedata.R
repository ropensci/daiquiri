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

checkobj <- check_dataset(filename,
													fieldtypes = fieldtypes,
													textfile_contains_columnnames = TRUE,
													override_columnnames = FALSE,
													na = c("","NULL"),
													aggregation_timeunit = "week",
													save_directory = ".",
													save_filename = NULL,
													showprogress = TRUE,
													log_directory = "./")

