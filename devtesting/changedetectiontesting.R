

# set up conditions for testing
# Restart R: Ctrl-Shift-F10
rm(list = ls())
devtools::load_all(".", reset = TRUE)

# create log file
# TODO: figure out where this should be
log_initialise("./devtesting/testoutput")

# test dataset
testfile <- "./devtesting/testdata/abx2014.csv"

# specify column types
testfile_fieldtypes <- fieldtypes(PrescriptionID = ft_ignore()
																	,TimepointDate = ft_timepoint()
																	,PrescriptionDate = ft_ignore()
																	,PrescriptionType = ft_categorical()
																	,AdmissionDate = ft_datetime()
																	,Drug = ft_categorical()
																	,Formulation = ft_freetext()
																	,Dose = ft_numeric()
																	,DoseUnit = ft_categorical()
																	,FirstAdministrationDateTime = ft_datetime()
																	,Clusterid = ft_uniqueidentifier()
																	,AntibioticsSource = ft_categorical(aggregate_by_each_category=TRUE))

testfile <- "./devtesting/testdata/abx_IORD2018.csv"
# specify column types
testfile_fieldtypes <- fieldtypes(PrescriptionID = ft_ignore()
																	,PrescriptionDate = ft_timepoint()
																	,PrescriptionStatus = ft_ignore()
																	,PrescriptionType = ft_categorical(aggregate_by_each_category = TRUE)
																	,AdmissionDate = ft_datetime()
																	,Prescriber = ft_categorical()
																	,Drug = ft_categorical()
																	,Formulation = ft_freetext()
																	,Dose = ft_numeric()
																	,DoseUnit = ft_ignore()
																	,Frequency = ft_ignore()
																	,Route = ft_ignore()
																	,Indication = ft_ignore()
																	,ScheduledStartDateTime = ft_ignore()
																	,ScheduledStopDateTime = ft_ignore()
																	,DurationEnteredByPrescriber = ft_ignore()
																	,ReviewDateTime = ft_ignore()
																	,FirstAdministrationDateTime = ft_ignore()
																	,LastAdministrationDateTime = ft_ignore()
																	,NumberOfDosesAdministered = ft_ignore()
																	,SpecialInstructions = ft_ignore()
																	,TreatmentFunctionCode = ft_ignore()
																	,TreatmentSpecialty = ft_ignore()
																	,ConsultantName = ft_ignore()
																	,PatientOrderLocation = ft_ignore()
																	,Weight = ft_ignore()
																	,idsetuid = ft_ignore()
																	,Clusterid = ft_uniqueidentifier()
																	,LinkedNHSNumber = ft_ignore()
																	,LinkedORHNumber = ft_ignore()
																	,LinkedSurname = ft_ignore()
																	,LinkedForename = ft_ignore()
																	,LinkedSex = ft_ignore()
																	,LinkedBirthdate = ft_ignore()
																	,LinkedDeathdate = ft_ignore()
																	,SourceNHSNumber = ft_ignore()
																	,SourceORHNumber = ft_ignore()
																	,SourceSurname = ft_ignore()
																	,SourceForename = ft_ignore()
																	,SourceSex = ft_ignore()
																	,SourceBirthdate = ft_ignore()
																	,SourceDeathdate = ft_ignore()
																	,SpineCheckDate = ft_ignore()
																	,LinkageWarningFlag = ft_ignore()
																	,LinkageResult = ft_ignore()
																	,AntibioticsSource = ft_categorical())

testcpdsourcedata <- load_data(testfile, fieldtypes = testfile_fieldtypes, na=c("","NULL"), showprogress=TRUE, log_directory = "./devtesting/testoutput/")

testcpdsourcedata <- load_data(source_df, fieldtypes = testfile_fieldtypes, na=c("","NULL"))
testcpdsourcedata <- load_data(clean_df, fieldtypes = fieldtypes(PrescriptionID = ft_uniqueidentifier()
																																		,TimepointDate = ft_timepoint()
																																		,PrescriptionDate = ft_datetime()
																																		,PrescriptionType = ft_categorical()
																																		,AdmissionDate = ft_datetime()
																																		,Drug = ft_categorical()
																																		,Formulation = ft_freetext()
																																		,Dose = ft_numeric()
																																		#																																		,DoseUnit = ft_ignore()
																																		,FirstAdministrationDateTime = ft_datetime()
																																		,Clusterid = ft_uniqueidentifier()
																																		,AntibioticsSource = ft_categorical()), na=c("","NULL"))


testcpdsourcedata <- load_data("./devtesting/testdata/abx_IORD2018.csv", fieldtypes(PrescriptionID = ft_uniqueidentifier()
																																											 ,PrescriptionDate = ft_timepoint()
																																											 ,PrescriptionStatus = ft_categorical()
																																											 ,PrescriptionType = ft_categorical(aggregate_by_each_category = TRUE)
																																											 ,AdmissionDate = ft_datetime()
																																											 ,Prescriber = ft_categorical()
																																											 ,Drug = ft_categorical()
																																											 ,Formulation = ft_freetext()
																																											 ,Dose = ft_numeric()
																																											 ,DoseUnit = ft_categorical()
																																											 ,Frequency = ft_categorical()
																																											 ,Route = ft_categorical()
																																											 ,Indication = ft_freetext()
																																											 ,ScheduledStartDateTime = ft_datetime()
																																											 ,ScheduledStopDateTime = ft_datetime()
																																											 ,DurationEnteredByPrescriber = ft_categorical()
																																											 ,ReviewDateTime = ft_datetime()
																																											 ,FirstAdministrationDateTime = ft_datetime()
																																											 ,LastAdministrationDateTime = ft_datetime()
																																											 ,NumberOfDosesAdministered = ft_numeric()
																																											 ,SpecialInstructions = ft_ignore()
																																											 ,TreatmentFunctionCode = ft_categorical()
																																											 ,TreatmentSpecialty = ft_categorical()
																																											 ,ConsultantName = ft_categorical()
																																											 ,PatientOrderLocation = ft_categorical()
																																											 ,Weight = ft_numeric()
																																											 ,idsetuid = ft_uniqueidentifier()
																																											 ,Clusterid = ft_uniqueidentifier()
																																											 ,LinkedNHSNumber = ft_uniqueidentifier()
																																											 ,LinkedORHNumber = ft_uniqueidentifier()
																																											 ,LinkedSurname = ft_freetext()
																																											 ,LinkedForename = ft_freetext()
																																											 ,LinkedSex = ft_categorical()
																																											 ,LinkedBirthdate = ft_datetime()
																																											 ,LinkedDeathdate = ft_datetime()
																																											 ,SourceNHSNumber = ft_uniqueidentifier()
																																											 ,SourceORHNumber = ft_uniqueidentifier()
																																											 ,SourceSurname = ft_freetext()
																																											 ,SourceForename = ft_freetext()
																																											 ,SourceSex = ft_categorical()
																																											 ,SourceBirthdate = ft_datetime()
																																											 ,SourceDeathdate = ft_datetime()
																																											 ,SpineCheckDate = ft_datetime()
																																											 ,LinkageWarningFlag = ft_categorical()
																																											 ,LinkageResult = ft_categorical()
																																											 ,AntibioticsSource = ft_categorical())
																	, na=c("","NULL"), showprogress=TRUE, log_directory = "./devtesting/testoutput/")


testcpdsourcedata <- load_data("./devtesting/testdata/abx_IORD2018.csv", fieldtypes(PrescriptionID = ft_ignore()
																																											 ,PrescriptionDate = ft_timepoint()
																																											 ,PrescriptionStatus = ft_ignore()
																																											 ,PrescriptionType = ft_categorical(aggregate_by_each_category = TRUE)
																																											 ,AdmissionDate = ft_datetime()
																																											 ,Prescriber = ft_categorical()
																																											 ,Drug = ft_categorical()
																																											 ,Formulation = ft_freetext()
																																											 ,Dose = ft_numeric()
																																											 ,DoseUnit = ft_ignore()
																																											 ,Frequency = ft_ignore()
																																											 ,Route = ft_ignore()
																																											 ,Indication = ft_ignore()
																																											 ,ScheduledStartDateTime = ft_ignore()
																																											 ,ScheduledStopDateTime = ft_ignore()
																																											 ,DurationEnteredByPrescriber = ft_ignore()
																																											 ,ReviewDateTime = ft_ignore()
																																											 ,FirstAdministrationDateTime = ft_ignore()
																																											 ,LastAdministrationDateTime = ft_ignore()
																																											 ,NumberOfDosesAdministered = ft_ignore()
																																											 ,SpecialInstructions = ft_ignore()
																																											 ,TreatmentFunctionCode = ft_ignore()
																																											 ,TreatmentSpecialty = ft_ignore()
																																											 ,ConsultantName = ft_ignore()
																																											 ,PatientOrderLocation = ft_ignore()
																																											 ,Weight = ft_ignore()
																																											 ,idsetuid = ft_ignore()
																																											 ,Clusterid = ft_uniqueidentifier()
																																											 ,LinkedNHSNumber = ft_ignore()
																																											 ,LinkedORHNumber = ft_ignore()
																																											 ,LinkedSurname = ft_ignore()
																																											 ,LinkedForename = ft_ignore()
																																											 ,LinkedSex = ft_ignore()
																																											 ,LinkedBirthdate = ft_ignore()
																																											 ,LinkedDeathdate = ft_ignore()
																																											 ,SourceNHSNumber = ft_ignore()
																																											 ,SourceORHNumber = ft_ignore()
																																											 ,SourceSurname = ft_ignore()
																																											 ,SourceForename = ft_ignore()
																																											 ,SourceSex = ft_ignore()
																																											 ,SourceBirthdate = ft_ignore()
																																											 ,SourceDeathdate = ft_ignore()
																																											 ,SpineCheckDate = ft_ignore()
																																											 ,LinkageWarningFlag = ft_ignore()
																																											 ,LinkageResult = ft_ignore()
																																											 ,AntibioticsSource = ft_categorical())
																	, na=c("","NULL")
																	, showprogress = TRUE
																	, log_directory = "./devtesting/testoutput/")


testcpdsourcedata2014 <- load_data("./devtesting/testdata/abx2014.csv", fieldtypes = fieldtypes(PrescriptionID = ft_ignore()
																																																	 ,TimepointDate = ft_timepoint()
																																																	 ,PrescriptionDate = ft_ignore()
																																																	 ,PrescriptionType = ft_ignore()
																																																	 ,AdmissionDate = ft_datetime()
																																																	 ,Drug = ft_ignore()
																																																	 ,Formulation = ft_ignore()
																																																	 ,Dose = ft_numeric()
																																																	 ,DoseUnit = ft_categorical()
																																																	 ,FirstAdministrationDateTime = ft_datetime()
																																																	 ,Clusterid = ft_ignore()
																																																	 ,AntibioticsSource = ft_categorical()), na=c("","NULL")
																			, log_directory = "./devtesting/testoutput/")

#print(testcpdsourcedata)

testcpddata2014_byweek <- aggregate_data(testcpdsourcedata2014, aggregation_timeunit = "week", changepointmethods = "all", showprogress = TRUE)


testcpddata_byday <- aggregate_data(testcpdsourcedata, changepointmethods = "none", showprogress = TRUE)
testcpddata_bymonth <- aggregate_data(testcpdsourcedata, aggregation_timeunit = "month", changepointmethods = "none", showprogress = TRUE)
testcpddata_byweek <- aggregate_data(testcpdsourcedata, aggregation_timeunit = "week", changepointmethods = "none", showprogress = TRUE)
testcpddata_byquarter <- aggregate_data(testcpdsourcedata, aggregation_timeunit = "quarter", changepointmethods = "none", showprogress = TRUE)
testcpddata_byyear <- aggregate_data(testcpdsourcedata, aggregation_timeunit = "year", changepointmethods = "none", showprogress = TRUE)

summarise_aggregated_data(testcpddata_byday)
print(testcpddata_byyear)
export_aggregated_data(testcpddata_byday, save_directory = "./devtesting/testoutput/")


plot_aggregatefield_byaggtype(aggfield = testcpddata_byday[["subaggregates"]][["AntibioticsSource"]][["ITU"]][["aggregatefields"]]$AdmissionDate,aggtype = "missing_n", changepoint_methods = "none", save_plot = FALSE)

plot_aggregatefield_byaggtype(aggfield = testcpddata_byday$subaggregates[[1]][[2]]$aggregatefields$PrescriptionType,aggtype = "distinct", save_plot = FALSE)


plot_aggregated_data(testcpddata_byday, save_plot = TRUE, save_directory = "./devtesting/testoutput/", save_filetype = "png", showprogress = TRUE)

plot_changepoint_summary(testcpddata_byday, save_plot = TRUE, save_directory = "./devtesting/testoutput", save_filename = "Changepoint_summary_abx_IORD2018", save_filetype = "png", showprogress = TRUE)
plot_changepoint_summary(testcpddata_byday, save_plot = FALSE, save_directory = "./devtesting/testoutput/", save_filetype = "png", showprogress = TRUE)
#plot_changepoint_summary(testcpddata_byday, changepoint_methods = "-is_na", save_plot = TRUE, save_directory = "../devtesting/testoutput/", save_filetype = "png", save_filename = "Changepoint_summary_-isna", showprogress = TRUE)
plot_changepoint_summary(testcpddata_byday$subaggregates[[1]][[1]], save_plot = TRUE, save_directory = "./devtesting/testoutput/", save_filetype = "png", showprogress = TRUE)
plot_changepoint_summary(testcpddata_byday$subaggregates[[1]][[2]], save_plot = TRUE, save_directory = "./devtesting/testoutput/", save_filetype = "png", showprogress = TRUE)

summarise_aggregated_data(testcpddata_byday)



report_data(sourcedata = testcpdsourcedata2014, aggregatedata = testcpddata2014_byweek, save_directory = "./devtesting/testoutput")

report_data(sourcedata = testcpdsourcedata,
								aggregatedata = testcpddata_byday,
								#								aggregatedata = aggregate_data(testcpdsourcedata, aggregation_timeunit = "week", changepointmethods = "none", showprogress = TRUE),
								save_directory = "./devtesting/testoutput",
								save_filename = "report_htmldoc")

rmarkdown::render(input = "./R/report_htmldoc.Rmd"
									, output_dir = "./devtesting/testoutput"
									, params = list(sourcedata = testcpdsourcedata, aggregatedata = testcpddata_byday))

rm(list = ls())
devtools::load_all(".", reset = TRUE)
# test against what will be public dataset
testfile <- system.file("extdata", "abx2014.csv", package = "daiquiri", mustWork = TRUE)
testfile <- "./inst/extdata/abx2014.csv"
testfile_fieldtypes <- fieldtypes(PrescriptionID = ft_uniqueidentifier()
																	,PrescriptionDate = ft_timepoint()
																	,AdmissionDate = ft_datetime(includes_time = FALSE)
																	,Drug = ft_freetext()
																	,Dose = ft_numeric()
																	,DoseUnit = ft_categorical()
																	,PatientID = ft_ignore()
																	,SourceSystem = ft_categorical(aggregate_by_each_category=TRUE))

daiqobj <- create_report(testfile,
													fieldtypes = testfile_fieldtypes,
													textfile_contains_columnnames = TRUE,
													override_columnnames = FALSE,
													na = c("","NULL"),
													aggregation_timeunit = "week",
													save_directory = ".",
													save_filename = NULL,
													showprogress = TRUE,
													log_directory = "./")

testcpdsourcedata <- load_data(testfile, fieldtypes = testfile_fieldtypes, na=c("","NULL"), showprogress=TRUE)
testcpddata_byday <- aggregate_data(testcpdsourcedata, aggregation_timeunit = "day", showprogress = TRUE)


testfile <- "W:/GRAMProj/IORD_GRAMProj_15_20200520_Episodes.csv"

fts <- fieldtypes(ClusterID = ft_uniqueidentifier(),
									PathMatchBloodCultureCollectionDuringSpell  = ft_categorical(),
									SpellID			 = ft_uniqueidentifier(),
									AdmissionDate		 = ft_timepoint(),
									DischargeDate = ft_datetime(),
									PatientClassificationCode			 = ft_categorical(),
									AdmissionMethodCode			 = ft_categorical(),
									AdmissionSourceCode			 = ft_categorical(),
									DischargeMethodCode					 = ft_categorical(),
									IntendedManagementCode = ft_categorical(),
									DischargeDestinationCode	 = ft_categorical(),
									EpisodeID			 = ft_uniqueidentifier(),
									EpisodeNumber			 = ft_numeric(),
									EpisodeStartDate			 = ft_datetime(),
									EpisodeEndDate = ft_datetime(),
									ConsultantMainSpecialtyCode			 = ft_categorical(),
									TreatmentFunctionCode = ft_categorical(),
									FirstWard	 = ft_categorical(),
									PrimaryDiagCode = ft_freetext(),
									NumberOfDiagCodes	 = ft_numeric(),
									Charlson2005			 = ft_numeric(),
									Charlson2012				 = ft_numeric(),
									IMDScore = ft_numeric(),
									PostcodeStub = ft_freetext(),
									LocalAuthority = ft_categorical())

sourcedata <- load_data(testfile,
													 fieldtypes = fts,
													 textfile_contains_columnnames = TRUE,
													 override_columnnames = FALSE,
													 na = c("","NULL"),
													 showprogress = TRUE)

aggregatedata <- aggregate_data(sourcedata, "day", showprogress = TRUE)

report_data(sourcedata, aggregatedata,
								save_directory = ".",
								save_filename = "gramepi",
								showprogress = TRUE
)

# test non-standard date formats
tb_raw <- read_csv("./BhamTB WGSdb_edited.csv", col_types = cols_only(
	`Lab number` = col_character(),
	`Date collected` = col_character(),
	`Date Heat killed for NGS` = col_character(),
	`Library conc (ng/ul)` = col_character()
))


tbreadr <- read_csv("./BhamTB WGSdb_edited.csv", col_types = cols_only(
	`Lab number` = col_character(),
	`Date collected` = col_date(),
	`Date Heat killed for NGS` = col_date(format = "%d.%m.%y"),
	`Library conc (ng/ul)` = col_double()
))


# file reading speed tests
# 139 MB
testfile <- "W:/MonitoringEColi/IORD_MonitoringEColi_30_20220117_Bloodgases.csv"

dstart <- Sys.time()
x <- readr::read_csv(testfile)
cat("read_csv:", Sys.time() - dstart)

dstart <- Sys.time()
x2 <- read.csv(testfile)
cat("read.csv:", Sys.time() - dstart)

dstart <- Sys.time()
x3 <- data.table::fread(testfile)
cat("fread:", Sys.time() - dstart)

dstart <- Sys.time()
x2[] <- lapply(x2, as.character)
cat("as.char:", Sys.time() - dstart)


# non-char columns in data frame
testfile <- "./devtesting/testdata/abx2014.csv"
testdf <- read_data(testfile)
testdf$PatientID <- as.numeric(df$PatientID)
testdf$Dose <- as.numeric(df$Dose)

fieldtypes <- fieldtypes(
	PrescriptionID = ft_uniqueidentifier(),
	PrescriptionDate = ft_timepoint(),
	AdmissionDate = ft_datetime(includes_time = FALSE),
	Drug = ft_freetext(),
	Dose = ft_numeric(),
	DoseUnit = ft_categorical(),
	PatientID = ft_ignore(),
	SourceSystem = ft_categorical(aggregate_by_each_category=TRUE)
)

daiqobj <- create_report(testdf, fieldtypes)
sourcedata <- prepare_data(testdf, fieldtypes)
aggregatedata <- aggregate_data(sourcedata)
report_data(sourcedata , aggregatedata)
