# uses test data in sister project
# Don't know where this file should live

# set up conditions for testing
# Restart R: Ctrl-Shift-F10
rm(list = ls())
devtools::load_all(".")



# test dataset
testfile <- ".\\devtesting\\testdata\\abx2014.csv"
# specify column types
testfile_fieldtypes <- fieldtypes(PrescriptionID = ft_uniqueidentifier()
																	,TimepointDate = ft_timepoint()
																	,PrescriptionDate = ft_datetime()
																	,PrescriptionType = ft_categorical()
																	,AdmissionDate = ft_datetime()
																	,Drug = ft_categorical()
																	,Formulation = ft_freetext()
																	,Dose = ft_number()
																	,DoseUnit = ft_ignore()
																	,FirstAdministrationDateTime = ft_datetime()
																	,Clusterid = ft_uniqueidentifier()
																	,AntibioticsSource = ft_source())
#
# testfile_bad_fieldtypes <- fieldtypes(PrescriptionID = ft_uniqueidentifier()
#                                       ,TimepointDate = ft_datetime()
#                                       ,PrescriptionDate = ft_datetime()
#                                       ,PrescriptionType = ft_categorical()
#                                       ,AdmissionDate = ft_datetime()
#                                       ,Drug = ft_ignore()
#                                       ,Formulation = ft_ignore()
#                                       ,Dose = ft_number()
#                                       ,DoseUnit = ft_ignore()
#                                       ,FirstAdministrationDateTime = ft_ignore()
#                                       ,Clusterid = ft_ignore()
#                                       ,AntibioticsSource = ft_source())

testcpdsourcedata <- load_dataset(testfile, fieldtypes = testfile_fieldtypes, na=c("","NULL"))
print(testcpdsourcedata)

testcpddata_byday <- aggregate_data(testcpdsourcedata, showprogress = TRUE)
summarise_aggregated_data(testcpddata_byday)
print(testcpddata_byday)
export_aggregated_data(testcpddata_byday, save_directory = ".\\devtesting\\testoutput\\")

plot_aggregated_data(testcpddata_byday, save_plot = TRUE, save_directory = ".\\devtesting\\testoutput\\", save_filetype = "png", showprogress = TRUE)

plot_changepoint_summary(testcpddata_byday, save_plot = TRUE, save_directory = ".\\devtesting\\testoutput\\", save_filetype = "png", showprogress = TRUE)
plot_changepoint_summary(testcpddata_byday, save_plot = FALSE, save_directory = ".\\devtesting\\testoutput\\", save_filetype = "png", showprogress = TRUE)
#plot_changepoint_summary(testcpddata_byday, changepoint_methods = "-is_na", save_plot = TRUE, save_directory = "..\\devtesting\\testoutput\\", save_filetype = "png", save_filename = "Changepoint_summary_-isna", showprogress = TRUE)
plot_changepoint_summary(testcpddata_byday$subaggregates[[1]][[1]], save_plot = TRUE, save_directory = ".\\devtesting\\testoutput\\", save_filetype = "png", showprogress = TRUE)

summarise_aggregated_data(testcpddata_byday)
