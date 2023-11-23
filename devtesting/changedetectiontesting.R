# interactive testing during development

# set up conditions for testing
# Restart R: Ctrl-Shift-F10
rm(list = ls())
devtools::load_all(".", reset = TRUE)

# create log file
initialise_log("./devtesting/testoutput")

# test against example dataset
testfile <- "./inst/extdata/example_prescriptions.csv"
df <- read_data(testfile)

testfile_fieldtypes <- field_types(PrescriptionID = ft_uniqueidentifier()
																	,PrescriptionDate = ft_timepoint()
																	,AdmissionDate = ft_datetime(includes_time = FALSE)
																	,Drug = ft_freetext()
																	,Dose = ft_numeric()
																	,DoseUnit = ft_categorical()
																	,PatientID = ft_ignore()
																	,Location = ft_categorical(aggregate_by_each_category=FALSE))

daiqobj <- daiquiri_report(df,
													field_types = testfile_fieldtypes,
													override_column_names = FALSE,
													na = c("","NULL"),
													aggregation_timeunit = "day",
													save_directory = "./devtesting/testoutput",
													save_filename = NULL,
													show_progress = TRUE,
													log_directory = "./devtesting/testoutput")

testsourcedata <- prepare_data(df, field_types = testfile_fieldtypes, na=c("","NULL"), show_progress=TRUE)
aggregated_data <- aggregate_data(testsourcedata, aggregation_timeunit = "day", show_progress = TRUE)
report_data(testsourcedata, aggregated_data,
            report_title = "this is a very long title xxxxxxx xxxxxxxx xxxxxxxx xxxxxxx xxxxxxxxxxxxx xxxxxxxxxxx xxxxxxxxx xxxxx xx xxxxxxx xxxxxxxxx",
						save_directory = "./devtesting/testoutput"
            )
report_data(testsourcedata, aggregated_data,
            report_title = "short title",
						save_directory = "./devtesting/testoutput"
            )


# complete test set
df <- read_data(test_path("testdata", "completetestset.csv"))
testsourcedata <- prepare_data(df,
															 fieldtypes = fieldtypes( col_timepoint_err = ft_ignore(),
															 												 col_timepoint = ft_timepoint(),
															 												 col_date_time_err = ft_ignore(),
															 												 col_date_time = ft_datetime(),
															 												 col_date_only_err = ft_ignore(),
															 												 col_date_only = ft_datetime(includes_time = FALSE),
															 												 col_date_uk_err = ft_ignore(),
															 												 col_date_uk = ft_datetime(includes_time = FALSE, format = "%d/%m/%Y"),
															 												 col_id_num_err = ft_ignore(),
															 												 col_id_num = ft_uniqueidentifier(),
															 												 col_id_string_err = ft_ignore(),
															 												 col_id_string = ft_uniqueidentifier(),
															 												 col_numeric_clean_err = ft_ignore(),
															 												 col_numeric_clean = ft_numeric(),
															 												 col_numeric_dirty_err = ft_ignore(),
															 												 col_numeric_dirty = ft_numeric(),
															 												 col_categorical_small_err = ft_ignore(),
															 												 col_categorical_small = ft_categorical(aggregate_by_each_category = TRUE),
															 												 col_categorical_large_err = ft_ignore(),
															 												 col_categorical_large = ft_categorical(),
															 												 col_freetext_err = ft_ignore(),
															 												 col_freetext = ft_freetext(),
															 												 col_simple_err = ft_ignore(),
															 												 col_simple = ft_simple()),
															 dataset_description = "completetestset",
															 showprogress = FALSE
)


# test modification of df passed into function
df <- data.frame(a=1:5, b = "hi")
testmodset <- function(df){
	dt <- setDT(df)
	dt
}
tracemem(df)
res <- testmod(df)
class(df)
tracemem(df)
class(res)
tracemem(res)

df <- data.frame(a=1:5, b = "hi")
testmodas <- function(df){
	dt <- as.data.table(df)
	dt
}
tracemem(df)
res <- testmodas(df)
class(df)
tracemem(df)
class(res)
tracemem(res)



# stratified
#testfile <- "./inst/extdata/example_prescriptions.csv"
testfile <- "C:/Users/pquan/Documents/Rworkspace/testdaiquiri/testoutput/example_prescriptions_16sites.csv"

df <- read_data(testfile)

field_types <- field_types(PrescriptionID = ft_uniqueidentifier()
																	,PrescriptionDate = ft_timepoint()
																	,AdmissionDate = ft_datetime(includes_time = FALSE)
																	,Drug = ft_freetext()
																	,Dose = ft_numeric()
																	,DoseUnit = ft_categorical()
																	,PatientID = ft_ignore()
#																	,Location = ft_categorical()
																	,Location = ft_strata()
																	)

daiqobj <- daiquiri_report(df,
													field_types,
													override_column_names = FALSE,
													na = c("","NULL"),
													aggregation_timeunit = "day",
													save_directory = "./devtesting/testoutput",
													save_filename = NULL,
													show_progress = TRUE,
													log_directory = "./devtesting/testoutput")

source_data <- prepare_data(df, field_types, na=c("","NULL"), show_progress=TRUE)
aggregated_data <- aggregate_data(source_data, aggregation_timeunit = "day", show_progress = TRUE)
print(Sys.time())
report_data(source_data, aggregated_data)
print(Sys.time())

agg_field_stratified <- aggregated_data$aggregated_fields_stratified[[1]]
aggregation_function <- "n"

plot_overview_combo_static(
  agg_fields = agg_fields,
  aggregation_function = "n",
  lineplot_field_name = aggregated_data$timepoint_field_name,
  title = paste("Records per", "day")
)

plot_overview_combo_static(
  agg_fields = aggregated_data$aggregated_fields_stratified,
  aggregation_function = "missing_n",
  lineplot_field_name = "[ALL_FIELDS_COMBINED]",
  title = paste("Missing per", "day"),
  stratum="SITE1"
)

agg_fields <- aggregated_data$aggregated_fields
stratify <- !is.null(aggregated_data$aggregated_fields_stratified)
agg_fields_strat <- aggregated_data$aggregated_fields_stratified
strata_labels <- source_data$strata_labels

      plot_subcat_heatmap_static(
        agg_field = agg_fields_strat[[source_data$strata_field_name]],
        agg_fun = "subcat_n"
      )

  plot_overview_combo_static(
    agg_field = agg_fields,
    agg_fun = "sum",
    lineplot_field_name = "[DUPLICATES]",
    lineplot_fill_colour = "yellow",
    heatmap_fill_colour = "orange",
    title = paste("Total duplicate records per", aggregated_data$aggregation_timeunit)
  )

show_progress<-TRUE
for (i in seq_along(names(agg_fields))) {
  agg_field <- agg_fields[[i]]
  field_name <- names(agg_fields)[i]
  cat(Sys.time(), ":", field_name, "\n")

#  if (stratify) {
    if (field_name != source_data$strata_field_name) {
#      cat("\n###", field_name, " {.tabset}\n")
      aggregation_functions <- agg_field$function_list
      for (aggregation_function in aggregation_functions) {
  cat(Sys.time(), ":", aggregation_function, "\n")
#        cat("\n####", agg_fun_friendly_name(aggregation_function, "short"), "\n")
        # get corresponding agg_field_strat object
        agg_field_strat <- agg_fields_strat[[field_name]]
        p <-
          plot_stratified_combo_static(
            agg_field = agg_field,
            agg_field_strat = agg_field_strat,
            aggregation_function = aggregation_function)
        print(p)
#        cat("\n")
      }
    }
#  }
}

timepoint_field_name <- "AdmissionDate"
cat(template_field_types_string(df, timepoint_field_name = timepoint_field_name))
cat(template_field_types_string(df))

default_field_types(df, timepoint_field_name = timepoint_field_name)


daiqobj <- daiquiri_report(df,
													field_types = field_types_minimal(timepoint_field_name = "AdmissionDate"),
													override_column_names = FALSE,
													na = c("","NULL"),
													aggregation_timeunit = "day",
													save_directory = "./devtesting/testoutput",
													save_filename = NULL,
													show_progress = TRUE,
													log_directory = "./devtesting/testoutput")
