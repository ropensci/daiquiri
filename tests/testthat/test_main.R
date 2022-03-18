test_that("Test read_data() on csv file created using Excel", {
	# read file correctly
	rawdata <- read_data(
	  test_path("testdata", "specialchars_excel.csv"),
	  delim = ",",
	  col_names = TRUE
	  )
	expect_equal(nrow(rawdata), 8)
	expect_equal(length(rawdata), 4)

	# read file assuming no column names
	rawdata <- read_data(
	  test_path("testdata", "specialchars_excel.csv"),
	  delim = ",",
	  col_names = FALSE
	  )
	expect_equal(nrow(rawdata), 9)
	expect_equal(length(rawdata), 4)

	# read file without acknowledging quotes
	rawdata <- read_data(
	  test_path("testdata", "specialchars_excel.csv"),
	  delim = ",",
	  quote = "",
	  col_names = TRUE
	  )
	expect_warning(
		expect_warning(as.numeric(rawdata$numeric_good), "NAs introduced by coercion"),
		"One or more parsing issues")

})

test_that("create_report() params are present and of correct type", {
	expect_error(create_report(fieldtypes = fieldtypes(Col_tp = ft_timepoint())),
							 class = "invalid_param_missing")

	expect_error(create_report(df = data.frame("Fieldname" = 123)),
							 class = "invalid_param_missing")

	expect_error(create_report(df = c("Fieldname", 123),
														fieldtypes = fieldtypes(Col_tp = ft_timepoint())),
							 class = "invalid_param_type")

	expect_error(create_report(df = data.frame("Fieldname" = 123),
														fieldtypes = TRUE),
							 class = "invalid_param_type")

})


test_that("create_data() creates report and returns daiquiri object successfully", {
	testdf <- read_data(test_path("testdata", "completetestset.csv"))
	testdaiqobj <- create_report(testdf,
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
															 dataset_shortdesc = "completetestset",
															 aggregation_timeunit = "week",
															 save_directory = tempdir(),
															 save_filename = "daiquiri_testthatreport",
															 showprogress = FALSE)

	expect_s3_class(testdaiqobj, "daiquiri_object")
	expect_s3_class(testdaiqobj$sourcedata, "sourcedata")
	expect_s3_class(testdaiqobj$aggregatedata, "aggregatedata")

	expect_equal(testdaiqobj$dataset_shortdesc, "completetestset")
	expect_equal(testdaiqobj$report_filename, file.path(tempdir(), "daiquiri_testthatreport.html"))

	# clean up
	expect_true(file.remove(testdaiqobj$report_filename))
})
