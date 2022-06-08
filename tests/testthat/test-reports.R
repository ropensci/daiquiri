
test_that("report_data() params are present and of correct type", {
	expect_error(report_data(),
							 class = "invalid_param_missing")

	expect_error(report_data(sourcedata = data.frame("Fieldname" = 123),
													 aggregatedata = structure(list(datafields = NA),
													 													class = "aggregatedata")),
							 class = "invalid_param_type")

	expect_error(report_data(sourcedata = structure(list(datafields = NA),
																									class = "sourcedata"),
													 aggregatedata = data.frame("Fieldname" = 123)),
							 class = "invalid_param_type")

})

test_that("report_data() creates report and returns path successfully", {
	testdf <- read_data(test_path("testdata", "completetestset.csv"))
	testsourcedata <- prepare_data(
		testdf,
		fieldtypes = fieldtypes(
			col_timepoint_err = ft_ignore(),
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
			col_simple = ft_simple()
		),
		dataset_shortdesc = "completetestset",
		showprogress = FALSE
	)
	testaggregatedata <-
		aggregate_data(testsourcedata,
									 aggregation_timeunit = "week",
									 showprogress = FALSE)
	testreportpath <-
		report_data(
			testsourcedata,
			testaggregatedata,
			save_directory = tempdir(),
			save_filename = "daiquiri_testthatreport",
			showprogress = FALSE)

	expect_type(testreportpath, "character")

	# clean up
	expect_true(file.remove(testreportpath))
})

test_that("plots still work when all values are missing", {
	testdf <-
		data.table::data.table("col_timepoint" = paste0("2022-01-", seq(10, 31)),
													 "col_numeric_missing" = "")
	testsourcedata <-
		prepare_data(
			testdf,
			fieldtypes = fieldtypes(
				col_timepoint = ft_timepoint(),
				col_numeric_missing = ft_numeric()
			),
			dataset_shortdesc = "blankplottest",
			override_columnnames = FALSE,
			na = c("", "NULL"),
			showprogress = FALSE
		)
	testdata_byday <-
		aggregate_data(testsourcedata,
									 aggregation_timeunit = "day",
									 showprogress = FALSE)

	expect_s3_class(
		plot_timeseries_static(
			aggfield = testdata_byday$aggregatefields$col_numeric_missing,
			aggtype = "missing_n"
		),
		"ggplot"
	)
	expect_s3_class(
		plot_overview_totals_static(
			aggfield = testdata_byday$aggregatefields$col_numeric_missing,
			aggtype = "missing_n"
		),
		"ggplot"
	)
})
