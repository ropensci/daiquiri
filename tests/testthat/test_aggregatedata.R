
test_that("aggregate_data() params are present and of correct type", {
	expect_error(aggregate_data(),
							 class = "invalid_param_missing")

	expect_error(aggregate_data(sourcedata = data.frame("Fieldname" = 123)),
							 class = "invalid_param_type")

	expect_error(aggregate_data(sourcedata = structure(list(datafields = NA), class = "sourcedata"),
															aggregation_timeunit = "hello"),
							 class = "invalid_param_type")

})


test_that("aggregate_data() creates aggregate object correctly", {
	testdf <- read_data(test_path("testdata", "completetestset.csv"))
	testsourcedata <- prepare_data(testdf,
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
																 showprogress = FALSE
	)
	testaggregatedata <- aggregate_data(testsourcedata, aggregation_timeunit = "week", showprogress = FALSE)

	expect_s3_class(testaggregatedata, "aggregatedata")

	# check the basic structure is ok
	expect_equal(testaggregatedata$timepoint_fieldname, "col_timepoint")
	expect_equal(testaggregatedata$aggregation_timeunit, "week")
	expect_setequal(names(testaggregatedata$aggregatefields), c(names(testsourcedata$cols_imported_indexes), "DUPLICATES", "ALLFIELDSCOMBINED"))
	expect_equal(nrow(testaggregatedata$aggregatefields$col_timepoint$values), 43)
	expect_equal(ncol(testaggregatedata$aggregatefields$col_timepoint$values), 4)
	expect_equal(nrow(testaggregatedata$aggregatefields$col_date_time$values), 43)
	expect_equal(ncol(testaggregatedata$aggregatefields$col_date_time$values), 10)
	expect_equal(nrow(testaggregatedata$aggregatefields$col_date_only$values), 43)
	expect_equal(ncol(testaggregatedata$aggregatefields$col_date_only$values), 8)
	expect_equal(nrow(testaggregatedata$aggregatefields$col_id_num$values), 43)
	expect_equal(ncol(testaggregatedata$aggregatefields$col_id_num$values), 7)
	expect_equal(nrow(testaggregatedata$aggregatefields$col_timepoint$values), 43)
	expect_equal(ncol(testaggregatedata$aggregatefields$col_numeric_clean$values), 10)
	expect_equal(nrow(testaggregatedata$aggregatefields$col_categorical_small$values), 43)
	expect_equal(ncol(testaggregatedata$aggregatefields$col_categorical_small$values), 19)
	expect_equal(nrow(testaggregatedata$aggregatefields$col_categorical_large$values), 43)
	expect_equal(ncol(testaggregatedata$aggregatefields$col_categorical_large$values), 5)
	expect_equal(nrow(testaggregatedata$aggregatefields$col_freetext$values), 43)
	expect_equal(ncol(testaggregatedata$aggregatefields$col_freetext$values), 4)
	expect_equal(nrow(testaggregatedata$aggregatefields$col_simple$values), 43)
	expect_equal(ncol(testaggregatedata$aggregatefields$col_simple$values), 4)
	expect_equal(nrow(testaggregatedata$aggregatefields$DUPLICATES$values), 43)
	expect_equal(ncol(testaggregatedata$aggregatefields$DUPLICATES$values), 3)
	expect_equal(nrow(testaggregatedata$aggregatefields$ALLFIELDSCOMBINED$values), 43)
	expect_equal(ncol(testaggregatedata$aggregatefields$ALLFIELDSCOMBINED$values), 6)

})

test_that("export_aggregated_data() params are present and of correct type", {
	expect_error(export_aggregated_data(save_directory = tempdir()),
							 class = "invalid_param_missing")

	expect_error(export_aggregated_data(aggregatedata = structure(list(datafields = NA), class = "aggregatedata")),
							 class = "invalid_param_missing")

	expect_error(export_aggregated_data(data.frame("Fieldname" = 123), save_directory = tempdir()),
							 class = "invalid_param_type")

})

test_that("export_aggregated_data() generates csv files", {
	testsourcedata <- prepare_data(data.frame(col1 = rep("2022-01-01", 5), col2 = rep(1, 5), col3 = 1:5),
																 fieldtypes = fieldtypes(col1 = ft_timepoint(),
																 												col2 = ft_simple(),
																 												col3 = ft_ignore()),
																 dataset_shortdesc = "exporttestset",
																 showprogress = FALSE
	)
	testaggregatedata <- aggregate_data(testsourcedata, aggregation_timeunit = "day", showprogress = FALSE)
	export_aggregated_data(testaggregatedata, save_directory = tempdir(), save_fileprefix = "test_")

	expect_snapshot_file(file.path(tempdir(), "test_col1.csv"))
	expect_snapshot_file(file.path(tempdir(), "test_col2.csv"))
	expect_snapshot_file(file.path(tempdir(), "test_ALLFIELDSCOMBINED.csv"))
	expect_snapshot_file(file.path(tempdir(), "test_DUPLICATES.csv"))

	expect_false(file.exists(file.path(tempdir(), "test_col3.csv")))

	# clean up
	file.remove(file.path(tempdir(), "test_col1.csv"))
	file.remove(file.path(tempdir(), "test_col2.csv"))
	file.remove(file.path(tempdir(), "test_ALLFIELDSCOMBINED.csv"))
	file.remove(file.path(tempdir(), "test_DUPLICATES.csv"))
})
