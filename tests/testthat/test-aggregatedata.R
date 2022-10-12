
test_that("aggregate_data() requires a sourcedata param", {
	expect_error(aggregate_data(),
							 class = "invalid_param_missing")
})

test_that("aggregate_data() requires sourcedata param to be a sourcedata object", {
	expect_error(aggregate_data(sourcedata = data.frame("Fieldname" = 123)),
							 class = "invalid_param_type")

})

test_that("aggregate_data() requires aggregation_timeunit param to be one of day/week/month/quarter/year", {
	expect_error(aggregate_data(
								sourcedata = structure(list(datafields = NA), class = "sourcedata"),
								aggregation_timeunit = "hello"
								),
							 class = "invalid_param_type")

})

test_that("aggregate_data() creates aggregate object correctly", {
	testdf <- read_data(test_path("testdata", "completetestset.csv"))
	testsourcedata <-
		prepare_data(
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

	expect_s3_class(testaggregatedata, "aggregatedata")

	# check the basic structure is ok
	expect_equal(testaggregatedata$timepoint_fieldname, "col_timepoint")
	expect_equal(testaggregatedata$aggregation_timeunit, "week")
	expect_setequal(names(testaggregatedata$aggregatefields),
									c(names(testsourcedata$cols_imported_indexes),
										"[DUPLICATES]",
										"[ALLFIELDSCOMBINED]"))
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
	expect_equal(nrow(testaggregatedata$aggregatefields[["[DUPLICATES]"]]$values), 43)
	expect_equal(ncol(testaggregatedata$aggregatefields[["[DUPLICATES]"]]$values), 3)
	expect_equal(nrow(testaggregatedata$aggregatefields[["[ALLFIELDSCOMBINED]"]]$values), 43)
	expect_equal(ncol(testaggregatedata$aggregatefields[["[ALLFIELDSCOMBINED]"]]$values), 6)

})


test_that("export_aggregated_data() requires a aggregatedata param", {
	expect_error(export_aggregated_data(save_directory = tempdir()),
							 class = "invalid_param_missing")

})

test_that("export_aggregated_data() requires a save_directory param", {
	expect_error(export_aggregated_data(
								aggregatedata = structure(list(datafields = NA), class = "aggregatedata")),
							 class = "invalid_param_missing")

})

test_that("export_aggregated_data() requires aggregatedata param to be an aggregatedata object", {
	expect_error(export_aggregated_data(
								aggregatedata = data.frame("Fieldname" = 123),
								save_directory = tempdir()),
							 class = "invalid_param_type")

})

test_that("export_aggregated_data() generates csv files", {
	testsourcedata <-
		prepare_data(
			data.frame(
				col1 = rep("2022-01-01", 5),
				col2 = rep(1, 5),
				col3 = 1:5
			),
			fieldtypes = fieldtypes(
				col1 = ft_timepoint(),
				col2 = ft_simple(),
				col3 = ft_ignore()
			),
			dataset_shortdesc = "exporttestset",
			showprogress = FALSE
		)
	testaggregatedata <- aggregate_data(testsourcedata,
																			aggregation_timeunit = "day",
																			showprogress = FALSE)
	export_aggregated_data(testaggregatedata,
												 save_directory = tempdir(),
												 save_fileprefix = "test_")

	expect_snapshot_file(file.path(tempdir(), "test_col1.csv"), compare = compare_file_text)
	expect_snapshot_file(file.path(tempdir(), "test_col2.csv"), compare = compare_file_text)
	expect_snapshot_file(file.path(tempdir(), "test_[ALLFIELDSCOMBINED].csv"), compare = compare_file_text)
	expect_snapshot_file(file.path(tempdir(), "test_[DUPLICATES].csv"), compare = compare_file_text)

	expect_false(file.exists(file.path(tempdir(), "test_col3.csv")))

	# clean up
	file.remove(file.path(tempdir(), "test_col1.csv"))
	file.remove(file.path(tempdir(), "test_col2.csv"))
	file.remove(file.path(tempdir(), "test_[ALLFIELDSCOMBINED].csv"))
	file.remove(file.path(tempdir(), "test_[DUPLICATES].csv"))
})


test_that("aggregateallfields() removes NAs when rowsumming", {
	testdf <-
		data.table::data.table(
			"col_timepoint" = paste0("2022-01-", 10 + c(seq(1, 3), seq(6, 21))),
			"col_numeric" = c("", "", rep(1, 17)),
			"col_numeric_missing" = ""
		)
	testsourcedata <-
		prepare_data(
			testdf,
			fieldtypes = fieldtypes(
				col_timepoint = ft_timepoint(),
				col_numeric = ft_numeric(),
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

	expect_true(identical(is.na(testdata_byday$aggregatefields$`[ALLFIELDSCOMBINED]`$values$missing_n),
												c(rep(FALSE, 3), rep(TRUE, 2), rep(FALSE, 16))))
})


test_that("aggregatedata object prints to console ok", {
	testsourcedata <-
		prepare_data(
			df = data.frame(
				col1 = rep("2022-01-01", 5),
				col2 = rep(1, 5),
				col3 = 1:5
			),
			fieldtypes = fieldtypes(
				col1 = ft_timepoint(),
				col2 = ft_simple(),
				col3 = ft_ignore()
			),
			showprogress = FALSE
		)
	testdata_byday <-
		aggregate_data(testsourcedata,
									 aggregation_timeunit = "day",
									 showprogress = FALSE)

	expect_snapshot_output(print(testdata_byday))
})


test_that("aggregated values contain NAs instead of Infs or NaNs", {
	testdf <-
		data.table::data.table(
			"col_timepoint" = paste0("2022-01-", 10 + c(seq(1, 3), seq(6, 21))),
			"col_numeric" = c("", "", seq(
				from = 2,
				to = 3,
				length.out = 17
			)),
			"col_datetime" = c("", "", rep("2022-01-01 00:00:00", 17)),
			"col_uniqueidentifier" = c("", "", seq(1, 17))
		)
	testsourcedata <-
		prepare_data(
			testdf,
			fieldtypes = fieldtypes(
				col_timepoint = ft_timepoint(),
				col_numeric = ft_numeric(),
				col_datetime = ft_datetime(includes_time = TRUE),
				col_uniqueidentifier = ft_uniqueidentifier()
			),
			override_columnnames = FALSE,
			na = c("", "NULL"),
			showprogress = FALSE
		)
	testdata_byday <-
		aggregate_data(testsourcedata,
									 aggregation_timeunit = "day",
									 showprogress = FALSE)

	expect_false(any(is.infinite(testdata_byday$aggregatefields$col_numeric$values$min)))
	expect_false(any(is.infinite(testdata_byday$aggregatefields$col_numeric$values$max)))
	expect_false(any(is.nan(testdata_byday$aggregatefields$col_numeric$values$mean)))

	expect_false(any(is.infinite(testdata_byday$aggregatefields$col_datetime$values$min)))
	expect_false(any(is.infinite(testdata_byday$aggregatefields$col_datetime$values$max)))
	expect_false(any(is.nan(testdata_byday$aggregatefields$col_datetime$values$midnight_perc)))

	expect_false(any(is.infinite(testdata_byday$aggregatefields$col_uniqueidentifier$values$minlength)))
	expect_false(any(is.infinite(testdata_byday$aggregatefields$col_uniqueidentifier$values$maxlength)))
	expect_false(any(is.nan(testdata_byday$aggregatefields$col_uniqueidentifier$values$meanlength)))

})

test_that("aggregated values contain all NAs when datafield values are all NA (except for 'n' which should be 0)", {
	testdf <-
		data.table::data.table(
			"col_timepoint" = paste0("2022-01-", 10 + c(seq(1, 3), seq(6, 21))),
			"col_numeric_allna" = "",
			"col_datetime_allna" = "",
			"col_uniqueidentifier_allna" = "",
			"col_categorical_allna" = ""
		)
	testsourcedata <-
		prepare_data(
			testdf,
			fieldtypes = fieldtypes(
				col_timepoint = ft_timepoint(),
				col_numeric_allna = ft_numeric(),
				col_datetime_allna = ft_datetime(),
				col_uniqueidentifier_allna = ft_uniqueidentifier(),
				col_categorical_allna = ft_categorical()
			),
			override_columnnames = FALSE,
			na = c("", "NULL"),
			showprogress = FALSE
		)
	testdata_byday <-
		aggregate_data(testsourcedata,
									 aggregation_timeunit = "day",
									 showprogress = FALSE)

	expect_true(all(testdata_byday$aggregatefields$col_numeric_allna$values$n == 0))

	fieldstotest <- names(testdata_byday$aggregatefields)[-1]
	fieldstotest <- fieldstotest[which(fieldstotest != "[DUPLICATES]")]
	for (dcol in fieldstotest) {
		aggtypestotest <- names(testdata_byday$aggregatefields[[dcol]]$values)[-1]
		aggtypestotest <- aggtypestotest[which(aggtypestotest != "n")]
		for (aggtype in aggtypestotest) {
			expect_true(all(is.na(testdata_byday$aggregatefields[[dcol]]$values[[aggtype]])),
									label = paste0(dcol, "$", aggtype, " is all NA"))
			expect_false(any(is.infinite(testdata_byday$aggregatefields[[dcol]]$values[[aggtype]])),
									 label = paste0(dcol, "$", aggtype, " contains Inf"))
			expect_false(any(is.nan(testdata_byday$aggregatefields[[dcol]]$values[[aggtype]])),
									 label = paste0(dcol, "$", aggtype, " contains NaN"))
		}
	}
})
