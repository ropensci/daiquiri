
test_that("aggregate_data() requires a source_data param", {
  expect_error(aggregate_data(),
    class = "invalid_param_missing"
  )
})

test_that("aggregate_data() requires source_data param to be a source_data object", {
  expect_error(aggregate_data(source_data = data.frame("Fieldname" = 123)),
    class = "invalid_param_type"
  )
})

test_that("aggregate_data() requires aggregation_timeunit param to be one of day/week/month/quarter/year", {
  expect_error(
    aggregate_data(
      source_data = structure(list(data_fields = NA), class = "daiquiri_source_data"),
      aggregation_timeunit = "hello"
    ),
    class = "invalid_param_type"
  )
})

test_that("aggregate_data() creates aggregate object correctly", {
  testdf <- read_data(test_path("testdata", "completetestset.csv"))
  testsource_data <-
    prepare_data(
      testdf,
      field_types = field_types(
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
        col_simple = ft_simple(),
        col_numeric_missing_err = ft_ignore(),
        col_numeric_missing = ft_numeric()
      ),
      dataset_description = "completetestset",
      show_progress = FALSE
    )
  testaggregated_data <-
    aggregate_data(testsource_data,
      aggregation_timeunit = "week",
      show_progress = FALSE
    )

  expect_s3_class(testaggregated_data, "daiquiri_aggregated_data")

  # check the basic structure is ok
  expect_equal(testaggregated_data$timepoint_field_name, "col_timepoint")
  expect_equal(testaggregated_data$aggregation_timeunit, "week")
  expect_setequal(
    names(testaggregated_data$aggregated_fields),
    c(
      names(testsource_data$cols_imported_indexes),
      "[DUPLICATES]",
      "[ALL_FIELDS_COMBINED]"
    )
  )
  expect_equal(nrow(testaggregated_data$aggregated_fields$col_timepoint$values), 43)
  expect_equal(ncol(testaggregated_data$aggregated_fields$col_timepoint$values), 4)
  expect_equal(nrow(testaggregated_data$aggregated_fields$col_date_time$values), 43)
  expect_equal(ncol(testaggregated_data$aggregated_fields$col_date_time$values), 10)
  expect_equal(nrow(testaggregated_data$aggregated_fields$col_date_only$values), 43)
  expect_equal(ncol(testaggregated_data$aggregated_fields$col_date_only$values), 8)
  expect_equal(nrow(testaggregated_data$aggregated_fields$col_id_num$values), 43)
  expect_equal(ncol(testaggregated_data$aggregated_fields$col_id_num$values), 7)
  expect_equal(nrow(testaggregated_data$aggregated_fields$col_timepoint$values), 43)
  expect_equal(ncol(testaggregated_data$aggregated_fields$col_numeric_clean$values), 10)
  expect_equal(nrow(testaggregated_data$aggregated_fields$col_categorical_small$values), 43)
  expect_equal(ncol(testaggregated_data$aggregated_fields$col_categorical_small$values), 19)
  expect_equal(nrow(testaggregated_data$aggregated_fields$col_categorical_large$values), 43)
  expect_equal(ncol(testaggregated_data$aggregated_fields$col_categorical_large$values), 5)
  expect_equal(nrow(testaggregated_data$aggregated_fields$col_freetext$values), 43)
  expect_equal(ncol(testaggregated_data$aggregated_fields$col_freetext$values), 4)
  expect_equal(nrow(testaggregated_data$aggregated_fields$col_simple$values), 43)
  expect_equal(ncol(testaggregated_data$aggregated_fields$col_simple$values), 4)
  expect_equal(nrow(testaggregated_data$aggregated_fields$col_numeric_missing$values), 43)
  expect_equal(nrow(testaggregated_data$aggregated_fields[["[DUPLICATES]"]]$values), 43)
  expect_equal(ncol(testaggregated_data$aggregated_fields[["[DUPLICATES]"]]$values), 3)
  expect_equal(nrow(testaggregated_data$aggregated_fields[["[ALL_FIELDS_COMBINED]"]]$values), 43)
  expect_equal(ncol(testaggregated_data$aggregated_fields[["[ALL_FIELDS_COMBINED]"]]$values), 6)
})


test_that("export_aggregated_data() requires a aggregated_data param", {
  expect_error(export_aggregated_data(save_directory = tempdir()),
    class = "invalid_param_missing"
  )
})

test_that("export_aggregated_data() requires a save_directory param", {
  expect_error(
    export_aggregated_data(
      aggregated_data = structure(list(data_fields = NA), class = "daiquiri_aggregated_data")
    ),
    class = "invalid_param_missing"
  )
})

test_that("export_aggregated_data() requires aggregated_data param to be an aggregated_data object", {
  expect_error(
    export_aggregated_data(
      aggregated_data = data.frame("Fieldname" = 123),
      save_directory = tempdir()
    ),
    class = "invalid_param_type"
  )
})

test_that("export_aggregated_data() generates csv files", {
  testsource_data <-
    prepare_data(
      data.frame(
        col1 = rep("2022-01-01", 5),
        col2 = rep(1, 5),
        col3 = 1:5
      ),
      field_types = field_types(
        col1 = ft_timepoint(),
        col2 = ft_simple(),
        col3 = ft_ignore()
      ),
      dataset_description = "exporttestset",
      show_progress = FALSE
    )
  testaggregated_data <- aggregate_data(testsource_data,
    aggregation_timeunit = "day",
    show_progress = FALSE
  )
  export_aggregated_data(testaggregated_data,
    save_directory = tempdir(),
    save_file_prefix = "test_"
  )

  expect_snapshot_file(file.path(tempdir(), "test_col1.csv"), compare = compare_file_text)
  expect_snapshot_file(file.path(tempdir(), "test_col2.csv"), compare = compare_file_text)
  expect_snapshot_file(file.path(tempdir(), "test_[ALL_FIELDS_COMBINED].csv"), compare = compare_file_text)
  expect_snapshot_file(file.path(tempdir(), "test_[DUPLICATES].csv"), compare = compare_file_text)

  expect_false(file.exists(file.path(tempdir(), "test_col3.csv")))

  # clean up
  file.remove(file.path(tempdir(), "test_col1.csv"))
  file.remove(file.path(tempdir(), "test_col2.csv"))
  file.remove(file.path(tempdir(), "test_[ALL_FIELDS_COMBINED].csv"))
  file.remove(file.path(tempdir(), "test_[DUPLICATES].csv"))
})


test_that("aggregate_combined_fields() removes NAs when rowsumming", {
  testdf <-
    data.table::data.table(
      "col_timepoint" = paste0("2022-01-", 10 + c(seq(1, 3), seq(6, 21))),
      "col_numeric" = c("", "", rep(1, 17)),
      "col_numeric_missing" = ""
    )
  testsource_data <-
    prepare_data(
      testdf,
      field_types = field_types(
        col_timepoint = ft_timepoint(),
        col_numeric = ft_numeric(),
        col_numeric_missing = ft_numeric()
      ),
      dataset_description = "blankplottest",
      override_column_names = FALSE,
      na = c("", "NULL"),
      show_progress = FALSE
    )
  testdata_byday <-
    aggregate_data(testsource_data,
      aggregation_timeunit = "day",
      show_progress = FALSE
    )

  expect_true(identical(
    is.na(testdata_byday$aggregated_fields$`[ALL_FIELDS_COMBINED]`$values$missing_n),
    c(rep(FALSE, 3), rep(TRUE, 2), rep(FALSE, 16))
  ))
})


test_that("aggregated_data object prints to console ok", {
  testsource_data <-
    prepare_data(
      df = data.frame(
        col1 = rep("2022-01-01", 5),
        col2 = rep(1, 5),
        col3 = 1:5
      ),
      field_types = field_types(
        col1 = ft_timepoint(),
        col2 = ft_simple(),
        col3 = ft_ignore()
      ),
      dataset_description = "completetestset",
      show_progress = FALSE
    )
  testdata_byday <-
    aggregate_data(testsource_data,
      aggregation_timeunit = "day",
      show_progress = FALSE
    )

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
  testsource_data <-
    prepare_data(
      testdf,
      field_types = field_types(
        col_timepoint = ft_timepoint(),
        col_numeric = ft_numeric(),
        col_datetime = ft_datetime(includes_time = TRUE),
        col_uniqueidentifier = ft_uniqueidentifier()
      ),
      override_column_names = FALSE,
      na = c("", "NULL"),
      show_progress = FALSE
    )
  testdata_byday <-
    aggregate_data(testsource_data,
      aggregation_timeunit = "day",
      show_progress = FALSE
    )

  expect_false(any(is.infinite(testdata_byday$aggregated_fields$col_numeric$values$min)))
  expect_false(any(is.infinite(testdata_byday$aggregated_fields$col_numeric$values$max)))
  expect_false(any(is.nan(testdata_byday$aggregated_fields$col_numeric$values$mean)))

  expect_false(any(is.infinite(testdata_byday$aggregated_fields$col_datetime$values$min)))
  expect_false(any(is.infinite(testdata_byday$aggregated_fields$col_datetime$values$max)))
  expect_false(any(is.nan(testdata_byday$aggregated_fields$col_datetime$values$midnight_perc)))

  expect_false(any(is.infinite(testdata_byday$aggregated_fields$col_uniqueidentifier$values$min_length)))
  expect_false(any(is.infinite(testdata_byday$aggregated_fields$col_uniqueidentifier$values$max_length)))
  expect_false(any(is.nan(testdata_byday$aggregated_fields$col_uniqueidentifier$values$mean_length)))
})

test_that("agg_fun_subcat_value() retrieves value correctly", {
  expect_equal(agg_fun_subcat_value(agg_fun = "subcat_n_16_thisvalue"),
               "thisvalue"
  )
})

test_that("aggregate_data() stratifies aggregations correctly", {
  #TODO: TEST IT WORKS WHEN THERE ARE SPECIAL CHARS IN THE STRATA NAMES
  testdf <-
    data.table::data.table(
      "col_timepoint" = c(rep("2022-01-01", 5), rep("2022-01-02", 5), rep("2022-01-04", 5), rep("2022-01-05", 5)),
      "col_numeric" = seq(
        from = 2,
        to = 3,
        length.out = 20
      ),
      "col_datetime" = c(paste0("2022-01-", 10 + c(seq(1, 9))), rep("", 11)),
      "col_uniqueidentifier" = c(seq(1, 20)),
      "col_categorical" = c(rep(c("a", "b"), 8), rep("a", 4)),
      "col_simple" = c(rep("", 10), rep("a", 10)),
      "col_stratify" = c("", "", rep("SITE1", 6), rep(c("SITE1", "SITE2"), 6))
    )
  testsource_data <-
    prepare_data(
      testdf,
      field_types = field_types(
        col_timepoint = ft_timepoint(),
        col_numeric = ft_numeric(),
        col_datetime = ft_datetime(includes_time = FALSE),
        col_uniqueidentifier = ft_uniqueidentifier(),
        col_categorical = ft_categorical(aggregate_by_each_category = TRUE),
        col_simple = ft_simple(),
        col_stratify = ft_categorical()
      ),
      na = c("", "NULL"),
      show_progress = FALSE
    )
  testdata_byday <-
    aggregate_data(
      source_data = testsource_data,
      aggregation_timeunit = "day",
      stratify_by = "col_stratify",
      show_progress = FALSE
    )

  # check the basic structure is ok
  expect_setequal(
    names(testdata_byday$aggregated_fields_stratified),
    names(testsource_data$cols_imported_indexes)[which(names(testsource_data$cols_imported_indexes) != "col_stratify")]
  )

  # the stratification categories should include NA
  expect_equal(unique(testdata_byday$aggregated_fields_stratified$col_numeric$values$col_stratify),
               c(NA, "SITE1", "SITE2")
  )

  # check the stratified values correspond to the overall value
  expect_equal(
    testdata_byday$aggregated_fields$col_numeric$values$n,
    testdata_byday$aggregated_fields_stratified$col_numeric$values[, list(n = sum(n)), by = list(col_timepoint_byday)][[2]]
    )
  # TODO: more tests needed for other agg_funs
  # (when all subvalues are NA the total should be NA, otherwise ignore the NAs)
  # expect_equal(
  #   testdata_byday$aggregated_fields$col_numeric$values$missing_n,
  #   testdata_byday$aggregated_fields_stratified$col_numeric$values[, list(missing_n = sum(missing_n, na.rm = TRUE)), by = list(col_timepoint_byday)][[2]]
  #   )


})

