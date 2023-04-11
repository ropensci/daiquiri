
test_that("report_data() requires a source_data param", {
  expect_error(
    report_data(aggregated_data = structure(list(data_fields = NA),
      class = "daiquiri_aggregated_data"
    )),
    class = "invalid_param_missing"
  )
})

test_that("report_data() requires a aggregated_data param", {
  expect_error(
    report_data(source_data = structure(list(data_fields = NA),
      class = "daiquiri_source_data"
    )),
    class = "invalid_param_missing"
  )
})

test_that("report_data() requires source_data param to be a source_data object", {
  expect_error(
    report_data(
      source_data = data.frame("Fieldname" = 123),
      aggregated_data = structure(list(data_fields = NA),
        class = "daiquiri_aggregated_data"
      )
    ),
    class = "invalid_param_type"
  )
})

test_that("report_data() requires aggregated_data param to be an aggregated_data object", {
  expect_error(
    report_data(
      source_data = structure(list(data_fields = NA),
        class = "daiquiri_source_data"
      ),
      aggregated_data = data.frame("Fieldname" = 123)
    ),
    class = "invalid_param_type"
  )
})

test_that("report_data() creates report and returns path successfully", {
  testdf <- read_data(test_path("testdata", "completetestset.csv"))
  testsource_data <- prepare_data(
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
  testreportpath <-
    report_data(
      testsource_data,
      testaggregated_data,
      report_title = "Complete Test Set",
      save_directory = tempdir(),
      save_filename = "daiquiri_testthatreport",
      show_progress = FALSE
    )

  expect_type(testreportpath, "character")

  # clean up
  expect_true(file.remove(testreportpath))
})

test_that("plots still work when all values are missing", {
  testdf <-
    data.table::data.table(
      "col_timepoint" = paste0("2022-01-", seq(10, 31)),
      "col_numeric_missing" = ""
    )
  testsource_data <-
    prepare_data(
      testdf,
      field_types = field_types(
        col_timepoint = ft_timepoint(),
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

  expect_s3_class(
    plot_timeseries_static(
      agg_field = testdata_byday$aggregatefields$col_numeric_missing,
      agg_fun = "missing_n"
    ),
    "ggplot"
  )
  expect_s3_class(
    plot_overview_totals_static(
      agg_field = testdata_byday$aggregatefields$col_numeric_missing,
      agg_fun = "missing_n"
    ),
    "ggplot"
  )
})
