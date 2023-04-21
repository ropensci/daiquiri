
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
  df <- read_data(test_path("testdata", "completetestset.csv"))
  source_data <- prepare_data(
    df,
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
  aggregated_data <-
    aggregate_data(source_data,
      aggregation_timeunit = "week",
      show_progress = FALSE
    )
  reportpath <-
    report_data(
      source_data,
      aggregated_data,
      report_title = "Complete Test Set",
      save_directory = tempdir(),
      save_filename = "daiquiri_testthatreport",
      show_progress = FALSE
    )

  expect_type(reportpath, "character")

  # clean up
  expect_true(file.remove(reportpath))
})

test_that("plots still work when all values are missing", {
  df <-
    data.table::data.table(
      "col_timepoint" = paste0("2022-01-", seq(10, 31)),
      "col_numeric_missing" = ""
    )
  source_data <-
    prepare_data(
      df,
      field_types = field_types(
        col_timepoint = ft_timepoint(),
        col_numeric_missing = ft_numeric()
      ),
      dataset_description = "blankplottest",
      override_column_names = FALSE,
      na = c("", "NULL"),
      show_progress = FALSE
    )
  aggregated_data <-
    aggregate_data(source_data,
      aggregation_timeunit = "day",
      show_progress = FALSE
    )

  expect_s3_class(
    plot_timeseries_static(
      agg_field = aggregated_data$aggregatefields$col_numeric_missing,
      agg_fun = "missing_n"
    ),
    "ggplot"
  )
  expect_s3_class(
    plot_overview_totals_static(
      agg_field = aggregated_data$aggregatefields$col_numeric_missing,
      agg_fun = "missing_n"
    ),
    "ggplot"
  )
})

test_that("report_data() creates stratified report without error", {
  df <-
    data.table::data.table(
      "col_timepoint" = c(rep("2022-01-01", 5),
                          rep("2022-01-02", 5),
                          rep("2022-01-04", 5),
                          rep("2022-01-05", 5)),
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
  source_data <-
    prepare_data(
      df,
      field_types = field_types(
        col_timepoint = ft_timepoint(),
        col_numeric = ft_numeric(),
        col_datetime = ft_datetime(includes_time = FALSE),
        col_uniqueidentifier = ft_uniqueidentifier(),
        col_categorical = ft_categorical(),
        col_simple = ft_simple(),
        col_stratify = ft_strata()
      ),
      na = c("", "NULL"),
      show_progress = FALSE
    )
  aggregated_data <-
    aggregate_data(
      source_data = source_data,
      aggregation_timeunit = "day",
      show_progress = FALSE
    )

  reportpath <-
    report_data(
      source_data,
      aggregated_data,
      report_title = "Stratified Test Set",
      save_directory = tempdir(),
      save_filename = "daiquiri_testthatreport",
      show_progress = FALSE
    )

  # clean up
  expect_true(file.remove(reportpath))
})

test_that("report_data() creates stratified report without error when strata contain special chars", {
  df <-
    data.table::data.table(
      "col_timepoint" = c(rep("2022-01-01", 5),
                          rep("2022-01-02", 5),
                          rep("2022-01-04", 11),
                          rep("2022-01-05", 5)),
      "col_numeric" = seq(
        from = 2,
        to = 3,
        length.out = 26
      ),
      "col_datetime" = c(paste0("2022-01-", 10 + c(seq(1, 9))), rep("", 17)),
      "col_uniqueidentifier" = c(seq(1, 26)),
      "col_categorical" = c(rep(c("a", "b"), 11), rep("a", 4)),
      "col_simple" = c(rep("", 10), rep("a", 16)),
      "col_missing" = rep("", 26),
      "col_stratify -.!%&\"'[]()" = c("", "",
                                      rep("SITE 1", 4),
                                      rep(paste0("SITE",
                                                 seq(2, 11),
                                                 c(" ", "-", ".", "!", "%", "&", "\"", "'", "[]", "()")), 2))
      )

  source_data <-
    prepare_data(
      df,
      field_types = field_types(
        col_timepoint = ft_timepoint(),
        col_numeric = ft_numeric(),
        col_datetime = ft_datetime(includes_time = FALSE),
        col_uniqueidentifier = ft_uniqueidentifier(),
        col_categorical = ft_categorical(),
        col_simple = ft_simple(),
        col_missing = ft_simple(),
        `col_stratify -.!%&\"'[]()` = ft_strata()
      ),
      na = c("", "NULL"),
      show_progress = FALSE
    )
  aggregated_data <-
    aggregate_data(
      source_data = source_data,
      aggregation_timeunit = "day",
      show_progress = FALSE
    )

  reportpath <-
    report_data(
      source_data,
      aggregated_data,
      report_title = "Stratified Test Set",
      save_directory = tempdir(),
      save_filename = "daiquiri_testthatreport",
      show_progress = FALSE
    )

  # clean up
  expect_true(file.remove(reportpath))
})

test_that("report_data() creates report without error when subcats contain special chars", {
  df <-
    data.table::data.table(
      "col_timepoint" = c(rep("2022-01-01", 5),
                          rep("2022-01-02", 5),
                          rep("2022-01-04", 11),
                          rep("2022-01-05", 5)),
      "col_categorical" = c("", "",
                            rep("SITE 1", 4),
                            rep(paste0("SITE",
                                       seq(2, 11),
                                       c(" ", "-", ".", "!", "%", "&", "\"", "'", "[]", "()")), 2))
    )

  source_data <-
    prepare_data(
      df,
      field_types = field_types(
        col_timepoint = ft_timepoint(),
        col_categorical = ft_categorical(aggregate_by_each_category = TRUE)
      ),
      na = c("", "NULL"),
      show_progress = FALSE
    )
  aggregated_data <-
    aggregate_data(
      source_data = source_data,
      aggregation_timeunit = "day",
      show_progress = FALSE
    )

  reportpath <-
    report_data(
      source_data,
      aggregated_data,
      report_title = "Stratified Test Set",
      save_directory = tempdir(),
      save_filename = "daiquiri_testthatreport",
      show_progress = FALSE
    )

  # clean up
  expect_true(file.remove(reportpath))
})

