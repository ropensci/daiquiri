
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

  p <-
    plot_timeseries_static(
      agg_field = aggregated_data$aggregated_fields$col_numeric_missing,
      agg_fun_colname = "min"
    )
  vdiffr::expect_doppelganger(
    title = "plot_timeseries_static_empty",
    fig = p
  )

  p <-
    plot_overview_totals_static(
      agg_field = aggregated_data$aggregated_fields$col_numeric_missing,
      aggregation_function = "min"
    )
  vdiffr::expect_doppelganger(
    title = "plot_overview_totals_static_empty",
    fig = p
  )
})


test_that("plot_timeseries_static() looks how it should", {
  df <- read_data(test_path("testdata", "completetestset.csv"))
  source_data <- prepare_data(
    df,
    field_types = field_types_advanced(
      col_timepoint = ft_timepoint(),
      col_numeric_clean = ft_numeric(),
      .default_field_type = ft_ignore()
    ),
    dataset_description = "completetestset_subset",
    show_progress = FALSE
  )
  aggregated_data <-
    aggregate_data(source_data,
      aggregation_timeunit = "week",
      show_progress = FALSE
    )

  p <-
    plot_timeseries_static(
      agg_field = aggregated_data$aggregated_fields$col_numeric_clean,
      agg_fun_colname = "n"
    )
  vdiffr::expect_doppelganger(
    title = "plot_timeseries_static",
    fig = p
  )
})


test_that("plot_overview_totals_static() looks how it should", {
  df <- read_data(test_path("testdata", "completetestset.csv"))
  source_data <- prepare_data(
    df,
    field_types = field_types_advanced(
      col_timepoint = ft_timepoint(),
      col_numeric_clean = ft_numeric(),
      .default_field_type = ft_ignore()
    ),
    dataset_description = "completetestset_subset",
    show_progress = FALSE
  )
  aggregated_data <-
    aggregate_data(source_data,
      aggregation_timeunit = "week",
      show_progress = FALSE
    )

  p <-
    plot_overview_totals_static(
      agg_field = aggregated_data$aggregated_fields$col_numeric_clean,
      aggregation_function = "n",
      title = "plot_overview_totals_static",
      stratum = NULL
    )
  vdiffr::expect_doppelganger(
    title = "plot_overview_totals_static",
    fig = p
  )
})


test_that("plot_overview_heatmap_static() looks how it should", {
  df <- read_data(test_path("testdata", "completetestset.csv"))
  source_data <- prepare_data(
    df,
    field_types = field_types_advanced(
      col_timepoint = ft_timepoint(),
      col_numeric_clean = ft_numeric(),
      col_date_only = ft_datetime(includes_time = FALSE),
      col_id_string = ft_uniqueidentifier(),
      col_categorical_small = ft_categorical(aggregate_by_each_category = TRUE),
      col_numeric_missing = ft_numeric(),
      .default_field_type = ft_ignore()
    ),
    dataset_description = "completetestset_subset",
    show_progress = FALSE
  )
  aggregated_data <-
    aggregate_data(source_data,
      aggregation_timeunit = "week",
      show_progress = FALSE
    )

  p <-
    plot_overview_heatmap_static(
      agg_fields = aggregated_data$aggregated_fields,
      aggregation_function = "n",
      stratum = NULL
    )
  vdiffr::expect_doppelganger(
    title = "plot_overview_heatmap_static",
    fig = p
  )
})


test_that("plot_overview_combo_static() looks how it should", {
  df <- read_data(test_path("testdata", "completetestset.csv"))
  source_data <- prepare_data(
    df,
    field_types = field_types_advanced(
      col_timepoint = ft_timepoint(),
      col_numeric_clean = ft_numeric(),
      col_date_only = ft_datetime(includes_time = FALSE),
      col_id_string = ft_uniqueidentifier(),
      col_categorical_small = ft_categorical(aggregate_by_each_category = TRUE),
      col_numeric_missing = ft_numeric(),
      .default_field_type = ft_ignore()
    ),
    dataset_description = "completetestset_subset",
    show_progress = FALSE
  )
  aggregated_data <-
    aggregate_data(source_data,
      aggregation_timeunit = "week",
      show_progress = FALSE
    )

  p <-
    plot_overview_combo_static(
      agg_fields = aggregated_data$aggregated_fields,
      aggregation_function = "n",
      lineplot_field_name = "col_timepoint",
      title = "plot_overview_combo_static",
      stratum = NULL
    )
  vdiffr::expect_doppelganger(
    title = "plot_overview_combo_static",
    fig = p
  )
})


test_that("plot_subcat_heatmap_static() looks how it should", {
  df <- read_data(test_path("testdata", "completetestset.csv"))
  source_data <- prepare_data(
    df,
    field_types = field_types_advanced(
      col_timepoint = ft_timepoint(),
      col_numeric_clean = ft_numeric(),
      col_date_only = ft_datetime(includes_time = FALSE),
      col_id_string = ft_uniqueidentifier(),
      col_categorical_small = ft_categorical(aggregate_by_each_category = TRUE),
      col_numeric_missing = ft_numeric(),
      .default_field_type = ft_ignore()
    ),
    dataset_description = "completetestset_subset",
    show_progress = FALSE
  )
  aggregated_data <-
    aggregate_data(source_data,
      aggregation_timeunit = "week",
      show_progress = FALSE
    )

  p <-
    plot_subcat_heatmap_static(
      agg_field = aggregated_data$aggregated_fields$col_categorical_small,
      aggregation_function = "subcat_n"
    )
  vdiffr::expect_doppelganger(
    title = "plot_subcat_heatmap_static",
    fig = p
  )
})


test_that("plot_stratified_facetgrid_static() looks how it should", {
  df <- read_data(test_path("testdata", "completetestset.csv"))
  source_data <- prepare_data(
    df,
    field_types = field_types_advanced(
      col_timepoint = ft_timepoint(),
      col_numeric_clean = ft_numeric(),
      col_date_only = ft_datetime(includes_time = FALSE),
      col_id_string = ft_uniqueidentifier(),
      col_categorical_small = ft_strata(),
      col_numeric_missing = ft_numeric(),
      .default_field_type = ft_ignore()
    ),
    dataset_description = "completetestset_subset_stratified",
    show_progress = FALSE
  )
  aggregated_data <-
    aggregate_data(source_data,
      aggregation_timeunit = "week",
      show_progress = FALSE
    )

  p <-
    plot_stratified_facetgrid_static(
      agg_field = aggregated_data$aggregated_fields_stratified$col_numeric_clean,
      aggregation_function = "n"
    )
  vdiffr::expect_doppelganger(
    title = "plot_stratified_facetgrid_static",
    fig = p
  )
})


test_that("plot_stratified_totals_static() looks how it should", {
  df <- read_data(test_path("testdata", "completetestset.csv"))
  source_data <- prepare_data(
    df,
    field_types = field_types_advanced(
      col_timepoint = ft_timepoint(),
      col_numeric_clean = ft_numeric(),
      col_date_only = ft_datetime(includes_time = FALSE),
      col_id_string = ft_uniqueidentifier(),
      col_categorical_small = ft_strata(),
      col_numeric_missing = ft_numeric(),
      .default_field_type = ft_ignore()
    ),
    dataset_description = "completetestset_subset_stratified",
    show_progress = FALSE
  )
  aggregated_data <-
    aggregate_data(source_data,
      aggregation_timeunit = "week",
      show_progress = FALSE
    )

  p <-
    plot_stratified_totals_static(
      agg_field = aggregated_data$aggregated_fields$col_numeric_clean,
      aggregation_function = "n",
      title = "plot_stratified_totals_static"
    )
  vdiffr::expect_doppelganger(
    title = "plot_stratified_totals_static",
    fig = p
  )
})


test_that("plot_stratified_combo_static() looks how it should", {
  df <- read_data(test_path("testdata", "completetestset.csv"))
  source_data <- prepare_data(
    df,
    field_types = field_types_advanced(
      col_timepoint = ft_timepoint(),
      col_numeric_clean = ft_numeric(),
      col_date_only = ft_datetime(includes_time = FALSE),
      col_id_string = ft_uniqueidentifier(),
      col_categorical_small = ft_strata(),
      col_numeric_missing = ft_numeric(),
      .default_field_type = ft_ignore()
    ),
    dataset_description = "completetestset_subset_stratified",
    show_progress = FALSE
  )
  aggregated_data <-
    aggregate_data(source_data,
      aggregation_timeunit = "week",
      show_progress = FALSE
    )

  p <-
    plot_stratified_combo_static(
      agg_field = aggregated_data$aggregated_fields$col_numeric_clean,
      agg_field_strat = aggregated_data$aggregated_fields_stratified$col_numeric_clean,
      aggregation_function = "n"
    )
  vdiffr::expect_doppelganger(
    title = "plot_stratified_combo_static",
    fig = p
  )
})

