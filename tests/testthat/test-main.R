test_that("read_data() reads csv file created using Excel correctly when col_names = TRUE", {
  raw_data <- read_data(
    test_path("testdata", "specialchars_excel.csv"),
    delim = ",",
    col_names = TRUE
  )
  expect_equal(nrow(raw_data), 8)
  expect_equal(length(raw_data), 4)
})

test_that("read_data() reads csv file created using Excel correctly when col_names = FALSE", {
  raw_data <- read_data(
    test_path("testdata", "specialchars_excel.csv"),
    delim = ",",
    col_names = FALSE
  )
  expect_equal(nrow(raw_data), 9)
  expect_equal(length(raw_data), 4)
})

test_that("read_data() raises a warning when there are parsing issues", {
  # read file without acknowledging quotes
  raw_data <- read_data(
    test_path("testdata", "specialchars_excel.csv"),
    delim = ",",
    quote = "",
    col_names = TRUE
  )
  expect_warning(
    expect_warning(as.numeric(raw_data$numeric_good), "NAs introduced by coercion"),
    "One or more parsing issues"
  )
})

test_that("daiquiri_report() requires a df param", {
  expect_error(daiquiri_report(field_types = field_types(Col_tp = ft_timepoint())),
    class = "invalid_param_missing"
  )
})

test_that("daiquiri_report() requires a field_types param", {
  expect_error(daiquiri_report(df = data.frame("Fieldname" = 123)),
    class = "invalid_param_missing"
  )
})

test_that("daiquiri_report() requires df param to be a data frame", {
  expect_error(
    daiquiri_report(
      df = c("Fieldname", 123),
      field_types = field_types(Col_tp = ft_timepoint())
    ),
    class = "invalid_param_type"
  )
})

test_that("daiquiri_report() requires field_types param to be a field_types object", {
  expect_error(
    daiquiri_report(
      df = data.frame("Fieldname" = 123),
      field_types = TRUE
    ),
    class = "invalid_param_type"
  )
})


test_that("daiquiri_report() creates report and returns daiquiri object successfully", {
  testdf <- read_data(test_path("testdata", "completetestset.csv"))
  testdaiq_obj <- daiquiri_report(
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
      col_simple = ft_simple()
    ),
    dataset_description = "completetestset",
    aggregation_timeunit = "week",
    report_title = "Complete Test Set",
    save_directory = tempdir(),
    save_filename = "daiquiri_testthatreport",
    log_directory = tempdir(),
    show_progress = FALSE
  )

  expect_s3_class(testdaiq_obj, "daiquiri_object")
  expect_s3_class(testdaiq_obj$source_data, "daiquiri_source_data")
  expect_s3_class(testdaiq_obj$aggregated_data, "daiquiri_aggregated_data")

  expect_equal(testdaiq_obj$dataset_description, "completetestset")
  expect_equal(testdaiq_obj$report_filename, file.path(
    tempdir(),
    "daiquiri_testthatreport.html"
  ))

  # clean up
  expect_true(file.remove(testdaiq_obj$report_filename))
  expect_true(file.remove(testdaiq_obj$log_filename))
})


test_that("daiquiri_report() works even when column_names contain special chars", {
  testdaiq_obj <-
    daiquiri_report(
      df = read_data(test_path("testdata", "specialchars_colnames.csv")),
      field_types = field_types(
        "col_underscore" = ft_ignore(),
        "col space" = ft_timepoint(),
        "col-dash" = ft_simple(),
        "col.dot" = ft_simple(),
        "col!exclamation" = ft_simple(),
        "col%percent" = ft_simple(),
        "col&ampersand" = ft_simple(),
        "col\"doublequote" = ft_simple(),
        "col'singlequote" = ft_simple(),
        "col[]squarebrackets" = ft_simple(),
        "col()brackets" = ft_simple()
      ),
      dataset_description = "specialchars_colnames",
      aggregation_timeunit = "day",
      save_directory = tempdir(),
      save_filename = "daiquiri_testthatreport",
      show_progress = FALSE
    )
  # clean up
  expect_true(file.remove(testdaiq_obj$report_filename))
})


test_that("daiquiri_report() gets dataset_description from call if NULL (default) passed in", {
  dfobj <-
    data.frame(
      col1 = rep("2022-01-01", 5),
      col2 = rep(1, 5),
      col3 = 1:5
    )
  testdaiq_obj <- daiquiri_report(
    df = dfobj,
    field_types = field_types(
      col1 = ft_timepoint(),
      col2 = ft_simple(),
      col3 = ft_ignore()
    ),
    aggregation_timeunit = "day",
    save_directory = tempdir(),
    save_filename = "daiquiri_testthatreport",
    show_progress = FALSE
  )
  expect_equal(testdaiq_obj$source_data$dataset_description, "dfobj")
  # clean up
  expect_true(file.remove(testdaiq_obj$report_filename))

  testdaiq_obj <-
    daiquiri_report(
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
      aggregation_timeunit = "day",
      save_directory = tempdir(),
      save_filename = "daiquiri_testthatreport",
      show_progress = FALSE
    )

  expect_equal(
    testdaiq_obj$source_data$dataset_description,
    "data.frame(col1 = rep(\"2022-01-01\", 5), col2 = rep(1, 5), col3 = 1:5)"
  )
  # clean up
  expect_true(file.remove(testdaiq_obj$report_filename))
})


test_that("daiquiri_object prints to console ok", {
  testdaiq_obj <-
    daiquiri_report(
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
      aggregation_timeunit = "day",
      save_directory = tempdir(),
      save_filename = "daiquiri_testthatreport",
      show_progress = FALSE
    )

  expect_snapshot_output(print(testdaiq_obj))
  # clean up
  expect_true(file.remove(testdaiq_obj$report_filename))
})
