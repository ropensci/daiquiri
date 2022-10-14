test_that("validate_column_names() checks that column names in data and field_types can match exactly", {
  expect_silent(validate_column_names(c("nonsense", "set", "of"),
    c("nonsense", "set", "of"),
    check_length_only = FALSE
  ))
})

test_that("validate_column_names() checks that column names in data and field_types can match in different order", {
  expect_silent(validate_column_names(c("nonsense", "set", "of"),
    c("nonsense", "of", "set"),
    check_length_only = FALSE
  ))
})

test_that("validate_column_names() checks that column names in data and field_types must match in length if check_length_only = TRUE", {
  expect_silent(validate_column_names(c("nonsense", "set"),
    c("nonsense", "names"),
    check_length_only = TRUE
  ))

  expect_error(validate_column_names(c("nonsense", "set", "of"),
    c("nonsense", "set"),
    check_length_only = TRUE
  ),
  class = "invalid_column_names"
  )
})

test_that("validate_column_names() checks that duplicate column names in data not allowed", {
  expect_error(
    validate_column_names(
      c("nonsense", "set", "of", "nonsense", "names"),
      c("nonsense", "set", "of", "names"),
      check_length_only = FALSE
    ),
    class = "invalid_column_names"
  )
})

test_that("validate_column_names() checks that column names in data but not in field_types is not allowed", {
  expect_error(validate_column_names(
    c("nonsense", "set", "of", "stuff", "names"),
    c("nonsense", "set", "of", "stuff"),
    check_length_only = FALSE
  ),
  class = "invalid_column_names"
  )
})

test_that("validate_column_names() checks that column names in field_types but not in data is not allowed", {
  expect_error(validate_column_names(
    c("nonsense", "set", "of"),
    c("nonsense", "set", "of", "stuff"),
    check_length_only = FALSE
  ),
  class = "invalid_column_names"
  )
})



test_that("prepare_data() requires a df param", {
  expect_error(prepare_data(field_types = field_types(Col_tp = ft_timepoint())),
    class = "invalid_param_missing"
  )
})

test_that("prepare_data() requires a field_types param", {
  expect_error(prepare_data(df = data.frame("Fieldname" = 123)),
    class = "invalid_param_missing"
  )
})

test_that("prepare_data() requires df param to be a data frame", {
  expect_error(prepare_data(c("Fieldname", 123),
    field_types = field_types(Col_tp = ft_timepoint())
  ),
  class = "invalid_param_type"
  )
})

test_that("prepare_data() requires field_types param to be a field_types object", {
  expect_error(prepare_data(
    df = data.frame("Fieldname" = 123),
    field_types = TRUE
  ),
  class = "invalid_param_type"
  )
})

test_that("prepare_data() checks that at least one valid timepoint value is present", {
  expect_error(prepare_data(
    df = data.frame(
      col1 = rep("01/01/2022", 5),
      col2 = rep(1, 5),
      col3 = 1:5
    ),
    field_types = field_types(
      col1 = ft_timepoint(),
      col2 = ft_simple(),
      col3 = ft_ignore()
    ),
    show_progress = FALSE
  ),
  class = "invalid_param_type"
  )
})


test_that("prepare_data() creates source_data object correctly", {
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
      col_simple = ft_simple()
    ),
    dataset_description = "completetestset",
    show_progress = FALSE
  )

  expect_s3_class(testsource_data, "source_data")

  expect_equal(testsource_data$timepoint_field_name, "col_timepoint")
  expect_equal(testsource_data$timepoint_missing_n, 6)
  expect_equal(testsource_data$rows_source_n, 900)
  expect_equal(testsource_data$rows_imported_n, 890)
  expect_equal(testsource_data$rows_duplicates_n, 4)
  expect_equal(testsource_data$cols_source_n, 24)
  expect_equal(testsource_data$cols_imported_n, 12)
  expect_equal(testsource_data$dataset_description, "completetestset")

  expect_snapshot(testsource_data$validation_warnings)
})


test_that("prepare_data() ignores nonchar columns (since readr::type_convert fails to skip nonchar cols)", {
  testsource_data <- prepare_data(
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
    dataset_description = "ignore nonchar columns",
    show_progress = FALSE
  )

  expect_equal(testsource_data$cols_imported_n, 2)
})


test_that("prepare_data() generates a validation warning when nonchar columns are provided", {
  testsource_data <- prepare_data(
    df = data.frame(
      col1 = rep("2022-01-01", 5),
      col2 = 1:5
    ),
    field_types = field_types(
      col1 = ft_timepoint(),
      col2 = ft_numeric()
    ),
    dataset_description = "nonchar columns",
    show_progress = FALSE
  )

  expect_equal(testsource_data$validation_warnings$field_name, "col2")
  expect_match(testsource_data$validation_warnings$message, "instead of character")
})


test_that("prepare_data() overrides column names correctly", {
  testsource_data <- prepare_data(
    df = data.frame(
      col1 = rep("2022-01-01", 5),
      col2 = rep("1", 5)
    ),
    field_types = field_types(
      cola = ft_timepoint(),
      colb = ft_simple()
    ),
    override_column_names = TRUE,
    dataset_description = "override colnames",
    show_progress = FALSE
  )

  expect_equal(names(testsource_data$data_fields)[1:2], c("cola", "colb"))
})


test_that("prepare_data() can accept a data.table with nonchar cols without error", {
  testsource_data <- prepare_data(
    df = data.table::data.table(
      col1 = rep("2022-01-01", 5),
      col2 = rep(1, 5)
    ),
    field_types = field_types(
      col1 = ft_timepoint(),
      col2 = ft_simple()
    ),
    override_column_names = TRUE,
    dataset_description = "pass in nonchar data.table",
    show_progress = FALSE
  )

  expect_equal(testsource_data$cols_imported_n, 2)
})


test_that("prepare_data() gets dataset_description from call if NULL (default) passed in", {
  dfobj <- data.frame(
    col1 = rep("2022-01-01", 5),
    col2 = rep(1, 5),
    col3 = 1:5
  )
  testsource_data <- prepare_data(
    df = dfobj,
    field_types = field_types(
      col1 = ft_timepoint(),
      col2 = ft_simple(),
      col3 = ft_ignore()
    ),
    show_progress = FALSE
  )

  expect_equal(testsource_data$dataset_description, "dfobj")

  testsource_data <- prepare_data(
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
    show_progress = FALSE
  )

  expect_equal(
    testsource_data$dataset_description,
    "data.frame(col1 = rep(\"2022-01-01\", 5), col2 = rep(1, 5), col3 = 1:5)"
  )
})


test_that("source_data object prints to console ok", {
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
      col_simple = ft_simple()
    ),
    dataset_description = "completetestset",
    show_progress = FALSE
  )

  expect_snapshot_output(print(testsource_data))
})

test_that("remove_rows() removes specified rows", {
  numrows <- 2000000
  testdt <- data.table::data.table(
    col1 = rep("2022-01-01", numrows),
    col2 = seq(1, numrows),
    col3 = rep("XXXXXXXXXXX", numrows)
  )
  # object.size(testdt)
  rowstoremove <- c(1, 4, 5, 6, numrows)
  row_indicator <- rep(FALSE, numrows)
  row_indicator[rowstoremove] <- TRUE
  postdt <- remove_rows(data.table::copy(testdt), row_indicator = row_indicator)

  # correct no. of rows removed
  expect_equal(nrow(postdt), numrows - length(rowstoremove))
  # correct rows removed
  expect_false(any(postdt$col2 %in% rowstoremove))
  # all columns still present
  expect_equal(names(testdt), names(postdt))
  # the contents of the first non-deleted row is the same before and after
  expect_equal(unlist(testdt[which(!row_indicator)[1]]), unlist(postdt[1]))
})

test_that("identify_duplicate_rows() identifies all exactly duplicated rows", {
  # unbatched
  numrows <- 200000
  testdt <- data.table::data.table(
    col1 = rep("2022-01-01", numrows),
    col2 = seq(1, numrows),
    col3 = rep("XXXXXXXXXXX", numrows)
  )
  # 	object.size(testdt)
  duplicaterows <- c(2, 5, 6, 7, numrows)
  duplicatevals <- c(1, 4, 4, 4, numrows - 1)
  testdt$col2[duplicaterows] <- duplicatevals

  result <- identify_duplicate_rows(testdt, "col2", show_progress = FALSE)

  expect_true(all(result[duplicaterows]))
  expect_true(all(!result[-duplicaterows]))

  # batched
  numrows <- 20000000
  testdt <- data.table::data.table(
    col1 = rep("2022-01-01", numrows),
    col2 = seq(1, numrows),
    col3 = rep("XXXXXXXXXXX", numrows)
  )
  # 	object.size(testdt)
  duplicaterows <- c(2, 5, 6, 7, numrows)
  duplicatevals <- c(1, 4, 4, 4, numrows - 1)
  testdt$col2[duplicaterows] <- duplicatevals

  result <- identify_duplicate_rows(testdt, "col2", show_progress = FALSE)

  expect_true(all(result[duplicaterows]))
  expect_true(all(!result[-duplicaterows]))
})
