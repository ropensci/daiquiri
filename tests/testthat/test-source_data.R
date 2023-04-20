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

  expect_error(
    validate_column_names(c("nonsense", "set", "of"),
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
  expect_error(
    validate_column_names(
      c("nonsense", "set", "of", "stuff", "names"),
      c("nonsense", "set", "of", "stuff"),
      check_length_only = FALSE
    ),
    class = "invalid_column_names"
  )
})

test_that("validate_column_names() checks that column names in field_types but not in data is not allowed", {
  expect_error(
    validate_column_names(
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
  expect_error(
    prepare_data(c("Fieldname", 123),
      field_types = field_types(Col_tp = ft_timepoint())
    ),
    class = "invalid_param_type"
  )
})

test_that("prepare_data() requires field_types param to be a field_types object", {
  expect_error(
    prepare_data(
      df = data.frame("Fieldname" = 123),
      field_types = TRUE
    ),
    class = "invalid_param_type"
  )
})

test_that("prepare_data() checks that at least one valid timepoint value is present", {
  expect_error(
    prepare_data(
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

  expect_s3_class(source_data, "daiquiri_source_data")

  expect_equal(source_data$timepoint_field_name, "col_timepoint")
  expect_equal(source_data$timepoint_missing_n, 6)
  expect_equal(source_data$rows_source_n, 900)
  expect_equal(source_data$rows_imported_n, 890)
  expect_equal(source_data$rows_duplicates_n, 4)
  expect_equal(source_data$cols_source_n, 26)
  expect_equal(source_data$cols_imported_n, 13)
  expect_equal(source_data$dataset_description, "completetestset")

  expect_snapshot(source_data$validation_warnings)
})


test_that("prepare_data() gets strata info correctly", {
  #TODO: TEST IT WORKS WHEN THERE ARE SPECIAL CHARS IN THE STRATA NAMES
  df <-
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
      "col_stratify" = c("", "", rep("SITE2", 6), rep(c("SITE1", "SITE2"), 6))
    )
  source_data <-
    prepare_data(
      df,
      field_types = field_types(
        col_timepoint = ft_timepoint(),
        col_numeric = ft_numeric(),
        col_datetime = ft_datetime(includes_time = FALSE),
        col_uniqueidentifier = ft_uniqueidentifier(),
        col_categorical = ft_categorical(aggregate_by_each_category = TRUE),
        col_simple = ft_simple(),
        col_stratify = ft_strata()
      ),
      na = c("", "NULL"),
      show_progress = FALSE
    )

  expect_equal(source_data$strata_field_name, "col_stratify")

  # the strata labels should be alphabetical with NA last
  expect_equal(source_data$strata_labels,
               c("SITE1", "SITE2", NA))

})


test_that("prepare_data() ignores nonchar columns (since readr::type_convert fails to skip nonchar cols)", {
  source_data <- prepare_data(
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

  expect_equal(source_data$cols_imported_n, 2)
})


test_that("prepare_data() generates a validation warning when nonchar columns are provided", {
  source_data <- prepare_data(
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

  expect_equal(source_data$validation_warnings$field_name, "col2")
  expect_match(source_data$validation_warnings$message, "instead of character")
})


test_that("prepare_data() matches nonchar validation warnings to column names correctly when fieldtypes are supplied in different order to df columns", {
  source_data <- prepare_data(
    df = data.frame(
      col1 = rep("2022-01-01", 5),
      col2 = 1:5,
      col3 = 1:5,
      col4 = 1:5
    ),
    field_types = field_types(
      col2 = ft_numeric(),
      col1 = ft_timepoint(),
      col3 = ft_ignore(),
      col4 = ft_numeric()
    ),
    dataset_description = "nonchar columns",
    show_progress = FALSE
  )

  expect_equal(source_data$validation_warnings$field_name, c("col2", "col4"))
  expect_match(source_data$validation_warnings$message, "instead of character")
})


test_that("prepare_data() matches type_convert() validation warnings to column names correctly when fieldtypes are supplied in different order to df columns", {
  source_data <- prepare_data(
    df = data.frame(
      col_tp = rep("2022-01-01", 5),
      col_ign = as.character(1:5),
      col_num = c("some text", 1:4),
      col_date = rep("2022-01-01", 5)
    ),
    field_types = field_types(
      col_num = ft_numeric(),
      col_tp = ft_timepoint(),
      col_ign = ft_ignore(),
      col_date = ft_datetime()
    ),
    dataset_description = "nonchar columns",
    show_progress = FALSE
  )

  expect_equal(source_data$validation_warnings$field_name, "col_num")
  expect_match(source_data$validation_warnings$message, "expected a double")
})


test_that("prepare_data() overrides column names correctly", {
  source_data <- prepare_data(
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

  expect_equal(names(source_data$data_fields)[1:2], c("cola", "colb"))
})


test_that("prepare_data() can accept a data.table with nonchar cols without error", {
  source_data <- prepare_data(
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

  expect_equal(source_data$cols_imported_n, 2)
})


test_that("prepare_data() gets dataset_description from call if NULL (default) passed in", {
  dfobj <- data.frame(
    col1 = rep("2022-01-01", 5),
    col2 = rep(1, 5),
    col3 = 1:5
  )
  source_data <- prepare_data(
    df = dfobj,
    field_types = field_types(
      col1 = ft_timepoint(),
      col2 = ft_simple(),
      col3 = ft_ignore()
    ),
    show_progress = FALSE
  )

  expect_equal(source_data$dataset_description, "dfobj")

  source_data <- prepare_data(
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
    source_data$dataset_description,
    "data.frame(col1 = rep(\"2022-01-01\", 5), col2 = rep(1, 5), col3 = 1:5)"
  )
})


test_that("source_data object prints to console ok", {
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

  expect_snapshot_output(print(source_data))
})


test_that("source_data object prints to console ok when there is a strata field", {
  df <-
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
      "col_stratify" = c("", "", rep("SITE2", 6), rep(c("SITE1", "SITE2"), 6))
    )
  source_data <-
    prepare_data(
      df,
      field_types = field_types(
        col_timepoint = ft_timepoint(),
        col_numeric = ft_numeric(),
        col_datetime = ft_datetime(includes_time = FALSE),
        col_uniqueidentifier = ft_uniqueidentifier(),
        col_categorical = ft_categorical(aggregate_by_each_category = TRUE),
        col_simple = ft_simple(),
        col_stratify = ft_strata()
      ),
      na = c("", "NULL"),
      show_progress = FALSE
    )

  expect_snapshot_output(print(source_data))
})


test_that("remove_rows() removes specified rows", {
  numrows <- 2000000
  testdt <- data.table::data.table(
    col1 = rep("2022-01-01", numrows),
    col2 = seq(1, numrows),
    col3 = rep("XXXXXXXXXXX", numrows)
  )
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
  duplicaterows <- c(2, 5, 6, 7, numrows)
  duplicatevals <- c(1, 4, 4, 4, numrows - 1)
  testdt$col2[duplicaterows] <- duplicatevals

  result <- identify_duplicate_rows(testdt, "col2", show_progress = FALSE)

  expect_true(all(result[duplicaterows]))
  expect_true(all(!result[-duplicaterows]))
})

test_that("data_field_count() calculates number of values correctly", {

  testvalues <- c("2021-06-01", "1",
                  "2021-06-02", "",
                  "2021-06-02", "",
                  "2021-06-03", "",
                  "2021-06-03", "1",
                  "2021-06-03", "",
                  "2021-06-05", "",
                  "2021-06-05", "",
                  "2021-06-05", "",
                  "2021-06-05", ""
                  )
  source_data <-
    prepare_data_testhelper(
      testvalues = testvalues,
      field_type = ft_numeric(),
      add_uid_field = TRUE)

  expect_equal(data_field_count(source_data$data_fields[["col_values"]]), 2)
})

test_that("data_field_count() returns 0 when all values are missing", {

  testvalues <- c("2021-06-01", "",
                  "2021-06-02", "",
                  "2021-06-02", "",
                  "2021-06-03", "",
                  "2021-06-03", "",
                  "2021-06-03", "",
                  "2021-06-05", "",
                  "2021-06-05", "",
                  "2021-06-05", "",
                  "2021-06-05", ""
                  )
  source_data <-
    prepare_data_testhelper(
      testvalues = testvalues,
      field_type = ft_numeric(),
      add_uid_field = TRUE)

  expect_equal(data_field_count(source_data$data_fields[["col_values"]]), 0)
})

test_that("column-specific missing value strings get set to NA", {

  df <-
    data.table::data.table(
      "col_timepoint" = c(paste0("2022-01-", 10 + c(seq(1, 9))), "1900-01-01"),
      "col_numeric" = c("", "0", seq(
        from = 2,
        to = 3,
        length.out = 8
      )),
      "col_datetime" = c("", "1800-01-01", "1900-01-01", rep("2022-01-01 00:00:00", 7)),
      "col_uniqueidentifier" = c("", "999", "0", seq(1, 7)),
      "col_categorical" = c("", "Unknown", seq(1, 8)),
      "col_freetext" = c("", ".", "Unknown", "Not known", seq(1, 6)),
      "col_simple" = c("", "Unk", seq(1, 8)),
      "col_none" = c("", "Unk", seq(1, 8))
    )

  source_data <-
    prepare_data(
      df = df,
      field_types = field_types(
        col_timepoint = ft_timepoint(na = "1900-01-01", includes_time = FALSE),
        col_numeric = ft_numeric(na = "0"),
        col_datetime = ft_datetime(na = "1800-01-01"),
        col_uniqueidentifier = ft_uniqueidentifier(na = "999"),
        col_categorical = ft_categorical(na = "Unknown"),
        col_freetext = ft_freetext(na = c(".", "Unknown", "Not known")),
        col_simple = ft_simple(na = "Unk"),
        col_none = ft_simple()
      ),
      na = "",
      show_progress = FALSE)

  expect_equal(nrow(source_data$data_fields[["col_timepoint"]]$values), 9)
  expect_equal(source_data$timepoint_missing_n, 1)

  expect_true(is.na(source_data$data_fields[["col_numeric"]]$values[1][[1]]))
  expect_true(is.na(source_data$data_fields[["col_numeric"]]$values[2][[1]]))
  expect_false(is.na(source_data$data_fields[["col_numeric"]]$values[3][[1]]))

  expect_true(is.na(source_data$data_fields[["col_datetime"]]$values[1][[1]]))
  expect_true(is.na(source_data$data_fields[["col_datetime"]]$values[2][[1]]))
  expect_false(is.na(source_data$data_fields[["col_datetime"]]$values[3][[1]]))
  expect_false(is.na(source_data$data_fields[["col_datetime"]]$values[4][[1]]))

  expect_true(is.na(source_data$data_fields[["col_uniqueidentifier"]]$values[1][[1]]))
  expect_true(is.na(source_data$data_fields[["col_uniqueidentifier"]]$values[2][[1]]))
  expect_false(is.na(source_data$data_fields[["col_uniqueidentifier"]]$values[3][[1]]))
  expect_false(is.na(source_data$data_fields[["col_uniqueidentifier"]]$values[4][[1]]))

  expect_true(is.na(source_data$data_fields[["col_categorical"]]$values[1][[1]]))
  expect_true(is.na(source_data$data_fields[["col_categorical"]]$values[2][[1]]))
  expect_false(is.na(source_data$data_fields[["col_categorical"]]$values[3][[1]]))

  expect_true(is.na(source_data$data_fields[["col_freetext"]]$values[1][[1]]))
  expect_true(is.na(source_data$data_fields[["col_freetext"]]$values[2][[1]]))
  expect_true(is.na(source_data$data_fields[["col_freetext"]]$values[3][[1]]))
  expect_true(is.na(source_data$data_fields[["col_freetext"]]$values[4][[1]]))
  expect_false(is.na(source_data$data_fields[["col_freetext"]]$values[5][[1]]))

  expect_true(is.na(source_data$data_fields[["col_simple"]]$values[1][[1]]))
  expect_true(is.na(source_data$data_fields[["col_simple"]]$values[2][[1]]))
  expect_false(is.na(source_data$data_fields[["col_simple"]]$values[3][[1]]))

  expect_true(is.na(source_data$data_fields[["col_none"]]$values[1][[1]]))
  expect_false(is.na(source_data$data_fields[["col_none"]]$values[2][[1]]))
})
