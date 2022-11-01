test_that("Valid field_types can be specified", {
  expect_s3_class(
    field_types(
      Col_tp = ft_timepoint(),
      Col_uid = ft_uniqueidentifier(),
      Col_cat = ft_categorical(),
      Col_cat2 = ft_categorical(aggregate_by_each_category = TRUE),
      Col_num = ft_numeric(),
      Col_dt = ft_datetime(),
      Col_dt2 = ft_datetime(includes_time = FALSE),
      Col_ft = ft_freetext(),
      Col_sim = ft_simple(),
      Col_ign = ft_ignore()
    ),
    "daiquiri_field_types"
  )
})

test_that("Invalid field_types cannot be specified", {
  expect_error(field_types(Col_bad = readr::col_character()), class = "invalid_field_types")
  expect_error(field_types(Col_bad = "hello"), class = "invalid_field_types")
})

test_that("Duplicate column names in field_types specification not allowed", {
  expect_error(
    field_types(
      Col_dup = ft_timepoint(),
      Col_dup = ft_uniqueidentifier()
    ),
    class = "invalid_field_types"
  )

  expect_error(
    field_types(
      Col_tp = ft_timepoint(),
      Col_dup = ft_uniqueidentifier(),
      Col_dup = ft_uniqueidentifier()
    ),
    class = "invalid_field_types"
  )
})

test_that("field_types object must include a timepoint field", {
  expect_error(field_types(Col_dt = ft_datetime()),
    class = "invalid_field_types"
  )
})

test_that("field_types object must not contain more than one timepoint field", {
  expect_error(
    field_types(
      Col_tp1 = ft_timepoint(),
      Col_tp2 = ft_timepoint()
    ),
    class = "invalid_field_types"
  )
})

test_that("[DUPLICATES] cannot be used as a field_type colname as it is a reserved word", {
  expect_error(field_types("[DUPLICATES]" = ft_timepoint()),
    class = "invalid_field_types"
  )

  expect_error(
    field_types(
      Col_tp = ft_timepoint(),
      "[DUPLICATES]" = ft_simple()
    ),
    class = "invalid_field_types"
  )
})

test_that("[ALL_FIELDS_COMBINED] cannot be used as a field_type colname as it is a reserved word", {
  expect_error(field_types("[ALL_FIELDS_COMBINED]" = ft_timepoint()),
    class = "invalid_field_types"
  )
})


test_that("print_field_types_template() requires a df param", {
  expect_error(print_field_types_template(),
    class = "invalid_param_missing"
  )
})

test_that("print_field_types_template() requires df param to be a data frame", {
  expect_error(print_field_types_template(df = c("Fieldname", 123)),
    class = "invalid_param_type"
  )
})

test_that("print_field_types_template() requires default_field_type param to be a field_type", {
  expect_error(
    print_field_types_template(
      df = data.frame("Fieldname" = 123),
      default_field_type = TRUE
    ),
    class = "invalid_param_type"
  )
})

test_that("print_field_types_template() generates template field_types output", {
  expect_snapshot_output(print_field_types_template(df = data.frame(
    "col1" = 123,
    "col2" = 123,
    "col3" = "hello"
  )))
})

test_that("field_types object prints to console ok", {
  testfield_types <- field_types(
    Col_tp = ft_timepoint(),
    Col_uid = ft_uniqueidentifier(),
    Col_cat = ft_categorical(),
    Col_cat2 = ft_categorical(aggregate_by_each_category = TRUE),
    Col_num = ft_numeric(),
    Col_dt = ft_datetime(),
    Col_dt2 = ft_datetime(includes_time = FALSE),
    Col_ft = ft_freetext(),
    Col_sim = ft_simple(),
    Col_ign = ft_ignore()
  )

  expect_snapshot_output(print(testfield_types))
})
