test_that("aggregate_and_append_values 'n' works as expected", {
  # counts number of values present (including non-conformant ones)
  # For timepoints with no records, value should show 0

  testvalues <- c("2021-06-01", "1",
                  "2021-06-02", "1",
                  "2021-06-02", "2",
                  "2021-06-03", "a",
                  "2021-06-03", "2",
                  "2021-06-03", "3",
                  "2021-06-05", "",
                  "2021-06-05", "2",
                  "2021-06-05", "3",
                  "2021-06-05", "4"
                  )

  grouped_values <-
    aggregate_and_append_values_testhelper(testvalues,
                                           ft_numeric(),
                                           "n",
                                           aggregation_timeunit = "day")

  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-01"][[2]], 1)
  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-02"][[2]], 2)
  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-03"][[2]], 3)
  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-04"][[2]], 0)
  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-05"][[2]], 3)
})

test_that("aggregate_and_append_values 'n' works as expected when all values are missing", {
  # counts number of values present (including non-conformant ones)
  # For timepoints with no records, value should show 0

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
  grouped_values <-
    aggregate_and_append_values_testhelper(testvalues = testvalues,
                                           field_type = ft_numeric(),
                                           aggregation_function = "n",
                                           aggregation_timeunit = "day",
                                           add_uid_field = TRUE)

  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-01"][[2]], 0)
  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-02"][[2]], 0)
  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-03"][[2]], 0)
  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-04"][[2]], 0)
  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-05"][[2]], 0)
})

test_that("aggregate_and_append_values 'missing_n' works as expected", {
  # counts number of values missing (excludes non-conformant ones)
  # For timepoints with no records, value should show NA

  testvalues <- c("2021-06-01", "",
                  "2021-06-02", "1",
                  "2021-06-02", "2",
                  "2021-06-03", "a",
                  "2021-06-03", "2",
                  "2021-06-03", "3",
                  "2021-06-05", "",
                  "2021-06-05", "",
                  "2021-06-05", "3",
                  "2021-06-05", "4"
                  )
  grouped_values <-
    aggregate_and_append_values_testhelper(testvalues,
                                           ft_numeric(),
                                           "missing_n",
                                           aggregation_timeunit = "day")

  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-01"][[2]], 1)
  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-02"][[2]], 0)
  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-03"][[2]], 0)
  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-04"][[2]], NA_real_)
  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-05"][[2]], 2)
})

test_that("aggregate_and_append_values 'missing_n' works as expected when all values are missing", {
  # counts number of values missing (excludes non-conformant ones)
  # For timepoints with no records, value should show NA

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
  grouped_values <-
    aggregate_and_append_values_testhelper(testvalues = testvalues,
                                           field_type = ft_numeric(),
                                           aggregation_function = "missing_n",
                                           aggregation_timeunit = "day",
                                           add_uid_field = TRUE)

  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-01"][[2]], 1)
  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-02"][[2]], 2)
  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-03"][[2]], 3)
  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-04"][[2]], NA_real_)
  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-05"][[2]], 4)
})

test_that("aggregate_and_append_values 'missing_perc' works as expected", {
  # percentage of values missing (excludes non-conformant ones) out of number of records
  # For timepoints with no records, value should show NA

  testvalues <- c("2021-06-01", "",
                  "2021-06-02", "1",
                  "2021-06-02", "2",
                  "2021-06-03", "a",
                  "2021-06-03", "2",
                  "2021-06-03", "3",
                  "2021-06-05", "",
                  "2021-06-05", "2",
                  "2021-06-05", "3",
                  "2021-06-05", "4"
                  )
  grouped_values <-
    aggregate_and_append_values_testhelper(testvalues,
                                           ft_numeric(),
                                           "missing_perc",
                                           aggregation_timeunit = "day")

  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-01"][[2]], 100)
  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-02"][[2]], 0)
  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-03"][[2]], 0)
  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-04"][[2]], NA_real_)
  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-05"][[2]], 25)
})

test_that("aggregate_and_append_values 'missing_perc' works as expected when all values are missing", {
  # percentage of values missing (excludes non-conformant ones) out of number of records
  # For timepoints with no records, value should show NA

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
  grouped_values <-
    aggregate_and_append_values_testhelper(testvalues = testvalues,
                                           field_type = ft_numeric(),
                                           aggregation_function = "missing_perc",
                                           aggregation_timeunit = "day",
                                           add_uid_field = TRUE)

  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-01"][[2]], 100)
  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-02"][[2]], 100)
  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-03"][[2]], 100)
  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-04"][[2]], NA_real_)
  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-05"][[2]], 100)
})

test_that("aggregate_and_append_values 'nonconformant_n' works as expected for numeric fields", {
  # counts number of nonconformant values
  # For timepoints with no records, value should show NA

  testvalues <- c("2021-06-01", "1",
                  "2021-06-02", "1",
                  "2021-06-02", "2",
                  "2021-06-03", "",
                  "2021-06-03", "2",
                  "2021-06-03", "3",
                  "2021-06-05", "a",
                  "2021-06-05", "2",
                  "2021-06-05", "3",
                  "2021-06-05", "4"
                  )
  grouped_values <-
    aggregate_and_append_values_testhelper(testvalues,
                                           ft_numeric(),
                                           "nonconformant_n",
                                           aggregation_timeunit = "day")

  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-01"][[2]], 0)
  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-02"][[2]], 0)
  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-03"][[2]], 0)
  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-04"][[2]], NA_real_)
  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-05"][[2]], 1)
})

test_that("aggregate_and_append_values 'nonconformant_n' works as expected for date fields", {
  # counts number of nonconformant values
  # For timepoints with no records, value should show NA

  testvalues <- c("2021-06-01", "2021-06-01",
                  "2021-06-02", "2021-06-31",
                  "2021-06-03", "2021-06",
                  "2021-06-04", "02/06/2021",
                  "2021-06-05", "2021-06-2",
                  "2021-06-06", "",
                  "2021-06-09", "21-06-03",
                  "2021-06-09", "2021-06-01",
                  "2021-06-09", "2021-06-02",
                  "2021-06-09", "2021-06-03"
                  )
  grouped_values <-
    aggregate_and_append_values_testhelper(testvalues,
                                           ft_datetime(),
                                           "nonconformant_n",
                                           aggregation_timeunit = "day")

  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-01"][[2]], 0)
  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-02"][[2]], 1)
  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-03"][[2]], 1)
  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-04"][[2]], 1)
  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-05"][[2]], 1)
  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-06"][[2]], 0)
  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-07"][[2]], NA_real_)
  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-08"][[2]], NA_real_)
  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-09"][[2]], 1)
})

test_that("aggregate_and_append_values 'nonconformant_n' works as expected when all values are missing", {
  # counts number of nonconformant values
  # For timepoints with no records, value should show NA

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
  grouped_values <-
    aggregate_and_append_values_testhelper(testvalues = testvalues,
                                           field_type = ft_numeric(),
                                           aggregation_function = "nonconformant_n",
                                           aggregation_timeunit = "day",
                                           add_uid_field = TRUE)

  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-01"][[2]], 0)
  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-02"][[2]], 0)
  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-03"][[2]], 0)
  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-04"][[2]], NA_real_)
  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-05"][[2]], 0)
})

test_that("aggregate_and_append_values 'nonconformant_perc' works as expected for numeric fields", {
  # percentage of nonconformant values out of number of records
  # For timepoints with no records, value should show NA

  testvalues <- c("2021-06-01", "1",
                  "2021-06-02", "1",
                  "2021-06-02", "2",
                  "2021-06-03", "",
                  "2021-06-03", "2",
                  "2021-06-03", "3",
                  "2021-06-05", "a",
                  "2021-06-05", "2",
                  "2021-06-05", "3",
                  "2021-06-05", "4"
                  )
  grouped_values <-
    aggregate_and_append_values_testhelper(testvalues,
                                           ft_numeric(),
                                           "nonconformant_perc",
                                           aggregation_timeunit = "day")

  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-01"][[2]], 0)
  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-02"][[2]], 0)
  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-03"][[2]], 0)
  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-04"][[2]], NA_real_)
  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-05"][[2]], 25)
})

test_that("aggregate_and_append_values 'nonconformant_perc' works as expected for date fields", {
  # percentage of nonconformant values out of number of records
  # For timepoints with no records, value should show NA

  testvalues <- c("2021-06-01", "2021-06-01",
                  "2021-06-02", "2021-06-31",
                  "2021-06-03", "2021-06",
                  "2021-06-04", "02/06/2021",
                  "2021-06-05", "2021-06-2",
                  "2021-06-06", "",
                  "2021-06-09", "21-06-03",
                  "2021-06-09", "2021-06-01",
                  "2021-06-09", "2021-06-02",
                  "2021-06-09", "2021-06-03"
                  )
  grouped_values <-
    aggregate_and_append_values_testhelper(testvalues,
                                           ft_datetime(),
                                           "nonconformant_perc",
                                           aggregation_timeunit = "day")

  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-01"][[2]], 0)
  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-02"][[2]], 100)
  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-03"][[2]], 100)
  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-04"][[2]], 100)
  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-05"][[2]], 100)
  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-06"][[2]], 0)
  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-07"][[2]], NA_real_)
  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-08"][[2]], NA_real_)
  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-09"][[2]], 25)
})

test_that("aggregate_and_append_values 'nonconformant_perc' works as expected when all values are missing", {
  # percentage of nonconformant values out of number of records
  # For timepoints with no records, value should show NA

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
  grouped_values <-
    aggregate_and_append_values_testhelper(testvalues = testvalues,
                                           field_type = ft_numeric(),
                                           aggregation_function = "nonconformant_perc",
                                           aggregation_timeunit = "day",
                                           add_uid_field = TRUE)

  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-01"][[2]], 0)
  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-02"][[2]], 0)
  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-03"][[2]], 0)
  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-04"][[2]], NA_real_)
  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-05"][[2]], 0)
})

test_that("aggregate_and_append_values 'sum' works as expected", {
  # sum of values (used to indicate number of duplicate rows removed)
  # For timepoints with no records, value should show NA

  testvalues <- c("2021-06-01", "1",
                  "2021-06-02", "1",
                  "2021-06-02", "1",
                  "2021-06-03", "",
                  "2021-06-03", "2",
                  "2021-06-03", "2",
                  "2021-06-05", "a",
                  "2021-06-05", "3",
                  "2021-06-05", "3",
                  "2021-06-05", "3"
                  )
  grouped_values <-
    aggregate_and_append_values_testhelper(testvalues,
                                           ft_numeric(),
                                           "sum",
                                           aggregation_timeunit = "day",
                                           field_offset = 1)

  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-01"][[2]], 0)
  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-02"][[2]], 1)
  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-03"][[2]], 1)
  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-04"][[2]], NA_real_)
  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-05"][[2]], 2)
})

test_that("aggregate_and_append_values 'nonzero_perc' works as expected", {
  # percentage of values which are non-zero out of number of values present
  #   (used to indicate percentage of remaining records that were duplicated)

  testvalues <- c("2021-06-01", "1",
                  "2021-06-02", "1",
                  "2021-06-02", "1",
                  "2021-06-03", "",
                  "2021-06-03", "2",
                  "2021-06-03", "2",
                  "2021-06-05", "a",
                  "2021-06-05", "3",
                  "2021-06-05", "3",
                  "2021-06-05", "3"
                  )
  grouped_values <-
    aggregate_and_append_values_testhelper(testvalues,
                                           ft_numeric(),
                                           "nonzero_perc",
                                           aggregation_timeunit = "day",
                                           field_offset = 1)

  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-01"][[2]], 0)
  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-02"][[2]], 100)
  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-03"][[2]], 50)
  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-04"][[2]], NA_real_)
  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-05"][[2]], 50)
})

test_that("aggregate_and_append_values 'distinct' works as expected", {
  # number of distinct values (excluding NAs)

  testvalues <- c("2021-06-01", "a",
                  "2021-06-02", "a",
                  "2021-06-02", "b",
                  "2021-06-03", "",
                  "2021-06-03", "b",
                  "2021-06-03", "b",
                  "2021-06-05", "a",
                  "2021-06-05", "b",
                  "2021-06-05", "c",
                  "2021-06-05", "a"
                  )
  grouped_values <-
    aggregate_and_append_values_testhelper(testvalues,
                                           ft_categorical(),
                                           "distinct",
                                           aggregation_timeunit = "day")

  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-01"][[2]], 1)
  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-02"][[2]], 2)
  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-03"][[2]], 1)
  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-04"][[2]], NA_real_)
  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-05"][[2]], 3)
})

test_that("aggregate_and_append_values 'distinct' works as expected when all values are missing", {
  # number of distinct values (excluding NAs)
  # For timepoints with no non-missing values, value should show NA for
  #   consistency with other fieldtype-specific agg_funs
  # For timepoints with no records, value should show NA

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
  grouped_values <-
    aggregate_and_append_values_testhelper(testvalues = testvalues,
                                           field_type = ft_categorical(),
                                           aggregation_function = "distinct",
                                           aggregation_timeunit = "day",
                                           add_uid_field = TRUE
                                           )

  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-01"][[2]], NA_real_)
  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-02"][[2]], NA_real_)
  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-03"][[2]], NA_real_)
  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-04"][[2]], NA_real_)
  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-05"][[2]], NA_real_)
})

test_that("aggregate_and_append_values 'midnight_n' works as expected", {
  # number of values whose time portion is midnight (used to check for missing time portions)

  testvalues <- c("2021-06-01", "2021-06-01",
                  "2021-06-02", "2021-06",
                  "2021-06-02", "2021-06-31 02:00:00",
                  "2021-06-02", "2021-06-01 24:00:00",
                  "2021-06-05", "2021-06-01 02:00:00",
                  "2021-06-06", "",
                  "2021-06-09", "",
                  "2021-06-09", "2021-06-01 00:00:00",
                  "2021-06-09", "2021-06-02 02:00",
                  "2021-06-09", "2021-06-03 02:00:00"
                  )

  grouped_values <-
    aggregate_and_append_values_testhelper(testvalues,
                                           ft_datetime(),
                                           "midnight_n",
                                           aggregation_timeunit = "day")

  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-01"][[2]], 1)
  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-02"][[2]], NA_real_)
  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-03"][[2]], NA_real_)
  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-04"][[2]], NA_real_)
  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-05"][[2]], 0)
  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-06"][[2]], NA_real_)
  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-07"][[2]], NA_real_)
  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-08"][[2]], NA_real_)
  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-09"][[2]], 1)
})

test_that("aggregate_and_append_values 'midnight_n' works as expected when all values are missing", {
  # number of values whose time portion is midnight (used to check for missing time portions)

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
  grouped_values <-
    aggregate_and_append_values_testhelper(testvalues = testvalues,
                                           field_type = ft_categorical(),
                                           aggregation_function = "midnight_n",
                                           aggregation_timeunit = "day",
                                           add_uid_field = TRUE
                                           )

  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-01"][[2]], NA_real_)
  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-02"][[2]], NA_real_)
  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-03"][[2]], NA_real_)
  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-04"][[2]], NA_real_)
  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-05"][[2]], NA_real_)
})

test_that("aggregate_and_append_values 'midnight_perc' works as expected", {
  # percentage of values whose time portion is midnight (used to check for missing time portions)
  #   out of number of values present

  testvalues <- c("2021-06-01", "2021-06-01",
                  "2021-06-02", "2021-06-01 00:00:00",
                  "2021-06-02", "2021-06-01 02:00:00",
                  "2021-06-02", "2021-06-01 24:00:00",
                  "2021-06-05", "2021-06-01 02:00:00",
                  "2021-06-06", "",
                  "2021-06-09", "",
                  "2021-06-09", "2021-06-01 00:00:00",
                  "2021-06-09", "2021-06-03 02:00:00",
                  "2021-06-09", "2021-06-03 03:00:00",
                  "2021-06-09", "2021-06-03 04:00:00"
                  )

  grouped_values <-
    aggregate_and_append_values_testhelper(testvalues,
                                           ft_datetime(),
                                           "midnight_perc",
                                           aggregation_timeunit = "day")

  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-01"][[2]], 100)
  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-02"][[2]], 50)
  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-03"][[2]], NA_real_)
  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-04"][[2]], NA_real_)
  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-05"][[2]], 0)
  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-06"][[2]], NA_real_)
  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-07"][[2]], NA_real_)
  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-08"][[2]], NA_real_)
  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-09"][[2]], 25)
})

test_that("aggregate_and_append_values 'midnight_perc' works as expected when all values are missing", {
  # percentage of values whose time portion is midnight (used to check for missing time portions)
  #   out of number of values present

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
  grouped_values <-
    aggregate_and_append_values_testhelper(testvalues = testvalues,
                                           field_type = ft_categorical(),
                                           aggregation_function = "midnight_perc",
                                           aggregation_timeunit = "day",
                                           add_uid_field = TRUE
                                           )

  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-01"][[2]], NA_real_)
  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-02"][[2]], NA_real_)
  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-03"][[2]], NA_real_)
  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-04"][[2]], NA_real_)
  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-05"][[2]], NA_real_)
})

test_that("aggregate_and_append_values 'min' works as expected for numeric fields", {
  # minimum value
  # For timepoints with no records, value should show NA

  testvalues <- c("2021-06-01", "1",
                  "2021-06-02", "a",
                  "2021-06-02", "2",
                  "2021-06-02", "3",
                  "2021-06-05", "b",
                  "2021-06-06", "",
                  "2021-06-09", "",
                  "2021-06-09", "1",
                  "2021-06-09", "2",
                  "2021-06-09", "3"
                  )

  grouped_values <-
    aggregate_and_append_values_testhelper(testvalues,
                                           ft_numeric(),
                                           "min",
                                           aggregation_timeunit = "day")

  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-01"][[2]], 1)
  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-02"][[2]], 2)
  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-03"][[2]], NA_real_)
  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-04"][[2]], NA_real_)
  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-05"][[2]], NA_real_)
  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-06"][[2]], NA_real_)
  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-07"][[2]], NA_real_)
  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-08"][[2]], NA_real_)
  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-09"][[2]], 1)
})

test_that("aggregate_and_append_values 'min' works as expected for date fields", {
  # minimum value
  # For timepoints with no records, value should show NA
  # first test value should be blank to check that the grouped_values get typed correctly

  testvalues <- c("2021-06-01", "",
                  "2021-06-02", "2021-06",
                  "2021-06-02", "2021-06-02",
                  "2021-06-02", "2021-06-03",
                  "2021-06-05", "2021-06",
                  "2021-06-06", "",
                  "2021-06-09", "",
                  "2021-06-09", "2021-06-01",
                  "2021-06-09", "2021-06-02",
                  "2021-06-09", "2021-06-03"
                  )

  grouped_values <-
    aggregate_and_append_values_testhelper(testvalues,
                                           ft_datetime(),
                                           "min",
                                           aggregation_timeunit = "day")

  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-01"][[2]], as.POSIXct(NA, tz = "UTC"))
  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-02"][[2]], as.POSIXct("2021-06-02", tz = "UTC"))
  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-03"][[2]], as.POSIXct(NA, tz = "UTC"))
  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-04"][[2]], as.POSIXct(NA, tz = "UTC"))
  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-05"][[2]], as.POSIXct(NA, tz = "UTC"))
  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-06"][[2]], as.POSIXct(NA, tz = "UTC"))
  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-07"][[2]], as.POSIXct(NA, tz = "UTC"))
  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-08"][[2]], as.POSIXct(NA, tz = "UTC"))
  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-09"][[2]], as.POSIXct("2021-06-01", tz = "UTC"))
})

test_that("aggregate_and_append_values 'min' works as expected when all values are missing", {
  # minimum value
  # For timepoints with no records, value should show NA

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
  grouped_values <-
    aggregate_and_append_values_testhelper(testvalues = testvalues,
                                           field_type = ft_categorical(),
                                           aggregation_function = "min",
                                           aggregation_timeunit = "day",
                                           add_uid_field = TRUE
                                           )

  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-01"][[2]], NA_real_)
  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-02"][[2]], NA_real_)
  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-03"][[2]], NA_real_)
  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-04"][[2]], NA_real_)
  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-05"][[2]], NA_real_)
})

test_that("aggregate_and_append_values 'max' works as expected for numeric fields", {
  # maximum value
  # For timepoints with no records, value should show NA

  testvalues <- c("2021-06-01", "1",
                  "2021-06-02", "a",
                  "2021-06-02", "2",
                  "2021-06-02", "3",
                  "2021-06-05", "b",
                  "2021-06-06", "",
                  "2021-06-09", "",
                  "2021-06-09", "1",
                  "2021-06-09", "2",
                  "2021-06-09", "3"
                  )

  grouped_values <-
    aggregate_and_append_values_testhelper(testvalues,
                                           ft_numeric(),
                                           "max",
                                           aggregation_timeunit = "day")

  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-01"][[2]], 1)
  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-02"][[2]], 3)
  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-03"][[2]], NA_real_)
  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-04"][[2]], NA_real_)
  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-05"][[2]], NA_real_)
  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-06"][[2]], NA_real_)
  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-07"][[2]], NA_real_)
  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-08"][[2]], NA_real_)
  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-09"][[2]], 3)
})

test_that("aggregate_and_append_values 'max' works as expected for date fields", {
  # maximum value
  # For timepoints with no records, value should show NA
  # first test value should be blank to check that the grouped_values get typed correctly

  testvalues <- c("2021-06-01", "",
                  "2021-06-02", "2021-06",
                  "2021-06-02", "2021-06-02",
                  "2021-06-02", "2021-06-03",
                  "2021-06-05", "2021-06",
                  "2021-06-06", "",
                  "2021-06-09", "",
                  "2021-06-09", "2021-06-01",
                  "2021-06-09", "2021-06-02",
                  "2021-06-09", "2021-06-03"
                  )

  grouped_values <-
    aggregate_and_append_values_testhelper(testvalues,
                                           ft_datetime(),
                                           "max",
                                           aggregation_timeunit = "day")

  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-01"][[2]], as.POSIXct(NA, tz = "UTC"))
  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-02"][[2]], as.POSIXct("2021-06-03", tz = "UTC"))
  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-03"][[2]], as.POSIXct(NA, tz = "UTC"))
  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-04"][[2]], as.POSIXct(NA, tz = "UTC"))
  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-05"][[2]], as.POSIXct(NA, tz = "UTC"))
  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-06"][[2]], as.POSIXct(NA, tz = "UTC"))
  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-07"][[2]], as.POSIXct(NA, tz = "UTC"))
  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-08"][[2]], as.POSIXct(NA, tz = "UTC"))
  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-09"][[2]], as.POSIXct("2021-06-03", tz = "UTC"))
})

test_that("aggregate_and_append_values 'max' works as expected when all values are missing", {
  # maximum value
  # For timepoints with no records, value should show NA

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
  grouped_values <-
    aggregate_and_append_values_testhelper(testvalues = testvalues,
                                           field_type = ft_categorical(),
                                           aggregation_function = "max",
                                           aggregation_timeunit = "day",
                                           add_uid_field = TRUE
                                           )

  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-01"][[2]], NA_real_)
  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-02"][[2]], NA_real_)
  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-03"][[2]], NA_real_)
  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-04"][[2]], NA_real_)
  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-05"][[2]], NA_real_)
})

test_that("aggregate_and_append_values 'mean' works as expected", {
  # mean value. Excludes NAs
  # For timepoints with no records, value should show NA

  testvalues <- c("2021-06-01", "1",
                  "2021-06-02", "a",
                  "2021-06-02", "2",
                  "2021-06-02", "3",
                  "2021-06-05", "b",
                  "2021-06-06", "",
                  "2021-06-09", "",
                  "2021-06-09", "1",
                  "2021-06-09", "2",
                  "2021-06-09", "6"
                  )

  grouped_values <-
    aggregate_and_append_values_testhelper(testvalues,
                                           ft_numeric(),
                                           "mean",
                                           aggregation_timeunit = "day")

  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-01"][[2]], 1)
  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-02"][[2]], 2.5)
  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-03"][[2]], NA_real_)
  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-04"][[2]], NA_real_)
  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-05"][[2]], NA_real_)
  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-06"][[2]], NA_real_)
  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-07"][[2]], NA_real_)
  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-08"][[2]], NA_real_)
  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-09"][[2]], 3)
})

test_that("aggregate_and_append_values 'median' works as expected", {
  # median value. Excludes NAs
  # For timepoints with no records, value should show NA

  testvalues <- c("2021-06-01", "1",
                  "2021-06-02", "a",
                  "2021-06-02", "2",
                  "2021-06-02", "3",
                  "2021-06-05", "b",
                  "2021-06-06", "",
                  "2021-06-09", "",
                  "2021-06-09", "1",
                  "2021-06-09", "2",
                  "2021-06-09", "6"
                  )

  grouped_values <-
    aggregate_and_append_values_testhelper(testvalues,
                                           ft_numeric(),
                                           "median",
                                           aggregation_timeunit = "day")

  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-01"][[2]], 1)
  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-02"][[2]], 2.5)
  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-03"][[2]], NA_real_)
  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-04"][[2]], NA_real_)
  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-05"][[2]], NA_real_)
  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-06"][[2]], NA_real_)
  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-07"][[2]], NA_real_)
  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-08"][[2]], NA_real_)
  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-09"][[2]], 2)
})

test_that("aggregate_and_append_values 'mean' works as expected when all values are missing", {
  # mean value
  # For timepoints with no records, value should show NA

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
  grouped_values <-
    aggregate_and_append_values_testhelper(testvalues = testvalues,
                                           field_type = ft_categorical(),
                                           aggregation_function = "mean",
                                           aggregation_timeunit = "day",
                                           add_uid_field = TRUE
                                           )

  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-01"][[2]], NA_real_)
  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-02"][[2]], NA_real_)
  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-03"][[2]], NA_real_)
  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-04"][[2]], NA_real_)
  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-05"][[2]], NA_real_)
})


test_that("aggregate_and_append_values 'min_length' works as expected", {
  # minimum character length
  # For timepoints with no records, value should show NA

  testvalues <- c("2021-06-01", "a",
                  "2021-06-02", "a",
                  "2021-06-02", "ab",
                  "2021-06-02", "abc",
                  "2021-06-05", "abc",
                  "2021-06-06", "",
                  "2021-06-09", "",
                  "2021-06-09", "a",
                  "2021-06-09", "ab"
                  )

  grouped_values <-
    aggregate_and_append_values_testhelper(testvalues,
                                           ft_uniqueidentifier(),
                                           "min_length",
                                           aggregation_timeunit = "day")

  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-01"][[2]], 1)
  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-02"][[2]], 1)
  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-03"][[2]], NA_real_)
  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-04"][[2]], NA_real_)
  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-05"][[2]], 3)
  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-06"][[2]], NA_real_)
  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-07"][[2]], NA_real_)
  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-08"][[2]], NA_real_)
  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-09"][[2]], 1)
})

test_that("aggregate_and_append_values 'min_length' works as expected when all values are missing", {
  # minimum character length
  # For timepoints with no records, value should show NA

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
  grouped_values <-
    aggregate_and_append_values_testhelper(testvalues = testvalues,
                                           field_type = ft_categorical(),
                                           aggregation_function = "min_length",
                                           aggregation_timeunit = "day",
                                           add_uid_field = TRUE
                                           )

  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-01"][[2]], NA_real_)
  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-02"][[2]], NA_real_)
  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-03"][[2]], NA_real_)
  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-04"][[2]], NA_real_)
  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-05"][[2]], NA_real_)
})

test_that("aggregate_and_append_values 'max_length' works as expected", {
  # maximum character length
  # For timepoints with no records, value should show NA

  testvalues <- c("2021-06-01", "a",
                  "2021-06-02", "a",
                  "2021-06-02", "ab",
                  "2021-06-02", "abc",
                  "2021-06-05", "abc",
                  "2021-06-06", "",
                  "2021-06-09", "",
                  "2021-06-09", "a",
                  "2021-06-09", "ab"
                  )

  grouped_values <-
    aggregate_and_append_values_testhelper(testvalues,
                                           ft_uniqueidentifier(),
                                           "max_length",
                                           aggregation_timeunit = "day")

  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-01"][[2]], 1)
  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-02"][[2]], 3)
  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-03"][[2]], NA_real_)
  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-04"][[2]], NA_real_)
  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-05"][[2]], 3)
  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-06"][[2]], NA_real_)
  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-07"][[2]], NA_real_)
  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-08"][[2]], NA_real_)
  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-09"][[2]], 2)
})

test_that("aggregate_and_append_values 'max_length' works as expected when all values are missing", {
  # maximum character length
  # For timepoints with no records, value should show NA

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
  grouped_values <-
    aggregate_and_append_values_testhelper(testvalues = testvalues,
                                           field_type = ft_categorical(),
                                           aggregation_function = "max_length",
                                           aggregation_timeunit = "day",
                                           add_uid_field = TRUE
                                           )

  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-01"][[2]], NA_real_)
  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-02"][[2]], NA_real_)
  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-03"][[2]], NA_real_)
  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-04"][[2]], NA_real_)
  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-05"][[2]], NA_real_)
})

test_that("aggregate_and_append_values 'mean_length' works as expected", {
  # mean character length
  # For timepoints with no records, value should show NA

  testvalues <- c("2021-06-01", "a",
                  "2021-06-02", "a",
                  "2021-06-02", "ab",
                  "2021-06-02", "abc",
                  "2021-06-05", "abc",
                  "2021-06-06", "",
                  "2021-06-09", "",
                  "2021-06-09", "a",
                  "2021-06-09", "ab"
                  )

  grouped_values <-
    aggregate_and_append_values_testhelper(testvalues,
                                           ft_uniqueidentifier(),
                                           "mean_length",
                                           aggregation_timeunit = "day")

  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-01"][[2]], 1)
  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-02"][[2]], 2)
  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-03"][[2]], NA_real_)
  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-04"][[2]], NA_real_)
  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-05"][[2]], 3)
  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-06"][[2]], NA_real_)
  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-07"][[2]], NA_real_)
  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-08"][[2]], NA_real_)
  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-09"][[2]], 1.5)
})

test_that("aggregate_and_append_values 'mean_length' works as expected when all values are missing", {
  # mean character length
  # For timepoints with no records, value should show NA

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
  grouped_values <-
    aggregate_and_append_values_testhelper(testvalues = testvalues,
                                           field_type = ft_categorical(),
                                           aggregation_function = "mean_length",
                                           aggregation_timeunit = "day",
                                           add_uid_field = TRUE
                                           )

  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-01"][[2]], NA_real_)
  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-02"][[2]], NA_real_)
  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-03"][[2]], NA_real_)
  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-04"][[2]], NA_real_)
  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-05"][[2]], NA_real_)
})

test_that("aggregate_and_append_values 'subcat_n' works as expected", {
  # number of times this particular category value appears
  # need to make sure rows don't get deduped first

  testvalues <- c("2021-06-01", "a",
                  "2021-06-02", "a",
                  "2021-06-02", "a",
                  "2021-06-02", "b",
                  "2021-06-02", "c",
                  "2021-06-04", "",
                  "2021-06-05", "",
                  "2021-06-05", "a",
                  "2021-06-05", "a",
                  "2021-06-05", "a"
                  )

  grouped_values <-
    aggregate_and_append_values_testhelper(testvalues,
                                           ft_categorical(),
                                           "subcat_n",
                                           aggregation_timeunit = "day",
                                           add_uid_field = TRUE)

  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-01"][[2]], 1)
  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-01"][[3]], 0)
  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-01"][[4]], 0)
  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-02"][[2]], 2)
  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-02"][[3]], 1)
  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-02"][[4]], 1)
  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-03"][[2]], NA_real_)
  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-03"][[3]], NA_real_)
  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-03"][[4]], NA_real_)
  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-04"][[2]], 0)
  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-04"][[3]], 0)
  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-04"][[4]], 0)
  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-05"][[2]], 3)
  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-05"][[3]], 0)
  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-05"][[4]], 0)
})

test_that("aggregate_and_append_values 'subcat_perc' works as expected", {
  # percentage this particular category value appears out of number of records
  # include all values in denominator, including NA and NaN
  # need to make sure rows don't get deduped first

  testvalues <- c("2021-06-01", "a",
                  "2021-06-02", "a",
                  "2021-06-02", "a",
                  "2021-06-02", "b",
                  "2021-06-02", "c",
                  "2021-06-04", "",
                  "2021-06-05", "",
                  "2021-06-05", "a",
                  "2021-06-05", "a",
                  "2021-06-05", "a"
                  )

  grouped_values <-
    aggregate_and_append_values_testhelper(testvalues,
                                           ft_categorical(),
                                           "subcat_perc",
                                           aggregation_timeunit = "day",
                                           add_uid_field = TRUE)

  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-01"][[2]], 100)
  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-01"][[3]], 0)
  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-01"][[4]], 0)
  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-02"][[2]], 50)
  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-02"][[3]], 25)
  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-02"][[4]], 25)
  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-03"][[2]], NA_real_)
  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-03"][[3]], NA_real_)
  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-03"][[4]], NA_real_)
  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-04"][[2]], 0)
  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-04"][[3]], 0)
  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-04"][[4]], 0)
  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-05"][[2]], 75)
  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-05"][[3]], 0)
  expect_equal(grouped_values[grouped_values[[1]] == "2021-06-05"][[4]], 0)
})
