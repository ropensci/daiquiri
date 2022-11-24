test_that("validate_params_required() checks is silent if required params are supplied", {
  # NOTE: testfn_params_required() defined in utilities.R as devtools::test() can't find it when it's defined here
  # 	Think it is something to do with environments but haven't figured it out

  expect_silent(testfn_params_required(1, 2))
  expect_silent(testfn_params_required(p2 = 1, p1 = 2))
})

test_that("validate_params_required() checks returns an error if any required params are not supplied", {
  # NOTE: testfn_params_required() defined in utilities.R as devtools::test() can't find it when it's defined here

  expect_error(testfn_params_required(),
    class = "invalid_param_missing"
  )
  expect_error(testfn_params_required(1),
    class = "invalid_param_missing"
  )
  expect_error(testfn_params_required(p2 = 1),
    class = "invalid_param_missing"
  )
})

test_that("validate_params_required() allows arbitrary additional params to be supplied via ...", {
  expect_silent(testfn_params_required(p2 = 1, p1 = 2, passthrough = 1))
})

test_that("validate_params_required() works with package prefix", {
  expect_error(daiquiri::initialise_log(),
    class = "invalid_param_missing"
  )
})


test_that("validate_params_type() is silent if all params are of correct type", {
  # NOTE: testfn_params_type() defined in utilities.R as devtools::test() can't find it when it's defined here

  # all default args are valid
  expect_silent(testfn_params_type())
})

test_that("validate_params_type() checks df params are of correct type", {
  expect_error(testfn_params_type(df = c("Fieldname" = 123)),
    class = "invalid_param_type"
  )
})

test_that("validate_params_type() checks override_column_names params are of correct type", {
  expect_error(testfn_params_type(override_column_names = c("col1", "col2")),
    class = "invalid_param_type"
  )
})

test_that("validate_params_type() checks na params are of correct type", {
  expect_error(testfn_params_type(na = NA),
    class = "invalid_param_type"
  )
  expect_error(testfn_params_type(na = c(NULL)),
    class = "invalid_param_type"
  )
  expect_error(testfn_params_type(na = c(1, 2, 3)),
    class = "invalid_param_type"
  )
})

test_that("validate_params_type() checks dataset_description params are of correct type", {
  expect_silent(testfn_params_type(dataset_description = ""))
  expect_silent(testfn_params_type(dataset_description = NULL))
  expect_error(testfn_params_type(dataset_description = 123),
    class = "invalid_param_type"
  )
  expect_error(testfn_params_type(dataset_description = c("col1", "col2")),
    class = "invalid_param_type"
  )
})

test_that("validate_params_type() checks report_title params are of correct type", {
  expect_silent(testfn_params_type(report_title = ""))
  expect_silent(testfn_params_type(report_title = NULL))
  expect_error(testfn_params_type(report_title = 123),
    class = "invalid_param_type"
  )
  expect_error(testfn_params_type(report_title = c("col1", "col2")),
    class = "invalid_param_type"
  )
})

test_that("validate_params_type() checks aggregation_timeunit params are one of day/week/month/quarter/year", {
  expect_silent(testfn_params_type(aggregation_timeunit = "day"))
  expect_silent(testfn_params_type(aggregation_timeunit = "week"))
  expect_silent(testfn_params_type(aggregation_timeunit = "month"))
  expect_silent(testfn_params_type(aggregation_timeunit = "quarter"))
  expect_silent(testfn_params_type(aggregation_timeunit = "year"))
  expect_error(testfn_params_type(aggregation_timeunit = "other"),
    class = "invalid_param_type"
  )
})

test_that("validate_params_type() checks aggregation_timeunit params cannot be a vector", {
  expect_error(testfn_params_type(aggregation_timeunit = c("day", "week")),
    class = "invalid_param_type"
  )
})

test_that("validate_params_type() checks save_directory params are of correct type", {
  # Real dir
  expect_silent(testfn_params_type(save_directory = test_path()))
  # Real dir with trailing slash
  expect_silent(testfn_params_type(save_directory = paste0(test_path(), "/")))
  # Fake dir
  expect_error(testfn_params_type(save_directory = "fakedir"),
    class = "invalid_param_type"
  )
  # Dir includes filename
  expect_error(testfn_params_type(save_directory = test_path("test_utilities.R")),
    class = "invalid_param_type"
  )
})

test_that("validate_params_type() checks save_filename params are allowed to contain alphanumerics, - and _", {
  expect_silent(testfn_params_type(save_filename = "alpha123"))
  expect_silent(testfn_params_type(save_filename = "alpha-123"))
  expect_silent(testfn_params_type(save_filename = "alpha_123"))
})

test_that("validate_params_type() checks save_filename params are allowed to be NULL", {
  expect_silent(testfn_params_type(save_filename = NULL))
})

test_that("validate_params_type() checks save_filename params are not allowed to contain the extension", {
  expect_error(testfn_params_type(save_filename = "badname.html"),
    class = "invalid_param_type"
  )
})

test_that("validate_params_type() checks save_filename params are not allowed to contain punctuation other than - and _", {
  expect_error(testfn_params_type(save_filename = "bad.name"),
    class = "invalid_param_type"
  )
  expect_error(testfn_params_type(save_filename = "badname&"),
    class = "invalid_param_type"
  )
  expect_error(testfn_params_type(save_filename = "badname*"),
    class = "invalid_param_type"
  )
})

test_that("validate_params_type() checks show_progress params are of correct type", {
  expect_error(testfn_params_type(show_progress = 1),
    class = "invalid_param_type"
  )
})

test_that("validate_params_type() checks log_directory params are of correct type", {
  expect_silent(testfn_params_type(log_directory = NULL))
  expect_error(testfn_params_type(log_directory = "fakedir"),
    class = "invalid_param_type"
  )
})

test_that("validate_params_type() checks source_data params are of correct type", {
  expect_error(testfn_params_type(source_data = 1),
    class = "invalid_param_type"
  )
})

test_that("validate_params_type() checks aggregated_data params are of correct type", {
  expect_error(testfn_params_type(aggregated_data = 1),
    class = "invalid_param_type"
  )
})

test_that("validate_params_type() works with package prefix", {
  expect_error(daiquiri::initialise_log(log_directory = "hello"),
    class = "invalid_param_type"
  )
})


test_that("initialise_log() requires a log_directory param", {
  expect_error(initialise_log(),
    class = "invalid_param_missing"
  )
})

test_that("initialise_log() requires log_directory param to be a valid path", {
  expect_error(initialise_log(log_directory = "hello"),
    class = "invalid_param_type"
  )
})

test_that("initialise_log() creates a file", {
  log_filename <- initialise_log(log_directory = tempdir())
  # clean up
  expect_true(file.remove(log_filename))
})

test_that("log_message() writes to log", {
  log_filename <- initialise_log(log_directory = tempdir())
  expect_silent(log_message("test message", show_progress = FALSE))
  log_text <- readLines(log_filename)
  expect_true(any(grepl("test message", log_text)))
  # clean up
  expect_true(file.remove(log_filename))
})

test_that("close_log() returns the name of the closed log file", {
  log_filename <- initialise_log(log_directory = tempdir())
  expect_equal(close_log(), log_filename)
})

test_that("close_log() returns empty string if no log file found", {
  expect_equal(close_log(), "")
})
