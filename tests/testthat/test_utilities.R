test_that("validate_params_required() checks if params are supplied", {
	# NOTE: testfn_params_required() defined in utilities.R as devtools::test() can't find it when it's defined here
	# 	Think it is something to do with environments but haven't figured it out

	# testfn_params_required <- function(p1, p2, p3 = NULL){
	# 	validate_params_required(match.call())
	# }

	expect_silent(testfn_params_required(1, 2))
	expect_silent(testfn_params_required(p2 = 1, p1 = 2))

	expect_error(testfn_params_required(),
							 class = "invalid_param_missing")
	expect_error(testfn_params_required(1),
							 class = "invalid_param_missing")
	expect_error(testfn_params_required(p2 = 1),
							 class = "invalid_param_missing")

})


test_that("validate_params_type() checks if params are of correct type", {
	# NOTE: testfn_params_type() defined in utilities.R as devtools::test() can't find it when it's defined here
	# 	Think it is something to do with environments but haven't figured it out
	# TODO: consider splitting this into multiple tests

	# all default args are valid
	expect_silent(testfn_params_type())

	expect_error(testfn_params_type(df = c("Fieldname" = 123)),
							 class = "invalid_param_type")

	expect_error(testfn_params_type(override_columnnames = c("col1", "col2")),
							 class = "invalid_param_type")

	expect_error(testfn_params_type(na = NA),
							 class = "invalid_param_type")
	expect_error(testfn_params_type(na = c(NULL)),
							 class = "invalid_param_type")
	expect_error(testfn_params_type(na = c(1,2,3)),
							 class = "invalid_param_type")

	expect_silent(testfn_params_type(dataset_shortdesc = ""))
	expect_silent(testfn_params_type(dataset_shortdesc = NULL))
	expect_error(testfn_params_type(dataset_shortdesc = 123),
							 class = "invalid_param_type")
	expect_error(testfn_params_type(dataset_shortdesc = c("col1", "col2")),
							 class = "invalid_param_type")

	expect_silent(testfn_params_type(aggregation_timeunit = "day"))
	expect_silent(testfn_params_type(aggregation_timeunit = "week"))
	expect_silent(testfn_params_type(aggregation_timeunit = "month"))
	expect_silent(testfn_params_type(aggregation_timeunit = "quarter"))
	expect_silent(testfn_params_type(aggregation_timeunit = "year"))
	expect_error(testfn_params_type(aggregation_timeunit = "other"),
							 class = "invalid_param_type")
	expect_error(testfn_params_type(aggregation_timeunit = c("day", "week")),
							 class = "invalid_param_type")

	# Real dir
	expect_silent(testfn_params_type(save_directory = test_path()))
	# Real dir with trailing slash
	expect_silent(testfn_params_type(save_directory = paste0(test_path(), "/")))
	# Fake dir
	expect_error(testfn_params_type(save_directory = "fakedir"),
							 class = "invalid_param_type")
	# Dir includes filename
	expect_error(testfn_params_type(save_directory = test_path("test_utilities.R")),
							 class = "invalid_param_type")

	# Good names
	expect_silent(testfn_params_type(save_filename = "alpha123"))
	expect_silent(testfn_params_type(save_filename = "alpha-123"))
	expect_silent(testfn_params_type(save_filename = "alpha_123"))
	expect_silent(testfn_params_type(save_filename = NULL))
	# Bad names
	expect_error(testfn_params_type(save_filename = "badname.html"),
							 class = "invalid_param_type")
	expect_error(testfn_params_type(save_filename = "bad.name"),
							 class = "invalid_param_type")
	expect_error(testfn_params_type(save_filename = "badname&"),
							 class = "invalid_param_type")
	expect_error(testfn_params_type(save_filename = "badname*"),
							 class = "invalid_param_type")


	expect_error(testfn_params_type(showprogress = 1),
							 class = "invalid_param_type")

	expect_silent(testfn_params_type(log_directory = NULL))
	expect_error(testfn_params_type(log_directory = "fakedir"),
							 class = "invalid_param_type")

	expect_error(testfn_params_type(sourcedata = 1),
							 class = "invalid_param_type")

	expect_error(testfn_params_type(aggregatedata = 1),
							 class = "invalid_param_type")

})


