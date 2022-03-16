test_that("validate_params_required() checks if params are supplied", {
	testfun <- function(p1, p2, p3 = NULL){
		validate_params_required(c("p1", "p2"))
	}

	expect_silent(testfun(1, 2))
	expect_silent(testfun(p2 = 1, p1 = 2))

	expect_error(testfun(),
							 class = "invalid_param_missing")
	expect_error(testfun(1),
							 class = "invalid_param_missing")
	expect_error(testfun(p2 = 1),
							 class = "invalid_param_missing")

})

test_that("validate_param_file() checks if file exists", {
	# Real file
	expect_silent(validate_param_file(test_path("test_utilities.R")))

	# Fake file
	expect_error(validate_param_file("fakefilename.R"),
							 class = "invalid_file_or_path")

})

test_that("validate_param_dir() checks if dir exists", {
	# Real dir
	expect_silent(validate_param_dir(test_path()))

	# Real dir with trailing slash
	expect_silent(validate_param_dir(paste0(test_path(), "/")))

	# Fake dir
	expect_error(validate_param_dir("./fakedir"),
							 class = "invalid_file_or_path")

	# Dir includes filename
	expect_error(validate_param_dir(test_path("test_utilities.R")),
							 class = "invalid_file_or_path")
})

test_that("validate_param_savefilename() checks if supplied name is acceptable", {
	# Good names
	expect_silent(validate_param_savefilename("alpha123", allownull = FALSE))
	expect_silent(validate_param_savefilename("alpha-123", allownull = FALSE))
	expect_silent(validate_param_savefilename("alpha_123", allownull = FALSE))
	expect_silent(validate_param_savefilename(NULL, allownull = TRUE))

	# Bad names
	expect_error(validate_param_savefilename(NULL, allownull = FALSE),
							 class = "invalid_file_or_path")
	expect_error(validate_param_savefilename("alpha&", allownull = FALSE),
							 class = "invalid_file_or_path")
	expect_error(validate_param_savefilename("alpha.", allownull = FALSE),
							 class = "invalid_file_or_path")
	expect_error(validate_param_savefilename("alpha.html", allownull = FALSE),
							 class = "invalid_file_or_path")
	expect_error(validate_param_savefilename("alpha*", allownull = FALSE),
							 class = "invalid_file_or_path")

})
