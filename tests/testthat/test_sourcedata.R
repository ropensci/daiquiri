test_that("validate_columnnames() checks that colnames match fieldtypes spec", {
	# Column names in data and fieldtypes match exactly
	expect_silent(validate_columnnames(c("nonsense","set","of")
																		 ,c("nonsense","set","of"), check_length_only = FALSE))

	# Column names in data and fieldtypes match in different order
	expect_silent(validate_columnnames(c("nonsense","set","of")
																		 ,c("nonsense","of","set"), check_length_only = FALSE))

	# Column names in data and fieldtypes match in length only
	expect_silent(validate_columnnames(c("nonsense","set")
																		 ,c("nonsense","names"), check_length_only = TRUE))

	# Column names in data and fieldtypes don't match in length only
	expect_error(validate_columnnames(c("nonsense","set","of")
																		,c("nonsense","set"), check_length_only = TRUE),
							 class = "invalid_columnnames")

	# Duplicate column names in data not allowed
	expect_error(validate_columnnames(c("nonsense","set","of","nonsense","names")
																		,c("nonsense","set","of","names"), check_length_only = FALSE),
							 class = "invalid_columnnames")

	# Column names in data not in fieldtypes not allowed
	expect_error(validate_columnnames(c("nonsense","set","of","stuff","names")
																		,c("nonsense","set","of","stuff"), check_length_only = FALSE),
							 class = "invalid_columnnames")

	# Column names in fieldtypes not in data not allowed
	expect_error(validate_columnnames(c("nonsense","set","of")
																		,c("nonsense","set","of","stuff"), check_length_only = FALSE),
							 class = "invalid_columnnames")

})



test_that("prepare_data() params are present and of correct type", {
	expect_error(prepare_data(fieldtypes = fieldtypes(Col_tp = ft_timepoint())),
							 class = "invalid_param_missing")

	expect_error(prepare_data(df = data.frame("Fieldname" = 123)),
							 class = "invalid_param_missing")

	expect_error(prepare_data(c("Fieldname", 123),
														fieldtypes = fieldtypes(Col_tp = ft_timepoint())),
							 class = "invalid_param_type")

	expect_error(prepare_data(df = data.frame("Fieldname" = 123),
														fieldtypes = TRUE),
							 class = "invalid_param_type")

})
