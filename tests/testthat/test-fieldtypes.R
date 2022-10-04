test_that("Valid fieldtypes can be specified", {
	expect_s3_class(
		fieldtypes(
			Col_tp = ft_timepoint(),
			Col_uid = ft_uniqueidentifier(),
			# NOTE: Partitionfield functionality disabled until we work out how to present it
			#Col_part = ft_partition(),
			Col_cat = ft_categorical(),
			Col_cat2 = ft_categorical(aggregate_by_each_category = TRUE),
			Col_num = ft_numeric(),
			Col_dt = ft_datetime(),
			Col_dt2 = ft_datetime(includes_time = FALSE),
			Col_ft = ft_freetext(),
			Col_sim = ft_simple(),
			Col_ign = ft_ignore()
		),
		"fieldtypes"
	)
})

test_that("Invalid fieldtypes cannot be specified", {
	expect_error(fieldtypes(Col_bad = readr::col_character()), class = "invalid_fieldtypes")
	expect_error(fieldtypes(Col_bad = "hello"), class = "invalid_fieldtypes")
})

test_that("Duplicate column names in fieldtypes specification not allowed", {
	expect_error(fieldtypes(Col_dup = ft_timepoint(),
													Col_dup = ft_uniqueidentifier()),
							 class = "invalid_fieldtypes")

	expect_error(fieldtypes(Col_tp = ft_timepoint(),
													Col_dup = ft_uniqueidentifier(),
													Col_dup = ft_uniqueidentifier()),
							 class = "invalid_fieldtypes")
})

test_that("Must include one and only one timepoint field", {
	expect_error(fieldtypes(Col_dt = ft_datetime()),
							 class = "invalid_fieldtypes")

	expect_error(fieldtypes(Col_tp1 = ft_timepoint(),
													Col_tp2 = ft_timepoint()),
							 class = "invalid_fieldtypes")
})

test_that("Fieldtype colnames cannot include reserved words", {
	expect_error(fieldtypes("[DUPLICATES]" = ft_timepoint()),
							 class = "invalid_fieldtypes")

	expect_error(fieldtypes(Col_tp = ft_timepoint(),
													"[DUPLICATES]" = ft_simple()),
							 class = "invalid_fieldtypes")

	expect_error(fieldtypes("[ALLFIELDSCOMBINED]" = ft_timepoint()),
							 class = "invalid_fieldtypes")
})


test_that("print_fieldtypes_template() params are present and of correct type", {
	expect_error(print_fieldtypes_template(),
							 class = "invalid_param_missing")

	expect_error(print_fieldtypes_template(df = c("Fieldname", 123)),
							 class = "invalid_param_type")

	expect_error(print_fieldtypes_template(df = data.frame("Fieldname" = 123),
														default_fieldtype = TRUE),
							 class = "invalid_param_type")
})

test_that("print_fieldtypes_template() generates template fieldtypes output", {
	expect_snapshot_output(print_fieldtypes_template(df = data.frame("col1" = 123,
																														 "col2" = 123,
																														 "col3" = "hello")))
})

test_that("fieldtypes object prints to console ok", {
	testfieldtypes <- fieldtypes(
		Col_tp = ft_timepoint(),
		Col_uid = ft_uniqueidentifier(),
		# NOTE: Partitionfield functionality disabled until we work out how to present it
		#Col_part = ft_partition(),
		Col_cat = ft_categorical(),
		Col_cat2 = ft_categorical(aggregate_by_each_category = TRUE),
		Col_num = ft_numeric(),
		Col_dt = ft_datetime(),
		Col_dt2 = ft_datetime(includes_time = FALSE),
		Col_ft = ft_freetext(),
		Col_sim = ft_simple(),
		Col_ign = ft_ignore()
	)

	expect_snapshot_output(print(testfieldtypes))
})
