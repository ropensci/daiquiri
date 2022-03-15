test_that("Valid fieldtypes can be specified", {
	expect_s3_class(fieldtypes(Col_tp = ft_timepoint()
											 ,Col_uid = ft_uniqueidentifier()
											 # NOTE: Partitionfield functionality disabled until we work out how to present it
											 #,Col_part = ft_partition()
											 ,Col_cat = ft_categorical()
											 ,Col_cat2 = ft_categorical(aggregate_by_each_category = TRUE)
											 ,Col_num = ft_numeric()
											 ,Col_dt = ft_datetime()
											 ,Col_dt2 = ft_datetime(includes_time = FALSE)
											 ,Col_ft = ft_freetext()
											 ,Col_sim = ft_simple()
											 ,Col_ign = ft_ignore())
						, "fieldtypes")
})

test_that("Invalid fieldtypes cannot be specified", {
	expect_error(fieldtypes(Col_bad = readr::col_character()), class = "invalid_fieldtypes")
	expect_error(fieldtypes(Col_bad = "hello"), class = "invalid_fieldtypes")
})

test_that("Duplicate column names in fieldtypes specification not allowed", {
	expect_error(fieldtypes(Col_dup = ft_timepoint()
													,Col_dup = ft_uniqueidentifier(), class = "invalid_fieldtypes")
	)
	expect_error(fieldtypes(Col_tp = ft_timepoint()
													,Col_dup = ft_uniqueidentifier()
													,Col_dup = ft_uniqueidentifier(), class = "invalid_fieldtypes")
	)
})

test_that("Must include one and only one timepoint field", {
	expect_error(fieldtypes(Col_dt = ft_datetime()), class = "invalid_fieldtypes")
	expect_error(fieldtypes(Col_tp1 = ft_timepoint()
													,Col_tp2 = ft_timepoint()), class = "invalid_fieldtypes")
})


