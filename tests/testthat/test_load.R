context("loading data")
library(daiquiri)

## TEST THAT FIELDTYPES SPECIFICATION AND COLUMNNAMES ARE VALID ##

test_that("Valid fieldtypes can be specified", {
	expect_is(fieldtypes(Col_tp = ft_timepoint()
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

test_that("Duplicate column names in fieldtypes specification not allowed", {
	expect_error(fieldtypes(Col_tp = ft_timepoint()
													,Col_tp = ft_uniqueidentifier())
	)
})

test_that("Invalid fieldtypes cannot be specified", {
	expect_error(fieldtypes(Col_bad = readr::col_character()))
})

test_that("Must include a timepoint field", {
	expect_error(fieldtypes(Col_dt = ft_datetime()))
})

test_that("More than one timepoint field not allowed", {
	expect_error(fieldtypes(Col_tp1 = ft_timepoint()
													,Col_tp2 = ft_timepoint()))
})


test_that("Text files must have override_columnnames set to TRUE if textfile_contains_columnnames is set to FALSE", {
	expect_error(load_dataset(".\\devtesting\\testdata\\antibiotics_example.csv", fieldtypes = fieldtypes(Col_tp = ft_timepoint()), textfile_contains_columnnames = FALSE, override_columnnames = FALSE, na = NULL, showprogress = FALSE))
})

test_that("Column names in data and fieldtypes match exactly", {
	expect_silent(validate_columnnames(c("nonsense","set","of")
																		 ,c("nonsense","set","of"), check_length_only = FALSE))
})

test_that("Column names in data and fieldtypes match in different order", {
	expect_silent(validate_columnnames(c("nonsense","set","of")
																		 ,c("nonsense","of","set"), check_length_only = FALSE))
})

test_that("Column names in data and fieldtypes match in length only", {
	expect_silent(validate_columnnames(c("nonsense","set")
																		 ,c("nonsense","names"), check_length_only = TRUE))
})

test_that("Column names in data and fieldtypes don't match in length only", {
	expect_error(validate_columnnames(c("nonsense","set","of")
																		,c("nonsense","set"), check_length_only = TRUE))
})

test_that("Duplicate column names in data not allowed", {
	expect_error(validate_columnnames(c("nonsense","set","of","nonsense","names")
																		,c("nonsense","set","of","names"), check_length_only = FALSE))
})

test_that("Column names in data not in fieldtypes not allowed", {
	expect_error(validate_columnnames(c("nonsense","set","of","stuff","names")
																		,c("nonsense","set","of","stuff"), check_length_only = FALSE))
})

test_that("Column names in fieldtypes not in data not allowed", {
	expect_error(validate_columnnames(c("nonsense","set","of")
																		,c("nonsense","set","of","stuff"), check_length_only = FALSE))
})


## TEST THAT DATA TYPE TO LOAD IS VALID ##

test_that("Invalid filename supplied", {
	expect_error(load_dataset("fakedatafile.csv", fieldtypes = fieldtypes(Col_tp = ft_timepoint()), textfile_contains_columnnames = TRUE, override_columnnames = FALSE, na = NULL, showprogress = FALSE))
})

test_that("Non-csv files not allowed", {
	expect_error(load_dataset("./DESCRIPTION", fieldtypes = fieldtypes(Col_tp = ft_timepoint()), override_columnnames = FALSE, na = NULL, showprogress = FALSE))
})

test_that("Non-data frames not allowed", {
	expect_error(load_dataset(c("Fieldname", 123), fieldtypes = fieldtypes(Col_tp = ft_timepoint()), override_columnnames = FALSE, na = NULL, showprogress = FALSE))
})
