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

test_that("prepare_data() checks that at least one valid timepoint value is present", {
	expect_error(prepare_data(df = data.frame(col1 = rep("01/01/2022", 5), col2 = rep(1, 5), col3 = 1:5),
														fieldtypes = fieldtypes(col1 = ft_timepoint(),
																										col2 = ft_simple(),
																										col3 = ft_ignore()),
														showprogress = FALSE),
							 class = "invalid_param_type")

})


test_that("prepare_data() creates sourcedata object correctly", {
	testdf <- read_data(test_path("testdata", "completetestset.csv"))
	testsourcedata <- prepare_data(testdf,
																 fieldtypes = fieldtypes( col_timepoint_err = ft_ignore(),
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
																													col_simple = ft_simple()),
																 dataset_shortdesc = "completetestset",
																 showprogress = FALSE
																 )

	expect_s3_class(testsourcedata, "sourcedata")

	expect_equal(testsourcedata$timepoint_fieldname, "col_timepoint")
	expect_equal(testsourcedata$timepoint_missing_n, 6)
	expect_equal(testsourcedata$rows_source_n, 900)
	expect_equal(testsourcedata$rows_imported_n, 890)
	expect_equal(testsourcedata$rows_duplicates_n, 4)
	expect_equal(testsourcedata$cols_source_n, 24)
	expect_equal(testsourcedata$cols_imported_n, 12)
	expect_equal(testsourcedata$dataset_shortdesc, "completetestset")

	expect_snapshot(testsourcedata$validation_warnings)

})

test_that("prepare_data() ignores nonchar columns (since readr::type_convert fails to skip nonchar cols)", {
	testsourcedata <- prepare_data(df = data.frame(col1 = rep("2022-01-01", 5), col2 = rep(1, 5), col3 = 1:5),
														fieldtypes = fieldtypes(col1 = ft_timepoint(),
																										col2 = ft_simple(),
																										col3 = ft_ignore()),
														dataset_shortdesc = "nonchar columns",
														showprogress = FALSE)

	expect_equal(testsourcedata$cols_imported_n, 2)
})

test_that("prepare_data() gets dataset_shortdesc from call if NULL (default) passed in", {
	dfobj <- data.frame(col1 = rep("2022-01-01", 5), col2 = rep(1, 5), col3 = 1:5)
	testsourcedata <- prepare_data(df = dfobj,
																 fieldtypes = fieldtypes(col1 = ft_timepoint(),
																 												col2 = ft_simple(),
																 												col3 = ft_ignore()),
																 showprogress = FALSE)

	expect_equal(testsourcedata$dataset_shortdesc, "dfobj")

	testsourcedata <- prepare_data(df = data.frame(col1 = rep("2022-01-01", 5), col2 = rep(1, 5), col3 = 1:5),
																 fieldtypes = fieldtypes(col1 = ft_timepoint(),
																 												col2 = ft_simple(),
																 												col3 = ft_ignore()),
																 showprogress = FALSE)

	expect_equal(testsourcedata$dataset_shortdesc,
							 "data.frame(col1 = rep(\"2022-01-01\", 5), col2 = rep(1, 5), col3 = 1:5)")
})


test_that("sourcedata object prints to console ok", {
	testdf <- read_data(test_path("testdata", "completetestset.csv"))
	testsourcedata <- prepare_data(testdf,
																 fieldtypes = fieldtypes( col_timepoint_err = ft_ignore(),
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
																													col_simple = ft_simple()),
																 dataset_shortdesc = "completetestset",
																 showprogress = FALSE
																 )

	expect_snapshot_output(print(testsourcedata))
})

test_that("remove_rows() removes specified rows", {
	numrows <- 2000000
	testdt <- data.table::data.table(col1 = rep("2022-01-01", numrows), col2 = seq(1, numrows), col3 = rep("XXXXXXXXXXX", numrows))
	#object.size(testdt)
	rowstoremove <- c(1,4,5,6,numrows)
	rowindicator <- rep(FALSE, numrows)
	rowindicator[rowstoremove] <- TRUE
	postdt <- remove_rows(data.table::copy(testdt), rowindicator = rowindicator)

	# correct no. of rows removed
	expect_equal(nrow(postdt), numrows - length(rowstoremove))
	# correct rows removed
	expect_false(any(postdt$col2 %in% rowstoremove))
	# all columns still present
	expect_equal(names(testdt), names(postdt))
	# the contents of the first non-deleted row is the same before and after
	expect_equal(unlist(testdt[which(!rowindicator)[1]]), unlist(postdt[1]))

})

test_that("identify_duplicaterows() identifies all exactly duplicated rows", {
	# unbatched
	numrows <- 200000
	testdt <- data.table::data.table(col1 = rep("2022-01-01", numrows), col2 = seq(1, numrows), col3 = rep("XXXXXXXXXXX", numrows))
#	object.size(testdt)
	duplicaterows <- c(2,5,6,7,numrows)
	duplicatevals <- c(1,4,4,4,numrows - 1)
	testdt$col2[duplicaterows] <- duplicatevals

	result <- identify_duplicaterows(testdt, "col2", showprogress = FALSE)

	expect_true(all(result[duplicaterows]))
	expect_true(all(!result[-duplicaterows]))

	# batched
	numrows <- 20000000
	testdt <- data.table::data.table(col1 = rep("2022-01-01", numrows), col2 = seq(1, numrows), col3 = rep("XXXXXXXXXXX", numrows))
#	object.size(testdt)
	duplicaterows <- c(2,5,6,7,numrows)
	duplicatevals <- c(1,4,4,4,numrows - 1)
	testdt$col2[duplicaterows] <- duplicatevals

	result <- identify_duplicaterows(testdt, "col2", showprogress = FALSE)

	expect_true(all(result[duplicaterows]))
	expect_true(all(!result[-duplicaterows]))

})

