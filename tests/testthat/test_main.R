test_that("Test read_data() on csv file created using Excel", {
	# read file correctly
	rawdata <- read_data(
	  test_path("testdata", "specialchars_excel.csv"),
	  delim = ",",
	  col_names = TRUE
	  )
	expect_equal(nrow(rawdata), 8)
	expect_equal(length(rawdata), 4)

	# read file assuming no column names
	rawdata <- read_data(
	  test_path("testdata", "specialchars_excel.csv"),
	  delim = ",",
	  col_names = FALSE
	  )
	expect_equal(nrow(rawdata), 9)
	expect_equal(length(rawdata), 4)

	# read file without acknowledging quotes
	rawdata <- read_data(
	  test_path("testdata", "specialchars_excel.csv"),
	  delim = ",",
	  quote = "",
	  col_names = TRUE
	  )
	expect_warning(as.numeric(rawdata$numeric_good), "NAs introduced by coercion")

})

