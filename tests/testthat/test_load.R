context("loading data")
library(ehrchangepoints)


test_that("Creation of fieldtypes object", {
	expect_is(fieldtypes(Col1 = ft_timepoint()
											 ,Col2 = ft_uniqueidentifier()
											 ,Col3 = ft_source()
											 ,Col4 = ft_categorical()
											 ,Col5 = ft_number()
											 ,Col6 = ft_datetime()
											 ,Col7 = ft_freetext()
											 ,Col8 = ft_simple()
											 ,Col9 = ft_ignore())
						, "fieldtypes")
})
