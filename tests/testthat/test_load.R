context("loading data")
library(ehrchangepoints)


test_that("Creation of fieldtypes object", {
	expect_is(fieldtypes(Col1 = ft_timepoint()
											 ,Col2 = ft_uniqueidentifier()
											 ,Col3 = ft_partition()
											 ,Col4 = ft_categorical()
											 ,Col5 = ft_categorical(aggregate_by_each_category = TRUE)
											 ,Col6 = ft_number()
											 ,Col7 = ft_datetime()
											 ,Col8 = ft_freetext()
											 ,Col9 = ft_simple()
											 ,Col10 = ft_ignore())
						, "fieldtypes")
})
