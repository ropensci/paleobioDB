# test for pbdb_subtaxa in pbdb

context ("pbdb_subtaxa")

test_that("pbdb_subtaxa output is a dataframe",{ 

	data<- read.table ("../../data/canidae_quat.csv", sep=",", header=T)
	response<- pbdb_subtaxa(data)
	expect_is(response, 'data.frame')
})

