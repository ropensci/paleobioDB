# test for number_of_subtaxa in pbdb

context ("number_of_subtaxa")

test_that("number_of_subtaxa output is a dataframe",{ 
	
	data<- read.table ("../../data/canidae_quat.csv", sep=",", header=T)
	response<- pbdb_subtaxa(data)
	expect_is(response, 'data.frame')
})

