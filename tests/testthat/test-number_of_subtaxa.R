# test for number_of_subtaxa in pbdb

context ("number_of_subtaxa")
data<- read.table ("data/canidae_quat.csv", sep=",", header=T)
response<- number_of_subtaxa(data)

test_that("number_of_subtaxa output is a dataframe",{ 
  expect_is(response, 'data.frame')
})

