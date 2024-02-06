# test for pbdb_subtaxa

context("pbdb_subtaxa")
test_that("pbdb_subtaxa output is a dataframe, and the names are characters", {
  skip_if_offline("paleobiodb.org")
  data<-  pbdb_occurrences (limit="100", vocab="pbdb",
                            base_name="Canidae", show=c('classext', 'ident') )
  
  response<- pbdb_subtaxa (data, do.plot=F)
  expect_true(is.data.frame (response))
  expect_is (names (response)[1], "character")
  expect_true (dim (response)[1]==1)
})
