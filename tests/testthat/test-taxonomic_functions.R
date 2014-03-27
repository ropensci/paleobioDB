# test for pbdb_subtaxa

context("pbdb_subtaxa")
test_that("pbdb_subtaxa output is a dataframe, and the names are characters", {
  data<-  pbdb_occurrences (limit="100", vocab="pbdb",
                            base_name="Canidae")
  
  response<- pbdb_subtaxa (data)
  expect_true(is.data.frame (response))
  expect_is (names (response)[1], "character")
  expect_true (dim (response)[1]==1)
})
