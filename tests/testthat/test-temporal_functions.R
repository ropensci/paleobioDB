# test for the temporal functions

context("pbdb_temporal_resolution")
test_that("pbdb_temporal_resolution output is a dataframe, and the names are characters", {
  data<-  pbdb_occurrences (limit="100", vocab="pbdb",
                            base_name="Canidae",  interval="Quaternary")
  response<- pbdb_temporal_resolution (data, do.plot=F)
  expect_true(is.list (response))
  expect_is (names (response)[1], "character")
  expect_true (length (response)==2)
})

context(" pbdb_time_spam")
test_that(" pbdb_time_spam output is a dataframe, and the names are characters", {
  data<-  pbdb_occurrences (limit="100", vocab="pbdb",
                            base_name="Canidae",  interval="Quaternary")
  response<-  pbdb_time_spam (data, rank="species", do.plot=F)
  expect_true(is.data.frame (response))
  expect_is (names (response)[1], "character")
  expect_true (dim (response)[1]>=1)
})

