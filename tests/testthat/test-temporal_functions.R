# test for the temporal functions


data<-  pbdb_occurrences (limit="100", vocab="pbdb",
                          base_name="Canidae",  interval="Quaternary", 
                          show=c("phylo", "ident"))

context("pbdb_temporal_resolution")
test_that("pbdb_temporal_resolution output is a dataframe, and the names are characters", {
 
  response<- pbdb_temporal_resolution (data, do.plot=F)
  expect_true(is.list (response))
  expect_is (names (response)[1], "character")
  expect_true (length (response)==2)
})

context("pbdb_temp_range")
test_that("pbdb_temp_range output is a dataframe, and the names are characters", {

  response<-  pbdb_temp_range (data, rank="species", do.plot=F)
  expect_true(is.data.frame (response))
  expect_is (names (response)[1], "character")
  expect_gt(nrow(response), 0)
})


context("pbdb_richness")
test_that("pbdb_richness output is a dataframe, and the names are characters", {
  response<-  pbdb_richness (data, 
                             rank="family", 
                             res=1, 
                             temporal_extent=c(0,3), do.plot=F)
  expect_true(is.data.frame (response))
  expect_is (names (response)[1], "character")
  expect_gt (nrow(response), 0)
})

context("pbdb_orig_ext")
test_that("pbdb_orig_ext output is a dataframe, and the names are characters", {
  response<-  pbdb_orig_ext (data, 
                             rank="family", 
                             , do.plot=F,temporal_extent=c(0,10))
  expect_true(is.data.frame (response))
  expect_is (names (response)[1], "character")
  expect_gt(nrow(response), 0)
 
})

