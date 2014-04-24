# test for pbdb_queries

context("pbdb_occurrence")
test_that("pbdb_occurrence output is a dataframe, and the names are characters", {
  response<- pbdb_occurrence (id=1001)
  expect_true(is.data.frame (response))
  expect_is (names (response)[1], "character")
  expect_true (dim (response)[1]==1)
})


context("pbdb_occurrences")
test_that("pbdb_occurrences output is a dataframe, and the names are characters", {
  response<- pbdb_occurrences (id=c(10, 11)) 
  expect_true(is.data.frame (response))
  expect_is (names (response)[1], "character")
  expect_true (dim (response)[1]>=1)
})

context("pbdb_collection")
test_that("pbdb_collection output is a dataframe, and the names are characters", {
  response<- pbdb_collection (id=1003, vocab="pbdb")
  expect_true(is.data.frame (response))
  expect_is (names (response)[1], "character")
  expect_true (dim (response)[1]==1)
})

context("pbdb_collections")
test_that("pbdb_collections output is a dataframe, and the names are characters", {
  response<- pbdb_collections (id=c(10, 11)) 
  expect_true(is.data.frame (response))
  expect_is (names (response)[1], "character")
  expect_true (dim (response)[1]>=1)
})

context("pbdb_collections_geo")
test_that("pbdb_collections_geo output is a dataframe, and the names are characters", {
  response<- pbdb_collections_geo (vocab="pbdb", lngmin=0.0, lngmax=15.0, latmin=0.0, latmax=15.0, level=2)
  expect_true(is.data.frame (response))
  expect_is (names (response)[1], "character")
  expect_true (dim (response)[1]>=1)
})

context("pbdb_taxon")
test_that("pbdb_taxon output is a dataframe, and the names are characters", {
  response<- pbdb_taxon (name="Canis", vocab="pbdb")
  expect_true(is.data.frame (response))
  expect_is (names (response)[1], "character")
  expect_true (dim (response)[1]==1)
})

#context("pbdb_taxa")
#test_that("pbdb_taxa output is a dataframe, and the names are characters", {
#  response<- pbdb_taxa (name="Canidae")
#  expect_true(is.data.frame (response))
#  expect_is (names (response)[1], "character")
#  expect_true (dim (response)[1]==1)
#})

#context("pbdb_taxa")
#test_that("pbdb_taxa output is a dataframe, and the names are characters", {
#  response<- pbdb_taxa_auto (name="Canis", limit=10)
#  expect_true(is.data.frame (response))
#  expect_is (names (response)[1], "character")
#  expect_true (dim (response)[1]>=1)
#})

context("pbdb_interval")
test_that("pbdb_interval output is a dataframe, and the names are characters", {
  response<- pbdb_interval (id=1)
  expect_true(is.data.frame (response))
  expect_is (names (response)[1], "character")
  expect_true (dim (response)[1]==1)
})

context("pbdb_intervals")
test_that("pbdb_intervals output is a dataframe, and the names are characters", {
  response<- pbdb_intervals (min_ma= 0, max_ma=2)
  expect_true(is.data.frame (response))
  expect_is (names (response)[1], "character")
  expect_true (dim (response)[1]>=1)
})

context("pbdb_scale")
test_that("pbdb_scale output is a dataframe, and the names are characters", {
  response<- pbdb_scale (id=1)
  expect_true(is.data.frame (response))
  expect_is (names (response)[1], "character")
  expect_true (dim (response)[1]==1)
})

context("pbdb_scales")
test_that("pbdb_scales output is a dataframe, and the names are characters", {
  response<- pbdb_scales ()
  expect_true(is.data.frame (response))
  expect_is (names (response)[1], "character")
  expect_true (dim (response)[1]>=1)
})

context("pbdb_strata")
test_that("pbdb_scales output is a dataframe, and the names are characters", {
  response<- pbdb_strata (lngmin=0, lngmax=15, latmin=0, latmax=5, rank="formation")
  expect_true(is.data.frame (response))
  expect_is (names (response)[1], "character")
  expect_true (dim (response)[1]>=1)
})

context("pbdb_strata_auto")
test_that("pbdb_strata_auto output is a dataframe, and the names are characters", {
  response<- pbdb_strata_auto (name= "Pin")
  expect_true(is.data.frame (response))
  expect_is (names (response)[1], "character")
  expect_true (dim (response)[1]>=1)
})

context("pbdb_reference")
test_that("pbdb_reference output is a dataframe, and the names are characters", {
  response<- pbdb_reference (id=360)
  expect_true(is.data.frame (response))
  expect_is (names (response)[1], "character")
  expect_true (dim (response)[1]==1)
})

#  context("pbdb_references")
# test_that("pbdb_references output is a dataframe, and the names are characters", {
#   response<- pbdb_references (author="Turner")
#   expect_true(is.data.frame (response))
#   expect_is (names (response)[1], "character")
#   expect_true (dim (response)[1]>=1)
# })

context("pbdb_ref_occurrences")
test_that("pbdb_ref_occurrences output is a dataframe, and the names are characters", {
  response<- pbdb_ref_occurrences (taxon_name="Canis", year=2000)
  expect_true(is.data.frame (response))
  expect_is (names (response)[1], "character")
  expect_true (dim (response)[1]>=1)
})

context("pbdb_ref_collections")
test_that("pbdb_ref_collections output is a dataframe, and the names are characters", {
  response<- pbdb_ref_collections (id=1)
  expect_true(is.data.frame (response))
  expect_is (names (response)[1], "character")
  expect_true (dim (response)[1]>=1)
})


context("pbdb_ref_taxa")
test_that("pbdb_ref_taxa output is a dataframe, and the names are characters", {
  response<- pbdb_ref_taxa (name="Felidae")
  expect_true(is.data.frame (response))
  expect_is (names (response)[1], "character")
  expect_true (dim (response)[1]>=1)
})

