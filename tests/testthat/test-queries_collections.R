test_that("pbdb_collection() returns a data frame with an id column", {
  skip_if_offline("paleobiodb.org")
  response <- pbdb_collection(id = 1003, vocab = "pbdb")
  expect_s3_class(response, "data.frame")
  expect_true("collection_no" %in% names(response))
  expect_equal(nrow(response), 1)
})

test_that("pbdb_collections() returns a data frame with an id column", {
  skip_if_offline("paleobiodb.org")
  response <- pbdb_collections(coll_id = c(10, 11), vocab = "pbdb")
  expect_s3_class(response, "data.frame")
  expect_true("collection_no" %in% names(response))
  expect_equal(nrow(response), 2)
})

test_that("pbdb_collections_geo() returns a data frame with an id column", {
  skip_if_offline("paleobiodb.org")
  response <- pbdb_collections_geo(
    lngmin = 0.0, lngmax = 15.0, latmin = 0.0, latmax = 15.0,
    level = 2,
    vocab = "pbdb"
  )
  expect_s3_class(response, "data.frame")
  expect_true("bin_id" %in% names(response))
})
