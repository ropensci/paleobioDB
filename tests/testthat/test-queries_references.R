test_that("pbdb_reference() returns a data frame with an id column", {
  skip_if_offline("paleobiodb.org")
  response <- pbdb_reference(id = 360, vocab = "pbdb")
  expect_s3_class(response, "data.frame")
  expect_true("reference_no" %in% names(response))
  expect_equal(nrow(response), 1)
})

test_that("pbdb_references() returns a data frame with an id column", {
  skip_if_offline("paleobiodb.org")
  response <- pbdb_references(ref_author = "Turner", vocab = "pbdb")
  expect_s3_class(response, "data.frame")
  expect_true("reference_no" %in% names(response))
  expect_gt(nrow(response), 1)
})

test_that("pbdb_ref_occurrences() returns a data frame with an id column", {
  skip_if_offline("paleobiodb.org")
  response <- pbdb_ref_occurrences(
    base_name = "Canis", ref_pubyr = 2000, vocab = "pbdb"
  )
  expect_s3_class(response, "data.frame")
  expect_true("reference_no" %in% names(response))
  expect_gt(nrow(response), 1)
})

test_that("pbdb_ref_collections() returns a data frame with an id column", {
  skip_if_offline("paleobiodb.org")
  response <- pbdb_ref_collections(id = 1, vocab = "pbdb")
  expect_s3_class(response, "data.frame")
  expect_true("reference_no" %in% names(response))
})

test_that("pbdb_ref_taxa() returns a data frame with an id column", {
  skip_if_offline("paleobiodb.org")
  response <- pbdb_ref_taxa(name = "Felidae")
  expect_s3_class(response, "data.frame")
  expect_true("oid" %in% names(response))
})
