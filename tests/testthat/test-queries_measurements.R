test_that("pbdb_specimen() returns a data frame with an id column", {
  skip_if_offline("paleobiodb.org")
  response <- pbdb_specimen(id = 1003, vocab = "pbdb")
  expect_s3_class(response, "data.frame")
  expect_true("specimen_no" %in% names(response))
  expect_equal(nrow(response), 1)
})

test_that("pbdb_specimens() returns a data frame with an id column", {
  skip_if_offline("paleobiodb.org")
  response <- pbdb_specimens(
    base_name = "Cetacea",
    interval = "Miocene",
    vocab = "pbdb"
  )
  expect_s3_class(response, "data.frame")
  expect_true("specimen_no" %in% names(response))
  expect_gt(nrow(response), 1)
})

test_that("pbdb_ref_specimens() returns a data frame with an id column", {
  skip_if_offline("paleobiodb.org")
  response <- pbdb_ref_specimens(spec_id = c(1, 358))
  expect_s3_class(response, "data.frame")
  expect_true("oid" %in% names(response))
  expect_equal(substr(response$oid[1], 1, 3), "ref")
})

test_that("pbdb_measurements() returns a data frame with an id column", {
  skip_if_offline("paleobiodb.org")
  response <- pbdb_measurements(spec_id = c(1, 358))
  expect_s3_class(response, "data.frame")
  expect_true("oid" %in% names(response))
  expect_equal(substr(response$oid[1], 1, 3), "mea")
})
