test_that("pbdb_interval() returns a data frame with an id column", {
  skip_if_offline("paleobiodb.org")
  response <- pbdb_interval(id = 1, vocab = "pbdb")
  expect_s3_class(response, "data.frame")
  expect_true("interval_no" %in% names(response))
  expect_equal(nrow(response), 1)
})

test_that("pbdb_intervals() returns a data frame with an id column", {
  skip_if_offline("paleobiodb.org")
  response <- pbdb_intervals(min_ma = 0, max_ma = 2, vocab = "pbdb")
  expect_s3_class(response, "data.frame")
  expect_true("interval_no" %in% names(response))
  expect_gt(nrow(response), 1)
})

test_that("pbdb_scale() returns a data frame with an id column", {
  skip_if_offline("paleobiodb.org")
  response <- pbdb_scale(id = 1, vocab = "pbdb")
  expect_s3_class(response, "data.frame")
  expect_true("scale_no" %in% names(response))
  expect_equal(nrow(response), 1)
})

test_that("multivalue elements are converted to a string with semicolons", {
  skip_if_offline("paleobiodb.org")
  response <- pbdb_scales()
  expect_true("lvs" %in% names(response))
  expect_true(all(grepl(";", response$lvs)))
})
