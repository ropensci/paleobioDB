test_that("pbdb_occurrence() returns a data frame with an id column", {
  skip_if_offline("paleobiodb.org")
  response <- pbdb_occurrence(id = 1001, vocab = "pbdb")
  expect_s3_class(response, "data.frame")
  expect_true("occurrence_no" %in% names(response))
  expect_equal(nrow(response), 1)
})

test_that("pbdb_occurrences() returns a data frame with an id column", {
  skip_if_offline("paleobiodb.org")
  response <- pbdb_occurrences(id = c(10, 11), vocab = "pbdb")
  expect_s3_class(response, "data.frame")
  expect_true("occurrence_no" %in% names(response))
})

test_that("latitude and longitude columns are returned as numeric", {
  skip_if_offline("paleobiodb.org")
  response <- pbdb_occurrences(id = c(10, 11), vocab = "pbdb", show = "coords")
  expect_true(all(c("lng", "lat") %in% names(response)))
  expect_type(response$lng, "double")
  expect_type(response$lat, "double")
})

test_that("limit parameter is respected", {
  skip_if_offline("paleobiodb.org")
  response <- pbdb_occurrences(base_name = "Canis", limit = 2)
  expect_equal(nrow(response), 2)
})
