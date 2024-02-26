test_that("pbdb_opinion() returns a data frame with an id column", {
  skip_if_offline("paleobiodb.org")
  response <- pbdb_opinion(id = 1003, vocab = "pbdb")
  expect_s3_class(response, "data.frame")
  expect_true("opinion_no" %in% names(response))
  expect_equal(nrow(response), 1)
})

test_that("pbdb_opinions() returns a data frame with an id column", {
  skip_if_offline("paleobiodb.org")
  response <- pbdb_opinions(id = c(10, 11), vocab = "pbdb")
  expect_s3_class(response, "data.frame")
  expect_true("opinion_no" %in% names(response))
  expect_equal(nrow(response), 2)
})

test_that("pbdb_opinions_taxa() returns a data frame with an id column", {
  skip_if_offline("paleobiodb.org")
  response <- pbdb_opinions_taxa(name = "Felidae")
  expect_s3_class(response, "data.frame")
  expect_true("oid" %in% names(response))
  expect_equal(substr(response$oid[1], 1, 3), "opn")
})
