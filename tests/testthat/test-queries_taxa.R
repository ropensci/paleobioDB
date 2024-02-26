test_that("pbdb_taxon() returns a data frame with an id column", {
  skip_if_offline("paleobiodb.org")
  response <- pbdb_taxon(name = "Canis", vocab = "pbdb")
  expect_s3_class(response, "data.frame")
  expect_true(all(c("orig_no", "taxon_no") %in% names(response)))
  expect_equal(nrow(response), 1)
})

test_that("pbdb_taxa() returns a data frame with an id column", {
  skip_if_offline("paleobiodb.org")
  response <- pbdb_taxa(name = "Chordata", rel = "all_parents")
  expect_s3_class(response, "data.frame")
  expect_true("oid" %in% names(response))
  expect_gt(nrow(response), 1)
})

test_that("pbdb_taxa_auto() returns a data frame with an id column", {
  skip_if_offline("paleobiodb.org")
  response <- pbdb_taxa_auto(name = "cani")
  expect_s3_class(response, "data.frame")
  expect_true("oid" %in% names(response))
  expect_gt(nrow(response), 1)
})
