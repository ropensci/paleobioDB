test_that("warnings returned from the API are reported", {
  skip_if_offline("paleobiodb.org")
  expect_warning(
    response <- pbdb_occurrences(
      id = c(10, 11),
      vocab = "pbdb",
      show = c("class", "coodrs")
    ),
    regexp = "Your query to the PBDB API generated the following warnings:"
  )
  expect_s3_class(response, "data.frame")
})

test_that("nonexistent parameters produce an error", {
  skip_if_offline("paleobiodb.org")
  expect_error(
    pbdb_occurrences(id = c(10, 11), voacb = "pbdb"),
    regexp = "Error in API response"
  )
})

test_that("passing non-vectors to a query produces an error", {
  expect_error(
    pbdb_occurrence(id = 1, function() FALSE),
    regexp = "Vector expected"
  )
})

test_that("valid queries that return no records produce a warning", {
  skip_if_offline("paleobiodb.org")
  expect_warning(
    pbdb_ref_occurrences(vocab = "pbdb", base_name = "Canis", ref_pubyr = 800),
    regexp = "The PBDB API returned no records for this query."
  )
})

test_that("pbdb_collections_geo() requires a 'level' parameter", {
  expect_error(
    pbdb_collections_geo(
      lngmin = 0.0,
      lngmax = 15.0,
      latmin = 0.0,
      latmax = 15.0
    ),
    regexp = "Parameter \"level\" is required."
  )
})
