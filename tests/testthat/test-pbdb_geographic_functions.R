test_that("pbdb_map_occur() throws an error if coordinates are missing", {
  data <- pbdb_occurrences(limit = 100, base_name = "Canis")
  expect_error(
    pbdb_map_occur(data),
    regexp = "Please, add 'show = c\\(\"coords\"\\)"
  )
})

test_that("pbdb_map_richness() throws an error if coordinates are missing", {
  data <- pbdb_occurrences(limit = 100, base_name = "Canis")
  expect_error(
    pbdb_map_richness(data),
    regexp = "use the following argument:"
  )
})
