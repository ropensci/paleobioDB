# test for the geographic functions


context("pbdb_map_occur")
test_that("tests on pbdb_map_occur", {
  skip_if_offline("paleobiodb.org")
  ##missing coordinates
  data<-  pbdb_occurrences (limit="100", vocab="pbdb",
                            base_name="Canis")
  expect_error(pbdb_map_occur (data))
})

context("pbdb_map_richness")
test_that("tests on pbdb_map_richness", {
  skip_if_offline("paleobiodb.org")
  ##missing coordinates
  data<-  pbdb_occurrences (limit="100", vocab="pbdb", base_name="canis")
  expect_error(pbdb_map_richness (data))
})
