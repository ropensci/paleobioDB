test_that("pbdb_strata() returns a data frame with time ranges", {
  skip_if_offline("paleobiodb.org")
  response <- pbdb_strata(
    lngmin = 0, lngmax = 15, latmin = 0, latmax = 5, rank = "formation"
  )
  expect_s3_class(response, "data.frame")
  expect_true(all(c("eag", "lag") %in% names(response)))
  expect_gt(nrow(response), 1)
})

test_that("pbdb_strata_auto() returns a data frame with strata names", {
  skip_if_offline("paleobiodb.org")
  response <- pbdb_strata_auto(name = "Pin")
  expect_s3_class(response, "data.frame")
  expect_true("nam" %in% names(response))
  expect_gt(nrow(response), 1)
})
