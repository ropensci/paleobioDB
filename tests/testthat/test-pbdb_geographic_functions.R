data <- readRDS(test_path("fixtures", "canidae_quat_df.rds"))
data_nc <- readRDS(test_path("fixtures", "nocoord.rds"))

test_that("pbdb_map() throws an error if coordinates are missing", {
  expect_error(
    pbdb_map(data_nc),
    regexp = "Use the argument 'show = \"coords\"'"
  )
})

test_that("pbdb_map() returns a data.frame with the number of occurrences", {
  df <- pbdb_map(data, do_plot = FALSE)
  expect_s3_class(df, "data.frame")
  expect_identical(names(df), c("lng", "lat", "Occur"))
  expect_equal(max(df$Occur), 11)
})

test_that("pbdb_map_occur() throws an error if coordinates are missing", {
  expect_error(
    pbdb_map_occur(data_nc),
    regexp = "Please, add 'show = c\\(\"coords\"\\)"
  )
})

test_that("pbdb_map_occur() returns a SpatRaster object", {
  r <- pbdb_map_occur(data, do_plot = FALSE)
  expect_s4_class(r, "SpatRaster")
})

test_that("pbdb_map_richness() throws an error if coordinates are missing", {
  expect_error(
    pbdb_map_richness(data_nc, do_plot = FALSE),
    regexp = "use the following argument:"
  )
})

test_that("pbdb_map_richness() returns a SpatRaster object", {
  r <- pbdb_map_richness(data, do_plot = FALSE)
  expect_s4_class(r, "SpatRaster")
})
