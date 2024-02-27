data <- readRDS(test_path("fixtures", "canidae_quat_df.rds"))

test_that("pbdb_temporal_resolution() returns a list with a summary", {
  tr <- pbdb_temporal_resolution(data, do_plot = FALSE)
  expect_type(tr, "list")
  expect_true(all(c("summary", "temporal_resolution") %in% names(tr)))
})

test_that("pbdb_temporal_resolution() throws errors", {
  data_incomplete <- data
  data_incomplete$min_ma <- NULL
  expect_error(
    pbdb_temporal_resolution(data_incomplete, do_plot = FALSE),
    regexp = "No temporal information found"
  )
})

test_that("pbdb_temp_range() returns a data frame", {
  df <- pbdb_temp_range(data, do_plot = FALSE)
  expect_s3_class(df, "data.frame")
  expect_identical(names(df), c("max", "min"))
})

test_that("pbdb_richness() returns a data frame", {
  df <- pbdb_richness(data, do_plot = FALSE)
  expect_s3_class(df, "data.frame")
  expect_identical(names(df), c("temporal_intervals", "richness"))
})

test_that("pbdb_orig_ext() returns a data frame", {
  df <- pbdb_orig_ext(
    data,
    temporal_extent = c(0, 3),
    res = 0.5,
    do_plot = FALSE
  )
  expect_s3_class(df, "data.frame")
  expect_identical(names(df), c("new", "ext"))
})
