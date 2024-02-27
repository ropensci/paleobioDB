data <- readRDS(test_path("fixtures", "canidae_quat_df.rds"))

test_that("pbdb_subtaxa() returns a data frame", {
  df <- pbdb_subtaxa(data, do_plot = FALSE)
  ns <- c("species", "genera", "families", "orders", "classes", "phyla")
  expect_s3_class(df, "data.frame")
  expect_identical(names(df), ns)
})
