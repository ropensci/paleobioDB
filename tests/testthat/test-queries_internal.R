test_that("list is converted to comma separated string", {
  expect_equal(.implode_to_string(list("aa", "bb")), "aa,bb")
})

test_that("character vector is converted to comma separated string", {
  expect_equal(.implode_to_string(c("aa", "bb")), "aa,bb")
})

test_that("character vectors of length one are not modified", {
  expect_equal(.implode_to_string("aa"), "aa")
})
