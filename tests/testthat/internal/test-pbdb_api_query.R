context("rpbdb query functions and uri generation")

# uri generation

# .pbdb_setup()

# setup_api_endpoint
# test_that("pbdb_uri_builder works", { 

# 	expect_equal(.setup_api_endpoint('testendpoint', 'occs/test.%s', uri_builder = .pbdb_uri_builder)),
# })



# tests on pbdb raw responses, error handling ...

# error_json_response <- 

# test_that("An error response (fixture) from pbdb gives legible error warning", { 

# })

# 
# 

# Conversion of params that allow several comma-separated values

test_that("Param with several (as list) values joins to csv ok", { 
	expect_equal(.implode_to_string(list("aa","bb")), 'aa,bb')
})

test_that("Param with several (as character vector) values joins to csv ok", { 
	expect_equal(.implode_to_string(c("aa","bb")), 'aa,bb')
})

test_that("Param with one value remains", {
	expect_equal(.implode_to_string("aa"), "aa")
})



