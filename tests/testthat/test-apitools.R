context("rest_api_tools: Core API set up and endpoint configuration functions")

# test default url generation

.setup_api_endpoint('testend1', 'controller1')

test_base_uri <- 'http://sample.org'

test_that("Well formed URIs with default builder and no params", {
	expect_equal(.build_uri('testend1', api_base = test_base_uri), 'http://sample.org/controller1')
})

test_that("Well formed URIs with default builder with params as list", {
	q<-list(p1 = 'one', p2 = 'two')
	expect_equal(.build_uri('testend1', api_base = test_base_uri, query = q), 'http://sample.org/controller1?p1=one&p2=two')
})

test_that("Well formed URIs with default builder and params as ecliptic parameters", {
	expect_equal(.build_uri('testend1', api_base = test_base_uri, p1 = 'one', p2 = 'two'), 'http://sample.org/controller1?p1=one&p2=two')
})

# tests on api_base config

test_that("Error if no config for an endpoint", {
	expect_error(.build_uri('testend_nonexistent'))
})

test_that("Globally setted api_base leads to correct URI", {

	.setup_api_endpoint('testend2', 'controller2')
	.package_cache_delete('api_base')
	.set_api_base('http://sample_global.org')

	expect_equal(.build_uri('testend2'), 'http://sample_global.org/controller2')
})

# test on compulsory query string params

test_that("Error if a an expected query string param is missing", {

	.setup_api_endpoint('testend3', 'controller3', query_params = list('mustbe'))

	expect_error(.build_uri('testend3'))
})


test_that("Error if uri_builder not a not a function", {
	# some cases
	expect_error(.setup_api_endpoint('testend4', 'controller4', uri_builder = NULL))
	expect_error(.setup_api_endpoint('testend4', 'controller4', uri_builder = "wrongBuilder"))
	expect_error(.setup_api_endpoint('testend4', 'controller4', uri_builder = list("a")))
})


# We are adding this set up here because the tests in this file may modify the runtime 
# configuration of the package, which is a problem when developing 
.pbdb_setup()