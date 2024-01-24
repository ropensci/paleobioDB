# Functions abstracting common behaviours for different API enpoint configurations.
# The aim is to provide a framework for all the API calls in the package

# Configuration for the several api endpoints
# .api_end_points <- list()

#' .setup_api_endpoint
#'
#' Sets up configuration for an API endpoint
#' 
#' @param name Internal use name of the endpoint
#' @param endpoint_base Base of the endpoint. Should not include de api_base. 
#' eg: products in a http://sample.org/api/products where http://sample.org/api 
#' ould be the api_base
#' @param ... Custom parameters passed to the uri_builder callback when generating the URIs
#' @param uri_builder Function that generates the URIs to the API
#' @param query_params Query parameters (currently unused)
#' @noRd
.setup_api_endpoint<-function(name, endpoint_base, ..., uri_builder = .default_uri_builder, 
	query_params = list()){

	if(!is.function(uri_builder)){
		stop("uri_builder must be a function")
	}

	config <- c(list(...), endpoint_base = endpoint_base, uri_builder = uri_builder, 
		query_params = query_params)

	api_end_points<-.package_cache_return_or_setup('api_end_points', function(){ 
		return (list()) 
	})

	api_end_points[[name]] <- config

	.package_cache_set('api_end_points', api_end_points)
}


#' .default_uri_builder
#' 
#' Default uri builder
#' 

.default_uri_builder<-function(api_base_uri, config, ..., querystring = ''){

	endpoint_base <- config[['endpoint_base']]

	uri <- paste(api_base_uri, '/', endpoint_base, sep = "")

	if(querystring != ''){
		uri <- paste(uri, querystring, sep="?")
	}

	uri
}

#' .build_uri
#'
#' Generates a URI based on the endpoint configuration and query parameters
#'
#' @param endpoint Name of an endopoint. The endopoint must have been configured
#' previously with .setup_api_endpoint
#' @param ... Custom parameters for the query string
#' @param query URI parameters for the query string passed as a named list
#' @param api_base API base for this URI, no matter if global value was defined
#' @return character
#'

.build_uri<-function(endpoint, ..., query = list(), api_base = NULL){

	# passed or global
	if(is.null(api_base)){
		if(.package_cache_has('api_base')){
			api_base <- .package_cache_get('api_base')
		} else {
			stop("No API base path configured")
		}
	}

	api_end_points<-.package_cache_get('api_end_points')

	config <- api_end_points[[endpoint]]

	if(is.null(config)){
		stop(sprintf("No config for endpoint '%s' is registered", endpoint))
	}

	query <- c(query, list(...))

	# check compulsory params for end point
	if(!is.null(config['query_params']) && length(config['query_params'][[1]] > 0)){
		.stop_on_missing_param(config['query_params'][[1]], query)
	}

	# qs computed here so custom uri builders don't have to make it
	qs<-.build_query_string(query)

	# TODO: find way to force signature for builders ... oop?
	builder <- config[['uri_builder']]

	uri <- builder(api_base, config, querystring = qs)

	uri
}

#' .set_api_base
#'
#' Wrapper to set up the base address of the api
#' @param api_base the API base URL
#' @noRd

.set_api_base<-function(api_base){
	.package_cache_set('api_base', api_base)
}

#' .stop_on_missing_param
#'
#' Throws error if a compulsory parameter is not found in query
#' @param compulsory_params list of compulsory
#' @param query list of query name/value pairs
#'
.stop_on_missing_param<-function(compulsory_params, query){

	q_params <- names(query)

	for(c_param in compulsory_params) {
		if(!is.element(c_param, q_params)){
			stop(sprintf("Query string param '%s' is missing", c_param))
		}
	}
}
