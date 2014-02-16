# Functions and variables for setting up an managing the comunication 
# with the paleobiodb.org REST API

pbdb_set_format<-function(format){
	.package_cache_set('api_format', format)
}

#' Function that generates the URIs for the paleobiodb.org API
#' 
#' @param api_base_url Base url for the 
#
# .rpbdb_uri_builder <- function(api_base_url, config, ..., api_format = NULL, querystring = ''){
.pbdb_uri_builder <- function(api_base_url, config, query = query, querystring = qs, 
	endpoint_name = endpoint, api_format = NULL){
	
	if(is.null(api_format)){
		stop("api_format param is required for building pbdb API calls")
	}

	# endpoint_base is expected to be a string format to fit the api_format in
	uri <- paste(api_base_url, '/', sprintf(config[['endpoint_base']], api_format), sep = "")

	if(querystring != ''){
		uri <- paste(uri, querystring, sep="?")
	}

	uri
}

#' This functions registers all the endpoints available from the paleobiodb.org REST API
#'
.pbdb_set_up_endpoints<-function(){

	# single occurrencies
	.setup_api_endpoint('occs/single', 'occs/single.%s', uri_builder = .pbdb_uri_builder, 
		compulsory_params = list('id'))

	# occurrencies list
	.setup_api_endpoint('occs/list', 'occs/list.%s', uri_builder = .pbdb_uri_builder)
	
}


.pbdb_setup<-function(){
	.set_api_base('http://paleobiodb.org/data1.1')
	.pbdb_set_up_endpoints()
	pbdb_set_format('json')
}

.pbdb_setup()