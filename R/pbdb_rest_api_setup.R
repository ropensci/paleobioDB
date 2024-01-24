# Functions and variables for setting up and managing the communication
# with the paleobiodb.org REST API

#' .pbdb_uri_builder
#'
#' Function that generates the URIs for the paleobiodb.org API
#'
#' @param api_base_url Base url for the API
#' @param config Configuration of the endpoint
#' @param querystring compiled querystring
#' @noRd

.pbdb_uri_builder <- function(api_base_url, config, querystring = ''){
	
	api_format<-'json'

	# endpoint_base is expected to be a string format to fit the api_format in
	uri <- paste(api_base_url, '/', sprintf(config[['endpoint_base']], api_format), sep = "")

	if(querystring != ''){
		uri <- paste(uri, querystring, sep="?")
	}

	uri
}


#' .pbdb_set_up_endpoints
#'
#' This function registers all the endpoints available from the paleobiodb.org REST API
#' @noRd
.pbdb_set_up_endpoints<-function(){

	# single occurrencies
	.setup_api_endpoint('occs/single', 'occs/single.%s', uri_builder = .pbdb_uri_builder, 
		compulsory_params = list('id'))

	# occurrencies list
	.setup_api_endpoint('occs/list', 'occs/list.%s', uri_builder = .pbdb_uri_builder)
	
  	# occurrences references list 
	.setup_api_endpoint('occs/refs', 'occs/refs.%s', uri_builder = .pbdb_uri_builder)
  
  	# fossil collection
	.setup_api_endpoint('colls/single', 'colls/single.%s', uri_builder = .pbdb_uri_builder, 
	                    compulsory_params = list('id'))
  
	# fossil collections
	.setup_api_endpoint('colls/list', 'colls/list.%s', uri_builder = .pbdb_uri_builder)
  
	# fossil collections geo
	.setup_api_endpoint('colls/summary', 'colls/summary.%s', uri_builder = .pbdb_uri_builder)
	
  	#taxa single
	.setup_api_endpoint('taxa/single', 'taxa/single.%s', uri_builder = .pbdb_uri_builder)
	
	# taxa list
	.setup_api_endpoint('taxa/list', 'taxa/list.%s', uri_builder = .pbdb_uri_builder)
	
	# taxa auto
	.setup_api_endpoint('taxa/auto', 'taxa/auto.%s', uri_builder = .pbdb_uri_builder)

	#intervals single
	.setup_api_endpoint('intervals/single', 'intervals/single.%s', uri_builder = .pbdb_uri_builder, 
	                    compulsory_params = list('id'))
	
	# intervals list
	.setup_api_endpoint('intervals/list', 'intervals/list.%s', uri_builder = .pbdb_uri_builder)
	
	#scales single
	.setup_api_endpoint('scales/single', 'scales/single.%s', uri_builder = .pbdb_uri_builder, 
	                    compulsory_params = list('id'))
	
	# scales list
	.setup_api_endpoint('scales/list', 'scales/list.%s', uri_builder = .pbdb_uri_builder)

  	# strata list
	.setup_api_endpoint('strata/list', 'strata/list.%s', uri_builder = .pbdb_uri_builder)
	
	# strata auto
	.setup_api_endpoint('strata/auto', 'strata/auto.%s', uri_builder = .pbdb_uri_builder)
	
	# refs list
	.setup_api_endpoint('refs/list', 'refs/list.%s', uri_builder = .pbdb_uri_builder)
  
	# refs single
	.setup_api_endpoint('refs/single', 'refs/single.%s', uri_builder = .pbdb_uri_builder)
  
	# colls refs
	.setup_api_endpoint('colls/refs', 'colls/refs.%s', uri_builder = .pbdb_uri_builder)
	
  	# refs taxa
	.setup_api_endpoint('taxa/refs', 'taxa/refs.%s', uri_builder = .pbdb_uri_builder)
	
}


# Initialize configuration of package API
.pbdb_setup<-function(){
	.set_api_base("https://paleobiodb.org/data1.2")
	.pbdb_set_up_endpoints()
	.package_cache_set("api_format", "json")
}

.pbdb_setup()
