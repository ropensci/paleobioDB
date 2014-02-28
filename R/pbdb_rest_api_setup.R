# Functions and variables for setting up and managing the comunication 
# with the paleobiodb.org REST API

.pbdb_set_format<-function(format){
	.package_cache_set('api_format', format)
}

#' Function that generates the URIs for the paleobiodb.org API
#' 
#' @param api_base_url Base url for the 
#
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
  
	# colls refs
	.setup_api_endpoint('colls/refs', 'colls/refs.%s', uri_builder = .pbdb_uri_builder)
	
}


.pbdb_setup<-function(){
	.set_api_base('http://paleobiodb.org/data1.1')
	.pbdb_set_up_endpoints()
	.pbdb_set_format('json')
}

.pbdb_setup()