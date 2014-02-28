# R Functions leveraging the use o the different API endpoints available


#'
#'
.pbdb_query<-function(endpoint, query = list()){
	uri <- pbdb_query_uri(endpoint, query)

	df <- .get_data_from_uri(uri)

	df
}

pbdb_query_uri<-function(endpoint, query = list()){

  query <- lapply(query, .implode_to_string)

  return (.build_uri(endpoint, query = query))
}

#' pbdb_occurrence 
#' 
#' Returns information about a single occurrence record from the Paleobiology Database.
#' 
#'@param id identifier of the occurrence. 
#'@param ... all the parameters available in http://paleobiodb.org/data1.1/occs/single 
#'@param taxon_name Return only records associated with the specified taxonomic name(s). 
#'You may specify multiple names, separated by commas.
#'@param base_name  Return only records associated with the specified taxonomic name(s), or any of their children. 
#'You may specify multiple names, separated by commas.
#'@param show to show extra variables (e.g. coords)
#' 
pbdb_occurrence<-function(id, ...){
  l<-list(...)
  
  # todo: merge lists properly  
  .pbdb_query('occs/single', query = c(list(id = id), l))
}

pbdb_occurrences<-function(...){

  l<-list(...)
	.pbdb_query('occs/list', query = l)

}


#' Converts a list in a comma separated string
.implode_to_string<-function(param){

  if(!(is.vector(param))){
  	stop("Vector expected")
  }

  if(length(param) > 1){
    str <- param[[1]]
    for (p in param[2:length(param)]) {
      str <- paste(str, ",", p, sep = "")
    }
  } else {
  	str <- param
  }

  return (str)
}