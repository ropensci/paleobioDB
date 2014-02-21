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

pbdb_query_occurrence<-function(id, ...){
  l<-list(...)
  
  # todo: merge lists properly  
  .pbdb_query('occs/single', query = c(list(id = id), l))
}

pbdb_query_occurrences<-function(...){

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