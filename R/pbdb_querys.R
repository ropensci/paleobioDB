# R Functions leveraging the use o the different API endpoints available


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

#'
#'
.pbdb_query<-function(endpoint, query = list()){
	uri <- pbdb_query_uri(endpoint, query)

	df <- .get_data_from_uri(uri)

	df
}

.pbdb_query_uri<-function(endpoint, query = list()){

  query <- lapply(query, .implode_to_string)

  return (.build_uri(endpoint, query = query))
}

#' pbdb_occurrence 
#' 
#' Returns information about a single occurrence record from the Paleobiology Database.
#' 
#'@param id identifier of the occurrence. This parameter is required.
#'@param ... documentation for all the parameters is available in http://paleobiodb.org/data1.1/occs/single
#' Below, we describe the most common filters that 
#' paleontologists and ecologists might use.
#'@param vocab set vocab="pbdb" to show the complete name of the variables
#'(by default variables have short 3-letter names)
#'@param show to show extra variables (e.g. coords)
#'
#' @return a dataframe with a single occurrence 
#' 
#' @export 
#' @examples \dontrun{
#' pbdb_occurrence (id=1001)
#' pbdb_occurrence (id=1001, vocab="pbdb", show="coords")
#'}
#'
#' 
#' 
pbdb_occurrence<-function(id, ...){
  l<-list(...)
  
  # todo: merge lists properly  
  .pbdb_query('occs/single', query = c(list(id = id), l))
}

#'pbdb_occurrences
#'
#'Returns information about species occurrence records stored in the Paleobiology Database.
#'
#'@param ... documentation for all the parameters is available in http://paleobiodb.org/data1.1/occs/list
#' Below, we describe the most common filters that 
#' paleontologists and ecologists might use.
#'@param limit set limit to "all" to download all the occurrences. By defauls the limit is 500. 
#'@param taxon_name Return only records associated with the specified taxonomic name(s). 
#'You may specify multiple names, separated by commas.
#'@param base_name  Return only records associated with the specified taxonomic name(s), or any of their children. 
#'You may specify multiple names, separated by commas.
#'
#'@param lngmin numeric. The longitude boundaries will be normalized 
#'to fall between -180 and 180. (Note that if you specify lngmin then you must also specify lngmax). 
#'Return only records whose geographic location falls 
#'within the given bounding box (defined by lngmin, lngmax, latmin, latmax).
#'It generate two adjacent bounding boxes if the range crosses the antimeridian. 
#'@param lngmax numeric. The longitude boundaries will be normalized 
#'to fall between -180 and 180.
#'@param latmin numeric. between -90 and 90. (Note that if you specify latmin then you must also specify latmax)
#'@param latmax numeric. between -90 and 90.
#'
#'@param min_ma return only records whose temporal locality is at least this old, specified in Ma.
#'@param max_ma return only records whose temporal locality is at most this old, specified in Ma.
#'@param interval return only records whose temporal locality falls within the named geologic time interval (e.g. "Miocene").
#'@param continent return only records whose geographic location falls within the specified continent(s). 
#'@param show to show extra variables (e.g. coords)
#' 
#' @return a dataframe with the species occurrences 
#' 
#' @export 
#' @examples \dontrun{
#' pbdb_occurrences (limit="all", vocab= "pbdb", taxon_name="Canis", show="coords")
#' pbdb_occurrences (limit="all", vocab= "pbdb", base_name="Canidae", show="coords")
#'}
#'
pbdb_occurrences<-function(...){

  l<-list(...)
	.pbdb_query('occs/list', query = l)

}

#'pbdb_ref_occurrences
#'
#'Returns information about the bibliographic references associated with fossil occurrences from the database.
#'
#'@param ... documentation for all the parameters is available in http://paleobiodb.org/data1.1/occs/refs
#' go to ?pbdb_occurrences to see an explanation about the main filtering parameters 
#' 
#' @param author Select only references for which any of the authors matches the specified name
#' @param year Select only references published in the specified year
#' @param pubtitle Select only references that involve the specified publication
#' @param order Specifies the order in which the results are 
#' returned. You can specify multiple values separated by commas, 
#' and each value may be appended with .asc or .desc.  Accepted values are:
#' author, year, pubtitle, created, modified, rank. (see documentation in http://paleobiodb.org/data1.1/occs/refs)
#' 
#' @return a dataframe with the information about the references that match the query
#' 
#' @export 
#' @examples \dontrun{
#' pbdb_ref_occurrences (vocab="pbdb", taxon_name="Canis", year=2000)
#'}
#'
pbdb_ref_occurrences<-function(...){
  
  l<-list(...)
  .pbdb_query('occs/refs', query = l)
  
}


#' pbdb_collection 
#' 
#'Returns information about a single collection record from the Paleobiology Database.
#' 
#'@param id identifier of the collection. This parameter is required.
#'@param ... documentation for all the parameters is available in http://paleobiodb.org/data1.1/colls/single
#' Below, we describe the most common filters that 
#' paleontologists and ecologists might use.
#'@param vocab set vocab="pbdb" to show the complete name of the variables
#'(by default variables have short 3-letter names)
#'@param show to show extra variables (e.g. "loc" to show additional information about the geographic locality of the collection)
#'
#' @return a dataframe with a single occurrence 
#' 
#' @export 
#' @examples \dontrun{
#' pbdb_collection (id=1003, vocab="pbdb", show="loc")
#' 
#'}
#'
#' 
#' 
pbdb_collection<-function(id, ...){
  l<-list(...)
  
  # todo: merge lists properly  
  .pbdb_query('colls/single', query = c(list(id = id), l))
}

#'pbdb_collections
#'
#'Returns information about multiple collections, selected according to the parameters you provide.
#'
#'@param ... documentation for all the parameters is available in http://paleobiodb.org/data1.1/colls/list
#' go to ?pbdb_occurrences to see an explanation about the main filtering parameters 
#' 
#' @return a dataframe with the collections that match the query
#' 
#' @export 
#' @examples \dontrun{
#' pbdb_collections (base_name="Cetacea", interval="Miocene")
#'}
#'
pbdb_collections<-function(...){
  
  l<-list(...)
  .pbdb_query('colls/list', query = l)
  
}

#'pbdb_collections_geo
#'
#'This path returns information about geographic clusters of collections from the Paleobiology Database. 
#'These clusters are defined in order to facilitate the 
#'generation of maps at low resolutions. 
#'You can make a config request via http://paleobiodb.org/data1.1/config
#'in order to get a list of the available summary levels.
#'
#'@param ... documentation for all the parameters is available in http://paleobiodb.org/data1.1/colls/summary
#' go to ?pbdb_occurrences to see an explanation about the main filtering parameters 
#' 
#' @return a dataframe with the collections that match the query
#' 
#' @export 
#' @examples \dontrun{
#' pbdb_collections_geo (vocab="pbdb", lngmin=0.0, lngmax=15.0, latmin=0.0, latmax=15.0, level=2)
#'}
#'
pbdb_collections_geo<-function(...){
  
  l<-list(...)
  .pbdb_query('colls/summary', query = l)
  
}
