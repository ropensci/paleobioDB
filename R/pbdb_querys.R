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
#' pbdb_occurrences (id=c(10, 11)) 
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
#' @param author Select only references for which any of the authors matches the specified name
#' @param year Select only references published in the specified year
#' @param pubtitle Select only references that involve the specified publication
#' @param order Specifies the order in which the results are 
#' returned. You can specify multiple values separated by commas, 
#' and each value may be appended with .asc or .desc.  Accepted values are:
#' author, year, pubtitle, created, modified, rank. (see documentation in http://paleobiodb.org/data1.1/occs/refs)
#'@param ... documentation for all the parameters is available in http://paleobiodb.org/data1.1/occs/refs
#' go to ?pbdb_occurrences to see an explanation about the main filtering parameters 
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
#'@param vocab set vocab="pbdb" to show the complete name of the variables
#'(by default variables have short 3-letter names)
#'@param show to show extra variables (e.g. "loc" to show additional information about the geographic locality of the collection)
#'@param ... documentation for all the parameters 
#'is available in http://paleobiodb.org/data1.1/colls/single
#' go to ?pbdb_occurrences to see an explanation about the main filtering parameters 
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

#' pbdb_taxon
#' 
#' Returns information about a single taxonomic name, identified either by name or by identifier.
#' 
#'@param name Return information about the most fundamental taxonomic name matching this string. 
#' The % and _ characters may be used as wildcards.
#'@param id Return information about the taxonomic name 
#'corresponding to this identifier. You may not specify both 
#'name and id in the same query.
#'
#'@param ... documentation for all the parameters is available 
#'in http://paleobiodb.org/data1.1/taxa/single
#' go to ?pbdb_taxa to see an explanation about the main filtering parameters 
#'
#' @return a dataframe with information from a single taxon
#' 
#' @export 
#' @examples \dontrun{
#' pbdb_taxon (name="Canis", vocab="pbdb", show=c("attr", "app", "size"))
#' 
#'}
#'
#'
pbdb_taxon<-function(...){
  l<-list(...)
  
  # todo: merge lists properly  
  .pbdb_query('taxa/single', query = l)
}


#' pbdb_taxa
#' 
#'Returns information about multiple taxonomic names. 
#'This function can be used to query for 
#'all of the children or parents of a given taxon, among other operations.
#'
#'@param name Return information about the most fundamental taxonomic name matching this string. 
#' The % and _ characters may be used as wildcards.
#'@param id Return information about the taxonomic name 
#'corresponding to this identifier. You may not specify both 
#'name and id in the same query.
#'@param exact if this parameter is specified, then the taxon exactly 
#'matching the specified name or identifier is selected, 
#'rather than the senior synonym which is the default.
#'@param show to show extra variables: attr (The attribution of this taxon (author and year)), 
#'app (The age of first and last appearance of this taxon from the occurrences recorded in this database), 
#'size (The number of subtaxa appearing in this database), nav (Additional information for the PBDB Navigator taxon browser)
#'@param rel set rel="synonyms" to select all synonyms of the base taxon or taxa; 
#'rel="children" to select the taxa immediately contained within the base taxon or taxa; 
#'rel="common_ancestor" to select the most specific taxon that contains all of the base taxa.
#'@param extant TRUE/FALSE to select extinct/extant taxa.
#'
#'@param ... documentation for all the parameters is available.
#'
#'in http://paleobiodb.org/data1.1/taxa/list
#'
#' @return a dataframe with information from a list of taxa
#' 
#' @export 
#' @examples \dontrun{
#' pbdb_taxa (name="Canidae", vocab="pbdb", show=c("attr", "app", "size", "nav"))
#' pbdb_taxa (id =c(10, 11), vocab="pbdb", show=c("attr", "app", "size", "nav"))
#' pbdb_taxa (id =c(10, 11), vocab="pbdb", show=c("attr", "app", "size", "nav"), rel="common_ancestor")
#'}
#'
#'
pbdb_taxa<-function(...){
  l<-list(...)
  
  # todo: merge lists properly  
  .pbdb_query('taxa/list', query = l)
}


#' pbdb_taxa_auto
#' 
#' Returns a list of names matching the given prefix or partial name. 
#' 
#'@param name A partial name or prefix. It must have at least 3 significant characters,
#' and may include both a genus (possibly abbreviated) and a species.
#' @param limit set the limit to the number of matches
#' @param ... 
#' @return a dataframe with information about the matches (taxon rank and number of occurrences in the database)
#' 
#' @export 
#' @examples \dontrun{
#' pbdb_taxa_auto (name="Cani", limit=10)
#' 
#'}
#'
#'
pbdb_taxa_auto<-function(...){
  l<-list(...)
  
  # todo: merge lists properly  
  .pbdb_query('taxa/auto', query = l)
}



#'pbdb_interval
#' 
#' Returns information about a single interval, selected by identifier.
#' 
#'@param id identifier of the temporal interval. This parameter is required.
#'@param vocab set vocab="pbdb" to show the complete name of the variables
#'(by default variables have short 3-letter names)
#'@param ... documentation for all the parameters 
#'is available in http://paleobiodb.org/data1.1/intervals/single
#' @return a dataframe with information from a single temporal interval
#' 
#' @export 
#' @examples \dontrun{
#' pbdb_interval (id=1, vocab="pbdb")
#' 
#'}
#'
#' 
#' 

pbdb_interval<-function(id, ...){
  l<-list(...)
  
  # todo: merge lists properly  
  .pbdb_query('intervals/single', query = c(list(id = id), l))
}



#'pbdb_intervals
#' 
#' Returns information about multiple intervals, 
#' selected according to the parameters you provide.
#' 
#'@param id identifiers of the temporal intervals.
#'@param min_ma return only intervals that are at least this old
#'@param max_ma return only intervals that are at most this old
#'@param order return the intervals in order starting as specified. 
#'Possible values include older, younger. Defaults to younger.
#'@param vocab set vocab="pbdb" to show the complete name of the variables
#'(by default variables have short 3-letter names)
#'@param ... documentation for all the parameters 
#'is available in http://paleobiodb.org/data1.1/intervals/lists
#' @return a dataframe with information from several temporal intervals
#' 
#' @export 
#' @examples \dontrun{
#' pbdb_intervals (min_ma= 0, max_ma=2, vocab="pbdb")
#' 
#'}
#'
#' 
#' 

pbdb_intervals<-function(id, ...){
  l<-list(...)
  
  # todo: merge lists properly  
  .pbdb_query('intervals/list', query = l)
}


#'pbdb_scale
#' 
#' Returns information about a single time scale, selected by identifier.
#' 
#'@param id identifier of the temporal interval. This parameter is required.
#'@param vocab set vocab="pbdb" to show the complete name of the variables
#'(by default variables have short 3-letter names)
#'@param ... documentation for all the parameters 
#'is available in http://paleobiodb.org/data1.1/scales/single
#' @return a dataframe with information from a single scale
#' 
#' @export 
#' @examples \dontrun{
#' pbdb_scale (id=1, vocab="pbdb")
#' 
#'}
#'
#' 
#' 

pbdb_scale<-function(id, ...){
  l<-list(...)
  
  # todo: merge lists properly  
  .pbdb_query('scales/single', query = c(list(id = id), l))
}

#'pbdb_scales
#' 
#' Returns information about multiple time scales.
#' 
#'@param id identifiers of the scales. 
#'@param vocab set vocab="pbdb" to show the complete name of the variables
#'(by default variables have short 3-letter names)
#'@param ... documentation for all the parameters 
#'is available in http://paleobiodb.org/data1.1/scales/list
#' @return a dataframe with information from the selected scales
#' 
#' @export 
#' @examples \dontrun{
#' # Get a dataframe with all the scales available in PBDB setting no ids
#' pbdb_scale ()
#' 
#'}
#'
#' 

pbdb_scales<-function(id, ...){
  l<-list(...)
  
  # todo: merge lists properly  
  .pbdb_query('scales/list', query = l)
}



#'pbdb_strata
#' 
#' Returns information about geological strata, 
#' selected by name, rank, and/or geographic location.
#'
#'@param name A full or partial name. You can use % and _ as wildcards, 
#'but the query will be very slow if you put a wildcard at the beginning
#'@param rank Return only strata of the specified rank: formation, group or member.
#'@param lngmin numeric. The longitude boundaries will be normalized 
#'to fall between -180 and 180. (Note that if you specify lngmin then you must also specify lngmax). 
#'Return only records whose geographic location falls 
#'within the given bounding box (defined by lngmin, lngmax, latmin, latmax).
#'It generate two adjacent bounding boxes if the range crosses the antimeridian. 
#'@param lngmax numeric. The longitude boundaries will be normalized 
#'to fall between -180 and 180.
#'@param latmin numeric. between -90 and 90. (Note that if you specify latmin then you must also specify latmax)
#'@param latmax numeric. between -90 and 90.
#'@param loc Return only strata associated with some occurrence whose geographic 
#'location falls within the specified geometry, specified in WKT format.
#'@param vocab set vocab="pbdb" to show the complete name of the variables
#'(by default variables have short 3-letter names)
#'@param ... documentation for all the parameters 
#'is available in http://paleobiodb.org/data1.1/strata/list
#' @return a dataframe with information from the selected strata
#' 
#' @export 
#' @examples \dontrun{
#' pbdb_strata (lngmin=0, lngmax=15, latmin=0, latmax=15, rank="formation", vocab="pbdb")
#' 
#'}
#'
#' 

pbdb_strata<-function(id, ...){
  l<-list(...)
  
  # todo: merge lists properly  
  .pbdb_query('strata/list', query = l)
}



#'pbdb_strata_auto
#' 
#' Returns a list of strata matching the given prefix or partial name. 
#' This can be used to implement auto-completion for strata names, 
#' and can be limited by geographic location if desired.
#'
#'@param name A full or partial name. You can use % and _ as wildcards, 
#'but the query will be very slow if you put a wildcard at the beginning
#'@param rank Return only strata of the specified rank: formation, group or member.
#'@param lngmin numeric. The longitude boundaries will be normalized 
#'to fall between -180 and 180. (Note that if you specify lngmin then you must also specify lngmax). 
#'Return only records whose geographic location falls 
#'within the given bounding box (defined by lngmin, lngmax, latmin, latmax).
#'It generate two adjacent bounding boxes if the range crosses the antimeridian. 
#'@param lngmax numeric. The longitude boundaries will be normalized 
#'to fall between -180 and 180.
#'@param latmin numeric. between -90 and 90. (Note that if you specify latmin then you must also specify latmax)
#'@param latmax numeric. between -90 and 90.
#'@param loc Return only strata associated with some occurrence whose geographic 
#'location falls within the specified geometry, specified in WKT format.
#'@param vocab set vocab="pbdb" to show the complete name of the variables
#'(by default variables have short 3-letter names)
#'@param ... documentation for all the parameters 
#'is available in http://paleobiodb.org/data1.1/strata/auto
#' @return a dataframe with information from the strata that matches our letters.
#' 
#' @export 
#' @examples \dontrun{
#' pbdb_strata_auto (name= "Pin", vocab="pbdb")
#' 
#'}
#'
#' 

pbdb_strata_auto<-function(id, ...){
  l<-list(...)
  
  # todo: merge lists properly  
  .pbdb_query('strata/auto', query = l)
}


#'pbdb_references
#'
#'Returns information about multiple references, selected according to the parameters you provide.
#'
#' @param author Select only references for which any of the authors matches the specified name
#' @param year Select only references published in the specified year
#' @param pubtitle Select only references that involve the specified publication
#' @param order Specifies the order in which the results are 
#' returned. You can specify multiple values separated by commas, 
#' and each value may be appended with .asc or .desc.  Accepted values are:
#' author, year, pubtitle, created, modified, rank. 
#'@param ... documentation for all the parameters is available in http://paleobiodb.org/data1.1/refs/list 
#' 
#' @return a dataframe with the information about the references that match the query
#' 
#' @export 
#' @examples \dontrun{
#' pbdb_references (author="Turner")
#'}
#'
pbdb_references<-function(...){
  
  l<-list(...)
  .pbdb_query('refs/list', query = l)
  
}


#'pbdb_ref_collections
#'
#'Returns information about the references from which the selected collection data were entered.
#'
#' @param id A comma-separated list of collection identifiers.
#' @param author Select only references for which any of the authors matches the specified name
#' @param year Select only references published in the specified year
#' @param pubtitle Select only references that involve the specified publication
#' @param order Specifies the order in which the results are 
#' returned. You can specify multiple values separated by commas, 
#' and each value may be appended with .asc or .desc.  Accepted values are:
#' author, year, pubtitle, created, modified, rank.
#'@param ... documentation for all the parameters is available in http://paleobiodb.org/data1.1/colls/refs
#' 
#' @return a dataframe with the information about the references that match the query
#' 
#' @export 
#' @examples \dontrun{
#' pbdb_ref_collections (id=1)
#'}
#'
pbdb_ref_collections <-function(...){
  
  l<-list(...)
  .pbdb_query('colls/refs', query = l)
  
}

