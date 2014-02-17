
pbdb_query_occurrence (1001)

#' Complete list of paramenters is available in http://paleobiodb.org/data1.1/occs/list
#' Here, for practical reasons we selected the most common filters that 
#' paleontologists and ecologists might use
#' 
#' 
#'@param taxon_name Return only records associated with the specified taxonomic name(s). 
#'You may specify multiple names, separated by commas.
#'@param base_name  Return only records associated with the specified taxonomic name(s), or any of their children. 
#'You may specify multiple names, separated by commas.
#' @param show 
#' attr The attribution of this taxon (author and year) 
#' app  The age of first and last appearance of this taxon from the occurrences recorded in this database 
#' size	The number of subtaxa appearing in this database
#' 
#' @param min_ma  Return only intervals that are at least this old
#' @param max_ma	Return only intervals that are at most this old
#' 
#' 


#' Set the basic query: 
#' limit= "all" to download all the occurrences (by default the limit is 500) 
#' vocab= "pbdb" to show the names of the variables complete (by default variables have short 3-letter names)
#' base_name= "Canis" to download all the records from the taxon and subtaxa 
#' (e.g., "Canis", "Canis lupus", "Canis mosbachensis", etc.)
#' show ="coords" to show latitude and longitude (by default they are not shown)
#' 
#' 

canis<- pbdb_query_occurrences (limit="all", 
                                vocab= "pbdb", 
                                base_name="Canis", 
                                show="coords")



