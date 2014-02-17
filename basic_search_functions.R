

#' @param name  Select the all taxa matching each of the specified name(s). 
#' To specify more than one, separate them by commas. 
#' The % character may be used as a wildcard.
#' @param exact  If this parameter is specified, 
#' then the taxon exactly matching the specified name or identifier is selected, 
#' rather than the senior synonym which is the default.
#' @param rank  Return only taxonomic names at the specified rank, e.g. genus.
#' @param show 
#' attr The attribution of this taxon (author and year) 
#' app	The age of first and last appearance of this taxon from the occurrences recorded in this database 
#' size	The number of subtaxa appearing in this database
#' 
#' 
#' 
#' 
#' Complete list of parameters in http://paleobiodb.org/data1.1/taxa/list_doc.html

canis<- pbdb_query_occurrences (name="Canis")
