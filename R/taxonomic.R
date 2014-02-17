#' 
#' count the number of subtaxa within a given taxa. 
#' e.g. number of species within a genus. 
#' 
#' @usage number_of_subtaxa (data, show=c("species", "genera", "tribes", "subfamilies", "families","superfamilies",  
#' "orders", "classes", "subclasses", "subphyla", "phyla"))
#' 
#' @param data input dataframe with our query
#' @param show use show to choose which subtaxa should be shown: "species", "genera", "tribes", "subfamilies", "families","superfamilies",
#' "orders", "classes", "subclasses", "subphyla", "phyla". By default the function shows all of them. 
#' @return a dataframe with the number of subtaxa of the chosen cathegories
#' @export 
#' @examples \dontrun{
#' canidae_quat<-  pbdb_query_occurrences (limit="all", base_name="Canidae",  interval="Quaternary", show="coords")
#' number_of_subtaxa (canidae_quat)
#'}
#'
#'

number_of_subtaxa<- function (data, show=c("species", "genera", "tribes", "subfamilies", "families","superfamilies",  
                                           "orders", "classes", "subclasses", "subphyla", "phyla")){
  
  if('rnk' %in% colnames(data)) {
    
  number_subphyla<- length(which (data$rnk== 25))
  number_phyla<- length(which (data$rnk== 20))
  
  number_class_plants_invertebrates<- length(which (data$rnk== 17))
  number_subclass<- length(which (data$rnk== 16))
  
  number_class<- length(which (data$rnk== 15))
  
  number_order<- length(which (data$rnk== 13))
  
  number_superfamilies<- length(which (data$rnk== 10))
  number_families<- length(which (data$rnk== 9))
  number_subfamilies<- length(which (data$rnk== 8))
  
  number_tribes<- length(which (data$rnk== 7))
  
  number_genera<- length(which (data$rnk== 5)) 
  
  number_species<- length(which (data$rnk== 3)) 
  
  all<- c("species", "genera", "tribes", "subfamilies", "families","superfamilies",  
          "orders",  "subclasses", "classes","subphyla", "phyla")
  
  subtaxa<- data.frame( number_species, number_genera, number_tribes, number_subfamilies, number_families, 
                        number_superfamilies, number_order, number_subclass, number_class, number_subphyla, number_phyla)
  names (subtaxa)<- all
  
  subtaxa [,match (show, all)]} else {
  stop ("variable names should have the 3-letters code (the default in the query to PBDB)" )
  }
}




