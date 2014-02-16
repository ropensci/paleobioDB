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
#' data<- pbdbQueryTaxa (taxa= "Canidae", time= "Quaternary")
#' number_of_subtaxa (data, show=c("genus", "species"))
#'}
#'
#'

number_of_subtaxa<- function (data, show=c("species", "genera", "tribes", "subfamilies", "families","superfamilies",  
                                           "orders", "classes", "subclasses", "subphyla", "phyla")){
  
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
  
  subtaxa [,match (show, all)]
  
}

number_of_subtaxa ()
species<- data [data$rnk==3, ]
max_sp<- aggregate(species$eag, list(species$tna), max)
min_sp<- aggregate(species$lag, list(species$tna), min)
temporal_range<- data.frame (max_sp [,2], min_sp[,2])
row.names (temporal_range)<- max_sp[,1]
colnames (temporal_range)<- c("max", "min")
pos<- c(1:dim (temporal_range)[1])
t_range<- cbind (temporal_range, pos)
order_by_appearance<- t_range [order (t_range$max),]
t_range<- order_by_appearance
t_range$pos<- c(1:dim (t_range)[1])

division<- seq(from=0, to=2, by=0.2)

which (t_range$min <= division [1] & t_range$max>= division [2])



