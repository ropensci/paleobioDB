#' pbdb_subtaxa
#' 
#' count the number of subtaxa within a given taxa. 
#' e.g. number of species within a genus. 
#' 
#' @usage number_of_subtaxa (data, show=c("species", "genera", "tribes", "subfamilies", "families","superfamilies",  
#' "orders", "classes", "subclasses", "subphyla", "phyla"))
#' 
#' @param data dataframe with our query to the paleoBD \code{\link{pbdb_occurrences}} 
#' @param do.plot by default this function make a plot to visualize the distribution of taxa. Set to FALSE to skip the plot.
#' @param show use show to choose which subtaxa should be shown: "species", "genera", "tribes", "subfamilies", "families","superfamilies",
#' "orders", "classes", "subclasses", "subphyla", "phyla". By default the function shows all of them. 
#' @return a dataframe with the number of subtaxa of the chosen cathegories
#' @export 
#' @examples \dontrun{
#' canidae_quat<-  pbdb_occurrences (limit="all", 
#' base_name="Canidae",  interval="Quaternary", show="coords")
#' pbdb_subtaxa (canidae_quat)
#'}
#'


pbdb_subtaxa<- function (data, col="skyblue2", 
                         do.plot= TRUE, show=c("species", "genus", "tribe", "subfamily", "family","superfamily",  
                                           "order", "class", "subclass", "subphylum", "phylum")){
  
  all<- c("species", "genus", "tribe", "subfamily", "family","superfamily",  
          "order",  "subclass", "class","subphylum", "phylum")
  
  if('rnk' %in% colnames(data)) {
  number_phyla<- length(which (data$rnk== 20)) 
  number_subphyla<- length(which (data$rnk== 25))
  
  number_class_plants_invertebrates<- length(which (data$rnk== 17))
 
  number_class<- length(which (data$rnk== 15))
  number_subclass<- length(which (data$rnk== 16))
  
  number_order<- length(which (data$rnk== 13))
  
  number_superfamilies<- length(which (data$rnk== 10))
  number_families<- length(which (data$rnk== 9))
  number_subfamilies<- length(which (data$rnk== 8))
  
  number_tribes<- length(which (data$rnk== 7))
  
  number_genera<- length(which (data$rnk== 5)) 
  
  number_species<- length(which (data$rnk== 3)) 
  
  subtaxa<- data.frame( number_species, number_genera, number_tribes, number_subfamilies, number_families, 
                        number_superfamilies, number_order, number_subclass, number_class, number_subphyla, number_phyla)
  names (subtaxa)<- all
  } 
  
  if ('taxon_rank' %in% colnames(data)) {
  taxa<- as.data.frame (table (data$taxon_rank))
  subtaxa<- taxa [,2]
  names (subtaxa)<- taxa[,1]
  subtaxa<- as.data.frame (t(subtaxa))
  subtaxa<- subtaxa [, rank (match (names (subtaxa), all))]
  }
  
  if (do.plot==TRUE){
  st<- match (show, names (subtaxa))
  par (mar=c(8,4,2,0))
  barplot (unlist (subtaxa [, sort(st [!is.na (st)])]),  
           beside = T, horiz=F,
           col=col,
           border=F,
           las=2)
  }
  return (subtaxa)
}


  

