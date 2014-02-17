data(wrld_simpl)
land<- readShapePoly ("C:/Users/sara/Documents/_CIENCIAS/pbdb/docs_desarrollo/ne_110m_land.shp")
ice<- readShapePoly ("C:/Users/sara/Documents/_CIENCIAS/pbdb/docs_desarrollo/ne_110m_glaciated_areas.shp")

proj4string (ocean)
proj4string(ocean)<-CRS("+init=epsg:4623") 

plot (wrld_simpl, border="darkgrey", lwd="0.0001", col="white", add=T)


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
#' number_of_taxa (data, show=c("genus", "species"))
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



#' 
#' to plot the occurrences of the taxa in a global map 
#' 
#' @usage map_taxa (data, taxa, time)
#' 
#' @param data input dataframe with our query
#' @param taxa name of the taxa to be plotted, e.g. "Canis"
#' @param name temporal extent of the fossil records to be plotted, e.g. "Quaternary".
#' @return a global plot with the geographic position of the fossil records.
#' @export 
#' @examples \dontrun{
#' data<- pbdbQueryTaxa (taxa= "Canidae", time= "Quaternary")
#' map_taxa (data, "Canis", "Quaternary")
#'}
#'
#'
#'
library (maptools)
library (scales)

map_taxa<- function (data, taxa, time){
ocean<- readShapePoly ("C:/Users/sara/Documents/_CIENCIAS/pbdb/docs_desarrollo/ne_110m_ocean.shp")
par(mar=c(0,1,0,0))
plot (ocean, col="black", border=FALSE)
points (data$lng, data$lat, cex=0.8, pch=16, col=alpha ("red", 0.5))
text(180, 110, taxa, col="firebrick2", font=3, adj = c(1,1))
text(-185, -85,  time, srt= 90, font=2, col="firebrick4", adj = c(0,0))
}


#' to plot the occurrences of the taxa in a global map 
#' 
#' @usage map_taxa (data, taxa, time)
#' 
#' @param data input dataframe with our query
#' @param taxa name of the taxa to be plotted, e.g. "Canis"
#' @param name temporal extent of the fossil records to be plotted, e.g. "Quaternary".
#' @return a global plot with the geographic position of the fossil records.
#' @export 
#' @examples \dontrun{
#' data<- pbdbQueryTaxa (taxa= "Canidae", time= "Quaternary")
#' map_taxa (data, "Canis", "Quaternary")
#'}
#'
#'
temporal_resolution<- function (data, show=c("summary", "raw")) {
  list (summary=summary (data$eag - data$lag), raw_data=(data$eag - data$lag))
}

