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
points (data$lng, data$lat, cex=0.8, pch=20, col=alpha ("red", 0.5))
text(180, 110, taxa, col="gray50", font=3, adj = c(1,1))
text(-185, -85,  time, srt= 90, font=2, col="gray30", adj = c(0,0))
}

map_taxa (data, "Canis", "Quaternary")

#' to plot the oldest and the latest records of a taxa
#' 
#' @usage map_origin_ext (data, taxon)
#' 
#' @param data input dataframe with our query
#' @param taxon label for the map, name of the taxon of interest, e.g. "Canis"
#' @return a plot with the oldest (in red) and latest (in green) fossil records of a taxon.
#' Brown points indicate fossil sites with both, old and young records 
#' (or with a temporal extent overlapping both extremes))
#'  
#' @export 
#' @examples \dontrun{
#' data<- pbdbQueryTaxa (taxa= "Canis", time= "Quaternary")
#' map_origin_ext (data, "Canis")
#'}
#'
#'
#'

map_origin_ext<- function (data, taxon){
  ocean<- readShapePoly ("C:/Users/sara/Documents/_CIENCIAS/pbdb/docs_desarrollo/ne_110m_ocean.shp")
  par(mar=c(0,1,0,0))
  plot (ocean, col="black", border=FALSE)
  points ( data[ which (data$eag==max (data$eag)),]$lng, 
           data[ which (data$eag==max (data$eag)),]$lat, 
           cex=0.8, pch=16, col=alpha ("red", 0.8))
  points (data[ which (data$lag==min (data$lag)),]$lng, 
          data[ which (data$lag==min (data$lag)),]$lat, 
          cex=0.8, pch=16, col=alpha ("green3", 0.5))
  
  last_records<-  data.frame( data[ which (data$lag==min (data$lag)),], 
                              data[ which (data$lag==min (data$lag)),])
  
  old_records<-  data.frame(data[ which (data$eag==max (data$eag)),],
                            data[ which (data$eag==max (data$eag)),]) 
  names (last_records)<- names (data)
  names (old_records)<- names (data)
  repeated_records<-  sqldf('SELECT * FROM last_records INTERSECT SELECT * FROM old_records')
  
  points (repeated_records$lng, repeated_records$lat, 
          cex=0.8, pch=16, col=alpha ("brown", 0.8))
  
  text(180, 105, taxon, col="gray50", font=3, adj = c(1,1))
  text(-185, -85,  paste (max (data$eag), "Ma", sep=" "), 
       srt= 90, font=2, col="salmon", adj = c(0,0))
  text(-185, 90,  paste (min (data$eag), "Ma", sep=" "), 
       srt= 90, font=2, col="darkolivegreen3", adj = c(1,0))
  
}

map_origin_ext(data, "Canis")

