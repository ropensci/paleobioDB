#' to show the temporal resolution of the fossil data
#' 
#' @usage temporal_resolution (data)
#' 
#' @param data input dataframe with our query
#' @return a list with a summary of the temporal resolution of the data 
#' (min, max, 1st and 3rd quartils, median and mean), and the temporal resolution of each fossil record (Ma).
#' @export 
#' @examples \dontrun{
#' data<- pbdbQueryTaxa (taxa= "Canidae", time= "Quaternary")
#' temporal_resolution (data)
#'}
#'
#'
pbdb_temporal_resolution<- function (data) {
  list (summary=summary (data$eag - data$lag), temporal_resolution=(data$eag - data$lag))
}

#' to show the time spam of the species included in the query
#' 
#' @usage time_spam_species (data)
#' 
#' @param data input dataframe with our query (set vocab= "pbdb" in the query)
#' @return  a plot with the time spam of the taxa selected (species, genus, etc.)
#' @export 
#' @examples \dontrun{
#' canis_quaternary<- pbdb_query_occurrences (limit="all", vocab= "pbdb", base_name="Canis", interval="Quaternary", show="coords")
#' plot_time_spam (canis_quaternary, rank="species", names=TRUE)
#'}
#'
#'

pbdb_time_spam<- function (data, rank, col="skyblue2", names=TRUE){
  species<- data [data$taxon_rank==rank, ]
  max_sp<- aggregate(species$early_age, list(species$taxon_name), max)
  min_sp<- aggregate(species$late_age, list(species$taxon_name), min)
  temporal_range<- data.frame (max_sp [,2], min_sp[,2])
  row.names (temporal_range)<- max_sp[,1]
  colnames (temporal_range)<- c("max", "min")
  temporal_range<- temporal_range[with(temporal_range, order(-max, min)), ]
  pos<- c(1:dim (temporal_range)[1]-0.9)
  t_range<- cbind (temporal_range, pos)
  par(mar = c(4, 0, 1, 0))
  plot(c(min (t_range$min), max (t_range$max)),
       c(0, dim (t_range)[1]),
       type = "n",axes = FALSE, xlab = "Time (Ma)", ylab = "")
  segments(x0 = t_range$min,
           y0 = t_range$pos,
           x1 = t_range$max,
           y1 = t_range$pos,
           col = col,
           lwd = 6,
           lend = 2)
  axis(1, col="gray30", cex.axis=0.8)
  
  if (names==TRUE){
    text(x = t_range$min, y = t_range$pos +0.3,
         labels = row.names (t_range), adj=c(0,0), cex=0.8, col="gray30")
  }
}
