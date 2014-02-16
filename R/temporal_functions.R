#' to show the temporal resolution of the fossil data
#' 
#' @usage temporal_resolution (data)
#' 
#' @param data input dataframe with our query
#' @return a list with a summary of the temporal resolution of the data 
#' (min, max, 1st and 3rd quartils, median and mean), and the temporal resolution of each fossil record.
#' @export 
#' @examples \dontrun{
#' data<- pbdbQueryTaxa (taxa= "Canidae", time= "Quaternary")
#' temporal_resolution (data)
#'}
#'
#'
temporal_resolution<- function (data) {
  list (summary=summary (data$eag - data$lag), temporal_resolution=(data$eag - data$lag))
}


time_spam_species<- function (data){
 
  species<- data [data$rnk==3, ]
  max_sp<- aggregate(species$eag, list(species$tna), max)
  min_sp<- aggregate(species$lag, list(species$tna), min)
  temporal_range<- data.frame (max_sp [,2], min_sp[,2])
  row.names (temporal_range)<- max_sp[,1]
  colnames (temporal_range)<- c("max", "min")
  pos<- c(1:dim (temporal_range)[1])
  t_range<- cbind (temporal_range, pos)
  
  plot(c(min (t_range$min), max (t_range$max)),
       c(0, dim (t_range)[1]+1),
       type = "n",axes = FALSE, xlab = "Time (Ma)", ylab = "")
  segments(x0 = t_range$min,
           y0 = t_range$pos,
           x1 = t_range$max,
           y1 = t_range$pos,
           col = "red",
           lwd = 6,
           lend = 2)
  text(x = t_range$min, y = t_range$pos +0.3,
       labels = row.names (t_range), adj=c(0,0), cex=0.8, col="gray30")
  axis(1, col="gray30") 
}


order_by_appearance<- t_range [order (t_range$max),]
t_range<- order_by_appearance
t_range$pos<- c(1:dim (t_range)[1])

plot(c(min (t_range$min), max (t_range$max)),
     c(0, dim (t_range)[1]+1),
     type = "n",axes = FALSE, xlab = "Time (Ma)", ylab = "")
segments(x0 = t_range$min,
         y0 = t_range$pos,
         x1 = t_range$max,
         y1 = t_range$pos,
         col = "red",
         lwd = 6,
         lend = 2)
text(x = t_range$min, y = t_range$pos +0.3,
     labels = row.names (t_range), adj=c(0,0), cex=0.6, col="gray30")
axis(1, col="gray30") 




