#' pbdb_subtaxa
#' 
#' count the number of subtaxa within a given taxa. 
#' e.g. number of species within a genus. 
#' 
#' @usage pbdb_subtaxa (data, do.plot, col)
#' 
#' @param data dataframe with our query to the 
#' paleoBD \code{\link{pbdb_occurrences}} 
#' @param do.plot by default this function make a plot to 
#' visualize the distribution of taxa. Set to FALSE to skip the plot.
#' @param col set the colour of the histogram. skyblue2 by default.
#' @return a plot and a dataframe with the number of subtaxa in the data.
#' @export 
#' @examples \dontrun{
#' canidae_quat<-  pbdb_occurrences (limit="all", 
#' base_name="Canidae",  interval="Quaternary", 
#' show=c("coords", "phylo", "ident")
#' pbdb_subtaxa (canidae_quat)
#'}
#'

pbdb_subtaxa<- function (data, 
                         do.plot=TRUE,  
                         col="#0000FF"){
  
  species<- nrow (pbdb_time_span (data=data, rank="species",do.plot=FALSE))
  genera<- nrow (pbdb_time_span(data=data, rank="genus",do.plot=FALSE))
  families<- nrow (pbdb_time_span (data=data, rank="family",do.plot=FALSE))
  orders<- nrow (pbdb_time_span (data=data, rank="order",do.plot=FALSE))
  classes<- nrow (pbdb_time_span (data=data, rank="class",do.plot=FALSE))
  phyla<- nrow (pbdb_time_span (data=data, rank="phylum",do.plot=FALSE))
  subtaxa<- data.frame (species, genera, families, orders, classes, phyla)

if (do.plot==TRUE){
  par (mar=c(8,4,2,0))
  barplot (unlist (subtaxa),  
           beside = T, horiz=F,
           col=col,
           border=F,
           las=2)
}
  return (subtaxa)
}


  

