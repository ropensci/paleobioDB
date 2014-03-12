#' pbdb_temporal_resolution
#' 
#' to show the temporal resolution of the fossil data
#' 
#' @usage pbdb_temporal_resolution (data)
#' 
#' @param data input dataframe with our query
#' @param do.plot TRUE/FALSE. To show a frequency plot of the data (TRUE by default).
#' @return a list with a summary of the temporal resolution of the data 
#' (min, max, 1st and 3rd quartils, median and mean), and the temporal resolution of each fossil record (Ma).
#' @export 
#' @examples \dontrun{
#' data<- pbdb_occurrences (taxa= "Canidae", time= "Quaternary")
#' pbdb_temporal_resolution (data)
#'}
#'
#'
pbdb_temporal_resolution<- function (data, do.plot=TRUE) {
  tr<- list (summary=summary (data$eag - data$lag), temporal_resolution=(data$eag - data$lag))
  if (do.plot ==TRUE) {
  hist (unlist (tr [[2]]), freq=T, col="skyblue2", border=F,
        breaks= 50, xlab="Temporal resolution of the data (Ma)", 
        main="")
  }
  return (tr)
}

#' pbdb_time_spam
#' 
#' to show the time spam of a selected taxon rank included in the query
#' 
#' @usage pbdb_time_spam (data)
#' 
#' @param data input dataframe with our query (set vocab= "pbdb" in the query)
#' @param rank to set which taxon rank you are interested. By default rank= "species"
#' @param col to change the colour of the bars in the plot, skyblue2 by default. 
#' @names to include or not the name of the taxa in the plot (TRUE by default)
#' @return  a plot with the time spam of the taxa selected (species, genus, etc.)
#' @export 
#' @examples \dontrun{
#' canis_quaternary<- pbdb_occurrences (limit="all", vocab= "pbdb", base_name="Canis", interval="Quaternary", show="coords")
#' pbdb_time_spam (canis_quaternary, rank="species", names=TRUE)
#'}
#'
#'

pbdb_time_spam<- function (data, rank="species", col="skyblue2", names=TRUE){
  
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
  return (temporal_range)
}


#' pbdb_ext_evo
#' 
#' To plot the extinct and new taxa across time.
#' 
#' 
#' @usage pbdb_ext_evo (data)
#' 
#' @param data our query to the PBDB database
#' @rank to set the rank of the taxa of interest, species by default.
#' @return a plot
#' 
#' @examples \dontrun{
#' canidae<-  pbdb_query_occurrences (limit="all", vocab="pbdb",
#' base_name="Canidae")
#' pbdb_ext_evo (canidae, rank="genus")
#' pbdb_ext_evo (canidae, rank="species")
#'}
#' 
#' 
??gridSample
pbdb_ext_evo<- function (data, rank="species", colour=c("black","skyblue2")) { 
  
if (length (data$taxon_rank)!=0){ 
species<- data [data$taxon_rank==rank, ]
max_sp<- aggregate(species$early_age, list(species$taxon_name), max)
min_sp<- aggregate(species$late_age, list(species$taxon_name), min)
}

if (length (data$rnk)!=0){
  rnnk<- data.frame (c("species", "genera", "families", "orders", "classes"), 
                     c(3,5,9,13,15))
  rnkk<- rnnk [match (rank, rnnk[,1]), 2]
  species<- data [data$rnk==rnkk, ]
  max_sp<- aggregate(species$eag, list(species$tna), max)
  min_sp<- aggregate(species$lag, list(species$tna), min)   
}

  temporal_range<- data.frame (max_sp [,2], min_sp[,2])
  row.names (temporal_range)<- max_sp[,1]
  colnames (temporal_range)<- c("max", "min")
  temporal_range<- temporal_range[with(temporal_range, order(-max, min)), ]
  evo<- as.data.frame (table (temporal_range[,1]), stringsAsFactors=F)
  ext<- as.data.frame (table (temporal_range[,2]), stringsAsFactors=F)
  ext<- ext [ext$Var1!=0,]
  evo$Var1<- as.numeric (evo$Var1)
  ext$Var1<- as.numeric (ext$Var1)
   
  ymx<- max (c(evo[,2], ext[,2]))
  ymm<- min (c(evo[,2], ext[,2]))
  
  xmx<- max (c(evo[,1], ext[,1]))
  xmm<- min (c(evo[,1], ext[,1]))
  
  par (mar=c(4,4,2,2))
  plot (evo, xlab="Time (Ma)", type="o", lty=2, pch=16, col= colour[1],
        ylab=paste ("Number of", rank), axes=FALSE, xlim=c(xmm-1, xmx+1),
        ylim=c(ymm-1, ymx+1))
  lines (ext, type="o", lty=2, pch=16, col=colour[2])
  legend("topright", c("evolution","extinction"), cex=0.8, 
         col=colour, pch=16:16, lty=2:2)
  axis (1)
  axis (2)
}

#' pbdb_richness
#' 
#' plot richness across time
#' 
#' 

x<- 1920:1970
y1<- rnorm (51)
y2<- rnorm (51)
dev.off()
plot.new()
par (font.lab=1, col.lab="grey20", col.axis="grey50", cex.axis=0.8)
plot.window(xlim=c(1920,1970), xaxs="i",
            ylim=c(0,max(y)), yaxs="i")

rect(xleft=min(x), ybottom=min(y), xright=max(x), ytop=max(y), 
     density = NULL, angle = 45,
     col = "#88888890", border = NA)
abline(v=seq(min(x), max(x), by=1), col="#FFFFFF40")
abline(h=seq(0, max(y), by=0.5), col="#FFFFFF40")
xx = c(min(x), x, max(x))
yy = c(0, y, 0)
polygon(xx, yy, col="#FFFFFF40", border=NA)
axis(1)
axis(2, las=1)
mtext("Years", line=3, adj=1, side=1)
mtext("Extinctions", line= 3 , adj=1, side=2)


y<- y2
xx = c(min(x), x, max(x))
yy = c(0, y, 0)
polygon(xx, yy, col="#FF000050", border=NA)





pbdb_richness <- function (data, rank= "species", 
                           resolution=1, 
                           temporal_extent=c(0,100)){
  if (length (data$taxon_rank)!=0){
  species<- data [data$taxon_rank==rank, ]
  max_sp<- aggregate(species$early_age, list(species$taxon_name), max)
  min_sp<- aggregate(species$late_age, list(species$taxon_name), min)
}
if (length (data$rnk)!=0){
  rnnk<- data.frame (c("species", "genera", "families", "orders", "classes"), 
                    c(3,5,9,13,15))
  rnkk<- rnnk [match (rank, rnnk[,1]), 2]
  species<- data [data$rnk==rnkk, ]
}

  max_sp<- aggregate(species$eag, list(species$tna), max)
  min_sp<- aggregate(species$lag, list(species$tna), min) 
  temporal_range<- data.frame (max_sp [,2], min_sp[,2])
  row.names (temporal_range)<- max_sp[,1]
  colnames (temporal_range)<- c("max", "min")
  temporal_range<- temporal_range[with(temporal_range, order(-max, min)), ]
  te<- temporal_extent
  sequence<- seq (from=min(te), to= (max(te)), by=resolution)
  
  a<- temporal_range [,2]<=min(te)
    for (i in sequence[2:(length (sequence)-1)]) {  
      b<- temporal_range [,1]>=sequence [i] & temporal_range [,2]<=sequence [i+1]
      a<- cbind (a,b)
    }
  b<- temporal_range [,1]>=max(te)
  a<- cbind (a,b)
  richness<- colSums (a+0, na.rm=T)
  richness<- data.frame (sequence, richness)

  plot.new()
  par (font.lab=1, col.lab="grey20", col.axis="grey50", cex.axis=0.8)
  plot.window(xlim=c(max (te),min(te)), xaxs="i",
              ylim=c(0,(max(richness [,2]))+(max(richness [,2])/10)), yaxs="i")
  
  rect(xleft=min(te), ybottom=min(richness [,2]), 
       xright=max(te), ytop=max(richness [,2])+(max(richness [,2])/10), 
       density = NULL, angle = 45,
       col = "#88888890", border = NA)
  abline(v=seq(min(te), max(te), by=1), col="#FFFFFF40")
  abline(h=seq(0, max(richness [,2])+(max(richness [,2])/10), 
               by=(max(richness [,2])/10)), col="#FFFFFF40")
  xx = c(min(te), sequence, max(te))
  yy = c(0, richness[,2], 0)
  polygon(xx, yy, col="#8470FF", border="#7D9EC0")
  axis(1)
  axis(2, las=1)
  mtext("Million years before present", line=3, adj=1, side=1)
  mtext(paste ("Number of", rank), line= 3 , adj=0, side=2)
  return (richness)
}

