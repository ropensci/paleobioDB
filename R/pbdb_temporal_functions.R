#' pbdb_temporal_resolution
#' 
#' to show the temporal resolution of the fossil data
#' 
#' @usage pbdb_temporal_resolution (data)
#' 
#' @param data dataframe with our query to the paleoBD \code{\link{pbdb_occurrences}} 
#' @param do.plot TRUE/FALSE. To show a frequency plot of the data (TRUE by default).
#' @return a plot and a list with a summary of the temporal resolution of the data 
#' (min, max, 1st and 3rd quartils, median and mean), and the temporal resolution of each fossil record (Ma).
#' @export 
#' @examples \dontrun{
#' data<- pbdb_occurrences (taxa= "Canidae", time= "Quaternary")
#' pbdb_temporal_resolution (data)
#'}


pbdb_temporal_resolution<- function (data, do.plot=TRUE) {
  if('eag' %in% colnames(data)) {
  tr<- list (summary=summary (data$eag - data$lag), 
             temporal_resolution=(data$eag - data$lag))
  }
  
  if('early_age' %in% colnames(data)) {
    tr<- list (summary=summary (data$early_age - data$late_age), 
               temporal_resolution=(data$early_age - data$late_age))
    
  }
  
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
#' @param data dataframe with our query to the paleoBD \code{\link{pbdb_occurrences}} 
#' @param rank to set which taxon rank you are interested. By default rank= "species"
#' @param col to change the colour of the bars in the plot, skyblue2 by default. 
#' @param names TRUE/FALSE (TRUE by default). To include or not the name of the taxa in the plot 
#' @param do.plot TRUE/FALSE (TRUE by default).
#' @return a plot and a dataframe with the time spam of the taxa selected (species, genus, etc.)
#' @export 
#' @examples \dontrun{
#' canis_quaternary<- pbdb_occurrences (limit="all", vocab= "pbdb", base_name="Canis", interval="Quaternary", show="coords")
#' pbdb_time_spam (canis_quaternary, rank="species", names=TRUE)
#'}
  

pbdb_time_spam<- function (data, rank="species", 
                             col="skyblue2", names=TRUE, 
                             do.plot=TRUE){
  if('taxon_rank' %in% colnames(data)) {
  
    if (rank=="species"){ 
  selection<- data [data$taxon_rank==rank, ]
  max_sp<- tapply(selection$early_age, list(selection$taxon_no), max)
  min_sp<- tapply(selection$late_age, list(selection$taxon_no), min)
  temporal_range<- data.frame (max_sp, min_sp)
  row.names (temporal_range)<- paste (selection$genus_name[match (row.names (temporal_range), 
                                                                  selection$taxon_no)], 
                                      selection$species_name [match (row.names (temporal_range), 
                                                                     selection$taxon_no)])
  
    }
  
  if (rank=="genus"){
    max_sp<- tapply(data$early_age, list(data$genus_name), max)
    min_sp<- tapply(data$late_age, list(data$genus_name), min)
    temporal_range<- data.frame (max_sp, min_sp)
  }
  
  if (rank=="family"){ 
    max_sp<- tapply(data$early_age, list(data$family), max)
    min_sp<- tapply(data$late_age, list(data$family), min)
    temporal_range<- data.frame (max_sp, min_sp)
  }
  if (rank=="order"){ 
    max_sp<- tapply(data$early_age, list(data$order), max)
    min_sp<- tapply(data$late_age, list(data$order), min)
    temporal_range<- data.frame (max_sp, min_sp)
  }
  if (rank=="class"){ 
    max_sp<- tapply(data$early_age, list(data$class), max)
    min_sp<- tapply(data$late_age, list(data$class), min)
    temporal_range<- data.frame (max_sp, min_sp)
  }
  
  if (rank=="phylum"){ 
    max_sp<- tapply(data$early_age, list(data$phylum), max)
    min_sp<- tapply(data$late_age, list(data$phylum), min)
    temporal_range<- data.frame (max_sp, min_sp)
  }
    }
  
  if('rnk' %in% colnames(data)) {
    
    if (rank=="species"){ 
      selection<- data [data$rnk==3, ]
      max_sp<- tapply(selection$eag, list(selection$tid), max)
      min_sp<- tapply(selection$lag, list(selection$tid), min)
      temporal_range<- data.frame (max_sp, min_sp)
      row.names (temporal_range)<- paste (selection$idt[match (row.names (temporal_range), 
                                                               selection$tid)], 
                                          selection$ids [match (row.names (temporal_range), 
                                                                selection$tid)])
    }
  
  if (rank=="genus"){
    max_sp<- tapply(data$eag, list(data$idt), max)
    min_sp<- tapply(data$lag, list(data$idt), min)
    temporal_range<- data.frame (max_sp, min_sp)
  }
  
  if (rank=="family"){ 
    max_sp<- tapply(data$eag, list(data$fml), max)
    min_sp<- tapply(data$lag, list(data$fml), min)
    temporal_range<- data.frame (max_sp, min_sp)
  }
  if (rank=="order"){ 
    max_sp<- tapply(data$eag, list(data$odn), max)
    min_sp<- tapply(data$lag, list(data$odn), min)
    temporal_range<- data.frame (max_sp, min_sp)
  }
  if (rank=="class"){ 
    max_sp<- tapply(data$eag, list(data$cll), max)
    min_sp<- tapply(data$lag, list(data$cll), min)
    temporal_range<- data.frame (max_sp, min_sp)
  }
  
  if (rank=="phylum"){ 
    max_sp<- tapply(data$eag, list(data$phl), max)
    min_sp<- tapply(data$lag, list(data$phl), min)
    temporal_range<- data.frame (max_sp, min_sp)
  }
  
}
    
    colnames (temporal_range)<- c("max", "min")
    temporal_range<- temporal_range[with(temporal_range, order(-max, min)), ]
    pos<- c(1:dim (temporal_range)[1]-0.9)
    t_range<- cbind (temporal_range, pos)
  
  if (do.plot==TRUE){
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
  
  return (temporal_range)
}



#' pbdb_richness
#' 
#' Plots the number of the interested.
#' 
#' @usage pbdb_evo (data)
#' 
#' @param data ataframe with our query to the paleoBD \code{\link{pbdb_occurrences}} 
#' @rank to set the rank of the taxa of interest, species by default.
#' @param resolution numeric. Sets the temporal resolution for the analysis in Millions of years (Ma)
#' @param temporal_extent vector to set the temporal extent of the analysis (Ma)
#' @param colour sets the colour of the plot
#' @param bord sets the colour of the border of the polygon
#' @param do.plot TRUE/FALSE. TRUE by default.
#' 
#' @return a plot and a dataframe with the richness aggregated by the taxon rank in the specified temporal extent and resolution.
#' 
#' @examples \dontrun{
#' data<-  pbdb_query_occurrences (limit="all", vocab="pbdb",
#' base_name="Canidae")
#' pbdb_richness (data, rank="family", resolution=1, temporal_extent=c(0,3))
#'}
#' 

pbdb_richness <- function (data, rank= "species", 
                           resolution=1, 
                           temporal_extent=c(0,100), 
                           colour="#0000FF30", 
                           bord="#0000FF", 
                           do.plot=TRUE){
  if (length (data$taxon_rank)!=0){
    species<- data [data$taxon_rank==rank, ]
    max_sp<- aggregate(species$early_age, list(species$taxon_name), max)
    min_sp<- aggregate(species$late_age, list(species$taxon_name), min)
  }
  if (length (data$rnk)!=0){
    rnnk<- data.frame (c("species", "genus", "family", "order", "class"), 
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
  par (mar=c(5,5,0,5), font.lab=1, col.lab="grey20", col.axis="grey50", cex.axis=0.8)
  plot.window(xlim=c(max (te),min(te)), xaxs="i",
              ylim=c(0,(max(richness [,2]))+(max(richness [,2])/10)), yaxs="i")
  
  abline(v=seq(min(te), max(te), by=1), col="grey90", lwd=1)
  abline(h=seq(0, max(richness [,2])+(max(richness [,2])/10), 
               by=(max(richness [,2])/10)), col="grey90", lwd=1)
  xx = c(min(te), sequence, max(te))
  yy = c(0, richness[,2], 0)
  polygon(xx, yy, col=colour, border=bord)
  axis(1)
  axis(2, las=1)
  mtext("Million years before present", line=3, adj=1, side=1)
  mtext("Richness", line= 3 , adj=0, side=2)
  return (richness)
}


#' pbdb_evo
#' 
#' Plots the appearance of new taxa across time.
#' 
#' @usage pbdb_evo (data)
#' 
#' @param data ataframe with our query to the paleoBD \code{\link{pbdb_occurrences}} 
#' @rank to set the rank of the taxa of interest, species by default.
#' @param colour sets the colour of the plot
#' @param bord sets the colour of the border of the polygon
#' @param do.plot TRUE/FALSE. TRUE by default.
#' @return a plot
#' 
#' @examples \dontrun{
#' canidae<-  pbdb_query_occurrences (limit="all", vocab="pbdb",
#' base_name="Canidae")
#' pbdb_evo (canidae, rank="genus")
#' pbdb_evo (canidae, rank="species")
#'}
#' 


pbdb_evo<- function (data, rank="species", 
                     colour="#0000FF30", bord="#0000FF", do.plot=TRUE) { 
  
if (length (data$taxon_rank)!=0){ 
species<- data [data$taxon_rank==rank, ]
max_sp<- aggregate(species$early_age, list(species$taxon_name), max)
min_sp<- aggregate(species$late_age, list(species$taxon_name), min)
}

if (length (data$rnk)!=0){
  rnnk<- data.frame (c("species", "genus", "family", "order", "class"), 
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
  evo$Var1<- as.numeric (evo$Var1)

  ymx<- max (evo[,2])
  ymn<- min (evo[,2])
  
  xmx<- max (evo[,1])
  xmn<- min (evo[,1])

if (do.plot==TRUE){
plot.new()
par (mar=c(5,5,2,5),font.lab=1, col.lab="grey20", col.axis="grey50", cex.axis=0.8)
plot.window(xlim=c(xmn, xmx), xaxs="i",
            ylim=c(ymn,ymx), yaxs="i")
abline(v=seq(xmn, xmx, by=1), col="grey90", lwd=1)
abline(h=seq(0, ymx, 
             by=(ymx/10)), col="grey90", lwd=1)
xx = c(xmn, evo$Var1, xmx)
yy = c(0, evo$Freq, 0)
polygon(xx, yy, col=colour, border=bord)

axis(1)
axis(2, las=1)
mtext("Million years before present", line=3, adj=1, side=1)
mtext(paste ("Taxon rank= ", rank), line= 3 , adj=0, side=2)
}

return (temporal_range)

}





pbdb_ext<- function (data, rank="species", colour="#0000FF30", bord="#0000FF") { 
  
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
  ext<- as.data.frame (table (temporal_range[,2]), stringsAsFactors=F)
  ext<- ext [ext$Var1!=0,]
  ext$Var1<- as.numeric (ext$Var1)
  
  ymx<- max (ext[,2])
  ymn<- min (ext[,2])
  
  xmx<- max (ext[,1])
  xmn<- min (ext[,1])
  
  plot.new()
  par (font.lab=1, col.lab="grey20", col.axis="grey50", cex.axis=0.8)
  plot.window(xlim=c(xmn, xmx), xaxs="i",
              ylim=c(ymn,ymx), yaxs="i")
  abline(v=seq(xmn, xmx, by=1), col="grey90", lwd=1)
  abline(h=seq(0, ymx, 
               by=(ymx/10)), col="grey90", lwd=1)
  xx = c(xmn, evo$Var1, xmx)
  yy = c(0, evo$Freq, 0)
  polygon(xx, yy, col=colour, border=bord)
  
  axis(1)
  axis(2, las=1)
  mtext("Million years before present", line=3, adj=1, side=1)
  mtext(paste ("Number of extinct", rank), line= 3 , adj=0, side=2)
}




pbdb_evo_rate<- function (data, rank="species", colour="#0000FF30", bord="#0000FF") { 
  
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
  evo$Var1<- as.numeric (evo$Var1)
  perc<- (evo$Freq[1:length (evo$Freq)-1]/evo$Freq[2:length (evo$Freq)])*100
  
  ymx<- max (perc)
  ymn<- min (perc)
  xmx<- max (evo[,1])
  xmn<- min (evo[,1])
  
  plot.new()
  par (font.lab=1, col.lab="grey20", col.axis="grey50", cex.axis=0.8)
  plot.window(xlim=c(xmn, xmx), xaxs="i",
              ylim=c(ymn,ymx), yaxs="i")
  abline(v=seq(xmn, xmx, by=1), col="grey90", lwd=1)
  abline(h=seq(0, ymx, 
               by=(ymx/10)), col="grey90", lwd=1)
  xx = c(xmn, evo$Var1, xmx)
  yy = c(0, c(0,perc), 0)
  polygon(xx, yy, col=colour, border=bord)
  
  axis(1)
  axis(2, las=1)
  mtext("Million years before present", line=3, adj=1, side=1)
  mtext(paste ("Percentage of new", rank), line= 3 , adj=0, side=2)
}


pbdb_ext_rate<- function (data, rank="species", colour="#0000FF30", bord="#0000FF") { 
  
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
  ext<- as.data.frame (table (temporal_range[,2]), stringsAsFactors=F)
  ext<- ext [ext$Var1!=0,]
  ext$Var1<- as.numeric (ext$Var1)
  perc<- (ext$Freq[1:length (ext$Freq)-1]/ext$Freq[2:length (ext$Freq)])*100
  
  ymx<- max (perc)
  ymn<- min (perc)
  xmx<- max (ext[,1])
  xmn<- min (ext[,1])
  
  plot.new()
  par (font.lab=1, col.lab="grey20", col.axis="grey50", cex.axis=0.8)
  plot.window(xlim=c(xmn, xmx), xaxs="i",
              ylim=c(ymn,ymx), yaxs="i")
  abline(v=seq(xmn, xmx, by=1), col="grey90", lwd=1)
  abline(h=seq(0, ymx, 
               by=(ymx/10)), col="grey90", lwd=1)
  xx = c(xmn, ext$Var1, xmx)
  yy = c(0, c(0,perc), 0)
  polygon(xx, yy, col=colour, border=bord)
  
  axis(1)
  axis(2, las=1)
  mtext("Million years before present", line=3, adj=1, side=1)
  mtext(paste ("Percentage of extinct", rank), line= 3 , adj=0, side=2)
}


