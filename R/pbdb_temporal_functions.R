#' pbdb_temporal_resolution
#' 
#' to show the temporal resolution of the fossil data
#' 
#' @usage pbdb_temporal_resolution (data, do.plot=TRUE)
#' 
#' @param data dataframe with our query to the paleoBD \code{\link{pbdb_occurrences}} 
#' @param do.plot TRUE/FALSE. To show a frequency plot of the data (TRUE by default).
#' @return a plot and a list with a summary of the temporal resolution of the data 
#' (min, max, 1st and 3rd quartils, median and mean), and the temporal resolution of each fossil record (Ma).
#' @export 
#' @examples \dontrun{
#' data<- pbdb_occurrences (taxon_name= "Canidae", interval= "Quaternary")
#' pbdb_temporal_resolution (data)
#'}


pbdb_temporal_resolution<- function (data, do.plot=TRUE) {
  if('eag' %in% colnames(data)) {
    diff <- .numeric_age(data$eag) - .numeric_age(data$lag)
    tr<- list (summary=summary (diff), temporal_resolution=diff)
  }
  
  if('early_age' %in% colnames(data)) {
    diff <- .numeric_age(data$early_age) - .numeric_age(data$late_age)
    tr<- list (summary=summary (diff), temporal_resolution=diff)
    
  }
  
  if (do.plot ==TRUE) {
    
    hist (unlist (tr [[2]]), freq=T, col="#0000FF", border=F, 
          xlim= c(max(unlist (tr [[2]]), na.rm = TRUE), 0),
          breaks= 50, xlab="Temporal resolution of the data (Ma)", 
          main="", col.lab="grey30", col.axis="grey30", cex.axis=0.8)
  }
  return (tr)
}

#' pbdb_temp_range
#' 
#' constructs a plot and a dataframe with the temporal range of the taxa (species, genera, families, etc.) within in a selected higher taxon. 
#' 
#' @usage pbdb_temp_range (data, rank, col = "#0000FF", 
#' names = TRUE, do.plot =TRUE)
#' 
#' @param data dataframe with our query to the paleoBD \code{\link{pbdb_occurrences}}. 
#' Important, it is required to show the name of the families, orders, etc. in the dataframe, 
#' to do that
#' set: show=c("phylo", "ident") (see example).
#' @param rank to set which taxon rank you are interested.
#' @param col to change the colour of the bars in the plot, skyblue2 by default. 
#' @param names TRUE/FALSE (TRUE by default). To include or not the name of the taxa in the plot 
#' @param do.plot TRUE/FALSE (TRUE by default).
#' @return a plot and a dataframe with the time span of the taxa selected (species, genus, etc.)
#' @export 
#' @examples \dontrun{
#' canis_quaternary<- pbdb_occurrences (limit="all", base_name="Canis", 
#'                  interval="Quaternary", show=c("coords", "phylo", "ident"))
#' pbdb_temp_range (canis_quaternary, rank="species", names=FALSE)
#'}
  

pbdb_temp_range<- function (data, rank, 
                             col="#0000FF", names=TRUE, 
                             do.plot=TRUE){
  
  temporal_range <- .extract_temporal_range(data, rank)

  if (do.plot==TRUE){
    pos<- c(1:dim (temporal_range)[1]-0.9)
    t_range<- cbind (temporal_range, pos)
    par(mar = c(4, 0, 1, 15))
    plot(c(min (t_range$max), max (t_range$max)),
         c(0, dim (t_range)[1]), 
         type = "n",axes = FALSE, 
         xlab = "Time (Ma)", ylab = "", 
         xlim=c(max (t_range$max), min (t_range$max)))
    segments(x0 = t_range$min,
             y0 = t_range$pos,
             x1 = t_range$max,
             y1 = t_range$pos,
             col = col,
             lwd = 6,
             lend = 2)
    axis(1, col="gray30", cex.axis=0.8)  
    if (names==TRUE){
      text(x = t_range$min - 0.3, y = t_range$pos,
      labels = row.names (t_range), adj=c(0,0), 
      cex=0.5, col="gray30") 
    }
  }
  
  return (temporal_range)
}

.extract_temporal_range <- function (data, rank) {

  if ('taxon_rank' %in% colnames(data)) {
    long_names <- TRUE
  } else if ('rnk' %in% colnames(data)) {
    long_names <- FALSE
  } else {
    stop("Cannot extract temporal range from data. Please add show=c('phylo', 'ident') to your pbdb_occurrences query")
  }
  
  early_age_col <- if (long_names) 'early_age' else 'eag'
  late_age_col <- if (long_names) 'late_age' else 'lag'
  matched_rank_col <- if (long_names) 'matched_rank' else 'mra'
  matched_name_col <- if (long_names) 'matched_name' else 'mna'
  genus_col <- if (long_names) 'genus' else 'gnl'
  family_col <- if (long_names) 'family' else 'fml'
  order_col <- if (long_names) 'order' else 'odl'
  class_col <- if (long_names) 'class' else 'cll'
  phylum_col <- if (long_names) 'phylum' else 'phl'
  
  if (rank=="species"){
    selection<- data [data[, matched_rank_col]==rank, ]
    max_sp<- tapply(.numeric_age(selection[, early_age_col]), as.character (selection[, matched_name_col]), max)
    min_sp<- tapply(.numeric_age(selection[, late_age_col]), as.character (selection[, matched_name_col]), min)
  } else {
    
    early_age = .numeric_age(data[, early_age_col])
    late_age = .numeric_age(data[, late_age_col])
    
    if (rank=="genus"){
      max_sp<- tapply(early_age, as.character (data[, genus_col]), max)
      min_sp<- tapply(late_age, as.character (data[, genus_col]), min)
    } else if (rank=="family"){
      max_sp<- tapply(early_age, as.character (data[, family_col]), max)
      min_sp<- tapply(late_age, as.character (data[, family_col]), min)
    } else if (rank=="order"){
      max_sp<- tapply(early_age, as.character (data[, order_col]), max)
      min_sp<- tapply(late_age, as.character (data[, order_col]), min)
    } else if (rank=="class"){
      max_sp<- tapply(early_age, as.character (data[, class_col]), max)
      min_sp<- tapply(late_age, as.character (data[, class_col]), min)
    } else if (rank=="phylum"){
      max_sp<- tapply(early_age, as.character (data[, phylum_col]), max)
      min_sp<- tapply(late_age, as.character (data[, phylum_col]), min)
    } else {
      stop(paste("Unknown rank", rank))
    }
  }

  temporal_range<- data.frame (max_sp, min_sp)
  colnames (temporal_range)<- c("max", "min")
  temporal_range<- temporal_range[with(temporal_range, order(-max, min)), ]

  return (temporal_range)
}

.numeric_age <- function (f) { as.numeric(as.character(f)) }

#' pbdb_richness
#' 
#' Plots the number of the interested.
#' 
#' @usage pbdb_richness (data, rank, res, temporal_extent, colour, bord, do.plot)
#' 
#' @param data dataframe with our query to the paleoBD \code{\link{pbdb_occurrences}}. 
#' Important, it is required to show the name of the families, orders, etc. in the dataframe, 
#' to do that
#' set: show=c("phylo", "ident") (see example).
#' @param rank to set which taxon rank you are interested. By default rank= "species"
#' @param colour to change the colour of the bars in the plot, skyblue2 by default. 
#' @param bord to set the colour of the border of the polygon
#' @param temporal_extent vector to set the temporal extent (min, max)
#' @param res numeric. to set the intervals of the temporal extent
#' @param do.plot TRUE/FALSE (TRUE by default).
#' @export 
#' @return a plot and a dataframe with the richness aggregated by the taxon rank in the specified temporal extent and resolution.
#' 
#' @examples \dontrun{
#' data<-  pbdb_occurrences (limit="all", vocab="pbdb",
#' base_name="Canidae", show=c("phylo", "ident"))
#' pbdb_richness (data, rank="species", res=1, temporal_extent=c(0,3))
#'}
 

pbdb_richness <- function (data, rank, 
                           res=1, 
                           temporal_extent=c(0,10), 
                           colour="#0000FF30", 
                           bord="#0000FF", 
                           do.plot=TRUE){
  
  temporal_range<- pbdb_temp_range (data=data, rank=rank,do.plot=FALSE)
  te<- temporal_extent
  time<- seq (from=min(te), to= (max(te)), by=res)
  
  means<- NULL
  for (i in 1:length (time)-1){
    x<- (time [i +1] + time [i])/2
    means<- c(means, x)
  }
  a<- NULL
  for (i in 1:(length (time)-1)) {
    b<- temporal_range [,1]>time[i] & temporal_range [,2]<=time [i+1]
    a<- cbind (a,b)
  }
  
  richness<- colSums (a+0, na.rm=T)
  temporal_intervals<- paste (time[-length (time)], time[-1], sep="-")
  richness<- data.frame (temporal_intervals, richness)
  if (do.plot==TRUE) {
    plot.new()
    par (mar=c(5,5,1,5), font.lab=1, col.lab="grey20", col.axis="grey50", 
         cex.axis=0.8)
    plot.window(xlim=c(max (te),min(te)), xaxs="i",
                ylim=c(0,(max(richness [,2]))+(max(richness [,2])/10)), yaxs="i")
    
    abline(v=seq(min(te), max(te), by=res), col="grey90", lwd=1)
    abline(h=seq(0, max(richness [,2])+(max(richness [,2])/10), 
                 by=(max(richness [,2])/10)), col="grey90", lwd=1)
    xx <- c(means [1], means, means [length (means)])
    yy <- c(0, richness[,2], 0)
    polygon(xx, yy, col=colour, border=bord)
    axis(1, line=1, las=2, labels=temporal_intervals, 
         at=means)
    axis(2, line=1, las=1)
    mtext("Million years before present", line=3.5, adj=1, side=1)
    mtext("Richness", line= 3.5 , adj=0, side=2)
  }
  return (richness)
}



#' pbdb_orig_ext
#' 
#' Plots the appearance of new taxa across time.
#' 
#' @usage pbdb_orig_ext (data, rank, 
#' temporal_extent, res, orig_ext,  
#' colour="#0000FF30", bord="#0000FF", do.plot=TRUE)
#' 
#' @param data dataframe with our query to the paleoBD \code{\link{pbdb_occurrences}}. 
#' Important, it is required to show the name of the families, orders, etc. in the dataframe, 
#' to do that set: show=c("phylo", "ident") (see example).
#' @param rank to set which taxon rank you are interested. By default rank= "species"
#' @param temporal_extent vector to set the temporal extent (min, max)
#' @param res numeric. to set the intervals of the temporal extent
#' @param orig_ext 1= origination, 2=extinction.
#' @param colour to change the colour of the bars in the plot, skyblue2 by default. 
#' @param bord to set the colour of the border of the polygon
#' @param do.plot TRUE/FALSE (TRUE by default).
#' @export 
#' @return a  dataframe with the 
#' number of first appearances and extinctions of the selected taxon rank across time, 
#' and a plot with the first appearances or extinctions of the selected taxon rank across time.
#' 
#' @examples \dontrun{
#' canidae<-  pbdb_occurrences (limit="all", vocab="pbdb",
#' base_name="Canidae", show=c("phylo", "ident"))
#' 
#' # plot of the evolutive rates.
#' pbdb_orig_ext (canidae, rank="genus", temporal_extent=c(0, 10), 
#' res=1, orig_ext=1) 
#' 
#' # plot of the extinction rates.
#' pbdb_orig_ext (canidae, rank="species", temporal_extent=c(0, 10), 
#' res=1, orig_ext=2) 
#'}



pbdb_orig_ext<- function (data, rank, temporal_extent, 
                          res, orig_ext=1, 
                          colour="#0000FF30", bord="#0000FF", 
                          do.plot=TRUE) { 
  
  temporal_range<- pbdb_temp_range (data=data, rank=rank, do.plot=FALSE)
  te<- temporal_extent
  sequence<- seq (from=min(te), to= (max(te)), by=res)
  intv<- data.frame (min=sequence [1:length (sequence)-1], 
                     max=sequence [2:length (sequence)]) 
  labels1<- paste (intv[,1], intv[,2], sep="-")
  labels2<- paste (labels1[2:(length (labels1))],
                   labels1[1:(length (labels1)-1)], 
                   sep=" to ")
  
  res_sp<- list ()
  for (i in 1:dim(intv)[1])
  {
    intvv<- intv [i,]
    cases1<-  which (as.numeric (temporal_range$min)>= intvv$min &
                       as.numeric (temporal_range$min)<= intvv$max &
                       as.numeric (temporal_range$max)>= intvv$max)
    
    cases2<-  which (as.numeric (temporal_range$min)<= intvv$min &
                       as.numeric (temporal_range$max)<= intvv$max &
                       as.numeric (temporal_range$max)>= intvv$min)
    
    cases3<-  which (as.numeric (temporal_range$min)<= intvv$min &
                       as.numeric (temporal_range$max)>= intvv$max)
    
    cases<- unique (c(cases1, cases2, cases3))
    sps<-temporal_range [cases,]
    res_sp[[i]]<- sps
  }
  
  change<- data.frame ()
  for (i in length (res_sp):2)
  {
    new_taxa<- length (setdiff (row.names (res_sp[[i-1]]), row.names (res_sp[[i]])))
    ext<- length (setdiff (row.names (res_sp[[i]]), row.names (res_sp[[i-1]])))
    col<- c(new_taxa, ext)
    change<- rbind (change, col)
  }  
  
  names (change)<- c("new", "ext")
  change<- change[rev(as.numeric (row.names(change))),]
  row.names (change)<- labels2
  
  if (do.plot==TRUE){
    ymx<- max (change[,orig_ext])
    ymn<- min (change[,orig_ext])
    xmx<- sequence[length (sequence)-1]
    xmn<- sequence [2]
    plot.new()
    par (mar=c(5,5,2,5),font.lab=1, col.lab="grey20", col.axis="grey50", cex.axis=0.8)
    plot.window(xlim=c(xmx, xmn), xaxs="i",
                ylim=c(ymn,ymx), yaxs="i")
    abline(v=seq(xmn, xmx, by=res), col="grey90", lwd=1)
    abline(h=seq(0, ymx, 
                 by=(ymx/10)), col="grey90", lwd=1)
    xx <- c(xmn,  sequence[2:(length (sequence)-1)], xmx)
    yy <- c(0, change[,orig_ext], 0)
    polygon(xx, yy, col=colour, border=bord)
    
    axis(1, line=1, labels=labels2, at= xx [-c(1,length (xx))])
    axis(2, line=1, las=1)
    mtext("Million years before present", line=3, adj=1, side=1)
    mtext(paste ("Number of ", rank, sep=""), line= 3 , adj=0, side=2)
    title (ifelse (orig_ext==1,"First appearences", "Last appearences"))
  }
  return (change)
}