#' pbdb_temporal_resolution
#'
#' Shows the temporal resolution of the fossil data.
#'
#' @usage pbdb_temporal_resolution(data, do.plot = TRUE)
#'
#' @param data data.frame with our query to the paleobiodb
#'   \code{\link{pbdb_occurrences}}
#' @param do.plot logical. If \code{TRUE}, the function creates a
#'   frequency plot of the data.
#' @return a plot and a list with a summary of the temporal resolution
#'   of the data (min, max, 1st and 3rd quartiles, median and mean),
#'   and the temporal resolution of each fossil record (Ma).
#' @export
#' @examples \dontrun{
#'   data <- pbdb_occurrences(taxon_name = "Canidae", interval = "Quaternary")
#'   pbdb_temporal_resolution(data)
#' }
pbdb_temporal_resolution <- function(data, do.plot = TRUE) {
  if (!any(c("eag", "max_ma") %in% names(data))) {
    err_msg <- strwrap(
      paste(
        "No temporal information found in the provided data.frame.",
        "Make sure that it has a \"max_ma\" and a \"min_ma\" column",
        "(or \"eag\" and \"lag\" in compact form)."
      )
    )
    stop(paste(err_msg, collapse = "\n"))
  }

  early_age_col <- if ("eag" %in% names(data)) "eag" else "max_ma"
  late_age_col <- if ("lag" %in% names(data)) "lag" else "min_ma"

  diff <- as.numeric(data[[early_age_col]]) - as.numeric(data[[late_age_col]])
  tr <- list(summary = summary(diff), temporal_resolution = diff)

  if (do.plot) {
    hist(unlist(tr[[2]]), freq = TRUE, col = "#0000FF", border = FALSE,
         xlim = c(max(unlist(tr[[2]]), na.rm = TRUE), 0),
         breaks = 50, xlab = "Temporal resolution of the data (Ma)",
         main = "", col.lab = "grey30", col.axis = "grey30", cex.axis = 0.8)
  }

  tr
}

#' pbdb_temp_range
#' 
#' Constructs a plot and a dataframe with the temporal range of the
#' taxa (species, genera, families, etc.) within in a selected higher
#' taxon.
#' 
#' @usage pbdb_temp_range(data, rank, col = "#0000FF", names = TRUE, do.plot = TRUE)
#' 
#' @param data dataframe with our query to the paleobiodb \code{\link{pbdb_occurrences}}.
#' Important, it is required to show the name of the families, orders, etc. in the dataframe, 
#' to do that
#' set: show = c("classext", "ident") (see example).
#' @param rank to set which taxon rank you are interested.
#' @param col to change the colour of the bars in the plot, skyblue2 by default. 
#' @param names TRUE/FALSE (TRUE by default). To include or not the name of the taxa in the plot 
#' @param do.plot TRUE/FALSE (TRUE by default).
#' @return a plot and a dataframe with the time span of the taxa selected (species, genus, etc.)
#' @export 
#' @examples \dontrun{
#'   canis_quaternary <- pbdb_occurrences(
#'     limit = "all", base_name = "Canis", interval = "Quaternary",
#'     show = c("coords", "classext", "ident"), vocab = "pbdb"
#'   )
#'   pbdb_temp_range(canis_quaternary, rank = "species", names = FALSE)
#' }
pbdb_temp_range <- function(data, rank, col = "#0000FF", names = TRUE,
                            do.plot = TRUE) {
  temporal_range <- .extract_temporal_range(data, rank)

  if (do.plot) {
    pos <- seq_len(nrow(temporal_range)) - 0.9
    t_range <- cbind(temporal_range, pos)
    # Make right margin large enough to fit the longest name
    right_margin <- max(nchar(row.names(t_range))) * 0.2
    opar <- par(mar = c(4, 1, 1, right_margin))
    on.exit(par(opar))
    plot(c(min(t_range$min), max(t_range$max)),
      c(0, nrow(t_range)),
      type = "n", axes = FALSE,
      xlab = "Time (Ma)", ylab = "",
      xlim = c(max(t_range$max), min(t_range$min))
    )
    segments(
      x0 = t_range$min,
      y0 = t_range$pos,
      x1 = t_range$max,
      y1 = t_range$pos,
      col = col,
      lwd = 6,
      lend = 2
    )
    axis(1, col = "gray30", cex.axis = 0.8)
    if (names) {
      text(
        x = t_range$min, y = t_range$pos,
        labels = paste("  ", row.names(t_range)),
        adj = c(0, 0.5), cex = 0.5, col = "gray30", xpd = NA
      )
    }
  }

  temporal_range
}

.extract_temporal_range <- function(data, rank) {
  rank_lc <- tolower(rank)

  col_abbr <- c(
    accepted_name = "tna", genus = "gnl", family = "fml",
    order = "odl", class = "cll", phylum = "phl"
  )

  if (!("phylum" %in% names(data) || "phl" %in% names(data))) {
    stop(
      paste(
        "Cannot extract temporal range for rank from data without rank",
        "information. Please add 'show = c(\"classext\")' or",
        "'show = c(\"class\")' to your pbdb_occurrences query."
      )
    )
  }

  long_names <- "phylum" %in% names(data)

  rank_col <- rank_lc

  if (rank_lc == "species") {
    # This is the corresponding column for the species name in the
    # data.frame
    rank_col <- "accepted_name"

    # Only select records with an accepted name of species rank
    if (long_names) {
      data <- data[data$accepted_rank == "species", ]
    } else {
      # See <https://paleobiodb.org/data1.2/config.txt?show=ranks>
      data <- data[data$rnk == 3, ]
    }
  }

  if (!(rank_col %in% names(col_abbr))) {
    stop(paste("Unknown rank", rank))
  }
  rank_col <- if (long_names) rank_col else col_abbr[[rank_col]]
  early_age_col <- if (long_names) "max_ma" else "eag"
  late_age_col <- if (long_names) "min_ma" else "lag"

  early_age <- as.numeric(data[[early_age_col]])
  late_age <- as.numeric(data[[late_age_col]])

  max_sp <- tapply(early_age, as.character(data[[rank_col]]), max)
  min_sp <- tapply(late_age, as.character(data[[rank_col]]), min)

  temporal_range <- data.frame(max_sp, min_sp)
  colnames(temporal_range) <- c("max", "min")
  temporal_range <- temporal_range[with(temporal_range, order(-max, -min)), ]

  temporal_range
}

#' pbdb_richness
#' 
#' Plots temporal variation in taxon richness from a dataframe of occurrences.
#' 
#' @usage pbdb_richness(data, rank, res, temporal_extent, colour, bord, do.plot)
#' 
#' @param data dataframe with our query to the paleobiodb
#'   \code{\link{pbdb_occurrences}}.  Important, it is required to
#'   show the name of the families, orders, etc. in the dataframe, to
#'   do that set: show = c("classext", "ident") (see example).
#' @param rank to set which taxon rank you are interested. By default
#'   rank = "species"
#' @param colour to change the colour of the bars in the plot,
#'   skyblue2 by default.
#' @param bord to set the colour of the border of the polygon
#' @param temporal_extent vector to set the temporal extent (min, max)
#' @param res numeric. to set the intervals of the temporal extent
#' @param do.plot TRUE/FALSE (TRUE by default).
#' @export
#' @return a plot and a dataframe with the richness aggregated by the
#'   taxon rank in the specified temporal extent and resolution.
#' 
#' @examples \dontrun{
#'   data <- pbdb_occurrences(
#'     limit = "all", vocab = "pbdb", base_name = "Canidae",
#'     show = c("classext", "ident")
#'   )
#'   pbdb_richness(data, rank = "species", res = 1, temporal_extent = c(0, 3))
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
#' @usage
#' pbdb_orig_ext(data, rank, temporal_extent, res, orig_ext,
#'               colour = "#0000FF30", bord = "#0000FF", do.plot = TRUE)
#' 
#' @param data dataframe with our query to paleobiodb \code{\link{pbdb_occurrences}}.
#' Important, it is required to show the name of the families, orders, etc. in the dataframe, 
#' to do that set: show=c("classext", "ident") (see example).
#' @param rank to set which taxon rank you are interested. By default rank= "species"
#' @param temporal_extent vector to set the temporal extent (min, max)
#' @param res numeric. to set the intervals of the temporal extent
#' @param orig_ext 1 = origination, 2 = extinction.
#' @param colour to change the colour of the bars in the plot, skyblue2 by default. 
#' @param bord to set the colour of the border of the polygon
#' @param do.plot TRUE/FALSE (TRUE by default).
#' @export 
#' @return a  dataframe with the 
#' number of first appearances and extinctions of the selected taxon rank across time, 
#' and a plot with the first appearances or extinctions of the selected taxon rank across time.
#' 
#' @examples \dontrun{
#'   canidae <- pbdb_occurrences(
#'     limit = "all", vocab = "pbdb",
#'     base_name = "Canidae", show = c("classext", "ident")
#'   )
#' 
#'   # Plot of the evolutionary rates
#'   pbdb_orig_ext(
#'     canidae, rank = "genus", temporal_extent = c(0, 10), res = 1, orig_ext = 1
#'   )
#' 
#'   # Plot of the extinction rates
#'   pbdb_orig_ext(
#'     canidae, rank = "species", temporal_extent = c(0, 10), res = 1, orig_ext = 2
#'   )
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
