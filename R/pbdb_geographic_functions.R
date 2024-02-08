.extract.LatLong <- function(data) {
  latlong <- data.frame(lng = data$lng, lat = data$lat)
  spl_df <- split(latlong, list(latlong$lng, latlong$lat), drop = TRUE)
  spl_count <- lapply(spl_df, function(x) cbind(x[1, ], Occur = nrow(x)))
  counts <- do.call(rbind, spl_count)
  counts
}

.add.ColOcean <- function(col.ocean, col.int, ...) {
  par(mar = c(0, 0, 0, 0), xpd = TRUE, ...)
  maps::map(type = "n", ...)
  rect(
    par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4],
    col = col.ocean
  )
  maps::map(col = col.int, fill = TRUE, add = TRUE, ...)
}

#' @importFrom grDevices adjustcolor colorRampPalette
.add.Points <- function(Y, col.point, pch, ...) {
  Pal <- colorRampPalette(col.point)
  Y$n <- as.numeric(cut(Y$Occur, breaks = 5))
  Y$Col <- Pal(5)[Y$n]
  points(Y[, 1:2], col = adjustcolor(Y$Col, alpha.f = 0.8), pch = pch, ...)
  Y
}


.add.Legend <- function(Y1, col.int, pch,...){
    
    n<-length(unique(Y1$Col))
    Col<- unique(Y1$Col)[order(unique(Y1$n))]
    Legend<-as.integer (seq(min(Y1$Occur), max(Y1$Occur),
                            length.out= n))
    legend("bottom", col= Col, inset= c(0,-0.14), legend= Legend, ncol= n, 
           title="Occurrences",bg=col.int, pch=pch,...)
}  

#' pbdb_map
#'
#' Maps the fossil records
#'
#' The function opens a new window for the map
#'
#' @usage
#' pbdb_map(data, col.int = "white", pch = 19, col.ocean = "black",
#'          main = NULL, col.point = c("light blue", "blue"), ...)
#'
#' @param data Input dataframe. This dataframe is the output of \code{\link{pbdb_occurrences}} function using the
#' argument: \code{show = "coords"}. See too: \strong{Details} and \strong{Examples}
#' @param col.int The colour of the mainland.
#' @param pch See: \code{\link{par}}
#' @param col.ocean The colour of the ocean.
#' @param main To set the title of the map. See: \code{\link{par}}
#' @param col.point Two or more colours. To generate the colour gradient used to show the number of occurrences per cell in map
#' @param ... Other parameters. See \code{\link{par}} and \code{\link{map}}
#' @details \strong{CAUTION!} The argument \code{show = "coords"} in \code{\link{pbdb_occurrences}} function is required. 
#' We recommend the use of a cairo device (\code{\link{X11}}) for better visualization of the graphs. See \strong{Examples}
#' @return A map showing the distribution of the fossil records, with the points with a color gradient, according to the number of occurrences per cell.
#' @seealso See \code{\link{pbdb_occurrences}}, \code{\link{map}}, \code{\link{par}} and \code{\link{colors}} help pages
#' @export
#' @examples \dontrun{
#'   data <- pbdb_occurrences(
#'     limit = "all", vocab = "pbdb", base_name = "Canis", show = "coords"
#'   )
#'   X11(width = 12, height = 8)
#'   pbdb_map(data)
#'   pbdb_map(data, pch = 1)
#'   pbdb_map(
#'     data,
#'     pch = 19, col.point = c("pink", "red"), col.ocean = "light blue",
#'     main = "Canis"
#'   )
#' }

pbdb_map <- function(data, col.int='white' ,pch=19, col.ocean='black',
                     main=NULL, col.point=c('light blue','blue'), ...){
    
    if (sum((colnames(data) %in% c("lat","lng")))!=2){
        stop("Invalid data input. Use in \"pbdb_occurrences\" function the argument: show=\"coords\". e.g. pbdb_occurrences(..., show=\"coords\").
             See \"pbdb_map\" help page" )}
    
    .add.ColOcean(col.ocean, col.int,...)
    Y <- .extract.LatLong(data)
    Y1<- .add.Points(Y,col.point, pch,...)
    title(main=main,line=1,...)
    .add.Legend(Y1, col.int, pch,...)
}

#-------------------------------------------------

.add.ColOcean2 <- function(col.ocean, col.int, ...) {
  par(mar = c(0, 0, 0, 0), ...)
  maps::map(type = "n", add = TRUE, ...)
  rect(
    par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4],
    col = col.ocean
  )
  maps::map(col = col.int, fill = TRUE, add = TRUE, ...)
}

.Raster <- function(Y, res, col.int, col.ocean, ...) {
  e <- maps::map(plot = FALSE, ...)
  ext <- ext(e$range)
  r <- rast(ext)
  res(r) <- c(res, res)
  values(r) <- NA
  r <- rasterize(Y[, 1:2], r, Y[, 3], fun = sum)
  r
}

#' @importFrom grDevices adjustcolor colorRampPalette
.add.pattern <- function(r, col.eff, ...) {
  Pal <- colorRampPalette(col.eff)
  plot(r, col = adjustcolor(Pal(5), alpha.f = 0.8), add = TRUE, ...)
}

.plot.Raster.rich <- function(r, col.eff, col.ocean, col.int, res, ...) {
  par(oma = c(4, 0, 2, 2), ...)
  e <- maps::map(type = "n", ...)
  ext <- ext(e$range)
  r2 <- rast(ext)
  res(r2) <- c(res, res)
  values(r2) <- NA
  plot(r2, xaxt = "n", yaxt = "n")
  .add.ColOcean2(col.ocean, col.int, ...)
  maps::map(col = col.int, fill = TRUE, add = TRUE, ...)
  .add.pattern(r, col.eff, ...)
}

#' pbdb_map_occur
#'
#' Creates a SpatRaster object and a plot of the sampling effort (number of fossil records per cell).
#'
#' @usage pbdb_map_occur (data, res=5, col.int="white", col.ocean="black",
#' col.eff=c("light blue","blue"), do.plot=TRUE, ...)
#'
#' @param data Input dataframe. This dataframe is the output of \code{\link{pbdb_occurrences}} function using the argument: \code{show="coords"}. See too: \strong{Details} and \strong{Examples}
#' @param res the resolution of the SpatRaster object (in decimal degrees). See: \code{\link{terra}}
#' @param col.int The colour of the mainland
#' @param col.ocean The colour of the ocean
#' @param col.eff Two or more colours. To generate the colour gradient used to show the number of occurrences per cell in map
#' @param do.plot Logical; \code{TRUE} the function returns a SpatRaster and a plot.
#' @param ... Others parameters. See \code{\link{par}} and \code{\link{map}}
#' @details \strong{CAUTION!} The argument \code{show = "coords"} in \code{\link{pbdb_occurrences}} function is required. 
#' We recommend the use of a cairo device (\code{\link{X11}}) for better visualization of the graphs. See \strong{Examples}
#' @return A SpatRaster object and a plot with the sampling effort (number of fossil records per cell). This SpatRaster object have the resolution controlled by the argument \code{res}. The deflaut is \code{res=1}.
#' @seealso See \code{\link{pbdb_occurrences}}, \code{\link{map}}, \code{\link{par}} and \code{\link{colors}} help pages
#' @export
#' @examples \dontrun{
#'   data <- pbdb_occurrences(
#'     limit = "all", vocab = "pbdb", base_name = "Canis", show = "coords"
#'   )
#'   X11(width = 13, height = 7.8)
#'   pbdb_map_occur(data, res = 2)
#'   ## to obtain the raster object without plotting it
#'   pbdb_map_occur(data, res = 3, do.plot = FALSE)
#' }

pbdb_map_occur <- function(data,res=5,col.int="white", col.ocean="black",
                           col.eff=c("light blue","blue"), do.plot=TRUE, ...){
    if (sum((colnames(data) %in% c("lat","lng")))!=2){
        stop("Invalid data input. Please, add show=c(\"coords\") to your pbdb_occurrences query")
    }
    
    Y <- as.matrix(.extract.LatLong(data))
    r <- .Raster(Y, res, col.int, col.ocean, ...)
    if(do.plot==T){
        
        .plot.Raster.rich(r,col.eff,col.ocean,col.int,res,...)
        mtext("Number of records",4,line=-1,cex=2)}
    r
}

#-------------------------------------------------

.extract.rank.specie <- function(data, res = res) {
  e <- maps::map(plot = FALSE)
  ext <- ext(e$range)
  r <- rast(ext)
  res(r) <- c(res, res)
  values(r) <- 0
  if (length(data$accepted_rank) != 0) {
    identified <- data[!is.na(data$accepted_rank), ]
    species <- identified[identified$accepted_rank == "species", ]
    S <- split(species, species$accepted_no)
  }

  if (length(data$mra) != 0) {
    identified <- data[!is.na(data$mra), ]
    species <- identified[identified$mra == 3, ]
    S <- split(species, species$mid)
  }
  R <- lapply(S, function(y) {
    s <- split(y, paste(y$lng, y$lat))
    X <- as.matrix(
      do.call(rbind, lapply(s, function(x) c(x$lng[1], x$lat[1], 1)))
    )
    X <- rbind(X[1, ], X)
    rasterize(X[, 1:2], r, X[, 3])
  })
  names(R) == NULL
  all <- sum(rast(R), na.rm = TRUE)
  values(all)[values(all) == 0] <- NA
  all
}

.extract.rank.all <- function(data, res = res, rank = "genus") {
  e <- maps::map(plot = FALSE)
  ext <- ext(e$range)
  r <- rast(ext)
  res(r) <- c(res, res)
  values(r) <- 0
  ranks <- data.frame(
    rank = c("genus", "family", "order", "class", "phylum"),
    accepted_rank = c(
      "genus_no", "family_no", "order_no", "class_no", "phylum_no"
    ),
    mra = c("gnn", "fmn", "odn", "cln", "phn")
  )
  if (length(data$accepted_rank) != 0) {
    identified <- data[!is.na(data$accepted_rank), ]
    col <- paste(ranks$accepted_rank[ranks$rank == rank])
    ident <- identified[!is.na(identified[, col]), ]
    f <- paste(ident[, col])
    S <- split(ident, f)
  }

  if (length(data$mra) != 0) {
    identified <- data[!is.na(data$mra), ]
    col <- paste(ranks$mra[ranks$rank == rank])
    ident <- identified[!is.na(identified[, col]), ]
    f <- paste(ident[, col])
    S <- split(ident, f)
  }
  R <- lapply(S, function(y) {
    s <- split(y, paste(y$lng, y$lat))
    X <- as.matrix(
      do.call(rbind, lapply(s, function(x) c(x$lng[1], x$lat[1], 1)))
    )
    X <- rbind(X[1, ], X)
    rasterize(X[, 1:2], r, X[, 3])
  })
  names(R) <- NULL
  all <- sum(rast(R), na.rm = TRUE)
  values(all)[values(all) == 0] <- NA
  all
}

#' pbdb_map_richness
#'
#' Creates a SpatRaster object and a plot with richness of species, genera, families, etc. per cell.
#'
#' @usage
#' pbdb_map_richness(data, rank = "species", do.plot = TRUE, res = 5,
#'                   col.int = "white", col.ocean = "black",
#'                   col.rich = c("light blue", "blue"), ...)
#'
#' @param data Input dataframe. This dataframe is the output of \code{\link{pbdb_occurrences}} function using the argument: \code{show = c("classext", "coords", "ident")}. See too: \strong{Details} and \strong{Examples}
#' @param rank To set which taxon rank you are interested for calculate richness. The options are: "species", "genus", "family", "order", "class" or "phylum")
#' @param do.plot Logical; \code{TRUE} the function returns a SpatRaster and a plot.
#' @param res The resolution of the SpatRaster object (in decimal degrees). See: \code{\link{terra}}
#' @param col.int The colour of the mainland
#' @param col.ocean The colour of the ocean
#' @param col.rich Two or more colours. To generate the colour gradient used to show the richness per cell in map
#' @param ... Others parameters. See \code{\link{par}} and \code{\link{map}}
#' @details \strong{CAUTION!} The argument \code{show = "coords"} in \code{\link{pbdb_occurrences}} function is required. 
#' We recommend the use of a cairo device (\code{\link{X11}}) for better visualization of the graphs. See \strong{Examples}
#' @return A SpatRaster object and a plot with richness of species, genera, families, etc. per cell. This SpatRaster object have the resolution controlled by
#' the argument \code{res}. The default is \code{res=1}.
#' @seealso See \code{\link{pbdb_occurrences}}, \code{\link{map}}, \code{\link{par}} and \code{\link{colors}} help pages
#' @export
#' @examples \dontrun{
#'   data <- pbdb_occurrences(
#'     limit = 1000, vocab = "pbdb", base_name = "mammalia",
#'     show = c("classext", "coords", "ident")
#'   )
#'   X11(width = 13, height = 7.8)
#'   pbdb_map_richness(data, res = 8, rank = "genus")
#'   pbdb_map_richness(data, res = 8, rank = "family")
#'   ## to obtain the raster object without plotting the map
#'   pbdb_map_richness(data, res = 8, rank = "family", do.plot = FALSE)
#' }

pbdb_map_richness <- function(data, rank="species", do.plot=TRUE, res=5,col.int="white", col.ocean="black",
                              col.rich=c("light blue","blue"),...){
    if(!any(rank==c("species", "genus","family","order","class","phylum"))){
        stop("Invalid rank name. Use: \"species\", \"genus\", \"family\", \"order\", \"class\" or \"phylum\".
             See \"pbdb_map_richness\" help page" )}
    if (sum(colnames(data) %in% c("lat","lng","genus","family","order","class","phylum","mid","fml","odl","cll","phl"))!=7){
        stop("Invalid data input. In the \"pbdb_occurrences\" function, use the following argument: show=c(\"classext\",\"coords\",\"ident\"). e.g. pbdb_occurrences(..., show=c(\"classext\",\"coords\",\"ident\")).
             See \"pbdb_map_richness\" help page" )}
    
    if(rank=="species"){
        r<-.extract.rank.specie(data,res)
    }
    else
    {
        r<-.extract.rank.all(data,res,rank)
    }
    
    if(do.plot==TRUE){
        .plot.Raster.rich(r,col.rich,col.ocean,col.int,res,...)
        mtext(paste("Richness of", rank),4,line=-1,cex=2)
        
    }
    r
}
