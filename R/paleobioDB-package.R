#'We have developed paleobioDB, an R-package designed to make easy and flexible queries 
#'of the Paleobiology Database, as well as to visualize and download selected data. 
#'This package will make it easy to access paleontological data in a way that should 
#'allow those data to be further analyzed, 
#'including via packages and libraries available in R.
#'
#'We programmed two different groups of functions. 
#'First, we developed a set of general and flexible functions to wrap the 
#'\href{https://paleobiodb.org/data1.2/}{PaleobioDB API}.
#'These functions connect R with each of the endpoints of the PaleobioDB API. 
#'Second, based on these base functions, we programmed a second set of functions 
#'intended to explore and visualize the fossil 
#'occurrences in their geographic, temporal and taxonomic dimensions.
#'
#' @name paleobioDB
#' @aliases paleobioDB-package
#' @docType package
#' @title paleobioDB: An R-package for downloading, visualizing and
#'   processing data from the Paleobiology Database
#' @author Sara Varela \email{sara.varela@@uvigo.gal}
#' @author Javier Gonzalez \email{javigzz@@yahoo.es}
#' @author Luciano Fabris Sgarbi \email{luciano.f.sgarbi@@gmail.com}
#' @references Sara Varela, Javier González-Hernández, Luciano Fabris
#'   Sgarbi, Charles Marshall, Mark D. Uhen, Shanan Peters, Michael
#'   McClennen, 2015. paleobioDB: an R package for downloading,
#'   visualizing and processing data from the Paleobiology
#'   Database. Ecography, 38:
#'   419-425. \url{https://doi.org/10.1111/ecog.01154}
#' 
#' @keywords internal
#'
#' @seealso {
#' \url{https://paleobiodb.org}
#' }
#'
#' @import rjson gtools RCurl plyr scales
#' @importFrom grDevices colorRampPalette
#' @importFrom graphics abline axis barplot hist legend mtext par
#'   plot.new plot.window points polygon rect segments text title
#' @importFrom maps map
#' @importFrom utils URLencode
#' @importFrom terra ext rast rasterize res res<- values values<-
#' @importMethodsFrom terra plot
#'
#' @examples \dontrun{
#' canidae <- pbdb_occurrences(
#'   vocab = "pbdb",
#'   limit = "all",
#'   base_name = "canidae",
#'   interval = "Quaternary",
#'   show = c("coords", "classext", "ident")
#' )
#'
#' ## to explore the number of subtaxa
#' pbdb_subtaxa(canidae)
#'
#' ## to explore the temporal resolution of the fossil records
#' pbdb_temporal_resolution(canidae)
#'
#' ## returns a dataframe and a plot with the temporal span
#' ## of the species, genera, etc.
#' pbdb_temp_range(canidae, rank = "genus", names = FALSE)
#'
#' ## returns a dataframe and a plot showing the species, genera, etc.
#' ## richness across time
#' pbdb_richness(canidae, rank = "species", temporal_extent = c(0, 10), res = 1)
#'
#' ## returns a dataframe and a plot showing the evolutionary
#' ## and extinction rates across time
#'
#' ## evolutionary rates: orig_ext = 1
#' pbdb_orig_ext(
#'   canidae,
#'   rank = "species", temporal_extent = c(0, 10), res = 1,
#'   orig_ext = 1
#' )
#'
#' ## extinction rates: orig_ext = 2
#' pbdb_orig_ext(
#'   canidae,
#'   rank = "species", temporal_extent = c(0, 10), res = 1,
#'   orig_ext = 2
#' )
#'
#' ## maps the fossil occurrences
#' pbdb_map(canidae, main = "Canidae", pch = 19, cex = 0.7)
#'
#' ## maps the sampling effort
#' pbdb_map_occur(canidae, res = 5)
#'
#' ## maps the species, genera, etc. richness
#' pbdb_map_richness(canidae, rank = "species", res = 5)
#'
#' }
#'
"_PACKAGE"
