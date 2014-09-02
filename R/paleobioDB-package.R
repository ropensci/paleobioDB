#'We have developed paleobioDB, an R-package designed to make easy and flexible queries 
#'of the Paleobiology Database, as well as to visualize and download selected data. 
#'This package will make it easy to access paleontological data in a way that should 
#'allow those data to be further analyzed, 
#'including via packages and libraries available in R.
#'
#'We programmed two different groups of functions. 
#'First, we developed a set of general and flexible functions to wrap the 
#'\href{http://paleobiodb.org/data1.1/}{PaleobioDB API}.
#'These functions connect R with each of the endpoints of the PaleobioDB API. 
#'Second, based on these base functions, we programmed a second set of functions 
#'intended to explore and visualize the fossil 
#'occurrences in their geographic, temporal and taxonomic dimensions.
#'
#' @name paleobioDB
#' @aliases paleobioDB-package
#' @docType package
#' @title paleobioDB: An R-package for downloading, visualizing and processing data from the Paleobiology Database
#' @author Sara Varela \email{svarela@@paleobiogeography.org}
#' @author Javier Gonzalez \email{javigzz@@yahoo.es}
#' @author Luciano Fabris Sgarbi \email{luciano.f.sgarbi@@gmail.com}
#' @references Sara Varela, Javier Gonzalez-Hernandez,
#' Luciano Fabris Sgarbi, Charles Marshall, Mark D. Uhen, in prep. paleobioDB: 
#' an R-package for downloading, visualizing and processing data from the Paleobiology Database
#' 
#' @keywords package
#'
#' @details \tabular{ll}{
#' Package: \tab paleobioDB\cr
#' Type: \tab Package\cr
#' Version: \tab 0.1\cr
#' Date: \tab 2014-04-04\cr
#' License: \tab GPL-2\cr
#' }
#' 
#'@seealso {
#'\url{http://paleobiodb.org}
#' }
#' 
#' @import rjson gtools RCurl plyr raster maps scales 
#' 
#'@examples \dontrun{
#'
#'canidae<-  pbdb_occurrences (limit="all", base_name="canidae", 
#'interval="Quaternary", show=c("coords", "phylo", "ident"))
#'
#'## to explore the number of subtaxa
#'pbdb_subtaxa (canidae)
#'
#'## to explore the temporal resolution of the fossil records
#'pbdb_temporal_resolution (canidae)
#'
#'## returns a dataframe and a plot with the temporal span 
#'##  of the species, genera, etc.
#'pbdb_temp_range (canidae, rank= "genus", names=FALSE)
#'
#'## returns a dataframe and a plot showing the species, genera, etc. 
#'richness across time
#'pbdb_richness (canidae, rank= "species", 
#'temporal_extent= c (0,10), res= 1)
#'
#'## returns a dataframe and a plot showing the evolutionary 
#'and extinction rates across time
#'
#'## evolutionary rates= evo_ext=1
#'pbdb_orig_ext (canidae, rank="species", temporal_extent=c(0, 10),
#'res=1, orig_ext=1)
#'
#'## extinction rates= evo_ext=2
#'pbdb_orig_ext (canidae, rank="species", temporal_extent=c(0, 10),
#'               res=1, orig_ext=2)
#'
#'## maps the fossil occurrences
#'pbdb_map (canidae, main = "Canidae", pch= 19, cex=0.7)
#'
#'## maps the sampling effort
#'pbdb_map_occur (canidae, res= 5)
#'
#'## maps the species, genera, etc. richness
#'pbdb_map_richness (canidae, rank="species", res= 5)
#'
#' }
#'
NULL