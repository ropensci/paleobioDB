\docType{package}
\name{paleobioDB}
\alias{paleobioDB}
\alias{paleobioDB-package}
\title{paleobioDB: An R-package for downloading, visualizing and processing data from the Paleobiology Database}
\description{
We have developed paleobioDB, an R-package designed to make
easy and flexible queries of the Paleobiology Database, as
well as to visualize and download selected data. This
package will make it easy to access paleontological data in
a way that should allow those data to be further analyzed,
including via packages and libraries available in R.
}
\details{
We programmed two different groups of functions. First, we
developed a set of general and flexible functions to wrap
the \href{http://paleobiodb.org/data1.1/}{PaleobioDB API}.
These functions connect R with each of the endpoints of the
PaleobioDB API. Second, based on these base functions, we
programmed a second set of functions intended to explore
and visualize the fossil occurrences in their geographic,
temporal and taxonomic dimensions.

\tabular{ll}{ Package: \tab paleobioDB\cr Type: \tab
Package\cr Version: \tab 0.1\cr Date: \tab 2014-04-04\cr
License: \tab GPL-2\cr }
}
\examples{
\dontrun{

canidae<-  pbdb_occurrences (limit="all", base_name="canidae",
interval="Quaternary", show=c("coords", "phylo", "ident"))

## to explore the number of subtaxa
pbdb_subtaxa (canidae, do.plot=TRUE)

## to explore the temporal resolution of the fossil records
pbdb_temporal_resolution (canidae)

## returns a dataframe and a plot with the temporal span
##  of the species, genera, etc.
pbdb_time_span (canidae, rank="species")

## returns a dataframe and a plot showing the species, genera, etc.
richness across time
pbdb_richness (canidae, rank="species",
temporal_extent=c(0,10), resolution=1)

## returns a dataframe and a plot showing the new appearances
and extinction rates across time
## to show the number of new taxa set orig_ext=1
pbdb_orig_ext (canidae, rank="species", orig_ext=1,
temporal_extent=c(0,10), resolution=1)

## extinction rates: orig_ext=2
pbdb_orig_ext (canidae, rank="species", orig_ext=2,
temporal_extent=c(0,10), resolution=1)

## maps the fossil occurrences
pbdb_map(canidae)

## maps the number of occurrences per cell
pbdb_map_occur (canidae, res= 2)

## maps the number of different species, genera, etc. per cell
pbdb_map_richness (data, res= 3, rank="species")

}
}
\author{
Sara Varela \email{svarela@paleobiogeography.org}

Javier Gonzalez \email{javigzz@yahoo.es}

Luciano Fabris Sgarbi \email{luciano.f.sgarbi@gmail.com}
}
\references{
Sara Varela et. al, in prep.
paleobioDB: an R-package for downloading, visualizing and
processing data from the Paleobiology Database
}
\seealso{
{ \url{http://paleobiodb.org} }
}
\keyword{package}

