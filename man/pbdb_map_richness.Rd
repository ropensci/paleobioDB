\name{pbdb_map_richness}
\alias{pbdb_map_richness}
\title{pbdb_map_richness}
\usage{
pbdb_map_effort (data, rank="species", do.plot=T, res=1, col.int="white", col.ocean="black", col.rich=c("light blue","blue"),...)
}
\arguments{
  \item{data}{Input dataframe. This dataframe is the output
  of \code{\link{pbdb_occurrences}} function using the
  argument: \code{\link{show = c("phylo", "coords",
  "ident")}}. See too: \strong{Details} and
  \strong{Examples}}

  \item{rank}{To set which taxon rank you are interested
  for calculate richness. The options are: "species",
  "genus", "family", "order", "class" or "phylum")}

  \item{do.plot}{Logical; \code{\link{TRUE}} the function
  returns a RasterLayer and a plot.}

  \item{res}{The resolution of the RasterLayer object (in
  decimal degrees). See: \code{\link{raster}} ()}

  \item{col.int}{The colour of the mainland}

  \item{col.ocean}{The colour of the ocean}

  \item{col.point}{Two or more colours. To generate the
  colour gradient used to show the number of occurrences
  per cell in map}

  \item{...}{Others parameters. See \code{\link{par}} and
  \code{\link{map()}}}
}
\value{
A RasterLayer object and a plot with richness of species,
genera, families, etc. per cell. This RasterLayer object
have the resolution controlled by the argument
\code{\link{res}}. The default is \code{\link{res=1}}.
}
\description{
Creates a RasterLayer object and a plot with richness of
species, genera, families, etc. per cell.
}
\details{
\strong{CAUTION!} The argument \code{\link{show =
"coords"}} in \code{\link{pbdb_occurrences}} function is
required. See \strong{Examples}
}
\examples{
\dontrun{
data<- pbdb_occurrences (limit=1000, vocab= "pbdb", base_name="mammalia", show=c("phylo","coords","ident"))
pbdb_map_richness (data,res=3,rank="genus")
pbdb_map_richness (data,res=8,rank="family")
pbdb_map_richness (data,res=3,rank="family",do.plot=F)
l_ply(dev.list(),dev.off)## to close all windows graphics
}
}
\seealso{
See \code{\link{pbdb_occurrences}}, \code{\link{map}},
\code{\link{par}} and \code{\link{colors}} help pages
}

