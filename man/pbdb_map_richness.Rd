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

  \item{do.plot}{Logical; If \code{\link{TRUE}} specifies
  that plotting should be done}

  \item{res}{The resolution of the RasterLayer object (in
  decimal degrees). See: \code{\link{raster}} ()}

  \item{col.int}{The color of mainland}

  \item{col.ocean}{The color of ocean}

  \item{col.rich}{Two or more colors. This generates a
  color gradient and, is used to show the richness in each
  cell}

  \item{...}{Others parameters. See \code{\link{par}} and
  \code{\link{map()}}}
}
\value{
A RasterLayer object with richness of fossil records by
cell. This RasterLayer object have the resolution
controlled by the argument \code{\link{res}}. The deflaut
is \code{\link{res=1}}. Additionally the
\code{\link{pbdb_map_effort}} function returns a richness
of the fossil records by cells.
}
\description{
Create a RasterLayer object with richness of fossil records
by cell. Furthermore this function maps the richness of the
fossil records using a cairo \code{\link{x11}}( ) device.
}
\details{
\strong{CAUTION!} Is required use the argument
\code{\link{show = c("phylo", "coords", "ident")}} in
\code{\link{pbdb_occurrences}} function. See
\strong{Examples}
}
\examples{
\dontrun{
data<- pbdb_occurrences (limit=100, vocab= "pbdb", base_name="mammalia", show="coords")
pbdb_map_richness (data,res=3,rank="genus")
## Error in pbdb_map_richness(data, res = 3, rank = "genus") :
## Invalid data input. Use in "pbdb_occurrences" function the argument: show=c("phylo","coords","ident").
## e.g. pbdb_occurrences(..., show=c("phylo","coords","ident")).
## See "pbdb_map_richness" help page
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

