\name{pbdb_map_effort}
\alias{pbdb_map_effort}
\title{pbdb_map_effort}
\usage{
pbdb_map_effort (data, res=1, col.int="white", col.ocean="black", col.eff=c("light blue","blue"), do.plot=TRUE, ...)
}
\arguments{
  \item{data}{Input dataframe. This dataframe is the output
  of \code{\link{pbdb_occurrences}} function using the
  argument: \code{\link{show="coords"}}. See too:
  \strong{Details} and \strong{Examples}}

  \item{res}{the resolution of the RasterLayer object (in
  decimal degrees). See: \code{\link{raster}} ()}

  \item{col.int}{The color of mainland}

  \item{col.ocean}{The color of ocean}

  \item{col.eff}{Two or more colors. This generates a color
  gradient and, is used to show the sampling effort in each
  cell}

  \item{do.plot}{Logical; If \code{\link{TRUE}} specifies
  that plotting should be done}

  \item{...}{Others parameters. See \code{\link{par}} and
  \code{\link{map()}}}
}
\value{
A RasterLayer object with sampling effort of fossil
records. This RasterLayer object have the resolution
controlled by the argument \code{\link{res}}. The deflaut
is \code{\link{res=1}}. Additionally the
\code{\link{pbdb_map_effort}} function returns a map with
the sampling effort by cells.
}
\description{
Create a RasterLayer object with sampling effort of fossil
records. Furthermore this function maps the sampling effort
of the fossil records using a cairo \code{\link{x11}}( )
device
}
\details{
\strong{CAUTION!} Is required use the argument
\code{\link{show="coords"}} in
\code{\link{pbdb_occurrences}} function. See
\strong{Examples}
}
\examples{
\dontrun{
data<- pbdb_occurrences (limit="all", vocab= "pbdb", base_name="Canis")
r<-pbdb_map_effort (data,res=2)
###
##Error in pbdb_map_effort(data,res=2) :
##Invalid data input. Use in "pbdb_occurrences" function the argument: show="coords". e.g. pbdb_occurrences(..., show="coords").
##See "pbdb_map_effort" help page
###
data<- pbdb_occurrences (limit="all", vocab= "pbdb", base_name="Canis", show="coords")
pbdb_map_effort (data,res=2)
pbdb_map_effort (data,res=2,do.plot=F)

l_ply(dev.list(),dev.off)## to close all windows graphics
}
}
\seealso{
See \code{\link{pbdb_occurrences}}, \code{\link{map}},
\code{\link{par}} and \code{\link{colors}} help pages
}

