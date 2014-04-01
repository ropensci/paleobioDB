\name{pbdb_map}
\alias{pbdb_map}
\title{pbdb_map}
\usage{
pbdb_map (data, col.int="white" ,pch=19, col.ocean="black", main=NULL, col.point=c("light blue","blue"), ...)
}
\arguments{
  \item{data}{Input dataframe. This dataframe is the output
  of \code{\link{pbdb_occurrences}} function using the
  argument: \code{\link{show = "coords"}}. See too:
  \strong{Details} and \strong{Examples}}

  \item{col.int}{The color of mainland.}

  \item{pch}{See: \code{\link{par}}}

  \item{col.ocean}{The color of ocean}

  \item{main}{The title of map}

  \item{col.point}{Two or more colors. This generates a
  color gradient and, is used to show the number of samples
  at the same point in map}

  \item{...}{Others parameters. See \code{\link{par}} and
  \code{\link{map()}}}
}
\value{
The map of occurrences of fossil records, with the points
with a color gradient, according to the number of samples
in each site
}
\description{
Shows the map of occurrences of fossil records in a cairo
\code{\link{x11}}( ) device
}
\details{
\strong{CAUTION!} Is required use the argument
\code{\link{show = "coords"}} in
\code{\link{pbdb_occurrences}} function. See
\strong{Examples}
}
\examples{
\dontrun{
data<- pbdb_occurrences (limit="all", vocab= "pbdb", base_name="Canis")
pbdb_map(data)
###
##Error in pbdb_map(data) :
##Invalid data input. Use in "pbdb_occurrences" function the argument: show="coords". e.g. pbdb_occurrences(..., show="coords").
##See "pbdb_map" help page
###
data<- pbdb_occurrences (limit="all", vocab= "pbdb", base_name="Canis", show="coords")
pbdb_map(data)
pbdb_map(data,pch=1)
pbdb_map(data,pch=19,col.point=c("pink","red"), col.ocean="light blue",main="canis")
l_ply(dev.list(),dev.off)## to close all windows graphics
}
}
\seealso{
See \code{\link{pbdb_occurrences}}, \code{\link{map}},
\code{\link{par}} and \code{\link{colors}} help pages
}

