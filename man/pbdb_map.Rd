\name{pbdb_map}
\alias{pbdb_map}
\title{pbdb_map}
\usage{
pbdb_map (data, col.int="white" ,pch=19, col.ocean="black", main=NULL,
col.point=c("light blue","blue"), ...)
}
\arguments{
  \item{data}{Input dataframe. This dataframe is the output
  of \code{\link{pbdb_occurrences}} function using the
  argument: \code{show = "coords"}. See too:
  \strong{Details} and \strong{Examples}}

  \item{col.int}{The colour of the mainland.}

  \item{pch}{See: \code{\link{par}}}

  \item{col.ocean}{The colour of the ocean.}

  \item{main}{To set the title of the map. See:
  \code{\link{par}}}

  \item{col.point}{Two or more colours. To generate the
  colour gradient used to show the number of occurrences
  per cell in map}

  \item{...}{Others parameters. See \code{\link{par}} and
  \code{\link{map}}}
}
\value{
A map showing the distribution of the fossil records, with
the points with a color gradient, according to the number
of occurrences per cell.
}
\description{
Maps the fossil records
}
\details{
The function opens a new window for the map

\strong{CAUTION!} The argument \code{show = "coords"} in
\code{\link{pbdb_occurrences}} function is required. We
recommend the use of a cairo device (\code{\link{X11}}) for
better visualization of the graphs. See \strong{Examples}
}
\examples{
\dontrun{
library(maps)
X11(width=12, height=8)
data<- pbdb_occurrences (limit="all", vocab= "pbdb",
base_name="Canis", show="coords")
pbdb_map(data)
pbdb_map(data,pch=1)
pbdb_map(data,pch=19,col.point=c("pink","red"), col.ocean="light blue",
main="canis")
}
}
\seealso{
See \code{\link{pbdb_occurrences}}, \code{\link{map}},
\code{\link{par}} and \code{\link{colors}} help pages
}

