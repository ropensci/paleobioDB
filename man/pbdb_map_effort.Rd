\name{pbdb_map_effort}
\alias{pbdb_map_effort}
\title{pbdb_map_effort}
\usage{
pbdb_map_effort (data, res=1, col.int='white', col.ocean='black', col.eff=c('light blue','blue'),...)
}
\arguments{
  \item{data}{Input dataframe. This dataframe is the output
  of \code{\link{pbdb_occurrences}} () function}

  \item{res}{the resolution of the RasterLayer object in
  decimal degrees. See: \code{\link{raster}} ()}

  \item{col.int}{The color of mainland}

  \item{col.ocean}{The color of ocean}

  \item{col.eff}{Two or more colors. This generates a color
  gradient and, is used to show the sampling effort in each
  cell}

  \item{...}{Others parameters. See \code{\link{par}} and
  \code{\link{map()}}}
}
\value{
A RasterLayer object with sampling effort of fossil. This
RasterLayer object have the resolution controlled by the
argument \code{\link{res}}. The deflaut is
\code{\link{res=1}} Additionally the
\code{\link{pbdb_map_effort}} function returns a map with
the sampling effort by cells
}
\description{
Create a RasterLayer object with sampling effort of fossil
records. Furthermore this function maps the sampling effort
of the fossil records
}
\details{
Only cairo \code{\link{x11}}( ) devices are supported. See
\strong{Examples}

The simplest use of this function is:
\code{\link{pbdb_map_effort(data)}}.

Before running the function
\code{\link{pbdb_map_effort(...)}}, we recommended to open
the \code{\link{x11()}} device, maximize and minimize.
}
\examples{
\dontrun{
data<- pbdb_occurrences (limit="all", vocab= "pbdb", base_name="Canis", show="coords")
x11() ## maximize and minimize
r<-pbdb_map_effort (data,res=2)
}
}
\seealso{
See \code{\link{pbdb_map}}, \code{\link{par}} and
\code{\link{colors}} help pages
}

