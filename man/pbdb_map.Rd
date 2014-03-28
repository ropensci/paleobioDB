\name{pbdb_map}
\alias{pbdb_map}
\title{pbdb_map}
\usage{
pbdb_map (data, col.int='white' ,pch=19, col.ocean='black', main=NULL, col.point=c('light blue','blue'),...)
}
\arguments{
  \item{data}{Input dataframe. This dataframe is the output
  of the \code{\link{pbdb_occurrences}} function}

  \item{col.int}{The color of mainland.}

  \item{pch}{See: \code{\link{par}}}

  \item{col.ocean}{The color of ocean}

  \item{main}{The title of map}

  \item{col.point}{Two or more colors. This generates a
  color gradient and, is used to show the number of samples
  at the same point}

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
Only cairo \code{\link{x11}}( ) devices are supported.

The simplest use of this function is:
\code{\link{pbdb_map(data)}}.

Before running the function \code{\link{pbdb_map(...)}}, we
recommended to open the \code{\link{x11()}} device,
maximize and minimize.
}
\examples{
\dontrun{
data<- pbdb_occurrences (limit="all", vocab= "pbdb", base_name="Canis", show="coords")
x11( ) ## maximize and minimize
pbdb_map(data)
pbdb_map(data,pch=1)
pbdb_map(data,pch=19,col.point=c('pink','red'), col.ocean='light blue',main='canis')
}
}
\seealso{
See \code{\link{pbdb_map}}, \code{\link{par}} and
\code{\link{colors}} help pages
}

