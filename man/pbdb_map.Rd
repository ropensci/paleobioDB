\name{pbdb_map}
\alias{pbdb_map}
\title{pbdb_map}
\usage{
pbdb_map (query, col.int='white' ,pch=19, col.ocean='black', main=NULL, col.point=c('light blue','blue'),...)
}
\arguments{
  \item{query}{Input dataframe. This dataframe is the
  output of \code{\link{pbdb_occurrences}} function}

  \item{col.int}{This will be the color of mainland}

  \item{pch}{See: \code{\link{par}}}

  \item{col.ocean}{This will be the color of ocean}

  \item{main}{The title of map}

  \item{col.point}{Two or more colors. This generates a
  color gradient and, is used to show the number of samples
  at the same point}

  \item{...}{Others parameters. See \code{\link{par}}}
}
\value{
A map of occurences
}
\description{
shows the map of occurrences of fossil records
}
\examples{
\dontrun{
data<- pbdb_occurrences (limit="all", vocab= "pbdb", base_name="Canis", show="coords")
X11( width=20, height=10) ## or x11()
pbdb_map(data)
pbdb_map(data,pch=1)
pbdb_map(data,pch=19,col.point=c('pink','red'), col.ocean='light blue',main='canis')
}
}

