\name{pbdb_map}
\alias{pbdb_map}
\title{pbdb_map}
\usage{
pbdb_map (query, col.int='white',  col.ocean='black', main=NULL, col.point=c('light blue','blue'),...)
}
\arguments{
  \item{query}{Input dataframe with our query}

  \item{col.int}{This will be the color of mainland}

  \item{col.ocean}{This will be the color of ocean}

  \item{main}{The title of map}

  \item{col.point}{Two or more colors. This generates a
  color gradient and, is used to show the number of samples
  at the same point}

  \item{...}{Other parameters. See \code{\link{par}}}
}
\value{
a list with a summary of the temporal resolution of the
data (min, max, 1st and 3rd quartils, median and mean), and
the temporal resolution of each fossil record (Ma).
}
\description{
to show of the map of occurences of the fossil data
}
\examples{
\dontrun{
data<- pbdb_occurrences (taxa= "Canidae", time= "Quaternary")
pbdb_temporal_resolution (data)
}
}

