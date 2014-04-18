\name{pbdb_collections_geo}
\alias{pbdb_collections_geo}
\title{pbdb_collections_geo}
\usage{
pbdb_collections_geo (...)
}
\arguments{
  \item{...}{documentation for all the parameters is
  available in http://paleobiodb.org/data1.1/colls/summary
  go to ?pbdb_occurrences to see an explanation about the
  main filtering parameters}
}
\value{
a dataframe with the collections that match the query
}
\description{
This path returns information about geographic clusters of
collections from the Paleobiology Database. These clusters
are defined in order to facilitate the generation of maps
at low resolutions. You can make a config request via
http://paleobiodb.org/data1.1/config in order to get a list
of the available summary levels.
}
\examples{
\dontrun{
pbdb_collections_geo (vocab="pbdb", lngmin=0.0,
lngmax=15.0, latmin=0.0, latmax=15.0, level=2)
}
}

