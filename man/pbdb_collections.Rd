\name{pbdb_collections}
\alias{pbdb_collections}
\title{pbdb_collections}
\usage{
pbdb_collections(...)
}
\arguments{
  \item{...}{documentation for all the parameters is
  available in http://paleobiodb.org/data1.1/colls/list go
  to ?pbdb_occurrences to see an explanation about the main
  filtering parameters}
}
\value{
a dataframe with the collections that match the query
}
\description{
Returns information about multiple collections, selected
according to the parameters you provide.
}
\examples{
\dontrun{
pbdb_collections (base_name="Cetacea", interval="Miocene")
}
}

