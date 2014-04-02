\name{pbdb_occurrence}
\alias{pbdb_occurrence}
\title{pbdb_occurrence}
\usage{
pbdb_occurrence (id, ...)

Returns information about a single occurrence record from the Paleobiology Database.
}
\arguments{
  \item{id}{identifier of the occurrence. This parameter is
  required.}

  \item{...}{documentation for all the parameters is
  available in http://paleobiodb.org/data1.1/occs/single
  Below, we describe the most common filters that
  paleontologists and ecologists might use. \code{vocab}
  set vocab="pbdb" to show the complete name of the
  variables (by default variables have short 3-letter
  names) \code{show} to show extra variables (e.g. coords)}
}
\value{
a dataframe with a single occurrence
}
\description{
pbdb_occurrence
}
\examples{
\dontrun {
pbdb_occurrence (id=1001)
pbdb_occurrence (id=1001, vocab="pbdb", show="coords")
}
}

