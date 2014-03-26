\name{pbdb_scale}
\alias{pbdb_scale}
\title{pbdb_scale}
\usage{
pbdb_scale(id, ...)
}
\arguments{
  \item{id}{identifier of the temporal interval. This
  parameter is required.}

  \item{vocab}{set vocab="pbdb" to show the complete name
  of the variables (by default variables have short
  3-letter names)}

  \item{...}{documentation for all the parameters is
  available in http://paleobiodb.org/data1.1/scales/single}
}
\value{
a dataframe with information from a single scale
}
\description{
Returns information about a single time scale, selected by
identifier.
}
\examples{
\dontrun{
pbdb_scale (id=1, vocab="pbdb")
}
}

