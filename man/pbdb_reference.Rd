\name{pbdb_reference}
\alias{pbdb_reference}
\title{pbdb_reference}
\usage{
pbdb_reference(id, ...)
}
\arguments{
  \item{id}{identifier of the reference. This parameter is
  required.}

  \item{vocab}{set vocab="pbdb" to show the complete name
  of the variables (by default variables have short
  3-letter names)}

  \item{...}{documentation for all the parameters is
  available in http://paleobiodb.org/data1.1/refs/single go
  to ?pbdb_occurrences to see an explanation about the main
  filtering parameters}
}
\value{
a dataframe with a single reference
}
\description{
Returns information about a single reference, selected by
identifier
}
\examples{
\dontrun{
pbdb_collection (id=1003, vocab="pbdb", show="loc")
}
}
