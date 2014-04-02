\name{pbdb_scales}
\alias{pbdb_scales}
\title{pbdb_scales}
\usage{
pbdb_scales(...)
}
\arguments{
  \item{...}{documentation for all the parameters is
  available in http://paleobiodb.org/data1.1/scales/list.
  For instance: \code{vocab} set vocab="pbdb" to show the
  complete name of the variables (by default variables have
  short 3-letter names)}
}
\value{
a dataframe with information from the selected scales
}
\description{
Returns information about multiple time scales.
}
\examples{
\dontrun{
Get a dataframe with all the scales available in PBDB setting no ids
pbdb_scale ()
}
}

