\name{pbdb_intervals}
\alias{pbdb_intervals}
\title{pbdb_intervals}
\usage{
pbdb_intervals(id, ...)
}
\arguments{
  \item{id}{identifiers of the temporal intervals.}

  \item{min_ma}{return only intervals that are at least
  this old}

  \item{max_ma}{return only intervals that are at most this
  old}

  \item{order}{return the intervals in order starting as
  specified. Possible values include older, younger.
  Defaults to younger.}

  \item{vocab}{set vocab="pbdb" to show the complete name
  of the variables (by default variables have short
  3-letter names)}

  \item{...}{documentation for all the parameters is
  available in
  http://paleobiodb.org/data1.1/intervals/lists}
}
\value{
a dataframe with information from several temporal
intervals
}
\description{
Returns information about multiple intervals, selected
according to the parameters you provide.
}
\examples{
\dontrun{
pbdb_intervals (min_ma= 0, max_ma=2, vocab="pbdb")

}
}

