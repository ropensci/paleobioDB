\name{pbdb_temporal_resolution}
\alias{pbdb_temporal_resolution}
\title{pbdb_temporal_resolution}
\usage{
pbdb_temporal_resolution (data)
}
\arguments{
  \item{data}{input dataframe with our query}

  \item{do.plot}{TRUE/FALSE. To show a frequency plot of
  the data (TRUE by default).}
}
\value{
a list with a summary of the temporal resolution of the
data (min, max, 1st and 3rd quartils, median and mean), and
the temporal resolution of each fossil record (Ma).
}
\description{
to show the temporal resolution of the fossil data
}
\examples{
\dontrun{
data<- pbdb_occurrences (taxa= "Canidae", time= "Quaternary")
pbdb_temporal_resolution (data)
}
}

