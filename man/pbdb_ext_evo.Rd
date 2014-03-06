\name{pbdb_ext_evo}
\alias{pbdb_ext_evo}
\title{pbdb_ext_evo}
\usage{
pbdb_ext_evo (data)
}
\arguments{
  \item{data}{our query to the PBDB database}
}
\value{
a plot
}
\description{
To plot the extinct and new taxa across time.
}
\examples{
\dontrun{
canidae<-  pbdb_query_occurrences (limit="all", vocab="pbdb",
base_name="Canidae")
pbdb_ext_evo (canidae, rank="genus")
pbdb_ext_evo (canidae, rank="species")
}
}

