\name{pbdb_subtaxa}
\alias{pbdb_subtaxa}
\title{pbdb_subtaxa}
\usage{
number_of_subtaxa (data, show=c("species", "genera", "tribes", "subfamilies", "families","superfamilies",
"orders", "classes", "subclasses", "subphyla", "phyla"))
}
\arguments{
  \item{data}{input dataframe with our query}

  \item{do.plot}{by default this function make a plot to
  visualize the distribution of taxa. Set to FALSE to skip
  the plot.}

  \item{show}{use show to choose which subtaxa should be
  shown: "species", "genera", "tribes", "subfamilies",
  "families","superfamilies", "orders", "classes",
  "subclasses", "subphyla", "phyla". By default the
  function shows all of them.}
}
\value{
a dataframe with the number of subtaxa of the chosen
cathegories
}
\description{
count the number of subtaxa within a given taxa. e.g.
number of species within a genus.
}
\examples{
\dontrun{
canidae_quat<-  pbdb_occurrences (limit="all", base_name="Canidae",  interval="Quaternary", show="coords")
pbdb_subtaxa (canidae_quat)
}
}

