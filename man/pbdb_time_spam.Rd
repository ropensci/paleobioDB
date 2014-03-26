\name{pbdb_time_spam}
\alias{pbdb_time_spam}
\title{pbdb_time_spam}
\usage{
pbdb_time_spam (data)
}
\arguments{
  \item{data}{input dataframe with our query (set vocab=
  "pbdb" in the query)}

  \item{rank}{to set which taxon rank you are interested.
  By default rank= "species"}

  \item{col}{to change the colour of the bars in the plot,
  skyblue2 by default.}

  \item{names}{TRUE/FALSE (TRUE by default). To include or
  not the name of the taxa in the plot}
}
\value{
a plot with the time spam of the taxa selected (species,
genus, etc.)
}
\description{
to show the time spam of a selected taxon rank included in
the query
}
\examples{
\dontrun{
canis_quaternary<- pbdb_occurrences (limit="all", vocab= "pbdb", base_name="Canis", interval="Quaternary", show="coords")
pbdb_time_spam (canis_quaternary, rank="species", names=TRUE)
}
}

