\name{pbdb_temp_range}
\alias{pbdb_temp_range}
\title{pbdb_temp_range}
\usage{
pbdb_temp_range (data, rank, col = "skyblue2", names = TRUE, do.plot =TRUE)
}
\arguments{
  \item{data}{dataframe with our query to the paleoBD
  \code{\link{pbdb_occurrences}}. Important, it is required
  to show the name of the families, orders, etc. in the
  dataframe, to do that set: show=c("phylo", "ident") (see
  example).}

  \item{rank}{to set which taxon rank you are interested.}

  \item{col}{to change the colour of the bars in the plot,
  skyblue2 by default.}

  \item{names}{TRUE/FALSE (TRUE by default). To include or
  not the name of the taxa in the plot}

  \item{do.plot}{TRUE/FALSE (TRUE by default).}
}
\value{
a plot and a dataframe with the time span of the taxa
selected (species, genus, etc.)
}
\description{
to show the time span of a selected taxon rank included in
the query
}
\examples{
\dontrun{
canis_quaternary<- pbdb_occurrences (limit="all", base_name="Canis",
                 interval="Quaternary", show=c("coords", "phylo", "ident"))
pbdb_temp_range (canis_quaternary, rank="species", names=TRUE)
}
}

