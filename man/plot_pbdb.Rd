\name{plot_pbdb}
\alias{plot_pbdb}
\title{plot_pbdb}
\usage{
plot_pbdb (query, title, colour, dir)
}
\arguments{
  \item{query}{a query to the PBDB database, it should have
  lng e.g. query<- pbdb_query_occurrences (limit="all",
  base_name="Canis", show="coords")}

  \item{title}{it sets the title of the map and the name of
  the png file}

  \item{colour}{colour of the points in the map. Turquoise
  by default.}

  \item{dir}{directory to save the plot}
}
\description{
returns a global map for our query. The size of the points
indicate the number of fossil records
}
\examples{
\dontrun{
canis<- pbdb_occurrences (limit="all", vocab= "pbdb", base_name="Canis", show="coords")
plot_pbdb (canis, "Canis",colour="red", dir="C:/Users/sara/Documents/_CIENCIAS/pbdb_paper")
}
}

