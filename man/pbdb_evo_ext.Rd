\name{pbdb_evo_ext}
\alias{pbdb_evo_ext}
\title{pbdb_evo}
\usage{
pbdb_evo (data, rank, colour, bord, do.plot)
}
\arguments{
  \item{data}{dataframe with our query to the paleoBD
  \code{\link{pbdb_occurrences}}. Important, it is required
  to show the name of the families, orders, etc. in the
  dataframe, to do that set: show=c("phylo", "ident") (see
  example).}

  \item{rank}{to set which taxon rank you are interested.
  By default rank= "species"}

  \item{colour}{to change the colour of the bars in the
  plot, skyblue2 by default.}

  \item{bord}{to set the colour of the border of the
  polygon}

  \item{evo_ext}{1= evolution, 2=extinction.}

  \item{do.plot}{TRUE/FALSE (TRUE by default).}
}
\value{
a dataframe with the number of new appearances and
extinctions of the selected taxon rank across time, and a
plot with the appearances or extinctions of the selected
taxon rank across time.
}
\description{
Plots the appearance of new taxa across time.
}
\examples{
\dontrun{
canidae<-  pbdb_occurrences (limit="all", vocab="pbdb",
base_name="Canidae", show=c("phylo", "ident"))
pbdb_evo_ext (canidae, rank="genus", evo_ext=1) # plot of the evolutive rates.
pbdb_evo_ext (canidae, rank="species", evo_ext=2) # plot of the extinction rates.
}
}

