\name{pbdb_orig_ext}
\alias{pbdb_orig_ext}
\title{pbdb_orig_ext}
\usage{
pbdb_orig_ext (data, rank, orig_ext, colour, bord, do.plot)
}
\arguments{
  \item{data}{dataframe with our query to the paleoBD
  \code{\link{pbdb_occurrences}}. Important, it is required
  to show the name of the families, orders, etc. in the
  dataframe, to do that set: show=c("phylo", "ident") (see
  example).}

  \item{rank}{to set which taxon rank you are interested.
  By default rank= "species"}

  \item{orig_ext}{1= origination, 2=extinction.}

  \item{colour}{to change the colour of the bars in the
  plot, skyblue2 by default.}

  \item{bord}{to set the colour of the border of the
  polygon}

  \item{do.plot}{TRUE/FALSE (TRUE by default).}
}
\value{
a dataframe with the number of first appearances and
extinctions of the selected taxon rank across time, and a
plot with the first appearances or extinctions of the
selected taxon rank across time.
}
\description{
Plots the appearance of new taxa across time.
}
\examples{
\dontrun{
canidae<-  pbdb_occurrences (limit="all", vocab="pbdb",
base_name="Canidae", show=c("phylo", "ident"))
pbdb_orig_ext (canidae, rank="genus", orig_ext=1) # plot of the evolutive rates.
pbdb_orig_ext (canidae, rank="species", orig_ext=2) # plot of the extinction rates.
}
}

