\name{pbdb_taxon}
\alias{pbdb_taxon}
\title{pbdb_taxon}
\usage{
pbdb_taxon(...)
}
\arguments{
  \item{name}{Return information about the most fundamental
  taxonomic name matching this string. The % and _
  characters may be used as wildcards.}

  \item{id}{Return information about the taxonomic name
  corresponding to this identifier. You may not specify
  both name and id in the same query.}

  \item{...}{documentation for all the parameters is
  available in http://paleobiodb.org/data1.1/taxa/single go
  to ?pbdb_taxa to see an explanation about the main
  filtering parameters}
}
\value{
a dataframe with information from a single taxon
}
\description{
Returns information about a single taxonomic name,
identified either by name or by identifier.
}
\examples{
\dontrun{
pbdb_taxon (name="Canis", vocab="pbdb", show=c("attr", "app", "size"))

}
}

