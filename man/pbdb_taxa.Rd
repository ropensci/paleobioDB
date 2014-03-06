\name{pbdb_taxa}
\alias{pbdb_taxa}
\title{pbdb_taxa}
\usage{
pbdb_taxa(...)
}
\arguments{
  \item{name}{Return information about the most fundamental
  taxonomic name matching this string. The % and _
  characters may be used as wildcards.}

  \item{id}{Return information about the taxonomic name
  corresponding to this identifier. You may not specify
  both name and id in the same query.}

  \item{exact}{if this parameter is specified, then the
  taxon exactly matching the specified name or identifier
  is selected, rather than the senior synonym which is the
  default.}

  \item{show}{to show extra variables: attr (The
  attribution of this taxon (author and year)), app (The
  age of first and last appearance of this taxon from the
  occurrences recorded in this database), size (The number
  of subtaxa appearing in this database), nav (Additional
  information for the PBDB Navigator taxon browser)}

  \item{rel}{set rel="synonyms" to select all synonyms of
  the base taxon or taxa; rel="children" to select the taxa
  immediately contained within the base taxon or taxa;
  rel="common_ancestor" to select the most specific taxon
  that contains all of the base taxa.}

  \item{extant}{TRUE/FALSE to select extinct/extant taxa.}

  \item{...}{documentation for all the parameters is
  available.

  in http://paleobiodb.org/data1.1/taxa/list}
}
\value{
a dataframe with information from a list of taxa
}
\description{
Returns information about multiple taxonomic names. This
function can be used to query for all of the children or
parents of a given taxon, among other operations.
}
\examples{
\dontrun{
pbdb_taxa (name="Canidae", vocab="pbdb", show=c("attr", "app", "size", "nav"))
pbdb_taxa (id =c(10, 11), vocab="pbdb", show=c("attr", "app", "size", "nav"))
pbdb_taxa (id =c(10, 11), vocab="pbdb", show=c("attr", "app", "size", "nav"), rel="common_ancestor")
}
}

