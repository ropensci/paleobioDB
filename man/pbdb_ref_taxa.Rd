\name{pbdb_ref_taxa}
\alias{pbdb_ref_taxa}
\title{pbdb_ref_taxa}
\usage{
pbdb_ref_taxa (...)
}
\arguments{
  \item{...}{documentation for all the parameters is
  available. in http://paleobiodb.org/data1.1/taxa/refs.
  For instance:

  \code{name} Return information about the most fundamental
  taxonomic name matching this string. The % and _
  characters may be used as wildcards. \code{id} Return
  information about the taxonomic name corresponding to
  this identifier. You may not specify both name and id in
  the same query. \code{exact} if this parameter is
  specified, then the taxon exactly matching the specified
  name or identifier is selected, rather than the senior
  synonym which is the default. \code{show} to show extra
  variables (see documentation for show in
  http://paleobiodb.org/data1.1/taxa/refs) \code{rel} set
  rel="synonyms" to select all synonyms of the base taxon
  or taxa; rel="children" to select the taxa immediately
  contained within the base taxon or taxa;
  rel="common_ancestor" to select the most specific taxon
  that contains all of the base taxa. \code{extant}
  TRUE/FALSE to select extinct/extant taxa.}
}
\value{
a dataframe with references from a list of taxa
}
\description{
This URL path returns information about the source
references associated with taxa in the Paleobiology
Database. You can use the same parameters that are
available with pbdb_taxa, but Reference records are
returned instead of Taxon records. One record is returned
per reference, even if it is associated with multiple taxa.
}
\examples{
\dontrun{
pbdb_ref_taxa (name="Canidae", vocab="pbdb", show=c("attr", "app", "size", "nav"))
}
}

