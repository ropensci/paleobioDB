\name{pbdb_ref_occurrences}
\alias{pbdb_ref_occurrences}
\title{pbdb_ref_occurrences}
\usage{
pbdb_ref_occurrences(...)
}
\arguments{
  \item{author}{Select only references for which any of the
  authors matches the specified name}

  \item{year}{Select only references published in the
  specified year}

  \item{pubtitle}{Select only references that involve the
  specified publication}

  \item{order}{Specifies the order in which the results are
  returned. You can specify multiple values separated by
  commas, and each value may be appended with .asc or
  .desc.  Accepted values are: author, year, pubtitle,
  created, modified, rank. (see documentation in
  http://paleobiodb.org/data1.1/occs/refs)}

  \item{...}{documentation for all the parameters is
  available in http://paleobiodb.org/data1.1/occs/refs go
  to ?pbdb_occurrences to see an explanation about the main
  filtering parameters}
}
\value{
a dataframe with the information about the references that
match the query
}
\description{
Returns information about the bibliographic references
associated with fossil occurrences from the database.
}
\examples{
\dontrun{
pbdb_ref_occurrences (vocab="pbdb", taxon_name="Canis", year=2000)
}
}

