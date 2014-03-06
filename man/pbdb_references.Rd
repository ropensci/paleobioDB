\name{pbdb_references}
\alias{pbdb_references}
\title{pbdb_references}
\usage{
pbdb_references(...)
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
  created, modified, rank.}

  \item{...}{documentation for all the parameters is
  available in http://paleobiodb.org/data1.1/refs/list}
}
\value{
a dataframe with the information about the references that
match the query
}
\description{
Returns information about multiple references, selected
according to the parameters you provide.
}
\examples{
\dontrun{
pbdb_references (author="Turner")
}
}

