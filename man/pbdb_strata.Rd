% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pbdb_querys.R
\name{pbdb_strata}
\alias{pbdb_strata}
\title{pbdb_strata}
\usage{
pbdb_strata (...)
}
\arguments{
\item{...}{arguments passed to the API. See
documentation for accepted parameters in
\url{http://paleobiodb.org/data1.1/strata/list}. Eg:
\itemize{
  \item \emph{name}: a full or partial name. You can
    use \% and _ as wildcards, but the
    query will be very slow if you put a wildcard at the beginning
  \item \emph{rank}: returns only strata of the specified 
    rank: formation, group or member.
  \item \emph{lngmin}: numeric. The longitude boundaries will be normalized to fall
    between -180 and 180. Note that if you specify lngmin then you must also
    specify lngmax. Returns only records whose geographic location falls within
    the given bounding box (defined by lngmin, lngmax, latmin, latmax). It
    generate two adjacent bounding boxes if the range crosses the antimeridian. 
  \item \emph{lngmax}: numeric. The longitude boundaries will be normalized to fall
    between -180 and 180.
  \item \emph{latmin}: numeric. between -90 and 90. Note that if you specify latmin
    then you must also specify latmax.
  \item \emph{latmax}: numeric. between -90 and 90.
  \item \emph{loc}: Return only strata associated with some occurrence whose
    geographic location falls within the specified geometry, specified in WKT
    format.
  \item \emph{vocab}: set vocab="pbdb" to show the complete name of the variables (by
    default variables have short 3-letter names)
  \item ...
}}
}
\value{
a dataframe with information from the selected strata
}
\description{
Returns information about geological strata, 
selected by name, rank, and/or geographic location.
}
\examples{
\dontrun{
pbdb_strata (lngmin=0, lngmax=15, latmin=0, latmax=15, rank="formation", vocab="pbdb") 
}
}

