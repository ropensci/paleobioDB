\name{pbdb_map_richness}
\alias{pbdb_map_richness}
\title{pbdb_map_richness}
\usage{
pbdb_map_effort (data, rank='species', do.plot=T, res=1, col.int='white', col.ocean='black', col.rich=c('light blue','blue'),...)
}
\arguments{
  \item{data}{Input dataframe. This dataframe is the output
  of \code{\link{pbdb_occurrences}} function using the
  argument: \code{\link{show = c('phylo', 'coords',
  'ident')}}. See too: \strong{Details} and
  \strong{Examples}}

  \item{rank}{To set which taxon rank you are interested
  for calculate richness. The options are: "species",
  "genus", "family", "order", "class" or "phylum")}

  \item{do.plot}{Logical; If \code{\link{TRUE}} specifies
  that plotting should be done}

  \item{res}{The resolution of the RasterLayer object (in
  decimal degrees). See: \code{\link{raster}} ()}

  \item{col.int}{The color of mainland}

  \item{col.ocean}{The color of ocean}

  \item{col.rich}{Two or more colors. This generates a
  color gradient and, is used to show the richness in each
  cell}

  \item{...}{Others parameters. See \code{\link{par}} and
  \code{\link{map()}}}
}
\value{
A map of occurences
}
\description{
Create a RasterLayer object with richness of fossil records
by cell. Furthermore this function maps the richness of the
fossil records
}
\details{
\strong{CAUTION!} Is required use the argument
\code{\link{show = c('phylo', 'coords', 'ident')}} in
\code{\link{pbdb_occurrences}} function. See
\strong{Examples}

Only cairo \code{\link{x11}}( ) devices are supported. See
\strong{Examples}

The simplest use of this function is:
\code{\link{pbdb_map_richness(data)}}.

Before running the function
\code{\link{pbdb_map_richness(...)}}, we recommended to
open the \code{\link{x11()}} device, maximize and minimize.
}
\examples{
\dontrun{
data<- pbdb_occurrences (limit=1000, vocab= "pbdb", base_name="mammalia", show="coords")
x11() ## maximize and minimize
r<-pbdb_map_richness (data,res=3,rank='genus')
## Error in pbdb_map_richness(data, res = 3, rank = "genus") :
## Invalid data input. Use in "pbdb_occurrences" function the argument: show=c("phylo","coords","ident").
## e.g. pbdb_occurrences(..., show=c("phylo","coords","ident")).
## See "pbdb_map_richness" help page
data<- pbdb_occurrences (limit=1000, vocab= "pbdb", base_name="mammalia", show=c("phylo","coords","ident"))
r1<- pbdb_map_richness (data,res=3,rank='genus')
r2<- pbdb_map_richness (data,res=3,rank='family')
r3<- pbdb_map_richness (data,res=3,rank='family',do.plot=F)
}
}

