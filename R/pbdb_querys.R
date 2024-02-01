# R Functions leveraging the use o the different API endpoints 
# available

#' .pbdb_query
#' 
#' Central function for sending all queries to the remote API
#'
#' @usage .pbdb_query(endpoint, query)
#'
#' @param endpoint Name of the endpoint, inside the API, to which the query must be sent. 
#' This endpoint must have been previously configured
#' @param query List of filter parameters for the api. The values provided to the parameters
#' may be a single string or a list of strings.
#' @return dataframe
#' @examples \dontrun{
#' .pbdb_query("occs/list", list(base_name="Canidae", show=c("coords", "classext", "ident")))
#' }
#' @noRd

.pbdb_query<-function(endpoint, query = list()){
  query <- lapply(query, .implode_to_string)
  uri <- .build_uri(endpoint, query = query)

  df <- .get_data_from_uri(uri)

  df
}


#' .implode_to_string
#'
#' Converts a list of strings in a single comma separated string
#' 
#' @param params list of strings
#' @return character
#' @examples \dontrun{
#' .implode_to_string(list("categoryA","categoryB","categoryC"))
#' }
#' @noRd

.implode_to_string<-function(params){
  
  if(!(is.vector(params))){
    stop("Vector expected")
  }
  
  if(length(params) > 1){
    str <- params[[1]]
    for (p in params[2:length(params)]) {
      str <- paste(str, ",", p, sep = "")
    }
  } else {
    str <- params
  }
  
  return (str)
}


#' pbdb_occurrence 
#' 
#' Returns information about a single occurrence record from the Paleobiology 
#' Database.
#' 
#' Documentation for all the parameters is available at 
#' https://paleobiodb.org/data1.2/occs/single. In the parameter list above, we
#' describe the most common filters that paleontologists and ecologists might
#' use.
#' 
#' @usage pbdb_occurrence(id, ...)
#' @param id identifier of the occurrence. This parameter is required
#' @param ... arguments passed to the API. See all available arguments in
#'   \url{https://paleobiodb.org/data1.2/occs/single}. E.g.:
#'   \itemize{
#'    \item \emph{vocab}: set vocab="pbdb" to show the complete name of the variables (by
#'      default variables have short 3-letter names)
#'   }
#' @return a dataframe with a single occurrence 
#' 
#' @export 
#' 
#' @examples \dontrun{
#'   pbdb_occurrence(id = 1001)
#'   pbdb_occurrence(id = 1001, vocab = "pbdb", show = "coords")
#' }
pbdb_occurrence <- function(id, ...) {
  l <- list(id = id, ...)
  .pbdb_query("occs/single", query = l)
}

#' pbdb_occurrences
#'
#' Returns information about species occurrence records stored in the
#' Paleobiology Database.
#'
#' Documentation for all the parameters is available 
#' at \url{https://paleobiodb.org/data1.2/occs/list}. We describe the most common
#' filters that paleontologists and ecologists might use in the parameter list above.
#'
#' Be aware that depending on the query, some columns may not be returned by the API
#' if those are empty across all the rows.
#' 
#' @param ... arguments passed to the API. See all available arguments in
#'   \url{https://paleobiodb.org/data1.2/occs/list}
#'   \itemize{
#'     \item \emph{limit}: Limits the number of records returned. The
#'       value may be a positive integer, zero, or "all".
#'     \item \emph{taxon_name}: Return only records associated with the 
#'       specified taxonomic name(s). 
#'       You may specify multiple names, separated by commas.
#'     \item \emph{base_name}: Return records associated with the
#'       specified taxonomic name(s) and any of their children
#'       (e.g. base_name = "Canis" will return "Canis", "Canis lupus",
#'       "Canis mosbachensis", etc.)
#'     \item \emph{lngmin}: numeric. The longitude boundaries will be normalized 
#'       to fall between -180 and 180. Note that if you specify 
#'       lngmin then you must also specify lngmax. 
#'       Returns only records whose geographic location falls 
#'       within the given bounding box (defined by lngmin, lngmax, 
#'       latmin, latmax).
#'       It generates two adjacent bounding boxes if the range crosses
#'       the antimeridian. 
#'     \item \emph{lngmax}: numeric. The longitude boundaries will be normalized 
#'       to fall between -180 and 180.
#'     \item \emph{latmin}: numeric. between -90 and 90. 
#'       Note that if you specify latmin then you must also specify latmax.
#'     \item \emph{latmax}: numeric. between -90 and 90.
#'     \item \emph{min_ma}: return only records whose temporal 
#'       locality is at least this old, specified in Ma.
#'     \item \emph{max_ma}: return only records whose temporal 
#'       locality is at most this old, specified in Ma.
#'     \item \emph{interval}: return only records whose temporal 
#'       locality falls within the named geologic time interval 
#'       (e.g. "Miocene").
#'     \item \emph{continent}: return only records whose geographic 
#'       location falls within the specified continent(s). 
#'     \item \emph{show}: to show extra variables (e.g. coords, classext, ident)
#'   }
#' @return a dataframe with the species occurrences 
#' 
#' @export 
#' 
#' @examples \dontrun{
#'   pbdb_occurrences(id = c(10, 11), show = c("coords", "classext", "ident"))
#'   pbdb_occurrences(
#'     limit = "all", vocab = "pbdb", taxon_name = "Canis",
#'     show = c("coords", "classext", "ident")
#'   )
#'   pbdb_occurrences(
#'     limit = "all", vocab = "pbdb", base_name = "Canidae",
#'     show = c("coords", "classext", "ident")
#'   )
#' }


pbdb_occurrences<-function(...){

  l<-list(...)
	.pbdb_query('occs/list', query = l)

}


#' pbdb_ref_occurrences
#' 
#' Returns information about the bibliographic references 
#' associated with fossil occurrences from the database.
#'
#' Go to \code{\link{pbdb_occurrences}} to see an explanation about the main 
#' filtering parameters. 
#'
#' @usage pbdb_ref_occurrences (...)
#'
#' @param ... arguments passed to the API. See all available arguments in
#'   \url{https://paleobiodb.org/data1.2/occs/refs}

## For now these parameters will be removed from the documentation
## because they produce an internal server error in this specific
## endpoint

# #'   \itemize{
# #'     \item \emph{ref_author} select only references for which any of the authors
# #'       matches the specified name
# #'     \item \emph{ref_pubyr} select only references published in the specified year
# #'     \item \emph{pub_title} select only references that involve the specified
# #'       publication
# #'   }

#' @return a dataframe with the information about the references that
#'   match the query
#' 
#' @export 
#' @examples \dontrun{
#'   pbdb_ref_occurrences(
#'     vocab = "pbdb", base_name = "Canis",
#'     latmin = 0, latmax = 35, lngmin = 0, lngmax = 35
#'   )
#'}


pbdb_ref_occurrences<-function(...){
  
  l<-list(...)
  .pbdb_query('occs/refs', query = l)
  
}


#' pbdb_collection 
#' 
#' Returns information about a single collection record from 
#' the Paleobiology Database.
#' 
#' Go to \code{\link{pbdb_occurrences}} to see an explanation about 
#' the main filtering parameters.
#' 
#' @usage pbdb_collection(id, ...)
#'  
#' @param id identifier of the collection. This parameter is required.
#' @param ... additional arguments passed to the API. See all available arguments in
#'   \url{https://paleobiodb.org/data1.2/colls/single}. E.g.:
#'   \itemize{
#'     \item \emph{vocab}: set vocab="pbdb" to show the complete name of the variables (by
#'       default variables have short 3-letter names)
#'     \item \emph{show}: show extra variables
#'     \item ...
#'   }
#'
#' @return a dataframe with a single occurrence 
#' 
#' @export 
#' @examples \dontrun{
#'   pbdb_collection(id = 1003, vocab = "pbdb", show = "loc")
#' }
pbdb_collection <- function(id, ...) {
  l <- list(id = id, ...)
  .pbdb_query("colls/single", query = l)
}

#' pbdb_collections
#'
#' Returns information about multiple collections, selected according
#' to the parameters you provide.
#'
#' @usage pbdb_collections(...)
#'
#' @param ... documentation for all the parameters is available in
#'   \url{https://paleobiodb.org/data1.2/colls/list}. Go to
#'   \code{\link{pbdb_occurrences}} to see an explanation about the
#'   main filtering parameters.
#'
#' @return a dataframe with the collections that match the query
#'
#' @export
#' @examples \dontrun{
#'   pbdb_collections(base_name = "Cetacea", interval = "Miocene")
#' }
pbdb_collections <- function(...) {
  l <- list(...)
  .pbdb_query("colls/list", query = l)
}

#' pbdb_collections_geo
#'
#' This path returns information about geographic clusters of
#' collections from the Paleobiology Database.  These clusters are
#' defined in order to facilitate the generation of maps at low
#' resolutions. You can make a config request via
#' https://paleobiodb.org/data1.2/config in order to get a list of the
#' available summary levels.
#'
#' @usage pbdb_collections_geo(..., level)
#'
#' @param ... documentation for all the parameters is available in
#'   \url{https://paleobiodb.org/data1.2/colls/summary}. Go to
#'   \code{\link{pbdb_occurrences}} to see an explanation about the
#'   main filtering parameters.
#' @param level an integer specifying a cluster level. Refer to
#'   \url{https://paleobiodb.org/data1.2/config.txt?show=clusters} for
#'   a list of available resolution levels ("cluster_level" column).
#'
#' @return a dataframe with the collections that match the query
#'
#' @export
#' @examples \dontrun{
#'   pbdb_collections_geo(
#'     vocab = "pbdb",
#'     lngmin = 0.0, lngmax = 15.0, latmin = 0.0, latmax = 15.0,
#'     level = 2
#'   )
#' }
pbdb_collections_geo <- function(..., level) {
  if (missing(level)) {
    err_msg <- strwrap(
      paste(
        "Parameter \"level\" is required. Refer to",
        "https://paleobiodb.org/data1.2/config.txt?show=clusters",
        "for a list of possible values."
      )
    )
    stop(paste(err_msg, collapse = "\n"))
  }

  l <- list(..., level = level)
  .pbdb_query("colls/summary", query = l)
}

#' pbdb_taxon
#'
#' Returns information about a single taxonomic name, identified
#' either by name or by identifier.
#'
#' @usage pbdb_taxon(...)
#' @param ... arguments passed to the API. See documentation for
#'   accepted parameters in
#'   \url{https://paleobiodb.org/data1.2/taxa/single}. One of the
#'   following parameters must be specified (but not both):
#'   \itemize{
#'     \item \emph{name}: returns information about the most fundamental
#'       taxonomic name matching this string.
#'       The \% and _ characters may be used as wildcards.
#'     \item \emph{id}: returns information about the taxonomic name
#'       corresponding to the specified identifier. The value can have
#'       different forms (see the API documentation in the link
#'       above).
#'   }
#' @return a dataframe with information from a single taxon
#'
#' @export
#' @examples \dontrun{
#'   pbdb_taxon(name = "Canis", vocab = "pbdb", show = c("attr", "app", "size"))
#' }
pbdb_taxon <- function(...) {
  l <- list(...)
  .pbdb_query("taxa/single", query = l)
}

#' pbdb_taxa
#' 
#'Returns information about multiple taxonomic names.  This function can be
#'used to query for all of the children or parents of a given taxon, among
#'other operations.
#'
#'@usage pbdb_taxa (...)
#'@param ... arguments passed to the API. See all available arguments in
#'  \url{https://paleobiodb.org/data1.2/taxa/list}
#'  \itemize{
#'    \item \emph{name}: returns information about the most fundamental 
#'      taxonomic name matching this string. 
#'      The \% and _ characters may be used as wildcards.
#'    \item \emph{id}: return information about the taxonomic name 
#'      corresponding to this identifier. You may not specify both 
#'      name and id in the same query.
#'    \item \emph{show}: to show extra variables. Some examples
#'      include: \emph{attr} the attribution of this taxon (author and
#'      year); \emph{app} the age of first and last appearance of this
#'      taxon from the occurrences recorded in this database;
#'      \emph{size} the number of subtaxa appearing in this database.
#'    \item \emph{rel}: set rel = "synonyms" to select all synonyms of
#'      the base taxon or taxa; rel = "children" to select the taxa
#'      immediately contained within the base taxon or taxa; rel =
#'      "common" to select the most specific taxon that contains all
#'      of the base taxa.
#'    \item \emph{extant}: TRUE/FALSE to select extant/extinct taxa.
#'  }

#' @return a dataframe with information from a list of taxa
#' 
#' @export 
#' @examples \dontrun{
#'   pbdb_taxa(name = "Canidae", vocab = "pbdb", show = c("attr", "app", "size", "nav"))
#'   pbdb_taxa(id = c(10, 11), vocab = "pbdb", show = c("attr", "app", "size", "nav"))
#'   pbdb_taxa(
#'     id = c(10, 11), vocab = "pbdb",
#'     show = c("attr", "app", "size", "nav"), rel = "common"
#'   )
#'}
pbdb_taxa <- function(...) {
  l <- list(...)
  .pbdb_query("taxa/list", query = l)
}

#' pbdb_taxa_auto
#'
#' Returns a list of names matching the given prefix or partial name.
#'
#' @usage pbdb_taxa_auto(...)
#' @param ... arguments passed to the API. See
#'   documentation for accepted parameters in
#'   \url{https://paleobiodb.org/data1.2/taxa/auto}. E.g.:
#'   \itemize{
#'     \item \emph{name}: a partial name or prefix.
#'       It must have at least 3 significant characters,
#'       and may include both a genus
#'       (possibly abbreviated) and a species.
#'     \item \emph{limit}: set the limit to the number of matches
#'     \item ...
#'   }

#' @return a dataframe with information about the matches
#' (taxon rank and number of occurrences in the database)
#'
#' @export
#' @examples \dontrun{
#'   pbdb_taxa_auto(name = "Cani", limit = 10)
#' }
pbdb_taxa_auto <- function(...) {
  l <- list(...)
  .pbdb_query("taxa/auto", query = l)
}

#' pbdb_interval
#'
#' Returns information about a single interval, selected by identifier.
#'
#' @usage pbdb_interval(...)
#'
#' @param ... additional arguments passed to the API. See
#'  documentation for accepted parameters at
#'  \url{https://paleobiodb.org/data1.2/intervals/single}. Either
#'   \code{name} or \code{id} must be specified, but both cannot be
#'   used in the same query:
#'   \itemize{
#'     \item \emph{name}: returns the interval with the specified
#'       name.
#'     \item \emph{id}: returns the interval corresponding to the
#'       specified identifier.
#'     \item \emph{vocab}: set vocab = "pbdb" to show the complete name
#'       of the variables (by default variables have short 3-letter
#'       names)
#'   }
#' @return a dataframe with information from a single temporal
#'   interval
#'
#' @export
#' @examples \dontrun{
#'   pbdb_interval(id = 1, vocab = "pbdb")
#' }
pbdb_interval <- function(...) {
  l <- list(...)

  .pbdb_query("intervals/single", query = l)
}

#' pbdb_intervals
#'
#' Returns information about multiple intervals, selected according to
#' the parameters you provide.
#'
#' @usage pbdb_intervals(...)
#'
#' @param ... arguments passed to the API. See documentation for
#'   accepted parameters in
#'   \url{https://paleobiodb.org/data1.2/intervals/list}. E.g.:
#'  \itemize{
#'    \item \emph{min_ma}: return only intervals that are at least this old
#'    \item \emph{max_ma}: return only intervals that are at most this old
#'    \item \emph{order}: return the intervals in order starting as specified.
#'      Possible values include age, name. Defaults to age
#'    \item \emph{vocab}: set vocab="pbdb" to show
#'      the complete name of the variables (by
#'      default variables have short 3-letter names)
#'    \item ...
#'  }
#'
#' @return a dataframe with information from several temporal intervals
#'
#' @export
#' @examples \dontrun{
#'   pbdb_intervals(min_ma = 0, max_ma = 2, vocab = "pbdb")
#' }
pbdb_intervals <- function(...) {
  l <- list(...)
  .pbdb_query("intervals/list", query = l)
}

#' pbdb_scale
#' 
#' Returns information about a single time scale, selected by 
#' identifier.
#'
#' @usage pbdb_scale(id, ...)
#' @param id identifier of the temporal interval. This parameter is required.
#' @param ... additional arguments passed to the API. See
#'   documentation for accepted parameters in
#'   \url{https://paleobiodb.org/data1.2/scales/single}. E.g.:
#'   \itemize{
#'     \item \emph{vocab}: set vocab="pbdb" to show the complete name of the variables (by
#'       default variables have short 3-letter names)
#'     \item ...
#'   }
#' @return a dataframe with information from a single scale
#' 
#' @export 
#' @examples \dontrun{
#'   pbdb_scale(id = 1, vocab = "pbdb")
#' }
pbdb_scale <- function(id, ...) {
  l <- list(id = id, ...)
  .pbdb_query("scales/single", query = l)
}

#' pbdb_scales
#' 
#' Returns information about multiple time scales.
#'
#' @param ... arguments passed to the API. See
#'   documentation for accepted parameters in
#'   \url{https://paleobiodb.org/data1.2/scales/list}. E.g.:
#'   \itemize{
#'     \item \emph{vocab}: set vocab="pbdb" to show the complete name of the variables (by
#'       default variables have short 3-letter names)
#'     \item ...
#'   }

#' @return a dataframe with information from the selected scales
#'  
#' @export 
#' @examples \dontrun{
#'   ## Get a dataframe with all the scales available in PBDB
#'   ## by setting no ids
#'   pbdb_scales()
#' }
pbdb_scales <- function(...) {
  l <- list(...)
  .pbdb_query("scales/list", query = l)
}

#'pbdb_strata
#'
#' Returns information about geological strata,
#' selected by name, rank, and/or geographic location.
#'
#' @usage pbdb_strata(...)
#' @param ... arguments passed to the API. See
#'   documentation for accepted parameters in
#'   \url{https://paleobiodb.org/data1.2/strata/list}. E.g.:
#'   \itemize{
#'     \item \emph{name}: a full or partial name. You can
#'       use \% and _ as wildcards, but the
#'       query will be very slow if you put a wildcard at the beginning
#'     \item \emph{rank}: returns only strata of the specified
#'       rank: formation, group or member.
#'     \item \emph{lngmin}: numeric. The longitude boundaries will be normalized to fall
#'       between -180 and 180. Note that if you specify lngmin then you must also
#'       specify lngmax. Returns only records whose geographic location falls within
#'       the given bounding box (defined by lngmin, lngmax, latmin, latmax). It
#'       generate two adjacent bounding boxes if the range crosses the antimeridian.
#'     \item \emph{lngmax}: numeric. The longitude boundaries will be normalized to fall
#'       between -180 and 180.
#'     \item \emph{latmin}: numeric. between -90 and 90. Note that if you specify latmin
#'       then you must also specify latmax.
#'     \item \emph{latmax}: numeric. between -90 and 90.
#'     \item \emph{loc}: Return only strata associated with some occurrence whose
#'       geographic location falls within the specified geometry, specified in WKT
#'       format.
#'     \item \emph{vocab}: set vocab = "pbdb" to show the complete
#'       name of the variables (by default variables have short
#'       3-letter names)
#'     \item ...
#'   }
#' @return a dataframe with information from the selected strata
#'
#' @export
#' @examples \dontrun{
#'   pbdb_strata(
#'     lngmin = 0, lngmax = 15, latmin = 0, latmax = 15,
#'     rank = "formation", vocab = "pbdb"
#'   )
#' }
pbdb_strata <- function(...) {
  l <- list(...)
  .pbdb_query("strata/list", query = l)
}

#' pbdb_strata_auto
#'
#' Returns a list of strata matching the given prefix or partial name.
#' This can be used to implement auto-completion for strata names,
#' and can be limited by geographic location if desired.
#'
#' @usage pbdb_strata_auto(...)
#'
#' @param ... arguments passed to the API. See
#'   documentation for accepted parameters in
#'   \url{https://paleobiodb.org/data1.2/strata/auto}. E.g.:
#'   \itemize{
#'     \item \emph{name}: a full or partial name. It must have at
#'     least 3 significant characters, and may end in a space followed
#'     by either 'g' or 'f' to indicate that you are looking for a
#'     group or formation.
#'     \item \emph{rank}: return only strata of the specified rank:
#'     formation or group. This may be overridden by a suffix on the
#'     value of name.
#'     \item \emph{lngmin}: numeric. The longitude boundaries will be normalized to fall
#'       between -180 and 180. Note that if you specify lngmin then you must also
#'       specify lngmax. Returns only records whose geographic location falls within
#'       the given bounding box (defined by lngmin, lngmax, latmin, latmax). It
#'       generates two adjacent bounding boxes if the range crosses the antimeridian.
#'     \item \emph{lngmax}: numeric. The longitude boundaries will be normalized to fall
#'       between -180 and 180.
#'     \item \emph{latmin}: numeric. between -90 and 90. Note that if you specify latmin
#'       then you must also specify latmax.
#'     \item \emph{latmax}: numeric. between -90 and 90.
#'     \item \emph{vocab}: set vocab="pbdb" to show the complete name of the variables (by
#'       default variables have short 3-letter names)
#'     \item ...
#'   }
#' @return a dataframe with information from the strata that matches
#'   the \code{name} parameter.
#'
#' @export
#' @examples \dontrun{
#'   pbdb_strata_auto(name = "Pin", vocab = "pbdb")
#' }
pbdb_strata_auto <- function(...) {
  l <- list(...)
  .pbdb_query("strata/auto", query = l)
}

#' pbdb_reference
#' 
#' Returns information about a single reference, selected by identifier.
#' Go to \code{\link{pbdb_occurrences}} to see an explanation about the main filtering parameters 
#' 
#' @usage pbdb_reference(id, ...)
#' 
#' @param id identifier of the reference. This parameter is required.
#' @param ... arguments passed to the API. See
#'   documentation for accepted parameters in
#'   \url{https://paleobiodb.org/data1.2/refs/single}. E.g.:
#'   \itemize{
#'     \item \emph{vocab}: set vocab="pbdb" to show the complete name of the variables (by
#'       default variables have short 3-letter names)
#'     \item ...
#'   }
#' @return a dataframe with a single reference 
#' @export 
#' @examples \dontrun{
#'   pbdb_reference(id = 1003, vocab = "pbdb", show = "both")
#' }
pbdb_reference <- function(id, ...) {
  l <- list(id = id, ...)
  .pbdb_query("refs/single", query = l)
}

#' pbdb_references
#' 
#' Returns information about multiple references, selected according to the parameters you provide.
#' 
#' @usage pbdb_references(...)
#' 
#' @param ... arguments passed to the API. See
#'   documentation for accepted parameters in
#'   \url{https://paleobiodb.org/data1.2/refs/list}. E.g.:
#'   \itemize{
#'     \item \emph{ref_author} select only references for which any of the authors
#'       matches the specified name
#'     \item \emph{ref_pubyr} select only references published in the specified year
#'     \item \emph{pub_title} select only references that involve the specified
#'       publication
#'     \item \emph{order} specifies the order in which the results are
#'       returned. You can specify multiple values separated by
#'       commas, and each value may be appended with .asc or
#'       .desc. Accepted values are: author, pubyr, reftitle,
#'       pubtitle, pubtype, created, modified, rank.
#'     \item ...
#'   }
#' @return a dataframe with the information about the references that match the query
#'  
#' @export
#' @examples \dontrun{
#'   pbdb_references(ref_author = "Polly")
#' }


pbdb_references<-function(...){
  
  l<-list(...)
  .pbdb_query('refs/list', query = l)
  
}


#' pbdb_ref_collections
#' 
#' Returns information about the references from which the selected collection data were entered.
#' 
#' @usage pbdb_ref_collections(...)
#' @param ... arguments passed to the API. See
#'   documentation for accepted parameters in
#'   \url{https://paleobiodb.org/data1.2/colls/refs}. E.g.:
#'   \itemize{
#'     \item \emph{id} comma-separated list of collection identifiers
#'     \item \emph{ref_author} select only references for which any of the authors
#'       matches the specified name
#'     \item \emph{ref_pubyr} select only references published in the specified year
#'     \item \emph{pub_title} select only references that involve the specified
#'       publication
#'     \item \emph{order} specifies the order in which the results are
#'       returned. You can specify multiple values separated by
#'       commas, and each value may be appended with .asc or
#'       .desc. Accepted values are: author, pubyr, reftitle,
#'       pubtitle, pubtype, created, modified, rank.
#'     \item ...
#'   }
#' @return a dataframe with the information about the references that match the query
#' 
#' @export 
#' @examples \dontrun{
#'   pbdb_ref_collections(id = 1)
#' }


pbdb_ref_collections <-function(...){
  
  l<-list(...)
  .pbdb_query('colls/refs', query = l)
  
}


#' pbdb_ref_taxa
#'
#' This URL path returns information about the source references associated
#' with taxa in the Paleobiology Database. You can use the same parameters
#' that are available with pbdb_taxa, but Reference records are returned
#' instead of Taxon records. One record is returned per reference,
#' even if it is associated with multiple taxa.
#'
#' @usage pbdb_ref_taxa(...)
#' @param ... arguments passed to the API. See all available arguments in
#'   \url{https://paleobiodb.org/data1.2/taxa/refs}
#'   \itemize{
#'     \item \emph{name}: returns information about the most fundamental
#'       taxonomic name matching this string.
#'       The \% and _ characters may be used as wildcards.
#'     \item \emph{id}: returns information about the taxonomic name
#'       corresponding to this identifier. You may not specify both
#'       name and id in the same query.
#'     \item \emph{show}: show extra variables
#'     \item \emph{rel}: set rel = "synonyms" to select all synonyms
#'       of the base taxon or taxa; rel = "children" to select the
#'       taxa immediately contained within the base taxon or taxa; rel
#'       = "all_children" to select all taxa contained within each
#'       matching taxon and within all synonymous taxa; rel =
#'       "all_parents" to select all taxa that contain any of the
#'       matching taxa.
#'     \item \emph{extant}: TRUE/FALSE to select extant/extinct taxa.
#'   }
#' @return a dataframe with references from a list of taxa
#'
#' @export
#' @examples \dontrun{
#'   pbdb_ref_taxa(
#'     name = "Canidae", vocab = "pbdb", show = c("both", "comments")
#'   )
#' }
pbdb_ref_taxa <- function(...) {
  l <- list(...)
  .pbdb_query("taxa/refs", query = l)
}
