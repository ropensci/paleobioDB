# R Functions leveraging the use of the different API endpoints
# available

#' .pbdb_query
#'
#' Central function for sending all queries to the remote API
#'
#' @param endpoint Name of the endpoint, inside the API, to which the
#'   query must be sent.  This endpoint must have been previously
#'   configured
#' @param query List of filter parameters for the api. The values
#'   provided to the parameters may be a single string or a list of
#'   strings.
#' @returns data frame
#' @examples \dontrun{
#'   .pbdb_query(
#'     "occs/list",
#'     list(base_name = "Canidae", show = c("coords", "classext", "ident"))
#'   )
#' }
#' @noRd
.pbdb_query <- function(endpoint, query = list()) {
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
#' @returns character
#' @examples \dontrun{
#'   .implode_to_string(list("categoryA", "categoryB", "categoryC"))
#' }
#' @noRd
.implode_to_string <- function(params) {
  if (!(is.vector(params))) {
    stop("Vector expected")
  }

  if (length(params) > 1) {
    str <- params[[1]]
    for (p in params[2:length(params)]) {
      str <- paste(str, ",", p, sep = "")
    }
  } else {
    str <- params
  }

  return(str)
}

#' Get information about a single occurrence record
#'
#' Returns information about a single occurrence record from the Paleobiology
#' Database.
#'
#' Documentation for all the parameters is available at
#' <https://paleobiodb.org/data1.2/occs/single>. In the parameter
#' list above, we describe the most common filters that
#' paleontologists and ecologists might use.
#'
#' @param id Identifier of the occurrence. This parameter is required.
#' @param ... Arguments passed to the API. See all available arguments at
#'   <https://paleobiodb.org/data1.2/occs/single>. E.g.:
#'
#' * `vocab`: Set to `"pbdb"` to show the complete name of the
#'   variables (by default variables have short 3-letter names).
#'
#' * `show`: Select additional blocks of information to be returned
#'   along with the basic record.  Some possible values include:
#'
#'     * `"class"`: The taxonomic classification of the
#'        occurence: phylum, class, order, family, genus.
#'
#'     * `"coords"`: The latitude and longitude of this
#'        occurrence.
#'
#'     * `"loc"`: Additional information about the
#'        geographic locality of the occurrence
#'
#'     * `"stratext"`: Detailed information about the
#'        stratigraphic context of the occurrence.
#'
#'     * `"lithext"`: Detailed information about the
#'        lithological context of the occurrence.
#'
#' @returns A data frame with a single occurrence.
#'
#' @export
#'
#' @examples \dontrun{
#'   pbdb_occurrence(id = 1001)
#'   pbdb_occurrence(id = 1001, vocab = "pbdb", show = c("class", "coords"))
#' }
pbdb_occurrence <- function(id, ...) {
  l <- list(id = id, ...)
  .pbdb_query("occs/single", query = l)
}

#' Get information about fossil occurrence records
#'
#' Returns information about fossil occurrence records stored in the
#' Paleobiology Database.
#'
#' Documentation for all the parameters is available at
#' <https://paleobiodb.org/data1.2/occs/list>. We describe the
#' most common filters that paleontologists and ecologists might use
#' in the parameter list above.
#'
#' Be aware that depending on the query, some columns may not be
#' returned by the API if those are empty across all the rows.
#'
#' @param ... Arguments passed to the API. See all available arguments at
#'   <https://paleobiodb.org/data1.2/occs/list>.
#'
#' * `limit`: Limits the number of records returned. The value may be
#'   a positive integer, zero, or `"all"`.
#'
#' * `taxon_name`: Return only records associated with the specified
#'   taxonomic name(s).  You may specify multiple names.
#'
#' * `base_name`: Return records associated with the specified
#'   taxonomic name(s) and any of their children (e.g. `base_name =
#'   "Canis"` will return "Canis", "Canis lupus",
#'   "Canis mosbachensis", etc.)
#'
#' * `lngmin`: Numeric. The longitude boundaries will be normalized to
#'   fall between -180 and 180. Note that if you specify `lngmin` then
#'   you must also specify `lngmax`.  Returns only records whose
#'   geographic location falls within the given bounding box (defined
#'   by `lngmin`, `lngmax`, `latmin`, `latmax`).  It generates two
#'   adjacent bounding boxes if the range crosses the antimeridian.
#'
#' * `lngmax`: Numeric. The longitude boundaries will be normalized to
#'   fall between -180 and 180.
#'
#' * `latmin`: Numeric value between -90 and 90.  Note that if you
#'    specify `latmin` then you must also specify `latmax`.
#'
#' * `latmax`: Numeric value between -90 and 90.
#'
#' * `min_ma`: Return only records whose temporal locality is at least
#'   this old, specified in millions of years.
#'
#' * `max_ma`: Return only records whose temporal locality is at most
#'   this old, specified in millions of years.
#'
#' * `interval`: Return only records whose temporal locality falls
#'   within the named geologic time interval (e.g. "Miocene").
#'
#' * `cc`: Return only records whose location falls within the
#'   specified geographic regions. The value of this parameter should
#'   be one or more two-character country codes and/or three-character
#'   continent codes as a comma-separated list (see
#'   <https://en.wikipedia.org/wiki/ISO_3166-1_alpha-2> and
#'   <https://paleobiodb.org/data1.2/config.txt?show=continents>). If
#'   the parameter value starts with !, then records falling into
#'   these regions are excluded instead of included. Any country codes
#'   starting with ^ are subtracted from the filter.
#'
#' * `show`: Show extra variables (e.g. `"coords"`, `"classext"`,
#'   `"ident"`).
#'
#' @returns A data frame with the fossil occurrences.
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
pbdb_occurrences <- function(...) {
  l <- list(...)
  .pbdb_query("occs/list", query = l)
}

#' Get references associated with fossil occurrences
#'
#' Returns information about the bibliographic references associated
#' with fossil occurrences from the database.
#'
#' Go to [pbdb_occurrences()] to see an explanation about
#' the main filtering parameters.
#'
#' @param ... arguments passed to the API. See all available arguments at
#'   <https://paleobiodb.org/data1.2/occs/refs>
#'
#' * `ref_author`: Select only references for which any of the authors
#'   matches the specified name.
#'
#' * `ref_pubyr`: Select only references published in the specified
#'   year.
#'
#' * `pub_title`: Select only references that involve the specified
#'   publication.
#'
#' @returns A data frame with the information about the references that
#'   match the query.
#'
#' @export
#' @examples \dontrun{
#'   pbdb_ref_occurrences(vocab = "pbdb", base_name = "Canis", ref_pubyr = 2000)
#' }
pbdb_ref_occurrences <- function(...) {
  l <- list(...)
  .pbdb_query("occs/refs", query = l)
}

#' Get information about a single collection record
#'
#' Returns information about a single collection record from
#' the Paleobiology Database.
#'
#' Go to [pbdb_occurrences()] to see an explanation about
#' the main parameters.
#'
#' @param id Identifier of the collection. This parameter is required.
#' @param ... Additional arguments passed to the API. See all
#'   available arguments at
#'   <https://paleobiodb.org/data1.2/colls/single>. E.g.:
#'
#' * `vocab`: Set to "pbdb" to show the complete name of the variables
#'       (by default variables have short 3-letter names).
#'
#' * `show`: Select additional blocks of information to be returned
#'       along with the basic record.  Some possible values include:
#'
#'     * `"loc"`: Additional information about the geographic locality
#'       of the collection
#'
#'     * `"stratext"`: Detailed information about the stratigraphic
#'       context of collection.
#'
#'     * `"lithext"`: Detailed information about the lithological
#'       context of the collection.
#'
#' @returns A data frame with a single occurrence.
#'
#' @export
#' @examples \dontrun{
#'   pbdb_collection(id = 1003, vocab = "pbdb", show = c("loc", "stratext"))
#' }
pbdb_collection <- function(id, ...) {
  l <- list(id = id, ...)
  .pbdb_query("colls/single", query = l)
}

#' Get information about multiple collections
#'
#' Returns information about multiple collections, selected according
#' to the parameters you provide.
#'
#' @param ... Additional arguments passed to the API.  See all
#'   available arguments at
#'   <https://paleobiodb.org/data1.2/colls/list>. Go to
#'   [pbdb_occurrences()] to see an explanation about the
#'   main filtering parameters.
#'
#' @returns A data frame with the collections that match the query.
#'
#' @export
#' @examples \dontrun{
#'   pbdb_collections(base_name = "Cetacea", interval = "Miocene")
#' }
pbdb_collections <- function(...) {
  l <- list(...)
  .pbdb_query("colls/list", query = l)
}

#' Get information about geographic clusters of collections
#'
#' This path returns information about geographic clusters of
#' collections from the Paleobiology Database. These clusters are
#' defined in order to facilitate the generation of maps at low
#' resolutions. You can make a config request via
#' <https://paleobiodb.org/data1.2/config> in order to get a list
#' of the available summary levels.
#'
#' @param level An integer specifying a cluster level. Refer to
#'   <https://paleobiodb.org/data1.2/config.txt?show=clusters> for
#'   a list of available resolution levels ("cluster_level" column).
#' @param ... Documentation for all the parameters is available at
#'   <https://paleobiodb.org/data1.2/colls/summary>. Go to
#'   [pbdb_occurrences()] to see an explanation about the
#'   main filtering parameters.
#'
#' @returns A data frame with the collections that match the query.
#'
#' @export
#' @examples \dontrun{
#'   pbdb_collections_geo(
#'     level = 2,
#'     vocab = "pbdb",
#'     lngmin = 0.0, lngmax = 15.0, latmin = 0.0, latmax = 15.0
#'   )
#' }
pbdb_collections_geo <- function(level, ...) {
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

#' Get information about a single taxonomic name
#'
#' Returns information about a single taxonomic name, identified
#' either by name or by identifier.
#'
#' @param ...
#'
#' Arguments passed to the API. See documentation for accepted
#' parameters at <https://paleobiodb.org/data1.2/taxa/single>. One of
#' the following parameters must be specified (but not both):
#'
#' * `name`: Returns information about the most fundamental taxonomic
#'   name matching this string.  The % and _ characters may be used as
#'   wildcards.
#'
#' * `id`: Returns information about the taxonomic name corresponding
#'   to the specified identifier. The value can have different forms
#'   (see the API documentation in the link above).
#'
#' @returns A data frame with information from a single taxon.
#'
#' @export
#' @examples \dontrun{
#'   pbdb_taxon(name = "Canis", vocab = "pbdb",
#'              show = c("attr", "app", "size"))
#' }
pbdb_taxon <- function(...) {
  l <- list(...)
  .pbdb_query("taxa/single", query = l)
}

#' Get information about multiple taxonomic names
#'
#' Returns information about multiple taxonomic names.  This function
#' can be used to query for all of the children or parents of a given
#' taxon, among other operations.
#'
#' @param ... Arguments passed to the API. See all available arguments
#'   at <https://paleobiodb.org/data1.2/taxa/list>.
#'
#' * `name`: Returns information about the most fundamental taxonomic
#'   name matching this string.  The % and _ characters may be used as
#'   wildcards.
#'
#' * `id`: Return information about the taxonomic name corresponding
#'   to this identifier. You may not specify both `name` and `id` in
#'   the same query.
#'
#' * `show`: Show extra variables. Some examples include: `"attr"` the
#'   attribution of this taxon (author and year); `"app"` the age of
#'   first and last appearance of this taxon from the occurrences
#'   recorded in this database; `"size"` the number of subtaxa
#'   appearing in this database.
#'
#' * `rel`: Set `rel = "synonyms"` to select all synonyms of the base
#'   taxon or taxa; `rel = "children"` to select the taxa immediately
#'   contained within the base taxon or taxa; `rel = "common"` to
#'   select the most specific taxon that contains all of the base
#'   taxa.
#'
#' * `extant`: Logical indicating whether to select only extant or
#'   non-extant taxa.
#'
#' @returns A data frame with information from a list of taxa.
#'
#' @export
#' @examples \dontrun{
#'   pbdb_taxa(name = "Canidae", rel = "all_parents", vocab = "pbdb",
#'             show = c("attr", "app", "size", "class"))
#'   pbdb_taxa(id = c(10, 11), vocab = "pbdb",
#'             show = c("attr", "app", "size", "class"))
#'   pbdb_taxa(
#'     id = c(10, 11), vocab = "pbdb",
#'     show = c("attr", "app", "size", "class"), rel = "common"
#'   )
#'}
pbdb_taxa <- function(...) {
  l <- list(...)
  .pbdb_query("taxa/list", query = l)
}

#' Get a list of taxonomic names matching a prefix or partial name
#'
#' Returns a list of taxonomic names matching the given prefix or
#' partial name.
#'
#' @param ... Arguments passed to the API. See documentation for
#'   accepted parameters at
#'   <https://paleobiodb.org/data1.2/taxa/auto>. E.g.:
#'
#' * `name`: A partial name or prefix.  It must have at least 3
#'   significant characters, and may include both a genus (possibly
#'   abbreviated) and a species.
#'
#' * `limit`: Set the limit to the number of matches.
#'
#' @returns A data frame with information about the matches (taxon rank
#'   and number of occurrences in the database).
#'
#' @export
#' @examples \dontrun{
#'   pbdb_taxa_auto(name = "Cani", limit = 10)
#' }
pbdb_taxa_auto <- function(...) {
  l <- list(...)
  .pbdb_query("taxa/auto", query = l)
}

#' Get information about a single interval
#'
#' Returns information about a single interval, selected by
#' identifier.
#'
#' @param ... Additional arguments passed to the API. See
#'   documentation for accepted parameters at
#'   <https://paleobiodb.org/data1.2/intervals/single>. Either `name`
#'   or `id` must be specified, but both cannot be used in the same
#'   query:
#'
#' * `name`: Returns the interval with the specified name.
#'
#' * `id`: Returns the interval corresponding to the specified
#'   identifier.
#'
#' * `vocab`: Set to `"pbdb"` to show the complete name of the
#'   variables (by default variables have short 3-letter names).
#'
#' @returns A data frame with information from a single temporal
#'   interval.
#'
#' @export
#' @examples \dontrun{
#'   pbdb_interval(id = 1, vocab = "pbdb")
#' }
pbdb_interval <- function(...) {
  l <- list(...)
  .pbdb_query("intervals/single", query = l)
}

#' Get information about multiple intervals
#'
#' Returns information about multiple intervals, selected according to
#' the parameters you provide.
#'
#' @param ... arguments passed to the API. See documentation for
#'   accepted parameters at
#'   <https://paleobiodb.org/data1.2/intervals/list>. E.g.:
#'
#' * `min_ma`: Return only intervals that are at least this old.
#'
#' * `max_ma`: Return only intervals that are at most this old.
#'
#' * `order`: Return the intervals in order starting as specified.
#'   Possible values include `"age"`, `"name"`. Defaults to `"age"`.
#'
#' * `vocab`: Set to `"pbdb"` to show the complete name of the
#'   variables (by default variables have short 3-letter names).
#'
#' @returns A data frame with information from several temporal intervals.
#'
#' @export
#' @examples \dontrun{
#'   pbdb_intervals(min_ma = 0, max_ma = 2, vocab = "pbdb")
#' }
pbdb_intervals <- function(...) {
  l <- list(...)
  .pbdb_query("intervals/list", query = l)
}

#' Get information about a single time scale
#'
#' Returns information about a single time scale, selected by
#' identifier.
#'
#' @param id Identifier of the temporal interval. This parameter is
#'   required.
#' @param ... Additional arguments passed to the API. See
#'   documentation for accepted parameters at
#'   <https://paleobiodb.org/data1.2/scales/single>. E.g.:
#'
#' * `vocab`: Set to `"pbdb"` to show the complete name of the
#'   variables (by default variables have short 3-letter names).
#'
#' @returns A data frame with information from a single scale.
#'
#' @export
#' @examples \dontrun{
#'   pbdb_scale(id = 1, vocab = "pbdb")
#' }
pbdb_scale <- function(id, ...) {
  l <- list(id = id, ...)
  .pbdb_query("scales/single", query = l)
}

#' Get information about multiple time scales
#'
#' Returns information about multiple time scales.
#'
#' @param ... Arguments passed to the API. See
#'   documentation for accepted parameters at
#'   <https://paleobiodb.org/data1.2/scales/list>. E.g.:
#'
#' * `vocab`: Set to `"pbdb"` to show the complete name of the
#'   variables (by default variables have short 3-letter names).
#'
#' @returns A data frame with information from the selected scales.
#'
#' @export
#' @examples \dontrun{
#'   ## Get a data frame with all the scales available in PBDB
#'   ## by setting no ids
#'   pbdb_scales()
#' }
pbdb_scales <- function(...) {
  l <- list(...)
  .pbdb_query("scales/list", query = l)
}

#' Get information about geological strata
#'
#' Returns information about geological strata, selected by name,
#' rank, and/or geographic location.
#'
#' @param ... Arguments passed to the API. See documentation for
#'   accepted parameters at
#'   <https://paleobiodb.org/data1.2/strata/list>. E.g.:
#'
#' * `name`: A full or partial name. You can use % and _ as wildcards.
#'
#' * `rank`: Returns only strata of the specified rank: `"formation"`,
#'   `"group"` or `"member"`.
#'
#' * `lngmin`: Numeric. The longitude boundaries will be normalized to
#'   fall between -180 and 180. Note that if you specify `lngmin` then
#'   you must also specify `lngmax`. Returns only records whose
#'   geographic location falls within the given bounding box (defined
#'   by `lngmin`, `lngmax`, `latmin`, `latmax`). It generates two
#'   adjacent bounding boxes if the range crosses the antimeridian.
#'
#' * `lngmax`: Numeric. The longitude boundaries will be normalized to
#'   fall between -180 and 180.
#'
#' * `latmin`: Numeric between -90 and 90. Note that if you specify
#'   `latmin` then you must also specify `latmax`.
#'
#' * `latmax`: Numeric between -90 and 90.
#'
#' * `loc`: Return only strata associated with some occurrence whose
#'   geographic location falls within the specified geometry,
#'   specified in WKT format.
#'
#' * `vocab`: Set to `"pbdb"` to show the complete name of the
#'   variables (by default variables have short 3-letter names).
#'
#' @returns A data frame with information from the selected strata.
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

#' Get a list of strata matching a given prefix or partial name
#'
#' Returns a list of strata matching the given prefix or partial name.
#' This can be used to implement auto-completion for strata names, and
#' can be limited by geographic location if desired.
#'
#' @param ... Arguments passed to the API. See documentation for
#'   accepted parameters at
#'   <https://paleobiodb.org/data1.2/strata/auto>. E.g.:
#'
#' * `name`: A full or partial name. It must have at least 3
#'   significant characters, and may end in a space followed by either
#'   'g' or 'f' to indicate that you are looking for a group or
#'   formation.
#'
#' * `rank`: Return only strata of the specified rank: `"formation"`
#'   or `"group"`. This may be overridden by a suffix on the value of
#'   `name`.
#'
#' * `lngmin`: Numeric. The longitude boundaries will be normalized to
#'   fall between -180 and 180. Note that if you specify `lngmin` then
#'   you must also specify `lngmax`. Returns only records whose
#'   geographic location falls within the given bounding box (defined
#'   by `lngmin`, `lngmax`, `latmin`, `latmax`). It generates two
#'   adjacent bounding boxes if the range crosses the antimeridian.
#'
#' * `lngmax`: Numeric. The longitude boundaries will be normalized to
#'   fall between -180 and 180.
#'
#' * `latmin`: Numeric between -90 and 90. Note that if you specify
#'   `latmin` then you must also specify `latmax`.
#'
#' * `latmax`: Numeric between -90 and 90.
#'
#' * `vocab`: Set to `"pbdb"` to show the complete name of the
#'   variables (by default variables have short 3-letter names).
#'
#' @returns A data frame with information from the strata that match
#'   the `name` parameter.
#'
#' @export
#' @examples \dontrun{
#'   pbdb_strata_auto(name = "Pin", vocab = "pbdb")
#' }
pbdb_strata_auto <- function(...) {
  l <- list(...)
  .pbdb_query("strata/auto", query = l)
}

#' Get information about a single reference
#'
#' Returns information about a single reference, selected by
#' identifier.
#'
#' @param id Identifier of the reference. This parameter is required.
#' @param ... Arguments passed to the API. See documentation for
#'   accepted parameters at
#'   <https://paleobiodb.org/data1.2/refs/single>. E.g.:
#'
#' * `vocab`: Set to `"pbdb"` to show the complete name of the
#'   variables (by default variables have short 3-letter names).
#'
#' * `show`: Additional information to be shown along with the basic
#'   record.  Some possible values include:
#'
#'     * `counts`: Report the number of taxonomic names, opinions,
#'       occurrences, specimens, and collections derived from this
#'       reference that have been entered into the database.
#'
#'     * `both`: Show both the formatted reference and the individual
#'        fields.
#'
#' @returns A data frame with a single reference.
#' @export
#' @examples \dontrun{
#'   pbdb_reference(id = 1003, vocab = "pbdb", show = "both")
#' }
pbdb_reference <- function(id, ...) {
  l <- list(id = id, ...)
  .pbdb_query("refs/single", query = l)
}

#' Get information about multiple references
#'
#' Returns information about multiple references, selected according
#' to the parameters you provide.
#'
#' @param ... Arguments passed to the API. See documentation for
#'   accepted parameters at
#'   <https://paleobiodb.org/data1.2/refs/list>. E.g.:
#'
#' * `ref_author`: Select only references for which any of the authors
#'   matches the specified name.
#'
#' * `ref_pubyr`: Select only references published in the specified
#'   year.
#'
#' * `pub_title`: Select only references that involve the specified
#'   publication.
#'
#' * `order`: Specifies the order in which the results are
#'   returned. You can specify multiple values separated by commas,
#'   and each value may be appended with .asc or .desc. Accepted
#'   values are: `"author"`, `"pubyr"`, `"reftitle"`, `"pubtitle"`,
#'   `"pubtype"`, `"created"`, `"modified"`, `"rank"`.
#'
#' @returns A data frame with the information about the references that
#'   match the query.
#'
#' @export
#' @examples \dontrun{
#'   pbdb_references(ref_author = "Polly")
#' }
pbdb_references <- function(...) {
  l <- list(...)
  .pbdb_query("refs/list", query = l)
}

#' Get references from which collection data were entered
#'
#' Returns information about the references from which the selected
#' collection data were entered.
#'
#' @param ... Arguments passed to the API. See documentation for
#'   accepted parameters at
#'   <https://paleobiodb.org/data1.2/colls/refs>. E.g.:
#'
#' * `id`: List of collection identifiers.
#'
#' * `ref_author`: Select only references for which any of the authors
#'   matches the specified name.
#'
#' * `ref_pubyr`: Select only references published in the specified
#'   year.
#'
#' * `pub_title`: Select only references that involve the specified
#'   publication.
#'
#' * `order`: Specifies the order in which the results are
#'   returned. You can specify multiple values separated by commas,
#'   and each value may be appended with .asc or .desc. Accepted
#'   values are: `"author"`, `"pubyr"`, `"reftitle"`, `"pubtitle"`,
#'   `"pubtype"`, `"created"`, `"modified"`, `"rank"`.
#'
#' @returns A data frame with the information about the references that
#'   match the query.
#'
#' @export
#' @examples \dontrun{
#'   pbdb_ref_collections(
#'     base_name = "Canidae",
#'     interval = "Quaternary",
#'     cc = "ASI"
#'   )
#' }
pbdb_ref_collections <- function(...) {
  l <- list(...)
  .pbdb_query("colls/refs", query = l)
}

#' Get references for taxonomic names
#'
#' Returns information about the source references associated with
#' taxa in the Paleobiology Database. You can use the same parameters
#' that are available with `pbdb_taxa`, but reference records are
#' returned instead of taxon records. One record is returned per
#' reference, even if it is associated with multiple taxa.
#'
#' @param ... Arguments passed to the API. See all available arguments at
#'   <https://paleobiodb.org/data1.2/taxa/refs>
#'
#' * `name`: Returns information about the most fundamental taxonomic
#'   name matching this string.  The % and _ characters may be used as
#'   wildcards.
#'
#' * `id`: Returns information about the taxonomic name corresponding
#'   to this identifier. You may not specify both `name` and `id` in
#'   the same query.
#'
#' * `show`: Show extra variables.
#'
#' * `rel`: Set `rel = "synonyms"` to select all synonyms of the base
#'   taxon or taxa; `rel = "children"` to select the taxa immediately
#'   contained within the base taxon or taxa; `rel = "all_children"`
#'   to select all taxa contained within each matching taxon and
#'   within all synonymous taxa; `rel = "all_parents"` to select all
#'   taxa that contain any of the matching taxa.
#'
#' * `extant`: Logical indicating whether to select only extant or
#'   non-extant taxa.
#'
#' @returns A data frame with references from a list of taxa.
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

#' Get information about a single fossil specimen
#'
#' Returns information about a single fossil specimen, identified
#' either by name or by identifier.
#'
#' @param id The identifier of the specimen.  This parameter is
#'   required.
#' @param ... Arguments passed to the API.  See all available
#'   arguments at <https://paleobiodb.org/data1.2/specs/single>.
#'
#' * `vocab`: Set to "pbdb" to show the complete name of the variables
#'   (by default variables have short 3-letter names).
#'
#' * `show`: Select additional blocks of information to be returned
#'   along with the basic record.  Some possible values include:
#'
#'     * `"loc"`: Additional information about the geographic locality
#'       of the associated occurrence, if any.
#'
#'     * `"stratext"`: Detailed information about the stratigraphic
#'       context of the associated occurrence.
#'
#'     * `"lithext"`: Detailed information about the lithological
#'       context of the associated occurrence.
#'
#'     * `"refattr"`: The author(s) and year of publication of the
#'       reference from which this data was entered. If no reference
#'       is recorded for this specimen, the information from the
#'       associated occurrence or collection reference is returned
#'       instead.
#'
#' @returns A data frame with information about a single specimen.
#'
#' @export
#' @examples \dontrun{
#' pbdb_specimen(id = 30050, show = c("class", "loc", "refattr"))
#' }
pbdb_specimen <- function(id, ...) {
  l <- list(id = id, ...)
  .pbdb_query("specs/single", query = l)
}

#' Get information about multiple fossil specimens
#'
#' Returns information about multiple fossil specimens, selected
#' according to the parameters you provide.  Depending upon which
#' output blocks you select (`show` parameter), the response will
#' contain some fields describing the specimens and some describing
#' the occurrences and collections (if any) with which they are
#' associated.
#'
#' @param ... Arguments passed to the API.  See all available
#'   arguments at <https://paleobiodb.org/data1.2/specs/list>. See the
#'   [pbdb_occurrences()] documentation for an explanation about the
#'   main filtering parameters.
#' @returns A data frame with the fossil specimens that match the
#'   query.
#'
#' @export
#' @examples \dontrun{
#'   pbdb_specimens(base_name = "Cetacea", interval = "Miocene", vocab = "pbdb")
#' }
pbdb_specimens <- function(...) {
  l <- list(...)
  .pbdb_query("specs/list", query = l)
}

#' Get references for fossil specimens
#'
#' Returns information about the bibliographic references associated
#' with the selected fossil specimens.
#'
#' @param ... Arguments passed to the API. See documentation for
#'   accepted parameters at
#'   <https://paleobiodb.org/data1.2/specs/refs>. E.g.:
#'
#' * `spec_id`: List of specimen identifiers.
#'
#' * `base_name`: Return only records associated with the specified
#'   taxonomic name(s), including all subtaxa and synonyms.
#'
#' * `ref_author`: Select only references for which any of the authors
#'   matches the specified name.
#'
#' * `ref_pubyr`: Select only references published in the specified
#'   year.
#'
#' * `pub_title`: Select only references that involve the specified
#'   publication.
#'
#' @returns A data frame with the information about the references that
#'   match the query.
#'
#' @export
#' @examples \dontrun{
#' pbdb_ref_specimens(spec_id = c(1505, 30050))
#' }
pbdb_ref_specimens <- function(...) {
  l <- list(...)
  .pbdb_query("specs/refs", query = l)
}

#' Get information about specimen measurements
#'
#' Returns information about the measurements associated with the
#' selected fossil specimens.
#'
#' @param ... Arguments passed to the API.  See all available
#'   arguments at <https://paleobiodb.org/data1.2/specs/measurements>.
#'
#' The following parameters can be used to retrieve measurements from
#' a known list of specimens, occurrences, or collections. Only the
#' records matching all specified parameters will be returned:
#'
#' * `spec_id`: A list of specimen identifiers.
#'
#' * `occ_id`: A list of occurrence identifiers.
#'
#' * `coll_id`: A list of collection identifiers.
#'
#' It is possible to return additional information along with the
#' basic record with the following parameter:
#'
#' * `show`: Possible values include:
#'
#'     * `"spec"`: Includes all of the core fields describing the
#'       specimen from which this measurement was taken.
#'
#'     * `"methods"`: Information about the collection methods used.
#'
#' See the [pbdb_occurrences()] documentation for an explanation about
#' more filtering parameters.
#'
#' @returns A data frame with information about the measurements that
#'   match the query.
#'
#' @export
#' @examples \dontrun{
#'   pbdb_measurements(
#'     spec_id = c(1505, 30050),
#'     show = c("spec", "class", "methods"),
#'     vocab = "pbdb"
#'   )
#' }
pbdb_measurements <- function(...) {
  l <- list(...)
  .pbdb_query("specs/measurements", query = l)
}

#' Get information about a single taxonomic opinion
#'
#' Returns information about a single taxonomic opinion, selected by
#' identifier.
#'
#' @param id Identifier of the opinion. This parameter is required.
#' @param ... Arguments passed to the API. See documentation for
#'   accepted parameters at
#'   <https://paleobiodb.org/data1.2/opinions/single>. E.g.:
#'
#' * `vocab`: Set to `"pbdb"` to show the complete name of the
#'   variables (by default variables have short 3-letter names).
#'
#' * `show`: Additional information to be shown along with the basic
#'   record.  Some possible values include:
#'
#'     * `basis`: The basis of the opinion, which can be
#'       "stated with evidence", "stated without evidence", "implied",
#'       or "second hand".
#'
#'     * `entname`: The names of the people who authorized, entered
#'       and modified this record.
#'
#'     * `refattr`: The author(s) and year of publication of the
#'       reference from which the opinion was entered.
#'
#' @returns A data frame with a single taxonomic opinion.
#' @export
#' @examples \dontrun{
#'   pbdb_opinion(id = 1000, vocab = "pbdb", show = "full")
#' }
pbdb_opinion <- function(id, ...) {
  l <- list(id = id, ...)
  .pbdb_query("opinions/single", query = l)
}

#' Get information about multiple taxonomic opinions
#'
#' Returns information about multiple taxonomic opinions, selected
#' according to criteria other than taxon name. This function could be
#' used to query for all of the opinions attributed to a particular
#' author, or to show all of the recently entered opinions.
#'
#' @param ... Arguments passed to the API. See documentation for
#'   accepted parameters at
#'   <https://paleobiodb.org/data1.2/opinions/list>. E.g.:
#'
#' * `id`: Selects the opinions corresponding to the specified
#'   identifier(s). You may provide more than one.
#'
#' * `op_author`: Selects only opinions attributed to the specified
#'   author. Note that the opinion author(s) may be different from the
#'   author(s) of the reference from which the opinion was
#'   entered. This parameter accepts last names only, no first
#'   initials. You can specify more than one author name, in which
#'   case all opinions which match any of these will be selected.
#'
#' * `ops_created_before`: Select only records associated with taxa
#'   that were created before the specified date or date/time. See
#'   <https://paleobiodb.org/data1.2/datetime> for documentation on
#'   how to specify date/time values.
#'
#' * `ops_created_after`: Select only records associated with taxa
#'   that were created on or after the specified date or
#'   date/time. See <https://paleobiodb.org/data1.2/datetime> for
#'   documentation on how to specify date/time values.
#'
#' * `op_type`: You can use this parameter to retrieve all opinions,
#'   or only the classification opinions, or only certain kinds of
#'   opinions. The default is all opinions. Accepted values include:
#'   `"all"` (the default), `"class"`, `"valid"`, `"accepted"`,
#'   `"junior"`, `"invalid"`.
#'
#' @returns A data frame with information about the taxonomic opinions
#'   that match the query.
#' @export
#' @examples \dontrun{
#'   pbdb_opinions(op_pubyr = 1818)
#' }
pbdb_opinions <- function(...) {
  l <- list(...)
  .pbdb_query("opinions/list", query = l)
}

#' Get taxonomic opinions about taxa
#'
#' Returns information about the taxonomic opinions used to build the
#' taxonomic hierarchy. From all of the opinions entered into the
#' database about a particular taxon, the most recent opinion that is
#' stated with the most evidence is used to classify that taxon. The
#' others are considered to be superseded and are ignored.
#'
#' @param ... Arguments passed to the API. See documentation for
#'   accepted parameters at
#'   <https://paleobiodb.org/data1.2/taxa/opinions>.
#'
#' @returns A data frame with information about the taxonomic opinions
#'   that match the query.
#' @export
#' @examples \dontrun{
#'   pbdb_opinions_taxa(base_name = "Canis")
#' }
pbdb_opinions_taxa <- function(...) {
  l <- list(...)
  .pbdb_query("taxa/opinions", query = l)
}
