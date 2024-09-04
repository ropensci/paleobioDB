#' Generate a URI conforming to the scheme documented in the
#' paleobioDB API
#'
#' @param endpoint Name of an endpoint.
#' @param query URI parameters for the query string passed as a named
#'   list.
#' @param api_base The server to be contacted.
#' @param format Format in which results will be returned.  For now,
#'   json is the only supported format.
#' @param data_serv The data service version.  Could be either
#'   "data1.1" or "data1.2"
#' @returns A URL to perform an HTTP GET request.
#' @noRd
.build_uri <- function(endpoint,
                       query = list(),
                       api_base = "https://paleobiodb.org",
                       format = "json",
                       data_serv = "data1.2") {
  uri <- paste(api_base, data_serv, endpoint, sep = "/")
  uri <- paste(uri, format, sep = ".")
  query_str <- .build_query_string(query)
  uri <- paste(uri, query_str, sep = "?")
  uri
}

#' .get_data_from_uri
#'
#' Grabs data as data frame from a URI. Expects the response data to be
#' in JSON format
#'
#' @param uri Public internet address of the data
#' @returns data frame with rows of data
#' @examples \dontrun{
#' .get_data_from_uri("http://api.example.com/ecologydata")
#' }
#' @noRd
.get_data_from_uri <- function(uri) {
  response <- curl::curl_fetch_memory(uri)
  raw_data <- .extract_response_body(response)
  df <- .parse_raw_data(raw_data)
  df
}

#' .extract_response_body
#'
#' Extracts the body from a HTTP response or throws error if not 200 status
#
#' @param response Raw http response
#' @returns character
#' @noRd
.extract_response_body <- function(response) {
  body <- rawToChar(response$content)

  if (response$status_code != 200) {
    err_msg <- strwrap(
      paste0(
        "Error in API response. The server returned a status ",
        response$status_code,
        ", which indicates that something went wrong with your request. ",
        "In order to debug the problem you may find useful information ",
        "in the following server response:\n\n",
        body
      )
    )
    stop(paste(err_msg, collapse = "\n"))
  }

  body
}

#' .parse_raw_data
#'
#' Parse raw json string as a data frame
#'
#' @param raw_data json encoded data
#' @returns data frame
#' @noRd
.parse_raw_data <- function(raw_data) {
  data_list <- jsonlite::fromJSON(raw_data)

  if ("warnings" %in% names(data_list)) {
    # Enumerate warnings that were returned by the PBDB API
    warn_list <- paste(
      paste(paste0(seq_along(data_list$warnings), "."), data_list$warnings),
      collapse = "\n"
    )
    warning(
      paste0(
        "Your query to the PBDB API generated the following warnings:\n",
        warn_list
      ),
      call. = FALSE
    )
  }

  if (length(data_list$records) == 0) {
    warning("The PBDB API returned no records for this query.", call. = FALSE)
    return(data.frame())
  }

  if (!is.data.frame(data_list$records)) {
    stop("The records in the response were not coerced to a data.frame")
  }

  .convert_data_frame_columns(data_list$records)
}

#' .build_query_string
#'
#' Builds a query string ready for been added to a url, from a list of
#' name/value parameters
#'
#' @param args list of parameters
#' @returns character
#' @examples \dontrun{
#'   .build_query_string(list(name = "Bob", city = "Berlin"))
#' }
#' @noRd
.build_query_string <- function(args) {
  qs <- ""

  for (arg_name in names(args)) {
    str_arg_value <- as.character(args[arg_name][[1]])
    encoded_arg_value <- utils::URLencode(str_arg_value)
    qs <- paste0(qs, arg_name, "=", encoded_arg_value, "&")
  }
  qs <- substr(qs, 0, nchar(qs) - 1)

  qs
}

#' .convert_data_frame_columns
#'
#' Converts some columns (lng, lat) from character to numeric.  This
#' conversion is needed because the paleobiodb API returns these
#' fields as strings, but the map plotting functions in this package
#' expect longitude and latitude to be numeric.
#'
#' @param df a data frame
#' @returns a data frame with its "lng" and "lat" columns converted to
#'   numeric
#'
#' @noRd
.convert_data_frame_columns <- function(df) {
  column_conversion_funs <- c("lng" = as.numeric, "lat" = as.numeric)

  columns_to_convert <- colnames(df)[
    colnames(df) %in% names(column_conversion_funs)
  ]

  for (column in columns_to_convert) {
    df[[column]] <- column_conversion_funs[[column]](df[[column]])
  }

  df
}
