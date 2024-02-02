#' .get_data_from_uri
#'
#' Grabs data as dataframe from a URI. Expects the response data to be
#' in JSON format
#'
#' @param uri Public internet address of the data
#' @return dataframe with rows of data
#' @examples \dontrun{
#' .get_data_from_uri("http://api.example.com/ecologydata")
#' }
#' @noRd

.get_data_from_uri<-function(uri){
	response <- RCurl::getURL(uri, header = TRUE)
	raw_data <- .extract_response_body(response)
	df<-.parse_raw_data(raw_data)  
  df
}


#' .extract_response_body
#'
#' Extracts the body from a HTTP response or throws error if not 200 status
#
#' @param response Raw http response
#' @return character
#' @noRd
.extract_response_body <- function(response) {
  sp <- strsplit(response, "\r\n\r\n", fixed = TRUE)[[1]]
  header <- sp[[1]]
  status <- substring(header, 10, 12)

  body <- sp[[2]]

  if (status != "200") {
    stop(sprintf("Error in API response. The server returned a status %s, which indicates that
something went wrong with your request. In order to debug the problem you may find
useful information in the following server response:\r\n%s", status, body))
  }

  body
}

#' .parse_raw_data
#'
#' Parse raw json string as a dataframe
#'
#' @param raw_data json encoded data
#' @return dataframe
#' @noRd
.parse_raw_data <- function(raw_data) {
  data_list <- fromJSON(raw_data)

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

  df <- .make_data_frame(data_list$records)
  df
}


#' .make_data_frame
#'
#' Makes a dataframe from a list of lists
#'
#' @param reg_list data rows as a list of lists
#' @return dataframe
#' @noRd
.make_data_frame <- function(reg_list) {
  if (length(reg_list) == 0) {
    warning("The PBDB API returned no records for this query.", call. = FALSE)
    return(data.frame())
  }

  dfr_rows <- lapply(reg_list, function(reg) {
    as.data.frame(lapply(reg, .collapse_array_columns_map))
  })

  dfr <- do.call(gtools::smartbind, dfr_rows)
  dfr <- .convert_data_frame_columns(dfr)
  dfr
}

#' .build_query_string
#' 
#' Builds a query string ready for been added to a url, from a list of name/value parameters
#'
#' @usage .build_query_string(args)
#'
#' @param args list of parameters
#' @return character
#' @examples \dontrun{
#' .build_query_string(list(name="Bob", city="Berlin"))
#' }
#' @noRd

.build_query_string<-function(args){
  
	qs <- ''

	for (argName in names(args)) {
    strArgValue <- as.character(args[argName][[1]])
	  encodedArgValue<-  URLencode(strArgValue)
		qs <- paste(qs, argName, "=", encodedArgValue, '&', sep = "")
	}
	qs <- substr(qs,0,nchar(qs)-1)

	qs
}


#' .collapse_array_columns_map
#'
#' Maps multivalue elements to semicolon separated strings
#'
#' @param element a vector representing some data field
#' @return a string with the elements of the provided vector separated
#'   by semicolons if it has more than one element or the vector as it
#'   was passed to the function if it has length one
#' @noRd

.collapse_array_columns_map<- function (element){  
  if (length (element) > 1){
    mapped<- paste (element, collapse=";")
  }else {
    mapped<- element
  }
  
  mapped
}


#' .convert_data_frame_columns
#'
#' Converts some columns (lng, lat) from character to numeric.  This
#' conversion is needed because the paleobiodb API returns these
#' fields as strings, but the map plotting functions in this package
#' expect longitude and latitude to be numeric.
#'
#' @param df a data frame
#' @return a data frame with its "lng" and "lat" columns converted to
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
