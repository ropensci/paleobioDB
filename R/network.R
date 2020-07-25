#' .get_data_from_uri
#' 
#' Grabs data as dataframe from a URI. Expects the response data to be in JSON format
#'
#' @param uri Public internet address of the data
#' @return dataframe with rows of data
#' @param dataframe_factory function used to transform the rows of data into dataframe rows
#' @examples \dontrun{
#' .get_data_from_uri("http://api.example.com/ecologydata")
#' }

.get_data_from_uri<-function(uri, dataframe_factory = as.data.frame){
	response <- RCurl::getURL(uri, header = TRUE)
	raw_data <- .extract_response_body(response)
	df <- .parse_raw_data(raw_data, dataframe_factory)
	df
}


#' .extract_response_body
#'
#' Extracts de body from a HTTP response or throws error if not 200 status
#
#' @param response Raw http response
#' @return character
#'

.extract_response_body<-function(response){
	
	sp<-strsplit(response, '\r\n\r\n', fixed=TRUE)[[1]]
	header<-sp[[1]]
	status <- substring(header, 10, 12)
  
	body<-sp[[2]]

	if(status != '200'){
		stop(sprintf('Error in API response. The server returned a status %s, which indicates that 
			something went wrong with your request. In order to debug the problem you may find
			usefull information in the following server response:\r\n%s', status, body))
	}

	body
}


#' .parse_raw_data
#'
#' Parse raw json string as a dataframe
#'
#' @param raw_data json encoded data
#' @param dataframe_factory function used to transform the rows of data into dataframe rows
#' @return dataframe
#'

.parse_raw_data <- function(raw_data, dataframe_factory = as.data.frame){
    data_list <- fromJSON(raw_data)
    df <- .make_data_frame(data_list[["records"]], dataframe_factory)
    df
}


#' .make_data_frame
#' 
#' Makes a dataframe from a list of lists
#'
#' @param reg_list data rows as a list of lists
#' @param dataframe_factory function used to transform the rows of data into dataframe rows
#' @return dataframe
#'

.make_data_frame<-function(reg_list, dataframe_factory){
  
  reg_count <- length(reg_list)
  first_line <- TRUE
  
  for (reg in reg_list) {

    reg <- lapply(reg, .collapse_array_columns_map)

    if (first_line) {
      df<- dataframe_factory(reg)
    } else {
      df<- smartbind(df, dataframe_factory(reg))
    }
    
    first_line <- FALSE
  }
  
  df_count <- nrow(df)
  
  if (reg_count != df_count) {
    stop(sprintf('The length of the response dataframe (%d rows) is different than the original ammount of 
      results returned by the paleobioDB API (%d rows).\r\n
      Please report this bug to the maintainer of rpbdb package', df_count, reg_count))
  }
  
  df
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
#' 

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
#' Maps multivalue elements to comma separated strings 
#'
#' @param reg_list data rows as a list of lists
#' @return dataframe
#'

.collapse_array_columns_map<- function (element){  
  if (length (element) > 1){
    mapped<- paste (element, collapse=";")
  }else {
    mapped<- element
  }
  
  mapped
}


