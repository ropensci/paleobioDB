#' .get_data_from_uri
#' 
#' Grabs data as dataframe from a URI. Expects the response data to be in JSON format
#'
#' @param uri Public internet address of the data
#' @return dataframe with rows of data
#' @examples \dontrun{
#' .get_data_from_uri("http://api.example.com/ecologydata")
#' }

.get_data_from_uri<-function(uri){

	response <- RCurl::getURL(uri, header = TRUE)
	raw_data <- .extract_response_body(response)

	df<-.parse_raw_data(raw_data)

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
	
	sp<-strsplit(response, '\r\n\r\n')[[1]]
	header<-sp[[1]]
	status <- substring(header, 10, 12)
  
	body<-sp[[2]]

	if(status != '200'){
		stop(sprintf('Error in API response. The server returned a status %s, which indicates that 
			something went wrong with your request. \r\nIn order to debug the problem you may find
			usefull information in the following server response:\r\n%s', status, body))
	}

	body
}

#' .parse_raw_data
#' 
#' Parse raw json string as a dataframe
#' 
#' @param raw_data json encoded data
#' @return dataframe
#' 

.parse_raw_data<-function(raw_data){

	data_list <- fromJSON(raw_data)
	df <- .make_data_frame(data_list[[1]])

	df	
}

#' .make_data_frame
#' 
#' Makes a dataframe from a list of lists
#'
#' @param reg_list data rows as a list of lists
#' @return dataframe
#'

.make_data_frame<-function(reg_list){

	df<-as.data.frame(reg_list[1])

	len <- length(reg_list)

	if(len < 2){
		return (df)
	}

	# TODO: fix warning thrown when different types of data found for a column. smartbind
	# converts to character and throws warning. see: pbdb_ref_taxa (name="Canidae")
	# fix: - convert certain columns to character on the fly

	for (reg in reg_list[2:len]){
	    # smartbind from gtools let us bind rows with different
	    # names to the dataframe
	    df<-smartbind(df, reg)
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
	  argValue<-  URLencode(args[argName][[1]])
		qs <- paste(qs, argName, "=", argValue, '&', sep = "")
	}
	qs <- substr(qs,0,nchar(qs)-1)

	qs
}