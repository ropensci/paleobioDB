#' Grabs data as dataframe from a URI in the data_format format
#' 
.get_data_from_uri<-function(uri, data_format = 'json'){

	# RCurl: parseHTTPHeader...
	
	response <- getURL(uri, header = TRUE)
	raw_data <- .extract_response_body(response)

	df <- .parse_raw_data(raw_data, data_format = data_format)

	df
}

#' Extracts de body from a HTTP response.
#
#' @param error_on_not_200 if set to TRUE, will throw error if finds reponse
#' status differente to 200
#' 
.extract_response_body<-function(response, error_on_not_200 = TRUE){
	
	sp<-strsplit(response, '\r\n\r\n')[[1]]
	header<-sp[[1]]
	status <- substring(header, 10, 12)
  
	body<-sp[[2]]

	if(error_on_not_200 && (status != '200')){
		stop(sprintf('Error in API response. The server returned a status %s, which indicates that 
			something went wrong with your request. \r\nIn order to debug the problem you may find
			usefull information in the following server response:\r\n%s', status, body))
	}

	body
}


#' Parse raw data according to configured api call format
#'
#' @param data
#' @return data.frame
#' @export 
#'
.parse_raw_data<-function(data, data_format = 'json'){
	# package should allow get different formats ?

	if(data_format != 'json'){
		stop(paste('Format not implemented: ', .data_format))
	}

	# json
	# pack rjson
	data_list <- fromJSON(data)

	df <- .make_data_frame(data_list[[1]])

	df
}


#' Makes a dataframe from a list of lists (fromJSON response)

.make_data_frame<-function(reg_list){

	df<-as.data.frame(reg_list[1])

	len <- length(reg_list)

	if(len < 2){
		return (df)
	}

	for (reg in reg_list[2:len]){
	    # smartbind from gtools let us bind rows with different
	    # names to the dataframe
	    df<-smartbind(df, reg)
	}

	df
}