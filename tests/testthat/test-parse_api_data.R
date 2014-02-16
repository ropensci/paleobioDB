context("network: HTTP response handling; Parsing response data to R workable variables")

# HTTP

bad_http_response <- 'HTTP/1.1 400 OK\r\nContent-Type: application/json; charset=utf-8\r\nVary: Accept-Encoding,User-Agent\r\n\r\nTHE BODY'
good_http_response <- 'HTTP/1.1 200 OK\r\nContent-Type: application/json; charset=utf-8\r\nVary: Accept-Encoding,User-Agent\r\n\r\nTHE BODY'

# network tests
test_that("Error on non 200 status response", {
	expect_error(.extract_response_body(bad_http_response))
})

test_that("No error on HTTP error if error flag is false", {
	expect_equal(.extract_response_body(bad_http_response, FALSE), 'THE BODY')
})

test_that("Body extraction on successful request", {
	expect_equal(.extract_response_body(good_http_response), 'THE BODY')
})


# PARSING

# response to 
# "http://paleobiodb.org/data1.1/occs/list.json?base_name=Alopex&interval=Quaternary&show=loc,time"
# 1/2/2014
# 7 records
# record 199941 includes extra column cny
api_json_response_sample <- "{\"records\": [{\"oid\":198254,\"typ\":\"occ\",\"cid\":20434,\"tna\":\"Alopex lagopus\",\"rnk\":3,\"tid\":43911,\"eag\":1.80000,\"lag\":0.30000,\"rid\":[2048],\"cc2\":\"Canada\",\"sta\":\"Yukon\",\"cxi\":33,\"ein\":740,\"lin\":923},{\"oid\":198265,\"typ\":\"occ\",\"cid\":20436,\"tna\":\"Alopex lagopus\",\"rnk\":3,\"tid\":43911,\"eag\":1.80000,\"lag\":0.30000,\"rid\":[2048],\"cc2\":\"Canada\",\"sta\":\"Yukon\",\"cxi\":33,\"ein\":740,\"lin\":923},{\"oid\":199751,\"typ\":\"occ\",\"cid\":20626,\"tna\":\"Alopex lagopus\",\"rnk\":3,\"tid\":43911,\"eag\":0.12600,\"lag\":0.01170,\"rid\":[1784],\"cc2\":\"Canada\",\"sta\":\"Yukon\",\"cxi\":922,\"ein\":922,\"lin\":922},{\"oid\":199941,\"typ\":\"occ\",\"cid\":20643,\"tna\":\"Alopex lagopus\",\"rnk\":3,\"tid\":43911,\"eag\":0.12600,\"lag\":0.01170,\"rid\":[2872],\"cc2\":\"United States\",\"sta\":\"Alaska\",\"cny\":\"North Slope\",\"cxi\":922,\"ein\":922,\"lin\":922},{\"oid\":373590,\"typ\":\"occ\",\"cid\":35359,\"tna\":\"Alopex\",\"rnk\":5,\"tid\":41193,\"eag\":5.33300,\"lag\":0.01170,\"rid\":[9534],\"cc2\":\"Hungary\",\"sta\":\"Barany\",\"cxi\":1,\"ein\":34,\"lin\":33},{\"oid\":485281,\"typ\":\"occ\",\"cid\":48587,\"tna\":\"Alopex lagopus\",\"rnk\":3,\"tid\":43911,\"eag\":0.12600,\"lag\":0.01170,\"rid\":[12964],\"cc2\":\"Russian Federation\",\"sta\":\"Respublika Saha (Jakutija)\",\"cxi\":922,\"ein\":922,\"lin\":922},{\"oid\":766675,\"typ\":\"occ\",\"cid\":81830,\"tna\":\"Alopex lagopus\",\"rnk\":3,\"tid\":43911,\"eag\":0.12600,\"lag\":0.01170,\"rid\":[27585],\"cc2\":\"United States\",\"sta\":\"Alaska\",\"cxi\":922,\"ein\":922,\"lin\":922}]}"

api_json_response_sample_returns <- '{\n"records": [\n{"oid":1001,"typ":"occ","cid":160,"tna":"Wellerella","rnk":5,"tid":29018,"oei":"Missourian","eag":305.90000,"lag":303.40000,"rid":[12]}\n]\n}\n'

test_that(".parse_raw_data for a JSON string response is a data.frame", { 

	parsed <- .parse_raw_data(api_json_response_sample)
	expect_is(parsed, 'data.frame')
})

test_that(".parse_raw_data for a JSON string that includes \n yields just one row in the dataframe", { 

	parsed <- .parse_raw_data(api_json_response_sample_returns)
	expect_equal(dim(parsed)[1], 1)
})

test_that("A JSON string with different columns for some occurency resolves to a data.frame comprising all the columns", { 

	expected_names <- c("oid", "typ", "cid", "tna", "rnk", "tid", "eag", "lag", "rid", "cc2", "sta", "cxi", "ein", "lin", "cny")
	
	resp_names <- names(.parse_raw_data(api_json_response_sample))

	expect_identical(resp_names, expected_names)
})

