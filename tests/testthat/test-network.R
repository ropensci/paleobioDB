test_that(".build_uri() requires an endpoint", {
  expect_error(.build_uri(query = c(a = 1, b = 2)))
})

test_that(".build_uri() returns a URL conforming to the proper scheme", {
  expect_equal(
    .build_uri(
      endpoint = "some/endpoint",
      query = c(a = 1, b = "x,y"),
      format = "csv"
    ),
    "https://paleobiodb.org/data1.2/some/endpoint.csv?a=1&b=x,y"
  )
})

test_that(".extract_response_body() errors when status code != 200", {
  resp <- readRDS(test_path("fixtures", "invalid_id_resp.rds"))
  expect_error(.extract_response_body(resp), regexp = "Error in API response")
})

test_that(".extract_response_body() returns body when status code == 400", {
  resp <- readRDS(test_path("fixtures", "valid_id_resp.rds"))
  body <- paste0(
    "{\n\"elapsed_time\":0.00185,\n\"records\": [\n{\"oid\":\"occ:1\",\"cid\":",
    "\"col:1\",\"tna\":\"Australosutura llanoensis\",\"rnk\":3,\"tid\":\"txn:",
    "349412\",\"oei\":\"Ivorian\",\"eag\":353.8,\"lag\":345.3,\"rid\":\"",
    "ref:1\",\"lng\":\"-98.980003\",\"lat\":\"31.170000\"}\n]\n}\n"
  )
  expect_equal(.extract_response_body(resp), body)
})

test_that(".parse_raw_data() replicates warnings from the API", {
  resp <- readRDS(test_path("fixtures", "warn_resp.rds"))
  raw_data <- .extract_response_body(resp)
  expect_warning(
    .parse_raw_data(raw_data),
    regexp = "Your query to the PBDB API generated the following warnings:"
  )
})

test_that(".make_data_frame() warns the user if no records are returned", {
  expect_warning(
    .make_data_frame(list()),
    regexp = "The PBDB API returned no records for this query."
  )
})

test_that(".make_data_frame() returns a data frame", {
  resp <- readRDS(test_path("fixtures", "valid_id_resp.rds"))
  raw_data <- .extract_response_body(resp)
  data_list <- rjson::fromJSON(raw_data)
  df <- .make_data_frame(data_list$records)
  expect_s3_class(df, "data.frame")
  expect_identical(
    names(df),
    c(
      "oid",
      "cid",
      "tna",
      "rnk",
      "tid",
      "oei",
      "eag",
      "lag",
      "rid",
      "lng",
      "lat"
    )
  )
})

test_that(".collapse_array_columns_map() returns a length one character", {
  s <- .collapse_array_columns_map(1:3)
  expect_length(s, 1)
  expect_equal(s, "1;2;3")
})
