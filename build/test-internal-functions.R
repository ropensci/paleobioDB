source('source-all.R')

library(rjson)
library(gtools)
library(testthat)

test_dir('../tests/testthat/internal/', stop_on_failure = TRUE)
