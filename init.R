# this file is a provisional shorcut intended for early development stages
# will be removed when adopting a proper R package structure


library(rjson)
library(gtools)
library(RCurl)
library (testthat)
library(plyr)
library(maps)
library(scales)
library(raster)


source.with.encoding('R/network.R', encoding='UTF-8')
source.with.encoding('R/rest_api_tools.R', encoding='UTF-8')
source.with.encoding('R/pbdb_cache.R', encoding='UTF-8')
source.with.encoding('R/pbdb_rest_api_setup.R', encoding='UTF-8')
source.with.encoding('R/pbdb_querys.R', encoding='UTF-8')
source.with.encoding('R/pbdb_taxonomic_functions.R', encoding='UTF-8')
source.with.encoding('R/pbdb_geographic_functions.R', encoding='UTF-8')
source.with.encoding('R/pbdb_temporal_functions.R', encoding='UTF-8')
