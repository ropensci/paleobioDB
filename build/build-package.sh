#!/usr/bin/env bash

# Use this script to build and check "as CRAN" the R package

# Usage: 
# From the root folder of this repo: `./build/build-package.sh`

# Prerequisites: docker

echo 'Building docker image for r-devel, including the R dependencies'
docker build -f Docker/Dockerfile-build -t rpbdb-build .

echo "Building R package"
docker run --rm -ti -w /paleo -v $PWD:/paleo rpbdb-build R CMD build .

echo "Check as CRAN"
docker run --rm -ti -w /paleo -v $PWD:/paleo rpbdb-build R CMD check paleobioDB*tar.gz --as-cran
