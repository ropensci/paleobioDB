FROM rocker/tidyverse:3.5.1

RUN apt update && \
    apt install -y libgdal-dev

RUN Rscript -e 'install.packages(c("RCurl", "rjson", "maps", "gtools", "raster"))'
