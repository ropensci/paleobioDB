FROM rocker/tidyverse:4

RUN apt-get update && \
    apt-get install -y libgdal-dev

RUN Rscript -e 'install.packages(c("curl", "rjson", "maps", "gtools", "terra"))'
