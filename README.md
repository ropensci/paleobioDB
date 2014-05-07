[![Build Status](https://travis-ci.org/ropensci/paleobioDB.svg)](https://travis-ci.org/ropensci/paleobioDB)

paleobioDB
=======

### About

`paleobioDB` is a package for downloading, visualizing and processing data from [Paleobiology Database](http://paleobiodb.org/).


### Quick start

**Install**

Install and load dependencies


```coffee
install.packages(c("rjson","plyr","gtools", "RCurl", "maps", "scales", "raster"))
library(maps)
library(raster)
```

Install paleobioDB

```coffee
install.packages("devtools")
library(devtools)
install_github("ropensci/paleobioDB")
library(paleobioDB)
```

**General overview***

`paleobioDB` version 0.1 has 19 functions to wrap each endpoint of the PaleobioDB API, plus 8 functions to visualize and process the fossil data. The API documentation for the Paleobiology Database can be found [here](http://paleobiodb.org/data1.1/).

**Download fossil occurrences from the PaleobioDB** 

To download all the fossil data that belongs to the genus Canis, set base_name=Canis.  

```coffee
canidae<-  pbdb_occurrences (limit="all",
                               base_name="canidae", 
                               interval="Quaternary",             
                               show=c("coords", "phylo", "ident"))
```

**CAUTION WITH THE RAW DATA**

Beware of synonyms and errors, they could twist your estimations about species richness, evolutionary and extinction rates, etc. paleobioDB users should be critical about the raw data downloaded from the database and filter the data before analyzing it.

For instance, when using "base_name" for downloading the information with the function pbdb_occurrences, check out the synonyms and errors that could appear in "taxon_name", "genus_name", etc. In our example, in canidae$genus_name there are errors: "Canidae" and "Caninae" appeared as genus names. If not eliminated, they will increase the richness of genera. 


**pbdb_subtaxa**

To know how many species, genera, families, etc. are in your data.
  
```coffee
pbdb_subtaxa (canidae, do.plot=TRUE)         

```
**pbdb_temporal_resolution**

To find out about the temporal resolution of the data in your query

```coffee
pbdb_temporal_resolution (canidae)
```   

**pbdb_time_span**

Returns a dataframe and a plot with the time span of the species, genera, families, etc. in your query.

```coffee
pbdb_temp_range (canidae, rank="species")
``` 

**pbdb_richness**

Returns a dataframe and a plot with the number of species (or genera, families, etc.) across time. You should set the temporal extent and the temporal resolution for the steps.

```coffee
pbdb_richness (canidae, rank="species", temporal_extent=c(0,10), res=1)
``` 

**pbdb_orig_ext**

Returns a dataframe and a plot with the number of new appearances and last appearances of species, genera, families, etc. in your query across the time. You should set the temporal extent and the resolution of the steps. 

```coffee
# evolutionary rates= orig_ext=1
pbdb_orig_ext (canidae, rank="species", orig_ext=1, temporal_extent=c(0,10), res=1)

# extinction rates= orig_ext=2
pbdb_orig_ext(canidae, rank="species", orig_ext=2, temporal_extent=c(0,10), res=1)

``` 

**pbdb_map**

Returns a map with the species occurrences.

```coffee
pbdb_map(canidae)
``` 
**pbdb_map_occur**
Returns a map and a raster object with the sampling effort (number of fossil records per cell).

```coffee
pbdb_map_occur (canidae, res= 2)
``` 
**pbdb_map_richness**
Returns a map and a raster object with the number of different species, genera, family, etc. per cell.

```coffee
pbdb_map_richness (canidae, res= 3, rank="species")
``` 

## Meta

Please report any [issues or bugs](https://github.com/ropensci/pbdb/issues).

License: GPL-2

To cite package `paleobioDB` in publications use:

```coffee
To cite package `paleobioDB` in publications use:

Sara Varela, Javier Gonzalez-Hernandez and Luciano Fabris Sgarbi (2014). paleobioDB: an R-package for downloading, visualizing and processing data from the Paleobiology Database. R package version 0.1. https://github.com/ropensci/paleobioDB

A BibTeX entry for LaTeX users is

  @Manual{,
    title = {paleobioDB: an R-package for downloading, visualizing and processing data from the Paleobiology Database},
    author = {{Sara Varela} and {Javier Gonzalez-Hernandez} and {Luciano Fabris Sgarbi}},
    year = {2014},
    note = {R package version 0.1},
    base = {https://github.com/ropensci/paleobioDB},
  }
```

---

This package is part of the [rOpenSci](http://ropensci.org/packages) project.

[![](http://ropensci.org/public_images/github_footer.png)](http://ropensci.org)
