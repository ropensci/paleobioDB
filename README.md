paleobioDB
=======

### About

`paleobioDB` is a package for downloading, visualizing and processing data from [Paleobiology Database](http://paleobiodb.org/).


### Quick start

**Install**

Install dependencies


```coffee
install.packages(c("rjson","plyr","gtools", "RCurl"))
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

Beware of synonyms and errors, they could twist your estimations about species richness across time, evolutionary and extinction rates, etc. paleobioDB users should be critical about the raw data downloaded from the database and filter the data before analyzing it.

For instance, when using base_name in pbdb_occurrences check out the synonyms that could appear in "taxon_name", "genus_name", etc. 


**Subtaxa**

To know how many species, genera, families, etc. are in your data.
  
```coffee
pbdb_subtaxa (canidae, do.plot=TRUE)         

```

         
## Meta

Please report any [issues or bugs](https://github.com/ropensci/pbdb/issues).

License: CC0

To cite package `paleobioDB` in publications use:

```coffee
To cite package `paleobioDB` in publications use:

Sara Varela, Javier Gonzalez-Hernandez and Luciano F. Sgarbi (2014). paleobioDB: an R-package for downloading, visualizing and processing data from the Paleobiology Database. R package version 0.1. https://github.com/ropensci/paleobioDB

A BibTeX entry for LaTeX users is

  @Manual{,
    title = {paleobioDB: an R-package for downloading, visualizing and processing data from the Paleobiology Database},
    author = {{Sara Varela} and {Javier Gonzalez-Hernandez} and {Luciano F. Sgarbi}},
    year = {2014},
    note = {R package version 0.1},
    base = {https://github.com/ropensci/paleobioDB},
  }
```

---

This package is part of the [rOpenSci](http://ropensci.org/packages) project.

[![](http://ropensci.org/public_images/github_footer.png)](http://ropensci.org)
