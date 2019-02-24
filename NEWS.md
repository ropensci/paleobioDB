paleobioDB 0.6.0
================

MINOR IMPROVEMENTS

* Integrates codecov
* Adds tests execution to travis. Tests are not part of the CRAN check for this
package since they rely on calling to the PaleobioDB API.
* Improves error reporting in pbdb_temp_range. [issue 28](https://github.com/ropensci/paleobioDB/issues/28)

BUG FIXES

* Fix error in pbdb_occurrences under R 3.5.


paleobioDB 0.5.0
===============

BUG FIXES

* Fix bug after JSON responses from the API started including the "elapsed_time" before the "records" element.


paleobioDB 0.4.0
===============

MINOR IMPROVEMENTS

* From now our functions for exploring the data of the paleobioDB use "matched_name" and "matched_number" instead of raw names and raw taxon numbers of the former versions of the package. This means that the default output of richness and first and last occurrences of species/genus/etc. changed. Before we used the original taxonomic identification of the fossil records, now we use the revised taxonomic identification of the fossil records.

BUG FIXES

* Fix duplicated records bug in pbdb_occurrences caused by API returning and array of two identical values in field "reference_no". From now on, if a filed is returned by the API as an array it will be mapped to a semicolon separated string.
