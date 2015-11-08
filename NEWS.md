paleobioDB 0.4
===============

MINOR IMPROVEMENTS

* From now our functions for exploring the data of the paleobioDB use "matched_name" and "matched_number" instead of raw names and raw taxon numbers of the former versions of the package. This means that the default output of richness and first and last occurrences of species/genus/etc. changed. Before we used the original taxonomic identification of the fossil records, now we use the revised taxonomic identification of the fossil records.

BUG FIXES

* Fix duplicated records bug in pbdb_occurrences caused by API returning and array of two identical values in field "reference_no". From now on, if a filed is returned by the API as an array it will be mapped to a semicolon separated string.
