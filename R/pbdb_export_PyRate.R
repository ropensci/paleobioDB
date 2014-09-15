#' pbdb_export_PyRate
#'
#' Returns a dataframe with the adequate structure to run PyRate
#' http://www2.unil.ch/phylo/bioinformatics/pyrate.html
#'
#' @usage pbdb_export_PyRate (base_name)
#'
#' @param base_name Return only records associated with the specified taxonomic name(s), or any of their children. e.g. Canidae; the function will download all the occurrences from the Paleobiology Database 
#' of all the species belonging to the Canidae family. 
#' 
#' @return a dataframe with the species, minimum and maximum age of the occurrences, and of the species is extinct or extant.
#' 
#' @export
#' @examples \dontrun{
#' pbdb_export_PyRate ("Hyaenidae")
#' }
#'

pbdb_export_PyRate<- function (base_name){

data<-  pbdb_occurrences (limit="all", base_name= base_name, 
                          show=c("ident"))

data2<- data [data$rnk==3, c(10,11, 13, 14)]

extinct<- pbdb_taxa (name= base_name, rel="all_children")

columns<- match (paste (data2$idt, data2$ids, sep=" "), extinct$nam)

data3<- data.frame (species=paste (data2$idt, data2$ids, sep="_"), data2[,1:2], 
                    extant=extinct$ext[columns])
return (data3)
}

## TODOs: 
## 1. What to do with synonyms? 
## 2. What to do with NA in the extinct/extant field. In the example
## Hyaena brunnea is a NA! So, there are clear examples of living species with data, 
## but with no link to the paleobioDB extinct/extanct field

