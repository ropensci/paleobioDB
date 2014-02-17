#' pbdb_query_occurrences is a highly flexible function. It allows all the parameters available in the PBDB API.
#' 
#' Complete list of paramenters is available in http://paleobiodb.org/data1.1/occs/list
#' Here, for practical reasons we selected the most common filters that 
#' paleontologists and ecologists might use to run our examples.
#' 
#'@param taxon_name Return only records associated with the specified taxonomic name(s). 
#'You may specify multiple names, separated by commas.
#'@param base_name  Return only records associated with the specified taxonomic name(s), or any of their children. 
#'You may specify multiple names, separated by commas.
#'@param show to show extra variables (e.g. coords)
#' 
#' Set the basic query: 
#' limit= "all" to download all the occurrences (by default the API limit is 500) 
#' vocab= "pbdb" to show the names of the variables complete (by default variables have short 3-letter names)
#' base_name= "Canis" to download all the records from the taxon and subtaxa 
#' (e.g., "Canis", "Canis lupus", "Canis mosbachensis", etc.)
#' show ="coords" to show latitude and longitude (by default they are not shown)
#' 
#' 

canis<- pbdb_query_occurrences (limit="100", 
                                vocab= "pbdb", 
                                base_name="Canis", 
                                show="coords")

# the function plot will plot the query and save a png file in your working directory
plot_pbdb (query= canis, name="Canis", col="turquoise1",  
           dir="C:/Users/sara/Documents/_CIENCIAS/pbdb_paper")


query<- canis
name<- "canis"
col="#32caf6"

# use min_ma and max_ma to add a filter to the data: minimum and maximum age (specified in Ma) 
canis_0_2<- pbdb_query_occurrences (limit=100, 
                                           vocab= "pbdb", 
                                           base_name="Canis", 
                                           min_ma = 0,
                                           max_ma = 2,
                                           show="coords")

plot_pbdb (query= canis_0_2, name= "Canis (0-2 Ma)", 
           dir="C:/Users/sara/Documents/_CIENCIAS/pbdb_paper")

canis_quaternary<- pbdb_query_occurrences (limit="all", 
                                    vocab= "pbdb", 
                                    base_name="Canis", 
                                    interval="Quaternary",
                                    show="coords")

plot_pbdb (canis_quaternary, "Canis Quaternary")
