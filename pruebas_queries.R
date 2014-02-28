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

canis<- pbdb_query_occurrences (limit="all", 
                                vocab= "pbdb", 
                                base_name="Canis", 
                                show="coords")

head (canis)
# the function plot will plot the query and save a png file in your working directory
plot_pbdb (canis, "Canis",
           dir="C:/Users/sara/Documents/_CIENCIAS/pbdb_paper")


# use min_ma and max_ma to add a filter to the data: minimum and maximum age (specified in Ma) 
canis_0_2<- pbdb_query_occurrences (limit=100, 
                                           base_name="Canis", 
                                           min_ma = 0,
                                           max_ma = 2,
                                           show="coords")

plot_pbdb (query= canis_0_2, col="red", name= "Canis (0-2 Ma)", 
           dir="C:/Users/sara/Documents/_CIENCIAS/pbdb_paper")

canis_quaternary<- pbdb_query_occurrences (limit="all", 
                                    #vocab= "pbdb", 
                                    base_name="Canis", 
                                    interval="Quaternary",
                                    show="coords")

data<- canis_quaternary
plot_pbdb (canis_quaternary, "Canis Quaternary", 
           dir="C:/Users/sara/Documents/_CIENCIAS/pbdb_paper")

names (canidae_quat)
# to check the number of species, genera, tribes, families, etc. within a taxon: 
# set the query not using the pbdb vocab, but the default 3 letters varibles 

canidae_quat<-  pbdb_query_occurrences (limit="all", 
                                           base_name="Canidae",  
                                           interval="Quaternary",
                                           show="coords")
names (canis_quaternary)


pbdb_subtaxa (canidae_quat)
write.table (canidae_quat, "C:/Users/sara/Documents/_CIENCIAS/pbdb/data/canidae_quat.csv", sep=",", row.names=F)

pbdb_subtaxa (canis_quaternary)

str (number of taxa)
# to check the temporal resolution of the data, in Ma. 
temporal_resolution (canidae_quat)

# to plot the temporal spam of taxa, set taxon (e.g., "species", "genus"...), 
# change the colour (blue by default), allow names or not (by default TRUE)

pbdb_time_spam (canis_quaternary, col="red", rank="species",
                names=F)




pbdb_query_occurrence (id=1001)



# from here, just mess.... UNDER CONSTRUCTION! 

species<- data [data$rnk==3, ]
max_sp<- aggregate(species$eag, list(species$tna), max)
min_sp<- aggregate(species$lag, list(species$tna), min)
temporal_range<- data.frame (max_sp [,2], min_sp[,2])
row.names (temporal_range)<- max_sp[,1]
colnames (temporal_range)<- c("max", "min")
pos<- c(1:dim (temporal_range)[1])
t_range<- cbind (temporal_range, pos)
order_by_appearance<- t_range [order (t_range$max),]
t_range<- order_by_appearance
t_range$pos<- c(1:dim (t_range)[1])

division<- seq(from=0, to=2, by=0.2)

which (t_range$min <= division [1] & t_range$max>= division [2])


order_by_appearance<- t_range [order (t_range$max),]
t_range<- order_by_appearance
t_range$pos<- c(1:dim (t_range)[1])

plot(c(min (t_range$min), max (t_range$max)),
     c(0, dim (t_range)[1]+1),
     type = "n",axes = FALSE, xlab = "Time (Ma)", ylab = "")
segments(x0 = t_range$min,
         y0 = t_range$pos,
         x1 = t_range$max,
         y1 = t_range$pos,
         col = "red",
         lwd = 6,
         lend = 2)
text(x = t_range$min, y = t_range$pos +0.3,
     labels = row.names (t_range), adj=c(0,0), cex=0.6, col="gray30")
axis(1, col="gray30") 



canis_0_2
kk (canis)



