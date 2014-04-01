
kk<- pbdb_occurrence (id=1001, show="coords")
is.data.frame (kk)

pbdb_occurrences (id=c(10, 11)) 

data<-  pbdb_occurrences (limit="all", vocab="pbdb",
                                  base_name="Mammalia",  
                          interval="Quaternary", 
                          show=c("coords", "phylo", "ident"))
carnivo<- data[duplicated (data$taxon_no), ]
canidae<- canis
pbdb_subtaxa (canidae, do.plot=TRUE) 
pbdb_temporal_resolution (canidae)
pbdb_time_span (canidae, rank="species")
pbdb_richness (canidae, rank="species", temporal_extent=c(0,10), resolution=1)
# evolutionary rates= evo_ext=1
pbdb_evo_ext (canidae, rank="species", evo_ext=1, temporal_extent=c(0,10), resolution=1)
# extinction rates= evo_ext=2
pbdb_evo_ext (canidae, rank="species", evo_ext=2, temporal_extent=c(0,10), resolution=1)
pbdb_map(canidae)
pbdb_map_effort (canidae, res= 2)
pbdb_map_richness (data, res= 3, rank="species")


head (data)
library (testthat)
test_dir("C:/Users/sara/Documents/_CIENCIAS/pbdb/tests/testthat")

carnivora<-  pbdb_occurrences (limit="all", vocab="pbdb",
                          base_name="Carnivora",  
                          interval="Quaternary", 
                          show=c("phylo", "ident"))

pbdb_evo_ext (carnivora, rank="species", temporal_extent =c(0, 10), 
              evo_ext=2) # plot of the evolutive rates.
pbdb_evo_ext (canis, rank="species", evo_ext=2, 
              temporal_extent=c(0,5), resolution=0.5)

x11()
  ) 
pbdb_map (canis)

canis
f1<-function(){
  print(names(dev.cur()))
  print(Sys.info())
}
f1()


f1<-function(){
  x11()
  print(names(dev.cur()))
  print(Sys.info())
}
f1()

pbdb_subtaxa (data)
names (canidae_quat)
data<- canidae_quat
head (canidae_quat)

pbdb_collection (id=1)
pbdb_collections (limit=100, base_name="Cetacea")

pbdb_ref_occurrences (vocab="pbdb", taxon_name="Canis", year=2000)
pbdb_taxon (name="Canis", vocab="pbdb", show=c("attr", "app", "size"))
pbdb_taxa (name="Canidae", vocab="pbdb", show=c("attr", "app", "size", "nav"))
pbdb_taxa (name =c("Canis lupus", "Vulpes vulpes"), vocab="pbdb", 
           show=c("attr", "app", "size", "nav"), rel="common_ancestor")

pbdb_taxa_auto (name="Canis", vocab="pbdb", limit=10)
pbdb_interval (id=1)
pbdb_interval (id=1, vocab="pbdb")
pbdb_intervals (min_ma= 0, max_ma=2)

pbdb_scale (id=1, vocab="pbdb")
pbdb_scales ()
pbdb_collection (1003, vocab="pbdb", show="loc")

pbdb_strata (lngmin=0, lngmax=15, latmin=0, latmax=15, rank="formation", vocab="pbdb")
pbdb_strata_auto (name= "Pin")
pbdb_collections (base_name="Cetacea", interval="Miocene")
pbdb_collections_geo (vocab="pbdb", lngmin=0.0, lngmax=15.0, latmin=0.0, latmax=15.0, level=2)
pbdb_ref_collections (id=1)
pbdb_references (author="Turner")

canis<- pbdb_occurrences (limit="50", vocab= "pbdb", base_name="Canis", show="coords")

head (canis)
# the function plot will plot the query and save a png file in your working directory
plot_pbdb (canis, "Canis",
           dir="C:/Users/sara/Documents/_CIENCIAS/pbdb_paper")

# use min_ma and max_ma to add a filter to the data: minimum and maximum age (specified in Ma) 
canis_0_2<- pbdb_occurrences (limit=100, 
                                           base_name="Canis", 
                                           min_ma = 0,
                                           max_ma = 2,
                                           show="coords")

plot_pbdb (query= canis_0_2, col="red", name= "Canis (0-2 Ma)", 
           dir="C:/Users/sara/Documents/_CIENCIAS/pbdb_paper")

canis_quaternary<- pbdb_occurrences (limit="all", 
                                    #vocab= "pbdb", 
                                    base_name="Canis", 
                                    interval="Quaternary",
                                    show="coords")

pbdb_reference(360)
pbdb_ref_taxa (name="Canidae")

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

carnivora2<- pbdb_occurrences (limit="all", 
                              base_name="Canis",
                              interval="Quaternary",             
                              show=c("coords", "phylo", "ident"))
data<- carnivora2
pbdb_richness(carnivora2)
pbdb_ext (canis, rank="species")
pbdb_ext (data, rank="species")
data<-  pbdb_occurrences (limit="all", vocab="pbdb",
base_name="Canidae", show=c("phylo", "ident"))
pbdb_richness (data, rank="species", resolution=0.1, temporal_extent=c(0,40))
 
carnivora [295,]
for (i in 1:dim (carnivora)[2]){
print (match (carnivora [i], carnivora2[i]))
}

cbind (names (carnivora), names (carnivora2))

kk<- carnivora [carnivora$rnk==3,]
?pbdb_occurrences

head (carnivora)
pbdb_subtaxa (carnivora)
head (carnivora)
pbdb_richness (carnivora, rank="genera", 
               resolution=1, temporal_extent=c(0,10))
pbdb_evo (carnivora, rank="species")

barplot (unlist (canis_sbtx), 
         col="turquoise1", 
         border=F, axes=F, xlab="")
?barplot
?hist

?tab

write.table (canidae_quat, "C:/Users/sara/Documents/_CIENCIAS/pbdb/data/canidae_quat.csv", sep=",", row.names=F)

pbdb_subtaxa (canis_quaternary)

str (number of taxa)
# to check the temporal resolution of the data, in Ma. 
pbdb_temporal_resolution (data)
names (carnivora)
names (data)

par ( mar=c(8,4,2,0))
hist (unlist (kk [[2]]), freq=T, col="blue", border=F,
      breaks= 50, xlab="Temporal resolution of the data (Ma)", 
      main="")

?hist
mydata<-  unlist (kk [[2]])
ggplot(mydata) + geom_histogram() + scale_x_log()


         beside = T, horiz=F,
         col=heat.colors(100),
         border=F,
         las=2)

# to plot the temporal spam of taxa, set taxon (e.g., "species", "genus"...), 
# change the colour (blue by default), allow names or not (by default TRUE)
names (canis)
pbdb_time_span (canis, col="blue", rank="species",
                names=T)




pbdb_query_occurrence (id=1001)

lista<- paste (selection$genus_name[match (row.names (temporal_range), 
                                               selection$taxon_no)], 
                   selection$species_name [match (row.names (temporal_range), 
                                                  selection$taxon_no)])

selection [which (selection$genus_name=="Phoca" & selection$species_name=="vitulina"),]


lista [which (duplicated (lista))]
  
  
carnivora$genus_name

# plot 
carnivora<-  pbdb_occurrences (limit="all", vocab="pbdb",
                             base_name="Carnivora", 
                             interval="Quaternary",             
                             show=c("coords", "phylo", "ident"))  
data<- carnivora
artiodactyla<-  pbdb_occurrences (limit="all",
                               base_name="Artiodactyla", 
                               interval="Quaternary",             
                               show=c("coords", "phylo", "ident"))  

pbdb_subtaxa (artiodactyla, do.plot=TRUE)


canis<-  pbdb_occurrences (limit="all", vocab="pbdb",
                           base_name="canis", 
                               interval="Quaternary",             
                               show=c("coords", "phylo", "ident")) 

pbdb_time_span (canidae, rank="genus")

canidae$genus_name
pbdb_temporal_resolution (canidae)
pbdb_richness (canidae, rank="species", temporal_extent=c(0,10), resolution=1)

pbdb_evo_ext (canidae, rank="species", evo_ext=2, temporal_extent=c(0,10), resolution=1)


names (carnivora)
carnivora2$rnk
dev.off()
pbdb_subtaxa (canidae, do.plot=TRUE)
canidae

canis
par ( mar=c(8,4,2,0))
barplot (unlist (canis_sbtx),  
         beside = T, horiz=F,
         col=heat.colors(12),
         border=F,
         las=2)

pbdb_time_spam (canis, rank="species")

## extinction
canidae<-  pbdb_occurrences (limit=100,
                                        base_name="Canis")

head (canidae)
pbdb_evo (canidae)
pbdb_ext_evo (canis, rank="genus")
pbdb_ext_evo (canis, rank="species")

head (canis)

canis$early_age

data<- canis
data<- carnivora
names (carnivora)
names (canidae)
pbdb_richness (canidae, 
               rank="family", 
               resolution=1, 
               temporal_extent=c(0,3))


head (carnivora)
canidae<- pbdb_taxa (name="Canidae", vocab="pbdb", show=c("attr", "app", "size", "nav"))
canidae


canis 
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



