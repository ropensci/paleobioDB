query<- canis_0_2
name<- "Canis"
col="turquoise1" 
dir="C:/Users/sara/Documents/_CIENCIAS/pbdb_paper"

plot_pbdb<- function (query, name, col="turquoise1", dir){
    # read shapefile
    wmap <- readOGR(dsn="maps/ne_110m_land.shp", layer="ne_110m_land")
    bbox <- readOGR("maps/ne_110m_wgs84_bounding_box.shp", layer="ne_110m_wgs84_bounding_box")
    countries <- readOGR("maps/ne_110m_admin_0_countries.shp", layer="ne_110m_admin_0_countries")
    
    # Winkel tripel projection
    countries_wintri <- spTransform(countries, CRS("+proj=wintri"))
    bbox_wintri <- spTransform(bbox, CRS("+proj=wintri"))
    wmap_wintri <- spTransform(wmap, CRS("+proj=wintri"))
    
    #read data and project using winkel tripel projection
    latlong <- project(cbind(query[,11], query[,12]), proj="+proj=wintri")
    latlong<- as.data.frame (latlong)
    names (latlong)<- c("lng", "lat")
    counts<- ddply(latlong,.(lng,lat),nrow)
   
    # create a blank ggplot theme
    theme_opts <- list(theme(panel.grid.minor = element_blank(),
                             panel.grid.major = element_blank(),
                             panel.background = element_blank(),
                             plot.background = element_rect(fill="white"),
                             panel.border = element_blank(),
                             axis.line = element_blank(),
                             axis.text.x = element_blank(),
                             axis.text.y = element_blank(),
                             axis.ticks = element_blank(),
                             axis.title.x = element_blank(),
                             axis.title.y = element_blank(),
                             plot.title = element_text(size=22)))
    
  
  
  #plot (the size of the points is the number of records in the site)
  
  p<- ggplot(bbox_wintri, aes(long,lat, group=group)) +
    
    geom_polygon(fill="black") +
    
    geom_polygon(data=countries_wintri, 
                 aes(long,lat, group=group, fill=hole)) +
    
    geom_point(data=counts, 
               aes(counts$lng, counts$lat, 
                   group=NULL, fill=NULL, 
                   size=counts$V1), 
               color=col, alpha=0.5) +
    
    geom_path(data=countries_wintri, 
              aes(long,lat, group=group, fill=hole), 
              color="grey50", size=0.3) +
    
    # geom_path(data=grat_wintri, aes(long, lat, group=group, fill=NULL), linetype=3, color="grey60") +
    labs(title=name) +
    coord_equal(ratio=1) +
    theme_opts +
    scale_fill_manual(values=c("white", "white"), guide="none") + # remove legend
    scale_size_continuous(limits=c(1,20), guide="none") # remove legend
  
  plot (p)
  
  ggsave(plot=p, paste (dir,"/", name, ".png", sep=""), width=12.5, height=8.25, dpi=300)
    
}



