library(rgdal)
library(ggplot2)



# read shapefile
setwd ("C:/Users/sara/Documents/_CIENCIAS/pbdb/docs_desarrollo")
wmap <- readOGR(dsn="ne_110m_land.shp", layer="ne_110m_land")
grat <- readOGR("ne_110m_graticules_15.shp", layer="ne_110m_graticules_15")
bbox <- readOGR("ne_110m_wgs84_bounding_box.shp", layer="ne_110m_wgs84_bounding_box")
countries <- readOGR("ne_110m_admin_0_countries.shp", layer="ne_110m_admin_0_countries")

# Winkel tripel projection
countries_wintri <- spTransform(countries, CRS("+proj=wintri"))
bbox_wintri <- spTransform(bbox, CRS("+proj=wintri"))
wmap_wintri <- spTransform(wmap, CRS("+proj=wintri"))
grat_wintri <- spTransform(grat, CRS("+proj=wintri"))


#read data PBDB, dataframe, latlong, number of fossil record. 
places <- readOGR("ne_110m_populated_places.shp", layer="ne_110m_populated_places")
places_df <- as(places, "data.frame")
places_robin_df <- project(cbind(places_df$LONGITUDE, places_df$LATITUDE), proj="+proj=wintri")
places_robin_df <- as.data.frame(places_robin_df)
names(places_robin_df) <- c("LONGITUDE", "LATITUDE")
places_robin_df$POP2000 <- places_df$POP2000 

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


# con meridianos y paralelos
ggplot(bbox_wintri, aes(long,lat, group=group)) +
  geom_polygon(fill="black") +
  geom_polygon(data=countries_wintri, 
               aes(long,lat, group=group, fill=hole)) +
  geom_point(data=places_robin_df, 
             aes(LONGITUDE, LATITUDE, 
                 group=NULL, fill=NULL, 
                 size=POP2000/2), 
             color="#32caf6", alpha=0.5) +
  geom_path(data=countries_wintri, 
            aes(long,lat, group=group, fill=hole), 
            color="grey50", size=0.3) +
  # geom_path(data=grat_wintri, aes(long, lat, group=group, fill=NULL), linetype=3, color="grey60") +
  labs(title="PBDB: Canis + Quaternary") +
  coord_equal(ratio=1) +
  theme_opts +
  scale_fill_manual(values=c("white", "white"), guide="none") + # change colors & remove legend
  scale_size_continuous(range=c(1,20), 
                        guide="none") # change colors & remove legend

p<- ggplot(bbox_wintri, aes(long,lat, group=group)) +
  geom_polygon(fill="black") +
  geom_polygon(data=countries_wintri, 
               aes(long,lat, group=group, fill=hole)) +
  geom_point(data=places_robin_df, 
             aes(LONGITUDE, LATITUDE, 
                 group=NULL, fill=NULL, 
                 size=POP2000/2), 
             color="#32caf6", alpha=0.5) +
  geom_path(data=countries_wintri, 
            aes(long,lat, group=group, fill=hole), 
            color="grey50", size=0.3) +
  # geom_path(data=grat_wintri, aes(long, lat, group=group, fill=NULL), linetype=3, color="grey60") +
  labs(title="PBDB: Canis + Quaternary") +
  coord_equal(ratio=1) +
  theme_opts +
  scale_fill_manual(values=c("white", "white"), guide="none") + # change colors & remove legend
  scale_size_continuous(range=c(1,20), 
                        guide="none") # change colors & remove legend

plot (p)
ggsave(plot=p, "map.png", width=12.5, height=8.25, dpi=300)





