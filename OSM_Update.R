##Create Map

setwd("C:/Users/Annika/Desktop/Grundkarte/Bayern")
Packages <- c("osmdata", "tidyverse", "sf", "ggmap", "rgdal")

#install the osmdata, sf, tidyverse and ggmap package

for(pkg in Packages){
  if(!require(pkg, character.only = TRUE)) install.packages(pkg, character.only = TRUE)
}

#load packages
for(pkg in Packages){
  library(pkg, character.only = TRUE)
}

#our background map
mad_map <- get_map(getbb("Madrid"),maptype = "toner-background")

#final map
ggmap(mad_map)+
  geom_sf(data=cinema$osm_points,
          inherit.aes =FALSE,
          colour="#238443",
          fill="#004529",
          alpha=.5,
          size=4,
          shape=21)+
  labs(x="",y="")


#shape <- readOGR(dsn = "C:/Users/Annika/Desktop/Grundkarte/Bayern/Ausschnitt.shp")
