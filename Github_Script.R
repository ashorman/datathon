library(leaflet)
library(tidyverse)
library(readr)
library(TraMineR)
library(cluster)
library(questionr)
library(tidyverse)
library(RColorBrewer)
library(ggplot2)
library(httr)
library(ggmap)
library(maps)
library(sf)
library(mapview)
mymap <- read_sf("data/schools.geojson")
mapview(mymap)
View(mymap)


freq(mymap$libelle_region, cum = T, sort = "dec")
mymap2 <- mymap
mymap2 <- as.data.frame(mymap2)
mymap2 <- mymap2[!grepl("Martinique", mymap2$libelle_region),]
mymap2 <- mymap2[!grepl("La Réunion", mymap2$libelle_region),]
mymap2 <- mymap2[!grepl("Guadeloupe", mymap2$libelle_region),]
mymap2 <- mymap2[!grepl("TOM et Collectivités territoriales", mymap2$libelle_region),]
mymap2 <- mymap2[!grepl("Corse", mymap2$libelle_region),]
mymap2 <- mymap2[!grepl("Mayotte", mymap2$libelle_region),]
mymap2 <- mymap2[!grepl("Guyane", mymap2$libelle_region),]

mymap <- mymap2
#### The red map ####
mymap$secteur_public_prive_libe
mymap %>%
  leaflet()%>%
  addTiles()%>%
  addCircleMarkers(radius = 0.05, color = "blue")
  #addCircleMarkers(radius = 0.05, color = ~pal(secteur_public_prive_libe), group = "circles")


#### API: Final mapping of schools in France ####
library(devtools)
require(devtools)
#devtools::install_github("dkahle/ggmap", ref = "tidyup")
library(ggmap)
register_google(key = "ENTER YOUR API KEY")
names(mymap2)
basemap <- get_map(location = 'france', zoom=6, maptype = "terrain",
                   source='google',color='color')
display.brewer.all(5)
ggmap(basemap) +
  stat_density2d(data = mymap2, aes(x = longitude, y = latitude, fill = ..level.., alpha = ..level..),
                 geom = "polygon", size = 0.05, bins = 600) +
  scale_fill_gradientn(colours=rev(brewer.pal(7,"BuPu")))+
  scale_alpha(range = c(0, 0.3), guide = FALSE)
