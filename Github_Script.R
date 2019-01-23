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

mymap$secteur_public_prive_libe
pal <- colorBin("PuOr", mymap$secteur_public_prive_libe, bins = c(0, 2))
mymap %>%
  leaflet()%>%
  addTiles()%>%
  addCircleMarkers(radius = 0.05, color = "blue")
  #addCircleMarkers(radius = 0.05, color = ~pal(secteur_public_prive_libe), group = "circles")