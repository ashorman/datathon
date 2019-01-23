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

mymap$secteur_public_prive_libe
mymap %>%
  leaflet()%>%
  addTiles()%>%
  addCircleMarkers(radius = 0.05, color = "blue")
  #addCircleMarkers(radius = 0.05, color = ~pal(secteur_public_prive_libe), group = "circles")