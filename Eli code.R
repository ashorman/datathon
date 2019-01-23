## Libraries
library(tidyverse)
install.packages("mapview")
library(mapview)
install.packages("sf")
library(sf)
install.packages("leaflet")
library(leaflet)
install.packages("questionr")
library(questionr)
library(TraMineR)
install.packages("tmap")
library(tmap)
library(tmaptools)
install.packages("tmaptools")

## Load data
schools <- read_sf("./fr-en-adresse-et-geolocalisation-etablissements-premier-et-second-degre.geojson")


freq(schools$libelle_region, cum= T, sort= "dic")

## Get rid of territoires d'autre mer

schools2 <- schools

schools2 <- schools2[!grepl("La Réunion", schools2$libelle_region),]
schools2 <- schools2[!grepl("Corse", schools2$libelle_region),]
schools2 <- schools2[!grepl("Martinique", schools2$libelle_region),]
schools2 <- schools2[!grepl("Mayotte", schools2$libelle_region),]
schools2 <- schools2[!grepl("TOM et Collectivités territoriales", schools2$libelle_region),]
schools2 <- schools2[!grepl("Guadeloupe", schools2$libelle_region),]
schools2 <- schools2[!grepl("Guyane", schools2$libelle_region),]
schools <- schools2

## Plot map n°1 (France red dots)

mapview(schools)
schools %>% 
  leaflet() %>% 
  addTiles() %>% 
  addCircleMarkers(radius = 0.05, color = "red")
 


schools %>% group_by(code_postal_uai) %>% 
  summarise(total= n())

mapview(popula)

map('france',col="#f2f2f2", fill=TRUE, bg="#A6CAE0", mar=rep(0,4), border=0 )










library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)
france <- get_map(location = 'france', zoom = 11)


## Plot map n°2 density (Andrei's code)

install.packages("OpenStreetMap")
library(OpenStreetMap)

schools <- st_transform(schools, crs = 2154)
 basemap <- read_osm(schools, type = "stamen-toner")
 schools_density <- smooth_map(schools, bandwidth =  0.5)


 
 
 
 
 
 
 
## NOT THIS Open data

edu <- read_csv("./edugeoloc.csv") #do import dataset
rm(edu)

## Select relevant variables
names(edugeoloc)
seccode <- c("Secteur Public/Privé","Code postal")
newdata <- edugeoloc[seccode]
seccode2 <- newdata[(newdata$`Secteur Public/Privé` == "Public"),]
sort(table(seccode2$`Code postal`), decreasing =  TRUE)

