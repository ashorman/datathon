#Datathon, Andrei's Code


#Libraries

library(readr) 
library(tidyverse)
library(maps)
library(ggmap) #Cite ggmap is used
library(DT)
library(knitr)
library(geojsonio)
library(mapview)
library(ggplot2)
library(leaflet)
library(raster)
library(scales)
library(tmap)
library(tmaptools)
library(sf)
library(geojson)
library(OpenStreetMap)

  
devtools::install_github("dkahle/ggmap") #installed github version in order to use Google API

register_google(key = "AIzaSyBi_RiXjH48o-83enpZgJOczG67WQu2S4k")

#Maternites Data

library(readr)
Donnees_maternites_2016_Sheet1 <- read_csv("Donnees_maternites_2016 _Sheet1.csv")
View(Donnees_maternites_2016_Sheet1)

Donnees_maternites_2016_Sheet2 <- Donnees_maternites_2016_Sheet1[!grepl("Prive", Donnees_maternites_2016_Sheet1$statut),]
View(Donnees_maternites_2016_Sheet2)


Donnees_maternites_2016_Sheet2 <- Donnees_maternites_2016_Sheet2 %>% 
  filter(!is.na(statut)) %>%
  filter(!is.na(ville)) %>% 
  filter(!is.na(code_postal)) %>% 
  filter(!is.na(adresse)) %>% 
     view()

Donnees_maternites_2016_Sheet2 <- Donnees_maternites_2016_Sheet2[!grepl("Saint-Benoit", Donnees_maternites_2016_Sheet2$ville),]
Donnees_maternites_2016_Sheet2 <- Donnees_maternites_2016_Sheet2[!grepl("Basse-Terre", Donnees_maternites_2016_Sheet2$ville),]
Donnees_maternites_2016_Sheet2 <- Donnees_maternites_2016_Sheet2[!grepl("Mamoudzou", Donnees_maternites_2016_Sheet2$ville),]
Donnees_maternites_2016_Sheet2 <- Donnees_maternites_2016_Sheet2[!grepl("Baie-Mahault", Donnees_maternites_2016_Sheet2$ville),]
Donnees_maternites_2016_Sheet2 <- Donnees_maternites_2016_Sheet2[!grepl("Cayenne", Donnees_maternites_2016_Sheet2$ville),]
Donnees_maternites_2016_Sheet2 <- Donnees_maternites_2016_Sheet2[!grepl("Kourou", Donnees_maternites_2016_Sheet2$ville),]
Donnees_maternites_2016_Sheet2 <- Donnees_maternites_2016_Sheet2[!grepl("Saint-Laurent-du-Maroni", Donnees_maternites_2016_Sheet2$ville),]
Donnees_maternites_2016_Sheet2 <- Donnees_maternites_2016_Sheet2[!grepl("Fort-de-France", Donnees_maternites_2016_Sheet2$ville),]
Donnees_maternites_2016_Sheet2 <- Donnees_maternites_2016_Sheet2[!grepl("Saint-Pierre", Donnees_maternites_2016_Sheet2$ville),]
Donnees_maternites_2016_Sheet2 <- Donnees_maternites_2016_Sheet2[!grepl("Saint-Paul", Donnees_maternites_2016_Sheet2$ville),]
Donnees_maternites_2016_Sheet2 <- Donnees_maternites_2016_Sheet2[!grepl("97400", Donnees_maternites_2016_Sheet2$code_postal),]
Donnees_maternites_2016_Sheet2 <- Donnees_maternites_2016_Sheet2[!grepl("Ajaccio", Donnees_maternites_2016_Sheet2$ville),]

Donnees_maternites_2016_Sheet2$location <- paste0(Donnees_maternites_2016_Sheet2$code_postal, ", " , Donnees_maternites_2016_Sheet2$ville, ", " , Donnees_maternites_2016_Sheet2$adresse)
geo <- geocode(location = Donnees_maternites_2016_Sheet2$location, output="latlon", source="google")

geo %>%
  leaflet() %>% 
  addTiles() %>% 
  addCircleMarkers(radius = 2, color = "red")

#Departement Data

departements <- shapefile("./departements-20160218.shp")
coords <- geo[c("lon", "lat")]
coords <- coords[complete.cases(coords),]
sp <- SpatialPoints(coords)
plot(departements)
plot(sp, col="red", add=TRUE)

departements_fortify <- fortify(departements)

#Geographical distribution code

gg <- ggplot()
gg <- gg + geom_polygon(data=departements_fortify, aes(x=long, y=lat, group=group, fill=NA), color = "black", fill=NA, size=0.5) 
gg <- gg + geom_point(data=geo, aes(x=lon, y=lat, color="#ba2525"))
gg <- gg +  coord_map(xlim = c(-7, 12), ylim = c(52,41))
gg <- gg + labs(x=NULL, y=NULL, 
                title="Geographic Distribution Public 
                and Associatives Maternites in France in 2016",
                subtitle=NULL,
                caption="Source: Le Monde, www.data.gouv.fr")
gg <- gg + theme(plot.title=element_text(face="bold", family="roboto_condensed", size=12))
gg <- gg + theme(plot.caption=element_text(face="bold", family="roboto_condensed", size=9, color="gray", margin=margin(t=10, r=80)))
gg <- gg + theme(legend.position="none")
gg <- gg + theme(axis.line =  element_blank(),
                 axis.text =  element_blank(),
                 axis.ticks =  element_blank(),
                 panel.grid.major = element_blank(),
                 panel.grid.minor = element_blank(),
                 panel.border = element_blank(),
                 panel.background = element_blank())

print(gg)

#Heatmap code

gg <- ggplot()
gg <- gg + stat_density2d(data=geo, show.legend=F, aes(x=lon, y=lat, fill=..level.., alpha=..level..), geom="polygon", size=2, bins=10)
gg <- gg + geom_polygon(data=departements_fortify, aes(x=long, y=lat, group=group, fill=NA), color = "black", fill=NA, size=0.5) 
gg <- gg + scale_fill_gradient(low="#ea4f4f", high="#ff0000", name="Distribution")
gg <- gg +  coord_map("polyconic", xlim = c(-7, 12), ylim = c(52,41)) 
gg <- gg + labs(x=NULL, y=NULL, 
                title="Heatmap Distribution of Public and 
                Associative Maternites in France 2016",
                subtitle=NULL,
                caption="Source: Le Monde, www.data.gouv.fr")
gg <- gg + theme(plot.title=element_text(face="bold", family="roboto_condensed", size=12))
gg <- gg + theme(plot.caption=element_text(face="bold", family="roboto_condensed", size=9, color="gray", margin=margin(t=10, r=80)))
gg <- gg + theme(legend.position="none")
gg <- gg + theme(axis.line =  element_blank(),
                 axis.text =  element_blank(),
                 axis.ticks =  element_blank(),
                 panel.grid.major = element_blank(),
                 panel.grid.minor = element_blank(),
                 panel.border = element_blank(),
                 panel.background = element_blank())
gg

#Schools Distribution Code

schools <- read_sf("./fr-en-adresse-et-geolocalisation-etablissements-premier-et-second-degre.geojson", promote_to_multi = FALSE)
departementgeo <- read_sf("./departements.geojson")

schools2 <- schools[!grepl("La Réunion", schools$libelle_region),]
schools2 <- schools[!grepl("Corse", schools$libelle_region),]
schools2 <- schools[!grepl("Martinique", schools$libelle_region),]
schools2 <- schools[!grepl("Mayotte", schools$libelle_region),]
schools2 <- schools[!grepl("TOM et Collectivités territoriales", schools$libelle_region),]
schools2 <- schools[!grepl("Guadeloupe", schools$libelle_region),]
schools2 <- schools[!grepl("Guyane", schools$libelle_region),]

schools2 <- st_transform(schools2, crs = 2154)
departementgeo <- st_transform(departementgeo, crs = 2154)

schools2 <- schools2 %>% 
  filter(!is.na(longitude)) %>%
  filter(!is.na(latitude)) %>% 
  view()

basemap <- read_osm(schools2, type = "stamen-toner")
school_density <- smooth_map(schools2, bandwidth = 0.2, cover = departementgeo)

tm_shape(departementgeo) +
  tm_borders() +
  tm_shape(school_density$polygons) +
  tm_fill(col = "level", palette = "Blues" ,title = "Density in France", alpha = 0.5, text.to.columns = TRUE) +
  tm_shape(schools2) +
  tm_dots(shape = 0.03, col = "#f44542") +
  tm_style("gray", title = "School") +
  tm_scale_bar(position = c("left", "BOTTOM")) +
  tm_compass() +
  tm_legend(outside = TRUE)
