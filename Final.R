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

#### API: Final mapping of schools in France ####
library(devtools)
require(devtools)
#devtools::install_github("dkahle/ggmap", ref = "tidyup")
library(ggmap)
register_google(key = "ENTER YOUR API KEY") #### you have to apply for your own API key in Google
names(mymap2)
basemap <- get_map(location = 'france', zoom=6, maptype = "terrain",
                   source='google',color='color')
display.brewer.all(5)
ggmap(basemap) +
  stat_density2d(data = mymap2, aes(x = longitude, y = latitude, fill = ..level.., alpha = ..level..),
                 geom = "polygon", size = 0.05, bins = 600) +
  scale_fill_gradientn(colours=rev(brewer.pal(7,"BuPu")))+
  scale_alpha(range = c(0, 0.3), guide = FALSE)


#### Population data set clean-up ####

population <- read_csv("data/pop2015.csv")
population <- population[5:49699,]
# C14_PMEN_MENCOUPAENF, P15_POP15P & libcom
colnames(population) <- population[2,]
names(population)
#freq(population$Nom.de.la.région, cum=T, sort="dec")
library(stringr)
population$LIBCOM2<- str_replace_all(population$LIBCOM, "<e9>", "e")
population$LIBCOM2<- str_replace_all(population$LIBCOM, "�", "e")
population <- population[3:49695,]

pop2 <- population
pop2$P15_POP15P <- as.numeric(pop2$P15_POP15P)
pop2$P15_POP15P <- round(pop2$P15_POP15P, 1)
pop2$P15_POP15P <- as.factor(pop2$P15_POP15P)
names(mymap2)
latlon <- mymap2[,c("libelle_region", "latitude", "longitude")]
View(latlon)
names(pop2)
names(latlon)
pop2_without_missing <- pop2[!is.na(pop2$P15_POP15P),]
pop_tot <- right_join(pop2_without_missing, latlon, by =c("LIBCOM2" = "libelle_region"))
View(pop_tot)
names(pop_tot)
graph_pop <- pop_tot[,c("LIBCOM2", "latitude", "longitude", "P15_POP15P")]
graph_pop <- graph_pop[!is.na(graph_pop$P15_POP15P),]
unique(pop2$LIBCOM2)
unique(pop2$REG)
unique(latlon$libelle_region)
unique(schools$Code.région)
unique(mymap2$code_region)
pop2 <- pop2[!grepl("NA", pop2$REG),]
pop2 <- pop2[!grepl("04", pop2$REG),]
pop2 <- pop2[!grepl("03", pop2$REG),]
pop2 <- pop2[!grepl("02", pop2$REG),]
pop2 <- pop2[!grepl("01", pop2$REG),]
pop2 <- pop2[!grepl("94", pop2$REG),]
pop_tot <- right_join(pop2, latlon, by =c("LIBCOM2" = "libelle_region"))
graph_pop <- pop_tot[,c("REG", "LIBCOM2", "latitude", "longitude", "P15_POP15P")]
names(pop_tot)

#### Brittany Schools and population ####
basemap_pop <- get_map(location = 'Brittany, France', zoom=8, maptype = "terrain",
                       source='google',color='color')

ggmap(basemap_pop) + 
  geom_point(data=pop_tot, aes(x=longitude,y=latitude, fill = P15_POP15P), 
             alpha = 0.4)+scale_size_continuous(range=c(1,6))+
  theme(legend.position="none")

ggmap(basemap_pop) + 
  geom_point(data=mymap2, aes(x=longitude,y=latitude, color = "schools"), 
             alpha = 0.5)+scale_size_continuous(range=c(1,6))
