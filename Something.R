library(readr)
rne <- read_csv("Repertoire-national-des-elus.csv")
View(rne)

library(tidyverse)
rne %>% 
  filter(`Code sexe` %in% "F") %>% 
  mutate(`Code sexe` = recode(`Code sexe`,"F"="Female", "H" = "Male")) %>% 
  arrange(`Date de naissance`) %>% 
  group_by(`Libellé de la profession`) %>%
  summarise(n = n(), Age=mean(Age)) %>% 
  arrange(desc(n)) %>% 
  view()

rne %>% 
  gather("mandat", "value", `Conseiller Municipal`: Maire) %>% 
  filter(value %in% TRUE) %>%
  filter(!(`Date de naissance` %in% lubridate::ymd("1900-01-01"))) %>% 
  select(-value) %>% 
  group_by(mandat) %>%
  filter(!is.na(Age)) %>% 
  summarise(Age=mean(Age, na.rm = TRUE)) %>% 
  view()

rne %>% 
  gather("mandat", "value", `Conseiller Municipal`: Maire) %>% 
  filter(value %in% TRUE) %>%
  select(-value) %>%
  group_by(Identifiant) %>% 
  summarise(offices = n(), occupation= unique(`Libellé de la profession`) , gender = unique(`Code sexe`)) %>%
  ungroup() %>% 
  group_by(occupation, gender) %>% 
  summarise(offices = mean(offices)) %>% 
  view()

rne %>% 
  filter(`Code sexe` %in% "F") %>% #To choose what to look for and where
  group_by(`Libellé de la profession`) %>% #to group by a certain variable
  arrange(desc(Age)) %>% #arrange according to a variable
  slice(2) #slice a given variable by a rule (second youngest here)
mutate(rank = 1:n()) %>% #modify or create a variable
  view()

rne %>%
  mutate(number=case_when(`Nombre de mandats` %in% 1 ~"one",
                          `Nombre de mandats` %in% 2 ~ "two",
                          `Nombre de mandats` %in% 3 ~ "three",
                          `Nombre de mandats` %in% 4 ~ "four"))


library(hrbrthemes)

cairo_pdf(file = "./plot1.pdf", width = 12, height = 7)
rne %>% 
  mutate(gender = recode(`Code sexe`, "M" = "Male", "F" = "Female")) %>% 
  count(`Libellé de la profession`, gender, sort = TRUE) %>% 
  filter(!is.na(`Libellé de la profession`)) %>%
  ungroup %>% 
  arrange(gender, n) %>% 
  filter(n > 1000) %>% 
  mutate(order = row_number()) %>% 
  mutate(occupation = fct_inorder(`Libellé de la profession`)) %>% 
  mutate(coord = if_else(n > 22000, n - 1000, n + 1000),
         colour = if_else(n > 22000, "white", "black")) %>% 
  ggplot(aes(x = order, y = n)) +
  geom_bar(aes(fill = gender), stat = "identity", width = 0.8) +
  scale_fill_discrete(guide = FALSE) + 
  scale_y_continuous(labels = scales::comma) +
  coord_flip() +
  geom_text(aes(label = occupation, y = coord, colour = colour), hjust = "inward", vjust = "center", size = 3) +
  scale_color_manual(values = c("black", "white"), guide = FALSE) +
  facet_wrap(facets = vars(gender), scales = "free_y") +
  xlab("") +
  ylab("") +
  ylim(0, NA) +
  scale_x_discrete(labels = NULL) +
  theme(axis.ticks.y = element_blank()) +
  theme_ipsum(grid = "X") +
  labs(title = "Most elected officials are employees, farmers or retired.", subtitle = "Number of elected officials in France in 2018 by occupation.", caption = "Source: RNE (Ministère de l'intérieur), computation by Sciences Po students.")
dev.off()

#DAY 2 - SPATIAL DATA

library(sf)
toilets <- read_sf("./sanisettesparis2011.geojson")
toilets

library(mapview)
mapview(toilets)

library(leaflet)

toilets %>%
  leaflet() %>% 
  addTiles() %>% 
  addCircleMarkers(radius = 2, color = "red")

library(tmap)
streets <- read_sf("./voie.geojson")
trees <- read_sf("./arbresremarquablesparis.geojson")

streets %>% 
  tm_shape() +
  tm_lines(alpha = 0.2) +
  tm_shape(toilets) +
  tm_dots(col = "red") +
  tm_shape(trees) +
  tm_dots(col = "darkgreen") +
  tm_scale_bar(position = c("left", "bottom")) +
  tm_compass(position = c("left", "top"))

#DAY 2 - APIs

library(httr)
library(sf)
library(mapview)
url <- "http://api-adresse.data.gouv.fr/search/"
query <- GET(url, query= list(q ="13 Rue de l'Université, Paris"))
status_code(query)
geojson <- content(query, as="text")
adresses <- read_sf(geojson)
mapview::mapview(adresses)

#Datathon



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
  
devtools::install_github("dkahle/ggmap") #installed github version in order to use Google API

api <- readLines("API.api")
register_google(key = "AIzaSyBi_RiXjH48o-83enpZgJOczG67WQu2S4k")


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

departements <- shapefile("./departements-20180101.shp")
coords <- geo[c("lon", "lat")]
coords <- coords[complete.cases(coords),]
sp <- SpatialPoints(coords)
plot(departements)
plot(sp, col="red", add=TRUE)

gg <- ggplot()
gg <- gg + geom_polygon(data=departements_fortify, aes(x=long, y=lat, group=group, fill=NA), color = "black", fill=NA, size=0.5) 
gg <- gg + geom_point(data=geo, aes(x=lon, y=lat, color="red"))
gg <- gg +  coord_map(xlim = c(-7, 12), ylim = c(52,41))
gg <- gg + labs(x=NULL, y=NULL, 
                title="Public and Associatives Maternités in France in 2016",
                subtitle=NULL,
                caption="Le Monde, www.data.gouv.fr")
gg <- gg + theme(plot.title=element_text(face="bold", family="roboto_condensed", size=13))
gg <- gg + theme(plot.caption=element_text(face="bold", family="roboto_condensed", size=7, color="gray", margin=margin(t=10, r=80)))
gg <- gg + theme(legend.position="none")
gg <- gg + theme(axis.line =  element_blank(),
                 axis.text =  element_blank(),
                 axis.ticks =  element_blank(),
                 panel.grid.major = element_blank(),
                 panel.grid.minor = element_blank(),
                 panel.border = element_blank(),
                 panel.background = element_blank()) 
print(gg)