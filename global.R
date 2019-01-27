library(shiny)
library(dplyr)
library(ggplot2)
library(rsconnect)
library(maps)
library(stringr)
library(RColorBrewer)
library(reshape2)
library(plotly)
library(mapproj)
library(rgdal)
library(rgeos)
library(leaflet)



data <- read.csv(file = "Lycee_Data.csv",encoding = "UTF-8")
a = data %>% select(.,Departement,Groupe,Taux_L,Taux_ES,Taux_S,Taux_Tech,Taux_Total,nb_L)
h = a %>% group_by(.,Departement)
c = summarise(h,L = round(mean(Taux_L),1),ES = round(mean(Taux_ES),1),S = round(mean(Taux_S),1),total = round(mean(Taux_Total),1))
length(c$total)



#########Code for bar charts comparing technical vs non-technical
#########
e = data %>% select(.,Departement,Groupe,Taux_L,Taux_ES,Taux_S,Taux_Tech,Taux_Total,nb_L)
f = e %>% group_by(.,Groupe)
summarise(f,L = mean(Taux_L),ES = mean(Taux_ES),S = mean(Taux_S),total = mean(Taux_Total))
data.m = melt(data,id.vars = "Groupe", measure.vars = c("Taux_S","Taux_ES","Taux_L","Taux_Total"))

box_group = ggplot(data.m) + geom_boxplot(aes(x=Groupe,y=value,color=variable)) +
  scale_y_continuous(name = "Passing %",
                     breaks = seq(60, 100, 2),
                     limits=c(60, 100)) +
  labs(x = "", 
       y = "", 
       title = "Passing Rates for Schools Offering a Tech Diploma")

box_group

############# passing rates for all
############
data.m2 = melt(data,id.vars = "Total", measure.vars = c("Taux_S","Taux_ES","Taux_L","Taux_Total"))
box_group2 = ggplot(data.m2) + geom_boxplot(aes(x=Total,y=value,color=variable)) +
  scale_y_continuous(name = "Passing %",
                     breaks = seq(60, 100, 2),
                     limits=c(60, 100)) +
  labs(x = "", 
       y = "", 
       title = "Passing Rates for all Schools Confounded")

box_group2

###### secteur prive vs publique
##########
data.m1 = melt(data,id.vars = "Secteur", measure.vars = c("Taux_S","Taux_ES","Taux_L","Taux_Total"))
box_group1 = ggplot(data.m1) + geom_boxplot(aes(x=Secteur,y=value,color=variable)) +
  scale_y_continuous(name = "Passing %",
                     breaks = seq(60, 100, 2),
                     limits=c(60, 100)) +
  labs(x = "", 
       y = "", 
       title = "Passing Rates for Private & Public Schools")

box_group1

##########attendance by filiere
#########


sums = data.frame("Section" = c("S","ES","L","Tech"), "Nb_of_Students" = c(sum(data$nb_S),sum(data$nb_ES),sum(data$nb_L),sum(data$nb_Tech)))


p<-ggplot(data=sums, aes(x=sums$Section, y=sums$Nb_of_Students)) +
  geom_bar(stat="identity") + 
  scale_y_continuous(name = "Number of students",
                     breaks = seq(0, 170000, 10000),
                     limits=c(0, 170000)) +
  labs(x = "", 
       y = "", 
       title = "Student Division")
p
head(data,5)

###### try at a divided plot
######


L = unique(data$Departement)

table1 = data %>% select(Etablissement,nb_L,nb_ES,nb_S,nb_Total,Taux_L,Taux_ES,Taux_S,Taux_Total)
library(DT)
library(ggplot2)
require("maptools")

library(maptools)

library(rgdal)
df_departement <- readOGR(
  dsn = "shapefiles/departements-20140306-100m-shp", 
  layer = "departements-20140306-100m", stringsAsFactors = FALSE
)
spdf_departement <- readOGR(
  dsn = "shapefiles/departements-20140306-100m-shp", 
  layer = "departements-20140306-100m", stringsAsFactors = FALSE
)

df_departement <- ggplot2::fortify(spdf_departement, region = "code_insee")
df_departement %>% 
  filter(str_length(id) == 2)


c$Departement

bins <- c(0,85,86,87,89,90,91,92,93,94,95,96,97,98,100)
pal1 <- colorBin("RdYlGn", domain = c$total, bins = bins)


aa = c('a','b','c','d','e','Alpes-Maritimes','Ardeche','Ardennes','Ariege','Aube','Aude','Aveyron','Bouches du Rhone','Calvados','Cantal','Charente','Charente Maritime','Cher','Correze',"Cote d'Or","Cotes d'Armor",'Creuse','Dordogne','Doubs','Drome','Eure','Eure-et-Loir','Finistere','Corse du Sud','Haute-Corse','Gard','Haute-Garonne','Gers','Gironde','Herault','Ille-et-Vilaine','Indre','Indre-et-Loire','Isere','Jura','Landes','Loir-et-Cher','Loire','Haute-Loire','Loire-Atlantique','Loiret','Lot','Lot-et-Garonne','Lozere','Maine-et-Loire','Manche','Marne','Haute-Marne','Mayenne','Meurthe-et-Moselle','Meuse','Morbihan','Moselle','Nievre','Nord','Oise','Orne','Pas-de-Calais','Puy-de-Dome','Pyrenees-Atlantiques','Hautes-Pyrenees','Pyrenees-Orientales','Bas-Rhin','Haut-Rhin','Rhone','Haute-Saone','Saone-et-Loire','Sarthe','Savoie','Haute-Savoie','Paris','Seine-Maritime','Seine-et-Marne','Yvelines','Deux-Sevres','Somme','Tarn','Tarn-et-Garonne','Var','Vaucluse','Vendee','Vienne','Haute-Vienne','Vosges','Yonne','Territoire-de-Belfort','Essonne','Hauts-de-Seine','Seine-St-Denis','Val-de-Marne',"Val-d'Oise")    
bb = c('Ain','Aisne','Allier','Alpes de Haute-Provence','Hautes-Alpes')
cc = c(aa,bb)
labels <- sprintf(
  "<strong>%s</strong><br/>%g %% passing rate",
  cc, c$total
) %>% lapply(htmltools::HTML)
L1 = c("L","ES","S","total")

library(leaflet)
library(stringr)
library(magrittr)
map_1 = spdf_departement %>% 
  subset(. , str_length(code_insee) == 2) %>% 
  leaflet() %>% 
  addProviderTiles("Esri.WorldStreetMap") %>%
  addPolygons(fillColor = ~pal1(c$total),
    weight = 2,
    opacity = 1,
    color = "white",
    dashArray = "5",
    fillOpacity = 0.7,
    highlight = highlightOptions(
      weight = 5,
      color = "#666",
      dashArray = "",
      fillOpacity = 0.7,
      bringToFront = TRUE),
    label = labels,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px",
      direction = "auto")) %>% addLegend(pal = pal1, values = ~density, opacity = 0.7,
            position = "bottomright",title="Total passing rate %")

