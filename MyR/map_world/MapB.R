library(geosphere)
library(maps)
library(leaflet)
library(htmlwidgets)
library(htmltools)
library(dplyr)
library(ggmap)
library(ggrepel)
library(Leaflet.markercluster)
#сделать чтобы мышкой когда навадился показывала координаты
# группировка по странам (изминить бд получаеться)
library(ggplot2)
library(plotly)
library(gapminder)
library(streamgraph)
#cartography
##############################################
df <- read.csv2("C:/Users/Obito/Desktop/Projects/MyR/map_world/coor.txt",
                sep = "\t", dec = ".", header = TRUE)

m <- leaflet() %>% 
  addTiles() %>% 
  setView( lng = 2.34, lat = 48.85, zoom = 4 ) %>% 
  addProviderTiles("NASAGIBS.ViirsEarthAtNight2012", group="Стиль 1") %>%
  addTiles(options = providerTileOptions(noWrap = TRUE), group="background 2") %>%
  
  addCircleMarkers(data=df, lng=df$Long , lat=df$Lat, radius=8 , color="black",
                   fillColor="red", stroke = TRUE, fillOpacity = 0.8, group="Red") %>%
  
  addLayersControl(overlayGroups ="Red" , baseGroups = c("Стиль 1","Стиль 2"), 
                   options = layersControlOptions(collapsed = FALSE))
m

#а что если я HTML GAY ?
df <- read.csv2("C:/Users/Obito/Desktop/Projects/MyR/map_world/coor.txt",
                sep = "\t", dec = ".", header = TRUE)

leaflet(df) %>% addTiles() %>%
  addMarkers(~Long, ~Lat, label = ~htmlEscape("number"))
#######################3
map <- leaflet() %>% addTiles() %>%
  # leaflet(quakes) %>% addTiles() %>% addMarkers(
  #   clusterOptions = markerClusterOptions()) %>%
  addMarkers(lat=47.267, lng=11.400, popup = "1\nСтрана: Россия") %>%
  addMarkers(lat=47.050, lng=12.9500, popup = "2") %>%
  addMarkers(lat=48.233, lng=16.350, popup = "3") %>%
  addMarkers(lat=56.767, lng=8.317, popup = "4") %>%
  addMarkers(lat=48.823, lng=2.337, popup = "5") %>%
  addMarkers(lat=49.883, lng=10.883, popup = "6") %>%
  addMarkers(lat=47.800, lng=11.017, popup = "7") %>%
  addMarkers(lat=52.383, lng=13.067, popup = "8") %>%
  addMarkers(lat=53.371, lng=5.217, popup = "9") %>%
  addMarkers(lat=53.185, lng=6.601, popup = "10") %>%
  addMarkers(lat=52.879, lng=7.061, popup = "11") %>%
  addMarkers(lat=52.645, lng=5.068, popup = "12") %>%
  addMarkers(lat=52.396, lng=6.051, popup = "13") %>%
  addMarkers(lat=52.311, lng=4.704, popup = "14") %>%
  addMarkers(lat=51.983, lng=6.701, popup = "15") %>%
  addMarkers(lat=51.677, lng=3.865, popup = "16") %>%
  addMarkers(lat=51.569, lng=4.531, popup = "17") %>%
  addMarkers(lat=51.182, lng=5.967, popup = "18") %>%
  addMarkers(lat=59.117, lng=11.383, popup = "19") %>%
  addMarkers(lat=60.650, lng=6.217, popup = "20") %>%
  addMarkers(lat=47.250, lng=9.350, popup = "21") %>%
  addMarkers(lat=47.733, lng=10.317, popup = "22")
map
map %>% addProviderTiles(providers$Stamen.Toner)
######################################################################################
                                        #Final

MapB <- leaflet(df) %>% 
  addTiles() %>% 
  setView( lng = 2.34, lat = 48.85, zoom = 4 ) %>% 
  addMarkers(~Long, ~Lat) %>%
  addProviderTiles("Esri.DeLorme") %>%
  addProviderTiles("NASAGIBS.ViirsEarthAtNight2012") %>%
  addProviderTiles("Stadia.AlidadeSmoothDark") %>%
  addProviderTiles("OpenTopoMap")  %>%
  
##
  addProviderTiles(providers$MtbMap) %>%
  addProviderTiles(providers$Stamen.TonerLines,
                   options = providerTileOptions(opacity = 0.35)) %>%
  addProviderTiles(providers$Stamen.TonerLabels)

  
##
  
  addTiles(options = providerTileOptions(noWrap = TRUE), group="background") %>%
  
  addLayersControl(overlayGroups ="Red" , baseGroups = c("Стиль 1","Стиль 2","Стиль 3", "Стиль 4"), 
                 options = layersControlOptions(collapsed = FALSE)) %>%
  addLayersControl(overlayGroups ="Red" , baseGroups = c("Стиль 1","Стиль 2"), 
                   options = layersControlOptions(collapsed = FALSE))

MapB




#_________________________________________________________________________________________
#                                  Неудачные попытки )))

#________________________________Создаем карту с красивым задним фоном____________
# Background 1: NASA
# m <- leaflet() %>%
#   addTiles() %>%
#   setView( lng = 2.34, lat = 48.85, zoom = 4 ) %>%
#   addProviderTiles("NASAGIBS.ViirsEarthAtNight2012")
# m
# 
# # Background 2: World Imagery
# m <- leaflet() %>%
#   addTiles() %>%
#   setView( lng = 2.34, lat = 48.85, zoom = 4 ) %>%
#   addProviderTiles("Esri.WorldImagery")
# m
# # Background 3: Dark
# map <- leaflet() %>%
#   addTiles() %>%
#   setView( lng = 2.34, lat = 48.85, zoom = 4 ) %>%
#   addProviderTiles("BasemapAT")
# map
# #______________________________________Вывожу точки да _________
# # No margin
# par(mar=c(0,0,0,0))
# #Background map
# map('world',
#     col="#f2f2f2", fill=TRUE, bg="white", lwd=0.05,
#     mar=rep(0,4),border=0, ylim=c(-80,80)
# )
# map <- leaflet() %>% 
#   addTiles() %>% 
#   setView( lng = 2.34, lat = 48.85, zoom = 4 ) %>% 
#   addProviderTiles("Stadia.AlidadeSmoothDark")
# 
# #Обозначаю точками местоположение станций
# points(x=df$SH, y=df$DL, col="slateblue", cex=0.3, pch=20)
# 
# # Make data with several positions
# data_red <- data.frame(LONG=42+rnorm(10), LAT=23+rnorm(10), PLACE=paste("Red_place_",seq(1,10)))
# data_blue <- data.frame(LONG=42+rnorm(10), LAT=23+rnorm(10), PLACE=paste("Blue_place_",seq(1,10)))
# 
# # Initialize the leaflet map:
# m <- leaflet() %>% 
#   setView(lng=42, lat=23, zoom=6 ) %>%
#   
#   # Add two tiles
#   addProviderTiles("Esri.WorldImagery", group="background 1") %>%
#   addTiles(options = providerTileOptions(noWrap = TRUE), group="background 2") %>%
#   
#   # Add 2 marker groups
#   addCircleMarkers(data=data_red, lng=~LONG , lat=~LAT, radius=8 , color="black",
#                    fillColor="red", stroke = TRUE, fillOpacity = 0.8, group="Red") %>%
#   addCircleMarkers(data=data_blue, lng=~LONG , lat=~LAT, radius=8 , color="black",
#                    fillColor="blue", stroke = TRUE, fillOpacity = 0.8, group="Blue") %>%
#   
#   # Add the control widget
#   addLayersControl(overlayGroups = c("Red","Blue") , baseGroups = c("background 1","background 2"), 
#                    options = layersControlOptions(collapsed = FALSE))
# 
# m
#####################################################################3
# library(streamgraph)
# 
# # Create data:
# data <- data.frame(
#   year=rep(seq(1990,2016) , each=10),
#   name=rep(letters[1:10] , 27),
#   value=sample( seq(0,1,0.0001) , 270)
# )
# 
# # Stream graph with a legend
# pp <- streamgraph(data, key="name", value="value", date="year", height="300px", width="1000px") %>%
#   sg_legend(show=TRUE, label="names: ")
# 
# 
# # save the widget
# # library(htmlwidgets)
# # saveWidget(pp, file=paste0( getwd(), "/HtmlWidget/streamgraphDropdown.html"))