# Coskun Kucukkaragoz
rm(list=ls())
# Tut to practice GIS coding, following Jasper's instructions in the GIS notes
#### Vectors ####
library(sf)
library(osmdata)
library(tidyverse) #calls ggplot2 and other Tidyverse packages together
# interactive map
library(leaflet)
library(htmltools)

site <- st_read("./gis_files/temnospondyl_tracksite.kmz")
veg <- st_read("./gis_files/vegetation/vegetation.shp")


factpal <- colorFactor(topo.colors(10), domain =  veg$BIOREGION_)
leaflet() %>%
  addTiles(group = "OSM") %>%
  #addProviderTiles("Esri.NatGeoWorldMap", group="ESRI") %>%
  # addProviderTiles("CartoDB.DarkMatter", group= "CartoDB") %>%
  addPolygons(data = veg, group = "BIOREGION_",stroke = F,smoothFactor = 0.2, color = ~factpal(BIOREGION_)) %>%
  addLegend("bottomright", pal = factpal, values = veg$BIOREGION_, title = "Bioregions", group = "BIOREGION_") %>%
  addCircleMarkers(data = site, radius = 3, color = "green")
