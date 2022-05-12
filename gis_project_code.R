# Coskun Kucukkaragoz
## Code for the assignment of the GIS module for BIO4000W

# rm(list=ls())

library(sf) # spatial package 
library(leaflet) # interactive map


## Reading in data, the lines for the cropping and reprojection of the vegetation dataset are included but commented out. The large file size of the original vegetation map made it necessary to save the cropped version instead for the repository

# veg <- st_read("./gis_files/VEGMAP2018_AEA_16082019Final/NVM2018_AEA_V22_7_16082019_final.shp")
# aoi <- st_read("./gis_files/area_of_interest/aoi.shp")
site <- st_read("./gis_files/temnospondyl_tracksite.kmz")
veg <- st_read("./gis_files/vegetation/vegetation.shp")


# Cropping and reprojection of vegetation map -----------------------------

# ## Cropping the vegetation map
# veg <- st_crop(veg, aoi)
# # changing the CRS of the vegetation map to match the track site
# veg <- st_transform(veg, st_crs(site))
# # writing the new cropped and projected vegetation map to 
# st_write(veg, "./gis_files/vegetation/vegetation.shp", append = FALSE)

# End of cropping and reprojection ----------------------------------------


# Interactive Map ---------------------------------------------------------

factpal <- colorFactor(topo.colors(10), domain =  veg$BIOREGION_) # create palette for bioregions

# Create the interactive map
leaflet() %>%
  addTiles(group = "OSM") %>% # add base layer
  addPolygons(data = veg, group = "BIOREGION_", stroke = F,smoothFactor = 0.2, color = ~factpal(BIOREGION_)) %>% # vegetation map
  addLegend("bottomright", pal = factpal, values = veg$BIOREGION_, title = "Bioregions", group = "BIOREGION_") %>% #legend
  addCircleMarkers(data = site, radius = 3, color = "green", fillOpacity=1) %>% # track site
  addScaleBar("bottomleft") # scale bar
