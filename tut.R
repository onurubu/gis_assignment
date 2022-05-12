# Coskun Kucukkaragoz
# Tut to practice GIS coding, following Jasper's instructions in the GIS notes
#### Vectors ####
library(sf)
veg1 <- st_read("./data/veg/Vegetation_Indigenous.shp")
st_crs(veg1)
class(veg1)
head(veg1)
st_write(veg1, "./data/veg/Vegetation_Indigenous_duplicate.shp", append = FALSE)
file.exists("./data/veg/Vegetation_Indigenous_duplicate.shp")
st_drivers()
plot(veg1) # bad
plot(veg1[3]) # better...

#ggplot
library(tidyverse) #calls ggplot2 and other Tidyverse packages together
ggplot() +
  geom_sf(data=veg1, aes(fill = `National_`)) # need to crop

#Make a vector with desired coordinates in metres according to TM Lo19
ext <- c(-66642.18, -3809853.29, -44412.18, -3750723.29) 
ext
#Give the vector names
names(ext) <- c("xmin", "ymin", "xmax", "ymax") 
ext
veg1 <- st_crop(veg1, ext) #Note that I'm overwriting the old data object "veg1"
ggplot() + geom_sf(data=veg1, aes(fill = `National_`))

#Make a vector of the veg1 types we want
split_veg <- c("Peninsula Granite Fynbos - North", 
               "Peninsula Granite Fynbos - South", 
               "Cape Flats Dune Strandveld - West Coast", 
               "Cape Flats Dune Strandveld - False Bay")

#Use base R indexing to select attributes
vegsub <- veg1[which(veg1$National_ %in% split_veg),]

#Plot
ggplot() + geom_sf(data=vegsub, aes(fill = `National_`))

#Using tidyverse piping to filter and plot
veg1 %>% 
  filter(National_ %in% split_veg) %>%
  ggplot() +
  geom_sf(aes(fill = `National_`)) #The advantage being that you don't have to make the intermediate "vegsub" object

# No split
vegsub$National_ <- str_replace_all(vegsub$National_,
                                    c("Peninsula Granite Fynbos - North" = "Peninsula Granite Fynbos", 
                                      "Peninsula Granite Fynbos - South" = "Peninsula Granite Fynbos", 
                                      "Cape Flats Dune Strandveld - West Coast" = "Cape Flats Dune Strandveld", 
                                      "Cape Flats Dune Strandveld - False Bay" = "Cape Flats Dune Strandveld"))

ggplot() + geom_sf(data=vegsub, aes(fill = `National_`))

# Get rid of "fake boundaries"
vegsub %>% group_by(National_) %>% 
  summarize() %>% 
  ggplot() + geom_sf(aes(fill = National_))

#### iNaturalist Section

library(rinat)

#Call the data directly from iNat
pc <- get_inat_obs(taxon_name = "Protea cynaroides",
                   bounds = c(-35, 18, -33.5, 18.5),
                   maxresults = 1000)

#View the first few rows of data
head(pc)

#Filter returned observations by a range of column attribute criteria
pc <- pc %>% filter(positional_accuracy<46 & 
                      latitude<0 &
                      !is.na(latitude) &
                      captive_cultivated == "false" &
                      quality_grade == "research")

class(pc) # this is a dataframe with lat/long data, but it isn’t registered as an object with spatial attributes

#Make the dataframe a spatial object of class = "sf"
pc <- st_as_sf(pc, coords = c("longitude", "latitude"), crs = 4326)
#Note the new "geometry" column
names(pc)
#Plot
ggplot() + geom_sf(data=pc)

library(rosm)
library(ggspatial)

ggplot() + 
  annotation_map_tile(type = "osm", progress = "none") + 
  geom_sf(data=pc) # Note that there are quite a few base layer/tile options that can be set with type = "". Try rosm::osm.types() to see them all

# interactive map
library(leaflet)
library(htmltools)
leaflet() %>%
  # Add default OpenStreetMap map tiles
  addTiles(group = "Default") %>%  
  # Add our points
  addCircleMarkers(data = pc,
                   group = "Protea cynaroides",
                   radius = 3, 
                   color = "green") 

# want to get rid of remaining plants in peoples gardens
#Get the remnants layer
vegr <- st_read("./data/veg/Vegetation_Indigenous_Remnants.shp")
hmm <- st_intersection(pc, vegr) # Oops! The Coordinate Reference Systems are different! We will need to reproject one of the two datasets…

st_crs(pc)
st_crs(vegr)

# In this case, either CRS is fine for our purposes, but let’s stick with Transverse Mercator Lo19, because it’ll be useful later. For this we need to reproject the veg1 layer like so:
  pc <- st_transform(pc, st_crs(vegr)) 

#time to intersect
#call the dimensions of pc
  dim(pc)  

pc <- st_intersection(pc, vegr)
dim(pc)

ggplot() + 
  annotation_map_tile(type = "osm", progress = "none") + 
  geom_sf(data=pc)


## Make the plots/maps pretty
library(wesanderson)

pal <- wes_palette("Darjeeling1", 7, type = "continuous")

ggplot() + 
  annotation_map_tile(type = "osm", progress = "none") + 
  geom_sf(data=pc, aes(col = National_)) +
  scale_colour_manual(values = pal)

# all seem to be in peninsula sandstone fynbos...
pc %>% group_by(National_) %>% summarise(n())

# Note the numbers in column n(). But I can’t see where the Hangklip Sand Fynbos record is, so let’s label that one with text using geom_sf_label().
hsf <- pc %>% filter(National_ == "Hangklip Sand Fynbos") #find the locality

ggplot() + 
  annotation_map_tile(type = "osm", progress = "none") + 
  geom_sf(data=pc, aes(col = National_)) +
  scale_colour_manual(values = pal) +
  geom_sf_label(data=hsf, aes(label = "Here"))


## Buffering
#Find the localities that are not in Peninsula Sandstone Fynbos and add a 250m buffer
npsf <- pc %>% 
  filter(National_ != "Peninsula Sandstone Fynbos") %>%
  st_buffer(dist = 250)

#NOTE that st_buffer() makes them polygons, because they now have area!
npsf$geometry[1] #The first geometry in npsf
#Get the number of unique iNaturalist record numbers
length(unique(npsf$id)) 
#Intersect new polygons with veg1 remnants and filter for those that overlap Peninsula Sandstone Fynbos only
npsf <- st_intersection(npsf, vegr) %>% filter(National_.1 == "Peninsula Sandstone Fynbos")
#Get the number of unique iNaturalist record numbers that overlap PSF
length(unique(npsf$id))

###  Within distance and intersect

#Get the watercourse data
water <- st_read("./data/veg/Open_Watercourses.geojson")
#Check it's CRS
st_crs(water)

#Call the data directly from iNat
bs <- get_inat_obs(taxon_name = "Brabejum stellatifolium",
                   bounds = c(-35, 18, -33.5, 18.5),
                   maxresults = 1000)

#Filter returned observations by a range of attribute criteria
bs <- bs %>% filter(positional_accuracy<46 & 
                      latitude<0 &
                      !is.na(latitude) &
                      captive_cultivated == "false" &
                      quality_grade == "research")

#See how many records we got
nrow(bs)

#Make the dataframe a spatial object of class = "sf"
bs <- st_as_sf(bs, coords = c("longitude", "latitude"), crs = 4326) #Note that I had to define the CRS (as Geographic WGS84)!!!
#Crop the water courses to the extent of the locality data
water <- st_crop(water, bs)
#Plot
ggplot() + 
  annotation_map_tile(type = "osm", progress = "none") + 
  geom_sf(data = water, colour = "blue") +
  geom_sf(data=bs)

st_intersects(bs, water) %>% unlist()

bs <- st_transform(bs, st_crs(vegr))

water <-  st_transform(water, st_crs(vegr))

st_intersects(bs, water) %>% unlist() #lines and points do not have area so they dont intersect per se, have to buffer

st_is_within_distance(bs, water, 20) %>% unlist() %>% unique()
st_is_within_distance(water, bs, 20) %>% unlist() %>% unique()

st_is_within_distance(water, bs, 100) %>% unlist() %>% unique()




#### Rasters ####

library(terra)
dem <- rast("./data/elevation/10m_BA.tif")

class(dem)

dem #Typing the name of a "SpatRaster" class data object gives you the details
crs(dem)

# cropping
dem <- crop(dem, ext(c(-66642.18, -44412.18, -3809853.29, -3750723.29)))
dem

# aggreagate for larger (less fine) resolution
dem30 <- aggregate(dem, fact = 3, fun = mean)
dem30

# basic plotting
plot(dem30)
# call tidyverse libraries and plot
library(tidyverse)

dem30 %>% as.data.frame(xy = TRUE) %>%
  ggplot() +
  geom_raster(aes(x = x, y = y, fill = `10m_BA`))
# Note that I had to know that the column name for the elevation data is "10m_BA"... and that you need to use ` ` around a variable name when feeding it to a function if it starts with a digit.
dem %>%
  as.data.frame(xy = TRUE) %>%
  ggplot() +
  geom_raster(aes(x = x, y = y, fill = `10m_BA`))

# disaggregating
dem10 <- disagg(dem30, fact = 3, method = "bilinear")

### RASTER MATHS
dem10 <- crop(dem10, dem) #crop bigger one by smaller so that extent is equal

diff <- dem - dem10 #maths with rasters!
diff %>%
  as.data.frame(xy = TRUE) %>%
  ggplot() +
  geom_raster(aes(x = x, y = y, fill = `10m_BA`)) 

diff %>%
  as.data.frame(xy = TRUE) %>%
  ggplot() +
  geom_histogram(aes(`10m_BA`))

# Focal and terrain calculations
aspect <- terrain(dem30, "aspect", unit = "radians")

slope <- terrain(dem30, "slope", unit = "radians")

hillshade <- shade(slope, aspect)

plot(hillshade)

# probably prettier with tidyverse...
hillshade %>%
  as.data.frame(xy = TRUE) %>%
  ggplot() +
  geom_raster(aes(x = x, y = y, fill = lyr1)) + #note that the hillshade column name in this case is "lyr1"
  scale_fill_gradient(low = "grey10", high = "grey90")

# Raster stacks

dstack <- c(dem30, slope, aspect, hillshade)

dstack
names(dstack) <- c("elevation", "slope", "aspect", "shade")

# Extracting raster to vector
# get some species data

library(rinat)
library(sf)

#Call data for two species directly from iNat
pc <- get_inat_obs(taxon_name = "Protea cynaroides",
                   bounds = c(-35, 18, -33.5, 18.5),
                   maxresults = 1000)

ll <- get_inat_obs(taxon_name = "Leucadendron laureolum",
                   bounds = c(-35, 18, -33.5, 18.5),
                   maxresults = 1000)

#Combine the records into one dataframe
pc <- rbind(pc,ll)

#Filter returned observations by a range of attribute criteria
pc <- pc %>% filter(positional_accuracy<46 & 
                      latitude<0 &
                      !is.na(latitude) &
                      captive_cultivated == "false" &
                      quality_grade == "research")

#Make the dataframe a spatial object of class = "sf"
pc <- st_as_sf(pc, coords = c("longitude", "latitude"), crs = 4326)

#Set to the same projection as the elevation data
pc <- st_transform(pc, crs(dem30))

# Now let’s extract the data to the points.

# NOTE!!! terra doesn’t play nicely with sf objects at this stage, so you need to coerce them into terra’s own vector format using vect()

dat <- extract(dem30, vect(pc)) # note vect()

head(dat)

# Nice, but not all that handy on it’s own. Let’s add the elevation column to our points layer, so we can match it with the species names and plot.

pc$dem <- dat$`10m_BA`

pc %>% ggplot() +
  geom_boxplot(aes(scientific_name, dem))

# (Hmm… do you think those Leucadendron laureolum outliers that must be near Maclear’s Beacon could actually be Leucadendron strobilinum?)

# Ok, that’s handy, but what if we have data lots of rasters? We don’t want to have to do that for every raster! This is where raster stacks come into their own!

#extract from stack
dat <- extract(dstack, vect(pc)) 

#bind columns to points to match the names
edat <- cbind(as.data.frame(pc), dat)

#select columns we want and tidy data into long format
edat <- edat %>% 
  dplyr::select(scientific_name, elevation, slope, aspect, shade) %>% 
  pivot_longer(c(elevation, slope, aspect, shade))

#panel boxplot of the variables extracted
edat %>% ggplot() +
  geom_boxplot(aes(scientific_name, value)) +
  facet_wrap(~name, scales = "free") 

# Something I should have mentioned is that if you would like each point to sample a larger region you can add a buffer = argument to the extract() function, and a function (fun =) to summarize the neighbourhood of pixels sampled, like so:

pc$dem30 <- extract(dem30, vect(pc), buffer = 200, fun = mean)$X10m_BA #Note the sneaky use of $ to access the column I want

pc %>% ggplot() +
  geom_boxplot(aes(scientific_name, dem30))

# Now let’s try that with our vegetation polygons.
#Get historical vegetation layer
veg1 <- st_read("./data/veg/Vegetation_Indigenous.shp")
#Crop to same extent as DEM
veg1 <- st_crop(veg1, ext(dem30)) #Note that I just fed it the extent of the DEM

#Best to dissolve polygons first - otherwise you get repeat outputs for each polygon within each veg1 type
vegsum <- veg1 %>% group_by(National_) %>% 
  summarize()

#Do extraction - note the summary function
vegdem <- extract(dem30, vect(vegsum), fun = mean, na.rm = T)

#Combine the names and vector extracted means into a dataframe
vegdem <- cbind(vegdem, vegsum$National_)

#Rename the columns to something meaningful
names(vegdem) <- c("ID", "Mean elevation (m)", "Vegetation type")

#Plot
vegdem %>% ggplot() +
  geom_col(aes(y = `Mean elevation (m)`, x = `Vegetation type`)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# Rasterizing
# Rasterizing essentially means turning a vector layer into a raster. To rasterize, you need an existing raster grid to rasterize to, like dem30 in this case.

#Make the vegetation type a factor
vegsum$National_ <- as.factor(vegsum$National_)

#Rasterize
vegras <- rasterize(vect(vegsum), dem30, field = "National_")

#Plot
vegras %>% 
  as.data.frame(xy = TRUE) %>%
  ggplot() +
  geom_raster(aes(x = x, y = y, fill = National_))

# Visualizing multiple datasets on one map

ggplot() +
  geom_raster(data = as.data.frame(vegras, xy = TRUE),
              aes(x = x, y = y, fill = National_)) +
  geom_contour(data = as.data.frame(dem30, xy = TRUE), 
               aes(x = x, y = y, z = `10m_BA`), breaks = seq(0, 1100, 100), colour = "black") +
  geom_sf(data=pc, colour = "white", size = 0.5)

# Cloud Optimized GeoTiffs (COGs)!!!
# I thought I’d add this as a bonus section, reinforcing the value of standardized open metadata and file formats from the Data Management module.

# First, let’s open a connection to our COG, which is stored in the cloud. To do this, we need to pass a URL to the file’s online location to terra.
cog.url <- "/vsicurl/https://storage.googleapis.com/grootbos-tiff/grootbos_cog.tif"

grootbos <- rast(cog.url)

grootbos

# This has given us the metadata about the file, but has not read it into R’s memory. The file is ~1.8GB so it would do bad things if we tried to read the whole thing in…

# Now let’s retrieve a subset of the file. To do this we need to make a vector polygon for our region of interest, like so:
roi <- vect(data.frame(lon = c(19.433975, 19.436451),
                       lat = c(-34.522733, -34.520735)),
            crs = "epsg:4326")

# And transform it to the same projection as the COG:
roi <- project(roi, crs(grootbos))

# And then extract our ROI
roi_ras <- crop(grootbos, roi)

roi_ras

# Now we have a raster with 3 layers in memory. There are Red Green and Blue, so we should be able to plot them, like so:
plotRGB(roi_ras)

