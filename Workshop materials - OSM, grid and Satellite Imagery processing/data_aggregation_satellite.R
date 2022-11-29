######################################################################################
#                                                                                    #
#       SEA 2022 workshop                                                            #
#       Data-to-grid aggregation and basics of satellite imagery processing          #
#       Ewa Dobrowolska                                                              #
#       22.06.2022                                                                   #
#                                                                                    #
######################################################################################

# Loading the packages

# install.packages("sf")
library(sp)
library(sf)
library(osmdata)
library(dplyr)
library(ggplot2)
library(rgdal)

# Setting the working directory
getwd()
setwd(" ... ")

# Reading the data
grid<-readOGR(".", "grid_waw")
grid.sf<-st_as_sf(grid)
grid.sf <- grid.sf %>% st_transform(crs = "+proj=longlat +datum=NAD83")

### Part I: Data-to-grid aggregation ###
df<-data.frame(ID=1:nrow(grid.sf)) # the dataset we will be building

# We will be using OpenStreetMaps to access spatial data in Warsaw and aggregate it in the desired form. 
# Thanks to the osmdata package, the OSM data can be downloaded directly into R (there's no need to visit the website).
# The types of OSM data are:
# -> osm_points (point data)
# -> osm_lines 
# -> osm_polygons 
# -> osm_multilines (sets of linestrings)
# -> osm_multipolygons (sets of polygons)
# OSM data are sf objects.
# To download OSM data into R, you need to know the "key" and the corresponding "value" of your desired feature. You can find all 
# the available features, their keys and values here: https://wiki.openstreetmap.org/wiki/Map_features . You can also use R functions:
# available_features() - displays the "keys"
# available_tags("key") - displays all the possible "values" of the "key" passed on as a function argument. (for example: available_tags("amenity"),
# available_tags("shop")). You will need internet access to use these functions.

# 1. Aggregating points
# The code below downloads data concerning location of restaurants in Warsaw (point data) and calculates how many restaurants
# (how many points) are located within each cell.

bbox<-st_bbox(grid.sf)

restaurants.sf = opq(bbox) %>%
  add_osm_feature(key="amenity", value = "restaurant") %>%
  osmdata_sf()

restaurants.sf # 2568 points, 95 polygons

restaurants.sf$osm_points <- restaurants.sf$osm_points %>% st_transform(crs = "+proj=longlat +datum=NAD83") #we need to change crs
points<-restaurants.sf$osm_points

within_list<-st_within(points, grid.sf)  
View(within_list) # list of 2568 (we had 2568 points, one for each restaurant)
                  # The "value" column specifies in which grid cell the given point lies

within_unlisted<-within_list %>% unlist() 
within_unlisted   # A vector of length 2266: some points were located outside grid and here are omitted

table<- within_unlisted %>% table()
table # How many points are there in each grid cell?
grid.cells<-names(table)
class(grid.cells) #character!
grid.cells<-as.numeric(grid.cells)
class(grid.cells)

agg<-data.frame(ID=grid.cells,  restaurants=as.numeric(table))
head(agg)

df<-merge(df, agg, by="ID",  all.x=TRUE, sort=TRUE)
summary(df) #there are NA's!

df$restaurants[is.na(df$restaurants)]<-0
summary(df) #Now it's better

# Let's see how it looks on a map
grid.sf$restaurants<-df$restaurants

ggplot()+
  geom_sf(grid.sf, mapping=aes(geometry=geometry, fill=restaurants))+ # plots grid
  geom_sf(points, mapping=aes(geometry=geometry), col="red", size=0.5)+ # plots restaurants as points
  scale_fill_viridis_c()+ # nicer colors
  theme_minimal()

# What if we wanted to know if in a given cell there's a restaurant, and not how many there are exactly?

df$restaurants_b<-ifelse(df$restaurants>0, 1, 0)
grid.sf$restaurants_b<-df$restaurants_b

ggplot()+
  geom_sf(grid.sf, mapping=aes(geometry=geometry, fill=restaurants_b))+ # plots grid
  geom_sf(points, mapping=aes(geometry=geometry), col="red", size=0.5)+ # plots restaurants as points
  scale_fill_viridis_c()+ # nicer colors
  theme_minimal()

# both plots above look good.

# Sometimes OSM data is not reported correctly - one point not always represents one establishment. For example, one big restaurant
# could be coded as a group of points instead of one polygon (or multipolygon). How can we avoid such errors?
# We could calculate only points, that are located outside OSM polygons, and later calculate number of polygons separately.

# in this case, be don't have multipolygons, so we will focus on restaurants.

union<-st_union(restaurants.sf$osm_polygons) 
union # one multipolygon made of 966 polygons

union <- union %>% st_transform(crs = "+proj=longlat +datum=NAD83")
p_list<-!lengths(st_intersects(points, union))
p_list # a vector of length 2568 - for each point (we had 2568 of them) it indicates whether it lies outside the "union" multipolygon.
    
points_outside<-points[p_list,]
points_inside<-points[!p_list,]

nrow(points_outside)
nrow(points_inside) # 624 points lie inside polygons!

ggplot()+
  geom_sf(grid.sf, mapping=aes(geometry=geometry, fill=restaurants))+ # plots grid
  geom_sf(restaurants.sf$osm_polygons, mapping=aes(geometry=geometry), size=2, fill="gold")+ # plots restaurants as points
  geom_sf(points_inside, mapping=aes(geometry=geometry), col="red", size=0.5)+ # plots restaurants as points
  geom_sf(points_outside, mapping=aes(geometry=geometry), col="green", size=0.5)+ # plots restaurants as points
  scale_fill_viridis_c()+ # nicer colors
  theme_minimal()

# the rest of the code is the same

within_list<-st_within(points_outside, grid.sf)  
View(within_list) 

within_unlisted<-within_list %>% unlist() 
within_unlisted   # A vector of length 1794

table<- within_unlisted %>% table()
table # How many points are there in each grid cell?
grid.cells<-names(table)
class(grid.cells) #character!
grid.cells<-as.numeric(grid.cells)
class(grid.cells)

agg<-data.frame(ID=grid.cells,  restaurants_out=as.numeric(table))

df<-merge(df, agg, by="ID",  all.x=TRUE, sort=TRUE)
summary(df) #there are NA's!

df$restaurants_out[is.na(df$restaurants_out)]<-0
summary(df) # Difference: Max. 158 vs Max. 131! 

# 2. Calculating total area of polygons intersecting grid cells
# The code below downloads data concerning location of parks in Warsaw (polygons) and for each cell it calculates it's area 
# covered with parks (in m^2)

parks.sf = opq(bbox) %>%
  add_osm_feature(key="leisure", value = "park") %>%
  osmdata_sf()

parks.sf # 15462 points, 5 linestrings, 555 polygons, 12 multipolygons
poly<-parks.sf$osm_polygons
poly <- poly %>% st_transform(crs = "+proj=longlat +datum=NAD83")
poly

id<-st_intersects(poly, grid.sf)
View(id) # Similarly, we have a list of 555 (we had 555 polygons), and for each of them the IDs of grid cells that intersect with it
id<-unlist(id)
intersection<-st_intersection(grid.sf, poly) 
intersection # sf object

area<-st_area(intersection) 
head(area) # vector of areas - in m^2

id_area<-data.frame(ID=id, parks=area)
head(id_area)

agg<-aggregate(id_area$parks, by=list(id_area$ID), FUN="sum")
colnames(agg)<-c("ID", "parks")
agg$parks<-agg$parks %>% as.numeric()
head(agg)

df<-merge(df, agg, by="ID",  all.x=TRUE, sort=TRUE)
summary(df) # NA's!

df$parks[is.na(df$parks)]<-0
summary(df) # better

# Let's see how it looks on a map

grid.sf$parks<-df$parks

ggplot()+
  geom_sf(grid.sf, mapping=aes(geometry=geometry, fill=parks))+ # plots grid
  geom_sf(poly, mapping=aes(geometry=geometry), fill="forestgreen")+ # plots parks as polygons
  scale_fill_viridis_c()+ # nicer colors
  theme_minimal()

# Dummy variable version 

df$parks_b<-ifelse(df$parks>0, 1, 0)
grid.sf$parks_b<-df$parks_b

ggplot()+
  geom_sf(grid.sf, mapping=aes(geometry=geometry, fill=parks_b))+ # plots grid
  geom_sf(poly, mapping=aes(geometry=geometry), fill="forestgreen")+ # plots parks as polygons
  scale_fill_viridis_c()+ # nicer colors
  theme_minimal()

# 3. Calculating the total length of lines intersecting grid cells

metro.sf = opq(bbox) %>%
  add_osm_feature(key="railway", value = "subway") %>%
  osmdata_sf()

metro.sf # 1158  points, 152  linestrings

lines<-metro.sf$osm_lines
lines <- lines %>% st_transform(crs = "+proj=longlat +datum=NAD83")

id<-st_intersects(lines, grid.sf) %>% unlist()
intersection<-st_intersection(lines, grid.sf) 
length<- st_length(intersection)
head(length) # in meters

id_length<-data.frame(ID=id, metro=length)
head(id_length)

agg<-aggregate(id_length$metro, by=list(id_length$ID), FUN="sum")
colnames(agg)<-c("ID", "metro")
head(agg) 
class(agg$metro) # units

agg$metro<-agg$metro %>% as.numeric()
head(agg)

df<-merge(df, agg, by="ID",  all.x=TRUE, sort=TRUE)
summary(df) # NA's!
df$metro[is.na(df$metro)]<-0
summary(df) # better

# Let's see how it looks on a plot
grid.sf$metro<-df$metro

ggplot()+
  geom_sf(grid.sf, mapping=aes(geometry=geometry, fill=metro))+ # plots grid
  geom_sf(lines, mapping=aes(geometry=geometry), col="red")+ # plots parks as polygons
  scale_fill_viridis_c()+ # nicer colors
  theme_minimal()

# Dummy variable version:
df$metro_b<-ifelse(df$metro>0, 1, 0)
grid.sf$metro_b<-df$metro_b

ggplot()+
  geom_sf(grid.sf, mapping=aes(geometry=geometry, fill=metro_b))+ # plots grid
  geom_sf(lines, mapping=aes(geometry=geometry), col="red")+ # plots parks as polygons
  scale_fill_viridis_c()+ # nicer colors
  theme_minimal()

# Excercises

# Ex. 1.: in class materials, you will find grid for Wroclaw, another major Polish city. Read the data into R, download data 
# concerning location of schools (hint: key="amenity", value="school") and build a data frame with ID of grid cells, number
# of schools in each grid cell (number of points) and number of points that lie outside polygons. How big is the difference?
# Show the results on a plot.

# Ex. 2.: Add another column to the data frame from exercise 1. This time, calculate the area of meadows in each grid cell
# (hint: key="landuse", value="meadow"). Show the results on a plot.

# Ex. 3.: Go to the website https://wiki.openstreetmap.org/wiki/Map_features, find a feature that is likely to consist of osm_lines (for 
# example roads), use the corresponding key and value to download the data into R. Calculate the total length of lines in grid cells. 
# (Hint: don't choose major roads, bicycle lanes etc. Too many lines will significantly increase computation time). Show the results on a plot.
# Save the data frame from exercises 1-3 in a single csv file (or in a shapefile).




### Part II: Satellite imagery processing ###

# The Copernicus Sentinel-2 mission is a project of the European Space Agency. It consists of two satellites, which collect spectral
# data in 12 bands (for details on bands, see: https://gisgeography.com/sentinel-2-bands-combinations/). The possible applications
# include monitoring land use changes, glaciers, lava, etc. It's also useful in agriculture and archaeology.
# To download Sentinel-2 data, go to the website https://scihub.copernicus.eu/dhus/#/home, create an account and log in. 
# Pick your Area of Interest (AoI), insert your search criteria (optionally) and click the "search" button. Choose a file from
# the list, and download it. In the downloaded .zip folder, find files with extensions ".jp2" (...-> GRANULE ->...-> IMG_DATA ->...) 
# and move them all to the working directory.
# The .jp2 files end with "_B01", "_B02", "_B03", ... , "_B12". It means: "Band number X" (12 bands - 12 ".jp2" files). Certain band
# combinations are useful in plotting (or calculating) different measures/indices. To simply plot a satellite image of the region,
# plot B4/B3/B2, all of them at once (B4 is "Red", B3 is "Green", B2 is "Blue" - you will get a standard RGB image).

# In R, satellite images are raster objects. Rasters are the data stored as pixels. Each pixel represents an area on Earth (usually
# small area. For example, in the code below each pixel covers 10m x 10m of surface). It can be imagined as a huge chessboard of
# high resolution, where each square stores a value (a measurement that the satellite has collected).
# A Satellite usually collects more than 1 measurement for each pixel. For example, 12 measurements would result in 12 "maps of pixels". 
# In R you can store all of them at once in one object called "RasterBrick" or "RasterStack". The difference is that RasterStack is 
# just a set of single RasterLayers - the Layers can cover different regions, be of different size, come from different files, etc.
# RasterBrick must come from a single file. When possible, it's usually better to use RasterBrick, as it is more efficient.

# Each RasterLayer has the following attributes: 
# -> Dimentions (nrow, ncol - number of rows and columns that form "the chessboard"; ncell - the total number of cells/pixels),
# -> Resolution (how big is the area that each pixel covers? In our case, 10 10 indicates that 1 pixel = 10m x 10m)
# -> Extent (the extent of the "map" of pixels - minimum and maximum coordinates)
# -> crs (the coordinate reference system)
# And in the RasterStack/RasterBrick:
# -> nlayers (number of RasterLayers)

# We will need a new package to handle raster objects - the package "raster".

# loading packages
library(raster)
library(tabularaster) 

# read data into R 

pow.sf <- st_read("powiaty.shp") # 380 poviats in Poland
pow.sf <- st_transform(pow.sf, crs = "+proj=longlat +datum=NAD83")
waw.pow.sf<-pow.sf[pow.sf$jpt_nazwa_=='powiat Warszawa',] # We want only Warsaw

s=stack("T34UDC_20220313T095031_B02.jp2","T34UDC_20220313T095031_B03.jp2","T34UDC_20220313T095031_B04.jp2","T34UDC_20220313T095031_B08.jp2") 
s
class(s) # "RasterStack", "raster"

# s is a raster of 10980x10980=120560400 cells, and 4 layers (we wanted only four. In the downloaded zip file there were 12!)
s[[1]] # For example, here we have only one layer.
class(s[[1]]) # "RasterLayer", "raster"

plot(s) # Plots all the layers at once
plot(s[[1]]) # Plots just the first layer

# Simple RGB image
plotRGB(s,r=3,g=2,b=1, stretch = "lin")

# Satellite image processing

waw.pow.sf2<-st_transform(waw.pow.sf, crs(s)@projargs) # we change crs: the new crs is crs of "s" object
waw.sat.s<-crop(s, extent(waw.pow.sf2)) # It "cuts" the raster into the size of Warsaw
waw.sat.s # RasterBrick, less pixels than before
plot(waw.sat.s)
plotRGB(waw.sat.s, r=3,g=2,b=1, stretch = "lin") # the image of Warsaw

waw_cut<-mask(waw.sat.s, mask=waw.pow.sf2)
waw_cut 
plotRGB(waw_cut, r=3, g=2, b=1, stretch = "lin") # the image of Warsaw, but now we see the city borders (it's been cut into the right shape)


ndvi<-(waw_cut[[4]]-waw_cut[[3]])/(waw_cut[[4]]+waw_cut[[3]]) # The formula for calculating NDVI. Each layer stores numbers!
plot(ndvi) # Image of NDVI values in Warsaw

ndvi.crds<-spTransform(xyFromCell(ndvi, spatial=TRUE, 1:ncell(waw.sat.s)), CRS("+proj=longlat +datum=NAD83"))
crds<-coordinates(ndvi.crds)

class(crds)
dim(crds)
head(crds) # it looks nice, but we want a data frame

crds.df<-data.frame(X=crds[,1], Y=crds[,2])
class(crds.df)
dim(crds.df)
head(crds.df)
ndvi.wart<-as.data.frame(ndvi)
head(ndvi.wart)

crds.df$NDVI<-ndvi.wart[,1]
head(crds.df)

summary(crds.df)
crds.df[100000:100010,] # it works!

# We can plot it but it takes a lot of time - there are 5591900 points to be plotted!
# ggplot(crds.df, aes(x=X, y=Y, col=NDVI))+
#   geom_point()

# We can get rid of points with the NA value of NDVI:
crds.df<-crds.df[!is.na(crds.df$NDVI),]
dim(crds.df)

# ggplot(crds.df, aes(x=X, y=Y, col=NDVI))+
#   geom_point()

# And save the results:
write.csv(crds.df, "ndvi.csv")


# Useful links - satellite imagery in R:
# (1) https://rspatial.org/raster/spatial/4-rasterdata.html
# (2) https://www.neonscience.org/resources/learning-hub/tutorials/image-raster-data-r





