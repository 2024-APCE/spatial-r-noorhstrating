# Spatial analysis in R
# Han Olff nov 2021

rm(list = ls())
# set the working directory where your GIS data are located
#setwd("G:/Shared drives/_Org OlffLab/Teaching/APCE/APCE2024/APCE2024GIS")
setwd("/Users/noorhoogerduijnstrating/Documents/RUG/Master ConsEco/APCE 24/APCE24GIS/apce2024gis") # change this to your own gis file

#### Restoring packages etc. ####
# restore the libraries of the project 
renv::restore()

install.packages("nlme")
install.packages("renv")
install.packages("tidyverse")
install.packages("viridis")
install.packages("wesanderson")
install.packages("tidyterra")

# het installeren van geopackage terra op mac
install.packages("remotes")
remotes::install_github("rspatial/terra")
install.packages("terra", configure.args="--with-gdal-config=/opt/homebrew/opt/gdal/bin/gdal-config --with-proj-include=/opt/homebrew/opt/proj/include --with-proj-lib=/opt/homebrew/opt/proj/lib")
install.packages("nlme")


#### Extra step for Mac users ####

# overige stappen voor mac 
Sys.setenv(PROJ_LIB = "/opt/homebrew/Cellar/proj/9.5.0/share/proj")

# --- andere ideeen voor het oplossen van het probleem in mac --- 
#Sys.setenv(PATH = paste("/opt/homebrew/bin", Sys.getenv("PATH"), sep=":"))

#system("which pkg-config")
#Sys.setenv(PKG_CONFIG_PATH = "/opt/homebrew/lib/pkgconfig")
#system("pkg-config --cflags --libs gdal proj")
#install.packages("terra", configure.args="--with-gdal-config=/opt/homebrew/opt/gdal/bin/gdal-config --with-proj-include=/opt/homebrew/opt/proj/include --with-proj-lib=/opt/homebrew/opt/proj/lib")
#install.packages("tidyterra")


#### Loading libraries and colourpalettes ####

# load the different libraries
library(terra)       # for working with raster data
library(tidyterra)   # for adding terra objects to ggplot
library(ggspatial)  # for scale bars
library(sf)          # for vector data objects
library(tidyverse)   # ggplot, dplyr etc
library(scales)      # for oob (out of bounds) scale
library(ggnewscale) # for using multiple color fill scales in ggplot
library(patchwork)  # for combining multiple ggplots in one panel plot

# explore color palettes
# also see https://www.datanovia.com/en/blog/top-r-color-palettes-to-know-for-great-data-visualization/
# Base R palettes
barplot(rep(1,10), col = grey.colors(10))
mycolors <- c("red", "white", "blue")
mycolors
barplot(rep(1,10), col = rev(topo.colors(10))) # rev turns the scale arround
barplot(rep(1,10), col = rev(terrain.colors(10))) # rev means reverse the colour palette order
library(RColorBrewer) 
RColorBrewer::display.brewer.all()
barplot(rep(1,10), col = RColorBrewer::brewer.pal(10, "Spectral"))

barplot(rep(1,10), col = RColorBrewer::brewer.pal(10, "BrBG"))
library(viridis)
barplot(rep(1,10), col = viridis::viridis(10))
barplot(rep(1,10), col = viridis::plasma(10))
#barplot(rep(1,10), col = viridis::heat(10)) --> heat is not a package from viridis
viridis::plasma(10)
library(wesanderson)
barplot(rep(1,10), col = rev(wesanderson::wes_palette("Zissou1", 10, type = "continuous")))
pal_zissou1<-rev(wesanderson::wes_palette("Zissou1", 10, type = "continuous"))
pal_zissou2<-wesanderson::wes_palette("Zissou1", 10, type = "continuous")
pal_zissou1

# load the vector data for the whole ecosystem
sf::st_layers("./2022_protected_areas/protected_areas.gpkg")
protected_areas<-terra::vect("./2022_protected_areas/protected_areas.gpkg",
            layer="protected_areas_2022") # read protected area boundaries)
sf::st_layers("./2022_rivers/rivers_hydrosheds.gpkg")
rivers<-terra::vect("./2022_rivers/rivers_hydrosheds.gpkg",
                    layer="rivers_hydrosheds")
sf::st_layers("./lakes/lakes.gpkg")
lakes<-terra::vect("./lakes/lakes.gpkg",
                   layer="lakes")  
sf::st_layers("./studyarea/studyarea.gpkg")
studyarea<-terra::vect("./studyarea/New GeoPackage Test.gpkg", # change this to the name of your own studyarea location
                              layer="my_study_area")



# load the raster data for the whole ecosystem
woodybiom<-terra::rast("./2016_WoodyVegetation/TBA_gam_utm36S.tif")
hillshade<-terra::rast("./2023_elevation/hillshade_z5.tif")
rainfall<-terra::rast("./rainfall/CHIRPS_MeanAnnualRainfall.tif")
elevation<-terra::rast("./2023_elevation/elevation_90m.tif")

# inspect the data 
class(protected_areas)
class(elevation)
plot(protected_areas)
plot(elevation)
plot(protected_areas,add=T)

# For the study area, I have used a CRS of EPSG:4326, so I will reproject the woodybiom raster to match the studyarea's CRS
# Reproject woodybiom to match studyarea's CRS (EPSG:4326)
#woodybiom <- terra::project(woodybiom, "EPSG:4326")


# Reproject the study area to match the CRS of the woodybiom raster (EPSG:4326)
studyarea_test <- terra::project(studyarea, terra::crs(woodybiom))

# Check the CRS of the study area after transformation
print(terra::crs(studyarea_test))

# Check the CRS of the woodybiom raster
print(terra::crs(woodybiom))

# Check the CRS of the studyarea vector
print(terra::crs(studyarea_test))



# set the limits of the map to show (xmin, xmax, ymin, ymax in utm36 coordinates)
xlimits<-c(550000,900000)
ylimits<-c(9600000,9950000)

# plot the woody biomass map that you want to predict
#class:
Sys.setenv(PROJ_LIB = "/opt/homebrew/Cellar/proj/9.5.0/share/proj")

# test plot
ggplot() +
  tidyterra::geom_spatraster(data=woodybiom) +
  scale_fill_gradientn(colours=rev(terrain.colors(6)),
                       limits=c(0.77, 6.55),
                       oob=squish,
                       name = "TBA/ha") +
  tidyterra::geom_spatvector(data=protected_areas,
                             fill=NA, linewidth=0.5)


# die van mij werkt niet, maar die van sanne wel?
woody_map <- ggplot() +
  tidyterra::geom_spatraster(data=woodybiom) +
  scale_fill_gradientn(colours=rev(terrain.colors(6)), 
                       limits=c(0.77, 6.55), # can be found in QGIS
                       oob=squish, # everything that is outside the scale will not be commited
                       name="meters") +
  tidyterra::geom_spatvector(data=protected_areas,
                             fill=NA, linewidth=0.5) +
  tidyterra::geom_spatvector(data=lakes,
                             fill="royalblue3", linewidth=0.5) +
  tidyterra::geom_spatvector(data=rivers,
                            col="deepskyblue2", linewidth=0.5) +
  tidyterra::geom_spatvector(data=studyarea_test,
                             fill=NA, linewidth=0.5, col="red") +
  labs(title = "Woody biomass") +
  coord_sf(xlimits, ylimits, datum = sf::st_crs(32736)) +
  theme(axis.text = element_blank(),
        axis.ticks= element_blank()) +
  ggspatial::annotation_scale(location="bl", width_hint = 0.2) # first graph that is needed in your document 
woody_map

# make an elevation map
elevation_map <- ggplot() +
  tidyterra::geom_spatraster(data=elevation) +
  scale_fill_gradientn(colours=terrain.colors(10), 
                       limits=c(500, 2100), # can be found in QGIS
                       oob=squish, # everything that is outside the scale will not be commited
                       name="meters") +
  tidyterra::geom_spatvector(data=protected_areas,
                             fill=NA, linewidth=0.5) +
  tidyterra::geom_spatvector(data=studyarea_test,
                             fill=NA, linewidth=0.5, col="red") +
  tidyterra::geom_spatvector(data=lakes,
                             fill="royalblue3", linewidth=0.5) +
  tidyterra::geom_spatvector(data=rivers,
                            col="deepskyblue2", linewidth=0.5) +
  labs(title = "Elevation") +
  coord_sf(xlimits, ylimits, datum = sf::st_crs(32736)) +
  theme(axis.text = element_blank(),
        axis.ticks= element_blank()) +
  ggspatial::annotation_scale(location="bl", width_hint = 0.2) # first graph that is needed in your document 
elevation_map


# plot the rainfall map but make the colours from light blue to dark blue
rainfall_map <- ggplot() +
  tidyterra::geom_spatraster(data=rainfall) +
  scale_fill_gradientn(colours=rev(viridis::viridis(10)), 
                       limits=c(1000, 3000), # can be found in QGIS
                       oob=squish, # everything that is outside the scale will not be commited
                       name="mm") +
  tidyterra::geom_spatvector(data=protected_areas,
                             fill=NA, linewidth=0.5) +
  tidyterra::geom_spatvector(data=studyarea,
                             fill=NA, linewidth=0.5, col="red") +
  tidyterra::geom_spatvector(data=lakes,
                             fill="royalblue3", linewidth=0.5) +
  tidyterra::geom_spatvector(data=rivers,
                            col="deepskyblue2", linewidth=0.5) +
  labs(title = "Rainfall") +
  coord_sf(xlimits, ylimits, datum = sf::st_crs(32736)) +
  theme(axis.text = element_blank(),
        axis.ticks= element_blank()) +
  ggspatial::annotation_scale(location="bl", width_hint = 0.2) # first graph that is needed in your document
rainfall_map


# combine the different maps  into one composite map using the patchwork library
# and save it to a high resolution png
woody_map + elevation_map + rainfall_map

all_maps<-woody_map +elevation_map + rainfall_map
  patchwork::plot_layout(ncol=1)
all_maps
ggsave("/Users/noorhoogerduijnstrating/Documents/RUG/Github/APCE24/spatial-r-noorhstrating/figures/all_maps.png", width = 18, height = 18, units = "cm",dpi=300)

############################
### explore your study area

# For the study area, I have used a CRS of EPSG:4326, so I will reproject the woodybiom raster to match the studyarea's CRS
# Reproject woodybiom to match studyarea's CRS (EPSG:4326)
woodybiom_tf <- terra::project(woodybiom, "EPSG:4326")
elevation_tf <- terra::project(elevation, "EPSG:4326")
rainfall_tf <- terra::project(rainfall, "EPSG:4326")

# Define x and y limits based on the extent of studyarea
xlimits_sa <- c(sf::st_bbox(studyarea)$xmin, sf::st_bbox(studyarea)$xmax)
ylimits_sa <- c(sf::st_bbox(studyarea)$ymin, sf::st_bbox(studyarea)$ymax)

# set the limits of your study area
xlimits<-sf::st_bbox(studyarea)[c(1,3)]
ylimits<-sf::st_bbox(studyarea)[c(2,4)]
saExt<-terra::ext(studyarea)

# crop the woody biomass to the extent of the studyarea
woodybiom_sa <- terra::crop(woodybiom_tf, studyarea)


# plot the woody biomass
woody_map_sa<-ggplot() +
  tidyterra::geom_spatraster(data=woodybiom_sa) +
  scale_fill_gradientn(colours=rev(terrain.colors(6)),
                       limits=c(0.77,6.55),
                       oob=squish,
                       name="TBA/ha") +
  tidyterra::geom_spatvector(data=protected_areas,
                             fill=NA,linewidth=0.5) +
  tidyterra::geom_spatvector(data=studyarea,
                             fill=NA,linewidth=0.5,col="red") +
  tidyterra::geom_spatvector(data=lakes,
                             fill="lightblue",linewidth=0.5) +
  tidyterra::geom_spatvector(data=rivers,
                             col="blue",linewidth=0.5) +
  labs(title="Woody biomass in the study area") +
  coord_sf(xlimits,ylimits,expand=F,
           datum = sf::st_crs(32736)) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank()) +
  ggspatial::annotation_scale(location="bl",width_hint=0.2)
woody_map_sa  


# make maps also for the other layers that you found
# make an elevation map for the study area
elevation_sa<-terra::crop(elevation_tf,studyarea)

elevation_map_sa<-ggplot() +
  tidyterra::geom_spatraster(data=elevation_sa) +
  scale_fill_gradientn(colours=terrain.colors(10),
                       limits=c(500,2100),
                       oob=squish,
                       name="meters") +
  tidyterra::geom_spatvector(data=protected_areas,
                             fill=NA,linewidth=0.5) +
  tidyterra::geom_spatvector(data=studyarea,
                             fill=NA,linewidth=0.5,col="red") +
  tidyterra::geom_spatvector(data=lakes,
                             fill="lightblue",linewidth=0.5) +
  tidyterra::geom_spatvector(data=rivers,
                             col="blue",linewidth=0.5) +
  labs(title="Elevation in the study area") +
  coord_sf(xlimits,ylimits,expand=F,
           datum = sf::st_crs(32736)) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank()) +
  ggspatial::annotation_scale(location="bl",width_hint=0.2)
elevation_map_sa

# make a rainfall map for the study area
rainfall_sa<-terra::crop(rainfall_tf, studyarea)

rainfall_map_sa<-ggplot() +
  tidyterra::geom_spatraster(data=rainfall_sa) +
  scale_fill_gradientn(colours=rev(viridis::viridis(10)),
                       limits=c(1000,3000),
                       oob=squish,
                       name="mm") +
  tidyterra::geom_spatvector(data=protected_areas,
                             fill=NA,linewidth=0.5) +
  tidyterra::geom_spatvector(data=studyarea,
                             fill=NA,linewidth=0.5,col="red") +
  tidyterra::geom_spatvector(data=lakes,
                             fill="lightblue",linewidth=0.5) +
  tidyterra::geom_spatvector(data=rivers,
                             col="blue",linewidth=0.5) +
  labs(title="Rainfall in the study area") +
  coord_sf(xlimits,ylimits,expand=F,
           datum = sf::st_crs(32736)) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank()) +
  ggspatial::annotation_scale(location="bl",width_hint=0.2)
rainfall_map_sa


# create 500 random points in our study area

# and add them to the previous map


# make distance to river map
# find dist2 river in files 
dist2river_sa<-terra::rast("/Users/noorhoogerduijnstrating/Documents/RUG/Master ConsEco/APCE 24/APCE24GIS/apce2024gis/2022_rivers/DistanceToRiver.tif")
dist2river_tf <- terra::project(dist2river_sa, "EPSG:4326")

# crop the distance to river to the extent of the studyarea
dist2river_sa<-terra::crop(dist2river_tf,studyarea)

# Check the extents of both the study area and the raster
print(terra::ext(studyarea))
print(terra::ext(dist2river_sa))


map_dist2river_sa<-ggplot() +
  tidyterra::geom_spatraster(data=dist2river_sa/1000) +
  scale_fill_gradientn(colours = pal_zissou2,
                       limits=c(0,10),
                       oob=squish,
                       name="kilometers") +
  tidyterra::geom_spatvector(data = protected_areas,fill=NA, linewidth=0.7) +
  tidyterra::geom_spatvector(data=rivers,linewidth=0.3,col="blue") +
  labs(title = "Distance to rivers") +
  coord_sf(xlim=xlimits,ylim=ylimits, # set bounding box
           expand=F,
           datum=sf::st_crs(32736)) +   # keep in original projected coordinates
  theme(axis.text = element_blank(),
        axis.ticks = element_blank()) +   # Remove axis coordinate labels
  ggspatial::annotation_scale(  # Add a scale bar
    location = "bl",             # Position: bottom left
    width_hint = 0.2)             # Adjust width of the scale bar +
map_dist2river_sa


### put all maps together
all_maps_sa<-woody_map_sa +map_dist2river_sa + elevation_map_sa + rainfall_map_sa
  patchwork::plot_layout(ncol=2)
all_maps_sa
ggsave("/Users/noorhoogerduijnstrating/Documents/RUG/Github/APCE24/spatial-r-noorhstrating/figures/all_maps_sa.png", width = 18, height = 18, units = "cm",dpi=300)


# extract your the values of the different raster layers to the points


# make long format

# plot how woody cover is predicted by different variables


