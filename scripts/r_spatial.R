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
fires <- terra::rast("/Users/noorhoogerduijnstrating/Documents/RUG/Master ConsEco/APCE 24/APCE24GIS/apce2024gis/landform/YearLastBurned.tif")

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
                             fill="deepskyblue2", linewidth=0.5) +
  tidyterra::geom_spatvector(data=rivers,
                            col="royalblue3", linewidth=0.5) +
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

# make a map for the fires 
fire_map <- ggplot() +
  tidyterra::geom_spatraster(data=fires) +
  scale_fill_gradientn(colours=rev(viridis::viridis(10)), 
                       limits=c(2001, 2024), # can be found in QGIS
                       oob=squish, # everything that is outside the scale will not be commited
                       name="Year") +
  tidyterra::geom_spatvector(data=protected_areas,
                             fill=NA, linewidth=0.5) +
  tidyterra::geom_spatvector(data=lakes,
                             fill="deepskyblue2", linewidth=0.5) +
  tidyterra::geom_spatvector(data=rivers,
                             col="royalblue3", linewidth=0.5) +
  tidyterra::geom_spatvector(data=studyarea_test,
                             fill=NA, linewidth=0.5, col="red") +
  labs(title = "Burned last year") +
  coord_sf(xlimits, ylimits, datum = sf::st_crs(32736)) +
  theme(axis.text = element_blank(),
        axis.ticks= element_blank()) +
  ggspatial::annotation_scale(location="bl", width_hint = 0.2) # first graph that is needed in your document 
fire_map


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
fires_tf <- terra::project(fires, "EPSG:4326")

# Define x and y limits based on the extent of studyarea
xlimits_sa <- c(sf::st_bbox(studyarea)$xmin, sf::st_bbox(studyarea)$xmax)
ylimits_sa <- c(sf::st_bbox(studyarea)$ymin, sf::st_bbox(studyarea)$ymax)

# set the limits of your study area
xlimits<-sf::st_bbox(studyarea)[c(1,3)]
ylimits<-sf::st_bbox(studyarea)[c(2,4)]
saExt<-terra::ext(studyarea)
saExt

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
                       limits=c(800,2100),
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

# first you need to increase the raster resolution to 30 m
# define the extent and resolution for the new raster
# Define the extent and resolution for the new raster
rainfall_30m <- rast(terra::ext(rainfall), resolution = 30, crs = crs(rainfall))
# Resample the raster to 30m resolution
rainfall_30m <- terra::resample(rainfall, rainfall_30m, method = "bilinear")
rainfall_30m <- terra::project(rainfall_30m, "EPSG:4326")
rainfall_sa<-terra::crop(rainfall_30m,saExt) # crop to study area

rainfall_map_sa<-ggplot() +
  tidyterra::geom_spatraster(data=rainfall_sa) +
  scale_fill_gradientn(colours=rev(viridis::viridis(10)),
                       limits=c(600,1000),
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

# soil cation exchange capacity (CEC)
cec_sa<-terra::rast("/Users/noorhoogerduijnstrating/Documents/RUG/Master ConsEco/APCE 24/APCE24GIS/apce2024gis/landform/CEC_5_15cm.tif")
hist(cec_sa)

cec_sa_tf <- terra::project(cec_sa, "EPSG:4326")
cec_sa_tf <- terra::crop(cec_sa_tf, studyarea)

cec_map_sa<-ggplot() +
  tidyterra::geom_spatraster(data=cec_sa_tf) +
  scale_fill_gradientn(colours=pal_zissou1,
                       limits=c(100,250),
                       oob=squish,
                       name="Soil\nCEC\n5-15cm") +
  tidyterra::geom_spatvector(data=protected_areas,
                             fill=NA,linewidth=0.5) +
  tidyterra::geom_spatvector(data=studyarea,
                             fill=NA,linewidth=0.5,col="red") +
  tidyterra::geom_spatvector(data=lakes,
                             fill="lightblue",linewidth=0.5) +
  tidyterra::geom_spatvector(data=rivers,
                             col="blue",linewidth=0.5) +
  labs(title="Soil CEC") +
  coord_sf(xlimits,ylimits,expand=F,
           datum = sf::st_crs(32736)) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank()) +
  ggspatial::annotation_scale(location="bl",width_hint=0.2)
cec_map_sa


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


dist2river_map_sa<-ggplot() +
  tidyterra::geom_spatraster(data=dist2river_sa) +
  scale_fill_gradientn(colours=topo.colors(6),
                       limits=c(0,12000),
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
  labs(title="Distance to river") +
  coord_sf(xlimits,ylimits,expand=F,
           datum = sf::st_crs(32736)) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank()) +
  ggspatial::annotation_scale(location="bl",width_hint=0.2)
dist2river_map_sa

# burning frequency map from 2001 - 2016
burnfreq_sa<-terra::rast("/Users/noorhoogerduijnstrating/Documents/RUG/Master ConsEco/APCE 24/APCE24GIS/apce2024gis/landform/BurnFreq.tif")
burnfreq_tf <- terra::project(burnfreq_sa, "EPSG:4326")
burnfreq_tf <- crop(burnfreq_tf, studyarea)

burnfreq_map_sa<-ggplot() +
  tidyterra::geom_spatraster(data=burnfreq_tf) +
  scale_fill_gradientn(colours=pal_zissou2,
                       limits=c(0,16),
                       oob=squish,
                       name="years\nburned") +
  tidyterra::geom_spatvector(data=protected_areas,
                             fill=NA,linewidth=0.5) +
  tidyterra::geom_spatvector(data=studyarea,
                             fill=NA,linewidth=0.5,col="red") +
  tidyterra::geom_spatvector(data=lakes,
                             fill="lightblue",linewidth=0.5) +
  tidyterra::geom_spatvector(data=rivers,
                             col="blue",linewidth=0.5) +
  labs(title="n years burned") +
  coord_sf(xlimits,ylimits,expand=F,
           datum = sf::st_crs(32736)) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank()) +
  ggspatial::annotation_scale(location="bl",width_hint=0.2)
burnfreq_map_sa

# landform of all layers
#landform_sa<-terra::rast("/Users/noorhoogerduijnstrating/Documents/RUG/Master ConsEco/APCE 24/APCE24GIS/apce2024gis/landform/landforms.tif")
#landform_tf <- terra::project(landform_sa, "EPSG:4326")
#landform_tf <- terra::crop(landform_tf, studyarea)

#landform_map_sa_layers<-ggplot() +
  tidyterra::geom_spatraster(data=as.factor(landform_tf)) +
  scale_fill_manual(
    values = c("#6f198c", "#e1f0e5", "#1c6330", "#dccdce", "#aa0000", "#808080", "#141414"),
    labels = c("Valley (narrow)", "Lower slope (flat)", "Lower slope (warm)", 
               "Upper slope (flat)", "Upper slope (warm)", "Peak/ridge (cold)", 
               "Peak/ridge (warm)"))+
  tidyterra::geom_spatvector(data=protected_areas,
                             fill=NA,linewidth=0.5) +
  tidyterra::geom_spatvector(data=studyarea,
                             fill=NA,linewidth=0.5,col="red") +
  tidyterra::geom_spatvector(data=lakes,
                             fill="lightblue",linewidth=0.5) +
  tidyterra::geom_spatvector(data=rivers,
                             col="blue",linewidth=0.5) +
  labs(title="Landform") +
  coord_sf(xlimits,ylimits,expand=F,
           datum = sf::st_crs(32736)) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank()) +
  ggspatial::annotation_scale(location="bl",width_hint=0.2)
#landform_map_sa_layers

# landform valleys and plains (CEC)
landform_sa_vp<-terra::rast("/Users/noorhoogerduijnstrating/Documents/RUG/Master ConsEco/APCE 24/APCE24GIS/apce2024gis/landform/hills.tif")
landform_tf_vp <- terra::project(landform_sa_vp, "EPSG:4326")
landform_tf_vp <- terra::crop(landform_tf_vp, studyarea)

landform_map_sa_vp <-ggplot() +
  tidyterra::geom_spatraster(data=as.factor(landform_sa_vp)) +
  scale_fill_manual(values=c("black","orange"),
                    labels=c("valleys\nand\nplains","hills")) +
  tidyterra::geom_spatvector(data=protected_areas,
                             fill=NA,linewidth=0.7) +
  tidyterra::geom_spatvector(data=studyarea,
                             fill=NA,linewidth=0.5,col="green") +
  tidyterra::geom_spatvector(data=lakes,
                             fill="lightblue",linewidth=0.5) +
  tidyterra::geom_spatvector(data=rivers,
                             col="blue",linewidth=0.5) +
  labs(title="Landform") +
  coord_sf(xlimits,ylimits,expand=F,
           datum = sf::st_crs(32736)) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank()) +
  ggspatial::annotation_scale(location="bl",width_hint=0.2)
landform_map_sa_vp

terra::values(landform_sa_vp)


# core_protected_areas  map 
r<-terra::rast("/Users/noorhoogerduijnstrating/Documents/RUG/Master ConsEco/APCE 24/APCE24GIS/apce2024gis/MyData/CoreProtectedAreas.tif") 
CoreProtectedAreas <- r |> #  replace NA by 0
  is.na() |>
  terra::ifel(0,r) 

CoreProtectedAreas_tf <- terra::project(CoreProtectedAreas, "EPSG:4326")
CoreProtectedAreas_sa <- terra::crop(CoreProtectedAreas_tf, studyarea)

CoreProtectedAreas_map_sa<-ggplot() +
  tidyterra::geom_spatraster(data=as.factor(CoreProtectedAreas_sa)) +
  scale_fill_manual(values=c("grey","lightgreen"),
                    labels=c("no","yes")) +
  tidyterra::geom_spatvector(data=protected_areas,
                             fill=NA,linewidth=0.5) +
  tidyterra::geom_spatvector(data=studyarea,
                             fill=NA,linewidth=0.5,col="red") +
  tidyterra::geom_spatvector(data=lakes,
                             fill="lightblue",linewidth=0.5) +
  tidyterra::geom_spatvector(data=rivers,
                             col="blue",linewidth=0.5) +
  labs(title="Core protected areas") +
  coord_sf(xlimits,ylimits,expand=F,
           datum = sf::st_crs(32736)) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank()) +
  ggspatial::annotation_scale(location="bl",width_hint=0.2)
CoreProtectedAreas_map_sa 


# create 250 random points in your study area
rpoints_crs4326 <- terra::project(rpoints, "EPSG:4326")
rpoints <- terra::crop(rpoints, studyarea)
set.seed(123)
rpoints <- terra::spatSample(studyarea, size = 250, 
                             method = "random")

# plot the points
rpoints_map_sa<-ggplot() +
  tidyterra::geom_spatvector(data=rpoints, size=0.5) +
  tidyterra::geom_spatvector(data=protected_areas,
                             fill=NA,linewidth=0.5) +
  tidyterra::geom_spatvector(data=studyarea,
                             fill=NA,linewidth=0.5,col="red") +
  tidyterra::geom_spatvector(data=lakes,
                             fill="lightblue",linewidth=0.5) +
  tidyterra::geom_spatvector(data=rivers,
                             col="blue",linewidth=0.5) +
  labs(title="250 random points") +
  coord_sf(xlimits,ylimits,expand=F,
           datum = sf::st_crs(32736)) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank()) +
  ggspatial::annotation_scale(location="bl",width_hint=0.2)
rpoints_map_sa


### put all maps together
all_maps_sa<-woody_map_sa + dist2river_map_sa + elevation_map_sa + rainfall_map_sa + burnfreq_map_sa + cec_map_sa + landform_map_sa + CoreProtectedAreas_map_sa + rpoints_map_sa
  patchwork::plot_layout(ncol=2)
all_maps_sa
ggsave("/Users/noorhoogerduijnstrating/Documents/RUG/Github/APCE24/spatial-r-noorhstrating/figures/all_maps_sa.png", width = 18, height = 18, units = "cm",dpi=300)

#########################
# extract your the values of the different raster layers to the points
# Extract raster values at the points
ls()  # Lists all objects in the current environment
names(woodybiom_sa)

# Check the extent of the reprojected raster
ext(woodybiom_sa)

# Check the extent of the reprojected points
ext(rpoints)



woody_points <- terra::extract(woodybiom_sa_projected, rpoints) |> 
  as_tibble() |>
  dplyr::rename(woody=TBA_gam_utm36s)
woody_points
dist2river_points <- terra::extract(dist2river_sa, rpoints) |> 
  as_tibble() |>
  dplyr::rename(dist2river=distance)
dist2river_points
elevation_points <- terra::extract(elevation, rpoints) |> 
  as_tibble() 
elevation_points
CorProtAr_points <- terra::extract(CoreProtectedAreas_sa, rpoints) |> 
  as_tibble() |>
  dplyr::rename(CorProtAr=CoreProtectedAreas)
CorProtAr_points
rainfall_points <- terra::extract(rainfall_sa, rpoints) |> 
  as_tibble() |> 
  dplyr::rename(rainfall=CHIRPS_MeanAnnualRainfall)
rainfall_points
cec_points <- terra::extract(cec_sa, rpoints) |> 
  as_tibble() |>
  dplyr::rename(cec='cec_5-15cm_mean')
cec_points
burnfreq_points <- terra::extract(burnfreq_sa, rpoints) |> 
  as_tibble() |>
  dplyr::rename(burnfreq=burned_sum)
burnfreq_points
landform_points <- terra::extract(landform_sa, rpoints) |> 
  as_tibble() |>
  dplyr::rename(hills=remapped)
landform_points

# merge the different variable into a single table
# use woody biomass as the last variable
pointdata<-cbind(dist2river_points[,2],elevation_points[,2],
                 CorProtAr_points[,2],rainfall_points[,2], 
                 cec_points[,2],burnfreq_points[,2],
                 landform_points[,2],woody_points[,2]) |>
  as_tibble()
pointdata
pointdata <- pointdata[complete.cases(pointdata),]


# make long format

# plot how woody cover is predicted by different variables
# Create a correlation panel plot
library(psych)
psych::pairs.panels(
  pointdata ,
  method = "pearson",     # Correlation method (use "spearman" for rank correlation)
  hist.col = "lightblue",  # Color for histograms
  density = TRUE,          # Add density plots
  ellipses = F,         # Add correlation ellipses
  lm = TRUE,                # Add linear regression lines
  stars=T
)

# make long format
names(pointdata)
pointdata_long<-pivot_longer(data=pointdata,
                             cols = dist2river:hills, # all except woody
                             names_to ="pred_var",
                             values_to = "pred_val")
pointdata_long

# panel plot
ggplot(data=pointdata_long, mapping=aes(x=pred_val,y=woody,group=pred_var)) +
  geom_point() +
  geom_smooth() +
  ylim(0,40) +
  facet_wrap(~pred_var,scales="free") 

# do a pca
# Load the vegan package
library(vegan)
# Perform PCA using the rda() function
pca_result <- vegan::rda(pointdata,
                         scale = TRUE)
# Display a summary of the PCA
summary(pca_result)

# Plot the PCA
plot(pca_result, scaling = 2, type="n", xlab="",ylab="")  # Use scaling = 1 for distance preservation, scaling = 2 for correlations
# Add points for samples
points(pca_result, display = "sites", pch=pointdata$CorProtAr+1, col = pointdata$hills+1, bg = "blue", cex = 1)
# Add arrows for variables
arrows(0, 0, scores(pca_result, display = "species")[, 1], scores(pca_result, display = "species")[, 2], 
       length = 0.1, col = "red")
# Label the variables with arrows
text(scores(pca_result, display = "species")[, 1], scores(pca_result, display = "species")[, 2], 
     labels = colnames(pointdata), col = "red", cex = 0.8, pos = 4)
# Add axis labels and a title
title(main = "PCA Biplot")
xlabel <- paste("PC1 (", round(pca_result$CA$eig[1] / sum(pca_result$CA$eig) * 100, 1), "%)", sep = "")
ylabel <- paste("PC2 (", round(pca_result$CA$eig[2] / sum(pca_result$CA$eig) * 100, 1), "%)", sep = "")
title(xlab=xlabel)
title(ylab=ylabel)
# add contours for woody cover
vegan::ordisurf(pca_result, pointdata$woody, add = TRUE, col = "green4")




