################## Central American protected connectivity area analysis ##########
# Date: 7-11-23
# updated: 
# Author: Ian McCullough, immccull@gmail.com
###################################################################################

#### R libraries ####
library(sf)
library(tidyverse)
library(terra)
library(ggplot2)
library(dplyr)
library(reshape2)
library(gridExtra)

#### Input data ####
setwd("C:/Users/immcc/Documents/Osa-Conservation-Connectivity-Project")

all_countries_shp <- st_read("data/spatial/area_of_interest/all_countries_islands.shp", quiet = TRUE)
focal_countries_shp <- st_read("data/spatial/area_of_interest/focal_countries.shp", quiet = TRUE)
aoi <- st_read("data/spatial/area_of_interest/aoi.shp", quiet = TRUE)
#KBA <- st_read("data/spatial/KBA/Americas_KBA.shp", quiet = TRUE)

# get protected areas extracted by Chris
focal_pa <- readRDS("data/spatial/protected_areas/focal_area_pa_shp.RDS")
focal_pa <- st_transform(focal_pa,st_crs(4326)) 
focal_pa <- focal_pa %>% st_sf() %>%  st_cast()

all_countries_shp <- st_transform(all_countries_shp, st_crs(4326))
focal_countries_shp <- st_transform(focal_countries_shp, st_crs(4326))
aoi <- st_transform(aoi, st_crs(4326))

##### Main program #####
# filter protected areas
# for now, per Saura et al. 2018: https://www.sciencedirect.com/science/article/pii/S0006320717312284
# with help from: 
# Citation
# UNEP-WCMC (2019). User Manual for the World Database on Protected Areas and world database on other
# effective area-based conservation measures: 1.6. UNEP-WCMC: Cambridge, UK. Available at:
#  http://wcmc.io/WDPA_Manual
focal_pa <- subset(focal_pa, MARINE %in% c(0,1)) #keep terrestrial and coastal
focal_pa <- subset(focal_pa, STATUS %in% c('Designated','Established','Inscribed')) #exclude "Proposed"; no "Adopted" in this dataset
#focal_pa <- subset(focal_pa, IUCN_CAT %in% c('Ia','Ib','II','III','IV','V','VI')) #remove 'Not Reported' and 'Not Applicable'
#focal_pa <- subset(focal_pa, !(ISO3 %in% c('CRI;PAN'))) #for now, removing one PA shared by Costa Rica/Panama
focal_pa$ISO3 <- ifelse(focal_pa$ISO3=='CRI;PAN', 'CRI', focal_pa$ISO3) #one PA is desginated as both in CR and Panama; designating as CR for now because more of it occurs in CR
focal_pa <- subset(focal_pa, !(DESIG_ENG == 'UNESCO-MAB Biosphere Reserve'))
# could filter based on PA size, but for now leaving as is

ggplot() + 
  geom_sf(data=aoi)+
  geom_sf(data = focal_pa, aes(fill = MARINE))
  
# convert data to spatial vector (terra) and reproject to UTM zone 17N 
focal_pa_vect <- terra::vect(focal_pa)   
focal_pa_vect <- terra::project(focal_pa_vect, "EPSG:31971")

aoi_vect <- terra::vect(aoi)
aoi_vect <- terra::project(aoi_vect, "EPSG:31971")

focal_countries_vect <- terra::vect(focal_countries_shp)
focal_countries_vect <- terra::project(focal_countries_vect, "EPSG:31971")

plot(aoi_vect, col='forestgreen')
plot(focal_pa_vect, add=T)

# save reprojected output for vector processing in QGIS
#writeVector(focal_pa_vect, filename="data/spatial/protected_areas/focal_pa_31971.shp", filetype='ESRI Shapefile')
#writeVector(focal_countries_vect, filename="data/spatial/area_of_interest/focal_countries_31971.shp", filetype='ESRI Shapefile')

## Get summary stats/basic plots of protected area coverage
# before cleaning up WDPA polygons
# set country plotting colors
country_colors <- c('BLZ'='blue','COL'='gold','CRI'='forestgreen','GTM'='orange','HND'='firebrick',
                    'MEX'='gray','NIC'='turquoise','PAN'='khaki','SLV'='chartreuse')

country_colors7 <- c('BLZ'='blue','CRI'='forestgreen','GTM'='orange','HND'='firebrick',
                    'NIC'='turquoise','PAN'='khaki','SLV'='chartreuse')

## correct % protection (deal with overlapping polygons, coastal spillover)
# read in clipped/dissolved PA polygons from Dissolve Boundaries in ArcGIS
pa_dissolved <- terra::vect("data/spatial/protected_areas/focal_pa_31971_onland_DissolveBoundaries2_SJ.shp")
plot(focal_countries_vect, col='tan')
plot(pa_dissolved, col='gold',add=T)
pa_dissolved_AEAC <- terra::project(pa_dissolved, "EPSG:9822")

#pa_singlepart <- terra::vect("data/spatial/protected_areas/focal_pa_31971_onland_singlepart2.shp")
pa_singlepart <- read_sf("data/spatial/protected_areas/focal_pa_31971_onland_singlepart2.shp")
# 3 polygons are CRI;PAN...I looked at map and 2 are fully in CR and one is majority in CR, so can assign all to CR
pa_singlepart$ISO3 <- ifelse(pa_singlepart$ISO3=='CRI;PAN', 'CRI', pa_singlepart$ISO3)

### Exploring R package Makurhini
#library(devtools)
#library(remotes)
#install_github("connectscape/Makurhini", dependencies = TRUE, upgrade = "never")

library(Makurhini)
# testing on a small country
belize_pa <- subset(pa_singlepart, ISO3=='BLZ')
belize_pa$NewID <- seq(1,nrow(belize_pa),1)

focal_countries_shp <- st_transform(focal_countries_shp, st_crs(31971))
belize <- subset(focal_countries_shp, iso_a3=='BLZ')

#plot(belize[1], col='gray50')
#plot(belize_pa[1], col='gold', add=T)

# these seem to screw up due to problem with data
distancemat <- distancefile(nodes=belize_pa, id='NewID', type='centroid',
                            distance_unit='km', resistance=F, pairwise=T)

test <- MK_ProtConn(nodes=belize_pa, region=belize,
                    area_unit='km2', distance=list(type='centroid', resistance=NULL),
                    distance_thresholds=10000,
                    probability=0.5, geom_simplify=T)

# this one fails due to memory, causes R studio to crash
test <- MK_ProtConn(nodes=Protected_areas, region=regions,
                    area_unit='km2', distance=list(type='centroid', resistance=NULL),
                    distance_thresholds=10000,
                    probability=0.5, plot=T)

## still throws error; something about different number of rows in arguments
data("list_forest_patches", package = "Makurhini")
data("study_area", package = "Makurhini")
#Max_attribute <- unit_convert(gArea(study_area), "m2", "ha")#doesn't work; maybe rgeos/sf compatability issue?
dECA_test <- MK_dECA(nodes= list_forest_patches, attribute = NULL, area_unit = "ha",
                     distance = list(type= "centroid"), metric = "PC",
                     probability = 0.05, distance_thresholds = 5000, plot= c("1993", "2003", "2007", "2011"))

## Will any of the examples work??
data("vegetation_patches", package = "Makurhini")
nrow(vegetation_patches) # Number of patches
class(vegetation_patches)[1]

IIC <- MK_dPCIIC(nodes = vegetation_patches, attribute = NULL,
                 distance = list(type = "centroid"),
                 metric = "IIC", distance_thresholds = 10000) #10 km
head(IIC)


PC <- MK_dPCIIC(nodes = vegetation_patches, attribute = NULL,
                distance = list(type = "centroid"),
                metric = "PC", probability = 0.05,
                distance_thresholds = 10000)

PCbelize <- MK_dPCIIC(nodes = belize_pa, attribute = NULL,
                distance = list(type = "centroid"),
                metric = "PC", probability = 0.05,
                distance_thresholds = 10000)
# warning: very slow
PCall <- MK_dPCIIC(nodes = pa_singlepart, attribute = NULL,
                      distance = list(type = "centroid"),
                      metric = "PC", probability = 0.05,
                      distance_thresholds = 10000)
