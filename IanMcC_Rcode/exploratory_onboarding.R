################### Exploratory onboarding analysis ###############################
# Date: 6-15-23
# updated: 6-20-23
# Author: Ian McCullough, immccull@gmail.com
###################################################################################

#### R libraries ####
library(stars)
library(sf)
library(googledrive)
library(mapview)
library(dplyr)
library(tidyverse)
library(terra)

#### Input data ####
setwd("C:/Users/immcc/Documents/Osa-Conservation-Connectivity-Project")
## Get files off Google Drive
# # Authorize google drive
# googledrive::drive_auth()
# 
# ## Download available shapefiles
# # create new folders
# dir.create("data")
# dir.create("data/spatial")
# 
# # specify folder link to id
# #ian_folder <- "https://drive.google.com/drive/folders/1V4_OugDTMvHganFWyBXtUSLiqVKDfHoT" #everything
ian_folder <- "https://drive.google.com/drive/folders/1ObzVyAOO5ganXkaJL7pf_I6vWRV5rv_G"
folder_id <- googledrive::drive_get(as_id(ian_folder))

# # find files in folder
# files = googledrive::drive_ls(folder_id)
# i <- 1
# #loop dirs and download files inside them
# for (i in seq_along(files$name)) {
#   #list files
#   i_dir = googledrive::drive_ls(files[i, ])
# 
#   #mkdir
#   dir.create(paste0("data/spatial/",files$name[i]))
# 
#   #download files
#   for (file_i in seq_along(i_dir$name)) {
#     #fails if already exists
#     try({
#       googledrive::drive_download(
#         googledrive::as_id(i_dir$id[file_i]),
#         path = paste0("data/spatial/", files$name[i], "/", i_dir$name[file_i]),
#         overwrite=T
#       )
#     })
#   }
# }

#### Main program ####
all_countries_shp <- st_read("data/spatial/area_of_interest/all_countries_islands.shp", quiet = TRUE)
focal_countries_shp <- st_read("data/spatial/area_of_interest/focal_countries.shp", quiet = TRUE)
aoi <- st_read("data/spatial/area_of_interest/aoi.shp", quiet = TRUE)
KBA <- st_read("data/spatial/KBA/Americas_KBA.shp", quiet = TRUE)

# get protected areas extracted by Chris
focal_pa <- readRDS("data/spatial/protected_areas/focal_area_pa_shp.RDS")
focal_pa <- st_transform(focal_pa,st_crs(4326)) 
focal_pa <- focal_pa %>% st_sf() %>%  st_cast()

#test <- focal_pa[,c('PARENT_ISO','ISO3')] #seems country columns are the same
#identical(test[['PARENT_ISO']],test[['ISO3']])

# filter protected areas
# for now, per Saura et al. 2018: https://www.sciencedirect.com/science/article/pii/S0006320717312284
# with help from: 
# Citation
# UNEP-WCMC (2019). User Manual for the World Database on Protected Areas and world database on other
# effective area-based conservation measures: 1.6. UNEP-WCMC: Cambridge, UK. Available at:
#  http://wcmc.io/WDPA_Manual
#focal_pa <- subset(focal_pa, MARINE %in% c(0,1)) #keep terrestrial and coastal
focal_pa <- subset(focal_pa, STATUS %in% c('Designated','Established','Inscribed'))
#focal_pa <- subset(focal_pa, IUCN_CAT %in% c('Ia','Ib','II','III','IV','V','VI')) #remove 'Not Reported' and 'Not Applicable'
focal_pa <- subset(focal_pa, !(ISO3 %in% c('CRI;PAN'))) #for now, removing one PA shared by Costa Rica/Panama
# could filter based on PA size, but for now leaving as is

## calculate and plot basic PA statistics by country
ggplot(data=as.data.frame(focal_pa), aes(y=GIS_AREA, x=ISO3)) +
  geom_boxplot()+
  theme_classic()+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        axis.title.x=element_blank())+
  scale_y_continuous(name='Area (sq km)', limits=c())+
  ggtitle("Protected area size by country")

pa_by_country <- as.data.frame(focal_pa) %>%
  group_by(ISO3) %>%
  summarize(min=min(GIS_AREA, na.rm=T), pct25=quantile(GIS_AREA, 0.25, na.rm=T), median=median(GIS_AREA, na.rm=T),
            mean=mean(GIS_AREA, na.rm=T), pct75=quantile(GIS_AREA, 0.75, na.rm=T),
            max=max(GIS_AREA, na.rm=T))

ggplot(data=as.data.frame(pa_by_country), aes(y=median, x=ISO3)) +
  geom_bar(stat='identity')+
  theme_classic()+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        axis.title.x=element_blank())+
  scale_y_continuous(name='Area (sq km)', limits=c())+
  ggtitle("Median protected area size by country")

ggplot(data=as.data.frame(pa_by_country), aes(y=mean, x=ISO3)) +
  geom_bar(stat='identity')+
  theme_classic()+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        axis.title.x=element_blank())+
  scale_y_continuous(name='Area (sq km)', limits=c())+
  ggtitle("Mean protected area size by country")

ggplot(data=as.data.frame(pa_by_country), aes(y=min, x=ISO3)) +
  geom_bar(stat='identity')+
  theme_classic()+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        axis.title.x=element_blank())+
  scale_y_continuous(name='Area (sq km)', limits=c())+
  ggtitle("Smallest protected area size by country")

ggplot(data=as.data.frame(pa_by_country), aes(y=max, x=ISO3)) +
  geom_bar(stat='identity')+
  theme_classic()+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        axis.title.x=element_blank())+
  scale_y_continuous(name='Area (sq km)', limits=c())+
  ggtitle("Largest protected area size by country")

# not very interesting due to highly skewed data (i.e., mostly small PAs)
# ggplot(focal_pa, aes(GIS_AREA)) + geom_density() + facet_wrap(~ISO3)
# ggplot(focal_pa, aes(GIS_AREA))+ 
#   geom_histogram(binwidth=100)+
#   #scale_x_continuous(breaks = seq(0, 10000, 100))+
#   facet_wrap(~ISO3)

## Exploring elevation data ##
#srtm_lower <- terra::rast("data/spatial/SRTM90_V4/SRTM90_V4.elevation_lower.tif")
#srtm_upper <- terra::rast("data/spatial/SRTM90_V4/SRTM90_V4.elevation_upper.tif")

srtm_all <- terra::rast("data/spatial/SRTM90_V4/SRTM90_V4.elevation_all.tif")
srtm_all_proj <- terra::project(srtm_all, "EPSG:31971", 
                                method='average', res=c(90,90))

#terra::origin(srtm_lower)
#terra::origin(srtm_upper)
#srtm_mosaic <- terra::mosaic(srtm_upper, srtm_lower, fun='mean')
#srtm_mosaic_proj <- terra::project(srtm_mosaic, "EPSG:31971", 
#                      method='average', res=c(90,90))
#terra::writeRaster(srtm_mosaic_proj, filename="data/spatial/SRTM90_V4/SRTM90_V4_mosaic.tif", overwrite=T)

# focal_pa_test <- focal_pa[c(1:5),]
# focal_pa_test <- terra::vect(focal_pa_test)
# focal_pa_test <- terra::project(focal_pa_test, srtm_mosaic_proj)
# test_extract <- terra::extract(srtm_mosaic_proj, focal_pa_test, fun=mean, method='simple', na.rm=T)
# test_extract

# reproject focal PAs before extracting elevation data
focal_pa_vect <- terra::vect(focal_pa)
focal_pa_proj <- terra::project(focal_pa_vect, srtm_all_proj)

focal_pa_elev_mean <- terra::extract(srtm_all_proj, focal_pa_proj, fun=mean, method='simple', na.rm=T)
colnames(focal_pa_elev_mean) <- c('ID','mean_m')
hist(focal_pa_elev_mean$mean_m)
focal_pa_elev_min <- terra::extract(srtm_all_proj, focal_pa_proj, fun=min, method='simple', na.rm=T)
colnames(focal_pa_elev_min) <- c('ID','min_m')
hist(focal_pa_elev_min$min_m)
focal_pa_elev_max <- terra::extract(srtm_all_proj, focal_pa_proj, fun=max, method='simple', na.rm=T)
colnames(focal_pa_elev_max) <- c('ID','max_m')
hist(focal_pa_elev_max$max_m)

# merge: 
df_list <- list(focal_pa_elev_min, focal_pa_elev_max, focal_pa_elev_mean)
focal_pa_elev <- df_list %>% reduce(full_join, by='ID')
focal_pa_elev$range_m <- focal_pa_elev$max_m - focal_pa_elev$min_m
hist(focal_pa_elev$range_m)

par(mfrow=c(2,2))
hist(focal_pa_elev$min_m, main='Min')
hist(focal_pa_elev$mean_m, main='Mean')
hist(focal_pa_elev$max_m, main='Max')
hist(focal_pa_elev$range_m, main='Range')
par(mfrow=c(1,1))

focal_pa_elev_attributes <- cbind.data.frame(focal_pa_elev, as.data.frame(focal_pa))

# plot PA elevation summary by country
ggplot(data=as.data.frame(focal_pa_elev_attributes), aes(y=mean_m, x=ISO3)) +
  geom_boxplot()+
  theme_classic()+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        axis.title.x=element_blank())+
  scale_y_continuous(name='Elevation (m)', limits=c())+
  ggtitle("Mean protected area elevation by country")


ggplot(data=as.data.frame(focal_pa_elev_attributes), 
       aes(y=mean_m, x=GIS_AREA, color=ISO3)) +
  geom_point()+
  theme_classic()+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'))+
  scale_y_continuous(name='Mean elevation (m)', limits=c())+
  scale_x_continuous(name='Area (sq km)', limits=c())+
  ggtitle("Protected area elevation vs. area")

ggplot(data=as.data.frame(focal_pa_elev_attributes), 
       aes(y=max_m, x=GIS_AREA, color=ISO3)) +
  geom_point()+
  theme_classic()+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'))+
  scale_y_continuous(name='Max elevation (m)', limits=c())+
  scale_x_continuous(name='Area (sq km)', limits=c())+
  ggtitle("Protected area elevation vs. area")

ggplot(data=as.data.frame(focal_pa_elev_attributes), 
       aes(y=range_m, x=GIS_AREA, color=ISO3)) +
  geom_point()+
  theme_classic()+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'))+
  scale_y_continuous(name=' Elevation range (m)', limits=c())+
  scale_x_continuous(name='Area (sq km)', limits=c())+
  ggtitle("Protected area elevation range vs. area")

### climate exploration with WorldClim (v2.1)
# https://www.worldclim.org/data/worldclim21.html
MAT <- terra::rast("data/WorldClim/wc2.1_30s_bio/wc2.1_30s_bio_1.tif")
MAP <- terra::rast("data/WorldClim/wc2.1_30s_bio/wc2.1_30s_bio_12.tif")

MAT_masked <- terra::mask(MAT, aoi, inverse=F) 
MAT_cropped <- terra::crop(MAT_masked, aoi)
MAT_cropped <- terra::project(MAT_cropped, "EPSG:31971", 
                                         method='average', res=c())
focal_pa_MAT <- terra::extract(MAT_cropped, focal_pa_proj, fun=mean, method='simple', na.rm=T)
colnames(focal_pa_MAT) <- c('ID','mean_MAT')
hist(focal_pa_MAT$mean_MAT, main='Mean annual temp (deg C)')

focal_pa_MAT_min <- terra::extract(MAT_cropped, focal_pa_proj, fun=min, method='simple', na.rm=T)
colnames(focal_pa_MAT_min) <- c('ID','min_MAT')
hist(focal_pa_MAT_min$min_MAT, main='Min mean annual temp (deg C)')

focal_pa_MAT_max <- terra::extract(MAT_cropped, focal_pa_proj, fun=max, method='simple', na.rm=T)
colnames(focal_pa_MAT_max) <- c('ID','max_MAT')
hist(focal_pa_MAT_max$max_MAT, main='Max mean annual temp (deg C)')

MAP_masked <- terra::mask(MAP, aoi, inverse=F) 
MAP_cropped <- terra::crop(MAP_masked, aoi)
MAP_cropped <- terra::project(MAP_cropped, "EPSG:31971", 
                              method='average', res=c())
focal_pa_MAP <- terra::extract(MAP_cropped, focal_pa_proj, fun=mean, method='simple', na.rm=T)
colnames(focal_pa_MAP) <- c('ID','mean_MAP')
hist(focal_pa_MAP$mean_MAP, main='Mean annual precip (mm)')

focal_pa_MAP_min <- terra::extract(MAP_cropped, focal_pa_proj, fun=min, method='simple', na.rm=T)
colnames(focal_pa_MAP_min) <- c('ID','min_MAP')
hist(focal_pa_MAP_min$min_MAP, main='Min mean annual precip (mm)')

focal_pa_MAP_max <- terra::extract(MAP_cropped, focal_pa_proj, fun=max, method='simple', na.rm=T)
colnames(focal_pa_MAP_max) <- c('ID','max_MAP')
hist(focal_pa_MAP_max$max_MAP, main='Max mean annual precip (mm)')

clim_list <- list(focal_pa_MAT, focal_pa_MAT_min, focal_pa_MAT_max,
                  focal_pa_MAP, focal_pa_MAP_min, focal_pa_MAP_max)
clim_list <- clim_list %>% reduce(full_join, by='ID')
clim_list$MAT_range <- clim_list$max_MAT-clim_list$min_MAT
clim_list$MAP_range <- clim_list$max_MAP-clim_list$min_MAP
summary(clim_list$MAT_range)
summary(clim_list$MAP_range)
hist(clim_list$MAT_range, main='MAT (deg C) range within protected areas',
     breaks=seq(0,20,1))
hist(clim_list$MAP_range, main='MAP (mm) range within protected areas',
     breaks=seq(0,3500,100))

focal_pa_clim_att <- cbind.data.frame(clim_list, as.data.frame(focal_pa))

focal_pa_MAT_masked <- terra::mask(MAT_cropped, focal_pa_proj, inverse=F)
focal_pa_MAP_masked <- terra::mask(MAP_cropped, focal_pa_proj, inverse=F)
focal_pa_elev_masked <- terra::mask(srtm_mosaic_proj, focal_pa_proj, inverse=F)

par(mfrow=c(1,3))
hist(MAT_cropped, main='MAT', xlim=c(5,30), breaks=seq(5,30,1))
hist(focal_pa_MAT_masked, main='MAT', xlim=c(5,30), breaks=seq(5,30,1),
     col='dodgerblue', add=T)
legend('topleft', legend=c('All','Protected areas'), 
       col=c('gray','dodgerblue'), pch=c(15,15), bty='n')

hist(MAP_cropped, main='MAP', xlim=c(500,5500), breaks=seq(500,5500,100))
hist(focal_pa_MAP_masked, main='MAP', xlim=c(500,5500), breaks=seq(500,5500,100),
     col='dodgerblue', add=T)

hist(srtm_mosaic_proj, main='Elevation', xlim=c(-100,5600), breaks=seq(-100,5600,100))
hist(focal_pa_elev_masked, main='Elevation', xlim=c(-100,5600), breaks=seq(-100,5600,100),
     col='dodgerblue', add=T)



# july_tavg_hist <- terra::rast("data/WorldClim/wc2.1_30s_tavg/wc2.1_30s_tavg_07.tif")
# plot(july_tavg_hist)
# july_tavg_hist_masked <- terra::mask(july_tavg_hist, aoi, inverse=F) 
# july_tavg_hist_cropped <- terra::crop(july_tavg_hist_masked, aoi)
# july_tavg_hist_cropped <- terra::project(july_tavg_hist_cropped, "EPSG:31971", 
#                                          method='average', res=c())
# plot(july_tavg_hist_cropped)
# 
# focal_pa_july_tavg_hist <- terra::extract(july_tavg_hist_cropped, focal_pa_proj, fun=mean, method='simple', na.rm=T)
# colnames(focal_pa_july_tavg_hist) <- c('ID','mean_july_tavg_hist')
# hist(focal_pa_july_tavg_hist$mean_july_tavg_hist, main='July avg temp (deg C)')
#
# focal_pa_july_tavg_hist_att <- cbind.data.frame(focal_pa_july_tavg_hist, as.data.frame(focal_pa))


ggplot(data=as.data.frame(focal_pa_clim_att), 
       aes(y=mean_MAP, x=mean_MAT, color=ISO3, size=GIS_AREA)) +
  geom_point()+
  theme_classic()+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'))+
  scale_y_continuous(name='Mean annual precipitation (mm)', limits=c())+
  scale_x_continuous(name='Mean annual temperature (deg C)', limits=c())+
  ggtitle("Central American protected areas (1971-2000)")

# ggplot(data=as.data.frame(focal_pa_clim_elev), 
#        aes(y=mean_MAP, x=mean_MAT, color=ISO3, size=mean_m)) +
#   geom_point()+
#   theme_classic()+
#   theme(axis.text.x=element_text(color='black'),
#         axis.text.y=element_text(color='black'))+
#   scale_y_continuous(name='Mean annual precipitation (mm)', limits=c())+
#   scale_x_continuous(name='Mean annual temperature (deg C)', limits=c())+
#   ggtitle("Central American protected areas (1971-2000)")


focal_pa_clim_elev <- merge(focal_pa_clim_att, focal_pa_elev_attributes[,c(1:5)], by='ID')
ggplot(data=as.data.frame(focal_pa_clim_elev), aes(y=range_m, x=ISO3)) +
  geom_boxplot()+
  theme_classic()+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        axis.title.x=element_blank())+
  scale_y_continuous(name='Elevation (m)', limits=c())+
  ggtitle("Protected area elevation range by country")

ggplot(data=as.data.frame(focal_pa_clim_elev), aes(y=MAT_range, x=ISO3)) +
  geom_boxplot()+
  theme_classic()+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        axis.title.x=element_blank())+
  scale_y_continuous(name='Mean annual temp (C)', limits=c())+
  ggtitle("Protected area MAT range by country")

# calculate stats on MAT range within PAs
focal_pa_clim_elev %>%
  dplyr::group_by(ISO3) %>%
  dplyr::summarize(median=median(MAT_range, na.rm=T),
                   mean=mean(MAT_range, na.rm=T),
                   min=min(MAT_range, na.rm=T),
                   max=max(MAT_range, na.rm=T))

# calculate stats on MAP range within PAs
focal_pa_clim_elev %>%
  dplyr::group_by(ISO3) %>%
  dplyr::summarize(median=median(MAP_range, na.rm=T),
                   mean=mean(MAP_range, na.rm=T),
                   min=min(MAP_range, na.rm=T),
                   max=max(MAP_range, na.rm=T))

# calculate stats on elevation range within PAs
focal_pa_clim_elev %>%
  dplyr::group_by(ISO3) %>%
  dplyr::summarize(median=median(range_m, na.rm=T),
                   mean=mean(range_m, na.rm=T),
                   min=min(range_m, na.rm=T),
                   max=max(range_m, na.rm=T))

ggplot(data=as.data.frame(focal_pa_clim_elev), aes(y=MAP_range, x=ISO3)) +
  geom_boxplot()+
  theme_classic()+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        axis.title.x=element_blank())+
  scale_y_continuous(name='Mean annual precip (mm)', limits=c())+
  ggtitle("Protected area MAP range by country")


# ggplot(data=as.data.frame(focal_pa_july_tavg_hist_att), 
#        aes(y=mean_july_tavg_hist, x=GIS_AREA, color=ISO3)) +
#   geom_point()+
#   theme_classic()+
#   theme(axis.text.x=element_text(color='black'),
#         axis.text.y=element_text(color='black'))+
#   scale_y_continuous(name=' July avg historical temp (C)', limits=c())+
#   scale_x_continuous(name='Area (sq km)', limits=c())+
#   ggtitle("Protected area July temp vs. area")

###########################
#KBA_focal <- terra::intersect(KBA, focal_countries_shp)
#m <- mapview(focal_countries_shp)
#leafem::addFeatures(m, KBA)#very slow, may run out of memory if try to plot big file

