################## Central American protected area analysis #######################
# Date: 6-21-23
# updated: 6-22-23
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


# Number of PAs by country
PA_count_country_summary <- as.data.frame(focal_pa) %>%
  dplyr::group_by(ISO3) %>%
  dplyr::summarize(n=n())
PA_count_country_summary <- as.data.frame(PA_count_country_summary)
PA_count_country_summary$ISO3 <- as.factor(PA_count_country_summary$ISO3)

ggplot(PA_count_country_summary, aes(x=ISO3, y=n, fill=ISO3))+
  geom_bar(stat='identity')+
  theme_classic()+
  scale_y_continuous('Count')+
  scale_x_discrete('Country')+
  scale_fill_manual(values=country_colors)+
  theme(axis.text.y=element_text(color='black'),
        axis.text.x=element_text(color='black'),
        legend.position=c('none'))+
  ggtitle('Number of protected areas by country')

## Protected area size distributions
# first all countries at once
ggplot(data=as.data.frame(focal_pa), aes(y=GIS_AREA, x=ISO3, fill=ISO3)) +
  geom_boxplot()+
  theme_classic()+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  scale_y_continuous(name='Area (sq km)', limits=c(0,1000))+
  scale_x_discrete(name='Country')+
  scale_fill_manual(values=country_colors)+
  ggtitle("Protected area size by country")

ggplot(data = focal_pa, aes(x = GIS_AREA))+ 
  geom_histogram(binwidth=50)+
  theme_classic()+
  scale_x_continuous(name='Area (sq km)', limits=c())+
  scale_y_continuous(name='Count', limits=c())+
  theme(axis.text.y=element_text(color='black'),
        axis.text.x=element_text(color='black'))+
  ggtitle('Size distribution of Central American protected areas')
summary(focal_pa$GIS_AREA)

p <- ggplot(data = focal_pa, aes(x = GIS_AREA))+ 
  geom_histogram(binwidth=50)+
  theme_classic()+
  scale_x_continuous(name='Area (sq km)', limits=c(0,1000))+
  scale_y_continuous(name='Count', limits=c(0,50))+
  theme(axis.text.y=element_text(color='black'),
        axis.text.x=element_text(color='black'))
p + facet_wrap(~ISO3)

# summary table of PA size by country
pa_by_country <- as.data.frame(focal_pa) %>%
  group_by(ISO3) %>%
  summarize(min=min(GIS_AREA, na.rm=T), pct25=quantile(GIS_AREA, 0.25, na.rm=T), median=median(GIS_AREA, na.rm=T),
            mean=mean(GIS_AREA, na.rm=T), pct75=quantile(GIS_AREA, 0.75, na.rm=T),
            max=max(GIS_AREA, na.rm=T))

ggplot(data=as.data.frame(pa_by_country), aes(y=median, x=ISO3, fill=ISO3)) +
  geom_bar(stat='identity')+
  theme_classic()+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  scale_y_continuous(name='Area (sq km)', limits=c())+
  scale_x_discrete(name='Country')+
  scale_fill_manual(values=country_colors)+
  ggtitle("Median protected area size by country")

## calculate % protection by country
# first need area of countries
# convert to Albers Equal Area Conic since we are looking at area (EPSG 9822)
# https://docs.geotools.org/latest/javadocs/org/geotools/referencing/operation/projection/AlbersEqualArea.html
focal_countries_AEAC <- terra::project(focal_countries_vect, "EPSG:9822")
focal_countries_area <- as.data.frame(terra::expanse(focal_countries_AEAC, 
                                                   unit='km', transform=T))# using transform=T seems to be closer match to areas I find online (which likely include islands)
focal_countries_area$Country <- focal_countries_shp$iso_a3
names(focal_countries_area) <- c('CountryArea_sqkm','ISO3')

# then get % protection by country
total_protection_country <- as.data.frame(focal_pa) %>%
  dplyr::group_by(ISO3) %>%
  dplyr::summarize(protected_sqkm=sum(GIS_AREA, na.rm=T))
total_protection_country <- as.data.frame(total_protection_country)

pct_protection_country <- merge(total_protection_country, focal_countries_area, by='ISO3')
pct_protection_country$pct_protected <- (pct_protection_country$protected_sqkm/pct_protection_country$CountryArea_sqkm)*100

ggplot(pct_protection_country, aes(x=ISO3, y=pct_protected, fill=ISO3))+
  geom_bar(stat='identity')+
  theme_classic()+
  scale_y_continuous('Percent protected', limits=c(0,100))+
  scale_x_discrete('Country')+
  scale_fill_manual(values=country_colors7)+
  theme(axis.text.y=element_text(color='black'),
        axis.text.x=element_text(color='black'),
        legend.position=c('none'))+
  geom_hline(yintercept=30, linetype="dashed", 
             color = "black", size=1)+
  annotate("text", x='SLV', y=34, label='30x30 target')+
  ggtitle('Protected area coverage by country')

## correct % protection (deal with overlapping polygons, coastal spillover)
# read in clipped/dissolved PA polygons from Dissolve Boundaries in ArcGIS
pa_dissolved <- terra::vect("data/spatial/protected_areas/focal_pa_31971_onland_DissolveBoundaries2_SJ.shp")
plot(focal_countries_vect, col='tan')
plot(pa_dissolved, col='gold',add=T)
pa_dissolved_AEAC <- terra::project(pa_dissolved, "EPSG:9822")

pa_singlepart <- terra::vect("data/spatial/protected_areas/focal_pa_31971_onland_singlepart2.shp")
# 3 polygons are CRI;PAN...I looked at map and 2 are fully in CR and one is majority in CR, so can assign all to CR
pa_singlepart$ISO3 <- ifelse(pa_singlepart$ISO3=='CRI;PAN', 'CRI', pa_singlepart$ISO3)

pa_singlepart_country_count <- as.data.frame(pa_singlepart) %>%
  dplyr::group_by(ISO3) %>%
  dplyr::summarize(nSinglepart=n())
pa_singlepart_country_count <- as.data.frame(pa_singlepart_country_count)

# # tes(ting on one country; should compare to protected planet report
# panama <- terra::subset(focal_countries_AEAC,
#                         focal_countries_AEAC$iso_a3=="PAN", NSE=T)
# pa_inter <- terra::intersect(pa_dissolved_AEAC, panama)
# xpanse <- terra::expanse(pa_inter, unit='km')
# sum(xpanse, na.rm=T)
# min(xpanse, na.rm=T)
# contig <- length(terra::expanse(pa_inter, unit='km'))
# min <- min()
# plot(panama)
# plot(pa_inter, add=T, col='red')

countries <- unique(focal_countries_AEAC$iso_a3)
dissolved_protected_df <- data.frame(ISO3=countries, 
                                     corrected_protected_sqkm=NA, 
                                     contigPAs=NA,
                                     min=NA, median=NA,
                                     mean=NA, max=NA)
for (i in 1:length(countries)) {
  country_sub <- terra::subset(focal_countries_AEAC,
                               focal_countries_AEAC$iso_a3==countries[i], NSE=T)
  pa_inter <- terra::intersect(pa_dissolved_AEAC, country_sub)
  xpanse <- terra::expanse(pa_inter, unit='km')
  areasum <- sum(xpanse, na.rm=T)
  dissolved_protected_df[i,2] <- areasum
  contig <- length(xpanse)
  dissolved_protected_df[i,3] <- contig
  min <- min(xpanse, na.rm=T)
  dissolved_protected_df[i,4] <- min
  median <- median(xpanse, na.rm=T)
  dissolved_protected_df[i,5] <- median
  mean <- mean(xpanse, na.rm=T)
  dissolved_protected_df[i,6] <- mean
  max <- max(xpanse, na.rm=T)
  dissolved_protected_df[i,7] <- max
  areasum=NULL
  pa_inter=NULL
  country_sub=NULL
  contig=NULL
  min=NULL
  max=NULL
  median=NULL
  mean=NULL
}

pct_protection_country <- merge(pct_protection_country, dissolved_protected_df, by='ISO3')
pct_protection_country$corrected_pct_protected <- (pct_protection_country$corrected_protected_sqkm/pct_protection_country$CountryArea_sqkm)*100

pct_protection_country <- merge(pct_protection_country, PA_count_country_summary, by='ISO3', all=F) 
names(pct_protection_country)[names(pct_protection_country) == 'n'] <- 'n_undissolved'
pct_protection_country <- merge(pct_protection_country, pa_singlepart_country_count, by='ISO3', all=F)

ggplot(pct_protection_country, aes(x=ISO3, y=corrected_pct_protected, fill=ISO3))+
  geom_bar(stat='identity')+
  theme_classic()+
  scale_y_continuous('Percent protected', limits=c(0,100))+
  scale_x_discrete('Country')+
  scale_fill_manual(values=country_colors7)+
  theme(axis.text.y=element_text(color='black'),
        axis.text.x=element_text(color='black'),
        legend.position=c('none'))+
  geom_hline(yintercept=30, linetype="dashed", 
             color = "black", size=1)+
  annotate("text", x='SLV', y=34, label='30x30 target')+
  ggtitle('Protected area coverage by country')

compare_protected_counts <- pct_protection_country[,c('ISO3','contigPAs','n_undissolved','nSinglepart')]
compare_protected_counts <- reshape2::melt(compare_protected_counts, variable.name='Dataset', value.name='Count')

ggplot(compare_protected_counts, aes(x=ISO3, y=Count, fill=Dataset))+
  geom_bar(stat='identity', position='dodge')+
  theme_classic()+
  scale_y_continuous('Number of protected areas', limits=c())+
  scale_x_discrete('Country')+
  scale_fill_discrete(labels=c('Contiguous','Unadjusted','Protected patches'))+
  theme(axis.text.y=element_text(color='black'),
        axis.text.x=element_text(color='black'),
        legend.position=c(0.8,0.8),
        legend.title=element_blank())+
  ggtitle('Protected areas by country')

plot(pct_protection_country$contigPAs ~ pct_protection_country$corrected_protected_sqkm)

# size distribution of contiguous PAs
contiguous_PA_area <- terra::expanse(pa_dissolved_AEAC, unit='km') 
hist(contiguous_PA_area, breaks=seq(0,25000,100))
summary(contiguous_PA_area)

contiguous_PA_size_list <- list()
for (i in 1:length(countries)) {
  country_sub <- terra::subset(focal_countries_AEAC,
                               focal_countries_AEAC$iso_a3==countries[i], NSE=T)
  pa_inter <- terra::intersect(pa_dissolved_AEAC, country_sub)
  xxpanse <- terra::expanse(pa_inter, unit='km')
  temp_df <- data.frame(areasqkm=xxpanse, ISO3=countries[i])
  contiguous_PA_size_list[[i]] <- temp_df
  temp_df=NULL
  pa_inter=NULL
  xxpanse=NULL
  country_sub=NULL
}

contiguous_PA_area_df <- bind_rows(contiguous_PA_size_list, .id = 'ISO3')
contiguous_PA_area_df <- contiguous_PA_area_df %>% mutate(ISO3=recode(ISO3, 
                         `1`="CRI",
                         `2`="NIC",
                         '3'="SLV",
                         '4'="GTM",
                         '5'="HND",
                         '6'="BLZ",
                         '7'="PAN"))
contiguous_PA_area_df %>% count(ISO3)#verify
contiguous_PA_area_df <- as.data.frame(contiguous_PA_area_df)

ggplot(data=as.data.frame(contiguous_PA_area_df), aes(y=areasqkm, x=ISO3, fill=ISO3)) +
  geom_boxplot()+
  theme_classic()+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  scale_y_continuous(name='Area (sq km)', limits=c(0,100))+
  scale_x_discrete(name='Country')+
  scale_fill_manual(values=country_colors7)+
  ggtitle("Protected area size by country")

q <- c(0,0.25, 0.5, 0.75,1)
contiguous_PA_area_stats <- contiguous_PA_area_df %>%
  dplyr::group_by(ISO3) %>%
  dplyr::summarize(q1 = quantile(areasqkm, probs = q[1]),
            q25 = quantile(areasqkm, probs = q[2]), 
            q50 = quantile(areasqkm, probs = q[3]),
            q75 = quantile(areasqkm, probs = q[4]),
            q100 = quantile(areasqkm, probs = q[5]))
contiguous_PA_area_stats <- as.data.frame(contiguous_PA_area_stats)
#write.csv(contiguous_PA_area_stats, file='Tables_Figures/contiguous_PA_area_stats.csv', row.names=F)

##### Representation of PAs along climate/elevational gradients #####
srtm_all <- terra::rast("data/spatial/SRTM90_V4/SRTM90_V4.elevation_all.tif")
srtm_all_proj <- terra::project(srtm_all, "EPSG:31971", 
                                method='average', res=c(90,90))
srtm_all_proj_mask <- terra::mask(srtm_all_proj, aoi_vect, inverse=F)

# not sure why it reports all values instead of the mean as specified, but this is OK
focal_pa_elev_mean <- terra::extract(srtm_all_proj, pa_dissolved, fun=mean, na.rm=T)
colnames(focal_pa_elev_mean) <- c('ID','elev_m')

# create summary by contiguous PA; can ignore warnings; a few small ones may not have elevation data
focal_pa_elev <- focal_pa_elev_mean %>%
  dplyr::group_by(ID) %>%
  dplyr::summarize(nCells=n(), mean_m=mean(elev_m, na.rm=T), 
                   min_m=min(elev_m, na.rm=T), 
                   median_m=median(elev_m, na.rm=T),
                   max_m=max(elev_m, na.rm=T))
focal_pa_elev$range_m <- focal_pa_elev$max_m- focal_pa_elev$min_m

hist(focal_pa_elev$mean_m)
hist(focal_pa_elev$min_m)
hist(focal_pa_elev$max_m)
hist(focal_pa_elev$median_m)
hist(focal_pa_elev$range_m)

## same for climate data
MAT <- terra::rast("data/WorldClim/wc2.1_30s_bio/wc2.1_30s_bio_1.tif")
MAP <- terra::rast("data/WorldClim/wc2.1_30s_bio/wc2.1_30s_bio_12.tif")

# prepare/reproject data ot UTM 17N
MAT_masked <- terra::mask(MAT, aoi, inverse=F) 
MAT_cropped <- terra::crop(MAT_masked, aoi)
MAT_cropped <- terra::project(MAT_cropped, "EPSG:31971", 
                              method='average', res=c())

MAP_masked <- terra::mask(MAP, aoi, inverse=F) 
MAP_cropped <- terra::crop(MAP_masked, aoi)
MAP_cropped <- terra::project(MAP_cropped, "EPSG:31971", 
                              method='average', res=c())

## extract MAT data (degC)
# for MAT and MAP, function issues warnings for a few very small PAs that don't have data; this is OK
focal_pa_MATx <- terra::extract(MAT_cropped, pa_dissolved, fun=mean, na.rm=T)
colnames(focal_pa_MATx) <- c('ID','MAT')

# create summary by contiguous PA; can ignore warnings; a few small ones may not have MATation data
focal_pa_MAT <- focal_pa_MATx %>%
  dplyr::group_by(ID) %>%
  dplyr::summarize(nCellsMAT=n(), mean_MAT=mean(MAT, na.rm=T), 
                   min_MAT=min(MAT, na.rm=T), 
                   median_MAT=median(MAT, na.rm=T),
                   max_MAT=max(MAT, na.rm=T))
focal_pa_MAT$range_MAT <- focal_pa_MAT$max_MAT- focal_pa_MAT$min_MAT

hist(focal_pa_MAT$mean_MAT)
hist(focal_pa_MAT$min_MAT)
hist(focal_pa_MAT$max_MAT)
hist(focal_pa_MAT$median_MAT)
hist(focal_pa_MAT$range_MAT)

# same for MAP (mm)
focal_pa_MAPx <- terra::extract(MAP_cropped, pa_dissolved, fun=mean, na.rm=T)
colnames(focal_pa_MAPx) <- c('ID','MAP')

# create summary by contiguous PA; can ignore warnings; a few small ones may not have MAPation data
focal_pa_MAP <- focal_pa_MAPx %>%
  dplyr::group_by(ID) %>%
  dplyr::summarize(nCellsMAP=n(), mean_MAP=mean(MAP, na.rm=T), 
                   min_MAP=min(MAP, na.rm=T), 
                   median_MAP=median(MAP, na.rm=T),
                   max_MAP=max(MAP, na.rm=T))
focal_pa_MAP$range_MAP <- focal_pa_MAP$max_MAP- focal_pa_MAP$min_MAP

hist(focal_pa_MAP$mean_MAP)
hist(focal_pa_MAP$min_MAP)
hist(focal_pa_MAP$max_MAP)
hist(focal_pa_MAP$median_MAP)
hist(focal_pa_MAP$range_MAP)

# combine elev, MAT and MAP data
df_list <- list(focal_pa_elev, focal_pa_MAT, focal_pa_MAP)
focal_pa_elev_clim <- df_list %>% reduce(full_join, by='ID')

## plots
ggplot(data = focal_pa_elev_clim, aes(x = range_m))+ 
  geom_histogram(binwidth=10)+
  theme_classic()+
  scale_x_continuous(name='Elevation range (m)', limits=c())+
  scale_y_continuous(name='Count', limits=c())+
  theme(axis.text.y=element_text(color='black'),
        axis.text.x=element_text(color='black'))+
  ggtitle('Elevation range')

ggplot(data = focal_pa_elev_clim, aes(x = mean_MAT))+ 
  geom_histogram(binwidth=1)+
  theme_classic()+
  scale_x_continuous(name='Mean annual temperature range (C)', limits=c())+
  scale_y_continuous(name='Count', limits=c(0,150))+
  theme(axis.text.y=element_text(color='black'),
        axis.text.x=element_text(color='black'))+
  ggtitle('Temperature range')

ggplot(data = focal_pa_elev_clim, aes(x = mean_MAP))+ 
  geom_histogram(binwidth=10)+
  theme_classic()+
  scale_x_continuous(name='Mean annual precipitation range (mm)', limits=c())+
  scale_y_continuous(name='Count', limits=c())+
  theme(axis.text.y=element_text(color='black'),
        axis.text.x=element_text(color='black'))+
  ggtitle('Precipitation range')


## merging PA attributes to climate and elevation data
test <- terra::as.data.frame(pa_dissolved)
testvars <- test[,c('iso_a3','name_en')]
focal_pa_elev_clim <- cbind.data.frame(focal_pa_elev_clim, testvars, contiguous_PA_area)

ggplot(data=focal_pa_elev_clim, 
       aes(y=mean_MAP, x=mean_MAT, color=iso_a3, size=contiguous_PA_area)) +
  geom_point()+
  theme_classic()+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'))+
  scale_x_continuous(name='Mean annual temperature (C)', limits=c())+
  scale_y_continuous(name='Mean annual precipitation (mm)', limits=c())+
  scale_color_manual(name='Country', values=country_colors)+
  scale_size_binned(name='Area (sq km)', breaks=c(1000,5000,10000,15000,20000,25000))+
  ggtitle("Central American protected area climate")

# multipanel plot
mean_elev <- ggplot(data=as.data.frame(focal_pa_elev_clim), aes(y=mean_m, x=iso_a3, fill=iso_a3)) +
  geom_boxplot()+
  theme_classic()+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'),
        axis.title.x=element_blank())+
  scale_y_continuous(name='Elevation (m)', limits=c())+
  #scale_x_discrete(name='Country')+
  scale_fill_manual(values=country_colors7)+
  ggtitle("Protected area elevation (mean)")

range_elev <- ggplot(data=as.data.frame(focal_pa_elev_clim), aes(y=range_m, x=iso_a3, fill=iso_a3)) +
  geom_boxplot()+
  theme_classic()+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'),
        axis.title.x=element_blank())+
  scale_y_continuous(name='Elevation (m)', limits=c())+
  scale_x_discrete(name='Country')+
  scale_fill_manual(values=country_colors7)+
  ggtitle("Protected area elevation (range)")


mean_MAT <- ggplot(data=as.data.frame(focal_pa_elev_clim), aes(y=mean_MAT, x=iso_a3, fill=iso_a3)) +
  geom_boxplot()+
  theme_classic()+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'),
        axis.title.x=element_blank())+
  scale_y_continuous(name='Temperature (C)', limits=c())+
  scale_x_discrete(name='Country')+
  scale_fill_manual(values=country_colors7)+
  ggtitle("Protected area temperature (mean)")

range_MAT <- ggplot(data=as.data.frame(focal_pa_elev_clim), aes(y=range_MAT, x=iso_a3, fill=iso_a3)) +
  geom_boxplot()+
  theme_classic()+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'),
        axis.title.x=element_blank())+
  scale_y_continuous(name='Temperature (C)', limits=c())+
  scale_x_discrete(name='Country')+
  scale_fill_manual(values=country_colors7)+
  ggtitle("Protected area temperature (range)")

mean_MAP <- ggplot(data=as.data.frame(focal_pa_elev_clim), aes(y=mean_MAP, x=iso_a3, fill=iso_a3)) +
  geom_boxplot()+
  theme_classic()+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'),
        axis.title.x=element_blank())+
  scale_y_continuous(name='Precipitation (mm)', limits=c())+
  scale_x_discrete(name='Country')+
  scale_fill_manual(values=country_colors7)+
  ggtitle("Protected area precipitation (mean)")

range_MAP <- ggplot(data=as.data.frame(focal_pa_elev_clim), aes(y=range_MAP, x=iso_a3, fill=iso_a3)) +
  geom_boxplot()+
  theme_classic()+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'),
        axis.title.x=element_blank())+
  scale_y_continuous(name='Precipitation (mm)', limits=c())+
  scale_x_discrete(name='Country')+
  scale_fill_manual(values=country_colors7)+
  ggtitle("Protected area precipitation (range)")

grid.arrange(mean_elev, range_elev,
             mean_MAT, range_MAT,
             mean_MAP, range_MAP, nrow=3)

# uses sample of data, so maybe not the best visual
# hist(srtm_all_proj_mask$SRTM90_V4.elevation_all, xlim=c(0,4500),
#      breaks=seq(-100,4500,100))
# hist(focal_pa_elev_mean$elev_m, col='dodgerblue',  xlim=c(0,4500),
#      breaks=seq(-100,4500,100))

hist(MAT_cropped)
hist(focal_pa_elev_clim$mean_MAT, col='dodgerblue', add=T)
hist(focal_pa_elev_clim$mean_MAT)

# # clearly, some coastal protected spill into the ocean and those will mess up area calculations
# focal_pa_onland <- terra::mask(focal_pa_vect, aoi_vect, inverse=F)
# #focal_pa_onland <- terra::erase(focal_pa_vect, focal_pa_onland)#CRASH!
# plot(focal_pa_onland)
# plot(aoi_vect, col='firebrick', add=T)
# # all of the st_ functions throw an error about degenerate Edge 171 duplicate vertex
# # seems like an issue with the underlying polygons from WDPA
# test <- st_intersects(focal_pa, aoi)

