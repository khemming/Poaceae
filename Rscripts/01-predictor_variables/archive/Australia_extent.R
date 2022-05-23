library(ggmap)
library(tidyr)
library(raster)
library(dplyr)
library(rgdal)
library(maptools) # for wrldsimpl

.libPaths()

############## (1) Grid Australia
  # Aim here is to define a useable outline of Australia
  rm(list = ls())
  getwd()

  setwd("C:/Users/s436862/Dropbox/Climate Matching/1. Data files")
  
  aus <- ("Shapefiles Australia\\nsaasr9nnd_02211a04es_geo___\\aust_cd66states.shp")
  plot(aus)
  
############## (2) Shapefile: Using wrld_simpl

  setwd("C:/Users/s436862/Dropbox/Climate Matching/1. Data files")
  data(wrld_simpl)
  aus <- subset(wrld_simpl, NAME=="Australia")
  projection(aus) <- "+proj=utm +zone=48 +datum=WGS84"
  plot(aus, add = T)
  #again, this didn't actually change the extent, it just plotted it better. 
  #Anyways. Looks good. I shall save it.
  writeOGR(aus, layer = 'australia_shapefile', 'C:/Users/s436862/Dropbox/Climate Matching/1. Data files/Shapefiles Australia', driver="ESRI Shapefile", overwrite = T)
  x <- readOGR("Shapefiles Australia")
# (2) Raster: using .... R
  # I want it to these specifications:
  #class       : RasterLayer 
  #dimensions  : 4428, 5328, 23592384  (nrow, ncol, ncell)
  #resolution  : 0.008333333, 0.008333333  (x, y)
  #extent      : 110.8, 155.2, -46, -9.1  (xmin, xmax, ymin, ymax)
  #coord. ref. : +proj=utm +zone=48 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0 
  #data source : C:\Users\s436862\Dropbox\Climate Matching\1. Data files\Worldclim\precip4\bio_12_Aus.asc 
  #names       : bio_12_Aus
  
  oz <- raster(xmn=110.8, xmx=155.2, ymn=-46, ymx=-7.1)
  res(oz) <- 0.008333334
  projection(oz) <- "+proj=utm +zone=48 +datum=WGS84"
  writeRaster(oz, "Australia raster/aus.r.grd", prj = T, overwrite = T)
  writeRaster(oz, "EFs/EFs raw/aus.r.grd", prj = T, overwrite = T)
  
  aus <- raster("Australia raster/aus.r.grd")
  
############## (2) read in BIOCLIM data and crop to aus
  # Australia shapefile
  aus32 <- readOGR("Shapefiles Australia/australia_shapefile.shp")
  projection(aus32) <- "+proj=utm +zone=48 +datum=WGS84"
  
  # Mean annual precipitation (Bio12)
  precip <- raster("Worldclim\\bio1-19_30s_bil\\bio_12.bil")
  projection(precip) <- "+proj=utm +zone=48 +datum=WGS84"
  
  # crop extent to something suitable  
  new.extent <- extent(c(110.8, 155.2, -46, -7.1))
  precip3 <- crop(precip, extent(new.extent))
  
  # mask unnecessary precipitation values
  precip4 <- mask(precip3, aus32)
  plot(precip4)
  precip4
  
  writeRaster(precip4, "Worldclim/precip4/bio_12_Aus.asc", prj = T, overwrite = T)
  # can do it by .asc, .bil, .tif, etc.
  
  
#############
  
  ##### EF crop 
  # cropping EFs WGS_1984 --> Australia extent
  
  ### crop extent to something suitable: options here
  # new extent
  new.extent <- extent(c(110.8, 155.2, -46, -7.1))
  # raster I made
  aus <- raster("Rasters Australia/aus_raster.asc")
  projection(aus) <- "+proj=utm +zone=48 +datum=WGS84"
  # ggplot oz?
  oz <- borders("world", region = "Australia")
  
  
  
  bio13 <- crop(bio1, extent(new.extent))
  projection(bio13) <- "+proj=utm +zone=48 +datum=WGS84"
  # mask unnecessary bio1itation values
  bio14 <- mask(bio13, aus32)
  plot(bio14)
  bio14
  
  new.extent <- extent(c(110.8, 155.2, -46, -7.1))
  bio15 <- crop(bio14, extent(new.extent))
  plot(bio15)
  writeRaster(bio14, "Worldclim/bio14/bio_12_Aus.asc", prj = T, overwrite = T)
