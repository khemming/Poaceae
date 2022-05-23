# date created: 17/7/17
# last updated: 19/9/18

# Cropping rasters to Australia region

  rm(list = ls())
  
  library(raster)
  library(ggplot2)
  library(ggmap)
  library(tidyr)
  library(rgdal)
  library(maptools)
  
  setwd("C:/Users/s436862/Dropbox/Climate Matching/Data files")

# Cropping -----------------------------------------------------
# Note: memory is limited, so I am completing this five rasters at a time, clearing environment between with 'set-up'.  
  
# 1-5 ------------------------------  
# Set up -------------------------------------------------------------  
# Template: 1-km resolution Australia raster
  rm(list = ls())
  aus <- raster("Australia/aus.grd")
  
# Template: shapefile
  aus.shp <- readOGR("Australia/australia_shapefile.shp")  
  
# Useful extent
  ext <- c(118, 152, -40, -11)
  
# Projection
  pro.crs <- "+proj=utm +zone=48 +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
# AMT -------------------------------------------
  amt <- raster("EFs/Rasters 1-km non-cropped/amt.grd")
  amt.c <- crop(amt, aus.shp)
  plot(amt.c)
  
  amt.pro <- projectRaster(amt.c, aus, res = 0.01, crs = pro.crs, method="bilinear")
  plot(amt.pro)
  
  amt.m <- mask(amt.pro, aus.shp)
  plot(amt.m)
  
  writeRaster(amt.m, "EFs/Rasters 1-km Aus-cropped/amt.grd", prj = T, overwrite = T)  
  
# MDR --------------------------
  mdr <- raster("EFs/Rasters 1-km non-cropped/mdr.grd") 
  mdr.c <- crop(mdr, aus.shp)
  plot(mdr.c)
  
  mdr.pro <- projectRaster(mdr.c, aus, res = 0.01, crs = pro.crs, method="bilinear")
  plot(mdr.pro)
  
  mdr.m <- mask(mdr.pro, aus.shp)
  plot(mdr.m)
  
  writeRaster(mdr.m, "EFs/Rasters 1-km Aus-cropped/mdr.grd", prj = T, overwrite = T)  
  
# ISO ---------------------------------------------------------------------------  
  iso <- raster("EFs/Rasters 1-km non-cropped/iso.grd")
  iso.c <- crop(iso, aus.shp)
  plot(iso.c)
  
  iso.pro <- projectRaster(iso.c, aus, res = 0.01, crs = pro.crs, method="bilinear")
  plot(iso.pro)
  
  iso.m <- mask(iso.pro, aus.shp)
  plot(iso.m)
  
  writeRaster(iso.m, "EFs/Rasters 1-km Aus-cropped/iso.grd", prj = T, overwrite = T)  

# TS -----------------------------------------------------------------------      
  ts <- raster("EFs/Rasters 1-km non-cropped/ts.grd")
  ts.c <- crop(ts, aus.shp)
  plot(ts.c)
  
  ts.pro <- projectRaster(ts.c, aus, res = 0.01, crs = pro.crs, method="bilinear")
  plot(ts.pro)
  
  ts.m <- mask(ts.pro, aus.shp)
  plot(ts.m)
  
  writeRaster(ts.m, "EFs/Rasters 1-km Aus-cropped/ts.grd", prj = T, overwrite = T)  
  
# TAR --------------------------------------------------------------  
  tar <- raster("EFs/Rasters 1-km non-cropped/ts.grd")
  tar.c <- crop(tar, aus.shp)
  plot(tar.c)
  
  tar.pro <- projectRaster(tar.c, aus, res = 0.01, crs = pro.crs, method="bilinear")
  plot(tar.pro)
  
  tar.m <- mask(tar.pro, aus.shp)
  plot(tar.m)
  
  writeRaster(tar.m, "EFs/Rasters 1-km Aus-cropped/tar.grd", prj = T, overwrite = T)  
  
# ----------------------------------
  
# 6-10 ---------------------------
# Set up -------------------------------------------------------------  
# Template: 1-km resolution Australia raster
  rm(list = ls())
  aus <- raster("Australia/aus.grd")
  
# Template: shapefile
  aus.shp <- readOGR("Australia/australia_shapefile.shp")  
  
# Useful extent
  ext <- c(118, 152, -40, -11)
  
# Projection
  pro.crs <- "+proj=utm +zone=48 +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
# TWARMM --------------------------------------------------------------    
  twarmm <- raster("EFs/Rasters 1-km non-cropped/twarmm.grd")
  twarmm.c <- crop(twarmm, aus.shp)
  plot(twarmm.c)
  
  twarmm.pro <- projectRaster(twarmm.c, aus, res = 0.01, crs = pro.crs, method="bilinear")
  plot(twarmm.pro)
  
  twarmm.m <- mask(twarmm.pro, aus.shp)
  plot(twarmm.m)
  
  writeRaster(twarmm.m, "EFs/Rasters 1-km Aus-cropped/twarmm.grd", prj = T, overwrite = T)  
  
# TCOLDM -------------------------------------------------------------
  tcoldm <- raster("EFs/Rasters 1-km non-cropped/tcoldm.grd")
  tcoldm.c <- crop(tcoldm, aus.shp)
  plot(tcoldm.c)
  
  tcoldm.pro <- projectRaster(tcoldm.c, aus, res = 0.01, crs = pro.crs, method="bilinear")
  plot(tcoldm.pro)
  
  tcoldm.m <- mask(tcoldm.pro, aus.shp)
  plot(tcoldm.m)
  
  writeRaster(tcoldm.m, "EFs/Rasters 1-km Aus-cropped/tcoldm.grd", prj = T, overwrite = T)  
  
# TWETQ ------------------------------------------------------------------
  twetq <- raster("EFs/Rasters 1-km non-cropped/twetq.grd")
  twetq.c <- crop(twetq, aus.shp)
  plot(twetq.c)
  
  twetq.pro <- projectRaster(twetq.c, aus, res = 0.01, crs = pro.crs, method="bilinear")
  plot(twetq.pro)
  
  twetq.m <- mask(twetq.pro, aus.shp)
  plot(twetq.m)
  
  writeRaster(twetq.m, "EFs/Rasters 1-km Aus-cropped/twetq.grd", prj = T, overwrite = T)  
  
# TDRYQ ----------------------------------------------------------------------  
  tdryq <- raster("EFs/Rasters 1-km non-cropped/tdryq.grd")
  tdryq.c <- crop(tdryq, aus.shp)
  plot(tdryq.c)
  
  tdryq.pro <- projectRaster(tdryq.c, aus, res = 0.01, crs = pro.crs, method="bilinear")
  plot(tdryq.pro)
  
  tdryq.m <- mask(tdryq.pro, aus.shp)
  plot(tdryq.m)
  
  writeRaster(tdryq.m, "EFs/Rasters 1-km Aus-cropped/tdryq.grd", prj = T, overwrite = T)  
  
# TWARMQ --------------------------------------------------------------------
  twarmq <- raster("EFs/Rasters 1-km non-cropped/twarmq.grd")
  twarmq.c <- crop(twarmq, aus.shp)
  plot(twarmq.c)
  
  twarmq.pro <- projectRaster(twarmq.c, aus, res = 0.01, crs = pro.crs, method="bilinear")
  plot(twarmq.pro)
  
  twarmq.m <- mask(twarmq.pro, aus.shp)
  plot(twarmq.m)
  
  writeRaster(twarmq.m, "EFs/Rasters 1-km Aus-cropped/twarmq.grd", prj = T, overwrite = T)  
# ----------------------------------
  
# 11-15 --------------------------- 
# Set up -------------------------------------------------------------  
# Template: 1-km resolution Australia raster
  rm(list = ls())
  aus <- raster("Australia/aus.grd")
  
# Template: shapefile
  aus.shp <- readOGR("Australia/australia_shapefile.shp")  
  
# Useful extent
  ext <- c(118, 152, -40, -11)
  
# Projection
  pro.crs <- "+proj=utm +zone=48 +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
# TCOLDQ -------------------------------------------------------------------  
  tcoldq <- raster("EFs/Rasters 1-km non-cropped/tcoldq.grd")
  tcoldq.c <- crop(tcoldq, aus.shp)
  plot(tcoldq.c)
  
  tcoldq.pro <- projectRaster(tcoldq.c, aus, res = 0.01, crs = pro.crs, method="bilinear")
  plot(tcoldq.pro)
  
  tcoldq.m <- mask(tcoldq.pro, aus.shp)
  plot(tcoldq.m)
  
  writeRaster(tcoldq.m, "EFs/Rasters 1-km Aus-cropped/tcoldq.grd", prj = T, overwrite = T)  
# AP -----------------------------------------------------------------------
  ap <- raster("EFs/Rasters 1-km non-cropped/ap.grd")
  ap.c <- crop(ap, aus.shp)
  plot(ap.c)
  
  ap.pro <- projectRaster(ap.c, aus, res = 0.01, crs = pro.crs, method="bilinear")
  plot(ap.pro)
  
  ap.m <- mask(ap.pro, aus.shp)
  plot(ap.m)
  
  writeRaster(ap.m, "EFs/Rasters 1-km Aus-cropped/ap.grd", prj = T, overwrite = T)  
# PWETM --------------------------------------------------------------------
  pwetm <- raster("EFs/Rasters 1-km non-cropped/pwetm.grd")
  pwetm.c <- crop(pwetm, aus.shp)
  plot(pwetm.c)
  
  pwetm.pro <- projectRaster(pwetm.c, aus, res = 0.01, crs = pro.crs, method="bilinear")
  plot(pwetm.pro)
  
  pwetm.m <- mask(pwetm.pro, aus.shp)
  plot(pwetm.m)
  
  writeRaster(pwetm.m, "EFs/Rasters 1-km Aus-cropped/pwetm.grd", prj = T, overwrite = T)  
# PDRYM --------------------------------------------------------------------
  pdrym <- raster("EFs/Rasters 1-km non-cropped/pdrym.grd")
  pdrym.c <- crop(pdrym, aus.shp)
  plot(pdrym.c)
  
  pdrym.pro <- projectRaster(pdrym.c, aus, res = 0.01, crs = pro.crs, method="bilinear")
  plot(pdrym.pro)
  
  pdrym.m <- mask(pdrym.pro, aus.shp)
  plot(pdrym.m)
  
  writeRaster(pdrym.m, "EFs/Rasters 1-km Aus-cropped/pdrym.grd", prj = T, overwrite = T)  
# PS ------------------------------------------------------------------------  
  ps <- raster("EFs/Rasters 1-km non-cropped/ps.grd")
  ps.c <- crop(ps, aus.shp)
  plot(ps.c)
  
  ps.pro <- projectRaster(ps.c, aus, res = 0.01, crs = pro.crs, method="bilinear")
  plot(ps.pro)
  
  ps.m <- mask(ps.pro, aus.shp)
  plot(ps.m)
  
  writeRaster(ps.m, "EFs/Rasters 1-km Aus-cropped/ps.grd", prj = T, overwrite = T)  
# ---------------------------------  

# 16-20 ------------------------- 
# Set up -------------------------------------------------------------  
# Template: 1-km resolution Australia raster
  rm(list = ls())
  aus <- raster("Australia/aus.grd")
  
# Template: shapefile
  aus.shp <- readOGR("Australia/australia_shapefile.shp")  
  
# Useful extent
  ext <- c(118, 152, -40, -11)
  
# Projection
  pro.crs <- "+proj=utm +zone=48 +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"    
# PWETQ ---------------------------------------------------------------------
  pwetq <- raster("EFs/Rasters 1-km non-cropped/pwetq.grd")
  pwetq.c <- crop(pwetq, aus.shp)
  plot(pwetq.c)
  
  pwetq.pro <- projectRaster(pwetq.c, aus, res = 0.01, crs = pro.crs, method="bilinear")
  plot(pwetq.pro)
  
  pwetq.m <- mask(pwetq.pro, aus.shp)
  plot(pwetq.m)
  
  writeRaster(pwetq.m, "EFs/Rasters 1-km Aus-cropped/pwetq.grd", prj = T, overwrite = T)  
# PDRYQ ----------------------------------------------------------------------  
  pdryq <- raster("EFs/Rasters 1-km non-cropped/pdryq.grd")
  pdryq.c <- crop(pdryq, aus.shp)
  plot(pdryq.c)
  
  pdryq.pro <- projectRaster(pdryq.c, aus, res = 0.01, crs = pro.crs, method="bilinear")
  plot(pdryq.pro)
  
  pdryq.m <- mask(pdryq.pro, aus.shp)
  plot(pdryq.m)
  
  writeRaster(pdryq.m, "EFs/Rasters 1-km Aus-cropped/pdryq.grd", prj = T, overwrite = T)  
# PWARMQ --------------------------------------------------------------------  
  pwarmq <- raster("EFs/Rasters 1-km non-cropped/pwarmq.grd")
  pwarmq.c <- crop(pwarmq, aus.shp)
  plot(pwarmq.c)
  
  pwarmq.pro <- projectRaster(pwarmq.c, aus, res = 0.01, crs = pro.crs, method="bilinear")
  plot(pwarmq.pro)
  
  pwarmq.m <- mask(pwarmq.pro, aus.shp)
  plot(pwarmq.m)
  
  writeRaster(pwarmq.m, "EFs/Rasters 1-km Aus-cropped/pwarmq.grd", prj = T, overwrite = T)  
# PCOLDQ ---------------------------------------------------------------------  
  pcoldq <- raster("EFs/Rasters 1-km non-cropped/pcoldq.grd")
  pcoldq.c <- crop(pcoldq, aus.shp)
  plot(pcoldq.c)
  
  pcoldq.pro <- projectRaster(pcoldq.c, aus, res = 0.01, crs = pro.crs, method="bilinear")
  plot(pcoldq.pro)
  
  pcoldq.m <- mask(pcoldq.pro, aus.shp)
  plot(pcoldq.m)
  
  writeRaster(pcoldq.m, "EFs/Rasters 1-km Aus-cropped/pcoldq.grd", prj = T, overwrite = T)  
  
# ARIDITY --------------------------------------------------------------------------  
  arid <- raster("EFs/Rasters 1-km non-cropped/arid.grd")
  arid.c <- crop(arid, aus.shp)
  plot(arid.c)
  
  arid.pro <- projectRaster(arid.c, aus, res = 0.01, crs = pro.crs, method="bilinear")
  plot(arid.pro)
  
  arid.m <- mask(arid.pro, aus.shp)
  plot(arid.m)
  
  writeRaster(arid.m, "EFs/Rasters 1-km Aus-cropped/arid.grd", prj = T, overwrite = T)  
  
  
# -------------------------------  
  
# 21-25 ------------------------- 
# Set up -------------------------------------------------------------  
# Template: 1-km resolution Australia raster
  rm(list = ls())
  aus <- raster("Australia/aus.grd")
  
# Template: shapefile
  aus.shp <- readOGR("Australia/australia_shapefile.shp")  
  
# Useful extent
  ext <- c(118, 152, -40, -11)
  
# Projection
  pro.crs <- "+proj=utm +zone=48 +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"      
# ELEVATION -------------------------------------------------------------------   
  elev <- raster("EFs/Rasters 1-km non-cropped/elev.grd")
  elev.c <- crop(elev, aus.shp)
  plot(elev.c)
  
  elev.pro <- projectRaster(elev.c, aus, res = 0.01, crs = pro.crs, method="bilinear")
  plot(elev.pro)
  
  elev.m <- mask(elev.pro, aus.shp)
  plot(elev.m)
  
  writeRaster(elev.m, "EFs/Rasters 1-km Aus-cropped/elev.grd", prj = T, overwrite = T)  
  
# PET -------------------------------------------------------------------------------
  pet <- raster("EFs/Rasters 1-km non-cropped/pet.grd")
  pet.c <- crop(pet, aus.shp)
  plot(pet.c)
  
  pet.pro <- projectRaster(pet.c, aus, res = 0.01, crs = pro.crs, method="bilinear")
  plot(pet.pro)
  
  pet.m <- mask(pet.pro, aus.shp)
  plot(pet.m)
  
  writeRaster(pet.m, "EFs/Rasters 1-km Aus-cropped/pet.grd", prj = T, overwrite = T)  
# HII ------------------------------------------------------------------------------
  hii <- raster("EFs/Rasters 1-km non-cropped/hii.grd")
  hii.c <- crop(hii, aus.shp)
  plot(hii.c)
  
  hii.pro <- projectRaster(hii.c, aus, res = 0.01, crs = pro.crs, method="bilinear")
  plot(hii.pro)
  
  hii.m <- mask(hii.pro, aus.shp)
  plot(hii.m)
  
  writeRaster(hii.m, "EFs/Rasters 1-km Aus-cropped/hii.grd", prj = T, overwrite = T)  
  
# RZ -------------------------------------------------------------------------------  
  rz <- raster("EFs/Rasters 1-km non-cropped/rz.grd")
  rz.c <- crop(rz, aus.shp)
  plot(rz.c)
  
  rz.pro <- projectRaster(rz.c, aus, res = 0.01, crs = pro.crs, method="bilinear")
  plot(rz.pro)
  
  rz.m <- mask(rz.pro, aus.shp)
  plot(rz.m)
  
  writeRaster(rz.m, "EFs/Rasters 1-km Aus-cropped/rz.grd", prj = T, overwrite = T)  
  
# SP ----------------------------------------------------------------------------  
  sp <- raster("EFs/Rasters 1-km non-cropped/sp.grd")
  sp.c <- crop(sp, aus.shp)
  plot(sp.c)
  
  sp.pro <- projectRaster(sp.c, aus, res = 0.01, crs = pro.crs, method="bilinear")
  plot(sp.pro)
  
  sp.m <- mask(sp.pro, aus.shp)
  plot(sp.m)
  
  writeRaster(sp.m, "EFs/Rasters 1-km Aus-cropped/sp.grd", prj = T, overwrite = T)  
  
  
# -------------------------------
  
# 26-29 ------------------------- 
# Set up -------------------------------------------------------------  
# Template: 1-km resolution Australia raster
  rm(list = ls())
  aus <- raster("Australia/aus.grd")
  
# Template: shapefile
  aus.shp <- readOGR("Australia/australia_shapefile.shp")  
  
# Useful extent
  ext <- c(118, 152, -40, -11)
  
# Projection
  pro.crs <- "+proj=utm +zone=48 +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"      
# ST -------------------------------------------------------------------------------  
  st <- raster("EFs/Rasters 1-km non-cropped/st.grd")
  st.c <- crop(st, aus.shp)
  plot(st.c)
  
  st.pro <- projectRaster(st.c, aus, res = 0.01, crs = pro.crs, method="bilinear")
  plot(st.pro)
  
  st.m <- mask(st.pro, aus.shp)
  plot(st.m)
  
  writeRaster(st.m, "EFs/Rasters 1-km Aus-cropped/st.grd", prj = T, overwrite = T)  
  
# PAWC -----------------------------------------------------------------------------  
  pawc <- raster("EFs/Rasters 1-km non-cropped/pawc.grd")
  pawc.c <- crop(pawc, aus.shp)
  plot(pawc.c)
  
  pawc.pro <- projectRaster(pawc.c, aus, res = 0.01, crs = pro.crs, method="bilinear")
  plot(pawc.pro)
  
  pawc.m <- mask(pawc.pro, aus.shp)
  plot(pawc.m)
  
  writeRaster(pawc.m, "EFs/Rasters 1-km Aus-cropped/pawc.grd", prj = T, overwrite = T)  

# PEWC ----------------------------------------------------------------------------  
  pewc <- raster("EFs/Rasters 1-km non-cropped/pewc.grd")
  pewc.c <- crop(pewc, aus.shp)
  plot(pewc.c)
  
  pewc.pro <- projectRaster(pewc.c, aus, res = 0.01, crs = pro.crs, method="bilinear")
  plot(pewc.pro)
  
  pewc.m <- mask(pewc.pro, aus.shp)
  plot(pewc.m)
  
  writeRaster(pewc.m, "EFs/Rasters 1-km Aus-cropped/pewc.grd", prj = T, overwrite = T)  
  
# ---------------------------------------------------------------------------------