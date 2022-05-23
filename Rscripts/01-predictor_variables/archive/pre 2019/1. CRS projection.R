########### Transforming projection systems of raw EFs

# date created: 17/7/17
# last updated: 19/9/18

# see EF projection metadata for supporting information  

  .libPaths()
  rm(list = ls()) 
  
  library(ggplot2)
  library(ggmap)
  library(tidyr)
  library(raster)
  library(dplyr)
  library(rgdal)
  library(maptools)
  
  setwd("C:/Users/s436862/Dropbox/Climate Matching/1. Data files")

### (a) Australian raster
  aus <- raster("Australia/aus")
# already in proj = WSG_1984

### (b) Australian shapefile
  aus.s <- readOGR("Australia/australia_shapefile.shp")
  projection(aus.s) <- "+proj=utm +zone=48 +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

# remember, export file in WGS_1984 from ArcGIS into EFs raw if crazy lat/long
# I'll tell you how to re-do that in a second; think put it as 'dataframe' and export
# it as it is. Though not sure how to vary lat/long to specifically WSG_84 HOWEVER
# the below code does that anyways, so as long as in lat/long (e.g. 106.7, 10.1, etc.), 
# it should be fine()

# these files are so big, so I am cropping them to something nearer Australia
  ext <- extent(c(100, 160, -55, 0))
 
  
# For all EFS
  setwd("C:/Users/s436862/Dropbox/Climate Matching/1. Data files")
  
# (1) aridity (note: /10000) -------------------------------------------
  arid <- raster("CGIR CSI Aridity and Evaporation/Global Aridity - Annual/AI_annual/ai_yr/hdr.adf")/10000
  projection(arid) <- "+proj=utm +zone=48 +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
  names(arid) <- ("arid")
  
  arid.e <- crop(arid, extent(ext))
  writeRaster(arid.e, "EFs/Raster 1-km non-cropped/arid.grd", prj = T, overwrite = T)
  b <- raster("EFs/Raster 1-km non-cropped/arid.grd")
  plot(b)
  
# (2) potential evapotranspiration (pet)-------------------------------------------
  pet <- raster("CGIR CSI Aridity and Evaporation/Global PET - Annual/PET_he_annual/pet_he_yr/hdr.adf")
  projection(pet) <- "+proj=utm +zone=48 +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
  names(pet) <- ("pet")
  
  pet.e <- crop(pet, extent(ext))
  writeRaster(pet.e, "EFs/Raster 1-km non-cropped/pet.grd", prj = T, overwrite = T)
  c <- raster("EFs/Raster 1-km non-cropped/pet.grd")
  plot(c)
  
# (3) elevation -------------------------------------------
  elev <- raster("CGIR Elevation/Elevation 30 sec/GloElev_30as.asc")
  projection(elev) <- "+proj=utm +zone=48 +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
  names(elev) <- ("elevation")
  
  elev.e <- crop(elev, extent(ext))
  writeRaster(elev.e, "EFs/Raster 1-km non-cropped/elev.grd", prj = T, overwrite = T)
  e <- raster("EFs/Raster 1-km non-cropped/elev.grd")
  plot(e) # goooooood


# (4) Annual Mean Temperature (AMT) -------------------------------------------
  amt <- raster("Worldclim/wc2.0_bio_30s_01.tif") 
  projection(amt) <- "+proj=utm +zone=48 +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
  names(amt) <- ("amt")
  
  amt.e <- crop(amt, extent(ext))
  writeRaster(amt.e, "EFs/Raster 1-km non-cropped/amt.grd", prj = T, overwrite = T)
  g <- raster("EFs/Raster 1-km non-cropped/amt.grd")
  plot(g)     

# Mean Diurnal Range (Mean of monthly (max temp - min temp)) (MDR) -----------------------------
  mdr <- raster("Worldclim/wc2.0_bio_30s_02.tif") 
  projection(mdr) <- "+proj=utm +zone=48 +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
  names(mdr) <- ("mdr")
  
  mdr.e <- crop(mdr, extent(ext))
  writeRaster(mdr.e, "EFs/Raster 1-km non-cropped/mdr.grd", prj = T, overwrite = T)
  i <- raster("EFs/Raster 1-km non-cropped/mdr.grd")
  plot(i)     
  
# Isothermality (BIO2/BIO7) (* 100) (ISO) -------------------------------------------
  iso <- raster("Worldclim/wc2.0_bio_30s_03.tif") 
  projection(iso) <- "+proj=utm +zone=48 +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
  names(iso) <- ("iso")
  
  iso.e <- crop(iso, extent(ext))
  writeRaster(iso.e, "EFs/Raster 1-km non-cropped/iso.grd", prj = T, overwrite = T)
  j <- raster("EFs/Raster 1-km non-cropped/iso.grd")
  plot(j)     
  
# Temperature Seasonality (standard deviation *100) (Note I divided by 100) (TS)  --------------------------
  ts <- raster("Worldclim/wc2.0_bio_30s_04.tif")/100
  projection(ts) <- "+proj=utm +zone=48 +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
  names(ts) <- ("ts")
  
  ts.e <- crop(ts, extent(ext))
  writeRaster(ts.e, "EFs/Raster 1-km non-cropped/ts.grd", prj = T, overwrite = T)
  k <- raster("EFs/Raster 1-km non-cropped/ts.grd")
  plot(k)     
  
# Max Temperature of Warmest Month (twarmm) ------------------------------------------
  twarmm <- raster("Worldclim/wc2.0_bio_30s_05.tif") 
  projection(twarmm) <- "+proj=utm +zone=48 +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
  names(twarmm) <- ("twarmm")
  
  twarmm.e <- crop(twarmm, extent(ext))
  writeRaster(twarmm.e, "EFs/Raster 1-km non-cropped/twarmm.grd", prj = T, overwrite = T)
  l <- raster("EFs/Raster 1-km non-cropped/twarmm.grd")
  plot(l)     
  
# Min Temperature of Coldest Month (tcoldm)   ----------------------------------------
  rm(list = ls()) 
  ext <- extent(c(100, 160, -55, 0))
  
  tcoldm <- raster("Worldclim/wc2.0_bio_30s_06.tif") 
  projection(tcoldm) <- "+proj=utm +zone=48 +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
  names(tcoldm) <- ("tcoldm")
  
  tcoldm.e <- crop(tcoldm, extent(ext))
  writeRaster(tcoldm.e, "EFs/Raster 1-km non-cropped/tcoldm.grd", prj = T, overwrite = T)
  m <- raster("EFs/Raster 1-km non-cropped/tcoldm.grd")
  plot(m)     
  
# Temperature Annual Range (BIO5-BIO6) (TAR)  ------------------------------------
  tar <- raster("Worldclim/wc2.0_bio_30s_07.tif") 
  projection(tar) <- "+proj=utm +zone=48 +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
  names(tar) <- ("tar")
  
  tar.e <- crop(tar, extent(ext))
  writeRaster(tar.e, "EFs/Raster 1-km non-cropped/tar.grd", prj = T, overwrite = T)
  n <- raster("EFs/Raster 1-km non-cropped/tar.grd")
  plot(n)     
  
# Mean Temperature of Wettest Quarter (TWETQ)   ------------------------------------
  twetq <- raster("Worldclim/wc2.0_bio_30s_08.tif") 
  projection(twetq) <- "+proj=utm +zone=48 +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
  names(twetq) <- ("twetq")
  
  twetq.e <- crop(twetq, extent(ext))
  writeRaster(twetq.e, "EFs/Raster 1-km non-cropped/twetq.grd", prj = T, overwrite = T)
  o <- raster("EFs/Raster 1-km non-cropped/twetq.grd")
  plot(o)     
  
# Mean Temperature of Driest Quarter (TDRYQ)  ------------------------------------------- 
  tdryq <- raster("Worldclim/wc2.0_bio_30s_09.tif") 
  projection(tdryq) <- "+proj=utm +zone=48 +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
  names(tdryq) <- ("tdryq ")
  
  tdryq.e <- crop(tdryq, extent(ext))
  writeRaster(tdryq.e, "EFs/Raster 1-km non-cropped/tdryq.grd", prj = T, overwrite = T)
  p <- raster("EFs/Raster 1-km non-cropped/tdryq.grd")
  plot(p)     
  
# Mean Temperature of Warmest Quarter (TWARMQ)  ------------------------------------------- 
  twarmq <- raster("Worldclim/wc2.0_bio_30s_10.tif")
  projection(twarmq) <- "+proj=utm +zone=48 +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
  names(twarmq) <- ("twarmq")
  
  twarmq.e <- crop(twarmq, extent(ext))
  writeRaster(twarmq.e, "EFs/Raster 1-km non-cropped/twarmq.grd", prj = T, overwrite = T)
  q <- raster("EFs/Raster 1-km non-cropped/twarmq.grd")
  plot(q)     
  
# Mean Temperature of Coldest Quarter (TCOLDQ)   ------------------------------------------- 
  tcoldq <- raster("Worldclim/wc2.0_bio_30s_11.tif")
  projection(tcoldq) <- "+proj=utm +zone=48 +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
  names(tcoldq) <- ("tcoldq")
  
  tcoldq.e <- crop(tcoldq, extent(ext))
  writeRaster(tcoldq.e, "EFs/Raster 1-km non-cropped/tcoldq.grd", prj = T, overwrite = T)
  r <- raster("EFs/Raster 1-km non-cropped/tcoldq.grd")
  plot(r)     
  
# Annual Precipitation (AP)    -------------------------------------------
  rm(list = ls()) 
  ext <- extent(c(100, 160, -55, 0))
  
  ap <- raster("Worldclim/wc2.0_bio_30s_12.tif")
  projection(ap) <- "+proj=utm +zone=48 +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
  names(ap) <- ("ap")
  
  ap.e <- crop(ap, extent(ext))
  writeRaster(ap.e, "EFs/Raster 1-km non-cropped/ap.grd", prj = T, overwrite = T)
  s <- raster("EFs/Raster 1-km non-cropped/ap.grd")
  plot(s)     
  
# Precipitation of Wettest Month (PWETM)   -----------------------------------------
  pwetm <- raster("Worldclim/wc2.0_bio_30s_13.tif")
  projection(pwetm) <- "+proj=utm +zone=48 +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
  names(pwetm) <- ("pwetm")
  
  pwetm.e <- crop(pwetm, extent(ext))
  writeRaster(pwetm.e, "EFs/Raster 1-km non-cropped/pwetm.grd", prj = T, overwrite = T)
  t <- raster("EFs/Raster 1-km non-cropped/pwetm.grd")
  plot(t)     
  
# Precipitation of Driest Month (PDRYM)   -------------------------------------------
  pdrym <- raster("Worldclim/wc2.0_bio_30s_14.tif")
  projection(pdrym) <- "+proj=utm +zone=48 +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
  names(pdrym) <- ("pdrym")
  
  pdrym.e <- crop(pdrym, extent(ext))
  writeRaster(pdrym.e, "EFs/Raster 1-km non-cropped/pdrym.grd", prj = T, overwrite = T)
  u <- raster("EFs/Raster 1-km non-cropped/pdrym.grd")
  plot(u)     
  
# Precipitation Seasonality (Coefficient of Variation) (PS) ------------------------
  ps <- raster("Worldclim/wc2.0_bio_30s_15.tif")
  projection(ps) <- "+proj=utm +zone=48 +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
  names(ps) <- ("ps")
  
  ps.e <- crop(ps, extent(ext))
  writeRaster(ps.e, "EFs/Raster 1-km non-cropped/ps.grd", prj = T, overwrite = T)
  v <- raster("EFs/Raster 1-km non-cropped/ps.grd")
  plot(v)     
  
# Precipitation of Wettest Quarter (PWETQ) ------------------------------------------
  rm(list = ls()) 
  ext <- extent(c(100, 160, -55, 0))
  
  pwetq <- raster("Worldclim/wc2.0_bio_30s_16.tif")
  projection(pwetq) <- "+proj=utm +zone=48 +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
  names(pwetq) <- ("pwetq")
  
  pwetq.e <- crop(pwetq, extent(ext))
  writeRaster(pwetq.e, "EFs/Raster 1-km non-cropped/pwetq.grd", prj = T, overwrite = T)
  w <- raster("EFs/Raster 1-km non-cropped/pwetq.grd")
  plot(w)     
  
# Precipitation of Driest Quarter (PDRYQ) -------------------------------------------
  pdryq <- raster("Worldclim/wc2.0_bio_30s_17.tif")
  projection(pdryq) <- "+proj=utm +zone=48 +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
  names(pdryq) <- ("pdryq")
  
  pdryq.e <- crop(pdryq, extent(ext))
  writeRaster(pdryq.e, "EFs/Raster 1-km non-cropped/pdryq.grd", prj = T, overwrite = T)
  x <- raster("EFs/Raster 1-km non-cropped/pdryq.grd")
  plot(x)     
  
# Precipitation of Warmest Quarter (PWARMQ)   -------------------------------------------
  pwarmq <- raster("Worldclim/wc2.0_bio_30s_18.tif")
  projection(pwarmq) <- "+proj=utm +zone=48 +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
  names(pwarmq) <- ("pwarmq")
  
  pwarmq.e <- crop(pwarmq, extent(ext))
  writeRaster(pwarmq.e, "EFs/Raster 1-km non-cropped/pwarmq.grd", prj = T, overwrite = T)
  y <- raster("EFs/Raster 1-km non-cropped/pwarmq.grd")
  plot(y)     

# Precipitation of Coldest Quarter (PCOLDQ)   -------------------------------------------
  pcoldq <- raster("Worldclim/wc2.0_bio_30s_19.tif")
  projection(pcoldq) <- "+proj=utm +zone=48 +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
  names(pcoldq) <- ("pcoldq")
  
  pcoldq.e <- crop(pcoldq, extent(ext))
  writeRaster(pcoldq.e, "EFs/Raster 1-km non-cropped/pcoldq.grd", prj = T, overwrite = T)
  z <- raster("EFs/Raster 1-km non-cropped/pcoldq.grd")
  plot(z)  
  
# ---------------------------------------------------------------------- 


### (26) Human influence index -------------------------------------------
  hii <- raster("The Human Influence Index (HII)/hii-global-geo-grid/hii_v2geo/hdr.adf")
  projection(hii) <- "+proj=utm +zone=48 +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
  names(hii) <- ("hii")
  
  hii.e <- crop(hii, extent(ext))
  writeRaster(hii.e, "EFs/Raster 1-km non-cropped/hii.grd", prj = T, overwrite = T)
  n <- raster("EFs/Raster 1-km non-cropped/hii.grd")
  plot(n)     
  
  

### (28) Derived Water-Holding Capacities -------------------------------------------
# 28a potential storage of water derived from soil texture (mm)) (st)
  st <- raster("Harmonised World Soil Database/WEBBSOIL_548/WEBBSOIL_548/data/wrtext.asc")
  projection(st) <- "+proj=utm +zone=48 +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
  names(st) <- ("st")
  st.e <- crop(st, extent(ext))
  plot(st.e)
  
  writeRaster(st.e, "EFs/Raster 1-km non-cropped/st.grd", prj = T, overwrite = T)
  o <- raster("EFs/Raster 1-km non-cropped/st.grd")
  plot(o)  
# 28b potential storage of water in the root zone (mm) (rz) -------------------------------------------
  rz <- raster("Harmonised World Soil Database/WEBBSOIL_548/WEBBSOIL_548/data/wrroot.asc")
  projection(rz) <- "+proj=utm +zone=48 +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
  names(rz) <- ("rz")
  rz.e <- crop(rz, extent(ext))
  plot(rz.e)
  
  writeRaster(rz.e, "EFs/Raster 1-km non-cropped/rz.grd", prj = T, overwrite = T)
  p <- raster("EFs/Raster 1-km non-cropped/rz.grd")
  plot(p) 
# 28c potential storage of water in the soil profile (mm) (sp) -------------------------------------------
  sp <- raster("Harmonised World Soil Database/WEBBSOIL_548/WEBBSOIL_548/data/wrprof.asc")
  projection(sp) <- "+proj=utm +zone=48 +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
  names(sp) <- ("sp")
  sp.e <- crop(sp, extent(ext))
  plot(sp.e)
  
  writeRaster(sp.e, "EFs/Raster 1-km non-cropped/sp.grd", prj = T, overwrite = T)
  q <- raster("EFs/Raster 1-km non-cropped/sp.grd")
  plot(q) 
  

# (30) plant available water capacity  -------------------------------------------
  # dunno which scipt I stuck these in '1. EFs raw' file, so I am doing it here
  pawc <- raster("C:/Users/s436862/Dropbox/Climate Matching/1. Data files/Plant water capacity/PAWC_1m/pawc_1m_src/hdr.adf")
  plot(pawc)
  # source layers that make up pawc1
  
  pawc1 <- raster("C:/Users/s436862/Dropbox/Climate Matching/1. Data files/Plant water capacity/PAWC_1m/pawc_1m/hdr.adf")
  plot(pawc1)
  projection(pawc1) <- "+proj=utm +zone=48 +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
  names(pawc1) <- ("pawc")
  pawc1.e <- crop(pawc1, extent(ext))
  plot(pawc1.e)
  
  writeRaster(pawc1.e, "EFs/Raster 1-km non-cropped/pawc.grd", prj = T, overwrite = T)
  s <- raster("EFs/Raster 1-km non-cropped/pawc.grd")
  plot(s) 
 
# (32) Plant-extractabel water capacity -------------------------------------------
  # again, dunno where I put og rasters (think I did them in ARC GIS?)
  # so I am doing it here
  pewc <- raster("C:/Users/s436862/Dropbox/Climate Matching/1. Data files/Plant water capacity/DUNNESOIL_545/dunne_soil.dat")
  projection(pewc) <- "+proj=utm +zone=48 +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
  names(pewc) <- ("pewc")
  pewc.e <- crop(pewc, extent(ext))
  plot(pewc.e)
  
  writeRaster(pewc.e, "EFs/Raster 1-km non-cropped/pewc.grd", prj = T, overwrite = T)
  t <- raster("EFs/Raster 1-km non-cropped/pewc.grd")
  plot(t) 
  
# --------------------------------------------------------------------------------
  

