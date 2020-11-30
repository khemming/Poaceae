# date created: 7/1/19
# last updated: 15/3/19 (adjusting outline of Australia to include marginal cells)


########### Transforming projection systems of raw EVs ####################################


# see EF projection metadata for supporting information  

# Library --------------------------------------------------------------
  library(ggplot2)
  library(ggmap)
  library(tidyr)
  library(raster)
  library(dplyr)
  library(rgdal)
  library(maptools)
  
  setwd("C:/Users/s436862/Dropbox/Rarefaction")

# Extent and projection  -----------------------------------------------
  ext <- extent(c(100, 180, -50, 10))
  proj <- "+proj=utm +zone=48 +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
  
# Environnmental variables ---------------------------------------------  
# (1) aridity (note: /10000) -------------------------------------------
  arid <- raster("Data files/EVs/CGIR CSI Aridity and Evaporation/Global Aridity - Annual/AI_annual/ai_yr/hdr.adf")/10000
  projection(arid) <- proj
  names(arid) <- ("arid")
  
  arid.c <- crop(arid, extent(ext))
  writeRaster(arid.c, "Results/EVs/Rasters/Reprojected/arid.grd", prj = T, overwrite = T)
  b <- raster("Results/EVs/Rasters/Reprojected/arid.grd")
  plot(b)
  
# (2) potential evapotranspiration (pet)-------------------------------------------
  pet <- raster("Data files/EVs/CGIR CSI Aridity and Evaporation/Global PET - Annual/PET_he_annual/pet_he_yr/hdr.adf")
  projection(pet) <- proj
  names(pet) <- ("pet")
  
  pet.c <- crop(pet, extent(ext))
  writeRaster(pet.c, "Results/EVs/Reprojected/pet.grd", prj = T, overwrite = T)
  b <- raster("Results/EVs/Reprojected/pet.grd")
  plot(b)
  
# (3) elevation -------------------------------------------
  elev <- raster("Data files/EVs/CGIR Elevation/Elevation 30 sec/GloElev_30as.asc")
  projection(elev) <- proj
  names(elev) <- ("elev")
  
  elev.c <- crop(elev, extent(ext))
  writeRaster(elev.c, "Results/EVs/Reprojected/elev.grd", prj = T, overwrite = T)
  b <- raster("Results/EVs/Reprojected/elev.grd")
  plot(b)

# (4) Annual Mean Temperature (AMT) -------------------------------------------
  amt <- raster("Data files/EVs/Worldclim/wc2.0_bio_30s_01.tif") 
  projection(amt) <- proj
  names(amt) <- ("amt")
  
  amt.c <- crop(amt, extent(ext))
  writeRaster(amt.c, "Results/EVs/Reprojected/amt.grd", prj = T, overwrite = T)
  b <- raster("Results/EVs/Reprojected/amt.grd")
  plot(b)   

# (5) Mean Diurnal Range (Mean of monthly (max temp - min temp)) (MDR) -----------------------------
  mdr <- raster("Data files/EVs/Worldclim/wc2.0_bio_30s_02.tif") 
  projection(mdr) <- proj
  names(mdr) <- ("mdr")
  
  mdr.c <- crop(mdr, extent(ext))
  writeRaster(mdr.c, "Results/EVs/Reprojected/mdr.grd", prj = T, overwrite = T)
  b <- raster("Results/EVs/Reprojected/mdr.grd")
  plot(b) 

# Free up workspace memory ---------------------------------------------------------
  rm(list = ls())
  ext <- extent(c(100, 180, -50, 10))
  proj <- "+proj=utm +zone=48 +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
  
  
# (6) Isothermality (BIO2/BIO7) (* 100) (ISO) -------------------------------------------
  iso <- raster("Data files/EVs/Worldclim/wc2.0_bio_30s_03.tif") 
  projection(iso) <- proj
  names(iso) <- ("iso")
  
  iso.c <- crop(iso, extent(ext))
  writeRaster(iso.c, "Results/EVs/Reprojected/iso.grd", prj = T, overwrite = T)
  b <- raster("Results/EVs/Reprojected/iso.grd")
  plot(b)
  
# (7) Temperature Seasonality (standard deviation *100) (Note I divided by 100) (TS)  ----------------------
  ts <- raster("Data files/EVs/Worldclim/wc2.0_bio_30s_04.tif")/100
  projection(ts) <- proj
  names(ts) <- ("ts")
  
  ts.c <- crop(ts, extent(ext))
  writeRaster(ts.c, "Results/EVs/Reprojected/ts.grd", prj = T, overwrite = T)
  b <- raster("Results/EVs/Reprojected/ts.grd")
  plot(b)    
  
# (8) Max Temperature of Warmest Month (twarmm) ------------------------------------------
  twarmm <- raster("Data files/EVs/Worldclim/wc2.0_bio_30s_05.tif") 
  projection(twarmm) <- proj
  names(twarmm) <- ("twarmm")
  
  twarmm.c <- crop(twarmm, extent(ext))
  writeRaster(twarmm.c, "Results/EVs/Reprojected/twarmm.grd", prj = T, overwrite = T)
  b <- raster("Results/EVs/Reprojected/twarmm.grd")
  plot(b)     
  
# (9) Min Temperature of Coldest Month (tcoldm)   ----------------------------------------
  tcoldm <- raster("Data files/EVs/Worldclim/wc2.0_bio_30s_06.tif") 
  projection(tcoldm) <- proj
  names(tcoldm) <- ("tcoldm")
  
  tcoldm.c <- crop(tcoldm, extent(ext))
  writeRaster(tcoldm.c, "Results/EVs/Reprojected/tcoldm.grd", prj = T, overwrite = T)
  b <- raster("Results/EVs/Reprojected/tcoldm.grd")
  plot(b)
  
# (10) Temperature Annual Range (BIO5-BIO6) (TAR)  ------------------------------------
  tar <- raster("Data files/EVs/Worldclim/wc2.0_bio_30s_07.tif") 
  projection(tar) <- proj
  names(tar) <- ("tar")
  
  tar.c <- crop(tar, extent(ext))
  writeRaster(tar.c, "Results/EVs/Reprojected/tar.grd", prj = T, overwrite = T)
  b <- raster("Results/EVs/Reprojected/tar.grd")
  plot(b)    
  
# Free up workspace memory ---------------------------------------------------------
  rm(list = ls())
  ext <- extent(c(100, 180, -50, 10))
  proj <- "+proj=utm +zone=48 +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
  
# (11) Mean Temperature of Wettest Quarter (TWETQ)   ------------------------------------
  twetq <- raster("Data files/EVs/Worldclim/wc2.0_bio_30s_08.tif") 
  projection(twetq) <- proj
  names(twetq) <- ("twetq")
  
  twetq.c <- crop(twetq, extent(ext))
  writeRaster(twetq.c, "Results/EVs/Reprojected/twetq.grd", prj = T, overwrite = T)
  b <- raster("Results/EVs/Reprojected/twetq.grd")
  plot(b)     
  
# (12) Mean Temperature of Driest Quarter (TDRYQ)  ------------------------------------------- 
  tdryq <- raster("Data files/EVs/Worldclim/wc2.0_bio_30s_09.tif") 
  projection(tdryq) <- proj
  names(tdryq) <- ("tdryq")
  
  tdryq.c <- crop(tdryq, extent(ext))
  writeRaster(tdryq.c, "Results/EVs/Reprojected/tdryq.grd", prj = T, overwrite = T)
  b <- raster("Results/EVs/Reprojected/tdryq.grd")
  plot(b)   
  
# (13) Mean Temperature of Warmest Quarter (TWARMQ)  ------------------------------------------- 
  twarmq <- raster("Data files/EVs/Worldclim/wc2.0_bio_30s_10.tif")
  projection(twarmq) <- proj
  names(twarmq) <- ("twarmq")
  
  twarmq.c <- crop(twarmq, extent(ext))
  writeRaster(twarmq.c, "Results/EVs/Reprojected/twarmq.grd", prj = T, overwrite = T)
  b <- raster("Results/EVs/Reprojected/twarmq.grd")
  plot(b)   
  
# (14) Mean Temperature of Coldest Quarter (TCOLDQ)   ------------------------------------------- 
  tcoldq <- raster("Data files/EVs/Worldclim/wc2.0_bio_30s_11.tif")
  projection(tcoldq) <- proj
  names(tcoldq) <- ("tcoldq")
  
  tcoldq.c <- crop(tcoldq, extent(ext))
  writeRaster(tcoldq.c, "Results/EVs/Reprojected/tcoldq.grd", prj = T, overwrite = T)
  b <- raster("Results/EVs/Reprojected/tcoldq.grd")
  plot(b)
  
# (15) Annual Precipitation (AP)    -------------------------------------------
  ap <- raster("Data files/EVs/Worldclim/wc2.0_bio_30s_12.tif")
  projection(ap) <- proj
  names(ap) <- ("ap")
  
  ap.c <- crop(ap, extent(ext))
  writeRaster(ap.c, "Results/EVs/Reprojected/ap.grd", prj = T, overwrite = T)
  b <- raster("Results/EVs/Reprojected/ap.grd")
  plot(b)     
  
# Free up workspace memory ---------------------------------------------------------
  rm(list = ls())
  ext <- extent(c(100, 180, -50, 10))
  proj <- "+proj=utm +zone=48 +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"  
  
# (16) Precipitation of Wettest Month (PWETM)   -----------------------------------------
  pwetm <- raster("Data files/EVs/Worldclim/wc2.0_bio_30s_13.tif")
  projection(pwetm) <- proj
  names(pwetm) <- ("pwetm")
  
  pwetm.c <- crop(pwetm, extent(ext))
  writeRaster(pwetm.c, "Results/EVs/Reprojected/pwetm.grd", prj = T, overwrite = T)
  b <- raster("Results/EVs/Reprojected/pwetm.grd")
  plot(b)   
  
# (17) Precipitation of Driest Month (PDRYM)   -------------------------------------------
  pdrym <- raster("Data files/EVs/Worldclim/wc2.0_bio_30s_14.tif")
  projection(pdrym) <- proj
  names(pdrym) <- ("pdrym")
  
  pdrym.c <- crop(pdrym, extent(ext))
  writeRaster(pdrym.c, "Results/EVs/Reprojected/pdrym.grd", prj = T, overwrite = T)
  b <- raster("Results/EVs/Reprojected/pdrym.grd")
  plot(b)   
  
# (18) Precipitation Seasonality (Coefficient of Variation) (PS) ------------------------
  ps <- raster("Data files/EVs/Worldclim/wc2.0_bio_30s_15.tif")
  projection(ps) <- proj
  names(ps) <- ("ps")
  
  ps.c <- crop(ps, extent(ext))
  writeRaster(ps.c, "Results/EVs/Reprojected/ps.grd", prj = T, overwrite = T)
  b <- raster("Results/EVs/Reprojected/ps.grd")
  plot(b)        
  
# (19) Precipitation of Wettest Quarter (PWETQ) ------------------------------------------
  pwetq <- raster("Data files/EVs/Worldclim/wc2.0_bio_30s_16.tif")
  projection(pwetq) <- proj
  names(pwetq) <- ("pwetq")
  
  pwetq.c <- crop(pwetq, extent(ext))
  writeRaster(pwetq.c, "Results/EVs/Reprojected/pwetq.grd", prj = T, overwrite = T)
  b <- raster("Results/EVs/Reprojected/pwetq.grd")
  plot(b)      
  
# (20) Precipitation of Driest Quarter (PDRYQ) -------------------------------------------
  pdryq <- raster("Data files/EVs/Worldclim/wc2.0_bio_30s_17.tif")
  projection(pwetq) <- proj
  names(pwetq) <- ("pwetq")
  
  pdryq.c <- crop(pdryq, extent(ext))
  writeRaster(pwetq.c, "Results/EVs/Reprojected/pwetq.grd", prj = T, overwrite = T)
  b <- raster("Results/EVs/Reprojected/pwetq.grd")
  plot(b)    
  
# Free up workspace memory ---------------------------------------------------------
  rm(list = ls())
  ext <- extent(c(100, 180, -50, 10))
  proj <- "+proj=utm +zone=48 +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"  
  
  
# (21) Precipitation of Warmest Quarter (PWARMQ)   -------------------------------------------
  pwarmq <- raster("Data files/EVs/Worldclim/wc2.0_bio_30s_18.tif")
  projection(pwarmq) <- proj
  names(pwarmq) <- ("pwarmq")
  
  pwarmq.c <- crop(pwarmq, extent(ext))
  writeRaster(pwarmq.c, "Results/EVs/Reprojected/pwarmq.grd", prj = T, overwrite = T)
  b <- raster("Results/EVs/Reprojected/pwarmq.grd")
  plot(b)     

# (22) Precipitation of Coldest Quarter (PCOLDQ)   -------------------------------------------
  pcoldq <- raster("Data files/EVs/Worldclim/wc2.0_bio_30s_19.tif")
  projection(pcoldq) <- proj
  names(pcoldq) <- ("pcoldq")
  
  pcoldq.c <- crop(pcoldq, extent(ext))
  writeRaster(pcoldq.c, "Results/EVs/Reprojected/pcoldq.grd", prj = T, overwrite = T)
  b <- raster("Results/EVs/Reprojected/pcoldq.grd")
  plot(b) 
  
# (23) Human influence index --------------------------------------------------------
  hii <- raster("Data files/EVs/The Human Influence Index (HII)/hii-global-geo-grid/hii_v2geo/hdr.adf")
  projection(hii) <- proj
  names(hii) <- ("hii")
  
  hii.c <- crop(hii, extent(ext))
  writeRaster(hii.c, "Results/EVs/Reprojected/hii.grd", prj = T, overwrite = T)
  b <- raster("Results/EVs/Reprojected/hii.grd")
  plot(b)    
  
  
# (24) potential storage of water derived from soil texture (mm)) (st) -------------------
  st <- raster("Data files/EVs/Harmonised World Soil Database/WEBBSOIL_548/WEBBSOIL_548/data/wrtext.asc")
  projection(st) <- proj
  names(st) <- ("st")
  
  st.c <- crop(st, extent(ext))
  writeRaster(st.c, "Results/EVs/Reprojected/st.grd", prj = T, overwrite = T)
  b <- raster("Results/EVs/Reprojected/st.grd")
  plot(b) 
  
# (25) potential storage of water in the root zone (mm) (rz) -------------------------------------------
  rz <- raster("Data files/EVs/Harmonised World Soil Database/WEBBSOIL_548/WEBBSOIL_548/data/wrroot.asc")
  projection(rz) <- proj
  names(rz) <- ("rz")
  
  rz.c <- crop(rz, extent(ext))
  writeRaster(rz.c, "Results/EVs/Reprojected/rz.grd", prj = T, overwrite = T)
  b <- raster("Results/EVs/Reprojected/rz.grd")
  plot(b) 
  
# Free up workspace memory ---------------------------------------------------------
  rm(list = ls())
  ext <- extent(c(100, 180, -50, 10))
  proj <- "+proj=utm +zone=48 +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"  
  
# (26) potential storage of water in the soil profile (mm) (sp) -------------------------------------------
  sp <- raster("Data files/EVs/Harmonised World Soil Database/WEBBSOIL_548/WEBBSOIL_548/data/wrprof.asc")
  projection(sp) <- proj
  names(sp) <- ("sp")
  
  sp.c <- crop(sp, extent(ext))
  writeRaster(sp.c, "Results/EVs/Reprojected/sp.grd", prj = T, overwrite = T)
  b <- raster("Results/EVs/Reprojected/sp.grd")
  plot(b) 

# (27) plant available water capacity  -------------------------------------------
  pawc <- raster("Data files/EVs/Plant water capacity/PAWC_1m/pawc_1m/hdr.adf")
  projection(pawc) <- proj
  names(pawc) <- ("pawc")
  
  pawc.c <- crop(pawc, extent(ext))
  writeRaster(pawc.c, "Results/EVs/Reprojected/pawc.grd", prj = T, overwrite = T)
  b <- raster("Results/EVs/Reprojected/pawc.grd")
  plot(b) 
 
# (28) Plant-extractabel water capacity -------------------------------------------
  # again, dunno where I put og rasters (think I did them in ARC GIS?)
  # so I am doing it here
  pewc <- raster("Data files/EVs/Plant water capacity/DUNNESOIL_545/dunne_soil.dat")
  projection(pewc) <- proj
  names(pewc) <- ("pewc")
  
  pewc.c <- crop(pewc, extent(ext))
  writeRaster(pewc.c, "Results/EVs/Reprojected/pewc.grd", prj = T, overwrite = T)
  b <- raster("Results/EVs/Reprojected/pewc.grd")
  plot(b) 
  
# (29) Zobler soil layers  -------------------------------------------
# See what is in this layer
  zobs <- raster("Data files/EVs/Zobler half degree soil layers/ZOBLERSOILDERIVED_540/data/z_soiltype.dat")
  projection(zobs) <- proj
  names(zobs) <- ("zobs")
  
  zobs.c <- crop(zobs, extent(ext))
  writeRaster(zobs.c, "Results/EVs/Reprojected/zobs.grd", prj = T, overwrite = T)
  b <- raster("Results/EVs/Reprojected/zobs.grd")
  plot(b) 
    
# --------------------------------------------------------------------------------
  

