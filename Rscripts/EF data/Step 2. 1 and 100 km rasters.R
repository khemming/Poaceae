
########################################################################################
# step two: reproject Australia 1 km raster to 100 km resolution
########################################################################################

# date created: 16/4/18
# last modified: 15/3/19 (update for new Step one version)

# Library ---------------------------------------------------------
  library(raster)
  library(dplyr)
  library(rgdal)
  library(purrr)
  
  rm(list = ls())
  
  setwd("C:/Users/s436862/Dropbox/Rarefaction/Results")
  
# scale up resolution of cells from 1 km to 100 km -------------------------------
# aridity ----------------------------------------------  
  arid <- raster("EVs/Rasters/1 km/arid.grd")
  arid.ag <- aggregate(arid, fact = 100, fun = mean)
  writeRaster(arid.ag, "EVs/Rasters/100 km/arid.grd", overwrite = T)
  plot(arid.ag)
# annual mean temperature ----------------------------------------------  
  amt <- raster("EVs/Rasters/1 km/amt.grd")
  amt.ag <- aggregate(amt, fact = 100, fun = mean)
  writeRaster(amt.ag, "EVs/Rasters/100 km/amt.grd", overwrite = T)
  plot(amt.ag)
  
# annual precipitation ----------------------------------------------
  ap <- raster("EVs/Rasters/1 km/ap.grd")
  ap.ag <- aggregate(ap, fact = 100, fun = mean)
  writeRaster(ap.ag, "EVs/Rasters/100 km/ap.grd", overwrite = T)
  plot(ap.ag)
  
# elevation -----------------------------------------------------  
  elev <- raster("EVs/Rasters/1 km/elev.grd")
  elev.ag <- aggregate(elev, fact = 100, fun = mean)
  writeRaster(elev.ag, "EVs/Rasters/100 km/elev.grd", overwrite = T)
  plot(elev.ag)
  
# topographical heterogeneity -----------------------------------------  
  th.ag <- aggregate(elev, fact = 100, fun = sd) # note: sd for topographic heterogeneity
  plot(th.ag)
  writeRaster(th.ag, "EVs/Rasters/100 km/th.grd", overwrite = T)
  
# human influence index ----------------------------------------------  
  hii <- raster("EVs/Rasters/1 km/hii.grd")
  hii.ag <- aggregate(hii, fact = 100, fun = median) # note median
  writeRaster(hii.ag, "EVs/Rasters/100 km/hii.grd", overwrite = T)
  plot(hii.ag)
  
# isothermality ----------------------------------------------------  
  iso <- raster("EVs/Rasters/1 km/iso.grd")
  iso.ag <- aggregate(iso, fact = 100, fun = mean)
  writeRaster(iso.ag, "EVs/Rasters/100 km/iso.grd", overwrite = T)
  plot(iso.ag)
  
# mean diurbnal range ----------------------------------------------
  mdr <- raster("EVs/Rasters/1 km/mdr.grd")
  mdr.ag <- aggregate(mdr, fact = 100, fun = mean)
  writeRaster(mdr.ag, "EVs/Rasters/100 km/mdr.grd", overwrite = T)
  plot(mdr.ag)
  
# plant available [soil] water capacity ------------------------
  pawc <- raster("EVs/Rasters/1 km/pawc.grd")
  pawc.ag <- aggregate(pawc, fact = 100, fun = mean)
  writeRaster(pawc.ag, "EVs/Rasters/100 km/pawc.grd", overwrite = T)
  plot(pawc.ag)
  
# precipitation of coldest quarter ------------------------  
  pcoldq <- raster("EVs/Rasters/1 km/pcoldq.grd")
  pcoldq.ag <- aggregate(pcoldq, fact = 100, fun = mean)
  writeRaster(pcoldq.ag, "EVs/Rasters/100 km/pcoldq.grd", overwrite = T)
  plot(pcoldq.ag)
  
# precipitation of driest month ---------------------------    
  pdrym <- raster("EVs/Rasters/1 km/pdrym.grd")
  pdrym.ag <- aggregate(pdrym, fact = 100, fun = mean)
  writeRaster(pdrym.ag, "EVs/Rasters/100 km/pdrym.grd", overwrite = T)
  plot(pdrym.ag)
  
# potential evapo-transiration ------------------------  
  pet <- raster("EVs/Rasters/1 km/pet.grd")
  pet.ag <- aggregate(pet, fact = 100, fun = mean)
  writeRaster(pet.ag, "EVs/Rasters/100 km/pet.grd", overwrite = T)
  plot(pet.ag)
  
# plant extractable [soil] water capacity ------------------------    
  pewc <- raster("EVs/Rasters/1 km/pewc.grd")
  pewc.ag <- aggregate(pewc, fact = 100, fun = mean)
  writeRaster(pewc.ag, "EVs/Rasters/100 km/pewc.grd", overwrite = T)
  plot(pewc.ag)
  
# precipitation seasonality -------------------------------------  
  ps <- raster("EVs/Rasters/1 km/ps.grd")
  ps.ag <- aggregate(ps, fact = 100, fun = mean)
  writeRaster(ps.ag, "EVs/Rasters/100 km/ps.grd", overwrite = T)
  plot(ps.ag)
  
# precipitation of the warmest quarter -------------------------
  pwarmq <- raster("EVs/Rasters/1 km/pwarmq.grd")
  pwarmq.ag <- aggregate(pwarmq, fact = 100, fun = mean)
  writeRaster(pwarmq.ag, "EVs/Rasters/100 km/pwarmq.grd", overwrite = T)
  plot(pwarmq.ag)
  
# precipitation of the wettest month -----------------------------  
  pwetm <- raster("EVs/Rasters/1 km/pwetm.grd")
  pwetm.ag <- aggregate(pwetm, fact = 100, fun = mean)
  writeRaster(pwetm.ag, "EVs/Rasters/100 km/pwetm.grd", overwrite = T)
  plot(pwetm.ag)
  
# precipitation of the wettest quarter -----------------------------  
  pwetq <- raster("EVs/Rasters/1 km/pwetq.grd")
  pwetq.ag <- aggregate(pwetq, fact = 100, fun = mean)
  writeRaster(pwetq.ag, "EVs/Rasters/100 km/pwetq.grd", overwrite = T)
  plot(pwetq.ag)
  
# [soil] water holding capcity of the root zone --------------------  
  rz <- raster("EVs/Rasters/1 km/rz.grd")
  rz.ag <- aggregate(rz, fact = 100, fun = mean)
  writeRaster(rz.ag, "EVs/Rasters/100 km/rz.grd", overwrite = T)
  plot(rz.ag)

# [soil] water holding capcity of the soil particles --------------------    
  sp <- raster("EVs/Rasters/1 km/sp.grd")
  sp.ag <- aggregate(sp, fact = 100, fun = mean)
  writeRaster(sp.ag, "EVs/Rasters/100 km/sp.grd", overwrite = T)
  plot(sp.ag)
  
# [soil] water holding capcity of the soil texture --------------------  
  st <- raster("EVs/Rasters/1 km/st.grd")
  st.ag <- aggregate(st, fact = 100, fun = mean)
  writeRaster(st.ag, "EVs/Rasters/100 km/st.grd", overwrite = T)
  plot(st.ag)
  
# temperature annual range---------------------- --------------------  
  tar <- raster("EVs/Rasters/1 km/tar.grd")
  tar.ag <- aggregate(tar, fact = 100, fun = mean)
  writeRaster(tar.ag, "EVs/Rasters/100 km/tar.grd", overwrite = T)
  plot(tar.ag)
  
# minimum tempature of the coldest month ---------------------------  
  tcoldm <- raster("EVs/Rasters/1 km/tcoldm.grd")
  tcoldm.ag <- aggregate(tcoldm, fact = 100, fun = mean)
  writeRaster(tcoldm.ag, "EVs/Rasters/100 km/tcoldm.grd", overwrite = T)
  plot(tcoldm.ag)
  
# mean temperature of the coldest quarter --------------------------  
  tcoldq <- raster("EVs/Rasters/1 km/tcoldq.grd")
  tcoldq.ag <- aggregate(tcoldq, fact = 100, fun = mean)
  writeRaster(tcoldq.ag, "EVs/Rasters/100 km/tcoldq.grd", overwrite = T)
  plot(tcoldq.ag)
  
# mean temperature of the driest quarter ------------------------
  tdryq <- raster("EVs/Rasters/1 km/tdryq.grd")
  tdryq.ag <- aggregate(tdryq, fact = 100, fun = mean)
  writeRaster(tdryq.ag, "EVs/Rasters/100 km/tdryq.grd", overwrite = T)
  plot(tdryq.ag)
  
# temperature seasonality ----------------------------------------  
  ts <- raster("EVs/Rasters/1 km/ts.grd")
  ts.ag <- aggregate(ts, fact = 100, fun = mean)
  writeRaster(ts.ag, "EVs/Rasters/100 km/ts.grd", overwrite = T)
  plot(ts.ag)
  
# maximum temperature of the warmest month ------------------------ 
  twarmm <- raster("EVs/Rasters/1 km/twarmm.grd")
  twarmm.ag <- aggregate(twarmm, fact = 100, fun = mean)
  writeRaster(twarmm.ag, "EVs/Rasters/100 km/twarmm.grd", overwrite = T)
  plot(twarmm.ag)
  
# mean temperature of the warmest quarter -----------------------
  twarmq <- raster("EVs/Rasters/1 km/twarmq.grd")
  twarmq.ag <- aggregate(twarmq, fact = 100, fun = mean)
  writeRaster(twarmq.ag, "EVs/Rasters/100 km/twarmq.grd", overwrite = T)
  plot(twarmq.ag)
  
# mean temperature of the wettest quarter -----------------------  
  twetq <- raster("EVs/Rasters/1 km/twetq.grd")
  twetq.ag <- aggregate(twetq, fact = 100, fun = mean)
  writeRaster(twetq.ag, "EVs/Rasters/100 km/twetq.grd", overwrite = T)
  plot(twetq.ag)
  
# clay percentage to 30 cm depth -----------------------  
  clay <- raster("EVs/Rasters/1 km/clay.grd")
  clay.ag <- aggregate(clay, fact = 100, fun = mean)
  writeRaster(clay.ag, "EVs/Rasters/100 km/clay.grd", overwrite = T)
  plot(clay.ag)
  
# -----------------------------------------------------------------  