
########################################################################################
# step one: crop and reproject to Australia at 1 km scale
########################################################################################

# date created: 7/1/19
# last updated: 15/3/19 (adjusting Australian region and taking it from Climate Matching folder which I will delete, and putting it into here -- 2 years later)

# notes -------------------------------------------------
# transorming teh projection (CRS) to even-sized cells
# and from the original, global extents to something that encompasses Australia

# see EF projection metadata for supporting information  
# -------------------------------------------------------
# library --------------------------------------------------------------
  library(ggmap)
  library(raster)
  library(rgdal)
 
  setwd("C:/Users/s436862/Dropbox/Poaceae")

  rm(list = ls())
  
  mask.layer <- raster("Data files/Australia/aus 1 km.grd")
  
# 1-km raster functions -------------------------------------------------
# notes: function requirements -------------------------------------------------------------
# raw.raster  = sourced raster layer, somewhere from the deep dark depths of the internet, loaded as a '.grd' R-raster file
# mask.layer  = this layer outlines the general extent by which we want to plot our region, andm ore importantly, the cells of our region of interest (i.e. masks islands, other countries)
# raster.name = short name for raster that will be with it for life
# save        = save file path and raster name, relative to wd()
  
# Australia 1 km function  -----------------------------------------------------------------------
   aus_1km <- function(raw.raster, raster.name, mask.layer, save) 
    {
    # set things up
     projection(raw.raster) <- "+proj=utm +zone=48 +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
     crop.extent <- extent(mask.layer)
     names(raw.raster) <- raster.name
     
    # crop larger extent
     crop.raster <- crop(raw.raster, crop.extent)
    
    # mask offshore values
     masked.raster <- mask(crop.raster, mask.layer)
   
    # for some reason extents are slightly different
     extent(masked.raster) <- crop.extent
     
     plot(masked.raster)
     
    # save
     writeRaster(masked.raster, save, overwrite = T)
     
     return(masked.raster)
      
     } # fun end
# test 1km function -------------------------------------------------------------------------  
  # arid <- raster("Data files/EVs/CGIR CSI Aridity and Evaporation/Global Aridity - Annual/AI_annual/ai_yr/hdr.adf")/10000
  # name <- "arid"
  # mask.layer <- raster("Data files/Australia/aus 1 km.grd")
  # save <- "Results/EVs/Rasters/1 km/arid.grd"
  # 
  # aus_1km(arid, "arid", mask.layer, save)
  # plot(masked.raster)
  
# --------------------------------------------------------------------------------------
# Australia 1 km function  for HII -----------------------------------------------------------------------
  aus_1km_hii <- function(raw.raster, raster.name, mask.layer, save) 
  {
    # set things up
    projection(raw.raster) <- "+proj=utm +zone=48 +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
    crop.extent <- extent(mask.layer)
    names(raw.raster) <- raster.name
    
    # crop larger extent
    crop.raster <- crop(raw.raster, crop.extent)
    
    # for some reason extents are slightly different
    extent(crop.raster) <- crop.extent
    
    # mask offshore values
    masked.raster <- mask(crop.raster, mask.layer)
    
    # for some reason extents are slightly different
    extent(masked.raster) <- crop.extent
    
    plot(masked.raster)
    
    # save
    writeRaster(masked.raster, save, overwrite = T)
    
    return(masked.raster)
    
  } # fun end
  
# tested with hii --------------------  
# ----------------------------------------------------------------------------------------  
# Australia 1 km function for >1 km res rasters --------------------------------------------------
# for rasters which will be the opposite of aggregated (disaggregated?) back to 1 km for cropping purposes
  aus_1km_disag <- function(raw.raster, raster.name, mask.layer, save) 
  {
  # set things up
    projection(raw.raster) <- "+proj=utm +zone=48 +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
    crop.extent <- extent(mask.layer)
    names(raw.raster) <- raster.name
    
  # crop larger extent
    crop.raster <- crop(raw.raster, crop.extent)
    crop.raster
    plot(crop.raster)
    
  # reproject to 1 km res
    repro.raster <- projectRaster(crop.raster, mask.layer, res = 0.008333334, crs = "+proj=utm +zone=48 +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0", method="ngb")
    repro.raster
    plot(repro.raster)
    
  # mask offshore values
    masked.raster <- mask(repro.raster, mask.layer)
   
  # for some reason extents are slightly different
    extent(masked.raster) <- crop.extent
    
    plot(masked.raster)
    
  # save
    writeRaster(masked.raster, save, overwrite = T)
    
    return(masked.raster)
    
  } # fun end
  
# test 1km disag --------------------------------------------------------------------
#   st <- raster("Data files/EVs/Harmonised World Soil Database/WEBBSOIL_548/WEBBSOIL_548/data/wrtext.asc")
#   save <- "Results/EVs/Rasters/1 km/st.grd"  
# 
#   raw.raster <- st
#   
# # set things up
#   projection(raw.raster) <- "+proj=utm +zone=48 +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
#   crop.extent <- extent(mask.layer)
#   names(raw.raster) <- raster.name
#   
#   # crop larger extent
#   crop.raster <- crop(raw.raster, crop.extent)
#   crop.raster
#   plot(crop.raster)
#   
#   # reproject to 1 km res
#   repro.raster <- projectRaster(crop.raster, mask.layer, res = 0.008333334, crs = "+proj=utm +zone=48 +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0", method="ngb")
#   repro.raster
#   plot(repro.raster)
#   
#   
#   # mask offshore values
#   masked.raster <- mask(repro.raster, mask.layer)
#   masked.raster
#   plot(masked.raster)
#   
#   # for some reason extents are slightly different
#   extent(masked.raster) <- crop.extent
  
  
  
#########################################################################
# run functions
#########################################################################  
# requirements --------------------------------------------------------
  mask.layer <- raster("Data files/Australia/aus 1 km.grd")
  
  
# (1) aridity  --------------------------------------------------------
# note: dividied by 10,000 from original raster values
  arid <- raster("Data files/EVs/CGIR CSI Aridity and Evaporation/Global Aridity - Annual/AI_annual/ai_yr/hdr.adf")/10000
  save <- "Results/EVs/Rasters/1 km/arid.grd"
  
  aus_1km(arid, "arid", mask.layer, save)
  
# (2) potential evapo-transpiration (pet)-------------------------------------------
  pet <- raster("Data files/EVs/CGIR CSI Aridity and Evaporation/Global PET - Annual/PET_he_annual/pet_he_yr/hdr.adf")
  save <- "Results/EVs/Rasters/1 km/pet.grd"
  
  aus_1km(pet, "pet", mask.layer, save)
 
# (3) elevation -------------------------------------------
  elev <- raster("Data files/EVs/CGIR Elevation/Elevation 30 sec/GloElev_30as.asc")
  save <- "Results/EVs/Rasters/1 km/elev.grd"
  
  aus_1km(elev, "elev", mask.layer, save)
  
# (4) Annual Mean Temperature (AMT) -------------------------------------------
  amt <- raster("Data files/EVs/Worldclim/wc2.0_bio_30s_01.tif") 
  save <- "Results/EVs/Rasters/1 km/amt.grd"
  
  aus_1km(amt,"amt", mask.layer, save)

# (5) Mean Diurnal Range (Mean of monthly (max temp - min temp)) (MDR) -----------------------------
  mdr <- raster("Data files/EVs/Worldclim/wc2.0_bio_30s_02.tif") 
  save <- "Results/EVs/Rasters/1 km/mdr.grd"
  
  aus_1km(mdr, "mdr", mask.layer, save)

# (6) Isothermality (BIO2/BIO7) (* 1) (ISO) -------------------------------------------
  iso <- raster("Data files/EVs/Worldclim/wc2.0_bio_30s_03.tif") 
  save <- "Results/EVs/Rasters/1 km/iso.grd"
  
  aus_1km(iso, "iso", mask.layer, save)
  
# (7) Temperature Seasonality (standard deviation *100) (Note I divided by 100) (TS)  ----------------------
  ts <- raster("Data files/EVs/Worldclim/wc2.0_bio_30s_04.tif")/100
  save <- "Results/EVs/Rasters/1 km/ts.grd"
  
  aus_1km(ts, "ts", mask.layer, save)
  
# (8) Max Temperature of Warmest Month (twarmm) ------------------------------------------
  twarmm <- raster("Data files/EVs/Worldclim/wc2.0_bio_30s_05.tif") 
  save <- "Results/EVs/Rasters/1 km/twarmm.grd"
  
  aus_1km(twarmm, "twarmm", mask.layer, save)
  
# (9) Min Temperature of Coldest Month (tcoldm)   ----------------------------------------
  tcoldm <- raster("Data files/EVs/Worldclim/wc2.0_bio_30s_06.tif") 
  save <- "Results/EVs/Rasters/1 km/tcoldm.grd"
  
  aus_1km(tcoldm, "tcoldm", mask.layer, save)
  
# (10) Temperature Annual Range (BIO5-BIO6) (TAR)  ------------------------------------
  tar <- raster("Data files/EVs/Worldclim/wc2.0_bio_30s_07.tif") 
  save <- "Results/EVs/Rasters/1 km/tar.grd"
  
  aus_1km(tar, "tar", mask.layer, save)
  
# (11) Mean Temperature of Wettest Quarter (TWETQ)   ------------------------------------
  twetq <- raster("Data files/EVs/Worldclim/wc2.0_bio_30s_08.tif") 
  save <- "Results/EVs/Rasters/1 km/twetq.grd"
  
  aus_1km(twetq, "twetq", mask.layer, save)
  
# (12) Mean Temperature of Driest Quarter (TDRYQ)  ------------------------------------------- 
  tdryq <- raster("Data files/EVs/Worldclim/wc2.0_bio_30s_09.tif") 
  save <- "Results/EVs/Rasters/1 km/tdryq.grd"
  
  aus_1km(tdryq, "tdryq", mask.layer, save) 
  
# (13) Mean Temperature of Warmest Quarter (TWARMQ)  ------------------------------------------- 
  twarmq <- raster("Data files/EVs/Worldclim/wc2.0_bio_30s_10.tif")
  save <- "Results/EVs/Rasters/1 km/twarmq.grd"
  
  aus_1km(twarmq, "twarmq", mask.layer, save)
  
# (14) Mean Temperature of Coldest Quarter (TCOLDQ)   ------------------------------------------- 
  tcoldq <- raster("Data files/EVs/Worldclim/wc2.0_bio_30s_11.tif")
  save <- "Results/EVs/Rasters/1 km/tcoldq.grd"
  
  aus_1km(tcoldq, "tcoldq", mask.layer, save)
  
# (15) Annual Precipitation (AP)    -------------------------------------------
  ap <- raster("Data files/EVs/Worldclim/wc2.0_bio_30s_12.tif")
  save <- "Results/EVs/Rasters/1 km/ap.grd"
  
  aus_1km(ap, "ap", mask.layer, save)  
  
# (16) Precipitation of Wettest Month (PWETM)   -----------------------------------------
  pwetm <- raster("Data files/EVs/Worldclim/wc2.0_bio_30s_13.tif")
  save <- "Results/EVs/Rasters/1 km/pwetm.grd"
  
  aus_1km(pwetm, "pwetm", mask.layer, save)  
  
# (17) Precipitation of Driest Month (PDRYM)   -------------------------------------------
  pdrym <- raster("Data files/EVs/Worldclim/wc2.0_bio_30s_14.tif")
  save <- "Results/EVs/Rasters/1 km/pdrym.grd"
  
  aus_1km(pdrym, "pdrym", mask.layer, save)
  
# (18) Precipitation Seasonality (Coefficient of Variation) (PS) ------------------------
  ps <- raster("Data files/EVs/Worldclim/wc2.0_bio_30s_15.tif")
  save <- "Results/EVs/Rasters/1 km/ps.grd"
  
  aus_1km(ps, "ps", mask.layer, save)     
  
# (19) Precipitation of Wettest Quarter (PWETQ) ------------------------------------------
  pwetq <- raster("Data files/EVs/Worldclim/wc2.0_bio_30s_16.tif")
  save <- "Results/EVs/Rasters/1 km/pwetq.grd"
  
  aus_1km(pwetq, "pwetq", mask.layer, save)
  
# (20) Precipitation of Driest Quarter (PDRYQ) -------------------------------------------
  pdryq <- raster("Data files/EVs/Worldclim/wc2.0_bio_30s_17.tif")
  save <- "Results/EVs/Rasters/1 km/pdrym.grd"
  
  aus_1km(pdrym, "pdrym", mask.layer, save)
  
# (21) Precipitation of Warmest Quarter (PWARMQ)   -------------------------------------------
  pwarmq <- raster("Data files/EVs/Worldclim/wc2.0_bio_30s_18.tif")
  save <- "Results/EVs/Rasters/1 km/pwarmq.grd"
  
  aus_1km(pwarmq, "pwarmq", mask.layer, save)

# (22) Precipitation of Coldest Quarter (PCOLDQ)   -------------------------------------------
  pcoldq <- raster("Data files/EVs/Worldclim/wc2.0_bio_30s_19.tif")
  save <- "Results/EVs/Rasters/1 km/pcoldq.grd"
  
  aus_1km(pcoldq, "pcoldq", mask.layer, save)
  
# (23) Human influence index --------------------------------------------------------
  hii <- raster("Data files/EVs/The Human Influence Index (HII)/hii-global-geo-grid/hii_v2geo/hdr.adf")
  save <- "Results/EVs/Rasters/1 km/hii.grd"
  
  aus_1km_hii(hii, "hii", mask.layer, save)

# (24) potential storage of water derived from soil texture (mm)) (st) -------------------
  st <- raster("Data files/EVs/Harmonised World Soil Database/WEBBSOIL_548/WEBBSOIL_548/data/wrtext.asc")
  save <- "Results/EVs/Rasters/1 km/st.grd"
  
  aus_1km_disag(st, "st", mask.layer, save)
 
  
# (25) potential storage of water in the root zone (mm) (rz) -------------------------------------------
  rz <- raster("Data files/EVs/Harmonised World Soil Database/WEBBSOIL_548/WEBBSOIL_548/data/wrroot.asc")
  save <- "Results/EVs/Rasters/1 km/rz.grd"
  
  aus_1km_disag(rz, "rz", mask.layer, save)
  
# (26) potential storage of water in the soil profile (mm) (sp) -------------------------------------------
  sp <- raster("Data files/EVs/Harmonised World Soil Database/WEBBSOIL_548/WEBBSOIL_548/data/wrprof.asc")
  save <- "Results/EVs/Rasters/1 km/sp.grd"
  
  aus_1km_disag(sp, "sp", mask.layer, save)

# (27) plant available [soil] water capacity  -------------------------------------------
  pawc <- raster("Data files/EVs/Plant water capacity/PAWC_1m/pawc_1m/hdr.adf")
  save <- "Results/EVs/Rasters/1 km/pawc.grd"
  
  aus_1km_disag(pawc, "pawc", mask.layer, save)
 
# (28) Plant extractable [soil] water capacity -------------------------------------------
  # again, dunno where I put og rasters (think I did them in ARC GIS?)
  # so I am doing it here
  pewc <- raster("Data files/EVs/Plant water capacity/DUNNESOIL_545/dunne_soil.dat")
  save <- "Results/EVs/Rasters/1 km/pewc.grd"
  
  aus_1km_disag(pewc, "pewc", mask.layer, save)
  
# (29) Clay percentage to 30 cm depth (clay) -------------------------------------------
  clay <- raster("Data files/Sue/clay30/clay30/hdr.adf")
  save <- "Results/EVs/Rasters/1 km/clay.grd"
  
  aus_1km_disag(clay, "clay", mask.layer, save)
  
  
# --------------------------------------------------------------------------------
  

