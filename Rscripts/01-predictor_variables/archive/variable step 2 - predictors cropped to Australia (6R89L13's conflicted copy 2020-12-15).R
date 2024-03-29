
# library ---------------------------------------------------------------
  library(raster)
  library(rgdal)
  setwd("C:/Users/s436862/Dropbox/Poaceae")
  rm(list = ls())
  
# data ------------------------------------------------------------------
# australia template
  temp <- raster("Data files/Australia/Australia 1 km.grd")
  
# wrldclim variables (19 originals)
  setwd("./Data files/predictor variables/raw files/wrldclim")
  my_names <- c("amt",    "mdr",   "iso",    "ts",    "twarmm", 
                "tcoldm", "tar",   "twetq",  "tdryq", "twarmq", 
                "tcoldq", "ap",    "pwetm",  "pdrym", "ps",     
                "pwetq",  "pdryq", "pwarmq", "pcoldq")
  wrldclim_files <- list.files(pattern = "bio_")
  wc_stack <- stack(wrldclim_files)
  list2env(setNames(unstack(wc_stack), my_names), .GlobalEnv)
  
# predictor variables (single files)
  setwd("C:/Users/s436862/Dropbox/Poaceae/Data files/predictor variables/raw files")
  arid <- raster("aridity/hdr.adf")
  elev <- raster("elevation/GloElev_30as.asc.ovr")
  pet <- raster("potential evapo-transpiration/hdr.adf")
  hii <- raster("human influence index 2018/wildareas-v3-2009-human-footprint.tif")
  st <- raster("potential storage of water/wrtext.asc")
  rz <- raster("potential storage of water/wrroot.asc")
  sp <- raster("potential storage of water/wrprof.asc")
  pawc <- raster("potential water capacity/hdr.adf")
  pewc <- raster("potential water capacity/dunne_soil.dat")
  clay <- raster("clay/clay30/clay30/hdr.adf")

  setwd("C:/Users/s436862/Dropbox/Poaceae")
  
# crop to Australia -----------------------------------------------------
# files with 1 km resolution --------------------------------------------
  crop_1km_fun <- function(raw_raster, raster_name) {
    # set things up
      mask <- temp
      projection(raw_raster) <- "+proj=utm +zone=48 +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
      crop_extent <- extent(mask)
      names(raw_raster) <- raster_name
      
    # crop larger extent
      crop_raster <- crop(raw_raster, crop_extent)
      
    # mask offshore values
      masked_raster <- mask(crop_raster, mask)
      
    # for some reason extents are slightly different
      extent(masked_raster) <- crop_extent
      
    # save
      save <- paste0("Data files/predictor variables/cropped/", raster_name)
      writeRaster(masked_raster, save, overwrite = T)
      
  }
  
# run function  
  crop_1km_fun(amt,  "amt")
  crop_1km_fun(ap,   "ap")
  crop_1km_fun(arid, "arid")
  crop_1km_fun(iso,  "iso")
  
  crop_1km_fun(mdr,    "mdr")
  crop_1km_fun(pcoldq, "pcoldq")
  crop_1km_fun(pdrym,  "pdrym")
  crop_1km_fun(pdryq,  "pdryq")
  crop_1km_fun(ps,     "ps")
  
  crop_1km_fun(pwarmq, "pwarmq")
  crop_1km_fun(pwetm,  "pwetm")
  crop_1km_fun(pwetq,  "pwetq")
  crop_1km_fun(tar,    "tar")
  
  crop_1km_fun(tcoldm, "tcoldm")
  crop_1km_fun(tcoldq, "tcoldq")
  crop_1km_fun(tdryq,  "tdryq")
  crop_1km_fun(ts,     "ts")
  crop_1km_fun(twarmm, "twarmm")
  crop_1km_fun(twarmq, "twarmq")
  
  crop_1km_fun(twetq, "twetq")
  
# crop to 1 km for >1km resolution rasters  --------------------------------------------
  crop_hi_res_fun <- function(raw_raster, raster_name) {
  
  # set things up
    mask <- temp
    projection(raw_raster) <- "+proj=utm +zone=48 +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
    crop_extent <- extent(mask)
    names(raw_raster) <- raster_name
  
  # crop larger extent
    crop_raster <- crop(raw_raster, crop_extent)
    crop_raster
    
  # reproject to 1 km res
    repro_raster <- projectRaster(crop_raster, mask, res = 0.008333334, crs = "+proj=utm +zone=48 +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0", method="ngb")
    
  # mask offshore values
    masked_raster <- mask(repro_raster, mask)
  
  # for some reason extents are slightly different
    extent(masked_raster) <- crop_extent
  
  # save
    save <- paste0("Data files/predictor variables/cropped/", raster_name, ".grd")
    writeRaster(masked_raster, save, overwrite = T)
  
  }
  
# run function   
  crop_hi_res_fun(pawc, "pawc")
  crop_hi_res_fun(pewc, "pewc")
  crop_hi_res_fun(rz,   "rz")
  crop_hi_res_fun(sp,   "sp")
  crop_hi_res_fun(st,   "st")

  crop_hi_res_fun(clay, "clay")
  
# crop to 1 km for different extents ---------------------------------------------------------------------------
  crop_ext_fun <- function(raw_raster, raster_name) {
  
  # reproject raster  
    mask <- temp
    repro_raster <- projectRaster(raw_raster,
                                  crs = crs(mask),
                                  res = 0.008333334)
    crop_extent <- extent(mask)
  # crop  
    crop_raster <- crop(raw_raster, crop_extent)
    
  # for some reason extents are slightly different
    extent(crop_raster) <- crop_extent   
    
  # mask offshore values
    masked_raster <- mask(crop_raster, mask)
    
  # for some reason extents are still slightly different
    extent(masked_raster) <- crop_extent
    names(masked_raster) <- raster_name
    
  # save
    save <- paste0("Data files/predictor variables/cropped/", raster_name)
    writeRaster(masked_raster, save, overwrite = T)
    
  }  
  
# run function
  crop_hii_fun(hii, "hii")
  
# -------------------------------------------------------------
  

# function
  test_fun <- function() {
  
  
  writeRaster(test3, "Data files/predictor variables/cropped/hii", overwrite = T)}  
  test_fun()
  