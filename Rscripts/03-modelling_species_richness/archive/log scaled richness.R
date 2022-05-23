


# library -------------------------------------------------------------
  library(raster)
  library(tidyverse)

  rm(list = ls())

# data ----------------------------------------------------------------
  setwd("C:/Users/Hemming/Dropbox/Poaceae/Results/rasters/iNEXT")
  
  current.list <- list.files(pattern = ".grd")
  names <- gsub(pattern = "\\.grd$", "", current.list)
  c.stack <- stack(current.list)
  names(c.stack) <- names
  list2env(setNames(unstack(c.stack), names(c.stack)), .GlobalEnv)
  
  setwd("C:/Users/Hemming/Dropbox/Poaceae")

# relative richness -----------------------------------------------    
# requires: raster -- loaded from above
#           raster.name -- identity for file location, so maybe put in if its observed or predicted, and in quotes
  prop.sr <- function(raster, raster.name) {
  
    log_t <- log(raster)
    prop <- log_t/cellStats(log_t, stat = 'max', na.rm = T)
  
    rasterfile <- paste0("Results/rasters/log scaled/", raster.name, ".grd")
  
  writeRaster(prop, rasterfile, overwrite = T)
  return(plot(prop))
  
  }
  
# native
  prop.sr(native_C3, "native_C3")
  prop.sr(native_C4, "native_C4")
  prop.sr(native_total, "native_total")
# non-native (exotic)
  prop.sr(nonnative_C3, "nonnative_C3")
  prop.sr(nonnative_C4, "nonnative_C4")
  prop.sr(nonnative_total, "nonnative_total")

 # -------------------------------------------------------------------
  
  
  