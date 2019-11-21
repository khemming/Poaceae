#######################################################################
# proportional species richness
#######################################################################

 # scope --------------------------------------------------------------
# re-do iNEXT species richness to relative proportions

# library -------------------------------------------------------------
  library(raster)
  library(tidyverse)

  rm(list = ls())

# data ----------------------------------------------------------------
  setwd("C:/Users/s436862/Dropbox/Poaceae/Results/iNEXT/Rasters/observed 15 rec 0.8 cov warn removed")
  
  current.list <- list.files(pattern = ".grd")
  names <- gsub(pattern = "\\.grd$", "", current.list)
  c.stack <- stack(current.list)
  names(c.stack) <- names
  list2env(setNames(unstack(c.stack), names(c.stack)), .GlobalEnv)
  
  setwd("C:/Users/s436862/Dropbox/Poaceae")

# proportional richness -----------------------------------------------    
# function
# requires: raster -- loaded from above
#           raster.name -- identity for file location, so maybe put in if its observed or predicted, and in quotes
  prop.sr <- function(raster, raster.name) {
  
    prop <- raster/cellStats(raster, stat = 'max', na.rm = T)
  rasterfile <- paste0("Results/iNEXT/Rasters/proportional species richness/", raster.name, ".grd")
  writeRaster(prop, rasterfile, overwrite = T)
  return(plot(prop))
  
  }
  
# native
 prop.sr(n.c3, "obs.nat.c3")
 prop.sr(n.c4, "obs.nat.c4")
 prop.sr(n.tot, "obs.nat.tot")
# non-native (exotic)
 prop.sr(e.c3, "obs.nonnat.c3")
 prop.sr(e.c4, "obs.nonnat.c4")
 prop.sr(e.tot, "obs.nonnat.tot")

 # -------------------------------------------------------------------
  
  
  