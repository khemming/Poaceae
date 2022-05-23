


# library -------------------------------------------------------------
  library(raster)
  library(tidyverse)

  rm(list = ls())

# data ----------------------------------------------------------------
# observed richness
  current.list <- list.files(path = "Results/rasters/scaled",
                             pattern = ".grd", full.names = T)
  names <- gsub(pattern = "Results/rasters/scaled/|.grd$", "", current.list)
  c.stack <- stack(current.list)
  names(c.stack) <- names
  list2env(setNames(unstack(c.stack), names(c.stack)), .GlobalEnv)
  
# predicted richness
  current.list <- list.files(path = "Results/rasters/predicted",
                             pattern = ".grd", full.names = T)
  names <- gsub(pattern = "Results/rasters/predicted/|.grd$", "", current.list)
  c.stack <- stack(current.list)
  names(c.stack) <- names
  list2env(setNames(unstack(c.stack), names(c.stack)), .GlobalEnv)
  
# invasion potential ---------------------------------------------------    
# requires: 
# predicted raster = native predicted
# observed raster  = non-native observed
# save_name        = status, pathway, richness category
  rr_pot <- function(pred_raster, obs_raster, save_name) {
    obs_raster[is.na(obs_raster[])] <- 0 
    rr_pot1 <- pred_raster - obs_raster
    rr_pot2 <- calc(rr_pot1, fun = function(x) {x[x<0] <- 0; return(x)})
    
    rasterfile <- paste0("Results/rasters/potential/", save_name, ".grd")
  
  writeRaster(rr_pot2, rasterfile, overwrite = T)
  return(plot(rr_pot2))
  
  }
  
  
# nonnative total 
  rr_pot(native_total_predicted, nonnative_total, "nonnative_total_potential")
  
# nonnative C3 
  rr_pot(native_C3_predicted, nonnative_C3, "nonnative_C3_potential")
  
# nonnative C4 
  rr_pot(native_C4_predicted, nonnative_C4, "nonnative_C4_potential")

# invasion potential index -----------------------------------------------
# total scaled invasion potential richness estimate
# required: 
# nat = native raster
# nonnat = nonnative raster
  output <- matrix (nrow = 3, ncol = 3)
  
# C3  
  output[1,1] <- sum(getValues(native_C3_predicted), na.rm = T)
  output[1,2] <- sum(getValues(nonnative_C3), na.rm = T)
  output[1,3] <- output[1,1] - output[1,2]
  
# C4
  output[2,1] <- sum(getValues(native_C4_predicted), na.rm = T)
  output[2,2] <- sum(getValues(nonnative_C4), na.rm = T)
  output[2,3] <- output[2,1] - output[2,2]
  
# Total 
  output[3,1] <- sum(getValues(native_total_predicted), na.rm = T)
  output[3,2] <- sum(getValues(nonnative_total), na.rm = T)
  output[3,3] <- output[3,1] - output[3,2]

  j <- round(data.frame(output), 0)  
  names(j) <- c("native_index", "nonnative", "IP_index")
  row.names(j) <- c("C3", "C4", "Total")

  write.csv(j, "Results/csv/invasion potential index.csv", row.names = T)  
  
# --------------------------------------------------------------------------  