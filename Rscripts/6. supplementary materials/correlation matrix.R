#######################################################################
# correlation matrix
#######################################################################

# library -------------------------------------------------------------
  library(tidyverse)
  library(broom)
  library(magrittr)
  library(raster)
  
  rm(list = ls())
  
# data ----------------------------------------------------------------
# observed 
  setwd("C:/Users/Hemming/Dropbox/Poaceae/Results/rasters/scaled")
  current.list <- list.files(pattern = ".grd")
  names_long <- gsub(pattern = "\\.grd$", "", current.list)
  ob.stack <- stack(current.list)
  names(ob.stack) <- names_long
  list2env(setNames(unstack(ob.stack), names(ob.stack)), .GlobalEnv)
  
# predicted  
  setwd("C:/Users/Hemming/Dropbox/Poaceae/Results/rasters/predicted")
  current.list <- list.files(pattern = ".grd")
  names_long <- gsub(pattern = "\\.grd$", "", current.list)
  pr.stack <- stack(current.list)
  names(pr.stack) <- names_long
  list2env(setNames(unstack(pr.stack), names(pr.stack)), .GlobalEnv)
  
  setwd("C:/Users/Hemming/Dropbox/Poaceae")
  
# data frame
  spp_stack <- stack(ob.stack, pr.stack)
  spp <- data.frame(getValues(spp_stack))
  spp2 <- spp %>% dplyr::select(native_C3, native_C3_predicted, 
                                native_C4, native_C4_predicted,
                                nonnative_C3, nonnative_C3_predicted,
                                nonnative_C4, nonnative_C4_predicted)
  
# correlation matrix ---------------------------------------------------
  spp_cm <- round(cor(spp2, method = "spearman", use = "pairwise.complete.obs"), 2)
    
  
# save
  write.csv(spp_cm, "Results/csv/spp correlation matrix.csv")
  
# ----------------------------------------------------------------------  
  x <- spp$native_C4_predicted
  y <- spp$nonnative_C4
  sprintf("%.2f", cor(x, y, method = "spearman", use = "complete.obs"), 2)
  