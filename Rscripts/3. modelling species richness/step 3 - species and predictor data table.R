


# library -------------------------------------------------------------
  library(raster)
  library(tidyverse)
  
  rm(list = ls())

# data ----------------------------------------------------------------
# observed richness
  setwd("Results/rasters/scaled")
  current.list <- list.files(pattern = ".grd")
  names <- gsub(pattern = "\\.grd$", "", current.list)
  c.stack <- stack(current.list)
  names(c.stack) <- names
  list2env(setNames(unstack(c.stack), names(c.stack)), .GlobalEnv)
  spp_df <- as.data.frame(c.stack)
  setwd("C:/Users/s436862/Dropbox/Poaceae")

# environmental data
# retained from varaible selection process
  pv_vs <- c("cell_id", "cell_category", "proportion_cover", "lat", "long", "amt", "arid", "ts", "pwarmq", "pcoldq", "pewc", "th", "hii")
  
 # variables for Australia
   pv <- read.csv("Results/csv/predictor variables 2538.csv") %>%
         dplyr::select_at(pv_vs)
   
   pv_1104 <- pv %>% filter(cell_category == "land")
   write.csv(pv_1104, "Results/csv/predictor variables 1104.csv", row.names = F)     
   
# species - ev data frames
  spp_pv <- cbind(spp_df, pv)
  write.csv(spp_pv, "Results/csv/spp predictor variables 2538.csv", row.names = F)      

# subset land cells
  spp_pv_1104 <- bind_cols(spp_df, pv) %>%
                filter(cell_category == "land")
  write.csv(spp_pv_1104, "Results/csv/spp predictor variables 1104.csv", row.names = F)      

# ------------------------------------------------------------------      
  

  
  