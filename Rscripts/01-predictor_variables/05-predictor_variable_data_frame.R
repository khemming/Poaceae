

# library ---------------------------------------------------------
  library(raster)
  library(dplyr)
  library(rgdal)
  library(purrr)
  library(RColorBrewer)
  library(ggthemes)
  library(corrplot)
  library(factoextra)
  
  rm(list = ls())

# data ---------------------------------------------------  
# cell id, category, and proportion cover (made from Australia script)
  cells <- read.csv("Data files/Australia/Australia 2538.csv") %>%
           dplyr::select(lat, long, proportion_cover, cell_category, cell_id)
  head(cells)

# rasters 
  files <- list.files(path = "Results/rasters/predictor variables",
                      pattern = ".grd", full.names = T)
  names <- gsub(pattern = "Results/rasters/predictor variables/|.grd", "", files)
  names
  stack <- stack(files)
  names(stack) <- names
  list2env(setNames(unstack(stack), names), .GlobalEnv)
  
# dataframe ----------------------------------------------------------
  pv_v <- getValues(stack)
  pv_df <- as.data.frame(pv_v)
  names(pv_df)
# scale and nomralise 
  pvs_norm <- scale(pv_df, center = T, scale = T)
  pvs_norm <- data.frame(pvs_norm)

# bind with cell categories
  pvs_scaled <- cbind(cells, pvs_norm)  
  head(pvs_scaled)
  
# save
  write.csv(pvs_scaled, "Results/csv/predictor variables 2538.csv", row.names = F)
  
# ----------------------------------------------------------------------------

  