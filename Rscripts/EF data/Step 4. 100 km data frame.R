
########################################################################################
# step three: 100 km scale data frame of EVs
########################################################################################
# date created: 16/4/18
# last modified: 19/3/19 (update for new Australia scripts)

# aim -------------------------------------------------------------
# 100 km dataframe of all EFs for input into linear regressions, centred and scaled

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
  
  setwd("C:/Users/s436862/Dropbox/Poaceae")

# data ---------------------------------------------------  
# cell id, category, and proportion cover (made from Australia script)
  cells <- read.csv("C:/Users/s436862/Dropbox/Poaceae/Data files/Australia/aus 100 km v2.csv")

# rasters 
  setwd("C:/Users/s436862/Dropbox/Poaceae/Results/EVs/Rasters/100 km")
  
  current.list <- list.files(pattern = ".grd")
  names <- gsub(pattern = "\\.grd$", "", current.list)
  
  c.stack <- stack(current.list)
  names(c.stack) <- names
  
  list2env(setNames(unstack(c.stack), names(c.stack)), .GlobalEnv)

# dataframe -------------------------------------------------------------------
  ef.v <- getValues(c.stack)
  ef.df <- as.data.frame(ef.v)

# ef.df.c <- mutate(ef.df, cell.cat = case_when(!is.na(pdryq) ~ "terrestrial",
#                                                        TRUE ~ "ocean"))

# scale and nomralise 
  efs_norm <- scale(ef.df, center = T, scale = T)
  efs_norm <- data.frame(efs_norm)

  efs_scaled <- cbind(cells, efs_norm)  

# save  
  write.csv(efs_scaled, file = "C:/Users/s436862/Dropbox/Poaceae/Results/EVs/CSV/100 km EFs scaled.csv", row.names = F)



