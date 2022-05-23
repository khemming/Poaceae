
########################################################################################
# step six: 100 km scale data frame of EVs
########################################################################################

# scope -------------------------------------------------------------
# 100 km dataframe of all EVs for input into linear regressions, centred and scaled
# with cell categories to filter cell lists later

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
  cells <- read.csv("Data files/Australia/Australia 100 km.csv")
  head(cells)

# rasters 
  setwd("./Results/EVs/Rasters/100 km")
  
  current.list <- list.files(pattern = ".grd")
  names <- gsub(pattern = "\\.grd$", "", current.list)
  
  c.stack <- stack(current.list)
  names(c.stack) <- names
  
  list2env(setNames(unstack(c.stack), names(c.stack)), .GlobalEnv)
  
  setwd("C:/Users/s436862/Dropbox/Poaceae")

# dataframe -------------------------------------------------------------------
  ef.v <- getValues(c.stack)
  ef.df <- as.data.frame(ef.v)

# scale and nomralise 
  evs_norm <- scale(ef.df, center = T, scale = T)
  evs_norm <- data.frame(evs_norm)

# bind with cell categories
  evs_scaled <- cbind(cells, evs_norm)  

  head(evs_scaled)
  
# save  
  write.csv(evs_scaled, "./Results/EVs/CSV/EVs scaled.csv", row.names = F)


# ----------------------------------------------------------------------------
