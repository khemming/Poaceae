########################################################################################
# step five: Australia 100 km raster and data frame
########################################################################################

# scope -------------------------------------------------------------
# 1 and 100 km dataframe of Australia done two ways
# first, a 'complete' version -- will all land-cells
# second, a PEWC-corrected version, which turns otherwise land-cells that have no associated PEWC-values to ocean-cells

# library ---------------------------------------------------------
  library(raster)
  library(tidyverse)
  library(rgdal)
  
  rm(list = ls())

# data files ------------------------------------------------------
# raster
  aus <- raster("Data files/Australia/Australia 1 km.grd")
  
# Australia outline
  aus_shp <- readOGR("Data files/Australia/australia_shapefile.shp")
  
# PEWC raster
  pewc <- raster("Results/EVs/rasters/100 km/pewc.grd")

# 100-km --------------------------------------------------------------------
# cut out Australia and provide cell values
  plot(aus)
  aus_100 <- aggregate(aus, fact = 100, fun = mean)
  plot(aus_100)
  plot(aus_shp, add = T)
  
  aus_100_template <- setValues(aus_100, 1:ncell(aus_100))
  plot(aus_100_template)
  aus_100_masked <- mask(aus_100_template, aus_100)
  plot(aus_100_masked)
  writeRaster(aus_100_masked, "Data files/Australia/Australia 100 km.grd", overwrite = T)
  
  
# proportion of cell covered by shapefile
  prop.cover1 <- rasterize(aus_shp, aus_100, getCover = T)
  prop.cover2 <- data.frame(getValues(prop.cover1))
  prop.cover3 <- prop.cover2/100
  
# cell_id
  cell_id <- 1:length(aus_100)
  
# cell category for all cells: ocean or land
  cell_category_all <- ifelse(prop.cover3 > 0, "land", "ocean")
  
# df
  aus_cells_100km <- data.frame(cbind(cell_id, cell_category_all, prop.cover3))
  colnames(aus_cells_100km) <- c("cell_id", "cell_category_all", "prop.cover")
  
# PEWC-corrected raster and data frame col ----------------------------------------  
  plot(pewc)
  pewc1 <- setValues(pewc, 1:ncell(aus_100))
  plot(pewc1)
  pewc2 <- mask(pewc1, pewc)
  plot(pewc2)
  writeRaster(pewc2, "Data files/Australia/Australia 100 km pewc.grd", overwrite = T)
  
# pewc column
  pewc_df <- getValues(pewc2)
  pewc_df <- data.frame(pewc_df)
  table(table(pewc_df, exclude = NULL))
  
# cell category for all cells: ocean or land
  cell_category_pewc <- ifelse(is.na(pewc_df) == F, "land", "ocean")
  cell_category_pewc <- data.frame(cell_category_pewc)
  names(cell_category_pewc) <- "cell_category_pewc"

# bind
  aus_df <- cbind(aus_cells_100km, cell_category_pewc)
  head(aus_df)
# save  
  write.csv(aus_df, "Data files/Australia/Australia 100 km.csv", row.names = F)
  
# ----------------------------------------------------------------------