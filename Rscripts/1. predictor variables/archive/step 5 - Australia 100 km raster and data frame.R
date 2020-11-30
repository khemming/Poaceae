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
  aus.shp <- readOGR("Data files/Australia/australia_shapefile.shp")
  
# PEWC raster
  pewc <- raster("Results/EVs/rasters/100 km/pewc.grd")

# 100-km --------------------------------------------------------------------
# cut out Australia and provide cell values
  plot(aus)
  aus.100 <- aggregate(aus, fact = 100, fun = mean)
  plot(aus.100)
  plot(aus.shp, add = T)
  
  aus.100.template <- setValues(aus.100, 1:ncell(aus.100))
  plot(aus.100.template)
  aus.100.masked <- mask(aus.100.template, aus.100)
  plot(aus.100.masked)
  writeRaster(aus.100.masked, "Data files/Australia/Australia 100 km.grd", overwrite = T)
  
  
# proportion of cell covered by shapefile
  prop.cover1 <- rasterize(aus.shp, aus.100, getCover = T)
  prop.cover2 <- data.frame(getValues(prop.cover1))
  prop.cover3 <- prop.cover2/100
  
# cell_id
  cell.id <- 1:length(aus.100)
  
# cell category for all cells: ocean or land
  cell.category.all <- ifelse(prop.cover3 > 0, "land", "ocean")
  
# df
  aus.cells.100km <- data.frame(cbind(cell.id, cell.category.all, prop.cover3))
  colnames(aus.cells.100km) <- c("cell.id", "cell.category.all", "prop.cover")
  
# PEWC-corrected raster and data frame col ----------------------------------------  
  plot(pewc)
  pewc1 <- setValues(pewc, 1:ncell(aus.100))
  plot(pewc1)
  pewc2 <- mask(pewc1, pewc)
  plot(pewc2)
  writeRaster(pewc2, "Data files/Australia/Australia 100 km pewc.grd", overwrite = T)
  
# pewc column
  pewc_df <- getValues(pewc2)
  pewc_df <- data.frame(pewc_df)
  table(table(pewc_df, exclude = NULL))
  
# cell category for all cells: ocean or land
  cell.category.pewc <- ifelse(is.na(pewc_df) == F, "land", "ocean")
  cell.category.pewc <- data.frame(cell.category.pewc)
  names(cell.category.pewc) <- "cell.category.pewc"

# bind
  aus_df <- cbind(aus.cells.100km, cell.category.pewc)
  head(aus_df)
# save  
  write.csv(aus_df, "Data files/Australia/Australia 100 km.csv", row.names = F)
  
# ----------------------------------------------------------------------