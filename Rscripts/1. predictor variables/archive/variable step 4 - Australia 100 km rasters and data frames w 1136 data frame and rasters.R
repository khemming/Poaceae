

# library ---------------------------------------------------------
  library(raster)
  library(tidyverse)
  library(rgdal)
  
  rm(list = ls())

# data files ------------------------------------------------------
# Australia outline
  aus_shp <- readOGR("Data files/Australia/Australia shapefile.shp")
  
# PEWC raster as template
  aus_100 <- raster("Results/rasters/predictor variables/pewc.grd")

# 100 km Australia --------------------------------------------------------------------
# cut out Australia and provide cell values
  plot(aus_100)
  plot(aus_shp, add = T)
  temp <- setValues(aus_100, 1:ncell(aus_100))
  plot(temp)
  mask <- mask(temp, aus_100)
  aus_cells <- length(na.omit(getValues(aus_100)))
  aus_cells 
  plot(mask)
  writeRaster(mask, "Data files/Australia/Australia 1102.grd", overwrite = T)

# lat/long centroid coordinates of raster cells (x/y)  
  cell_list <- rasterToPoints(mask)
  cell_list <- data.frame(cell_list)
  head(cell_list)
  names(cell_list) <- c("long", "lat", "cell_id")
  
# proportion of cell covered by shapefile
  pc1 <- rasterize(aus_shp, aus_100, getCover = T)
  pc2 <- data.frame(getValues(pc1))
  
# cell_id
  cell_id <- 1:length(aus_100)
  
# cell category for all cells: ocean or land
  cell_category_1136 <- ifelse(pc2 > 0, "land", "ocean")
  table(table(cell_category_1136, exclude = NULL))
  
# df
  aus_df <- data.frame(cbind(cell_id, cell_category_1136, pc2))
  colnames(aus_df) <- c("cell_id", "cell_category_1136", "proportion_cover")
  
# add in coordinates of land-cells
  aus_df2 <- left_join(aus_df, cell_list, by = "cell_id")
  slice(aus_df2, 400:410)
  
# PEWC-corrected raster and data frame col ----------------------------------------  
  plot(pewc)
  pewc1 <- setValues(pewc, 1:ncell(aus_100))
  plot(pewc1)
  pewc2 <- mask(pewc1, pewc)
  plot(pewc2)
  writeRaster(pewc2, "Data files/Australia/Australia 1102.grd", overwrite = T)
  
# pewc column
  pewc_df <- getValues(pewc2)
  pewc_df <- data.frame(pewc_df)
  table(table(pewc_df, exclude = NULL))
  
# cell category for all cells: ocean or land
  cell_category_1102 <- ifelse(is.na(pewc_df) == F, "land", "ocean")
  cell_category_1102 <- data.frame(cell_category_1102)
  names(cell_category_1102) <- "cell_category_1102"

# bind
  aus_df3 <- cbind(aus_df2, cell_category_1102)
  head(aus_df3)
  
# save  
  write.csv(aus_df3, "Data files/Australia/Australia 1102.csv", row.names = F)
  
# ----------------------------------------------------------------------------------
  
  