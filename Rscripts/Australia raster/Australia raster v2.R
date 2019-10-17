
# Australia raster and template for oceanic and land cells --------------
# v2: added in PEWC cell.proportions and cut offs
# have not updated 50 km scale
  library(raster)
  library(rgdal)
  library(dplyr)

  setwd("C:/Users/s436862/Dropbox/Poaceae")

  rm(list = ls())
  
# Australia outline
  aus.shp <- readOGR("Data files/Australia/australia_shapefile.shp")
  
# 1 km (template) -----------------------------------------------------
  arid <- raster("Results/EVs/Rasters/1 km/arid.grd")
  plot(arid)
  plot(aus.shp, add = T)
  
  aus <- arid
  aus.val <- setValues(aus, 1:ncell(aus))
  
  aus.masked <- mask(aus.val, aus)
  plot(aus.masked)
  
# save
  writeRaster(aus.masked, "Data files/Australia/aus 1 km.grd", overwrite = T)

# 100-km --------------------------------------------------------------------
# notes -----------------------------------------------------
# where PEWC doesn't have land values: on cells with small proportions (<5%, and only a few), there are no PEWC values. These aren't computed in the model anyways (and predicted distributions don't have their cells plotted), so it's not really useful having them or plotting them
# ---------------------------------------------------------
# making the 100 km raster template without PEWC cells
  aus <- raster("Data files/Australia/aus 1 km.grd")
  plot(aus)
  aus.100 <- aggregate(aus, fact = 100, fun = mean)
  plot(aus.100)
  plot(aus.shp, add = T)
  
# proportion of cell covered by shapefile
  prop.cover <- rasterize(aus.shp, aus.100, getCover = T)
  prop.cover <- data.frame(getValues(prop.cover))
  prop.cover <- prop.cover/100
  colnames(prop.cover) <- "prop.cover"

# cell_id
  cell.id <- 1:length(aus.100)
  
# v1 (non-PEWC) cell category: ocean or land
  cell.category.v1 <- ifelse(prop.cover > 0, "land", "ocean")
  colnames(cell.category.v1) <- "cell.category.v1"

# v2 (PEWC) cell category
  pewc <- read.csv("Results/EVs/CSV/100 km EFs scaled.csv") %>%
          dplyr::select(pewc)
  cell.category.v2 <- ifelse(is.na(pewc) == T, "ocean", "land")
  colnames(cell.category.v2) <- "cell.category.v2"
  
  cells <- cbind(cell.id, prop.cover, cell.category.v1, cell.category.v2)
  head(cells)  

# save  
  write.csv(cells, "Data files/Australia/aus 100 km v2.csv", row.names = F)

# raster, PEWC-corrected
  pewc.ras <- raster("Results/EVs/Rasters/100 km/pewc.grd")
  plot(pewc.ras)
  aus.100.template <- setValues(aus.100, 1:ncell(aus.100))
  plot(aus.100.template)
  aus.100.masked <- mask(aus.100.template, aus.100)
  plot(aus.100.masked)
  pewc.aus <- mask(aus.100.masked, pewc.ras)
  plot(pewc.aus)
  writeRaster(pewc.aus, "Data files/Australia/aus 100 km v2.grd", overwrite = T)
  
# 50-km --------------------------------------------------------------------  
# have not updated to v2 equivalent
  aus <- raster("Data files/Australia/aus 1 km.grd")
  plot(aus)
  aus.50 <- aggregate(aus, fact = 50, fun = mean)
  plot(aus.50)
  plot(aus.shp, add = T)
  
# proportion of cell covered by shapefile
  prop.cover <- rasterize(aus.shp, aus.50, getCover = T)
  prop.coverage <- data.frame(getValues(prop.cover))
  
# cell_id
  cell.id <- 1:length(aus.50)
  
# cell category: ocean or land
  cell.category <- ifelse(prop.coverage > 0, "land", "ocean")
  
# df
  aus.cells.50km <- data.frame(cbind(cell.id, cell.category, prop.coverage))
  colnames(aus.cells.50km) <- c("cell.id", "cell.cat", "prop.cover")
  write.csv(aus.cells.50km, "Australia/aus 50-km.csv", row.names = F)

# raster template
  aus.50.template <- setValues(aus.50, 1:ncell(aus.50))
  plot(aus.50.template)
  aus.50.masked <- mask(aus.50.template, aus.50)
  plot(aus.50.masked)
  writeRaster(aus.50.masked, "Data files/Australia/aus 50-km.grd", overwrite = T)

# ----------------------------------------------------------------------