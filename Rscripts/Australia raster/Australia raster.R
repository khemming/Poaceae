
# Australia raster and template for oceanic and land cells --------------

  library(raster)
  library(rgdal)

  setwd("C:/Users/s436862/Dropbox/Rarefaction/Data files")

  rm(list = ls())
  
# Australia outline
  aus.shp <- readOGR("Australia/australia_shapefile.shp")
  
# 1-km (template) -----------------------------------------------------
  arid <- raster("EVs/Rasters 1-km Aus-cropped/arid.grd")
  plot(arid)
  plot(aus.shp, add = T)
  
  aus <- arid
  aus.val <- setValues(aus, 1:ncell(aus))
  
  aus.masked <- mask(aus.val, aus)
  plot(aus.masked)
  
# save
  writeRaster(aus.masked, "Australia/aus 1-km.grd", overwrite = T)

# 100-km --------------------------------------------------------------------
  aus <- raster("Australia/aus 1-km.grd")
  plot(aus)
  aus.100 <- aggregate(aus, fact = 100, fun = mean)
  plot(aus.100)
  plot(aus.shp, add = T)
          
# proportion of cell covered by shapefile
  prop.cover <- rasterize(aus.shp, aus.100, getCover = T)
  prop.coverage <- data.frame(getValues(prop.cover))
  
# cell_id
  cell.id <- 1:length(aus.100)
  
# cell category: ocean or land
  cell.category <- ifelse(prop.coverage > 0, "land", "ocean")
  
# df
  aus.cells.100km <- data.frame(cbind(cell.id, cell.category, prop.coverage))
  colnames(aus.cells.100km) <- c("cell.id", "cell.cat", "prop.cover")
  write.csv(aus.cells.100km, "Australia/aus 100-km.csv", row.names = F)

# raster template
  aus.100.template <- setValues(aus.100, 1:ncell(aus.100))
  plot(aus.100.template)
  aus.100.masked <- mask(aus.100.template, aus.100)
  plot(aus.100.masked)
  writeRaster(aus.100.masked, "Australia/aus 100-km.grd", overwrite = T)
  
# 50-km --------------------------------------------------------------------  
  aus <- raster("Australia/aus 1-km.grd")
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
  writeRaster(aus.50.masked, "Australia/aus 50-km.grd", overwrite = T)

# ----------------------------------------------------------------------