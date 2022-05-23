############################################################
# Australia raster at 1 and 100 km scales
############################################################
# scope ----------------------------------------------------
# have the rasters and data frames that account fo roceanic and land cells

# library --------------------------------------------------
  library(raster)
  library(rgdal)
  library(dplyr)

  rm(list = ls())
# data -----------------------------------------------------  
# Australia outline
  aus.shp <- readOGR("Data files/Australia/australia_shapefile.shp")

# template raster
  arid <- raster("Results/EVs/Rasters/1 km/arid.grd")

# 1 km template --------------------------------------------
# with cell numbers
  plot(arid)
  plot(aus.shp, add = T)
  
  aus <- arid
  aus.val <- setValues(aus, 1:ncell(aus))
  
  aus.masked <- mask(aus.val, aus)
  plot(aus.masked)
  
# save
  writeRaster(aus.masked, "Data files/Australia/Australia 1 km.grd", overwrite = T)

# -----------------------------------------------------------