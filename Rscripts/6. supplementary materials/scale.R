
# library ----------------------------------------------
  library(tidyverse)
  library(raster)
  library(rgdal)
  
  rm(list = ls())

# data -------------------------------------------------
# Australia raster
  aus_1km <- raster("Data files/Australia/Australia 1 km.grd")
  plot(aus_1km)  
  
# shapefile
  oz <- readOGR("Data files/Australia/Australia shapefile.shp")
  crs(oz) <- "+proj=utm +zone=48 +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
  plot(oz)

# using all data to olook at coverage across Australia
  dat <- readRDS("Data files/ALA/master data/master grass records.rds") %>%
    dplyr::select(species, latitude, longitude, status) 
  xy <- cbind(dat$longitude, dat$latitude)
  spp <- as.numeric(as.factor(dat$species))
  
# summary statistics function via scale -----------------
# matrix
  x <- matrix(nrow = 7,
              ncol = 8)
  
# 10 - 300 km of different stats 
  scale <- c(10,  20,  50, 100, 
             150, 250, 500)
  
# loop ---------------------------------------------------
  for (i in 1:length(scale)){
  
  # total number of land cells
    x[, 1] <- scale
    raster <- aggregate(aus_1km, fact = scale[i], func = mean)
    total_cells <- length(na.omit(getValues(raster)))
    x[i, 2] <- total_cells
    
  # number of occupied cells with >=15 records
    rec_r <- rasterize(xy, raster, fun = function(x,...) {length(unique(na.omit(x))) })
    rec_df <- data.frame(getValues(rec_r)) 
    names(rec_df) <- "cells"
    occ_cells <- rec_df %>% 
                    filter(!is.na(cells),
                    cells >= 15)
    x[i, 3] <- nrow(occ_cells)
 
   # proportion of occupied cells
    x[i, 4] <- round(nrow(occ_cells)/total_cells, 2)
     
  # min, max, range
    min_rec <- min(occ_cells, na.rm = T)
    x[i, 5] <- min_rec
    max_rec <- max(occ_cells, na.rm = T)
    x[i, 6] <- max_rec
    range <- max_rec - min_rec
    x[i, 7] <- range
    
  # correlation between species richness and record number
    sr_r <- rasterize(xy, raster, field = spp, fun = function(x,...) {length(unique(na.omit(x))) })
    sr_v <- getValues(sr_r)
    sr_df <- data.frame(cbind(getValues(rec_r), sr_v))
    sr_narm <- sr_df %>% filter(!is.na(V1),
                                V1 >= 15)
    corr <- round(cor(sr_narm$V1, sr_narm$sr_v, use = "complete.obs", method = "pearson"), 2)
    x[i, 8] <- corr
    
  }
  
  y <- data.frame(x)
  names(y) <- c("scale",     "total_cells",          
                "occ_cells", "occ_cells_prop", 
                "min",       "max",               
                "range",     "sr_rec_cor")
  y
  glimpse(y)

# save
  write.csv(y, "Results/csv/supplementary materials - scale.csv", row.names = F)

# rasters of different scales ---------------------------------
# 10
  aus_10 <- aggregate(aus_1km, fact = 10, fun = mean)
  aus_10_r <- rasterize(xy, aus_10, fun = function(x,...) {length(unique(na.omit(x))) })
  writeRaster(aus_10_r, "Results/rasters/supplementary materials/10 km.grd", overwrite = T)
# 20  
  aus_20 <- aggregate(aus_1km, fact = 20, fun = mean)
  aus_20_r <- rasterize(xy, aus_20, fun = function(x,...) {length(unique(na.omit(x))) })
  writeRaster(aus_20_r, "Results/rasters/supplementary materials/20 km.grd", overwrite = T)
# 50  
  aus_50 <- aggregate(aus_1km, fact = 50, fun = mean)
  aus_50_r <- rasterize(xy, aus_50, fun = function(x,...) {length(unique(na.omit(x))) })
  writeRaster(aus_50_r, "Results/rasters/supplementary materials/50 km.grd", overwrite = T)
# 100  
  aus_100 <- aggregate(aus_1km, fact = 100, fun = mean)
  aus_100_r <- rasterize(xy, aus_100, fun = function(x,...) {length(unique(na.omit(x))) })
  writeRaster(aus_100_r, "Results/rasters/supplementary materials/100 km.grd", overwrite = T)
# 150  
  aus_150 <- aggregate(aus_1km, fact = 150, fun = mean)
  aus_150_r <- rasterize(xy, aus_150, fun = function(x,...) {length(unique(na.omit(x))) })
  writeRaster(aus_150_r, "Results/rasters/supplementary materials/150 km.grd", overwrite = T)
# 250  
  aus_250 <- aggregate(aus_1km, fact = 250, fun = mean)
  aus_250_r <- rasterize(xy, aus_250, fun = function(x,...) {length(unique(na.omit(x))) })
  writeRaster(aus_250_r, "Results/rasters/supplementary materials/250 km.grd", overwrite = T)
# 500  
  aus_500 <- aggregate(aus_1km, fact = 500, fun = mean)
  aus_500_r <- rasterize(xy, aus_500, fun = function(x,...) {length(unique(na.omit(x))) })
  writeRaster(aus_500_r, "Results/rasters/supplementary materials/500 km.grd", overwrite = T)
  
# -------------------------------------------------------------
    