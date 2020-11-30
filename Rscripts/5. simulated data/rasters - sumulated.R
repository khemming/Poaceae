

# scope ----------------------------------------------------------------
# simulated rasters of:
# complete native nonnative richness, 
# complete nonnative richness (marginally adjusted native richness)
# marginal invasion potential
# incomplete nonnative richness
# considerable invasion potential


# library --------------------------------------------------------------
  library(raster)
  library(rgdal)
  library(tidyverse)
  
  rm(list = ls())

# data -----------------------------------------------------------------
# creating raster data 
# large scale Australia 
  shp <- readOGR("Data files/Australia/Australia shapefile.shp")
  aus <- raster("Data files/Australia/Australia 1 km.grd")
  aus_200 <- aggregate(aus, fact = 200, fun = mean)
  plot(aus_200)
  plot(shp, add = T)
  
# simulating a north-south species richness gradient across Australia
# use this for both native and nonnative distributions
  aus_t1 <- setValues(aus_200, 1:ncell(aus_200))
  plot(aus_t1)
  aus_t2 <- mask(aus_t1, aus_200)
  plot(aus_t2)
  
# use longitude to assign cell values
  xy <- data.frame(xyFromCell(aus_t2, 1:length(aus_t2)))
  head(xy)
  xy$z <- round(rev(xy$y), 2)
  aus_t3 <- setValues(aus_t2, xy$z)
  aus_t4 <- mask(aus_t3, aus_200)
  aus_t5 <- aus_t4/min(xy$z, na.rm = T)
  plot(aus_t5)
  
# adjust raster data by marginal amount
  adj_seq <- seq(-0.05, 0.05, 0.01)

# native complete raster ---------------------------------------------------------------------------------
  nat_adj_sam <- sample(adj_seq, length(getValues(aus_t5)), replace = T)
  nat_comp <- aus_t5 - nat_adj_sam
  nat_comp_s <- nat_comp/cellStats(nat_comp, stat = "max") # scale back to 0 - 1
  plot(nat_comp_s)
  
# nonnative complete raster ----------------------------------------------------------------------------
  nonnat_adj_sam <- sample(adj_seq, length(getValues(aus_t5)), replace = T)
  nonnat_comp <- aus_t5 - nonnat_adj_sam
  nonnat_comp_s <- nonnat_comp/cellStats(nonnat_comp, stat = "max") # scale back to 0 - 1
  plot(nonnat_comp)
   
# minor invasion potential --------------------------------------------------------------------------------
  nonnat_comp[is.na(nonnat_comp[])] <- 0 
  m_sp <- nat_comp - nonnat_comp
  m_sp2 <- calc(m_sp, fun = function(x) {x[x<0] <- 0; return(x)})
  m_sp3 <- mask(m_sp2, aus_t5)
  plot(m_sp3)

# incomplete nonnative distribution ---------------------------------------------------------------------
# two steps: first, make a "northern" distribution; second, make it sneak down the East coast
# data 
  incomp <- data.frame(getValues(nonnat_comp))
  incomp$cell_id <- 1:length(nonnat_comp)
  colnames(incomp) <- c("value", "cell_id")
  head(incomp)
 
# plot Aus cells with/without values in window  
# set Console window to display 27 cols -- 622 in the bottom lefthand corner
  window1 <- setValues(aus_200, 1:length(aus_200))
  window2 <- mask(window1, aus_200)
  getValues(window2)
  
# north    
  nrth <- calc(nonnat_comp, fun = function(x) {x[x<0.68] <- NA; return(x)})
  plot(nrth)
  nrth_v <- data.frame(getValues(nrth))
  nrth_v$cell_id <- 1:length(nrth_v)
  colnames(nrth_v) <- c("value", "cell_id")
  head(nrth_v)
  
  north_ras <- mask(window1, nrth)
  nv <- getValues(north_ras)
  
# east
  east <- c(230, 235:236, 238, 240, 
            262, 264, 266:268,
            287, 289, 293:296, 
            319:320, 322:323, 
            348:350, 
            373, 375:377, 
            401:404,
            429:431,
            456:457,
            482:483,
            510)
  
# combine 
  nn_incomp <- nonnat_comp
  nn_incomp[ c(nv,
               east)] <- NA
 
# incomplete nonnative richness (continued at the bottom)
  nn_incomp2 <- mask(nonnat_comp, nn_incomp, inverse = F)
  nn_incomp2b <- mask(nn_incomp2, aus_t5)
  plot(nn_incomp2b)

# set some cell values to less than native richness
  nn_cor3 <- nn_incomp2b
  getValues(mask(window2, nn_cor3))
  # set Console window to display 27 cols -- 622 in the bottom lefthand corner -------------------------------------------------------------------|
  lower <- c(219, 252:254, 232:233, 
             299, 300, 302:304, 309, 314:315, 
             330, 346, 313:315, 239,
             246, 257, 259:261, 328, 330, 
             272, 274:276, 283, 285:286, 288, 
             328, 330, 345, 356, 290:292,
             504, 321, 265, 280, 309, 306, 277)
  nn_cor3[lower] <- nn_cor3[lower]*rnorm(lower, mean = 0.7, sd = 0.2)
  plot(nn_cor3)
  
  much_lower <- c(223, 231, 258, 246:248, 250, 255, 259:261, 
                  275, 283, 285:286, 288, 347, 274,
                  290:292, 339, 344:345, 350:352, 
                  367, 400, 342, 317, 369, 273)
  nn_cor3[much_lower] <- nn_cor3[much_lower]-0.3
  nn_cor4 <- calc(nn_cor3, fun = function(x) {x[x<0] <- 0; return(x)})
  plot(nn_cor4)
  getValues(mask(window2, nn_cor4))
  
# substantial invasion potential -----------------------------------------------------------------
  nn_incomp5 <- nn_incomp2b
  nn_incomp5[is.na(nn_incomp5[])] <- 0 
  s_sp <- nat_comp - nn_incomp5
  s_sp2 <- calc(s_sp, fun = function(x) {x[x<0] <- 0; return(x)})
  s_sp3 <- mask(s_sp2, aus_t5)
  plot(s_sp3)
  
# non-correlated nonnative ---------------------------------------------------------------------
  set.seed(496)
  
  nc_nn <- nn_incomp5
  nc_nn_v <- getValues(nc_nn)
  
  nc_sam <- runif(length(getValues(nc_nn)), 0, 1)
  
  nc_nn_v2 <- setValues(nc_nn, nc_sam)
  nc_nn_v4 <- mask(nc_nn_v3, nn_cor3)
  plot(nc_nn_v4)
  
# save rasters -----------------------------------------------------------------------------
  writeRaster(nat_comp_s, "Results/simulated results/native complete.grd", overwrite = T)
  writeRaster(nonnat_comp_s, "Results/simulated results/nonnative complete.grd", overwrite = T)
  writeRaster(m_sp3, "Results/simulated results/marginal invasion potential.grd", overwrite = T)
  writeRaster(nn_cor4, "Results/simulated results/nonnative incomplete.grd", overwrite = T)
  writeRaster(s_sp3, "Results/simulated results/substantial invasion potential.grd", overwrite = T)
  writeRaster(nc_nn_v4, "Results/simulated results/nonnative no correlation.grd", overwrite = T)
  
# ------------------------------------------------------------------------    
  
  