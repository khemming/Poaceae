
##############################################################
# predictive modelling of native and exotic distributions
##############################################################

# based on outputs from Rarefaction chapter model v7

# date created: 20/8/18
# updated: 


# library ------------------------------------------------------------------
  library(dplyr)
  library(broom)
  library(magrittr)
  library(raster)

  rm(list = ls())
  
  setwd("C:/Users/s436862/Dropbox/Rarefaction/Results")
  
# data ----------------------------------------------------------------   
# model data
  load("Rarefaction/Rdata/rarefaction_model_coefficient_values.RData")
  
# raster (100-km scale)
  aus <- raster("C:/Users/s436862/Dropbox/Rarefaction/Data files/Australia/aus 100 km v2.grd")
  
# environmental data
# retained from varaible selection process
  vs.evs <- c("cell.cat", "prop.cover", "pcoldq", "pwarmq", "amt", "ts", "arid", "pewc", "th", "hii")
  
# select from wider EV list
  pred.ev <- read.csv("EVs/CSV/100 km EFs scaled.csv", header = T) %>%
              dplyr::select(vs.evs) %>%
              mutate(prop.cover = prop.cover/100) %>% # convert prop cover from 0 - 1 
              filter(cell.cat == "land") %>%
              dplyr::select(-cell.cat)
    
    
# 2.2 Predicting distributions ------------------------------------------------
# Native ----------------------------------------------------------------------
# Total richness
  nat.tot.pred <- predict(m.n.tot, newdata = pred.ev)
  
# C3 richness  
  nat.c3.pred <- predict(m.n.c3, newdata = pred.ev)
  
# C4 richness  
  nat.c4.pred <- predict(m.n.c4, newdata = pred.ev)
  
# Exotic ----------------------------------------------------------------------
# Total richness
  exo.tot.pred <- predict(m.e.tot, newdata = pred.ev)
  
# C3 richness  
  exo.c3.pred <- predict(m.e.c3, newdata = pred.ev)
  
# C4 richness  
  exo.c4.pred <- predict(m.e.c4, newdata = pred.ev) 
  
# Rasterise predicted values --------------------------------------------------
# Turn 1003 cells to 2538 in correct order ------------------------------------
# cell ID
  cell.id <- read.csv("EVs/CSV/100 km EFs scaled.csv", header = T) %>%
              filter(cell.cat == "land") %>%
              dplyr::select(cell.id)
  
# bind predicted distributions and add cell id
  pred.distr <- cbind(cell.id, 
                      nat.tot.pred, nat.c3.pred, nat.c4.pred,
                      exo.tot.pred, exo.c3.pred, exo.c4.pred)
  
  pred.distr <- data.frame(pred.distr)
  
# generate list of occupied cells
  cell.list <- pred.distr$cell.id
  
# make a matrix with all missing values
  x <- matrix(NA, nrow = length(getValues(aus)), ncol = 6)
  
  pred.distr.m <- as.matrix(pred.distr)
  pred.distr.mm <- pred.distr.m[, 2:7]
  class(pred.distr.mm) <- "numeric"
  
# add the occupied cells
  x[cell.list, ] <- pred.distr.mm
  
# Producing rasters --------------------------------------------------------------
  setwd("C:/Users/s436862/Dropbox/Rarefaction/Results/Rarefaction/Rasters/Predicted distributions")
# Native  
  n.tot.p <- setValues(aus, x[, 1])
  writeRaster(n.tot.p, "n.tot.predicted", overwrite = T)
  plot(n.tot.p)
  
  n.c3.p <- setValues(aus, x[, 2])
  writeRaster(n.c3.p, "n.c3.predicted", overwrite = T)
  plot(n.c3.p)
  
  n.c4.p <- setValues(aus, x[, 3])
  writeRaster(n.c4.p, "n.c4.predicted", overwrite = T)
  plot(n.c4.p)

# Exotic  
  e.tot.p <- setValues(aus, x[, 4])
  writeRaster(e.tot.p, "e.tot.predicted", overwrite = T)
  plot(e.tot.p)
  
  e.c3.p <- setValues(aus, x[, 5])
  writeRaster(e.c3.p, "e.c3.predicted", overwrite = T)
  plot(e.c3.p)
  
  e.c4.p <- setValues(aus, x[, 6])
  writeRaster(e.c4.p, "e.c4.predicted", overwrite = T)
  plot(e.c4.p)   
  
  
# -----------------------------------------------------------------------
 
  