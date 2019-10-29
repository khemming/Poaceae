####################################################################
# iNEXT model
####################################################################
# date created: 2/5/19
# last updated:  

# aims ---------------------------------------------------------------------
# 1. model using previous version of the rarefaction technique (independent methodology rather than the proportion; but with the iNEXT estimator v2 results)
# 2. predict distributions using these models

# library ------------------------------------------------------------------
  library(dplyr)
  library(broom)
  library(magrittr)
  library(raster)

  rm(list = ls())
 
# 1. modelling observed data -------------------------------------------   
# data ----------------------------------------------------------------   
# rarefied data (generated from 'Plot' script)
  setwd("C:/Users/s436862/Dropbox/Poaceae/Results/iNEXT/Rasters/observed 15 rec 0.8 cov warn removed")
  
  current.list <- list.files(pattern = ".grd")
  names <- gsub(pattern = "\\.grd$", "", current.list)
  
  c.stack <- stack(current.list)
  names(c.stack) <- names
  
  list2env(setNames(unstack(c.stack), names(c.stack)), .GlobalEnv)
  
  spp <- as.data.frame(c.stack, na.rm = F)
 
# environmental data (retained from variable selection process)
# note this is also the order these pop up in the coefficient plots, so vary it here if need be
  setwd("C:/Users/s436862/Dropbox/Poaceae/Results")
  
  vs.evs <- c("cell.category.v2", "prop.cover", "pcoldq", "pwarmq", "amt", "ts", "arid", "pewc", "th", "hii")
  
# select from wider EV list
  evs <- read.csv("EVs/CSV/100 km EFs scaled.csv", header = T)
  
# bind spp and EV data and subset to terrestrial only cells   
  spp.ev <- cbind(evs, spp) %>%
    filter(cell.category.v2 == "land") %>%
    dplyr::select(-cell.category.v2)
  
# models --------------------------------------------------------------
# native total 
  m.n.tot <- lm(n.tot ~ pcoldq + pwarmq * amt + ts + arid + pewc + th + hii + prop.cover, data = spp.ev)
  summary(m.n.tot) 
  m.n.tot.sum <- tidy(m.n.tot)
  m.n.tot.ci <- confint(m.n.tot)
  
# native C3  
  m.n.c3 <- lm(n.c3 ~ pcoldq + pwarmq * amt + ts + arid + pewc + th + hii + prop.cover, data = spp.ev)
  summary(m.n.c3) 
  m.n.c3.sum <- tidy(m.n.c3)
  m.n.c3.ci <- confint(m.n.c3)

# native C4
  m.n.c4 <- lm(n.c4 ~ pcoldq + pwarmq * amt + ts + arid + pewc + th + hii + prop.cover, data = spp.ev)
  summary(m.n.c4) 
  m.n.c4.sum <- tidy(m.n.c4)
  m.n.c4.ci <- confint(m.n.c4)
   
# non-native total 
  m.e.tot <- lm(e.tot ~ pcoldq + pwarmq * amt + ts + arid + pewc + th + hii + prop.cover, data = spp.ev)
  summary(m.e.tot) 
  m.e.tot.sum <- tidy(m.e.tot)
  m.e.tot.ci <- confint(m.e.tot)
  
# non-native C3  
  m.e.c3 <- lm(e.c3 ~ pcoldq + pwarmq * amt + ts + arid + pewc + th + hii + prop.cover, data = spp.ev)
  summary(m.e.c3) 
  m.e.c3.sum <- tidy(m.e.c3)
  m.e.c3.ci <- confint(m.e.c3)

# non-native C4
  m.e.c4 <- lm(e.c4 ~ pcoldq + pwarmq * amt + ts + arid + pewc + th + hii + prop.cover, data = spp.ev)
  summary(m.e.c4) 
  m.e.c4.sum <- tidy(m.e.c4)
  m.e.c4.ci <- confint(m.e.c4)
  
# coefficient dataframe --------------------------------------------
# note we are dropping prop.cover
# Status (n/e) | coefficient (evs) | mean | lower.ci | upper.ci
  spp.ev.mat <- matrix(ncol = 5, nrow = 16)  
  spp.ev.mat[,1] <- rep(c("native", "non-native"), each = 8)
  spp.ev.mat[,2] <- rep(vs.evs[3:10], 2)
  colnames(spp.ev.mat) <- c("status", "coef", "estimate", "lower.ci", "upper.ci")

# one for each total, C3 and C4 richness (i.e. native and non-native will be plotted together) 
  tot.rich <- spp.ev.mat
  c3.rich <- spp.ev.mat
  c4.rich <- spp.ev.mat
  
# insert coeffficents & CIs --------------------------------
# 2:9 excludes the intercept and prop.cover terms
# total native  
  tot.rich[1:8, 3] <- m.n.tot.sum$estimate[2:9]
  tot.rich[1:8, 4:5] <- m.n.tot.ci[2:9, ]
# total non-native  
  tot.rich[9:16, 3] <- m.e.tot.sum$estimate[2:9]
  tot.rich[9:16, 4:5] <- m.e.tot.ci[2:9, ]
  
# C3 native   
  c3.rich[1:8, 3] <- m.n.c3.sum$estimate[2:9]
  c3.rich[1:8, 4:5] <- m.n.c3.ci[2:9, ]
# C3 non-native 
  c3.rich[9:16, 3] <- m.e.c3.sum$estimate[2:9]
  c3.rich[9:16, 4:5] <- m.e.c3.ci[2:9, ]
  
# C4 native  
  c4.rich[1:8, 3] <- m.n.c4.sum$estimate[2:9]
  c4.rich[1:8, 4:5] <- m.n.c4.ci[2:9, ]
# C4 non-native  
  c4.rich[9:16, 3] <- m.e.c4.sum$estimate[2:9]
  c4.rich[9:16, 4:5] <- m.e.c4.ci[2:9, ]
 
# data frame   
  tot.rich <- data.frame(tot.rich)
  c3.rich <- data.frame(c3.rich)
  c4.rich <- data.frame(c4.rich)
  
# mean and CIs as numbers for plotting  
  tot.rich[,3:5] %<>% lapply(function(x) as.numeric(as.character(x)))
  c3.rich[,3:5] %<>% lapply(function(x) as.numeric(as.character(x)))
  c4.rich[,3:5] %<>% lapply(function(x) as.numeric(as.character(x)))
  
# save workspace to use for plotting
  save.image("iNEXT/Rdata/model_coefficients.RData") 
   
# check ------------------------------------------------------
# plotting residuals of models
  plot(m.n.c3$residuals)
  plot(m.n.c4$residuals)
  plot(m.e.c3$residuals)
  plot(m.e.c4$residuals) # all all good

  hist(m.n.c3$residuals)
  hist(m.n.c4$residuals)
  hist(m.e.c3$residuals)
  hist(m.e.c4$residuals) # all all good
# ----------------------------------------------------------------------------

  
# 2. modelling predicted data -------------------------------------------
# data ----------------------------------------------------------------   
# model data (from above)
  rm(list = ls())
  setwd("C:/Users/s436862/Dropbox/Poaceae/Results")
  
  load("iNEXT/Rdata/model_coefficients.RData")
  
# raster (100-km scale)
  aus <- raster("C:/Users/s436862/Dropbox/Poaceae/Data files/Australia/aus 100 km v2.grd")
  
# environmental data
# retained from varaible selection process
  vs.evs <- c("cell.category.v2", "prop.cover", "pcoldq", "pwarmq", "amt", "ts", "arid", "pewc", "th", "hii")
  
# select from wider EV list
  pred.ev <- read.csv("EVs/CSV/100 km EFs scaled.csv", header = T) %>%
    dplyr::select(vs.evs) %>%
    mutate(prop.cover = prop.cover/100) %>% # convert prop cover from 0 - 1 
    filter(cell.category.v2 == "land") %>%
    dplyr::select(-cell.category.v2)
  
  
# predicting distributions ------------------------------------------------
# native ----------------------------------------------------------------------
# total richness
  nat.tot.pred <- predict(m.n.tot, newdata = pred.ev)
  
# C3 richness  
  nat.c3.pred <- predict(m.n.c3, newdata = pred.ev)
  
# C4 richness  
  nat.c4.pred <- predict(m.n.c4, newdata = pred.ev)
  
# non-native ----------------------------------------------------------------------
# total richness
  exo.tot.pred <- predict(m.e.tot, newdata = pred.ev)
  
# C3 richness  
  exo.c3.pred <- predict(m.e.c3, newdata = pred.ev)
  
# C4 richness  
  exo.c4.pred <- predict(m.e.c4, newdata = pred.ev) 
  
# rasterise predicted values --------------------------------------------------
# turn 1003 cells to 2538 in correct order ------------------------------------
# cell ID
  cell.id <- read.csv("EVs/CSV/100 km EFs scaled.csv", header = T) %>%
    filter(cell.category.v2 == "land") %>%
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
  
# producing rasters --------------------------------------------------------------
  setwd("C:/Users/kyleh/Dropbox/Poaceae/Results/iNEXT/Rasters/predicted 15 rec 0.8 cov warn removed")

# native  
  n.tot.p <- setValues(aus, x[, 1])
  writeRaster(n.tot.p, "n.tot.predicted", overwrite = T)
  plot(n.tot.p)
  
  n.c3.p <- setValues(aus, x[, 2])
  writeRaster(n.c3.p, "n.c3.predicted", overwrite = T)
  plot(n.c3.p)
  
  n.c4.p <- setValues(aus, x[, 3])
  writeRaster(n.c4.p, "n.c4.predicted", overwrite = T)
  plot(n.c4.p)
  
# non-native  
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
  
  
 