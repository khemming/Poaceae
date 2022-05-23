

# scope ------------------------------------------------------------
# investigating autocorrelation of native C3 and C4 models

# library ------------------------------------------------------------------
  library(tidyverse)
  library(broom)
  library(magrittr)
  library(raster)
  library(MuMIn)
  library(ape)
  library(nlme)  
  library(car)
  library(rgdal)
  library(lmtest)
  library(gvlma)
  
  rm(list = ls())
  
# data ---------------------------------------------------------------------   
# model data
  load("Data files/rdata/models.RData")
  
# australia
  aus <- raster("Data files/Australia/Australia 100 km pewc.grd")
  
# log scaled richness data 
  setwd("C:/Users/Hemming/Dropbox/Poaceae/Results/rasters/log scaled")
  current.list <- list.files(pattern = ".grd")
  names <- gsub(pattern = "\\.grd$", "", current.list)
  log_rich <- stack(current.list)
  names(log_rich) <- names
  log_r <- as.data.frame(log_rich)
  setwd("C:/Users/Hemming/Dropbox/Poaceae")
  
# bind spp and EV data and subset land-only cells   
  log_pv <- cbind(log_r, pvs) %>%
    filter(cell_category_pewc == "land")
  head(log_pv)
  
# spatial data frame for modelling  
  spp_pv_sp <- spp_pv
  coordinates(spp_pv_sp) <- ~ lat + long
  log_pv_sp <- log_pv
  coordinates(log_pv_sp) <- ~ lat + long
  
# predictor variables for predicting 
  pred_pv <-  read.csv("Results/csv/predictor variables 1003.csv")

# looking at spatial data  ---------------------------------------------------
# linear models
  s_n3_lm <- gls(native_C3 ~ hii + th + pewc + pcoldq + pwarmq + ts + arid + amt + proportion_cover, 
               data = spp_pv_sp, na.action = na.omit)
  s_n4_lm <- gls(native_C4 ~ hii + th + pewc + pcoldq + pwarmq + ts + arid + amt + proportion_cover, 
               data = spp_pv_sp, na.action = na.omit)  
# gls models
  s_n3_gls <- gls(native_C3 ~ hii + th + pewc + pcoldq + pwarmq + ts + arid + amt + proportion_cover, 
                  data = spp_pv_sp, correlation = corSpher(form = ~long + lat, nugget=T), na.action = na.omit, method = "ML")
  s_n4_gls <- gls(native_C4 ~ hii + th + pewc + pcoldq + pwarmq + ts + arid + amt + proportion_cover, 
                  data = spp_pv_sp, correlation = corExp(form = ~long + lat, nugget=T), na.action = na.omit, method = "ML")
  
# log scaled lm
  l_n3_lm <- gls(native_C3 ~ hii + th + pewc + pcoldq + pwarmq + ts + arid + amt + proportion_cover, 
  data = log_pv_sp, na.action = na.omit)
  
  l_n4_lm <- gls(native_C4 ~ hii + th + pewc + pcoldq + pwarmq + ts + arid + amt + proportion_cover, 
               data = log_pv_sp, na.action = na.omit) 

# log scaled glm (use same glm strucutre as lms')
  l_n3_gls <- gls(native_C3 ~ hii + th + pewc + pcoldq + pwarmq + ts + arid + amt + proportion_cover, 
                 data = log_pv_sp, correlation = corSpher(form = ~long + lat, nugget=T), na.action = na.omit)
  
  l_n4_gls <- gls(native_C4 ~ hii + th + pewc + pcoldq + pwarmq + ts + arid + amt + proportion_cover, 
                 data = log_pv_sp, correlation = corExp(form = ~long + lat, nugget=T), na.action = na.omit)
  
# check model assumptions: heteroscedasticity 
  plot(s_n3_lm) # hetero
  plot(s_n4_lm) # hetero plus other stuff going on with small values
  
  plot(s_n3_gls) # similar to lm
  plot(s_n4_gls) # "           "
  
  plot(l_n3_lm) # looking good
  plot(l_n4_lm) # clustered but not hetero per se
  
  plot(l_n3_gls) # looking good
  plot(l_n4_gls) # clustered but not hetero per se
  
# further model assumption checks  
  # bptest(lm(native_C3 ~ hii + th + pewc + pcoldq + pwarmq + ts + arid + amt + proportion_cover, 
  #           data = spp_pv_sp, na.action = na.omit))
  # gvlma(lm(native_C3 ~ hii + th + pewc + pcoldq + pwarmq + ts + arid + amt + proportion_cover, 
  #          data = spp_pv_sp, na.action = na.omit))
  # 
  
# semi-variogram 
  plot(nlme:::Variogram(s_n3_lm, form = ~ long + lat, resType = "normalized")) # not much spatial autocrrelation
  plot(nlme:::Variogram(s_n4_lm, form = ~ long + lat, resType = "normalized")) # ... but a lot here

  plot(nlme:::Variogram(s_n3_gls, form = ~ long + lat, resType = "normalized")) # not much spatial autocrrelation
  plot(nlme:::Variogram(s_n4_gls, form = ~ long + lat, resType = "normalized")) # ... but a lot here
  
  plot(nlme:::Variogram(l_n3_lm, form = ~ long + lat, resType = "normalized")) # more spatial auto than lm
  plot(nlme:::Variogram(l_n4_lm, form = ~ long + lat, resType = "normalized")) # not much spatial autocorrelation (but a wee bit)
  plot(nlme:::Variogram(l_n3_gls, form = ~ long + lat, resType = "normalized")) # no sp/ac whatsoever
  plot(nlme:::Variogram(l_n4_gls, form = ~ long + lat, resType = "normalized")) # "                  "
  
# let's predict distributions, and see what the prediction looks like, then what the R2 is. Ok.!
  
# predicted distributions + R2 -------------------------------------------------
# make matrix and predict things into matrix
  pred_mat <- matrix(ncol = 8, 
                     nrow = nrow(pred_pv))
  
  pred_mat[,1] <- predict(s_n3_lm, newdata = pred_pv)
  pred_mat[,2] <- predict(s_n4_lm, newdata = pred_pv)
  
  pred_mat[,3] <- predict(s_n3_gls, newdata = pred_pv)
  pred_mat[,4] <- predict(s_n4_gls, newdata = pred_pv)
  
  pred_mat[,5] <- predict(l_n3_lm, newdata = pred_pv)
  pred_mat[,6] <- predict(l_n4_lm, newdata = pred_pv)
  
  pred_mat[,7] <- predict(l_n3_gls, newdata = pred_pv)
  pred_mat[,8] <- predict(l_n4_gls, newdata = pred_pv)

# check
  head(pred_mat[, 3])
  
# rasterise predicted distributions (pd) 
# turn 1003 cells to 2538 in correct order
  cell_id <- pred_pv$cell_id
  pd <- data.frame(cbind(cell_id, pred_mat))
  glimpse(pd)  
  
# generate list of occupied cells
  oc_list <- pd$cell_id
  
# make a matrix with all missing values
  x <- matrix(NA, nrow = length(getValues(aus)), ncol = 8)
  class(pred_mat) <- "numeric"
  
# add the occupied cells
  x[oc_list, ] <- pred_mat

# raster stack with 8 rasters  
  y <- addLayer(log_rich, aus, aus)
  z <- addLayer(log_rich, aus, aus)
# produce rasters 
# function requires: raster column = from data frame 'x'
#                    file name = useful name for file save 
  raster_fun <- function(raster_col){
    
    r1 <- setValues(aus, x[, raster_col])
    r2 <- calc(r1, fun = function(x) {x[x<0] <- 0; return(x)})
    
    y[[raster_col]] <<- r2
    
  }
  
  # colnames(x) <- c("native_C3_predicted", "native_C4_predicted", 
  #                  "native_total_predicted", "nonnative_C3_predicted", 
  #                  "nonnative_C4_predicted", "nonnative_total_predicted")
  
  for (i in 1:ncol(x)){
    raster_fun(i)
  }
  
# checks
  plot(y[[1]]) # lm: great
  plot(y[[2]]) # lm: also great
  
  plot(y[[3]]) # gls: all good
  plot(y[[4]]) # gls: not good at all
  
  plot(y[[5]]) # log lm: good good
  plot(y[[6]]) # log lm: good good
  
  plot(y[[7]]) # log gls: real good
  plot(y[[8]]) # log gls: real good
  
  
# r2  
  adj_r2 <- function(obs_raster, pred_raster){
  y <- getValues(obs_raster)
  yhat <- getValues(pred_raster)
  r_squared = 1 - sum((y - yhat)^2, na.rm = T) / sum((y - mean(y, na.rm = T))^2, na.rm = T)
  r_squared
  
# adjusted for being shifty
  n <- length(na.omit(y))
  p <- 9 # number of predictors
  adj_r_squared = 1 - (1 - r_squared) * ((n - 1)/(n - p - 1))
  return(round(adj_r_squared, 2))
  
  } # ajd_r2 end
  
  q <- matrix(nrow = 8)
  
# do it
  q[1] <- adj_r2(native_C3, y[[1]]) # lm
  q[2] <- adj_r2(native_C4, y[[2]])   
  
  q[3] <- adj_r2(native_C3, y[[3]]) # gls
  q[4] <- adj_r2(native_C4, y[[4]]) 
  
  q[5] <- adj_r2(log_rich[[1]], y[[5]]) # log lm
  q[6] <- adj_r2(log_rich[[2]], y[[6]]) 
  
  q[7] <- adj_r2(log_rich[[1]], y[[7]]) # log gls
  q[8] <- adj_r2(log_rich[[2]], y[[8]]) 

  q  
  
# ------------------------------------------------------------------------------  