

# library ------------------------------------------------------------------
  library(tidyverse)
  library(broom)
  library(magrittr)
  library(MuMIn)
  library(ape)
  library(nlme)  
  library(car)
  library(rgdal)
  library(lmtest)
  library(gvlma)
  
  rm(list = ls())
 
# data ---------------------------------------------------------------------   
# raster names
  setwd("C:/Users/s436862/Dropbox/Poaceae/Results/rasters/scaled")
  current.list <- list.files(pattern = ".grd")
  spp <- gsub(pattern = "\\.grd$", "", current.list)
  setwd("C:/Users/s436862/Dropbox/Poaceae")
  
# species richness and predictor varaibles
  spp_pv <- read.csv("Results/csv/spp predictor variables 1102.csv")
  
# identify spatial auto-correlation --------------------------------------------------------------
# store all results for supplementary materials
  moran_l <- list()
  gls_l <- list()
  model_list <- list()
  ci_list <- list()
  cor_str <- matrix(nrow = 6)
  
# identify spatial autocorrelation function (returns p-value)
  moran_fun <- function(spp_col, col_no) {
    xy <- spp_pv %>% filter(!is.na(spp_col)) %>%
      dplyr::select(all_of(col_no), long, lat)
    coords = cbind(xy$long, xy$lat)
    w = fields:::rdist(coords)
    m_i <- Moran.I(x = xy[, 1], w = w, na.rm = T)
  return(m_i)
  
  }

# run 
  for (i in 1:length(spp)){
    
    moran_l[[i]] <- moran_fun(spp_pv[, i], i)
    
  }
  
  moran_l[[1]] # all spp have spatial-autocorrelation
  names(moran_l) <- spp
  # unlist if need
  
# Moran's I data frame for saving: 4 x 6
  m_mat <- round(matrix(unlist(moran_l), byrow = T, nrow=length(spp)), 4)
  row.names(m_mat) <- spp
  colnames(m_mat) <- c("observed","expected", "sd", "p.value")
  m_mat

# model selection --------------------------------------------------------------------------------
# test different methods for modelling spatial autocorrelation
  model_sel_fun <- function(spp_col) {
  # model methods to account for spatial autocorrelation
    model_e <- gls(spp_col ~ hii + th + pewc + pcoldq + pwarmq + ts + arid + amt + proportion_cover, 
                   data = spp_pv, correlation = corExp(form = ~long + lat, nugget=T) , na.action = na.omit, method = "ML")
    model_g <- gls(spp_col ~ hii + th + pewc + pcoldq + pwarmq + ts + arid + amt + proportion_cover, 
                   data = spp_pv, correlation = corGaus(form = ~long + lat, nugget=T), na.action = na.omit, method = "ML")
    model_s <- gls(spp_col ~ hii + th + pewc + pcoldq + pwarmq + ts + arid + amt + proportion_cover, 
                   data = spp_pv, correlation = corSpher(form = ~long + lat, nugget=T), na.action = na.omit, method = "ML")
    model_r <- gls(spp_col ~ hii + th + pewc + pcoldq + pwarmq + ts + arid + amt + proportion_cover,
                   data = spp_pv, correlation = corRatio(form = ~long + lat, nugget=T), na.action = na.omit, method = "ML")
    model_lm <- lm(spp_col ~ hii + th + pewc + pcoldq + pwarmq + ts + arid + amt + proportion_cover,
                   data = spp_pv, na.action = na.omit)
  # compare models using AICc
    model_sel <- model.sel(model_e, model_g , model_s, model_r, model_lm)
    return(model_sel)
  }
  
# run and choose best fit by AICc
  for (i in 1:length(spp)){
  spp_col <- spp_pv[, i]
  gls_l[[i]] <- model_sel_fun(spp_col)
  cor_str[i] <- gls_l[[i]]$correlation[1] # best correlation structure
  }
  
  cor_str
  
  
# test for C4 spp
  spp_col <- spp_pv$native_C4
  native_c4_models <- model_sel_fun(spp_col) # works fine
  native_c4_models
  gls_l[[2]]
  
# save all gls model data: 19 x 5 (*6)
  gls_mat <- matrix(nrow = 5, ncol = 114)
  
  gls_mat[,] <- unlist(gls_l, recursive = T)
  gls_mat2 <- gls_mat
  
  
  colnames(gls_mat2) <- colnames(gls_l[[1]])
  rownames(gls_mat2) <- rep(spp, each = 5)

# run lowest-AIC models ------------------------------------------------------------------
# native C3 =  corSpher(form = ~long + lat, nugget=T)
  model_list[[1]] <- gls(native_C3 ~ hii + th + pewc + pcoldq + pwarmq + ts + arid + amt + proportion_cover, 
                        data = spp_pv, correlation = corSpher(form = ~long + lat, nugget=T), na.action = na.omit, method = "ML")
  ci_list[[1]] <- data.frame(intervals(model_list[[1]], 0.95, which = "coef")$coef)
  
# native C4 =  corExp(form = ~long + lat, nugget=T)
  model_list[[2]] <- gls(native_C4 ~ hii + th + pewc + pcoldq + pwarmq + ts + arid + amt + proportion_cover, 
                         data = spp_pv, correlation = corExp(form = ~long+lat,T), na.action = na.omit, method = "ML")
  ci_list[[2]] <- data.frame(intervals(model_list[[2]], 0.95, which = "coef")$coef)

# native total =  corExp(form = ~long + lat, nugget=T)
  model_list[[3]] <- gls(native_total ~ hii + th + pewc + pcoldq + pwarmq + ts + arid + amt + proportion_cover, 
                         data = spp_pv, correlation = corExp(form = ~long + lat, nugget=T), na.action = na.omit, method = "ML")
  ci_list[[3]] <- data.frame(intervals(model_list[[3]], 0.95, which = "coef")$coef)
  
# nonnative C3 =  corExp(form = ~long + lat, nugget=T)
  model_list[[4]] <- gls(nonnative_C3 ~ hii + th + pewc + pcoldq + pwarmq + ts + arid + amt + proportion_cover, 
                         data = spp_pv, correlation = corExp(form = ~long + lat, nugget=T), na.action = na.omit, method = "ML")
  ci_list[[4]] <- data.frame(intervals(model_list[[4]], 0.95, which = "coef")$coef)
  
# nonnative C4 =  corExp(form = ~long + lat, nugget=T)
  model_list[[5]] <- gls(nonnative_C4 ~ hii + th + pewc + pcoldq + pwarmq + ts + arid + 
                         amt + proportion_cover, data = spp_pv, 
  correlation = corExp(form = ~long + lat, nugget=T), na.action = na.omit, method = "ML")
  ci_list[[5]] <- data.frame(intervals(model_list[[5]], 0.95, which = "coef")$coef)
  
# nonnative total = corExp(form = ~long + lat, nugget=T)
  model_list[[6]] <- gls(nonnative_total ~ hii + th + pewc + pcoldq + pwarmq + ts + arid + amt + proportion_cover, data = spp_pv, 
     correlation = corExp(form = ~long + lat, nugget=T), na.action = na.omit, method = "ML")
  
  ci_list[[6]] <- data.frame(intervals(model_list[[6]], 0.95, which = "coef")$coef)
  
  names(model_list) <- spp
  names(ci_list) <- spp

# save data ------------------------------------------------------------------
  write.csv(m_mat, "Results/csv/Morans I.csv", row.names = T)
  write.csv(gls_l7, "Results/csv/GLS model structures.csv", row.names = T)
  
  save.image("data files/rdata/models.RData")
# ----------------------------------------------------------------------------
