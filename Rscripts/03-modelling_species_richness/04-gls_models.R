

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
  current.list <- list.files(path = "Results/rasters/scaled",
                             pattern = ".grd", full.names = T)
  spp <- gsub(pattern = "Results/rasters/scaled/|.grd$", "", current.list)
 
# species richness and predictor varaibles
  spp_pv <- read.csv("Results/csv/spp predictor variables 1104.csv")
  
# identify spatial auto-correlation --------------------------------------------------------------
# store all results for supplementary materials
  moran_ls <- list()
  gls_ls <- list()
  model_ls <- list()
  ci_ls <- list()
  cor_str <- matrix(nrow = length(spp))
  
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
    
    moran_ls[[i]] <- moran_fun(spp_pv[, i], i)
    
  }
  
# check  
   names(moran_ls) <- spp
   moran_ls[[1]] # all spp have spatial-autocorrelation
 
  
# Moran's I data frame for saving: 4 x 6
  m_mat <- round(matrix(unlist(moran_ls), byrow = T, nrow = length(spp)), 4)
  row.names(m_mat) <- spp
  colnames(m_mat) <- c("observed","expected", "sd", "p.value")
  m_mat

# model selection --------------------------------------------------------------------------------
# model terms
  m_formula <- formula(spp_col ~ hii + th + pewc + pcoldq + pwarmq + ts + arid + amt + proportion_cover)
  
# test different methods for modelling spatial autocorrelation
  model_sel_fun <- function(spp_col) {
    model_e <- gls(m_formula, data = spp_pv, correlation = corExp(form = ~long + lat, nugget=T) , na.action = na.omit, method = "ML")
    model_g <- gls(m_formula, data = spp_pv, correlation = corGaus(form = ~long + lat, nugget=T), na.action = na.omit, method = "ML")
    model_s <- gls(m_formula, data = spp_pv, correlation = corSpher(form = ~long + lat, nugget=T), na.action = na.omit, method = "ML")
    model_r <- gls(m_formula, data = spp_pv, correlation = corRatio(form = ~long + lat, nugget=T), na.action = na.omit, method = "ML")
    model_lm <- lm(m_formula,
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
  
# save all gls model data
  gls_mat <- matrix(nrow = 5, ncol = 114) # ncol = 19 * no. of spp
  
  gls_mat[,] <- unlist(gls_l, recursive = T)
  gls_mat2 <- rbind(gls_mat[,1:19],  gls_mat[,20:38],  gls_mat[,39:57],
                    gls_mat[,58:76], gls_mat[,77:95],  gls_mat[,96:114])
  
  
  colnames(gls_mat2) <- colnames(gls_l[[1]])
  rownames(gls_mat2) <- rep(spp, each = 5)

# run lowest-AIC models  ---------------------------------
## dedicated model functions  
  model_e <- function(spp_col){gls(m_formula, 
                                  data = spp_pv, correlation = corExp(form = ~long + lat, nugget=T) , 
                                  na.action = na.omit, method = "ML")}
  model_g <- function(spp_col){gls(m_formula, 
                                  data = spp_pv, correlation = corGaus(form = ~long + lat, nugget=T), 
                                  na.action = na.omit, method = "ML")}
  model_s <- function(spp_col){gls(m_formula, 
                                  data = spp_pv, correlation = corSpher(form = ~long + lat, nugget=T), 
                                  na.action = na.omit, method = "ML")}
  model_r <- function(spp_col){gls(m_formula, 
                                  data = spp_pv, correlation = corRatio(form = ~long + lat, nugget=T), 
                                  na.action = na.omit, method = "ML")}
  
# re-calculate confidence intervals
  model_est <- function(model){
    data.frame(intervals(model, 0.95, which = "coef")$coef)}
  
  
# native C3
## model_g
  spp_col <- spp_pv$native_C3
  model_ls[[1]] <- model_g(spp_col)
  ci_ls[[1]] <- model_est(model_ls[[1]])
  
# native C4
## model_e
  spp_col <- spp_pv$native_C4
  model_ls[[2]] <- model_e(spp_col)
  ci_ls[[2]] <- model_est(model_ls[[2]])

# native total
## model_e
  spp_col <- spp_pv$native_total
  model_ls[[3]] <- model_e(spp_col)
  ci_ls[[3]] <- model_est(model_ls[[3]])
  
# nonnative C3
## model_e
  spp_col <- spp_pv$nonnative_C3
  model_ls[[4]] <- model_e(spp_col)
  ci_ls[[4]] <- model_est(model_ls[[4]])
  
# nonnative C4
## model_e
  spp_col <- spp_pv$nonnative_C4
  model_ls[[5]] <- model_e(spp_col)
  ci_ls[[5]] <- model_est(model_ls[[5]])
  
# nonnative total
## model_s
  spp_col <- spp_pv$nonnative_total
  model_ls[[6]] <- model_s(spp_col)
  ci_ls[[6]] <- model_est(model_ls[[6]])
  
  names(model_ls) <- spp
  names(ci_ls) <- spp

# save data ------------------------------------------------------------------
  write.csv(m_mat, "Results/csv/Morans I.csv", row.names = T)
  write.csv(gls_mat2, "Results/csv/GLS model structures.csv", row.names = T)
  
  save.image("Data files/rdata/models.RData")
# ----------------------------------------------------------------------------
