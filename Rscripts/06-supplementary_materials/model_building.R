

# scope -------------------------------------------------------------------
# produce tables for:
# Moran's I
# gls model selection correlation structures

# library -----------------------------------------------------------------
  library(raster)
  library(tidyverse)
  library(broom)
  library(magrittr)
  
  
  rm(list = ls())

# data --------------------------------------------------------------------   
# model data
  load("Data files/rdata/models.RData")
  
# Moran's I ---------------------------------------------------------------  
  mi <- unlist(moran_l)
  mi2 <- mi %>%   