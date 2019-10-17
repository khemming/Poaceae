

# Date created: v1 3/8/18
# Updated: 13/10

# Based on v1-4 of the same name (or the original) & Rarefaction-EV script 
# Continuing from Rarefaction chapter data and EV variable selection scripts


# Aim ------------------------------------------------------------------------
# Model the total, C3 and C4 richness for 15-rarefied richness

# Library ------------------------------------------------------------------
  library(dplyr)
  library(broom)
  library(magrittr)
  library(raster)

  rm(list = ls())
  
  setwd("C:/Users/s436862/Dropbox/Rarefaction/Results/Rarefaction")
  
# Data ----------------------------------------------------------------   
# We have two species data frames: nat and int
# These have the two rarefied records, and all three pp pathways
  nat <- read.csv("CSV/Native multiple cutoff COMPLETE.csv", header = T) %>%
    select(total.rare.15, C3.rare.15, C4.rare.15)
  colnames(nat) <- c("n.tot", "n.c3", "n.c4")
  head(nat)
  
  int <- read.csv("CSV/Introduced multiple cutoff COMPLETE.csv", header = T) %>%
    select(total.rare.15, C3.rare.15, C4.rare.15)
  colnames(int) <- c("i.tot", "i.c3", "i.c4")
  
# Environmental variables 
# 'variable selection' evs
  vs.evs <- c("cell.cat", "arid",  "amt", "pwarmq", "pcoldq", 
              "th", "pewc",    "hii",     "ts")
  
  evs <- read.csv("C:/Users/s436862/Dropbox/Climate Matching/Data files/EFs/CSV/100-km scaled.csv", header = T) %>%
    select(vs.evs)

# Bind spp and climate data    
   spp.ev <- cbind(nat, int, evs) %>%
              filter(cell.cat == "terrestrial") %>%
              select(-cell.cat)

# 1.0 Generating models -------------------------------------------------
# Native 
# total
  m1 <- lm(n.tot ~ arid + amt + pwarmq + pcoldq + th + pewc + hii + ts, data = spp.ev)
  summary(m1) 
  m1.sum <- tidy(m1)
  m1.ci <- confint(m1)
# C3  
  m2 <- lm(n.c3 ~ arid + amt + pwarmq + pcoldq + th + pewc + hii + ts, data = spp.ev)
  summary(m2) 
  m2.sum <- tidy(m2)
  m2.ci <- confint(m2)
# C4
  m3 <- lm(n.c4 ~ arid + amt + pwarmq + pcoldq + th + pewc + hii + ts, data = spp.ev)
  summary(m3) 
  m3.sum <- tidy(m3)
  m3.ci <- confint(m3)
   
# Introduced 
# Total   
  m4 <- lm(i.tot ~ arid + amt + pwarmq + pcoldq + th + pewc + hii + ts, data = spp.ev)
  summary(m4) 
  m4.sum <- tidy(m4)
  m4.ci <- confint(m4)
# C3  
  m5 <- lm(i.c3 ~ arid + amt + pwarmq + pcoldq + th + pewc + hii + ts, data = spp.ev)
  summary(m5) 
  m5.sum <- tidy(m5)
  m5.ci <- confint(m5)
# C4  
  m6 <- lm(i.c4 ~ arid + amt + pwarmq + pcoldq + th + pewc + hii + ts, data = spp.ev)
  summary(m6) 
  m6.sum <- tidy(m6)
  m6.ci <- confint(m6)
  
# Making coefficient dataframe --------------------------------------------
# Status (n/i) | coefficient (evs) | mean | lower.ci | upper.ci
  spp.ev.mat <- matrix(ncol = 5, nrow = 16)  
  spp.ev.mat[,1] <- rep(c("native", "exotic"), each = 8)
  spp.ev.mat[,2] <- rep(vs.evs[2:9], 2)
  colnames(spp.ev.mat) <- c("status", "coef", "mean", "lower.ci", "upper.ci")

# One for each total, C3 and C4 richness (i.e. native and introduced will be plotted together) 
  total.rich <- spp.ev.mat
  c3.rich <- spp.ev.mat
  c4.rich <- spp.ev.mat
  
# insert coeffficents & CIs --------------------------------
# Total native  
  total.rich[1:8, 3] <- m1.sum$estimate[2:9]
  total.rich[1:8, 4:5] <- m1.ci[2:9]
# Total introduced  
  total.rich[9:16, 3] <- m4.sum$estimate[2:9]
  total.rich[9:16, 4:5] <- m4.ci[2:9]
  
# C3 native   
  c3.rich[1:8, 3] <- m2.sum$estimate[2:9]
  c3.rich[1:8, 4:5] <- m2.ci[2:9]
# C3 introduced  
  c3.rich[9:16, 3] <- m5.sum$estimate[2:9]
  c3.rich[9:16, 4:5] <- m5.ci[2:9]
  
# C4 native  
  c4.rich[1:8, 3] <- m3.sum$estimate[2:9]
  c4.rich[1:8, 4:5] <- m3.ci[2:9]
# C4 introduced  
  c4.rich[9:16, 3] <- m6.sum$estimate[2:9]
  c4.rich[9:16, 4:5] <- m6.ci[2:9]
 
# data frame   
  total.rich <- data.frame(total.rich)
  c3.rich <- data.frame(c3.rich)
  c4.rich <- data.frame(c4.rich)
  
# Mean and CIs as numbers for plotting  
  cols = c(3, 4, 5)
  total.rich[,cols] %<>% lapply(function(x) as.numeric(as.character(x)))
  c3.rich[,cols] %<>% lapply(function(x) as.numeric(as.character(x)))
  c4.rich[,cols] %<>% lapply(function(x) as.numeric(as.character(x)))
  
# Save workspace to use for plotting
  save.image("Rdata/rarefaction_model_coefficient_values.RData") 
   
 
# ----------------------------------------------------------------------------
  
# 2.0 Predicting distributions based on model estimates ----------------------
# Use the coefficient scores to predict richness for my groups using the Predict function 
  
# 2.1 Data -----------------------------------------------------------------------
# Environmental variables retained from variable selection
  vs.evs <- c("cell.cat", "arid",  "amt", "pwarmq", "pcoldq", 
              "th", "pewc",    "hii",     "ts")
  
# Complete (1003 cells) EV data 
  pred.ev <- read.csv("C:/Users/s436862/Dropbox/Climate Matching/Data files/EFs/CSV/100-km scaled.csv") %>%
          filter(cell.cat == "terrestrial") %>%
          select(vs.evs, -cell.cat, -cell.id) 
  
# 2.2 Predicting distributions ------------------------------------------------
# Native ----------------------------------------------------------------------
# Total richness
  nat.tot.pred <- predict(m1, newdata = pred.ev)
  
# C3 richness  
  nat.c3.pred <- predict(m2, newdata = pred.ev)
  
# C4 richness  
  nat.c4.pred <- predict(m3, newdata = pred.ev)
  
# Exotic ----------------------------------------------------------------------
# Total richness
  exo.tot.pred <- predict(m4, newdata = pred.ev)
  
# C3 richness  
  exo.c3.pred <- predict(m5, newdata = pred.ev)
  
# C4 richness  
  exo.c4.pred <- predict(m6, newdata = pred.ev) 
  
# Rasterise predicted values --------------------------------------------------
# Turn 1003 cells to 2538 in correct order ------------------------------------
# raster (100-km scale)
  aus <- raster("C:/Users/s436862/Dropbox/Climate Matching/Data files/Australia/aus_100km")
  
# cell ID
  cell.id <- read.csv("C:/Users/s436862/Dropbox/Climate Matching/Data files/EFs/CSV/Terrestrial land categories.csv") %>%
    filter(cell.cat == "terrestrial") %>%
    select(cell.id)
    
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
  
  
# 3.0 Correlation matrix of observed and predicted distributions --------------------------  
# Take rasters apart and combine to data frame --------------------------------------------
# Observed raster data
  setwd("C:/Users/s436862/Dropbox/Rarefaction/Results/Rarefaction/Rasters/Rarefied rasters")
  
  current.list <- list.files(pattern = ".grd")
  names <- gsub(pattern = "\\.grd$", "", current.list)
  
  c.stack <- stack(current.list)
  names(c.stack) <- names
  
  list2env(setNames(unstack(c.stack), names(c.stack)), .GlobalEnv)

# Predicted raster data
  setwd("C:/Users/s436862/Dropbox/Rarefaction/Results/Rarefaction/Rasters/Predicted distributions")
  
  current.list <- list.files(pattern = ".grd")
  names <- gsub(pattern = "\\.grd$", "", current.list)
  
  c.stack <- stack(current.list)
  names(c.stack) <- names
  
  list2env(setNames(unstack(c.stack), names(c.stack)), .GlobalEnv) 

# Data frame required rasters -----------------------------------------------------------
# o = observed, p = predicted; n = native, e = exotic; tot, c3 and c4
  o.n.tot <- getValues(r15.n.tot)
  o.n.c3 <- getValues(r15.n.c3)
  o.n.c4 <- getValues(r15.n.c4)
  
  o.e.tot <- getValues(r15.i.tot)
  o.e.c3 <- getValues(r15.i.c3)
  o.e.c4 <- getValues(r15.i.c4)
  
  p.n.tot <- getValues(n.tot.predicted)
  p.n.c3 <- getValues(n.c3.predicted)
  p.n.c4 <- getValues(n.c4.predicted)
  
  p.e.tot <- getValues(e.tot.predicted)
  p.e.c3 <- getValues(e.c3.predicted)
  p.e.c4 <- getValues(e.c4.predicted)
  
  spp.df <- cbind(o.n.tot, o.n.c3, o.n.c4, 
                  o.e.tot, o.e.c3, o.e.c4,
                  p.n.tot, p.n.c3, p.n.c4,
                  p.e.tot, p.e.c3, p.e.c4) 
 spp.df <- data.frame(spp.df) 

# Correlation matrix ----------------------------------------------  
 p.o.cor <- cor(spp.df, use = "complete.obs")
 write.csv(p.o.cor, "C:/Users/s436862/Dropbox/Rarefaction/Results/Rarefaction/CSV/Observed-predicted distribution correlation matrix.csv", row.names = T)  
 