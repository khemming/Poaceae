

# Date created: v1 3/8/18
# Updated: 10/8

# Based on v1 of the same name (or no version, jsut the original) & Rarefaction-EF script 
# Continuing from Rarefaction chapter data and EF variable selection scripts


# Aim ------------------------------------------------------------------------
# Producing the models that will express how the environmental data predict species richness
# I will produce a few versions of each final output
# For better code lines and notes, refer to previous version

# Library ------------------------------------------------------------------
  library(RColorBrewer)
  library(raster)
  library(ggmap)
  library(tidyr)
  library(dplyr)
  library(rasterVis)
  library(maptools)
  library(ggthemes)
  library(ggcorrplot)
  library(corrplot)
  library(factoextra)
  library(car)
  library(glmnet)
  library(ncvreg)

  rm(list = ls())
  
  setwd("C:/Users/s436862/Dropbox/Rarefaction/4. Results/Rarefaction")
  
# Data ---------------------------------------------------------------------
# Set-up -------------------------------------------------------------------
# Codes for output data
# Rarefaction (n.rec): 50- or 15-records = 15 / 50  
  n.rec <- c("15", "50")
# Status (status): native or introduced = n / i  
  status <- c("nat", "int")
# Photosynthetic pathway (pp): total, C3 and C4 richness = tot / c3 / c4  
  pp <- c("tot", "c3", "c4")
# Model type: Ridge regression / LASSOO: 0 / 1
  m <- c("m1", "m0") # 1 = lasso, 0 = ridge
# Lambda score: min or 1se = m / s  
  l <- c("m", "s")
# EF coefficients = intercept, arid, etc. (= 9 cols)  
  coef <- "coef"
# R2, single value = r2  
  r2 <- "r2"

# Inputs 
  n.rec <- rep(c("15", "50"), each = 24)
  status <- rep(rep(c("n", "i"), each = 12),2)
  pp <- rep(rep(c("tot", "c3", "c4"), each = 4),4)
  model <- rep(rep(c("m0", "m1"), each = 2), 12)
  lambda <- rep(c("mn", "se"), 24)
  
# Outputs  
  coef <- matrix(nrow = 48, ncol = 9)
  coef.names <- c("intercept", "arid",  "mat", "pwarmq", "pcoldq", 
                         "th", "pewc",  "hii",     "ps")
  colnames(coef) <- coef.names
  r2 <- matrix(ncol = 1, nrow = 48)
  colnames(r2) <- "r2"
# Matrix  
  dat <- cbind(n.rec, status, pp, model, lambda, coef, r2)
 
# Data ----------------------------------------------------------------   
# We have two species data frames: nat and int
# These have teh two rarefied records, and all three pp pathways
  nat <- read.csv("CSV/Native multiple cutoff COMPLETE.csv", header = T) %>%
          select(total.rare.15, C3.rare.15, C4.rare.15,
                 total.rare.50, C3.rare.50, C4.rare.50)
  colnames(nat) <- c("r15.n.tot", "r15.n.c3", "r15.n.c4",
                     "r50.n.tot", "r50.n.c3", "r50.n.c4")
  head(nat)
  
  int <- read.csv("CSV/Introduced multiple cutoff COMPLETE.csv", header = T) %>%
          select(total.rare.15, C3.rare.15, C4.rare.15,
                 total.rare.50, C3.rare.50, C4.rare.50)
  colnames(int) <- c("r15.i.tot", "r15.i.c3", "r15.i.c4",
                     "r50.i.tot", "r50.i.c3", "r50.i.c4")

# Environmental variables 
# variable selection efs
  vs.efs <- c("arid",  "mat", "pwarmq", "pcoldq", 
                "th", "pewc",    "hii",     "ps")
  
  efs <- read.csv("C:/Users/s436862/Dropbox/Climate Matching/1. Data files/EFs/4. EFs complete/EF_COMPLETE.csv", header = T) %>%
      select(vs.efs)   

# Australian cell of species and efs 
  land.cat <- read.csv("C:/Users/s436862/Dropbox/Climate Matching/1. Data files/EFs/4. EFs complete/Terrestrial categories.csv", header = T)
  
# Model data set-up -------------------------------------------------------------
# Function to remove NAs -------------------------------------------------------
# We need to remove Nas. These will pop up individually for each spp-ef pair, so we need to make 12 dfs to deal with this
  
# Requires: spp = specific spp column
# land.cat = subset of spp and ef col that refers to Aus-cells
# efs = envuronmental variables in same format
  
# function
  fun.rm.na <- function(spp, land.cat, efs) {
    
    spp.na <- cbind(land.cat, spp, efs) %>% 
      filter(cell_category == "terrestrial") %>%
      select(-cell_category, -cell_id)
    
    spp <- spp.na[complete.cases(spp.na), ]
    
    return(spp)
  } # fun end
  
  sum(is.na(spp)) # check that it worked
  
# Native ------------------
# r15-------
# n.tot
  r15.n.tot <- nat$r15.n.tot
  r15.n.tot <- fun.rm.na(r15.n.tot, land.cat, efs)
  
  sum(is.na(r15.n.tot)) # check no NAs
  sum(r15.n.tot$spp)    # check this with C3 & C4 that it makes sense
  
# n.c3 
  r15.n.c3 <- nat$r15.n.c3
  r15.n.c3 <- fun.rm.na(r15.n.c3, land.cat, efs)
  
  sum(is.na(r15.n.c3)) # check no NAs
  sum(r15.n.c3$spp)
  
# n.c4  
  r15.n.c4 <- nat$r15.n.c4
  r15.n.c4 <- fun.rm.na(r15.n.c4, land.cat, efs)
  
  sum(is.na(r15.n.c4)) # check no NAs
  sum(r15.n.c4$spp)

# r50 ------  
# n.tot
  r50.n.tot <- nat$r50.n.tot
  r50.n.tot <- fun.rm.na(r50.n.tot, land.cat, efs)
  
  sum(is.na(r50.n.tot)) # check no NAs
  sum(r50.n.tot$spp)    # check this with C3 & C4 that it makes sense
  
# n.c3  
  r50.n.c3 <- nat$r50.n.c3
  r50.n.c3 <- fun.rm.na(r50.n.c3, land.cat, efs)
  
  sum(is.na(r50.n.c3)) # check no NAs
  sum(r50.n.c3$spp) 
 
# n.c4  
  r50.n.c4 <- nat$r50.n.c4
  r50.n.c4 <- fun.rm.na(r50.n.c4, land.cat, efs)
  
  sum(is.na(r50.n.c4)) # check no NAs
  sum(r50.n.c4$spp)
  
# ----------

# Introduced --------------
# r15-------
# n.tot
  r15.i.tot <- int$r15.i.tot
  r15.i.tot <- fun.rm.na(r15.i.tot, land.cat, efs)
  
  sum(is.na(r15.i.tot)) # check no NAs
  sum(r15.i.tot$spp)    # check this with C3 & C4 that it makes sense
  
# n.c3 
  r15.i.c3 <- int$r15.i.c3
  r15.i.c3 <- fun.rm.na(r15.i.c3, land.cat, efs)
  
  sum(is.na(r15.i.c3)) # check no NAs
  sum(r15.i.c3$spp)
  
# n.c4  
  r15.i.c4 <- int$r15.i.c4
  r15.i.c4 <- fun.rm.na(r15.i.c4, land.cat, efs)
  
  sum(is.na(r15.i.c4)) # check no NAs
  sum(r15.i.c4$spp)
  
# r50 ------  
# n.tot
  r50.i.tot <- int$r50.i.tot
  r50.i.tot <- fun.rm.na(r50.i.tot, land.cat, efs)
  
  sum(is.na(r50.i.tot)) # check no NAs
  sum(r50.i.tot$spp)    # check this with C3 & C4 that it makes sense
  
# n.c3  
  r50.i.c3 <- int$r50.i.c3
  r50.i.c3 <- fun.rm.na(r50.i.c3, land.cat, efs)
  
  sum(is.na(r50.i.c3)) # check no NAs
  sum(r50.i.c3$spp) 
  
# n.c4  
  r50.i.c4 <- int$r50.i.c4
  r50.i.c4 <- fun.rm.na(r50.i.c4, land.cat, efs)
  
  sum(is.na(r50.i.c4)) # check no NAs
  sum(r50.i.c4$spp)
# --------------  
 
# --------------   
# If NA = TRUE -----------------------------------------------------------------------------
# There is one NA, specifically, row 933, column 9 (PEWC) 
# Short-term solution: replace with mean
# Long-term solution: remove row (and spp. row too) OR hope PEWC is useless, and therefore we can leave it out and pretend this didn't happen 
# OR ask Richard...?

# NA --> mean(pewc)  
# efs[is.na(efs)] <- mean(efs[,7], na.rm= T)
# table(is.na(efs) == T) # score
# --------------

# VIF scores DNC --------------------------------------------------
  r15.n.tot.ef.df <- as.data.frame(scale(r15.n.tot.ef, center = T, scale = T)) 
  spp.vif <- vif(lm(r15.n.tot.spp ~ ., data = r15.n.tot.ef.df))
  spp.vif

  
# -------------------------------------------------------------------------  
  
# Model visualisation -----------------------------------------------------
# Coefficient variation function ------------------------------------------  
# How the coefficients vary as we incrase model complexity. These plots will help us visualise this
  setwd("C:/Users/s436862/Dropbox/Rarefaction/4. Results/Rarefaction/Graphs/Model outputs/Coefficient visualisation")
  
# Function requirements:
# Spp = spp-ef matrix, no NAs (see glmnet for specifics); spp column needs to be called 'spp', with the (in this case) 8 efs following suit
# Save0 and 1 = for the graphical outputs; exact location
# Title0 and 1 = plot title as the specific code (e.g. r50.n.tot.m0/m1) 
  
  fun.coef.vis <- function(spp, save0, save1, title0, title1) {
    
  # Environmental variables subsetted, transformed  
    spp.ef <- as.matrix(scale(spp[,2:9], center = T, scale = T))
   
  # Ridge 
    spp.m0 <- glmnet(spp.ef, spp$spp, family = "gaussian", alpha = 0)
    jpeg(save0, width = 14, height = 10, units = "cm", res = 500)
    plot(spp.m0, main = title0, xvar = "lambda")
    dev.off()
    
  # LASSO  
    spp.m1 <- glmnet(spp.ef, spp$spp, family = "gaussian", alpha = 1)
    jpeg(save1, width = 14, height = 10, units = "cm", res = 500)
    plot(spp.m1, main = title1, xvar = "lambda")
    dev.off()
   
  } # fun end
  
# Nat --------
# r15 ----
# n.tot  
  spp <- r15.n.tot
  save0 <-  "r15.n.tot.m0.jpeg"
  title0 <- "r15.n.tot.m0" 
  save1 <-  "r15.n.tot.m1.jpeg"
  title1 <- "r15.n.tot.m1"
  
  fun.coef.vis(r15.n.tot, save0, save1, title0, title1)

# n.c3    
  spp <- r15.n.c3
  save0 <-  "r15.n.c3.m0.jpeg"
  title0 <- "r15.n.c3.m0" 
  save1 <-  "r15.n.c3.m1.jpeg"
  title1 <- "r15.n.c3.m1"
  
  fun.coef.vis(r15.n.c3, save0, save1, title0, title1)
  
# n.c4    
  spp <- r15.n.c4
  save0 <-  "r15.n.c4.m0.jpeg"
  title0 <- "r15.n.c4.m0" 
  save1 <-  "r15.n.c4.m1.jpeg"
  title1 <- "r15.n.c4.m1"
  
  fun.coef.vis(r15.n.c4, save0, save1, title0, title1)
  
# r50 ----
# n.tot  
  spp <- r50.n.tot
  save0 <-  "r50.n.tot.m0.jpeg"
  title0 <- "r50.n.tot.m0" 
  save1 <-  "r50.n.tot.m1.jpeg"
  title1 <- "r50.n.tot.m1"
  
  fun.coef.vis(r50.n.tot, save0, save1, title0, title1)
  
# n.c3    
  spp <- r50.n.c3
  save0 <-  "r50.n.c3.m0.jpeg"
  title0 <- "r50.n.c3.m0" 
  save1 <-  "r50.n.c3.m1.jpeg"
  title1 <- "r50.n.c3.m1"
  
  fun.coef.vis(r50.n.c3, save0, save1, title0, title1)
  
# n.c4    
  spp <- r50.n.c4
  save0 <-  "r50.n.c4.m0.jpeg"
  title0 <- "r50.n.c4.m0" 
  save1 <-  "r50.n.c4.m1.jpeg"
  title1 <- "r50.n.c4.m1"
  
  fun.coef.vis(r50.n.c4, save0, save1, title0, title1)
  
# ------------ 

# Int --------
# r15 ----
# i.tot  
  spp <- r15.i.tot
  save0 <-  "r15.i.tot.m0.jpeg"
  title0 <- "r15.i.tot.m0" 
  save1 <-  "r15.i.tot.m1.jpeg"
  title1 <- "r15.i.tot.m1"
  
  fun.coef.vis(r15.i.tot, save0, save1, title0, title1)
  
# i.c3    
  spp <- r15.i.c3
  save0 <-  "r15.i.c3.m0.jpeg"
  title0 <- "r15.i.c3.m0" 
  save1 <-  "r15.i.c3.m1.jpeg"
  title1 <- "r15.i.c3.m1"
  
  fun.coef.vis(r15.i.c3, save0, save1, title0, title1)
  
# i.c4    
  spp <- r15.i.c4
  save0 <-  "r15.i.c4.m0.jpeg"
  title0 <- "r15.i.c4.m0" 
  save1 <-  "r15.i.c4.m1.jpeg"
  title1 <- "r15.i.c4.m1"
  
  fun.coef.vis(r15.i.c4, save0, save1, title0, title1)
  
# r50 ----
# i.tot  
  spp <- r50.i.tot
  save0 <-  "r50.i.tot.m0.jpeg"
  title0 <- "r50.i.tot.m0" 
  save1 <-  "r50.i.tot.m1.jpeg"
  title1 <- "r50.i.tot.m1"
  
  fun.coef.vis(r50.i.tot, save0, save1, title0, title1)
  
# i.c3    
  spp <- r50.i.c3
  save0 <-  "r50.i.c3.m0.jpeg"
  title0 <- "r50.i.c3.m0" 
  save1 <-  "r50.i.c3.m1.jpeg"
  title1 <- "r50.i.c3.m1"
  
  fun.coef.vis(r50.i.c3, save0, save1, title0, title1)
  
# i.c4    
  spp <- r50.i.c4
  save0 <-  "r50.i.c4.m0.jpeg"
  title0 <- "r50.i.c4.m0" 
  save1 <-  "r50.i.c4.m1.jpeg"
  title1 <- "r50.i.c4.m1"
  
  fun.coef.vis(r50.i.c4, save0, save1, title0, title1)
  
# ------------   
  
# Model selection ---------------------------------------------------------
# Here we get all our results
# This should be:
# (1) Lambda plot showing where minimum %dev explained and +1se lambda values fall
# (2) Lambda min and 1se coefficients
# (3) Lambda min and 1se R2 values
  setwd("C:/Users/s436862/Dropbox/Rarefaction/4. Results/Rarefaction/Graphs/Model outputs/Coefficient visualisation")
  
# Model requires:
# Spp = spp-ef matrix, no NAs (see glmnet for specifics); spp column needs to be called 'spp', with the (in this case) 8 efs following suit
# Save = m for model and p for plot; 0 and 1 for ridge/LASSO
# Title0 and 1 = plot title as the specific code (e.g. r50.n.tot.m0/m1) 
# i = row # for inserting coefs and R2 into dat
  
  fun.mod.coef.r2 <- function(spp, save.m0, save.m1, save.p0, save.p1, title0, title1, i) {
  
  # Environmental variables subsetted, transformed 
    r50.i.tot.ef <- as.matrix(scale(r50.i.tot[,2:9], center = T, scale = T))
    
  # Ridge ----
    r50.i.tot.m0.cv <- cv.glmnet(r50.i.tot.ef, r50.i.tot$spp, family = "gaussian", alpha = 0)
  # model save
    save(r50.i.tot.m0.cv, file = save.m0)
  # Plot  
    jpeg(save.p0, width = 14, height = 10, units = "cm", res = 500)
    plot(r50.i.tot.m0.cv, main = title0, xvar = "lambda")
    plot(spp.m0, main = title0, xvar = "lambda")
    dev.off() 
  # Coefficients
    r50.i.tot.m0.cv.coef <- t(coef(r50.i.tot.m0.cv, s = "lambda.min"))
    r50.i.tot.m0.cv.coef.vec <- as.vector(r50.i.tot.m0.cv.coef)
    
    dat[i, 6:14] <- r50.i.tot.m0.cv.coef.vec
  # R2
    #mod.a1 <- glmnet(ef.x, spp, family = "gaussian", alpha = 1) # Note: glmnet (not .cv)
    #cvfit <- cv.glmnet(ef.x, spp) # Ok, now we cv it
    #rsq <- 1 - cvfit$cvm/var(spp) # and then we do some stuff?
    #plot(cvfit)  #? is this the lambda min and 1se plot? # Yes
    #cvfit <- cv.ncvreg(ef.x, spp, penalty="lasso", returnY = T) # Are you my r2 hero?
    #plot(cvfit, type="rsq") # What are you
    
    r50.i.tot.m0 <- glmnet(r50.i.tot.ef, r50.i.tot$spp, family = "gaussian", alpha = 0)
    r50.i.tot.m0.cv <- cv.glmnet(r50.i.tot.ef, r50.i.tot$spp, family = "gaussian", alpha = 0)
    r50.i.tot.m0.rsq <- 1 - r50.i.tot.m0.cv$cvm/var(r50.i.tot$spp)
    r50.i.tot.m0.cvfit <- cv.ncvreg(r50.i.tot.ef, r50.i.tot$spp, penalty= "lasso", returnY = T)
    
    r50.i.tot.m0.cvfit.coef <- t(coef(r50.i.tot.m0.cvfit, s = "lambda.min"))
    
    plot(r50.i.tot.m0.cvfit, type="rsq", sub = "r50.i.tot.m0")
    plot(r50.i.tot.m0.cv)
    
    plot(r50.i.tot.m0, xvar = "lambda")
    
    r50.i.tot.m0.cvfit$lambda.min
    .r50.i.tot.m0.cvfit$ty
      
  } # fun end

  
#How does it compare with the model that corresponds to 'lambda.1se'?
  coef(mod.a1.cv, s = "lambda.1se")
# interesting - they're essentially the same
 
# Calculate R2 ------------------------------------------------------------------
# my data  
  mod.a1 <- glmnet(ef.x, spp, family = "gaussian", alpha = 1)
  
  cvfit <- cv.glmnet(ef.x, spp)
  rsq <- 1 - cvfit$cvm/var(spp)
  
  plot(cvfit)
  
  cvfit <- cv.ncvreg(ef.x, spp, penalty="lasso", returnY = T)

  plot(cvfit, type="rsq")

  
  # We can view the selected lambdas like this:
  r50.i.tot.cv$lambda.min  
  r50.i.tot.cv$lambda.1se    
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
    
  
  
  
  