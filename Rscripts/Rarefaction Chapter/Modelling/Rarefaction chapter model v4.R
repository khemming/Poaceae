

# Date created: v1 3/8/18
# Updated: 10/8

# Based on v1-3 of the same name (or the original) & Rarefaction-EF script 
# Continuing from Rarefaction chapter data and EF variable selection scripts


# Aim ------------------------------------------------------------------------
# Choosing the best model gicen multiple species (record cutoff value, richnesses) and modelling (LASSO, ridge, lambdas) tools
# For more notes refer to previous versions

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
  
# Output set-up ------------------------------------------------------------
# Codes for inputs ('rows'):
# Rarefaction: 50- or 15-records = 15 / 50  
  n.rec <- rep(c("15", "50"), each = 12)
# Status: native or introduced = n / i   
  status <- rep(rep(c("n", "i"), each = 6),2)
# Photosynthetic pathway (pp): total, C3 and C4 richness = tot / c3 / c4    
  pp <- rep(rep(c("tot", "c3", "c4"), each = 2),4)
# Model type: Ridge regression / LASSO: 0 / 1
  model <- rep(c("m0", "m1"), 12)
  
# Codes for outputs ('cols'):  
# So what I want to figure out is which lambda value corresponds to the highest R2. 
# Firstly, is it between or outside the min and 1se lambda values?
# And then, what is the max R2, and what are the coefficents for it. 
# If it falls out of the range of lamb.min and .se, then which ever of .min or .se are closer, I'll go with that one. Kinda? I'll look at this again later. For now: 
# Lambda value for mn and se  
# Lambda value for highest r2    
# The absolutely highest R2
  lambdas <- matrix(nrow = 24, ncol = 3)
  lambda.names <- c("lamb.min", "lamb.se", "lamb.mx.r2")
  colnames(lambdas) <- lambda.names
  
# And what are the EF coefficients for highest r2 = intercept, arid, etc. (= 9 cols)  
  mx.r2.coef <- matrix(nrow = 24, ncol = 9)
  coef.names <- c("intercept", "arid",  "mat", "pwarmq", "pcoldq", 
                  "th", "pewc",  "hii",     "ps")
  colnames(mx.r2.coef) <- coef.names
  
# Matrix  
  dat <- cbind(n.rec, status, pp, model, lambdas, mx.r2.coef)
  
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
# VIF scores (Did not complete) --------------------------------------------------
  # r15.n.tot.ef.df <- as.data.frame(scale(r15.n.tot.ef, center = T, scale = T)) 
  # spp.vif <- vif(lm(r15.n.tot.spp ~ ., data = r15.n.tot.ef.df))
  # spp.vif

  
# -------------------------------------------------------------------------  
  
# Modelling  --------------------------------------------------------------
# Outputs -----------------------------------------------------------------
# Coefficient-variation plot = how the coefficients values vary as we increase model complexity via reducing lambda
# Cross-validation plot = model fit via MSE from teh same lambda decrease + what the minimum error and +1se away from that are 
# The model associated with this (including all coefficients and lambdas)
# Finally, a similarly reproduced plot of model complexity as it pertains to R2 value, and the model associated with this
  
# Ridge regression -------------------------------------------------------
# Function + requirements ------------------------------------------------
  fun.ridge <- function(spp,              # spp data frame (with EFs, no NAs, in matrix form - see above)
                        coef.vis,         # save coef vis plot
                        title,            # model title - specific to ridge/lasso
                        cross.val.plot,   # save cross-validation of lambda-MSE plot
                        cross.val.mod,    # save cross-val model
                        cv.r2.plot,       # save cross-validation of lambda-MSE plot
                        cv.r2.mod,        # save cross-val model
                        i)                # dat$model row number - thus order of iteration needs to be the order of dat
                        
  { # function start
    
  # Environmental variables: subsetted, transformed  
    spp.ef <- as.matrix(scale(spp[,2:9], center = T, scale = T))
  # Generate lambdas for plotting  
    
    
  # Coefficient-lambda visualisation
    spp.m <- glmnet(spp.ef, spp$spp, family = "gaussian", alpha = 0)
  # Plot 1  
    jpeg(coef.vis, width = 14, height = 10, units = "cm", res = 500)
    plot(spp.m, sub = title, xvar = "lambda")
    dev.off()
  
  # (2) Cross-validation, lambda min and 1se 
    spp.cv <- cv.glmnet(spp.ef, spp$spp, family = "gaussian", lambda = spp.m$lambda, alpha = 0)
  # Plot 
    jpeg(cross.val.plot, width = 14, height = 10, units = "cm", res = 500)
    plot(spp.cv, sub = title)
    dev.off() 
  # Save model (can come back to this for different lambda and coef values)
    save(spp.cv, file = cross.val.mod)
  # Lamba min and se (cols 5 and 6)
    dat[i, 5] <<- log(spp.cv$lambda.min)
    dat[i, 6] <<- log(spp.cv$lambda.1se)
  
  # (3) R2
    spp.rsq <- 1 - spp.cv$cvm/var(spp$spp) 
    spp.cvfit <- cv.ncvreg(spp.ef, spp$spp, penalty= "lasso", returnY = T)
  # Plot R2 cross-val
    jpeg(cv.r2.plot, width = 14, height = 10, units = "cm", res = 500)
    plot(spp.cvfit, type = "rsq", sub = title)
    dev.off() 
    
  # Save model (can come back to this for different lambda and r2)
    save(spp.cvfit, file = cross.val.mod)
  # Lambda of highest r2
    dat[i, 7] <<- log(spp.cvfit$lambda.min)
  # Corresponding coefficients of highest r2  
    spp.cvfit.coef <- t(coef(spp.cvfit, s = "lambda.min")) # highest r2 lambda
    dat[i, 8:16] <<- spp.cvfit.coef 

    return(dat)
    
  } # fun end
  
# Ridge regressions (.m0) ------------------------------------------------------
# Completed in order of odd rows of data frame 'dat'
# Row 1 = r15.n.tot ------------------------------------------------------------
  i               <- 1
  spp             <- r15.n.tot          
  coef.vis        <- "Graphs/Model outputs/r15.n.tot/Ridge coefficient visualisation.jpeg"        
  title           <- "r15.n.tot.m0"         
  cross.val.plot  <- "Graphs/Model outputs/r15.n.tot/Ridge MSE cv.jpeg"  
  cross.val.mod   <- "Rdata/Models/r15.n.tot/Ridge mse cv.Rdata"  
  cv.r2.plot      <- "Graphs/Model outputs/r15.n.tot/Ridge R2 cross validation.jpeg"
  cv.r2.mod       <- "Rdata/Models/r15.n.tot/Ridge R2 cv.Rdata"   
  
  fun.ridge(spp, coef.vis, title, cross.val.plot, cross.val.mod, cv.r2.plot, cv.r2.mod, i)

# Row 3 = r15.n.c3 ------------------------------------------------------------
  i               <- 3
  spp             <- r15.n.c3          
  coef.vis        <- "Graphs/Model outputs/r15.n.c3/Ridge coefficient visualisation.jpeg"        
  title           <- "r15.n.c3.m0"         
  cross.val.plot  <- "Graphs/Model outputs/r15.n.c3/Ridge MSE cv.jpeg"  
  cross.val.mod   <- "Rdata/Models/r15.n.c3/Ridge mse cv.Rdata"  
  cv.r2.plot      <- "Graphs/Model outputs/r15.n.c3/Ridge R2 cross validation.jpeg"
  cv.r2.mod       <- "Rdata/Models/r15.n.c3/Ridge R2 cv.Rdata"   
  
  fun.ridge(spp, coef.vis, title, cross.val.plot, cross.val.mod, cv.r2.plot, cv.r2.mod, i)
  
# Row 5 = r15.n.c4 ------------------------------------------------------------
  i               <- 5
  spp             <- r15.n.c4          
  coef.vis        <- "Graphs/Model outputs/r15.n.c4/Ridge coefficient visualisation.jpeg"        
  title           <- "r15.n.c4.m0"         
  cross.val.plot  <- "Graphs/Model outputs/r15.n.c4/Ridge MSE cv.jpeg"  
  cross.val.mod   <- "Rdata/Models/r15.n.c4/Ridge mse cv.Rdata"  
  cv.r2.plot      <- "Graphs/Model outputs/r15.n.c4/Ridge R2 cross validation.jpeg"
  cv.r2.mod       <- "Rdata/Models/r15.n.c4/Ridge R2 cv.Rdata"   
  
  fun.ridge(spp, coef.vis, title, cross.val.plot, cross.val.mod, cv.r2.plot, cv.r2.mod, i)
  
# Row 7 = r15.i.tot ------------------------------------------------------------
  i               <- 7
  spp             <- r15.i.tot          
  coef.vis        <- "Graphs/Model outputs/r15.i.tot/Ridge coefficient visualisation.jpeg"        
  title           <- "r15.i.tot.m0"         
  cross.val.plot  <- "Graphs/Model outputs/r15.i.tot/Ridge MSE cv.jpeg"  
  cross.val.mod   <- "Rdata/Models/r15.i.tot/Ridge mse cv.Rdata"  
  cv.r2.plot      <- "Graphs/Model outputs/r15.i.tot/Ridge R2 cross validation.jpeg"
  cv.r2.mod       <- "Rdata/Models/r15.i.tot/Ridge R2 cv.Rdata"   
  
  fun.ridge(spp, coef.vis, title, cross.val.plot, cross.val.mod, cv.r2.plot, cv.r2.mod, i)
  
# Row 9 = r15.i.c3 ------------------------------------------------------------
  i               <- 9
  spp             <- r15.i.c3          
  coef.vis        <- "Graphs/Model outputs/r15.i.c3/Ridge coefficient visualisation.jpeg"        
  title           <- "r15.i.c3.m0"         
  cross.val.plot  <- "Graphs/Model outputs/r15.i.c3/Ridge MSE cv.jpeg"  
  cross.val.mod   <- "Rdata/Models/r15.i.c3/Ridge mse cv.Rdata"  
  cv.r2.plot      <- "Graphs/Model outputs/r15.i.c3/Ridge R2 cross validation.jpeg"
  cv.r2.mod       <- "Rdata/Models/r15.i.c3/Ridge R2 cv.Rdata"   
  
  fun.ridge(spp, coef.vis, title, cross.val.plot, cross.val.mod, cv.r2.plot, cv.r2.mod, i)
  
# Row 11 = r15.i.c4 ------------------------------------------------------------
  i               <- 11
  spp             <- r15.i.c4          
  coef.vis        <- "Graphs/Model outputs/r15.i.c4/Ridge coefficient visualisation.jpeg"        
  title           <- "r15.i.c4.m0"         
  cross.val.plot  <- "Graphs/Model outputs/r15.i.c4/Ridge MSE cv.jpeg"  
  cross.val.mod   <- "Rdata/Models/r15.i.c4/Ridge mse cv.Rdata"  
  cv.r2.plot      <- "Graphs/Model outputs/r15.i.c4/Ridge R2 cross validation.jpeg"
  cv.r2.mod       <- "Rdata/Models/r15.i.c4/Ridge R2 cv.Rdata"   
  
  fun.ridge(spp, coef.vis, title, cross.val.plot, cross.val.mod, cv.r2.plot, cv.r2.mod, i)
  
# r50 ----
# Row 13 = r50.n.tot ------------------------------------------------------------
  i               <- 13
  spp             <- r50.n.tot          
  coef.vis        <- "Graphs/Model outputs/r50.n.tot/Ridge coefficient visualisation.jpeg"        
  title           <- "r50.n.tot.m0"         
  cross.val.plot  <- "Graphs/Model outputs/r50.n.tot/Ridge MSE cv.jpeg"  
  cross.val.mod   <- "Rdata/Models/r50.n.tot/Ridge mse cv.Rdata"  
  cv.r2.plot      <- "Graphs/Model outputs/r50.n.tot/Ridge R2 cross validation.jpeg"
  cv.r2.mod       <- "Rdata/Models/r50.n.tot/Ridge R2 cv.Rdata"   
  
  fun.ridge(spp, coef.vis, title, cross.val.plot, cross.val.mod, cv.r2.plot, cv.r2.mod, i)
  
# Row 15 = r50.n.c3 ------------------------------------------------------------
  i               <- 15
  spp             <- r50.n.c3          
  coef.vis        <- "Graphs/Model outputs/r50.n.c3/Ridge coefficient visualisation.jpeg"        
  title           <- "r50.n.c3.m0"         
  cross.val.plot  <- "Graphs/Model outputs/r50.n.c3/Ridge MSE cv.jpeg"  
  cross.val.mod   <- "Rdata/Models/r50.n.c3/Ridge mse cv.Rdata"  
  cv.r2.plot      <- "Graphs/Model outputs/r50.n.c3/Ridge R2 cross validation.jpeg"
  cv.r2.mod       <- "Rdata/Models/r50.n.c3/Ridge R2 cv.Rdata"   
  
  fun.ridge(spp, coef.vis, title, cross.val.plot, cross.val.mod, cv.r2.plot, cv.r2.mod, i)
  
# Row 17 = r50.n.c4 ------------------------------------------------------------
  i               <- 17
  spp             <- r50.n.c4          
  coef.vis        <- "Graphs/Model outputs/r50.n.c4/Ridge coefficient visualisation.jpeg"        
  title           <- "r50.n.c4.m0"         
  cross.val.plot  <- "Graphs/Model outputs/r50.n.c4/Ridge MSE cv.jpeg"  
  cross.val.mod   <- "Rdata/Models/r50.n.c4/Ridge mse cv.Rdata"  
  cv.r2.plot      <- "Graphs/Model outputs/r50.n.c4/Ridge R2 cross validation.jpeg"
  cv.r2.mod       <- "Rdata/Models/r50.n.c4/Ridge R2 cv.Rdata"   
  
  fun.ridge(spp, coef.vis, title, cross.val.plot, cross.val.mod, cv.r2.plot, cv.r2.mod, i)
  
# Row 19 = r50.i.tot ------------------------------------------------------------
  i               <- 19
  spp             <- r50.i.tot          
  coef.vis        <- "Graphs/Model outputs/r50.i.tot/Ridge coefficient visualisation.jpeg"        
  title           <- "r50.i.tot.m0"         
  cross.val.plot  <- "Graphs/Model outputs/r50.i.tot/Ridge MSE cv.jpeg"  
  cross.val.mod   <- "Rdata/Models/r50.i.tot/Ridge mse cv.Rdata"  
  cv.r2.plot      <- "Graphs/Model outputs/r50.i.tot/Ridge R2 cross validation.jpeg"
  cv.r2.mod       <- "Rdata/Models/r50.i.tot/Ridge R2 cv.Rdata"   
  
  fun.ridge(spp, coef.vis, title, cross.val.plot, cross.val.mod, cv.r2.plot, cv.r2.mod, i)
  
# Row 21 = r50.i.c3 ------------------------------------------------------------
  i               <- 21
  spp             <- r50.i.c3          
  coef.vis        <- "Graphs/Model outputs/r50.i.c3/Ridge coefficient visualisation.jpeg"        
  title           <- "r50.i.c3.m0"         
  cross.val.plot  <- "Graphs/Model outputs/r50.i.c3/Ridge MSE cv.jpeg"  
  cross.val.mod   <- "Rdata/Models/r50.i.c3/Ridge mse cv.Rdata"  
  cv.r2.plot      <- "Graphs/Model outputs/r50.i.c3/Ridge R2 cross validation.jpeg"
  cv.r2.mod       <- "Rdata/Models/r50.i.c3/Ridge R2 cv.Rdata"   
  
  fun.ridge(spp, coef.vis, title, cross.val.plot, cross.val.mod, cv.r2.plot, cv.r2.mod, i)
  
# Row 23 = r50.i.c4 ------------------------------------------------------------
  i               <- 23
  spp             <- r50.i.c4          
  coef.vis        <- "Graphs/Model outputs/r50.i.c4/Ridge coefficient visualisation.jpeg"        
  title           <- "r50.i.c4.m0"         
  cross.val.plot  <- "Graphs/Model outputs/r50.i.c4/Ridge MSE cv.jpeg"  
  cross.val.mod   <- "Rdata/Models/r50.i.c4/Ridge mse cv.Rdata"  
  cv.r2.plot      <- "Graphs/Model outputs/r50.i.c4/Ridge R2 cross validation.jpeg"
  cv.r2.mod       <- "Rdata/Models/r50.i.c4/Ridge R2 cv.Rdata"   
  
  fun.ridge(spp, coef.vis, title, cross.val.plot, cross.val.mod, cv.r2.plot, cv.r2.mod, i)  
  
# -------------------------------------------------------------------------------  
  
# LASSO -------------------------------------------------------------------------
# Function + requirements -------------------------------------------------------
  fun.lasso <- function(spp,            # spp data frame (with EFs, no NAs, in matrix form - see above)
                        coef.vis,       # save coef vis plot
                        title,          # model title - specific to ridge/lasso
                        cross.val.plot, # save cross-validation of lambda-MSE plot
                        cross.val.mod,  # save cross-val model
                        cv.r2.plot,     # save cross-validation of lambda-MSE plot
                        cv.r2.mod,      # save cross-val model
                        i)              # dat$model row number - thus order of iteration needs to be the order of dat
    
  { # function start
    
  # Environmental variables: subsetted, transformed  
    spp.ef <- as.matrix(scale(spp[,2:9], center = T, scale = T))
  
  # Coefficient-lambda visualisation
    spp.m <- glmnet(spp.ef, spp$spp, family = "gaussian", alpha = 1)
  # Plot 1  
    jpeg(coef.vis, width = 14, height = 10, units = "cm", res = 500)
    plot(spp.m, sub = title, xvar = "lambda")
    dev.off()
    
  # (2) Cross-validation, lambda min and 1se 
    spp.cv <- cv.glmnet(spp.ef, spp$spp, family = "gaussian", lambda = spp.m$lambda, alpha = 1)
  # Plot 2
    jpeg(cross.val.plot, width = 14, height = 10, units = "cm", res = 500)
    plot(spp.cv, sub = title)
    dev.off() 
  # Save model (can come back to this for different lambda and coef values)
    save(spp.cv, file = cross.val.mod)
  # Lamba min and se (cols 5 and 6)
    dat[i, 5] <<- log(spp.cv$lambda.min)
    dat[i, 6] <<- log(spp.cv$lambda.1se)
    
  # (3) R2
    spp.rsq <- 1 - spp.cv$cvm/var(spp$spp) 
    spp.cvfit <- cv.ncvreg(spp.ef, spp$spp, penalty= "lasso", returnY = T)
  # Plot R2 cross-val
    jpeg(cv.r2.plot, width = 14, height = 10, units = "cm", res = 500)
    plot(spp.cvfit, type = "rsq", sub = title)
    dev.off() 
    
  # Save model (can come back to this for different lambda and r2)
    save(spp.cvfit, file = cross.val.mod)
    # Lambda of highest r2
    dat[i, 7] <<- log(spp.cvfit$lambda.min)
  # Corresponding coefficients of highest r2  
    spp.cvfit.coef <- t(coef(spp.cvfit, s = "lambda.min")) # highest r2 lambda
    dat[i, 8:16] <<- spp.cvfit.coef 
    
    return(dat)
    
 } # fun end
  
# LASSO (.m1) -------------------------------------------------------------------
# Completed in order of even rows of data frame 'dat'
# Row 2 = r15.n.tot -------------------------------------------------------------
  i               <- 2
  spp             <- r15.n.tot          
  coef.vis        <- "Graphs/Model outputs/r15.n.tot/Lasso coefficient visualisation.jpeg"        
  title           <- "r15.n.tot.m1"         
  cross.val.plot  <- "Graphs/Model outputs/r15.n.tot/Lasso MSE cv.jpeg"  
  cross.val.mod   <- "Rdata/Models/r15.n.tot/Lasso mse cv.Rdata"  
  cv.r2.plot      <- "Graphs/Model outputs/r15.n.tot/Lasso R2 cross validation.jpeg"
  cv.r2.mod       <- "Rdata/Models/r15.n.tot/Lasso R2 cv.Rdata"   
  
  fun.lasso(spp, coef.vis, title, cross.val.plot, cross.val.mod, cv.r2.plot, cv.r2.mod, i)
  
# Row 4 = r15.n.c3 ------------------------------------------------------------
  i               <- 4
  spp             <- r15.n.c3          
  coef.vis        <- "Graphs/Model outputs/r15.n.c3/Lasso coefficient visualisation.jpeg"        
  title           <- "r15.n.c3.m1"         
  cross.val.plot  <- "Graphs/Model outputs/r15.n.c3/Lasso MSE cv.jpeg"  
  cross.val.mod   <- "Rdata/Models/r15.n.c3/Lasso mse cv.Rdata"  
  cv.r2.plot      <- "Graphs/Model outputs/r15.n.c3/Lasso R2 cross validation.jpeg"
  cv.r2.mod       <- "Rdata/Models/r15.n.c3/Lasso R2 cv.Rdata"   
  
  fun.lasso(spp, coef.vis, title, cross.val.plot, cross.val.mod, cv.r2.plot, cv.r2.mod, i)
  
# Row 6 = r15.n.c4 ------------------------------------------------------------
  i               <- 6
  spp             <- r15.n.c4          
  coef.vis        <- "Graphs/Model outputs/r15.n.c4/Lasso coefficient visualisation.jpeg"        
  title           <- "r15.n.c4.m1"         
  cross.val.plot  <- "Graphs/Model outputs/r15.n.c4/Lasso MSE cv.jpeg"  
  cross.val.mod   <- "Rdata/Models/r15.n.c4/Lasso mse cv.Rdata"  
  cv.r2.plot      <- "Graphs/Model outputs/r15.n.c4/Lasso R2 cross validation.jpeg"
  cv.r2.mod       <- "Rdata/Models/r15.n.c4/Lasso R2 cv.Rdata"   
  
  fun.lasso(spp, coef.vis, title, cross.val.plot, cross.val.mod, cv.r2.plot, cv.r2.mod, i)
  
# Row 8 = r15.i.tot ------------------------------------------------------------
  i               <- 8
  spp             <- r15.i.tot          
  coef.vis        <- "Graphs/Model outputs/r15.i.tot/Lasso coefficient visualisation.jpeg"        
  title           <- "r15.i.tot.m1"         
  cross.val.plot  <- "Graphs/Model outputs/r15.i.tot/Lasso MSE cv.jpeg"  
  cross.val.mod   <- "Rdata/Models/r15.i.tot/Lasso mse cv.Rdata"  
  cv.r2.plot      <- "Graphs/Model outputs/r15.i.tot/Lasso R2 cross validation.jpeg"
  cv.r2.mod       <- "Rdata/Models/r15.i.tot/Lasso R2 cv.Rdata"   
  
  fun.lasso(spp, coef.vis, title, cross.val.plot, cross.val.mod, cv.r2.plot, cv.r2.mod, i)
  
# Row 10 = r15.i.c3 ------------------------------------------------------------
  i               <- 10
  spp             <- r15.i.c3          
  coef.vis        <- "Graphs/Model outputs/r15.i.c3/Lasso coefficient visualisation.jpeg"        
  title           <- "r15.i.c3.m1"         
  cross.val.plot  <- "Graphs/Model outputs/r15.i.c3/Lasso MSE cv.jpeg"  
  cross.val.mod   <- "Rdata/Models/r15.i.c3/Lasso mse cv.Rdata"  
  cv.r2.plot      <- "Graphs/Model outputs/r15.i.c3/Lasso R2 cross validation.jpeg"
  cv.r2.mod       <- "Rdata/Models/r15.i.c3/Lasso R2 cv.Rdata"   
  
  fun.lasso(spp, coef.vis, title, cross.val.plot, cross.val.mod, cv.r2.plot, cv.r2.mod, i)
  
# Row 12 = r15.i.c4 ------------------------------------------------------------
  i               <- 12
  spp             <- r15.i.c4          
  coef.vis        <- "Graphs/Model outputs/r15.i.c4/Lasso coefficient visualisation.jpeg"        
  title           <- "r15.i.c4.m1"         
  cross.val.plot  <- "Graphs/Model outputs/r15.i.c4/Lasso MSE cv.jpeg"  
  cross.val.mod   <- "Rdata/Models/r15.i.c4/Lasso mse cv.Rdata"  
  cv.r2.plot      <- "Graphs/Model outputs/r15.i.c4/Lasso R2 cross validation.jpeg"
  cv.r2.mod       <- "Rdata/Models/r15.i.c4/Lasso R2 cv.Rdata"   
  
  fun.lasso(spp, coef.vis, title, cross.val.plot, cross.val.mod, cv.r2.plot, cv.r2.mod, i)
  
# r50 ----
# Row 14 = r50.n.tot ------------------------------------------------------------
  i               <- 14
  spp             <- r50.n.tot          
  coef.vis        <- "Graphs/Model outputs/r50.n.tot/Lasso coefficient visualisation.jpeg"        
  title           <- "r50.n.tot.m1"         
  cross.val.plot  <- "Graphs/Model outputs/r50.n.tot/Lasso MSE cv.jpeg"  
  cross.val.mod   <- "Rdata/Models/r50.n.tot/Lasso mse cv.Rdata"  
  cv.r2.plot      <- "Graphs/Model outputs/r50.n.tot/Lasso R2 cross validation.jpeg"
  cv.r2.mod       <- "Rdata/Models/r50.n.tot/Lasso R2 cv.Rdata"   
  
  fun.lasso(spp, coef.vis, title, cross.val.plot, cross.val.mod, cv.r2.plot, cv.r2.mod, i)
  
# Row 16 = r50.n.c3 ------------------------------------------------------------
  i               <- 16
  spp             <- r50.n.c3          
  coef.vis        <- "Graphs/Model outputs/r50.n.c3/Lasso coefficient visualisation.jpeg"        
  title           <- "r50.n.c3.m1"         
  cross.val.plot  <- "Graphs/Model outputs/r50.n.c3/Lasso MSE cv.jpeg"  
  cross.val.mod   <- "Rdata/Models/r50.n.c3/Lasso mse cv.Rdata"  
  cv.r2.plot      <- "Graphs/Model outputs/r50.n.c3/Lasso R2 cross validation.jpeg"
  cv.r2.mod       <- "Rdata/Models/r50.n.c3/Lasso R2 cv.Rdata"   
  
  fun.lasso(spp, coef.vis, title, cross.val.plot, cross.val.mod, cv.r2.plot, cv.r2.mod, i)
  
# Row 18 = r50.n.c4 ------------------------------------------------------------
  i               <- 18
  spp             <- r50.n.c4          
  coef.vis        <- "Graphs/Model outputs/r50.n.c4/Lasso coefficient visualisation.jpeg"        
  title           <- "r50.n.c4.m1"         
  cross.val.plot  <- "Graphs/Model outputs/r50.n.c4/Lasso MSE cv.jpeg"  
  cross.val.mod   <- "Rdata/Models/r50.n.c4/Lasso mse cv.Rdata"  
  cv.r2.plot      <- "Graphs/Model outputs/r50.n.c4/Lasso R2 cross validation.jpeg"
  cv.r2.mod       <- "Rdata/Models/r50.n.c4/Lasso R2 cv.Rdata"   
  
  fun.lasso(spp, coef.vis, title, cross.val.plot, cross.val.mod, cv.r2.plot, cv.r2.mod, i)

# Row 20 = r50.i.tot ------------------------------------------------------------
  i               <- 20
  spp             <- r50.i.tot          
  coef.vis        <- "Graphs/Model outputs/r50.i.tot/Lasso coefficient visualisation.jpeg"        
  title           <- "r50.i.tot.m1"         
  cross.val.plot  <- "Graphs/Model outputs/r50.i.tot/Lasso MSE cv.jpeg"  
  cross.val.mod   <- "Rdata/Models/r50.i.tot/Lasso mse cv.Rdata"  
  cv.r2.plot      <- "Graphs/Model outputs/r50.i.tot/Lasso R2 cross validation.jpeg"
  cv.r2.mod       <- "Rdata/Models/r50.i.tot/Lasso R2 cv.Rdata"   
  
  fun.lasso(spp, coef.vis, title, cross.val.plot, cross.val.mod, cv.r2.plot, cv.r2.mod, i)
  
# Row 22 = r50.i.c3 ------------------------------------------------------------
  i               <- 22
  spp             <- r50.i.c3          
  coef.vis        <- "Graphs/Model outputs/r50.i.c3/Lasso coefficient visualisation.jpeg"        
  title           <- "r50.i.c3.m1"         
  cross.val.plot  <- "Graphs/Model outputs/r50.i.c3/Lasso MSE cv.jpeg"  
  cross.val.mod   <- "Rdata/Models/r50.i.c3/Lasso mse cv.Rdata"  
  cv.r2.plot      <- "Graphs/Model outputs/r50.i.c3/Lasso R2 cross validation.jpeg"
  cv.r2.mod       <- "Rdata/Models/r50.i.c3/Lasso R2 cv.Rdata"   
  
  fun.lasso(spp, coef.vis, title, cross.val.plot, cross.val.mod, cv.r2.plot, cv.r2.mod, i)
  
# Row 24 = r50.i.c4 ------------------------------------------------------------
  i               <- 24
  spp             <- r50.i.c4          
  coef.vis        <- "Graphs/Model outputs/r50.i.c4/Lasso coefficient visualisation.jpeg"        
  title           <- "r50.i.c4.m1"         
  cross.val.plot  <- "Graphs/Model outputs/r50.i.c4/Lasso MSE cv.jpeg"  
  cross.val.mod   <- "Rdata/Models/r50.i.c4/Lasso mse cv.Rdata"  
  cv.r2.plot      <- "Graphs/Model outputs/r50.i.c4/Lasso R2 cross validation.jpeg"
  cv.r2.mod       <- "Rdata/Models/r50.i.c4/Lasso R2 cv.Rdata"   
  
  fun.lasso(spp, coef.vis, title, cross.val.plot, cross.val.mod, cv.r2.plot, cv.r2.mod, i)  
  
# -------------------------------------------------------------------------------    

# Save
  write.csv(dat, "CSV/Model m0.1 highest r2 coeffcients.csv", row.names = F)
  
  
# -------------------------------------------------------------------------------
  
  
# Modelling without LASSO/ridge ------------------------------------------------- 
# Just the linear model   
# And just 15!
# Load library from above
  
  
  
# Data ----------------------------------------------------------------   
# We have two species data frames: nat and int
# These have teh two rarefied records, and all three pp pathways
  nat <- read.csv("CSV/Native multiple cutoff COMPLETE.csv", header = T) %>%
    select(total.rare.15, C3.rare.15, C4.rare.15)
  colnames(nat) <- c("n.tot", "n.c3", "n.c4")
  head(nat)
  
  int <- read.csv("CSV/Introduced multiple cutoff COMPLETE.csv", header = T) %>%
    select(total.rare.15, C3.rare.15, C4.rare.15)
  colnames(int) <- c("i.tot", "i.c3", "i.c4")
  
# Environmental variables 
# variable selection efs
  vs.efs <- c("arid",  "mat", "pwarmq", "pcoldq", 
              "th", "pewc",    "hii",     "ps")
  
  efs <- read.csv("C:/Users/s436862/Dropbox/Climate Matching/1. Data files/EFs/4. EFs complete/EF_COMPLETE.csv", header = T) %>%
    select(vs.efs)   
  
# Australian cell of species and efs 
  land.cat <- read.csv("C:/Users/s436862/Dropbox/Climate Matching/1. Data files/EFs/4. EFs complete/Terrestrial categories.csv", header = T)
  
# Data cleaning -------------------------------------------------------
# We need to remove Nas. These will pop up individually for each spp-ef pair, so we need to make 12 dfs to deal with this
# Requires: spp = specific spp column
# land.cat = subset of spp and ef col that refers to Aus-cells
# efs = envuronmental variables in same format
  
  spp.ef <- cbind()
  
  
  
  