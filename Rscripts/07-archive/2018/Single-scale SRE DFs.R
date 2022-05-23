

# Single-scale dfs (50, 100, 200) of all grass groups 

# NOTE: Lanum updated 23/01 (do this for rasters, too)

  library(rasterVis)
  library(ggplot2)
  library(ggmap)
  library(tidyr)
  library(raster)
  library(rgdal)
  library(maptools)
  library(dplyr)
  
  
  setwd("C:/Users/s436862/Dropbox/Climate Matching/1. Data files/")
 

# Data --------------------------------------------------------------------
  
# AVH --------------------------------------------------------------
  rm(list = ls())
  
  avh <- read.csv("AVH/AVH native records.csv", header = T) 
  
  xy <- cbind(avh$long, avh$lat)
  spp <- as.numeric(factor(avh$species))
  
  
# Introduced --------------------------------------------------------------
  rm(list = ls())
  
  int <- read.csv("AVH/AVH grass records.csv", header = T) %>%
    dplyr::filter(status == "introduced") %>%
    dplyr::select(species, lat, long)
  
  xy <- cbind(int$long, int$lat)
  spp <- as.numeric(factor(int$species))
  
  
# Native --------------------------------------------------------------
  rm(list = ls())
  
  nat <- read.csv("AVH/AVH grass records.csv", header = T) %>%
    dplyr::filter(status == "native") %>%
    dplyr::select(species, lat, long)
  
  xy <- cbind(nat$long, nat$lat)
  spp <- as.numeric(factor(nat$species))
  
  
# Poa --------------------------------------------------------------
  rm(list = ls())
  
  poa <- read.csv("AVH/AVH grass records.csv", header = T) %>%
    dplyr::select(species, lat, long)
  
  p <- poa
  xy <- cbind(poa$long, poa$lat)
  spp <- as.numeric(factor(poa$species))
  
# ---------------------------------------------------------------------  
  
  
# Scale --------------------------------

# scale    
  # width <- 50
  # width <- 100
  width <- 200
  
# raster template & Aus outline (shapefile) ---------------------------
  b <- raster("EFs/EFs cropped/arid.grd")
  oz <- borders("world", region = "Australia")

# aggregate original raster
  raster <- aggregate(b, fact = width, fun = mean)
    
# actual richness (a)
  a <- rasterize(xy, raster, field = spp, fun = function(x,...) {length(unique(na.omit(x))) })
# number of records (n)
  n <- rasterize(xy, raster, fun = function(x,...) {length(na.omit(x)) })
# single-, double-, thriple- and quadruple-tons
# going to refer these as f1, f2, f3 and f4 
  f1 <- rasterize(xy, raster, field = spp, fun = function(x,...) {length(which(table(x)==1)) })
  f2 <- rasterize(xy, raster, field = spp, fun = function(x,...) {length(which(table(x)==2)) })
  f3 <- rasterize(xy, raster, field = spp, fun = function(x,...) {length(which(table(x)==3)) })
  f4 <- rasterize(xy, raster, field = spp, fun = function(x,...) {length(which(table(x)==4)) })
    
# reference for extraterrestrial NA removal
 b <- getValues(raster)
    
# actual/AVH richness
  a <- getValues(a)
    
# SRE correctives
  f1 <- getValues(f1)
  f2 <- getValues(f2)
  f3 <- getValues(f3)
  f4 <- getValues(f4)
  n <- getValues(n)
    
# exclude cells outside of Australia, as dataframe
  ichao1_na <- data.frame(a, b, f1, f2, f3, f4, n)
  ichao1 <- ichao1_na[!is.na(b), ] 
    
# Jackknife estimators ----------------------------------------
    
# from Chapter 4., we'll do both first- and second-order estimates for the lols
# the formulae are: [a + f1] & [a + 2*f1 - f2]
    
  jack1st <- function(x) {
    ichao1$a + ichao1$f1
  } # finish function
    
  ichao1$jack1st <- jack1st(ichao1) 
    
    
    
  jack2nd <- function(x) {
    ichao1$a + 2*ichao1$f1 - ichao1$f2
  } # finish function   
    
    
    
  ichao1$jack2nd <- jack2nd(ichao1) 
  
# Lanumteang & Bohning, 2011 estimators ----------------------------------------
# f2 bias-corrected form included (f1) normal is f3, and f2 when there is no f3 (substituted for +1)
# and f3 bias-corrected
  lanum_f3 <- function(x) {
    ichao1$a + (3*(ichao1$f1^3)*ichao1$f3)/(4*(ichao1$f2^3))
    } 
    
  lanum_f1 <- function(x) {
    ichao1$a + (3/4 * (ichao1$f1*(ichao1$f1-1)*(ichao1$f1-2)*(ichao1$f3+1)) / ((ichao1$f2 + 1) * (ichao1$f2 + 2) * (ichao1$f2 + 3)))
  } 
  
  lanum_f2 <- function(x) {
    ichao1$a + (3*(ichao1$f1^3)*(ichao1$f3+1))/(4*(ichao1$f2^3))
  }
  
  ichao1$lanum <- ifelse(ichao1$f3 == 0, ifelse(f2 == 0, lanum_f1(ichao1), lanum_f2(ichao1)), lanum_f3(ichao1))
    
# iChao1 equations (Chiu et al. 2014) -----------------------------------------------    
    
# this section will look like this:
    
# define each equation as a function (e.g. 'chao1'), then put each into an all-in-one that has below's structure
# (a) ifelse(f2 ==0,
#            chao1_no_dbl,
#        (b) ifelse(f3 == 0,
#                   chao1_w_dbl,
#               (c) ifelse(f4 == 0,
#                          ichao1_no_f4,
#                     (d)  ichao1_w_f4)))
    
# first, calculate the 'old' chao1 estimator (henceforth called 'chao1')
  chao1_estimator <- function(x) {
      
    ifelse(ichao1$f2 == 0, 
          (ichao1$a + (((ichao1$n-1) / ichao1$n) * (ichao1$f1 * (ichao1$f1-1)) / (2 * (ichao1$f2+1)))),
          (ichao1$a + (((ichao1$n-1) / ichao1$n) * ((ichao1$f1^2)/(2*ichao1$f2))))
      )
      
    } # finish function
    
  ichao1$chao1 <- chao1_estimator(ichao1)
    
# (a) chao1 when no doubletons   
  chao1_no_f2 <- function(x) {
    ichao1$a + (((ichao1$n-1) / ichao1$n) * (ichao1$f1 * (ichao1$f1-1)) / (2 * (ichao1$f2+1)))
  } # finish function
    
# (b) chao1 with doubltons
  chao1_w_f2 <- function(x) {
    ichao1$a + (((ichao1$n-1) / ichao1$n) * ((ichao1$f1^2)/(2*ichao1$f2)))
  } # finish function  
    
# (c) ichao1 when no quadrupletons (f4's)
  ichao1_no_f4 <- function(x) {
      
    (ichao1$a + (((ichao1$n-1) / ichao1$n) * ((ichao1$f1^2)/(2*ichao1$f2)))) + 
      ((((ichao1$n-3) / (4*ichao1$n)) * (ichao1$f3/(ichao1$f4+1))) *
         (ifelse( (ichao1$f1 - ((ichao1$n-3) / (2*(ichao1$n-1))) * (ichao1$f2*ichao1$f3/(ichao1$f4+1))) > 0, 
                  (ichao1$f1 - ((ichao1$n-3) / (2*(ichao1$n-1))) * (ichao1$f2*ichao1$f3/(ichao1$f4+1))), 0 )))
  } # finish function
    
# (d) ichao1 with quadrupletons (f4's)
  ichao1_w_f4 <- function(x) {
      
     (ichao1$a + (((ichao1$n-1) / ichao1$n) * ((ichao1$f1^2)/(2*ichao1$f2)))) + 
      ((((ichao1$n-3) / (4*ichao1$n)) * (ichao1$f3/ichao1$f4)) *
          (ifelse( (ichao1$f1 - ((ichao1$n-3) / (2*(ichao1$n-1))) * (ichao1$f2*ichao1$f3/ichao1$f4)) > 0, 
                   (ichao1$f1 - ((ichao1$n-3) / (2*(ichao1$n-1))) * (ichao1$f2*ichao1$f3/ichao1$f4)), 0 )))
      
  } # finish function
    
# (a-d) complete ichao1 estimation   
  ichao1_estimator <- function(x) {
    ifelse(ichao1$f2 == 0,
            chao1_no_f2(ichao1),
            ifelse(ichao1$f3 == 0,
                  chao1_w_f2(ichao1),
                  ifelse(ichao1$f4 == 0,
                          ichao1_no_f4(ichao1),
                          ichao1_w_f4(ichao1)
                    )))
  } # finish function
    
  ichao1$ichao1 <- ichao1_estimator(ichao1)  
    
    
    
    
    
    
# Chao1 var, SE, CIs, coverages ----------------------------------------------
# from Chiu et al. 2014 -- NOT the same as the one derived from Chapter 4.
    
# chao1 variance with doubletons
# (from Chiu et al. -- has (n-1)/n correction) 
  chiu_var_f2 <-  function(x) {
    ichao1$f2 * 
      ( (((0.25 * (((ichao1$n-1)/ichao1$n)^2)) * ((ichao1$f1/ichao1$f2)^4))) + 
          ((((ichao1$n-1)/ichao1$n)^2) * ((ichao1$f1/ichao1$f2)^3)) + 
          (0.5 * ((ichao1$n-1)/ichao1$n) * ((ichao1$f1/ichao1$f2)^2)) )
  } # finish function
    
# chao1 variance no doubletons 
  chiu_var_f1 <- function(x) {
    (0.25 * (((ichao1$n-1)^2)/ichao1$n^2) * ichao1$f1 * ((2*ichao1$f1-1)^2)) + 
      ( (0.5 * ichao1$f1 * (ichao1$f1-1)) - 
          (0.25 * (ichao1$f1^4)/(ichao1$a + (((ichao1$n-1)/ichao1$n) * (ichao1$f1*(ichao1$f1-1))/(2*(ichao1$f2+1))))) )  
  } # finish function
    
# ifelse to calculate it properly  
  chiu_var <-  ifelse(ichao1$f2 == 0,
                      chiu_var_f1(ichao1),
                      chiu_var_f2(ichao1)
    )
    
# standard error
  ichao1$chiu_se <- sqrt(chiu_var) / sqrt(ichao1$n)
    
# Confidence interval from Chiu et al. (1) w dbls +2-sided 95% CI; (2) no dbl +1-sided 95%CI
# (1) f2s
# R for f2
  R_f2 <- exp(1.96*(1 + (chiu_var_f2(ichao1$chao1) / 
                             (ichao1$chao1 - ichao1$a)^2))^0.5)
# CIs f2
  lower_ci_f2 <- ichao1$a + (ichao1$chao1 - ichao1$a) / R_f2
  upper_ci_f2 <- ichao1$a + (ichao1$chao1 - ichao1$a) * R_f2
    
# (2) only f1s
# R f1
  R_f1 <- exp(1.64*(1 + (chiu_var_f1(ichao1$chao1) / 
                             (ichao1$chao1 - ichao1$a)^2))^0.5)
    
# CI f1
# note: no_dbl_lower_ci has no_dblton variance in it; whereas Chiu's has the ys_dblton 
  lower_ci_f1 <- ichao1$a + (ichao1$chao1 - ichao1$a) / R_f1 
# note: only supposed to be one of these for this
    
# I want to have upper + lower CIs when f2 > 0; lower no dbltons, ifelse
# reasons: Chiu says it's not good when sample-size low; this seems a good way define that (thus using var_f1 instead of _f2)
  ichao1$lower_ci <- ifelse(ichao1$f2 == 0, lower_ci_f1, lower_ci_f2) 
  ichao1$upper_ci <- ifelse(ichao1$f2 == 0, 0, upper_ci_f2) 
    
# 27/11: adding C from Chao Jost 2012; note: there is a dbl_bias version in Chiu 2014. Weird, but cool.
  ichao1$jost_C <- ifelse(ichao1$f2 > 0,
                          1 - ((ichao1$f1/ichao1$n) * (((ichao1$n-1)*ichao1$f1) / (((ichao1$n-1)*ichao1$f1) + 2*ichao1$f2))), 
                          1 - ((ichao1$f1/ichao1$n) * ((ichao1$n-1)*(ichao1$f1-1) / (((ichao1$n-1)*(ichao1$f1-1)) + 2))) )
    
# ratio: now (29/11) for Chao1 (not iChao) 
  ichao1$chao1_C <- ichao1$a / ichao1$chao1
    
# if this does anything  
  ichao1 <- as.data.frame(ichao1)
    
# Community coverage -----------------------
# up until this point, all the zeroes have been NAs
# I need to convert them to do the percentage estimates
  ichao1$n[is.na(ichao1$n)] <- 0 
  sapply(ichao1, function(x) sum(is.na(x)))
# # or simply
# sum(is.na(df.na))
    
# community estimates test 8/12 (7)
  ichao1$total_cells <- length(ichao1$n)
  ichao1$zero <- sum(ichao1$n == 0) / ichao1$total_cells * 100
  ichao1$ten <- sum(ichao1$n <= 10) / ichao1$total_cells * 100
  ichao1$hundred <- sum(ichao1$n <= 100) / ichao1$total_cells * 100
  ichao1$thousand <- sum(ichao1$n <= 1000) / ichao1$total_cells * 100
  ichao1$five_thousand <- sum(ichao1$n <= 5000) / ichao1$total_cells * 100
  ichao1$ten_thousand <- sum(ichao1$n <= 10000) / ichao1$total_cells * 100 
    

# Pearson's correlation coefficients between record# and SRE (6)
  ichao1$n_a_cor <- cor(ichao1$n, ichao1$a, method = "pearson") * 100
  ichao1$ n_ichao1_cor <- cor(ichao1$n, ichao1$ichao1, method = "pearson") * 100
  ichao1$n_chao1_cor <- cor(ichao1$n, ichao1$chao1, method = "pearson") * 100
  ichao1$n_jack1st_cor <- cor(ichao1$n, ichao1$jack1st, method = "pearson") * 100
  ichao1$n_jack2nd_cor <- cor(ichao1$n, ichao1$jack2nd, method = "pearson") * 100
  ichao1$n_lanum_cor <- cor(ichao1$n, ichao1$lanum, method = "pearson") * 100
    
# removing NAs (or not for model stuff)  
  ichao1_no_na <- ichao1[!is.na(ichao1$a), ]
  ichao1$n[is.na(ichao1$n)] <- 0 
  
# Saving various bits and pieces ------------------------------------- 

  
# --------------- 50 km --------------------
# AVH
  multi_scale_avh <- ichao1_no_na
  write.csv(multi_scale_avh, "C:/Users/s436862/Dropbox/Climate Matching/4. Results/Grass groups AVH/Single scales/AVH 50km SRE DF.csv", row.names = F)
  
  multi_scale_avh <- ichao1
  write.csv(multi_scale_avh, "C:/Users/s436862/Dropbox/Climate Matching/4. Results/Grass groups AVH/Single scales/AVH 50km SRE DF contains NAs.csv", row.names = F)
  
# Introduced
  multi_scale_int <- ichao1_no_na
  write.csv(multi_scale_int, "C:/Users/s436862/Dropbox/Climate Matching/4. Results/Grass groups AVH/Single scales/Int 50km SRE DF.csv", row.names = F)
  
  multi_scale_int <- ichao1
  write.csv(multi_scale_int, "C:/Users/s436862/Dropbox/Climate Matching/4. Results/Grass groups AVH/Single scales/Int 50km SRE DF contains NAs.csv", row.names = F)
  
# Native 
  multi_scale_nat <- ichao1_no_na
  write.csv(multi_scale_nat, "C:/Users/s436862/Dropbox/Climate Matching/4. Results/Grass groups AVH/Single scales/Nat 50km SRE DF.csv", row.names = F)
  
  multi_scale_nat_na <- ichao1
  write.csv(multi_scale_nat_na, "C:/Users/s436862/Dropbox/Climate Matching/4. Results/Grass groups AVH/Single scales/Nat 50km SRE DF contains NAs.csv", row.names = F)
  
# Poa
  multi_scale_poa <- ichao1_no_na
  write.csv(multi_scale_poa, "C:/Users/s436862/Dropbox/Climate Matching/4. Results/Grass groups AVH/Single scales/Poa 50km SRE DF.csv", row.names = F)
  
  multi_scale_poa <- ichao1
  write.csv(multi_scale_poa, "C:/Users/s436862/Dropbox/Climate Matching/4. Results/Grass groups AVH/Single scales/Poa 50km SRE DF contains NAs.csv", row.names = F)
  
  
# -------------- 100 km --------------------

# AVH
  multi_scale_avh <- ichao1_no_na
  write.csv(multi_scale_avh, "C:/Users/s436862/Dropbox/Climate Matching/4. Results/Grass groups AVH/Single scales/AVH 100km SRE DF.csv", row.names = F)
  
  multi_scale_avh <- ichao1
  write.csv(multi_scale_avh, "C:/Users/s436862/Dropbox/Climate Matching/4. Results/Grass groups AVH/Single scales/AVH 100km SRE DF contains NAs.csv", row.names = F)
  
# Introduced
  multi_scale_int <- ichao1_no_na
  write.csv(multi_scale_int, "C:/Users/s436862/Dropbox/Climate Matching/4. Results/Grass groups AVH/Single scales/Int 100km SRE DF.csv", row.names = F)
  
  multi_scale_int <- ichao1
  write.csv(multi_scale_int, "C:/Users/s436862/Dropbox/Climate Matching/4. Results/Grass groups AVH/Single scales/Int 100km SRE DF contains NAs.csv", row.names = F)

# Native 
  multi_scale_nat <- ichao1_no_na
  write.csv(multi_scale_nat, "C:/Users/s436862/Dropbox/Climate Matching/4. Results/Grass groups AVH/Single scales/Nat 100km SRE DF.csv", row.names = F)

  multi_scale_nat_na <- ichao1
  write.csv(multi_scale_nat_na, "C:/Users/s436862/Dropbox/Climate Matching/4. Results/Grass groups AVH/Single scales/Nat 100km SRE DF contains NAs.csv", row.names = F)
  
# Poa
  multi_scale_poa <- ichao1_no_na
  write.csv(multi_scale_poa, "C:/Users/s436862/Dropbox/Climate Matching/4. Results/Grass groups AVH/Single scales/Poa 100km SRE DF.csv", row.names = F)
  
  multi_scale_poa <- ichao1
  write.csv(multi_scale_poa, "C:/Users/s436862/Dropbox/Climate Matching/4. Results/Grass groups AVH/Single scales/Poa 100km SRE DF contains NAs.csv", row.names = F)

# -------------- 200 km --------------------
  
# AVH
  multi_scale_avh <- ichao1_no_na
  write.csv(multi_scale_avh, "C:/Users/s436862/Dropbox/Climate Matching/4. Results/Grass groups AVH/Single scales/AVH 200km SRE DF.csv", row.names = F)
  
  multi_scale_avh <- ichao1
  write.csv(multi_scale_avh, "C:/Users/s436862/Dropbox/Climate Matching/4. Results/Grass groups AVH/Single scales/AVH 200km SRE DF contains NAs.csv", row.names = F)
  
# Introduced
  multi_scale_int <- ichao1_no_na
  write.csv(multi_scale_int, "C:/Users/s436862/Dropbox/Climate Matching/4. Results/Grass groups AVH/Single scales/Int 200km SRE DF.csv", row.names = F)
  
  multi_scale_int <- ichao1
  write.csv(multi_scale_int, "C:/Users/s436862/Dropbox/Climate Matching/4. Results/Grass groups AVH/Single scales/Int 200km SRE DF contains NAs.csv", row.names = F)
  
# Native 
  multi_scale_nat <- ichao1_no_na
  write.csv(multi_scale_nat, "C:/Users/s436862/Dropbox/Climate Matching/4. Results/Grass groups AVH/Single scales/Nat 200km SRE DF.csv", row.names = F)
  
  multi_scale_nat_na <- ichao1
  write.csv(multi_scale_nat_na, "C:/Users/s436862/Dropbox/Climate Matching/4. Results/Grass groups AVH/Single scales/Nat 200km SRE DF contains NAs.csv", row.names = F)
  
# Poa
  multi_scale_poa <- ichao1_no_na
  write.csv(multi_scale_poa, "C:/Users/s436862/Dropbox/Climate Matching/4. Results/Grass groups AVH/Single scales/Poa 200km SRE DF.csv", row.names = F)
  
  multi_scale_poa <- ichao1
  write.csv(multi_scale_poa, "C:/Users/s436862/Dropbox/Climate Matching/4. Results/Grass groups AVH/Single scales/Poa 200km SRE DF contains NAs.csv", row.names = F)
  