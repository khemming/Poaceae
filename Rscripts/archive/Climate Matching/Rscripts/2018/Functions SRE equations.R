

# Functions for all AVH paper EDA + 100 km SRE calculations
# Not for plotting them; that's in its own thing
  

# Notes ----------------------
# setting up functions so they need a central dataframe
# e.g. x$f1, not f1
# this makes checking them harder
# but makes them more useful if working with dataframe

# Don't call 'x' ichao, chao, or any other SRE 

# assume you have a mix of these loaded ---------------------
  library(rasterVis)
  library(ggplot2)
  library(ggmap)
  library(tidyr)
  library(raster)
  library(rgdal)
  library(maptools)
  library(dplyr)
  

  setwd("C:/Users/s436862/Dropbox/Climate Matching/1. Data files/")

# Rasterizing -------------------------
# need a raster of Australia (or working location), have outline here too
  b <- raster("EFs/EFs cropped/arid.grd")
  oz <- borders("world", region = "Australia")
# for aggregating scale  
  width <- 100
# aggregate original raster
  raster <- aggregate(b, fact = width, fun = mean)
# baseline species richness (a)
  a <- rasterize(xy, raster, field = spp, fun = function(x,...) {length(unique(na.omit(x))) })
# number of records (n)
  n <- rasterize(xy, raster, fun = function(x,...) {length(na.omit(x)) })
# singletons etc.; henceforth f1, f2, f3 and f4 
  f1 <- rasterize(xy, raster, field = spp, fun = function(x,...) {length(which(table(x)==1)) })
  f2 <- rasterize(xy, raster, field = spp, fun = function(x,...) {length(which(table(x)==2)) })
  f3 <- rasterize(xy, raster, field = spp, fun = function(x,...) {length(which(table(x)==3)) })
  f4 <- rasterize(xy, raster, field = spp, fun = function(x,...) {length(which(table(x)==4)) })

# Making dataframe --------------------
  a <- getValues(a)
  n <- getValues(n)  
  f1 <- getValues(f1)
  f2 <- getValues(f2)
  f3 <- getValues(f3)
  f4 <- getValues(f4)
  
# Reference for NA removal of oceanic cells
  b <- getValues(raster)
  
# exclude extra-Aussie cells (NAs)
  x <- data.frame(b, a, n, f1, f2, f3, f4)
  x <- x[!is.na(b), ] 

# We now have the dataframe with all the necessary SRE components
  
# Chao1 estimator (Chao, 1984; ...) --------------------------------
# f2 bias-correction included  
  chao1 <- function(x) {
    ifelse(x$f2 == 0, 
           (x$a + (((x$n-1) / x$n) * (x$f1 * (x$f1-1)) / (2 * (x$f2+1)))),
           (x$a + (((x$n-1) / x$n) * ((x$f1^2)/(2*x$f2))))
    )
  } # finish function
  
  x$chao1 <- chao1(x)
  
# Chao1 iChao1 equations (Chiu et al. 2014) -----------------------------------------------    
# The iChao1 estimator uses f1, f2, f3, and f4. And because not all cells have these, there are a few (more) bias-corrections
# Therefore, this section will look like this:
# (a) ifelse(f2 ==0,
#            chao1_f1,
#        (b) ifelse(f3 == 0,
#                   chao1_f2,
#               (c) ifelse(f4 == 0,
#                          ichao1_f3,
#                     (d)  ichao1_f4)))
  
# (a) chao_f1; if there are no f2's, do the f1-only Chao1 equation
# (b) chao_f2; if there are no f3's, do the f2 Chao1 equation
# (c) ichao_f3; if there are no f4's, do the f3-only iChao1 equation
# (d) ichao_f4; if there are f4's, do the f4 iChao1 equation
  
# (a) chao_f1
  chao1_f1 <- function(x) {
    x$a + (((x$n-1) / x$n) * (x$f1 * (x$f1-1)) / (2 * (x$f2+1)))
  } # finish function
  
# (b) chao_f2
  chao1_f2 <- function(x) {
    x$a + (((x$n-1) / x$n) * ((x$f1^2)/(2*x$f2)))
  } # finish function  
  
# (c) chao_f3
  ichao1_f3 <- function(x) {
    (x$a + (((x$n-1) / x$n) * ((x$f1^2)/(2*x$f2)))) + 
      ((((x$n-3) / (4*x$n)) * (x$f3/(x$f4+1))) *
         (ifelse( (x$f1 - ((x$n-3) / (2*(x$n-1))) * (x$f2*x$f3/(x$f4+1))) > 0, 
                  (x$f1 - ((x$n-3) / (2*(x$n-1))) * (x$f2*x$f3/(x$f4+1))), 0)))
  } # finish function
  
# (d) chao_f4
  ichao1_f4 <- function(x) {
    
    (x$a + (((x$n-1) / x$n) * ((x$f1^2)/(2*x$f2)))) + 
      ((((x$n-3) / (4*x$n)) * (x$f3/x$f4)) *
         (ifelse( (x$f1 - ((x$n-3) / (2*(x$n-1))) * (x$f2*x$f3/x$f4)) > 0, 
                  (x$f1 - ((x$n-3) / (2*(x$n-1))) * (x$f2*x$f3/x$f4)), 0 )))
    
  } # finish function
  
# (a-d) 
  ichao1 <- function(x) {
    ifelse(x$f2 == 0,
           chao1_f1(x),
           ifelse(x$f3 == 0,
                  chao1_f2(x),
                  ifelse(x$f4 == 0,
                         ichao1_f3(x),
                         ichao1_f4(x)
                  )))
  } # finish function
  
  x$ichao1 <- ichao1(x)  
  

# Chao1 var, SE, CIs, coverages (Chiu et al. 2014) ----------------------------------------------
# Not the same as the one derived from Chapter 4.
# Chao1 variance f2 bias-correction
  chao1_var_f2 <-  function(x) {
    x$f2 * 
      ( (((0.25 * (((x$n-1)/x$n)^2)) * ((x$f1/x$f2)^4))) + 
          ((((x$n-1)/x$n)^2) * ((x$f1/x$f2)^3)) + 
          (0.5 * ((x$n-1)/x$n) * ((x$f1/x$f2)^2)) )
  } # finish function
  
# chao1 variance f1 
  chao1_var_f1 <- function(x) {
    (0.25 * (((x$n-1)^2)/x$n^2) * x$f1 * ((2*x$f1-1)^2)) + 
      ( (0.5 * x$f1 * (x$f1-1)) - 
          (0.25 * (x$f1^4)/(x$a + (((x$n-1)/x$n) * (x$f1*(x$f1-1))/(2*(x$f2+1))))) )  
  } # finish function
  
# Together  
  chao1_var <-  ifelse(x$f2 == 0,
                      chao1_var_f1(x),
                      chao1_var_f2(x)
  )
  
  # standard error
  x$chao1_se <- sqrt(chao1_var) / sqrt(x$n)
  
# Confidence intervals (chao1 et al. 2014) 
# (1) f2 +2-sided 95% CI; (2) f1 +1-sided 95%CI

# (1) f2
# R 
  R_f2 <- exp(1.96*(1 + (chao1_var_f2(x$chao1) / (x$chao1 - x$a)^2))^0.5)
# CIs
  lower_ci_f2 <- x$a + (x$chao1 - x$a) / R_f2
  upper_ci_f2 <- x$a + (x$chao1 - x$a) * R_f2
  
# (2) f1
  # R f1
  R_f1 <- exp(1.64*(1 + (chao1_var_f1(x$chao1) / (x$chao1 - x$a)^2))^0.5)
  
# CI f1
# note: no_dbl_lower_ci has no_dblton variance in it; whereas Chiu's has the ys_dblton 
  lower_ci_f1 <- x$a + (x$chao1 - x$a) / R_f1 
  # note: only supposed to be one CI of these for this
  
# I want to have upper + lower CIs when f2 > 0; lower no dbltons, ifelse
# reasons: Chiu says it's not good when sample-size low; this seems a good way define that (thus using var_f1 instead of _f2)
  x$lower_ci <- ifelse(x$f2 == 0, lower_ci_f1, lower_ci_f2) 
  x$upper_ci <- ifelse(x$f2 == 0, 0, upper_ci_f2) 
  
# Jackknife (Chapter 4.; Jackknife: A review (not sure of Author)) ----------------------------------------
  jack1st <- function(x) {
    x$a + x$f1
  } # finish function
  x$jackk1st <- jack1st(x)
  
  jack2nd <- function(x) {
    x$a + 2*x$f1 - x$f2
  } # finish function
  x$jackk2nd <- jack2nd(x)
  
# Lanumteang & Bohning (2011) ----------------------------------------
# f2  
  lanum_f2 <- function(x) {
    x$a + (3*(x$f1^3)*x$f3)/(4*(x$f2^3))
  } # finish function
  
#  f1 bias-corrected form 
  lanum_f1 <- function(x) {
    x$a + (3/4 * (x$f1*(x$f1-1)*(x$f1-2)*x$f3) / ((x$f2 + 1) * (x$f2 + 2) * (x$f2 + 3)))
  } # finish function
  x$lanum <- ifelse(x$f2 == 0, lanum_f1(x), lanum_f2(x)) 

# C ------------------------------------
# 27/11: adding C from Chao Jost 2012; note: there is a dbl_bias version in Chiu 2014. Weird, but cool.
  x$jost_C <- ifelse(x$f2 > 0,
                          1 - ((x$f1/x$n) * (((x$n-1)*x$f1) / (((x$n-1)*x$f1) + 2*x$f2))), 
                          1 - ((x$f1/x$n) * ((x$n-1)*(x$f1-1) / (((x$n-1)*(x$f1-1)) + 2))) )
  
# ratio: now (29/11) for Chao1 (not iChao) 
  x$chao1_C <- x$a / x$chao1
  

  
 