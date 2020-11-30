
# Species richness estimator equations (SREs)
  

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
  
  avh <- read.csv("AVH/AVH native records.csv", header = T) %>%
    dplyr::select(species, lat, long)
  
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
  xy <- cbind(p$long, p$lat)
  spp <- as.numeric(factor(p$species))
  
  
   
  
# ------------------------------

# 1 km Species Richness Estimations & Co.--------------------------------

# raster template
  raster <- raster("EFs/EFs cropped/arid.grd")
  
# SRE function for single width  
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
  
  rm(list=setdiff(ls(), "ichao1"))

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
  
  ichao1$lanum <- ifelse(ichao1$f3 == 0, ifelse(ichao1$f2 == 0, lanum_f1(ichao1), lanum_f2(ichao1)), lanum_f3(ichao1))

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
  total_cells <- length(ichao1$n)
  zero <- sum(ichao1$n == 0) / total_cells * 100
  ten <- sum(ichao1$n <= 10) / total_cells * 100
  hundred <- sum(ichao1$n <= 100) / total_cells * 100
  thousand <- sum(ichao1$n <= 1000) / total_cells * 100
  five_thousand <- sum(ichao1$n <= 5000) / total_cells * 100
  ten_thousand <- sum(ichao1$n <= 10000) / total_cells * 100 

# remove NAs   
  ichao1 <- ichao1[!is.na(ichao1$a), ]

# Output dataframe -------------------------
# raw SRE data (6)
  width <- 1
  n <- mean(ichao1$n)
  a <- mean(ichao1$a)
  f1 <- mean(ichao1$f1)
  f2 <- mean(ichao1$f2)
  f3 <- mean(ichao1$f3)
  f4 <- mean(ichao1$f4)

# SREs  (5)
  ichao1_sre <- mean(ichao1$ichao1)
  chao1_sre <- mean(ichao1$chao1)
  jack1st <- mean(ichao1$jack1st)
  jack2nd <- mean(ichao1$jack2nd)
  lanum_inf <- ichao1$lanum
  lanum_na <- ifelse(lanum_inf == "Inf", NA, lanum_inf)
  lanum <- mean(lanum_na, na.rm = T)

# SE + CIs(3)
  chiu_se <- mean(ichao1$chiu_se[ichao1$chiu_se!=0], na.rm = T)
  lower_ci <- mean(ichao1$lower_ci, na.rm = T)
  upper_ci <- mean(ichao1$upper_ci, na.rm = T)

# coverage  (2)
  chao_C <- mean(ichao1$chao1_C, na.rm = T)
  jost_C <- mean(ichao1$jost_C, na.rm = T)

# Pearson's correlation coefficients between record# and SRE (6)
  n_a_cor <- cor(ichao1$n, ichao1$a, method = "pearson") * 100
  n_ichao1_cor <- cor(ichao1$n, ichao1$ichao1, method = "pearson") * 100
  n_chao1_cor <- cor(ichao1$n, ichao1$chao1, method = "pearson") * 100
  n_jack1st_cor <- cor(ichao1$n, ichao1$jack1st, method = "pearson") * 100
  n_jack2nd_cor <- cor(ichao1$n, ichao1$jack2nd, method = "pearson") * 100
  n_lanum_cor <- cor(ichao1$n, ichao1$lanum, method = "pearson") * 100
  
# how many in out: 30        
  sre_1km <- as.data.frame(cbind(width, n, a, f1, f2, f3, f4,
                           # 6
                           ichao1_sre, chao1_sre, jack1st, jack2nd, lanum,
                           # 5
                           chiu_se, upper_ci, lower_ci,
                           # 3
                           chao_C, jost_C,  
                           # 2
                           n_a_cor, n_ichao1_cor, n_chao1_cor, n_jack1st_cor, n_jack2nd_cor, n_lanum_cor,
                           # 6
                           zero, ten, hundred, thousand, five_thousand, ten_thousand, total_cells))                                                                                             # 7
# total: 30 
   
# output ----------------------------
colnames(sre_1km) <- c("cell_width", "n", "a", "f1", "f2", "f3", "f4",
                   # 6
                   "ichao1", "chao1", "jack1st", "jack2nd", "lanum",
                   # 5
                   "chiu_se", "upper_ci", "lower_ci",
                   # 3
                   "chao1_C", "jost_C",  
                   # 2
                   "n_a_cor", "n_ichao1_cor", "n_chao1_cor", "n_jack1st_cor", "n_jack2nd_cor", "n_lanum_cor",
                   # 6
                   "zero", "ten", "hundred", "thousand", "five_thousand", "ten_thousand", "total_cells") 
                   #7
# total: 30 ; save
  
# AVH ----
# km1_scale_avh <- as.data.frame(sre_1km)
#   write.csv(km1_scale_avh, "C:/Users/s436862/Dropbox/Climate Matching/4. Results/Grass groups AVH/Multiscales/AVH SRE 1km DF.csv", row.names = F)

# Introduced ----
# km1_scale_int <- as.data.frame(sre_1km)
# write.csv(km1_scale_int, "C:/Users/s436862/Dropbox/Climate Matching/4. Results/Grass groups AVH/Multiscales/Int SRE 1km DF.csv", row.names = F)

# Native ----
# km1_scale_nat <- as.data.frame(sre_1km)
# write.csv(km1_scale_nat, "C:/Users/s436862/Dropbox/Climate Matching/4. Results/Grass groups AVH/Multiscales/Nat SRE 1km DF.csv", row.names = F)

# Poa ----
# km1_scale_poa <- as.data.frame(sre_1km)
# write.csv(km1_scale_poa, "C:/Users/s436862/Dropbox/Climate Matching/4. Results/Grass groups AVH/Multiscales/Poa SRE 1km DF.csv", row.names = F)

  

# 2-500 km Species Richness Estimations & Co.--------------------------------
  
# raster template
  b <- raster("EFs/EFs cropped/arid.grd")

# SRE function  
ras <- function(width) {

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

# exclude cells outside of Australia in dataframe
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

ichao1$lanum <- ifelse(ichao1$f3 == 0, ifelse(ichao1$f2 == 0, lanum_f1(ichao1), lanum_f2(ichao1)), lanum_f3(ichao1))
   
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
    total_cells <- length(ichao1$n)
    zero <- sum(ichao1$n == 0) / total_cells * 100
    ten <- sum(ichao1$n <= 10) / total_cells * 100
    hundred <- sum(ichao1$n <= 100) / total_cells * 100
    thousand <- sum(ichao1$n <= 1000) / total_cells * 100
    five_thousand <- sum(ichao1$n <= 5000) / total_cells * 100
    ten_thousand <- sum(ichao1$n <= 10000) / total_cells * 100 
    
    # remove NAs   
    ichao1 <- ichao1[!is.na(ichao1$a), ]
    
    # Output dataframe -------------------------
    # raw SRE data (6)
    n <- mean(ichao1$n)
    a <- mean(ichao1$a)
    f1 <- mean(ichao1$f1)
    f2 <- mean(ichao1$f2)
    f3 <- mean(ichao1$f3)
    f4 <- mean(ichao1$f4)
    
    # SREs  (5)
    ichao1_sre <- mean(ichao1$ichao1)
    chao1_sre <- mean(ichao1$chao1)
    jack1st <- mean(ichao1$jack1st)
    jack2nd <- mean(ichao1$jack2nd)
    lanum_inf <- ichao1$lanum
    lanum_na <- ifelse(lanum_inf == "Inf", NA, lanum_inf)
    lanum <- mean(lanum_na, na.rm = T)
    
    # SE + CIs(3)
    chiu_se <- mean(ichao1$chiu_se[ichao1$chiu_se!=0], na.rm = T)
    lower_ci <- mean(ichao1$lower_ci, na.rm = T)
    upper_ci <- mean(ichao1$upper_ci, na.rm = T)
    
    # coverage  (2)
    chao_C <- mean(ichao1$chao1_C, na.rm = T)
    jost_C <- mean(ichao1$jost_C, na.rm = T)
    
    # Pearson's correlation coefficients between record# and SRE (6)
    n_a_cor <- cor(ichao1$n, ichao1$a, method = "pearson") * 100
    n_ichao1_cor <- cor(ichao1$n, ichao1$ichao1, method = "pearson") * 100
    n_chao1_cor <- cor(ichao1$n, ichao1$chao1, method = "pearson") * 100
    n_jack1st_cor <- cor(ichao1$n, ichao1$jack1st, method = "pearson") * 100
    n_jack2nd_cor <- cor(ichao1$n, ichao1$jack2nd, method = "pearson") * 100
    n_lanum_cor <- cor(ichao1$n, ichao1$lanum, method = "pearson") * 100
    
    # how many in res: 30        
    res <- as.data.frame(cbind(n, a, f1, f2, f3, f4,
                               # 6
                               ichao1_sre, chao1_sre, jack1st, jack2nd, lanum,
                               # 5
                               chiu_se, upper_ci, lower_ci,
                               # 3
                               chao_C, jost_C,  
                               # 2
                               n_a_cor, n_ichao1_cor, n_chao1_cor, n_jack1st_cor, n_jack2nd_cor, n_lanum_cor,
                               # 6
                               zero, ten, hundred, thousand, five_thousand, ten_thousand, total_cells))                                                                                             # 7
    # total: 29 
    return(res)                                                                    
    
  }  #finish function
  
# output ----------------------------
# scale range (2 - 500 km)
  width <- 2 ^ seq(1, 9, 0.5)
  
  out <- matrix(nrow = length(width), ncol = 30) # res cols +1
  
  for(i in 1:length(width)) {
    out[i, 1] <- width[i]
    out[i, 2:30] <- as.numeric(ras(width[i]))
  }
  
  sre_mkm <- out

  colnames(sre_mkm) <- c("cell_width", "n", "a", "f1", "f2", "f3", "f4",
                     # 6 + 1
                     "ichao1", "chao1", "jack1st", "jack2nd", "lanum",
                     # 5
                     "chiu_se", "upper_ci", "lower_ci",
                     # 3
                     "chao1_C", "jost_C",  
                     # 2
                     "n_a_cor", "n_ichao1_cor", "n_chao1_cor", "n_jack1st_cor", "n_jack2nd_cor", "n_lanum_cor",
                     # 6
                     "zero", "ten", "hundred", "thousand", "five_thousand", "ten_thousand", "total_cells")   
  # total: 29 + 1

  
# --------------------------------  
# Saving various bits and pieces ------------------------------------- 
  
# # AVH ----
#   km1_sre_AVH <- read.csv("C:/Users/s436862/Dropbox/Climate Matching/4. Results/Grass groups AVH/Multiscales/AVH SRE 1km DF.csv", header = T)
#   multi_scale_AVH <- as.data.frame(rbind(km1_sre_AVH, sre))
#   write.csv(multi_scale_AVH, "C:/Users/s436862/Dropbox/Climate Matching/4. Results/Grass groups AVH/Multiscales/AVH SRE DF.csv", row.names = F)
#  
# # Introduced ----
#   km1_sre_int <- read.csv("C:/Users/s436862/Dropbox/Climate Matching/4. Results/Grass groups AVH/Multiscales/Int SRE 1km DF.csv", header = T)
#   multi_scale_int <- as.data.frame(rbind(km1_sre_int, sre))
#   write.csv(multi_scale_int, "C:/Users/s436862/Dropbox/Climate Matching/4. Results/Grass groups AVH/Multiscales/Int SRE DF.csv", row.names = F)
#     
# # Native ----
#   km1_sre_nat <- read.csv("C:/Users/s436862/Dropbox/Climate Matching/4. Results/Grass groups AVH/Multiscales/Nat SRE 1km DF.csv", header = T)
#   multi_scale_nat <- as.data.frame(rbind(km1_sre_nat, sre))
#   write.csv(multi_scale_nat, "C:/Users/s436862/Dropbox/Climate Matching/4. Results/Grass groups AVH/Multiscales/Nat SRE DF.csv", row.names = F)
#    
# # Poa ----
#   km1_sre_poa <- read.csv("C:/Users/s436862/Dropbox/Climate Matching/4. Results/Grass groups AVH/Multiscales/Nat SRE 1km DF.csv", header = T)
#   multi_scale_poa <- as.data.frame(rbind(km1_sre_poa, sre))
#   write.csv(multi_scale_poa, "C:/Users/s436862/Dropbox/Climate Matching/4. Results/Grass groups AVH/Multiscales/Poa SRE DF.csv", row.names = F)
#   
#   
#   
#   
#   
#   
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  