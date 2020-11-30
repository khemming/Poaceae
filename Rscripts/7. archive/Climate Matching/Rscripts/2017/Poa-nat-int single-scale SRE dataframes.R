# Species richness estimator equations (SREs) -----------------------------

# SAME FORMULAE AS IN MULTISCALE

# mainly iChao1 (Chiu et al. 2014), but included a few other indices
# have done this for multiple data sets.
# for plots, go to apply named 'plots' script



library(ggplot2)
library(ggmap)
library(tidyr)
library(raster)
library(rgdal)
library(maptools)
library(dplyr)


setwd("C:/Users/s436862/Dropbox/Climate Matching/1. Data files/")


rm(list = ls())


#  Data --------------------------------------------------------------------

#  Poa --------------------------------------------------------------------
  poa <- read.csv("AVH/AVH grass records.csv", header = T) %>%
    dplyr::select(species, lat, long)
  
  p <- poa
  xy <- cbind(p$long, p$lat)
  spp <- as.numeric(factor(p$species))

#  Nat --------------------------------------------------------------------
nat <- read.csv("AVH/AVH grass records.csv", header = T) %>%
  dplyr::filter(status == "native") %>%
  dplyr::select(species, lat, long)

xy <- cbind(nat$long, nat$lat)
spp <- as.numeric(factor(nat$species))

#  Int --------------------------------------------------------------------
int <- read.csv("AVH/AVH grass records.csv", header = T) %>%
  dplyr::filter(status == "introduced") %>%
  dplyr::select(species, lat, long)

xy <- cbind(int$long, int$lat)
spp <- as.numeric(factor(int$species))


#  AVH --------------------------------------------------------------------
avh <- read.csv("AVH/AVH native records.csv", header = T) %>%
  dplyr::select(species, lat, long)

xy <- cbind(avh$long, avh$lat)
spp <- as.numeric(factor(avh$species))


# width -------------------------------------------------------------------

# raster template
  b <- raster("EFs/EFs cropped/arid.grd")

  #width <- 50
  
  #width <- 100
  
  width <- 150
#
  
# Species richness estimations --------------------------------------------

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
  
# iChao1 equations (Chiu et al. 2014) -----------------------------------------------    
  
# this section will look like this:
  # define each equation as a function (e.g. 'chao1'), then put each into an all-in-one that has below's structure
  # (a) ifelse(f2 ==0,
  #            chao1_no_dbl,
  #        (b) ifelse(f3 == 0,
  #                   chao1_w_dbl,
  #               (c) ifelse(f4 == 0,
  #                          ichao1_no_f4,
  #                          ichao1_w_f4)))
  
  # first, calculate the 'old' chao1 estimator (henceforth called 'chao1')
  chao1_estimator <- function(x) {
    
    ifelse(ichao1$f2 == 0, 
           (ichao1$a + (((ichao1$n-1) / ichao1$n) * (ichao1$f1 * (ichao1$f1-1)) / (2 * (ichao1$f2+1)))),
           (ichao1$a + (((ichao1$n-1) / ichao1$n) * ((ichao1$f1^2)/(2*ichao1$f2))))
    )
    
  } # finish function
  
  ichao1$chao1 <- chao1_estimator(ichao1)
  
# chao1 when no doubletons   
  chao1_no_f2 <- function(x) {
    ichao1$a + (((ichao1$n-1) / ichao1$n) * (ichao1$f1 * (ichao1$f1-1)) / (2 * (ichao1$f2+1)))
  } # finish function
  
# chao1 with doubltons
  chao1_w_f2 <- function(x) {
    ichao1$a + (((ichao1$n-1) / ichao1$n) * ((ichao1$f1^2)/(2*ichao1$f2)))
  } # finish function  
  
# ichao1 when no quadrupletons (f4's)
  ichao1_no_f4 <- function(x) {
    
    (ichao1$a + (((ichao1$n-1) / ichao1$n) * ((ichao1$f1^2)/(2*ichao1$f2)))) + 
      ((((ichao1$n-3) / (4*ichao1$n)) * (ichao1$f3/(ichao1$f4+1))) *
         (ifelse( (ichao1$f1 - ((ichao1$n-3) / (2*(ichao1$n-1))) * (ichao1$f2*ichao1$f3/(ichao1$f4+1))) > 0, 
                  (ichao1$f1 - ((ichao1$n-3) / (2*(ichao1$n-1))) * (ichao1$f2*ichao1$f3/(ichao1$f4+1))), 0 )))
  } # finish function
  
# ichao1 with quadrupletons (f4's)
  ichao1_w_f4 <- function(x) {
    
    (ichao1$a + (((ichao1$n-1) / ichao1$n) * ((ichao1$f1^2)/(2*ichao1$f2)))) + 
      ((((ichao1$n-3) / (4*ichao1$n)) * (ichao1$f3/ichao1$f4)) *
         (ifelse( (ichao1$f1 - ((ichao1$n-3) / (2*(ichao1$n-1))) * (ichao1$f2*ichao1$f3/ichao1$f4)) > 0, 
                  (ichao1$f1 - ((ichao1$n-3) / (2*(ichao1$n-1))) * (ichao1$f2*ichao1$f3/ichao1$f4)), 0 )))
    
  } # finish function
  
# complete ichao1 estimation   
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
  
  
# Chao1 variances, sd, etc. ----------------------------------------------
  
  # (from Chiu et al. 2014 -- NOT the same as the one derived from Chapter 4.)
  # note: iChao1 variance equation is nasty -- not doing it
  
# chao1 variance with doubletons
# (from Chiu et al. -- has (n-1)/n correction) 
  chao1_chiu_var_w_dbl <-  function(x) {
    ichao1$f2 * 
      ( (((0.25 * (((ichao1$n-1)/ichao1$n)^2)) * ((ichao1$f1/ichao1$f2)^4))) + 
          ((((ichao1$n-1)/ichao1$n)^2) * ((ichao1$f1/ichao1$f2)^3)) + 
          (0.5 * ((ichao1$n-1)/ichao1$n) * ((ichao1$f1/ichao1$f2)^2)) )
  } # finish function
  
# chao1 variance no doubletons 
  chao1_chiu_var_no_dbl <- function(x) {
    (0.25 * (((ichao1$n-1)^2)/ichao1$n^2) * ichao1$f1 * ((2*ichao1$f1-1)^2)) + 
      ( (0.5 * ichao1$f1 * (ichao1$f1-1)) - 
          (0.25 * (ichao1$f1^4)/(ichao1$a + (((ichao1$n-1)/ichao1$n) * (ichao1$f1*(ichao1$f1-1))/(2*(ichao1$f2+1))))) )  
  } # finish function
  
# sd & var in one calc  
  ichao1$chao1_chiu_sd <-  sqrt(ifelse(ichao1$f2 == 0,
                                   chao1_chiu_var_no_dbl(ichao1),
                                   chao1_chiu_var_w_dbl(ichao1)
  ))
  
  
# sampling effort -- records per km^2
  ichao1$survey_effort <- ichao1$n / (width^2)  
  
# remove NAs   
  ichao1 <- ichao1[!is.na(ichao1$a), ]
  
# ratio  
  ichao1$ichao1_actual_ratio <- ichao1$a / ichao1$ichao1
  
# mean intensity in records km-2 (SD next to it)
# only be good for 50, 100, and 150 scales. And even then, might not chane a whole lot.  
# Soberon showed it to get bigger with lower resolution. 
  mean_intensity <- ichao1$n / (width^2) # right?? 
# and then plot against C -- C being ratio of a/ichao
  
# and then put like 4 scales on the same graph. 
# identifies individual points that need addressing
# Alice springs here we come ...
  
# Aslo: 
# The plot also displays at what resolutions intensity values are too small as to allow any meaningful
# querying of the database.
  
  
# if this does anything  
  ichao1 <- as.data.frame(ichao1)
  

  
# Plotting up various bits and pieces ------------------------------------- 

# csv's 
# -> it would just be heaps easier saving this into 3 / 4 csv's
# but, it is Friday arvo, adn my brain capacity is about room temperature 
# and no the AC is not really pumping the hot stuff
  
# Poa
write.csv(ichao1, "C:/Users/s436862/Dropbox/Climate Matching/4. Results/Poa/Single scales/50 km/ichao1 POA.csv", row.names = F)
write.csv(ichao1, "C:/Users/s436862/Dropbox/Climate Matching/4. Results/Poa/Single scales/100 km/ichao1 POA.csv", row.names = F)
write.csv(ichao1, "C:/Users/s436862/Dropbox/Climate Matching/4. Results/Poa/Single scales/150 km/ichao1 POA.csv", row.names = F)

# Native
write.csv(ichao1, "C:/Users/s436862/Dropbox/Climate Matching/4. Results/Poa/Single scales/50 km/ichao1 NAT.csv", row.names = F)
write.csv(ichao1, "C:/Users/s436862/Dropbox/Climate Matching/4. Results/Poa/Single scales/100 km/ichao1 NAT.csv", row.names = F)
write.csv(ichao1, "C:/Users/s436862/Dropbox/Climate Matching/4. Results/Poa/Single scales/150 km/ichao1 NAT.csv", row.names = F)

# Introduced
write.csv(ichao1, "C:/Users/s436862/Dropbox/Climate Matching/4. Results/Poa/Single scales/50 km/ichao1 INT.csv", row.names = F)
write.csv(ichao1, "C:/Users/s436862/Dropbox/Climate Matching/4. Results/Poa/Single scales/100 km/ichao1 INT.csv", row.names = F)
write.csv(ichao1, "C:/Users/s436862/Dropbox/Climate Matching/4. Results/Poa/Single scales/150 km/ichao1 INT.csv", row.names = F)

# AVH
write.csv(ichao1, "C:/Users/s436862/Dropbox/Climate Matching/4. Results/Poa/Single scales/50 km/ichao1 AVH.csv", row.names = F)
write.csv(ichao1, "C:/Users/s436862/Dropbox/Climate Matching/4. Results/Poa/Single scales/100 km/ichao1 AVH.csv", row.names = F)
write.csv(ichao1, "C:/Users/s436862/Dropbox/Climate Matching/4. Results/Poa/Single scales/150 km/ichao1 AVH.csv", row.names = F)
