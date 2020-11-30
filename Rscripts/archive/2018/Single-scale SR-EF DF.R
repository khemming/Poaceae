
# Single-scale species richness-enivronmental factor dfs for all AVH grass groups ------

# Built on script: 'Single-scale SRE Dfs.R'

# See metadata for more info

# only done 100 km Nat and Int calculations so far -- use 100km as template

  library(ggplot2)
  library(ggmap)
  library(tidyr)
  library(raster)
  library(rgdal)
  library(maptools)
  library(dplyr)
  library(car)
  
  
  setwd("C:/Users/s436862/Dropbox/Climate Matching/1. Data files/")
  
# ---------------------------------
  
# 50 km -------------------------   
# AVH
  rm(list = ls())
  avh_50 <- read.csv("C:/Users/s436862/Dropbox/Climate Matching/4. Results/Grass groups AVH/Single scales/AVH 50km SRE DF contains NAs.csv")
  spp <- avh_50
  
# Introduced
  rm(list = ls())
  int_50 <- read.csv("C:/Users/s436862/Dropbox/Climate Matching/4. Results/Grass groups AVH/Single scales/Int 50km SRE DF contains NAs.csv")
  spp <- int_50

# Native 
  rm(list = ls())
  nat_50 <- read.csv("C:/Users/s436862/Dropbox/Climate Matching/4. Results/Grass groups AVH/Single scales/Nat 50km SRE DF contains NAs.csv")
  spp <- nat_50

# Poa
  rm(list = ls())
  poa_50 <- read.csv("C:/Users/s436862/Dropbox/Climate Matching/4. Results/Grass groups AVH/Single scales/Poa 50km SRE DF contains NAs.csv")
  spp <- poa_50
  
  
# SR- EFs ------------------------------------   
# EF rasters  
  arid <- raster("EFs/EFs cropped/arid.grd") 
  ap <- raster("EFs/EFs cropped/ap.grd") 
  cm <- raster("EFs/EFs cropped/cm.grd")
  cq <- raster("EFs/EFs cropped/cq.grd")
  rz <- raster("EFs/EFs cropped/rz.grd")
  sp <- raster("EFs/EFs cropped/sp.grd")
  st <- raster("EFs/EFs cropped/st.grd")
  elev <- raster("EFs/EFs cropped/elev.grd")
  evap <- raster("EFs/EFs cropped/evap.grd")
  hii <- raster("EFs/EFs cropped/hii.grd")
  mat <- raster("EFs/EFs cropped/mat.grd")
  ps <- raster("EFs/EFs cropped/ps.grd")
  wm <- raster("EFs/EFs cropped/wm.grd")
  wq <- raster("EFs/EFs cropped/wq.grd")
  glu <- raster("Efs/EFs cropped/glu.grd")
  pawc <- raster("EFs/EFs cropped/pawc.grd")
  pewc <- raster("EFs/EFs cropped/pewc.grd")
  
  ef.stack <- stack(ap, arid, cm, cq, elev,
                    evap, hii, mat, ps, 
                    rz, sp, st, wm, wq, pawc, pewc)
  names(ef.stack) <- c("ap", "arid", "cm", "cq", "elev", "evap", "hii", "mat", "ps", "rz", "sp", "st", "wm", "wq", "pawc", "pewc")
  names(glu) <- ("glu")  

# aggregate (note: land use is categorical, requires modal agg. function)  
  ef.ag <- aggregate(ef.stack, fact = 50, fun = mean)
  

# extra calculations
# (1) topographic hetero -- calculated as sd
  th <- aggregate(elev, factor)
  
# (2) Global land use  
  glu.ag <- aggregate(glu, fact = 50, fun = modal)
  
# (3) PEWC  
  pewc_mode <- aggregate(pewc, fact = 50, fun = modal)
  plot(pewc_mode)
  pewc_mean <- aggregate(pewc, fact = 50, fun = mean)
  plot(pewc_mean)
  # not very different at all
  
# combine   
  ef <- stack(glu.ag, ef.ag, th, tb)

# get values + make dataframe
  ef_v_na <- as.data.frame(getValues(ef))

# make Spp_km same length as EF_v
  spp$n[is.na(spp$n)] <- 0 
  sapply(spp, function(x) sum(is.na(x)))
  
  arid_v <- getValues(ef$arid)
  ef_v <- ef_v_na[!is.na(arid_v), ] 
  
# Now both ef_v & AVH dfs same length, we merge
  spp_ef <- data.frame(spp, ef_v)
  # (Just saying, I am a genius God)
  
# make glu discrete factor
  spp_ef$glu <- factor(spp_ef$glu, levels = c(1, 2, 3, 5, 6, 7), labels = c("agriculture", "forest", "grassland", "urban", "arid", "water")) 

# save --------------------------------------------------
  
# AVH  
  #avh_ef_50 <- spp_ef
  write.csv(avh_ef_50, file = "C:/Users/s436862/Dropbox/Climate Matching/4. Results/Grass groups AVH/Single scales/AVH 50km SR-EFs DF.csv", row.names = F)
  
# Introduced
  #int_ef_50 <- spp_ef
  write.csv(int_ef_50, file = "C:/Users/s436862/Dropbox/Climate Matching/4. Results/Grass groups AVH/Single scales/Int 50km SR-EFs DF.csv", row.names = F)
  
# Native  
  #nat_ef_50 <- spp_ef
  write.csv(nat_ef_50, file = "C:/Users/s436862/Dropbox/Climate Matching/4. Results/Grass groups AVH/Single scales/Nat 50km SR-EFs DF.csv", row.names = F)

# Poa  
  #poa_ef_50 <- spp_ef
  write.csv(poa_ef_50, file = "C:/Users/s436862/Dropbox/Climate Matching/4. Results/Grass groups AVH/Single scales/Poa 50km SR-EFs DF.csv", row.names = F)
  

  
# ---------------------------------
  
# 100 km -------------------------   
  # AVH
  rm(list = ls())
  avh_100 <- read.csv("C:/Users/s436862/Dropbox/Climate Matching/4. Results/Grass groups AVH/Single scales/AVH 100km SRE DF contains NAs.csv")
  spp <- avh_100
  
  # Introduced
  rm(list = ls())
  int_100 <- read.csv("C:/Users/s436862/Dropbox/Climate Matching/4. Results/Grass groups AVH/Single scales/Int 100km SRE DF contains NAs.csv")
  spp <- int_100
  
  # Native 
  rm(list = ls())
  nat_100 <- read.csv("C:/Users/s436862/Dropbox/Climate Matching/4. Results/Grass groups AVH/Single scales/Nat 100km SRE DF contains NAs.csv")
  spp <- nat_100
  
  # Poa
  rm(list = ls())
  poa_100 <- read.csv("C:/Users/s436862/Dropbox/Climate Matching/4. Results/Grass groups AVH/Single scales/Poa 100km SRE DF contains NAs.csv")
  spp <- poa_100
  
  
# SR- EFs ------------------------------------   
# EF rasters  
  arid <- raster("EFs/EFs cropped/arid.grd") 
  ap <- raster("EFs/EFs cropped/ap.grd") 
  cm <- raster("EFs/EFs cropped/cm.grd")
  cq <- raster("EFs/EFs cropped/cq.grd")
  rz <- raster("EFs/EFs cropped/rz.grd")
  sp <- raster("EFs/EFs cropped/sp.grd")
  st <- raster("EFs/EFs cropped/st.grd")
  elev <- raster("EFs/EFs cropped/elev.grd")
  evap <- raster("EFs/EFs cropped/evap.grd")
  hii <- raster("EFs/EFs cropped/hii.grd")
  mat <- raster("EFs/EFs cropped/mat.grd")
  ps <- raster("EFs/EFs cropped/ps.grd")
  wm <- raster("EFs/EFs cropped/wm.grd")
  wq <- raster("EFs/EFs cropped/wq.grd")
  glu <- raster("Efs/EFs cropped/glu.grd")
  pawc <- raster("EFs/EFs cropped/pawc.grd")
  pewc <- raster("EFs/EFs cropped/pewc.grd")
  
  ef.stack <- stack(ap, arid, cm, cq, elev,
                    evap, mat, ps, 
                    rz, sp, st, wm, wq, pawc, pewc)
# aggregate (note: land use is categorical, requires modal agg. function)  
  ef.ag <- aggregate(ef.stack, fact = 100, fun = mean)
  glu.ag <- aggregate(glu, fact = 100, fun = modal)
  
# HII median values
  hii_ag <- aggregate(hii, fact = 100, fun = median)
  
# topographic hetero -- calculated as the SD of the mean of each cell
  th <- aggregate(elev, fact = 100, fun = sd)
 
# combine & name  
  ef <- stack(glu.ag, hii_ag, ef.ag, th)  
  names(ef) <- c("glu", "hii", "ap", "arid","cm", "cq", "elev", "evap", "mat", "ps", "rz", "sp", "st", "wm", "wq", "pawc", "pewc", "th")

# get values + make dataframe
  ef_v_na <- as.data.frame(getValues(ef))
  # why are these are all NAs, but the below scripts work?
  
# make Spp_km same length as EF_v
  spp$n[is.na(spp$n)] <- 0 
  sapply(spp, function(x) sum(is.na(x)))
  
  arid <- getValues(ef$arid)
  ef_v <- ef_v_na[!is.na(arid), ] 
  
# Now both ef_v & AVH dfs same length, we merge
  spp_ef <- data.frame(spp, ef_v)
  
# make glu discrete factor
  spp_ef$glu <- factor(spp_ef$glu, levels = c(1, 2, 3, 5, 6, 7), labels = c("agriculture", "forest", "grassland", "urban", "arid", "water")) 
  
# save --------------------------------------------------
  
  # AVH  
  #avh_ef_100 <- spp_ef
  write.csv(avh_ef_100, file = "C:/Users/s436862/Dropbox/Climate Matching/4. Results/Grass groups AVH/100 km scale/AVH SR-EFs DF.csv", row.names = F)
  
  # Introduced
  #int_ef_100 <- spp_ef
  write.csv(int_ef_100, file = "C:/Users/s436862/Dropbox/Climate Matching/4. Results/Grass groups AVH/100 km scale/Int SR-EFs DF.csv", row.names = F)
  
  # Native  
  #nat_ef_100 <- spp_ef
  write.csv(nat_ef_100, file = "C:/Users/s436862/Dropbox/Climate Matching/4. Results/Grass groups AVH/100 km scale/Nat SR-EFs DF.csv", row.names = F)
  
  # Poa  
  #poa_ef_100 <- spp_ef
  write.csv(poa_ef_100, file = "C:/Users/s436862/Dropbox/Climate Matching/4. Results/Grass groups AVH/100 km scale/Poa SR-EFs DF.csv", row.names = F)
  
  
# ---------------------------------
  
# 200 km -------------------------   
  # AVH
  rm(list = ls())
  avh_200 <- read.csv("C:/Users/s436862/Dropbox/Climate Matching/4. Results/Grass groups AVH/Single scales/AVH 200km SRE DF contains NAs.csv")
  spp <- avh_200
  
  # Introduced
  rm(list = ls())
  int_200 <- read.csv("C:/Users/s436862/Dropbox/Climate Matching/4. Results/Grass groups AVH/Single scales/Int 200km SRE DF contains NAs.csv")
  spp <- int_200
  
  # Native 
  rm(list = ls())
  nat_200 <- read.csv("C:/Users/s436862/Dropbox/Climate Matching/4. Results/Grass groups AVH/Single scales/Nat 200km SRE DF contains NAs.csv")
  spp <- nat_200
  
  # Poa
  rm(list = ls())
  poa_200 <- read.csv("C:/Users/s436862/Dropbox/Climate Matching/4. Results/Grass groups AVH/Single scales/Poa 200km SRE DF contains NAs.csv")
  spp <- poa_200
  
  
# SR- EFs ------------------------------------   
  arid <- raster("EFs/EFs cropped/arid.grd") 
  ap <- raster("EFs/EFs cropped/ap.grd") 
  cm <- raster("EFs/EFs cropped/cm.grd")
  cq <- raster("EFs/EFs cropped/cq.grd")
  rz <- raster("EFs/EFs cropped/rz.grd")
  sp <- raster("EFs/EFs cropped/sp.grd")
  st <- raster("EFs/EFs cropped/st.grd")
  elev <- raster("EFs/EFs cropped/elev.grd")
  evap <- raster("EFs/EFs cropped/evap.grd")
  hii <- raster("EFs/EFs cropped/hii.grd")
  mat <- raster("EFs/EFs cropped/mat.grd")
  ps <- raster("EFs/EFs cropped/ps.grd")
  wm <- raster("EFs/EFs cropped/wm.grd")
  wq <- raster("EFs/EFs cropped/wq.grd")
  glu <- raster("Efs/EFs cropped/glu.grd")
  pawc <- raster("EFs/EFs cropped/pawc.grd")
  pewc <- raster("EFs/EFs cropped/pewc.grd")
  
  ef.stack <- stack(ap, arid, cm, cq, elev,
                    evap, mat, ps, 
                    rz, sp, st, wm, wq, pawc, pewc)
  # aggregate (note: land use is categorical, requires modal agg. function)  
  ef.ag <- aggregate(ef.stack, fact = 200, fun = mean)
  glu.ag <- aggregate(glu, fact = 200, fun = modal)
  # HII median values
  hii_ag <- aggregate(hii, fact = 200, fun = median)
  
  # topographic hetero -- calculated as range (max - min; 'th') (forgot refs, but there are a few) & 300 m bands ('tb'; Kreft & Joltz, 2013) 
  elev_min <- aggregate(elev, fact = 200, fun = min)
  elev_max <- aggregate(elev, fact = 200, fun = max)
  elev_range <- elev_max - elev_min
  th <- elev_range
  tb <- elev_range/300
  
  # combine & name  
  ef <- stack(glu.ag, hii_ag, ef.ag, th, tb)  
  names(ef) <- c("glu", "hii", "ap", "arid","cm", "cq", "elev", "evap", "mat", "ps", "rz", "sp", "st", "wm", "wq", "pawc", "pewc", "th", "tb")
  
  # combine   
  ef <- stack(glu.ag, hii_ag, ef.ag, th, tb)
  
  # get values + make dataframe
  ef_v_na <- as.data.frame(getValues(ef))
  
  # make Spp_km same length as EF_v
  spp$n[is.na(spp$n)] <- 0 
  sapply(spp, function(x) sum(is.na(x)))
  
  arid <- getValues(ef$arid)
  ef_v <- ef_v_na[!is.na(arid), ] 
  
  # Now both ef_v & AVH dfs same length, we merge
  spp_ef <- data.frame(spp, ef_v)
  # (Just saying, I am a genius God)
  
  # make glu discrete factor
  spp_ef$glu <- factor(spp_ef$glu, levels = c(1, 2, 3, 5, 6, 7), labels = c("agriculture", "forest", "grassland", "urban", "arid", "water")) 
  
# save --------------------------------------------------
  
# AVH  
  #avh_ef_200 <- spp_ef
  write.csv(avh_ef_200, file = "C:/Users/s436862/Dropbox/Climate Matching/4. Results/Grass groups AVH/Single scales/AVH 200km SR-EFs DF.csv", row.names = F)
  
# Introduced
  #int_ef_200 <- spp_ef
  write.csv(int_ef_200, file = "C:/Users/s436862/Dropbox/Climate Matching/4. Results/Grass groups AVH/Single scales/Int 200km SR-EFs DF.csv", row.names = F)
  
# Native  
  #nat_ef_200 <- spp_ef
  write.csv(nat_ef_200, file = "C:/Users/s436862/Dropbox/Climate Matching/4. Results/Grass groups AVH/Single scales/Nat 200km SR-EFs DF.csv", row.names = F)
  
# Poa  
  #poa_ef_200 <- spp_ef
  write.csv(poa_ef_200, file = "C:/Users/s436862/Dropbox/Climate Matching/4. Results/Grass groups AVH/Single scales/Poa 200km SR-EFs DF.csv", row.names = F)
  
  
  
# ---------------------------------
  

  
  
