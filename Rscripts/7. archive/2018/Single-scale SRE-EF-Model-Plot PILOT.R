

# model All grass groups vs. EFs
# Note: load 'contains NA' files which include empty cells

# Note: have thus far only done 100 km Natives

# note: make a metadata for this (currently 21/1/18)

# completion so far is Nat @ 100 km
# basing  off template 'Multiscale SRE DFs'

# REMEMBER: contains_NAs file is required for modelling (need cells where spp are missing)
# Note: all teh correlation tests are NA for that one, but it doesn't matter

# Structure like this: 
# 50 km: AVH, Int, Nat, Poa EF-SRE dfs
# Then Model each in its own section
# 100 km: repeat
# ...


  rm(list = ls())

  library(ggplot2)
  library(ggmap)
  library(tidyr)
  library(raster)
  library(rgdal)
  library(maptools)
  library(dplyr)
  library(car)
  
  
  setwd("C:/Users/s436862/Dropbox/Climate Matching/1. Data files/")
  
  # hii <- raster("EFs/EFs cropped/hii.grd")
  # hii_v <- as.data.frame(getValues(hii))
  # hii_v1 <- select(hii_v < 30)
  # hist(hii_v)
  # mean(hii_v, na.rm = T) # = 4.84
  # median(hii_v, na.rm = T) # = 4
 
# 50 km -------------------------   
# Generating SRE-EF dataframe --------------------------------------------------------
  
  rm(list = ls())
  
  setwd("C:/Users/s436862/Dropbox/Climate Matching/1. Data files/")
  
# load dataframe
#   avh_50km <- read.csv("C:/Users/s436862/Dropbox/Climate Matching/4. Results/AVH/50 km scale/50km SREs AVH.csv")
#   
#   # EFs    
#   arid <- raster("EFs/EFs cropped/arid.grd") 
#   ap <- raster("EFs/EFs cropped/ap.grd") 
#   cm <- raster("EFs/EFs cropped/cm.grd")
#   cq <- raster("EFs/EFs cropped/cq.grd")
#   rz <- raster("EFs/EFs cropped/rz.grd")
#   sp <- raster("EFs/EFs cropped/sp.grd")
#   st <- raster("EFs/EFs cropped/st.grd")
#   elev <- raster("EFs/EFs cropped/elev.grd")
#   evap <- raster("EFs/EFs cropped/evap.grd")
#   hii <- raster("EFs/EFs cropped/hii.grd")
#   mat <- raster("EFs/EFs cropped/mat.grd")
#   ps <- raster("EFs/EFs cropped/ps.grd")
#   wm <- raster("EFs/EFs cropped/wm.grd")
#   wq <- raster("EFs/EFs cropped/wq.grd")
#   glu <- raster("Efs/EFs cropped/glu.grd")
#   
#   ef.stack <- stack(ap, arid, cm, cq, elev,
#                     evap, hii, mat, ps, 
#                     rz, sp, st, wm, wq)
#   names(ef.stack) <- c("ap", "arid", "cm", "cq", "elev", "evap", "hii", "mat", "ps", "rz", "sp", "st", "wm", "wq")
#   names(glu) <- ("glu")  
#   
# # aggregate (note: land use is categorical, requires modal agg. function)  
#   ef.ag <- aggregate(ef.stack, fact = 50, fun = mean)
#   glu.ag <- aggregate(glu, fact = 50, fun = modal)
#   # should HII be calculated with a median score? That's what Haque et al. 2017 did ...   Ask Richard
#   
# # combine   
#   ef <- stack(glu.ag, ef.ag)
#   
# # get values + make dataframe
#   ef_v_na <- as.data.frame(getValues(ef))
# 
# # remove NAs (same method as for AVH)  
#   arid <- getValues(ef$arid)
#   ef_v <- ef_v_na[!is.na(arid), ] 
#   
# # Now both ef_v & AVH dfs same length, we merge
#   avh_ef <- data.frame(avh_50km, ef_v)
#   
# # make glu a discrete factor
#   avh_ef$glu <- factor(avh_ef$glu, levels = c(1, 2, 3, 5, 6, 7), labels = c("agriculture", "forest", "grassland", "urban", "arid", "water")) 
#   
# # Note: have kept in zero values for AVH; i.e. where n = 0
#   
# # save 
#   write.csv(avh_ef, file = "C:/Users/s436862/Dropbox/Climate Matching/4. Results/AVH/50 km scale/50km SREs_EFs AVH.csv", row.names = F)
#   
#   
# -----------------------------------------
# Model ---------------------------------------------
#   avh_ef <- read.csv("C:/Users/s436862/Dropbox/Climate Matching/4. Results/AVH/50 km scale/50km SREs_EFs AVH.csv")
#   
# # Model nomenclature: 
# # avh; 
# # 1, 5, 2 for scale; 
# # a, b, c for EFs (a = all, b = redcued, c = s/t else);
# # na, vl, l, m for community status;  
# # e.g. avh_1_a_vl
#  
#  
# ##### EF correlation matrix 
# # df with only the efs
#   efs <- select(avh_ef, ap, arid, cm, cq, elev, evap, hii, mat, ps, rz, sp, st, wm, wq)
# # note: removed glu as categorical variables seem not to be welcome here; ask Richard how do
#   ef_cor_mat <- cor(efs, method = "pearson")
#   # csv
#   write.csv(ef_cor_mat, file = "C:/Users/s436862/Dropbox/Climate Matching/4. Results/AVH/50 km scale/50km EF correlation matrix AVH.csv")
#   # note: don't do rownames = F for this cos we want them in this instance :)
#   # note: HII didn't compute; not sure of the cause
#   
# Reducing no. of EFs --------------------------------------------  
# # everythang    
#   avh_5_na_a <- lm(ichao1 ~ glu + ap + arid + cm + cq + elev + evap + hii + mat + ps + rz + sp + st + wm + wq, data = avh_ef) 
#   # Ant says GLM is a better one for flexible
#   summary(avh_5_na_a)
#   # 100 km = 0.61
#   # here, 50 km, = 0.51
#   
# # avh_5_na_b (reduced efs on account of correlations)
# # taking out ARID, MAT, WM, and SP + ST  
#   avh_5_na_b <- lm(ichao1 ~ glu + ap + cm + cq + elev + evap + hii + ps + rz + wq, data = avh_ef) 
#   summary(avh_5_na_b)
#   # @ 100 = 0.57
#   # here 0.49
#   
# # avh_5_na_b (reduced efs on account of significance)
# # (note these are different between 100 and 50 km scales)
# # common ones being taken out: CM, EVAP; but PS is nearing signif and RZ is not 
#   avh_5_na_c <- lm(ichao1 ~ glu + ap + cq + elev + hii + ps + wq, data = avh_ef)
#   summary(avh_5_na_c)
#   # 0.566 for 100; cool
#   # 0.48 here
#   # note: AP still not signif but idc <- almost is in 50!
# 
# # reducing stuff via VIF calculations    
#   avh_5_na_c <- vif(lm(ichao1 ~ glu + ap + cq + elev + hii + ps + wq, data = avh_ef))
#   avh_5_na_c
# # therefore, removing ap and wq
#   avh_5_na_d <- lm(ichao1 ~ glu + cq + elev + hii + ps, data = avh_ef)
#   summary(avh_5_na_d)
#   # 0.42, pretty good, gonna keep it
#   
# # therefore, model with cells of <200 records  
#   avh_5_na_d <- lm(ichao1 ~ glu + cq + elev + hii + ps, data = avh_ef)
#   summary(avh_5_na_d)
#   # 0.42, pretty good, gonna keep it
#   
# # without <200
#   avh_ef_5_no_200 <- filter(avh_ef, n> 200)
#   avh_5_b_m <- lm(ichao1 ~ glu + ap + cq + elev + hii + ps + wq, data = avh_ef_5_no_200) 
#   summary(avh_5_b_m)
#   # 100 = 0.542
#   # 50 = 0.37 <- this must be because we are throwing aaway many more cells at this scale
# Reduce community numbers (low SR cells) -------------------------
#   # using ichao1 SR  
#   
# # no zeroes  
#   avh_ef_0 <- filter(avh_ef, n != 0)
#   avh_5_b_0 <- lm(ichao1 ~ glu + ap + cq + elev + hii + ps + wq, data = avh_ef_0) 
#   summary(avh_5_b_0)
#   # 100 = 0.566
#   # 50 = 0.46
#   
# # no v.low (<10)
#   avh_ef_vl <- filter(avh_ef, ichao1>= 10)
#   avh_5_b_vl <- lm(ichao1 ~ glu + ap + cq + elev + hii + ps + wq, data = avh_ef_vl) 
#   summary(avh_5_b_vl)
#   # 100 = 0.563
#   # 50 = 0.46
#   
# # no low (<50)
#   avh_ef_l <- filter(avh_ef, ichao1>= 50)
#   avh_5_b_l <- lm(ichao1 ~ glu + ap + cq + elev + hii + ps + wq, data = avh_ef_l) 
#   summary(avh_5_b_l)
#   # 100 = 0.557
#   # 50 = 0.42
#   
# # no med (<100)  
#   avh_ef_m <- filter(avh_ef, ichao1>= 100)
#   avh_5_b_m <- lm(ichao1 ~ glu + ap + cq + elev + hii + ps + wq, data = avh_ef_m) 
#   summary(avh_5_b_m)
#   # 100 = 0.542
#   # 50 = 0.37 <- this must be because we are throwing aaway many more cells at this scale
#   
# -----------------------------------------
  
# 100 km -------------------------   

# SR-EF DF -------------------------------
  rm(list = ls())
  
  nat_100 <- read.csv("C:/Users/s436862/Dropbox/Climate Matching/4. Results/Grass groups AVH/Single scales/Nat 100km SRE DF contains NAs.csv")
  spp <- nat_100
  
  
  
# EFs    
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
  
  ef.stack <- stack(ap, arid, cm, cq, elev,
                    evap, hii, mat, ps, 
                    rz, sp, st, wm, wq)
  names(ef.stack) <- c("ap", "arid", "cm", "cq", "elev", "evap", "hii", "mat", "ps", "rz", "sp", "st", "wm", "wq")
  names(glu) <- ("glu")  

# aggregate (note: land use is categorical, requires modal agg. function)  
  ef.ag <- aggregate(ef.stack, fact = 100, fun = mean)
  glu.ag <- aggregate(glu, fact = 100, fun = modal)
  # should HII be calculated with a median score? That's what Haque et al. 2017 did ...   Ask Richard

# combine   
  ef <- stack(glu.ag, ef.ag)

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
# Nat  
  nat_100 <- spp_ef
  write.csv(nat_100, file = "C:/Users/s436862/Dropbox/Climate Matching/4. Results/Grass groups AVH/Single scales/Nat 100km SR-EFs DF.csv", row.names = F)
# -----------------------------------------
  
  
# Introduced Model [update via Native model] ---------------------------------------------
  # avh_ef <- read.csv("C:/Users/s436862/Dropbox/Climate Matching/4. Results/AVH/100 km scale/100km SREs_EFs AVH.csv")
  
  # Model nomenclature: 
  # avh; 
  # 1, 5, 2 for scale; 
  # a, b, c for EFs (a = all, b = redcued, c = s/t else);
  # na, vl, l, m for community status;  
  # e.g. avh_1_a_vl
  
  
  ##### EF correlation matrix 
  # df with only the efs
  efs <- select(avh_ef, ap, arid, cm, cq, elev, evap, hii, mat, ps, rz, sp, st, wm, wq)
  # note: removed glu as categorical variables seem not to be welcome here; ask Richard how do
  ef_cor_mat <- cor(efs, method = "pearson")
  # csv
  write.csv(ef_cor_mat, file = "C:/Users/s436862/Dropbox/Climate Matching/4. Results/AVH/100 km scale/100km EF correlation matrix AVH.csv")
  # note: don't do rownames = F for this cos we want them in this instance :)
  # note: HII didn't compute; not sure of the cause
  
# Reducing no. of EFs --------------------------------------------  
  # everythang    
  avh_1_na_a <- lm(ichao1 ~ glu + ap + arid + cm + cq + elev + evap + hii + mat + ps + rz + sp + st + wm + wq, data = avh_ef) 
  # Ant says GLM is a better one for flexible
  summary(avh_1_na_a)
  # 100 km = 0.61
  # here, 50 km, = 0.51
  
  # avh_1_na_b (reduced efs on account of correlations)
  # taking out ARID, MAT, WM, and SP + ST  
  avh_1_na_b <- lm(ichao1 ~ glu + ap + cm + cq + elev + evap + hii + ps + rz + wq, data = avh_ef) 
  summary(avh_1_na_b)
  # @ 100 = 0.57
  # here 0.49
  
  # avh_1_na_b (reduced efs on account of significance)
  # (note these are different between 100 and 50 km scales)
  # common ones being taken out: CM, EVAP; but PS is nearing signif and RZ is not 
  avh_1_na_c <- lm(ichao1 ~ glu + ap + cq + elev + hii + ps + wq, data = avh_ef)
  summary(avh_1_na_c)
  # 0.566 for 100; cool
  # 0.48 here
  # note: AP still not signif but idc <- almost is in 50!
  
  # reducing stuff via VIF calculations    
  avh_1_na_c <- vif(lm(ichao1 ~ glu + ap + cq + elev + hii + ps + wq, data = avh_ef))
  avh_1_na_c
  # therefore, removing ap and wq (same for 50)
  avh_1_na_d <- lm(ichao1 ~ glu + cq + elev + hii + ps, data = avh_ef)
  summary(avh_1_na_d)
  # 0.42, pretty good, gonna keep it
  
  # therefore, model with cells of <200 records  
  avh_1_na_d <- lm(ichao1 ~ glu + cq + elev + hii + ps, data = avh_ef)
  summary(avh_1_na_d)
  # 0.42, pretty good, gonna keep it
  
  # without <200
  avh_ef_1_no_200 <- filter(avh_ef, n> 200)
  avh_1_b_m <- lm(ichao1 ~ glu + ap + cq + elev + hii + ps + wq, data = avh_ef_1_no_200) 
  summary(avh_1_b_m)
  # 100 = 0.542
  # 50 = 0.37 <- this must be because we are throwing aaway many more cells at this scale
# Reduce community numbers (low SR cells) -------------------------
  # using ichao1 SR  
  
  # no zeroes  
  avh_ef_0 <- filter(avh_ef, n != 0)
  avh_1_b_0 <- lm(ichao1 ~ glu + ap + cq + elev + hii + rz + wq, data = avh_ef_0) 
  summary(avh_1_b_0)
  # 0.566
  
  # no v.low (<10)
  avh_ef_vl <- filter(avh_ef, ichao1>= 10)
  avh_1_b_vl <- lm(ichao1 ~ glu + ap + cq + elev + hii + rz + wq, data = avh_ef_vl) 
  summary(avh_1_b_vl)
  # 0.563
  
  # no low (<50)
  avh_ef_l <- filter(avh_ef, ichao1>= 50)
  avh_1_b_l <- lm(ichao1 ~ glu + ap + cq + elev + hii + rz + wq, data = avh_ef_l) 
  summary(avh_1_b_l)
  # 0.557
  
  # no med (<100)  
  avh_ef_m <- filter(avh_ef, ichao1>= 100)
  avh_1_b_m <- lm(ichao1 ~ glu + ap + cq + elev + hii + rz + wq, data = avh_ef_m) 
  summary(avh_1_b_m)
  # 0.542
  
  # cool   
  
# -----------------------------------------
  
  
# Native Model ---------------------------------------------
  
# nat <- read.csv("C:/Users/s436862/Dropbox/Climate Matching/4. Results/AVH/100 km scale/100km SREs_EFs AVH.csv")

##### EF correlation matrix 
  # df with only the efs
  efs <- select(avh_ef, ap, arid, cm, cq, elev, evap, hii, mat, ps, rz, sp, st, wm, wq)
  # note: removed glu as categorical variables seem not to be welcome here; ask Richard how do
  ef_cor_mat <- cor(efs, method = "pearson")
  # csv
  write.csv(ef_cor_mat, file = "C:/Users/s436862/Dropbox/Climate Matching/4. Results/AVH/100 km scale/100km EF correlation matrix AVH.csv")
  # note: don't do rownames = F for this cos we want them in this instance :)
  # note: HII didn't compute; not sure of the cause
  
# Reducing no. of EFs --------------------------------------------  
  # everythang    
  avh_1_na_a <- lm(ichao1 ~ glu + ap + arid + cm + cq + elev + evap + hii + mat + ps + rz + sp + st + wm + wq, data = avh_ef) 
  # Ant says GLM is a better one for flexible
  summary(avh_1_na_a)
  # 100 km = 0.61
  # here, 50 km, = 0.51
  
  # avh_1_na_b (reduced efs on account of correlations)
  # taking out ARID, MAT, WM, and SP + ST  
  avh_1_na_b <- lm(ichao1 ~ glu + ap + cm + cq + elev + evap + hii + ps + rz + wq, data = avh_ef) 
  summary(avh_1_na_b)
  # @ 100 = 0.57
  # here 0.49
  
  # avh_1_na_b (reduced efs on account of significance)
  # (note these are different between 100 and 50 km scales)
  # common ones being taken out: CM, EVAP; but PS is nearing signif and RZ is not 
  avh_1_na_c <- lm(ichao1 ~ glu + ap + cq + elev + hii + ps + wq, data = avh_ef)
  summary(avh_1_na_c)
  # 0.566 for 100; cool
  # 0.48 here
  # note: AP still not signif but idc <- almost is in 50!
  
  # reducing stuff via VIF calculations    
  avh_1_na_c <- vif(lm(ichao1 ~ glu + ap + cq + elev + hii + ps + wq, data = avh_ef))
  avh_1_na_c
  # therefore, removing ap and wq (same for 50)
  avh_1_na_d <- lm(ichao1 ~ glu + cq + elev + hii + ps, data = avh_ef)
  summary(avh_1_na_d)
  # 0.42, pretty good, gonna keep it
  
  # therefore, model with cells of <200 records  
  avh_1_na_d <- lm(ichao1 ~ glu + cq + elev + hii + ps, data = avh_ef)
  summary(avh_1_na_d)
  # 0.42, pretty good, gonna keep it
  
  # without <200
  avh_ef_1_no_200 <- filter(avh_ef, n> 200)
  avh_1_b_m <- lm(ichao1 ~ glu + ap + cq + elev + hii + ps + wq, data = avh_ef_1_no_200) 
  summary(avh_1_b_m)
  # 100 = 0.542
  # 50 = 0.37 <- this must be because we are throwing aaway many more cells at this scale
# Reduce community numbers (low SR cells) -------------------------
# using ichao1 SR  

# no zeroes  
  avh_ef_0 <- filter(avh_ef, n != 0)
  avh_1_b_0 <- lm(ichao1 ~ glu + ap + cq + elev + hii + rz + wq, data = avh_ef_0) 
  summary(avh_1_b_0)
  # 0.566
  
# no v.low (<10)
  avh_ef_vl <- filter(avh_ef, ichao1>= 10)
  avh_1_b_vl <- lm(ichao1 ~ glu + ap + cq + elev + hii + rz + wq, data = avh_ef_vl) 
  summary(avh_1_b_vl)
  # 0.563
  
# no low (<50)
  avh_ef_l <- filter(avh_ef, ichao1>= 50)
  avh_1_b_l <- lm(ichao1 ~ glu + ap + cq + elev + hii + rz + wq, data = avh_ef_l) 
  summary(avh_1_b_l)
  # 0.557
  
# no med (<100)  
  avh_ef_m <- filter(avh_ef, ichao1>= 100)
  avh_1_b_m <- lm(ichao1 ~ glu + ap + cq + elev + hii + rz + wq, data = avh_ef_m) 
  summary(avh_1_b_m)
  # 0.542

# cool   
  
# -----------------------------------------

  
  
# 200 km -------------------------   
# Generating SRE-EF dataframe --------------------------------------------------------
  
  rm(list = ls())
  
  setwd("C:/Users/s436862/Dropbox/Climate Matching/1. Data files/")
  
  # load dataframe
  avh_200km <- read.csv("C:/Users/s436862/Dropbox/Climate Matching/4. Results/AVH/200 km scale/200km SREs AVH.csv")
  
  # EFs    
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
  
  ef.stack <- stack(ap, arid, cm, cq, elev,
                    evap, hii, mat, ps, 
                    rz, sp, st, wm, wq)
  names(ef.stack) <- c("ap", "arid", "cm", "cq", "elev", "evap", "hii", "mat", "ps", "rz", "sp", "st", "wm", "wq")
  names(glu) <- ("glu")  
  
  # aggregate (note: land use is categorical, requires modal agg. function)  
  ef.ag <- aggregate(ef.stack, fact = 200, fun = mean)
  glu.ag <- aggregate(glu, fact = 200, fun = modal)
  # should HII be calculated with a median score? That's what Haque et al. 2017 did ...   Ask Richard
  
  # combine   
  ef <- stack(glu.ag, ef.ag)
  
  # get values + make dataframe
  ef_v_na <- as.data.frame(getValues(ef))
  # remove NAs (same method as for AVH)  
  arid <- getValues(ef$arid)
  ef_v <- ef_v_na[!is.na(arid), ] 
  
  # Now both ef_v & AVH dfs same length, we merge
  avh_ef <- data.frame(avh_200km, ef_v)
  
  # make glu discrete factor
  avh_ef$glu <- factor(avh_ef$glu, levels = c(1, 2, 3, 5, 6, 7), labels = c("agriculture", "forest", "grassland", "urban", "arid", "water")) 
  
  # Note: have kept in zero values for AVH; i.e. where n = 0
  
  # save 
  write.csv(avh_ef, file = "C:/Users/s436862/Dropbox/Climate Matching/4. Results/AVH/200 km scale/200km SREs_EFs AVH.csv", row.names = F)
  
  
  # Modelling it ---------------------------------------------
  avh_ef <- read.csv("C:/Users/s436862/Dropbox/Climate Matching/4. Results/AVH/200 km scale/200km SREs_EFs AVH.csv")
  
  # Model nomenclature: 
  # avh; 
  # 1, 5, 2 for scale; 
  # a, b, c for EFs (a = all, b = redcued, c = s/t else);
  # na, vl, l, m for community status;  
  # e.g. avh_1_a_vl
  
  
##### EF correlation matrix 
  # df with only the efs
  efs <- select(avh_ef, ap, arid, cm, cq, elev, evap, hii, mat, ps, rz, sp, st, wm, wq)
  # note: removed glu as categorical variables seem not to be welcome here; ask Richard how do
  ef_cor_mat <- cor(efs, method = "pearson")
  # csv
  write.csv(ef_cor_mat, file = "C:/Users/s436862/Dropbox/Climate Matching/4. Results/AVH/200 km scale/200km EF correlation matrix AVH.csv")
  # note: don't do rownames = F for this cos we want them in this instance :)
  # note: HII didn't compute; not sure of the cause
  
# Reducing no. of EFs --------------------------------------------  
  # everythang    
  avh_2_na_a <- lm(ichao1 ~ glu + ap + arid + cm + cq + elev + evap + hii + mat + ps + rz + sp + st + wm + wq, data = avh_ef) 
  # Ant says GLM is a better one for flexible
  summary(avh_2_na_a)
  # 100 km = 0.61
  # here, 50 km, = 0.51
  
  # avh_2_na_b (reduced efs on account of correlations)
  # taking out ARID, MAT, WM, and SP + ST  
  avh_2_na_b <- lm(ichao1 ~ glu + ap + cm + cq + elev + evap + hii + ps + rz + wq, data = avh_ef) 
  summary(avh_2_na_b)
  # @ 100 = 0.57
  # here 0.49
  
  # avh_2_na_b (reduced efs on account of significance)
  # (note these are different between 100 and 50 km scales)
  # common ones being taken out: CM, EVAP; but PS is nearing signif and RZ is not 
  avh_2_na_c <- lm(ichao1 ~ glu + ap + cq + elev + hii + ps + wq, data = avh_ef)
  summary(avh_2_na_c)
  # 0.566 for 100; cool
  # 0.48 here
  # note: AP still not signif but idc <- almost is in 50!
  
  # reducing stuff via VIF calculations    
  avh_2_na_c <- vif(lm(ichao1 ~ glu + ap + cq + elev + hii + ps + wq, data = avh_ef))
  avh_2_na_c
  # therefore, removing ap and wq (same for 50)
  avh_2_na_d <- lm(ichao1 ~ glu + cq + elev + hii + ps, data = avh_ef)
  summary(avh_2_na_d)
  # 0.42, pretty good, gonna keep it
  
  # therefore, model with cells of <200 records  
  avh_2_na_d <- lm(ichao1 ~ glu + cq + elev + hii + ps, data = avh_ef)
  summary(avh_2_na_d)
  # 0.42, pretty good, gonna keep it
  
  # without <200
  avh_ef_2_no_200 <- filter(avh_ef, n> 200)
  avh_2_b_m <- lm(ichao1 ~ glu + ap + cq + elev + hii + ps + wq, data = avh_ef_2_no_200) 
  summary(avh_2_b_m)
  
  
  # 100 = 0.542
  # 50 = 0.37 <- this must be because we are throwing aaway many more cells at this scale
# Reduce community numbers (low SR cells) -------------------------
  # using ichao1 SR  
  
  # no zeroes  
  avh_ef_0 <- filter(avh_ef, n != 0)
  avh_2_b_0 <- lm(ichao1 ~  glu + ap + cm + elev + hii + ps + wq, data = avh_ef_0) 
  summary(avh_2_b_0)
  # 100 = 0.566
  # 200 = 0.58
  
  # no v.low (<10)
  avh_ef_vl <- filter(avh_ef, ichao1>= 10)
  avh_2_b_vl <- lm(ichao1 ~  glu + ap + cm + elev + hii + ps + wq, data = avh_ef_vl) 
  summary(avh_2_b_vl)
  # 100 = 0.563
  # 200 = 0.58
  
  # no low (<50)
  avh_ef_l <- filter(avh_ef, ichao1>= 50)
  avh_2_b_l <- lm(ichao1 ~  glu + ap + cm + elev + hii + ps + wq, data = avh_ef_l) 
  summary(avh_2_b_l)
  # 100 = 0.557
  # 200 = 0.579
  
  # no med (<100)  
  avh_ef_m <- filter(avh_ef, ichao1>= 100)
  avh_2_b_m <- lm(ichao1 ~  glu + ap + cm + elev + hii + ps + wq, data = avh_ef_m) 
  summary(avh_2_b_m)
  # 100 = 0.542
  # 200 = 0.59
  
  # cool   
  
# -----------------------------------------


# poa x AP --------------------------------------


library(ggthemes)

#plot developed 
ggplot(dat2, aes(y = native.species, x = precipitation)) +
  geom_point(size = 1.5, pch = 1, alpha = 1) +
  geom_smooth(method = "lm", color = "black") +
  labs(x = "Precipitation (log)",
       y = "Native species richness (log)") +
  theme_tufte() +
  #choosen colour palette
  theme(legend.position = "none",
        plot.title = element_text(size=16, hjust=0, face="italic", color="black"),
        plot.subtitle=element_text(size=16, hjust=0, face="italic", color="black"),
        legend.background = element_rect(fill="white", size=1, linetype="solid", colour ="black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y=element_text(colour = "black",size =14),
        axis.title.x = element_text(colour = "black",size =14),
        axis.text.x=element_text(size = 14),
        axis.text.y=element_text(colour = "black",size = 14),
        axis.ticks.x = element_line(size = 1),
        axis.ticks.y =element_line(size = 1),
        axis.line.x = element_line(size = 1),
        axis.line.y = element_line(size = 1)) +
  annotate("text", x = 7.8, y = 4.4, label = "italic(R) ^ 2 == 0.22",
           parse = TRUE, col = "blue") +
  #scale_y_continuous(limits=c(-2.5,2)) + 
  #scale_x_continuous(limits=c(0,4)) +
  ggtitle("Log annual precipitation ~ log native richness")

ggsave("C:/Users/s436862/Dropbox/Climate Matching/4. Results/Poa_x_AP/log.native_log.AP.jpeg", plot = last_plot(), scale = 1, dpi = 200, device = "jpeg")


ggplot(nat_df, aes(y = log(nat_v), x = log(precip_v))) +
  geom_point() +
  abline()
# R2 = 0.17 

n_sing <- rasterize(xy, precip_ag, field = spp, fun = function(x,...) {length(which(table(x)==1)) })

# number of doubletons ("                         " two occurrences in the sample)
n_doub <- rasterize(xy, precip_ag, field = spp, fun = function(x,...) {length(which(table(x)==2)) })

# chao1 estimator
chao <- poa_r + ((n_sing * (n_sing - 1)) / (2 * (n_doub + 1)))
plot(chao)
plot(log10(chao))

# introduced
int <- filter(poa, status == "introduced")
i.xy <- cbind(int$long, int$lat)
i.spp <- as.numeric(factor(int$species))

int_r <- rasterize(i.xy, precip_ag, field = n.spp, fun = function(x,...) {length(unique(na.omit(x)))})

# extract values 
int_v <- getValues(int_r)
precip_v <- getValues(precip_ag)
int_df <- data.frame(int_v, precip_v)

# plotting
m2 <- lm(log(int_v) ~ log(precip_v))
abline(m2)
summary(m2)
# R2 = 0.17 

m3 <- glm(int_v ~ int_v)
plot(m3)
summary(m3)

#test1
dat10 <-  filter(int_df, !precip_v == "NA" ) %>%
  filter(!int_v == "NA" )
m2 <- lm(log(dat10$int_v) ~ log(dat10$precip_v))
summary(m2)

m2 <- lm(log(dat10$int_v) ~ log(dat10$precip_v))
summary(m2)

plot(log(dat10$int_v) ~ log(dat10$precip_v))
abline(m2, col = "red", lwd = 5)


#test2
#anthony
head(dat10)
glimpse(dat10)
dat20 <- mutate(dat10, precipitation = log(as.numeric(precip_v)),
                introduced.species = log(as.numeric(int_v))) %>%
  filter(precip_v > 1 & int_v > 1)

m3 <- lm(dat20$introduced.species ~ dat20$precipitation)
summary(m3)

# dat3 <- select(dat2, precipitation, introduced.species) %>%
#   gather(va)

library(ggthemes)

#plot developed 
ggplot(dat20, aes(y = introduced.species, x = precipitation)) +
  geom_point(size = 1.5, pch = 1, alpha = 1) +
  geom_smooth(method = "lm", color = "black") +
  labs(x = "Precipitation (log)",
       y = "Introduced species richness (log))") +
  theme_tufte() +
  #choosen colour palette
  theme(legend.position = "none",
        plot.title = element_text(size=16, hjust=0, face="italic", color="black"),
        plot.subtitle=element_text(size=16, hjust=0, face="italic", color="black"),
        legend.background = element_rect(fill="white", size=1, linetype="solid", colour ="black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y=element_text(colour = "black",size =14),
        axis.title.x = element_text(colour = "black",size =14),
        axis.text.x=element_text(size = 14),
        axis.text.y=element_text(colour = "black",size = 14),
        axis.ticks.x = element_line(size = 1),
        axis.ticks.y =element_line(size = 1),
        axis.line.x = element_line(size = 1),
        axis.line.y = element_line(size = 1)) +
  annotate("text", x = 7.8, y = 4.4, label = "italic(R) ^ 2 == 0.13",
           parse = TRUE, col = "blue") +
  #scale_y_continuous(limits=c(-2.5,2)) + 
  #scale_x_continuous(limits=c(0,4)) +
  ggtitle("Log annual precipitation ~ log introduced richness")

ggsave("C:/Users/s436862/Dropbox/Climate Matching/4. Results/Poa_x_AP/log.introduced_log.AP.jpeg", plot = last_plot(), scale = 1, dpi = 200, device = "jpeg")

ggplot(int_df, aes(y = log(int_v), x = log(precip_v))) +
  geom_point() +
  abline()

f <- lm(log(int_v) ~ log(precip_v))
summary(f)
plot(f)

# R2 = 0.17 

# save
write.csv(poa_x_AP, file = "C:\\Users\\s436862\\Dropbox\\Climate Matching\\4. Results\\Poa_x_AP\\poa_x_AP.csv")

pdf(file = "C:\\Users\\s436862\\Dropbox\\Climate Matching\\4. Results\\Chao corrected Poa\\poa_x_AP.pdf", width = 7, heigh = 5)
plot(log(poa_v) ~ log(precip_v))
abline(0, 1)
dev.off()

