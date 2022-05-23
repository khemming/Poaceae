
# Date created: 14/3/18
# Last updated: 30/4

# Overall aim: to test how appropriate different measures of species richness are (SREs), including extrapolation and interpolation methods, for using herbarium data
# this script will collate all of the data 

# Based on "Rarefaction estimator grass_messed with" (and ""Rarefaction estimator grass)
# and Single-scale SRE-DF for how I did the SREs

# next scripts: Rarefaction model (which will include correlation analysis) & Rarefaction raster plots (which will include correlation figures)

# Outcomes:
# SRE dataframe (2538 long) with each SRE and 1147 too (Aus-only values)
# rasters --> a way to plot these from that dataframe (rather than the other way around). Cool. 
  
  library(raster)
  library(tidyverse)
  library(ggmap)
  library(oz)

  rm(list = ls())
  
  setwd("C:/Users/s436862/Dropbox/Rarefaction/1. Data files")
  
################# Proportion of cells occupied: rarefaction EDA #####################
# Aim: decide where the appropriate ranges are for cell width & rarefaction cutoff points
# These are opposing forces: we want the highest 'cutoff' point (giving the biggest range of relative species richness between cells), but the lowest % of cells excluded, to have a good coverage of Australia
  
# This script is split into two sections because of the aggregate function: first is the 1 km dataframe, and second the 2-250 km one, with their merger at the end
  
  rm(list = ls())
  
  setwd("C:/Users/s436862/Dropbox/Rarefaction/1. Data files")
  
# 1. Record coverage across scales #####################################  
# Note: add in 'mean correlation between cells' record# and HII at each cell width  
# 1.1 1km width dataframe ----------------------------------------------
# Aims: ----------------------------------------------------------------
# (1) compute 1 km cell width   
# (2) compute proportion of cells occupied (>0 records) at each width
# (3) compute proportion of cells occupied at different cell-record cutoffs (>25, >50, >100,>250, >500 records) at each width
# (4) compute the correlation between average record number and HII score for each scale
  
# HII raster (for record#-human correlation & to remove sea-related NA values)
  raster <- raster("EFs/EFs cropped/hii")
  
# Poa  
  spp <- read.csv("AVH/AVH grass records.csv", header = T)
  xy <- cbind(spp$long, spp$lat)
  
# number of records per cell (n)
  n <- rasterize(xy, raster, fun = function(x,...) {length(na.omit(x)) })
  n_val <- getValues(n)
  raster_val <- getValues(raster)
  n_rec_na <- data.frame(raster_val, n_val)
  n_rec_b <- n_rec_na[!is.na(raster_val), ] 
  n_rec <- n_rec_b[ ,2] # 9.9M cells. Wow
  
# convert NAs to zeroes
  n_rec[is.na(n_rec)] <- 0 
  sum(is.na(n_rec)) # beautiful
  
# community estimates
  cell_width <- 1
  total_cells <- length(n_rec)
  prop_occ <- sum(n_rec > 0) / total_cells * 100
  twenty_five <- sum(n_rec >= 25) / total_cells * 100
  fifty <- sum(n_rec >= 50) / total_cells * 100
  one_hundred <- sum(n_rec >= 100) / total_cells * 100
  two_fifty <- sum(n_rec >= 250) / total_cells * 100
  five_hundred <- sum(n_rec >= 500) / total_cells * 100 
  
# dataframe (8 cols)
  out_1km <- as.data.frame(cbind(cell_width, mean(n_rec), total_cells, prop_occ, twenty_five, fifty, one_hundred, two_fifty, five_hundred))
  
  colnames(out_1km) <- c("cell_width", "mean_rec", "total_cells", "all_records", "twenty_five", "fifty", "one_hundred", "two_fifty", "five_hundred")   
  # total: 8 + 1
  
# remove everything but output in environment to do next section
  rm(list=setdiff(ls(), "out_1km"))  
  
  
# 1.2 2-500 km width dataframe -----------------------------------------
# Required: -------------------------------------------------------------
# (1) cell widths: 2 km to where it asymptotes (100% occupied) -- 250 km for Poa has 99.51% coverage, using that  
# (2) compute proportion of cells occupied (>0 records) at each width
# (3) compute proportion of cells occupied at different cell-record cutoffs (>25, >50, >100,>250, >500 records) at each width

# raster template 
  raster <- raster("EFs/EFs cropped/arid.grd")
  
# Poa  
  spp <- read.csv("AVH/AVH grass records.csv", header = T)
  xy <- cbind(spp$long, spp$lat)
  
# test    
#width <- 100
  
# Cell-proportion function -------------------------
  prop <- function(width) 
  {
    
  # aggregate
    raster_agg <- aggregate(raster, fact = width, fun = mean)
    
  # number of records per cell (n)
    n <- rasterize(xy, raster_agg, fun = function(x,...) {length(na.omit(x)) })
    
  # exclude extra-terreestrial (NA) cells from dataframe
    b <- getValues(raster_agg)
    n_val <- getValues(n) 
    
    n_rec_na <- data.frame(b, n_val)
    n_rec_b <- n_rec_na[!is.na(b), ] 
    n_rec <- n_rec_b[ ,2]
    
  # convert NAs to zeroes
    n_rec[is.na(n_rec)] <- 0 
    sum(is.na(n_rec)) # beautiful
    
  # community estimates
    total_cells <- length(n_rec)
    prop_occ <- sum(n_rec > 0) / total_cells * 100
    twenty_five <- sum(n_rec >= 25) / total_cells * 100
    fifty <- sum(n_rec >= 50) / total_cells * 100
    one_hundred <- sum(n_rec >= 100) / total_cells * 100
    two_fifty <- sum(n_rec >= 250) / total_cells * 100
    five_hundred <- sum(n_rec >= 500) / total_cells * 100 
    
  # dataframe (8 cols)
    res <- as.data.frame(cbind(mean(n_rec), total_cells, prop_occ, twenty_five, fifty, one_hundred, two_fifty, five_hundred))                                                                                         
    return(res)                                                                    
    
  }  #finish function
  
# Output ----------------------------
# scale range (2 - 250 km)
  width <- c(2, 3, 4, 6, 8, 12, 16, 22, 32, 50, 75, 100, 125, 175, 250)
  
  out <- matrix(nrow = length(width), ncol = 9) # res cols +1
  
  for(i in 1:length(width)) 
  {
    out[i, 1] <- width[i]
    out[i, 2:9] <- as.numeric(prop(width[i]))
  }
  
  out_multi_km <- out
  
  colnames(out_multi_km) <- c("cell_width", "mean_rec", "total_cells", "all_records", "twenty_five", "fifty", "one_hundred", "two_fifty", "five_hundred")   
  # total: 8 + 1
  
# merge dataframes
  out_prop <- rbind(out_1km, out_multi_km)
  
# save 
  write.csv(out_prop, file = "C:/Users/s436862/Dropbox/Rarefaction/4. Results/Grass groups AVH/Rarefaction/CSV/Multiscale_rare_cell_occupation.csv", row.names = F)

# ---------------------------------------------------------------------    
  
  rm(list = ls())
  
# 2. SRE dataframe & rasters #########################################
# Rarefaction -----------------------------------------------------------------------
# required: 
# spp = species record list with lat/long
# raster = land area
# scale = scale by which raster will be converted to (assumed 1-km^2 raster and 10,000-km^2 scale)  
# cutoff = rarefied richness value  
  spp <- read.csv("AVH/AVH grass records.csv", header = T) %>%
    dplyr::filter(status == "native")   %>%
    dplyr::select(species, lat, long, year) 
  
  raster <- raster("australia raster/aus.grd")
  scale <- 100
  raster_agg <- aggregate(raster, fac = scale, fun = mean)
  values(raster_agg) <- 1:ncell(raster_agg)
  
  xy <- cbind(spp$long, spp$lat)
  
# assign each point in the dataframe to raster cell
  spp$cell <- raster::extract(raster_agg, xy)
    
# number of records per cell
  n_rec <- table(spp$cell)
  nr <- data.frame(cell = as.numeric(names(n_rec)), n_rec = as.vector(n_rec))
# add number of records, per cell, to the dataframe
  spp <- full_join(spp, nr) %>%
            arrange(cell) %>%
            select(cell, species, lat, long, year, n_rec)
# cell numbers with total records in each
  cell_list <- as.numeric(levels(factor(spp$cell)))
  
# Rarefaction function ------------------------------------------------------
  rarefaction <- function(spp, cutoff) 
  { 
      n <- length(spp)               # number of records           
      n_sp <- table(spp)             # number of records for each species    
      
      out <- numeric(length(n_sp))  # vector to store estimate for each species    
    
    # for each species, calculate the expected number of occurrences from n records
      for(i in 1:length(n_sp)) 
      {
        out[i] <- 1 - exp(lchoose((n - n_sp[i]), cutoff) - lchoose(n, cutoff))    
        # use lchoose (i.e. log scale) to avoid problems with big numbers
      }
    
    return(round(sum(out)))
  } # end function 
  
  out <- matrix(nrow = length(1:ncell(raster_agg)))
    
# run function ----------------------------------------------------------
  cutoff <- 50
  
  for(j in 1:length(cell_list)) { 
      cell <- filter(spp, cell == cell_list[j]) 
      sp <- as.character(cell$species)
      if(cell$n_rec[1] <= cutoff) out[j] <- NA 
      else { out[j, ] <- rarefaction(sp, cutoff) 
           }
  }
  
  for(j in 1:length(cell_list)) {
    cell <- filter(spp, cell == cell_list[j])
    if(cell$n_rec[1] <= cutoff) out_rare[j] <- NA else {
      sp <- as.character(cell$species)
      out_rare[j, ] <- rarefaction(sp, cutoff)
    }
  }
  # output I want to capture is out_rare
  
  rare_ras <- setValues(raster_agg, out_rare)
  plot(rare_ras)
# ----------------------------------------------------------------------------------     
    
# Extrapolation SREs ---------------------------------------------------------------
# required:    
# total number of records per cell (n)
  n <-  rasterize(xy, raster_agg, fun = function(x,...) {length(unique(na.omit(x))) })
# actual richness (a)
    a <- rasterize(xy, raster_agg, field = spp, fun = function(x,...) {length(unique(na.omit(x))) })
  
  # singletons, doubletons 
    f1 <- rasterize(xy, raster_agg, field = spp, fun = function(x,...) {length(which(table(x)==1)) })
    f2 <- rasterize(xy, raster_agg, field = spp, fun = function(x,...) {length(which(table(x)==2)) })
    
# ACE stuff 
  # rare species(<=10) per cell
    s_rare <- rasterize(xy, raster_agg, field = spp, fun = function(x,...) {length(which(table(x)<11)) })
  # abundant species (>10) per cell
    s_abun <- rasterize(xy, raster_agg, field = spp, fun = function(x,...) {length(which(table(x)>10)) })
  
  # number of rare individuals (records) per cell
    n_rare <- rasterize(xy, raster_agg, field = spp, fun = function(x,...) {sum(table(x)[which(table(x)<=10)]) })
  # validatation method (see below)* 
    n_comm <- rasterize(xy, raster_agg, field = spp, fun = function(x,...) {sum(table(x)[which(table(x)>10)]) })
    
  # kf = k*(k - 1)*fk part of the g2 eqn 
  # this might be tricky
  # Richard's method:  
  # kf <- sum(n[n <= 10] * (n[n <= 10] - 1)) * sRare 
  # let's see if doing this functionally equivalent in the rasterize function works
    kf <- rasterize(xy, raster_agg, field = spp, fun = function(x,...) {sum((table(x)[which(table(x)<=10)])*(table(x)[which(table(x)<=10)] - 1)) })
    kf <- getValues(kf)
    sum(kf, na.rm = T)# seem legit
    
  # get values
    s_rare <- getValues(s_rare)
    sum(s_rare, na.rm = T)
    
    s_abun <- getValues(s_abun)
    sum(s_abun, na.rm = T)
    
    n_rare <- getValues(n_rare)
    sum(n_rare, na.rm = T) 
    n_comm <- getValues(n_comm)
    sum(n_comm, na.rm = T)
  
  # *n_rare & n_comm should sum to equal n 
    sum(n_comm + n_rare, na.rm = T) 
    # Yuuuuuuus I am amazing // Mostly Richard is
    
# retrieve cell values of ther est of the SRE correctives
  # actual record no. + richness
    n <- getValues(n)
    a <- getValues(a)
  # SRE correctives
    f1 <- getValues(f1)
    f2 <- getValues(f2)
    
  # remove offshore values & replace NAs with zeroes for SRE calculations
    raster_df <- getValues(raster_agg)
    sre <- data.frame(raster_df, n, a, f1, f2, s_rare, s_abun, n_rare, kf, out_rare)
    sre[is.na(sre$raster), 2:10] <- NA
    sre[is.na(sre)] <- 0
    sum(is.na(sre)) # cool
    
  # split them back up
    n <- sre$n
    a <- sre$a
    f1 <- sre$f1
    f2 <- sre$f2
    s_rare <- sre$s_rare
    s_abun <- sre$s_abun
    n_rare <- sre$n_rare
    kf <- sre$kf
    

# ACE calculation 
  # so we have created thus far: s_rare, s_abun, n_rare, kf from the rasterization process
  # and so here we collect them into the ACE equation
  
  # sample coverage    
    c_ace <- 1 - (f1 / n_rare)
    
  # coefficient of variation (g2)
    g2a <- max(((s_rare * kf) / (c_ace * n_rare * (n_rare - 1))) - 1, 0, na.rm = T)
    
    g2b <- ((s_rare * kf) / (c_ace * n_rare * (n_rare - 1))) - 1
    g2b <- ifelse(g2b < 0, 0, g2b)
  
  # ACE estimator 
    ace <- s_abun + s_rare/c_ace + (f1/c_ace * g2b)
    
# chao1 
    chao1 <- ifelse(f2 == 0, 
             (a + (((n-1) / n) * (f1 * (f1-1)) / (2 * (f2+1)))),
             (a + (((n-1) / n) * ((f1^2)/(2*f2))))
      )
    # 'if' won't handle two conditional statements 
   
    
    
# Jackknife second order 
    jack_second <- a + 2*f1 - f2
    

    sre_df <- data.frame(n, a, chao1, ace, jack_second)  
    
    return(sre_df)
    
  } # finish function
  
# Run function ----------------------------------  
# raster (v)
  v <- raster("EFs/EFS cropped/arid.grd")
# scale/width (z)
  z <- 100
  
# Species data  
# Poa  
  x1 <- read.csv("AVH/AVH grass records.csv", header = T)
  sre_df_poa <- sre_function(x1, v, z) 
  write.csv(sre_df_poa, file = "C:/Users/s436862/Dropbox/Rarefaction/4. Results/Grass groups AVH/Rarefaction/CSV/Poa_SRE.csv", row.names = F)
# Nat  
  x2 <- read.csv("AVH/AVH grass records.csv", header = T) %>%
    dplyr::filter(status == "native")
  sre_df_nat <- sre_function(x2, v, z) 
  write.csv(sre_df_nat, file = "C:/Users/s436862/Dropbox/Rarefaction/4. Results/Grass groups AVH/Rarefaction/CSV/Nat_SRE.csv", row.names = F)
# Int
  x3 <- read.csv("AVH/AVH grass records.csv", header = T) %>%
    dplyr::filter(status == "introduced")
  sre_df_int <- sre_function(x3, v, z) 
  write.csv(sre_df_int, file = "C:/Users/s436862/Dropbox/Rarefaction/4. Results/Grass groups AVH/Rarefaction/CSV/Int_SRE.csv", row.names = F)
  

  
  
  
  
# Poa sre rasters ------------------------------------------
# SRE raster generation 
  poa <- read.csv("C:/Users/s436862/Dropbox/Rarefaction/4. Results/Grass groups AVH/Rarefaction/CSV/Poa_SRE.csv", header = T)  
  nat <- read.csv("C:/Users/s436862/Dropbox/Rarefaction/4. Results/Grass groups AVH/Rarefaction/CSV/Nat_SRE.csv", header = T) 
  int <- read.csv("C:/Users/s436862/Dropbox/Rarefaction/4. Results/Grass groups AVH/Rarefaction/CSV/Int_SRE.csv", header = T) setwd("C:/Users/s436862/Dropbox/Rarefaction/4. Results/Grass groups AVH/Rarefaction")
  
# dummy raster   
  aus <- raster("C:/Users/s436862/Dropbox/Rarefaction/1. Data files/Australia raster/aus") 
  width <- 100 
  aus <- aggregate(aus, fac = width, fun = mean)
  
# substitute dataframe values into rasters and save  
  values(aus) <- poa$n # tot_rec; need to log transform that for plotting
  poa_n <- aus
  writeRaster(poa_n, "Raster/Poa_n", overwrite = T)
  
  values(aus) <- poa$a # actual SR
  poa_a <- aus
  plot(poa_a)
  writeRaster(poa_a, "Raster/Poa_a", overwrite = T)
  
  values(aus) <- poa$a
  poa_a <- aus
  plot(poa_a)
  writeRaster(poa_a, "Raster/Poa_a", overwrite = T)
  
  values(aus) <- poa$jack_second
  poa_j <- aus
  plot(poa_j)
  writeRaster(poa_j, "Raster/Poa_jack2nd", overwrite = T)
 
  values(aus) <- poa$ichao1
  poa_ic <- aus
  plot(poa_ic)
  writeRaster(poa_ic, "Raster/Poa_ichao1", overwrite = T)
  
  values(aus) <- poa$lanum # log that one, too?
  poa_l <- aus
  plot(poa_l)
  writeRaster(poa_l,  "Raster/Poa_lanum", overwrite = T) 
  
 
# Native rasters ------------------------------------------------------     
  values(aus) <- nat$n # tot_rec; need to log transform that
  nat_n <- aus
  plot(nat_n)
  writeRaster(nat_n, "Raster/Nat_n", overwrite = T)
  
  values(aus) <- nat$a # actual SR
  nat_a <- aus
  plot(nat_a)
  writeRaster(nat_a, "Raster/Nat_a", overwrite = T)
  
  values(aus) <- nat$jack_second
  nat_j <- aus
  plot(nat_j)
  writeRaster(nat_j, "Raster/Nat_jack2nd", overwrite = T)
  
  values(aus) <- nat$chao1
  nat_c <- aus
  plot(nat_c)
  writeRaster(nat_c, "Raster/Nat_chao1", overwrite = T)
  
  values(aus) <- nat$ichao1
  nat_ic <- aus
  plot(nat_ic)
  writeRaster(nat_ic, "Raster/Nat_ichao1", overwrite = T)
  
  values(aus) <- nat$lanum # log that one, too?
  nat_l <- aus
  plot(nat_l)
  writeRaster(nat_l,  "Raster/Nat_lanum", overwrite = T) 
  
# Introduced rasters ------------------------------------------------------     
  values(aus) <- int$n # tot_rec; need to log transform that
  int_n <- aus
  plot(int_n)
  writeRaster(int_n, "Raster/Int_n", overwrite = T)
  
  values(aus) <- int$a # actual SR
  int_a <- aus
  plot(int_a)
  writeRaster(int_a, "Raster/Int_a", overwrite = T)
  
  values(aus) <- int$jack_second
  int_j <- aus
  plot(int_j)
  writeRaster(int_j, "Raster/Int_jack2nd", overwrite = T)
  
  values(aus) <- int$chao1
  int_c <- aus
  plot(int_c)
  writeRaster(int_c, "Raster/Int_chao1", overwrite = T)
  
  values(aus) <- int$ichao1
  int_ic <- aus
  plot(int_ic)
  writeRaster(int_ic, "Raster/Int_ichao1", overwrite = T)
  
  values(aus) <- int$lanum # log that one, too?
  int_l <- aus
  plot(int_l)
  writeRaster(int_l,  "Raster/Int_lanum", overwrite = T)
  
# Proportion of introduced species rasters ----------------------------------  
# For plots that I am using for my investigation
# FOr this I want: baseline (default) proportions; chao1's proportions, and rarefaction's
# Note: rarefaction is somewhere else in this script
  
# Baseline  
  values(aus) <- int$a / poa$a 
  int_prop_bl <- aus
  plot(int_prop_bl) # cool
  writeRaster(int_prop_bl,  "Raster/Int_proportion_baseline", overwrite = T)
  
# Chao1
  values(aus) <- int$chao1 / poa$chao1 
  int_prop_c <- aus
  plot(int_prop_c) # high values skew scale
  writeRaster(int_prop_c,  "Raster/Int_proportion_Chao1", overwrite = T)
  
# Jackknife (for fun -- Chao was weird, I want to see if Jack is too)
  values(aus) <- int$jack_second / poa$jack_second 
  int_prop_j <- aus
  plot(int_prop_j) # all goods
  writeRaster(int_prop_j,  "Raster/Int_proportion_Jackknife", overwrite = T)  
  
 
# 2.3 Complete SRE dataframe -------------------------------------
# Adding together the SREs and rarefaction into a single dataframe  
  sre <- read.csv("C:/Users/s436862/Dropbox/Rarefaction/4. Results/Grass groups AVH/Rarefaction/CSV/Poa_SRE.csv", header = T)  
  rare <- read.csv("C:/Users/s436862/Dropbox/Rarefaction/4. Results/Grass groups AVH/Rarefaction/CSV/rarefaction_Poa_Int_Nat_multi_cutoffs.csv", header = T) 
  
  
# ----------------------------------------------------------------------------------------  
  
  
################################# Extras ######################################
# Poa SR at 50- and 200-km scales --------------------------------------------
  setwd("C:/Users/s436862/Dropbox/Rarefaction/1. Data files")
  
  poa <- read.csv("AVH/AVH grass records.csv", header = T)
  xy <- cbind(poa$long, poa$lat) 
  spp <- as.numeric(factor(poa$species))
  
  aus <- raster("Australia raster/aus")
  
# 50-km  
  width <- 50 
  aus1 <- aggregate(aus, fac = width, fun = mean)
  poa_50 <- rasterize(xy, aus1, field = spp, fun = function(x,...) {length(unique(na.omit(x))) })
  plot(poa_50)
  writeRaster(poa_50,"C:/Users/s436862/Dropbox/Rarefaction/4. Results/Grass groups AVH/Rarefaction/Raster/Poa_50km", overwrite = T)
  
# 200-km  
  width <- 200 
  aus2 <- aggregate(aus, fac = width, fun = mean)
  poa_200 <- rasterize(xy, aus2, field = spp, fun = function(x,...) {length(unique(na.omit(x))) })
  plot(poa_200)
  writeRaster(poa_200,"C:/Users/s436862/Dropbox/Rarefaction/4. Results/Grass groups AVH/Rarefaction/Raster/Poa_200km", overwrite = T)  
  
  
# ----------------------------------------------------------------------------------------    

################################### EFs ########################################
# the aim of this section is to be able to join EFs onto the sides of the SRE scrips, 
# and then work out how what I want to do with this all
  
# basing this section off 'Single-scale SR-DF' -------------------------------------------
  
  library(raster)
  library(oz)
  library(tidyverse)
  library(ggmap)
  library(tidyr)
  library(oz)
  
  rm(list = ls())
  
  setwd("C:/Users/s436862/Dropbox/Rarefaction/1. Data files")
  
# making the EF dataframe ---------------------------------------------------  
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
  
# stack those which use 'mean' for aggregation function
  width <- 100 # or whatever you decided it was before
  
  ef.stack <- stack(arid, ap, cm, cq, elev,
                    evap, mat, ps, rz, sp, st, 
                    wm, wq, pawc, pewc)
  ef.ag <- aggregate(ef.stack, fact = width, fun = mean)
# modal function  
  glu.ag <- aggregate(glu, fact = width, fun = modal)
# median
  hii_ag <- aggregate(hii, fact = width, fun = median)
# topographic hetero -- calculated as the SD of the mean of each cell
  th <- aggregate(elev, fact = width, fun = sd) # <- love how easy that was
# combine & name  
  ef <- stack(glu.ag, hii_ag, ef.ag, th)  
  names(ef) <- c("glu", "hii", "ap", "arid","cm", "cq", "elev", "evap", "mat", "ps", "rz", "sp", "st", "wm", "wq", "pawc", "pewc", "th")
  
# make dataframe
  ef_v_na <- as.data.frame(getValues(ef))
  ef_v_na$glu <- factor(ef_v_na$glu, levels = c(1, 2, 3, 5, 6, 7), labels = c("agriculture", "forest", "grassland", "urban", "arid", "water"))
  
# save dataframe and I think we are good
  write.csv(ef_v_na, file = "C:/Users/s436862/Dropbox/Rarefaction/4. Results/Grass groups AVH/Rarefaction/CSV/EFs.csv", row.names = F)
  
# ----------------------------------------------------------------------------------------  
  
################################### SAC #################################################
# Species accumulation curves ----------------------------------------------------
# produced from "Why assymptotic estimators don't work" and beerbayes No. 3.   
  
# First I wat to make a plot of all the cells' communitites -- SR x record no.
# (then circle 4x comms I want to check out, using PP)
# 4x communitites in a 'z' type shape; basically, two high-records cells, of similar SR; and two low-record cells, of similar SR, as well.  
  
  rm(list = ls())
  
  setwd("C:/Users/s436862/Dropbox/Rarefaction/1. Data files")
  
# species records    
  poa <- read.csv("AVH/AVH grass records.csv", header = T)
# raster  
  aus <- raster("australia raster/aus_100km.grd")

# number of records by grid squares
  xy <- cbind(poa$long, poa$lat)
  
# assign each point in the dataframe to raster cells
  poa$cell <- raster:::extract(aus, xy)
  
# select relevant columns
  sp <- poa %>%
    dplyr::select(species, status, year, cell)
  
# number of records per cell
  n_rec <- table(sp$cell)
  nr1 <- data.frame(cell = as.numeric(names(n_rec)), n_rec = as.vector(n_rec))
  
# add number of records per cell to the dataframe
  sp <- full_join(sp, nr1)
  sp_df <- sp
  
# number of records per cell in order
  nr <- nr1[order(nr1$n_rec), ]
  tail(nr)  
  
# Communities -------------------------------------------------- 
# which cells have high/low richness etc.
  grass_cell_summary <- sp %>% 
    group_by(cell) %>%
    summarise(sr = n_distinct(species), 
              rec_no = n())
  
# eyeball cells of interest
  ggplot(grass_cell_summary, aes(x = rec_no, y = sr)) + 
    geom_point() +
    theme_bw() +
    scale_x_continuous(limits = c(0,4500)) # exclude that massive cell cos it's too big
  #ggsave("C:/Users/s436862/Dropbox/Rarefaction/4. Results/Grass groups AVH/Rarefaction/Graphs/Poa SR x Rec no.jpeg", plot = last_plot(), scale = 1, dpi = 500, device = "jpeg")  
  
# highest community (a)    
  a_f <- filter(grass_cell_summary, between(sr, 280, 310) & between(rec_no, 3000, 5000))
  a_1347 <- a_f[1,]
# higher community (b)    
  b_f <- filter(grass_cell_summary, between(sr, 260, 300) & between(rec_no, 1500, 1900))
  b_690 <- b_f
# lower community (c)    
  c_f <- filter(grass_cell_summary, between(sr, 130, 170) & between(rec_no, 1500, 1750))
  c_1935 <- c_f[2,]
# lowest community (d)    
  d_f <- filter(grass_cell_summary, between(sr, 140, 160) & between(rec_no, 400, 480))
  d_1455 <- d_f[3,]
  
  comms1 <- bind_rows(a_1347, b_690, c_1935, d_1455)
  community <- c("hs", "hr", "lr", "ls")
  comms <- cbind(community, comms1) # cheaaa, buddy
  
  #ggplot(comms, aes(x = rec_no, y = sr))  +
    geom_point() # cool
  
# ---------------------------------------------------------------------------------------- 
# Species accumulatiom curve (SAC): Per record, richness increase ---------------------------------------  
  sac <- function(x, y) {
    ord <- order(y) # turn you into year
    x <- x[ord]
    chao <- numeric(length(x))
    accum <- out
    ace <- out
    jack <- out
    for(i in 1:length(x)) {
      a <- x[1:i] # not i, but cumulative; or something. Idk.
      b <- table(a)
      obs.rich <- length(b)
      f1 <- sum(b == 1)   
      f2 <- sum(b == 2) 
      
      # Chao1
      chao[i] <- ifelse(f2 == 0, 
                        (obs.rich + (f1 * (f1-1)) / (2 * (f2+1))),
                        (obs.rich + ((f1^2)/(2*f2))))
      
      
      
      accum[i] <- obs.rich  # species accumulation over time
      
      # Jack second
      jack[i] <- obs.rich + (2 * f1 - f2)
      
      # ACE estimator
      s.rare <- sum(b <= 10)
      s.abun <- sum(b > 10)
      n.rare <- sum(b[b <= 10])
      c.ace <- 1 - (f1 / n.rare)
      k.f <- sum(b[b <= 10] * (b[b <= 10] - 1))
      g2.ace <- max(((s.rare * k.f) / (c.ace * n.rare * (n.rare - 1))) - 1, 0)
      ace[i] <- s.abun + s.rare/c.ace + (f1/c.ace)*g2.ace
      
    }
    return(data.frame(idx = 1:length(out), 
                      chao,
                      ace, 
                      accum,
                      jack))
  }
  
# ---------------------------------------------------------------------------------------- 
# Species accumulatiom curve (SAC): Per year, richness increase ---------------------------------------  
# order cells by year and see if curve saturates 
# requires as input: x = list of species names, one for each record
#                    y = the year of each record
  
# function checking: x = spp; y = yr
  #x <- sp
  #y <- yr
  
  sac <- function(x, y) {
    ord <- order(y) # Species richness of each year
    x <- x[ord]
  # lists of the SREs for looping via each year's record harvest
    out <- numeric(length(unique(y)))
    accum <- out
    ace <- out
    jack <- out
    chao <- out
    
    q <- cumsum(table(y)) # q = cumulative sum of all records in that year and previous
    
    for(i in 1:length(out)) {
      a <- x[1:q[i]] # giving me SR at each interval between 1:i (i being 125) -- think it's not accumulating, actually. how do I accumulate you, buddy. And I am not 100% on that
      b <- table(a)
      obs.rich <- length(b)
      f1 <- sum(b == 1)   
      f2 <- sum(b == 2)   
      chao[i] <- ifelse(f2 == 0, 
                      (obs.rich + (f1 * (f1-1)) / (2 * (f2+1))),
                      (obs.rich + ((f1^2)/(2*f2))))
                       
                       
                       
      accum[i] <- obs.rich  # species accumulation over time
      
    # Jack second
      jack[i] <- obs.rich + (2 * f1 - f2)
      
    # ACE estimator
      s.rare <- sum(b <= 10)
      s.abun <- sum(b > 10)
      n.rare <- sum(b[b <= 10])
      c.ace <- 1 - (f1 / n.rare)
      k.f <- sum(b[b <= 10] * (b[b <= 10] - 1))
      g2.ace <- max(((s.rare * k.f) / (c.ace * n.rare * (n.rare - 1))) - 1, 0)
      ace[i] <- s.abun + s.rare/c.ace + (f1/c.ace)*g2.ace
    }
    return(data.frame(idx = sort(unique(y)), 
                      chao,
                      ace, 
                      accum,
                      jack))
  }
# ----------------------------------------------------------------------------------------  
  
# Run it for 4x comms 
# ---------------------------------------------------------------------------------------- 
# HT; cell 1347 ------------------------
  input <- poa %>%
    filter(cell == 1347) %>%
    select(species, year)
  
  sp <- as.character(input$species)
  yr <- input$year
  length(table(sp))
  length(sp)
  
  ht <- sac(sp, yr) # find out what you are, basically
  plot(ht[[5]], type = "l")
  lines(ht[[3]], col = "blue")
  lines(ht[[4]], col = "red") 
  lines(ht[[2]], col = "green")
  
  ht <- gather(ht, est, val, -idx)
  
  ggplot(ht, aes(x = idx, y = val, color = est)) +
    geom_line() +
    theme_bw() +
  labs(title = "Largest record no.",
       x = "Year",
       y = "Species richness",
       fill = "SRE") # not sure about how to change 'est' to SRE
  ggsave("C:/Users/s436862/Dropbox/Rarefaction/4. Results/Grass groups AVH/Rarefaction/Graphs/SAC_HT.jpeg", plot = last_plot(), scale = 1, dpi = 500, device = "jpeg")  
  
# HR; cell 690 ------------------------
  input <- poa %>%
    filter(cell == 690) %>%
    select(species, year)
  
  sp <- as.character(input$species)
  yr <- inp$year
  length(table(sp))
  length(sp)
  
  hr <- sac(sp, yr) # find out what you are, basically
  plot(hr[[5]], type = "l")
  lines(hr[[3]], col = "blue")
  lines(hr[[4]], col = "red") 
  lines(hr[[2]], col = "green") # das ist Jack, ja
  
  hr <- gather(hr, est, val, -idx)
  
  ggplot(hr, aes(x = idx, y = val, color = est)) +
    geom_line() +
    theme_bw() +
    labs(title = "Second largest record no.",
         x = "Year",
         y = "Species richness",
         fill = "SRE") # not sure about how to change 'est' to SRE
  ggsave("C:/Users/s436862/Dropbox/Rarefaction/4. Results/Grass groups AVH/Rarefaction/Graphs/SAC_HR.jpeg", plot = last_plot(), scale = 1, dpi = 500, device = "jpeg")  
  
  
# LR; cell 1935 ------------------------
  input <- poa %>%
    filter(cell == 1935) %>%
    select(species, year)
  
  sp <- as.character(input$species)
  yr <- inp$year
  length(table(sp))
  length(sp)
  
  lr <- sac(sp, yr) # find out what you are, basically
  plot(lr[[5]], type = "l")
  lines(lr[[3]], col = "blue")
  lines(lr[[4]], col = "red") 
  lines(lr[[2]], col = "green") # das ist Jack, ja
  
  lr <- gather(lr, est, val, -idx)
  
  ggplot(lr, aes(x = idx, y = val, color = est)) +
    geom_line() +
    theme_bw() +
    labs(title = "Third largest record no.",
         x = "Year",
         y = "Species richness",
         fill = "SRE") # not sure about how to change 'est' to SRE
  ggsave("C:/Users/s436862/Dropbox/Rarefaction/4. Results/Grass groups AVH/Rarefaction/Graphs/SAC_LR.jpeg", plot = last_plot(), scale = 1, dpi = 500, device = "jpeg")  
# LT; cell 1455 ------------------------
  input <- poa %>%
    filter(cell == 1455) %>%
    select(species, year)
  
  sp <- as.character(input$species)
  yr <- inp$year
  length(table(sp))
  length(sp)
  
  lt <- sac(sp, yr) # find out what you are, basically
  plot(lt[[5]], type = "l")
  lines(lt[[3]], col = "blue")
  lines(lt[[4]], col = "red") 
  lines(lt[[2]], col = "green") # das ist Jack, ja
  
  lt <- gather(lt, est, val, -idx)
  
  ggplot(lt, aes(x = idx, y = val, color = est)) +
    geom_line() +
    theme_bw() +
    labs(title = "Smallest record no.",
         x = "Year",
         y = "Species richness",
         fill = "SRE") # not sure about how to change 'est' to SRE
  ggsave("C:/Users/s436862/Dropbox/Rarefaction/4. Results/Grass groups AVH/Rarefaction/Graphs/SAC_LT.jpeg", plot = last_plot(), scale = 1, dpi = 500, device = "jpeg")    

# Ratio of f1:f2 across Australia raster -----------------------------------------
# let's make two things:
# (1) raster of ratio of f1:f2;
# (2) correlation between ratio & record no.
#     would this work? I am not sure. Maybe not as a raster, but the values, surely  
  
  library(car)
  
  setwd("C:/Users/s436862/Dropbox/Rarefaction/1. Data files")
  
# data  
  poa <- read.csv("AVH/AVH grass records.csv", header = T)
  xy <- cbind(poa$long, poa$lat)
  spp <- as.numeric(factor(poa$species))
  raster <- raster("Australia raster/aus_100km")
  
# total number of records per cell (n)
  n <- rasterize(xy, raster, fun = function(x,...) {length(na.omit(x)) })
# actual richness (a)
  a <- rasterize(xy, raster, field = spp, fun = function(x,...) {length(unique(na.omit(x))) })
  
# single- & double-tons
  f1 <- rasterize(xy, raster, field = spp, fun = function(x,...) {length(which(table(x)==1)) })
  plot(f1)
  f2 <- rasterize(xy, raster, field = spp, fun = function(x,...) {length(which(table(x)==2)) })
  plot(f2)
  
# plot -- overlay, my friend, my enemy.  
  ratio <- overlay(f1, f2, fun = function(x, y) {return (x/y)})
  plot(ratio)
  
  writeRaster(ratio, "C:/Users/s436862/Dropbox/Rarefaction/4. Results/Grass groups AVH/Rarefaction/Raster/F1-F2 ratio", overwrite = T)
  
  arid <- raster("C:/Users/s436862/Dropbox/Rarefaction/1. Data files/EFs/EFs cropped/arid")
  
  a_v <- getValues(a)
  ratio_v <- getValues(ratio)
  a_ra <- cbind(a_v, ratio_v)
  
  a_ratio_cor <- cor(a_ra, method = "pearson", use = "complete.obs")
  cor(ef, method = "pearson", use ="complete.obs") # not sure why you're not playing ball
  
  