
# Date created: 25/2/18
# Last updated: 18/7

# Rarefaction estimator methods --------------------------------------------------
# The aim of this script is currently aimed as sussing out the best method for delivering Nat(c3/c4/total) & Int(c3/c4/total) into the Rarefaction chapter data script. Rarefaction 4.0 was the trial about how to rarefy in R. Please leave it be, and use this for anything methodological, and the chapter paper for species-related results.

# The second aim will to be the chapter for SREs, the apth towards deciding which SRE to use in general. Maybe I will make another script for that, and this can be the SRE v.4.0 like I said above. One tha tis messy and should be left alone. 

# See associated METADATA for more info

# Library -------------------------------------------------------------------------
  library(raster)
  library(oz)
  library(tidyverse)
  library(ggmap)
  library(tidyr)
  library(oz)
  
  rm(list = ls())
  
  setwd("C:/Users/s436862/Dropbox/Climate matching/1. Data files")

# 1. Rarefaction ------------------------------------------------------------------
# Aim: This is split into several sections because have a few different things to do here.
# As mentioned above, we have to assess rarefaction in terms of status (native/introduced), photosynthetic pathway (C3,C4, and total), cutoff (15 - 50 records)

  
# 1.1 Introduced-only rarefaction ------------------------------------------------------------
# Data formatting -----------------------------------------------------------------
# species    
  spp <- read.csv("Osborne C3-C4/AVH grass pp.csv", header = T) %>%
  ##  If doing pp stuff inlcude this: 
  #   mutate(pp = ifelse(pp == "C3 & C4", NA, as.character(pp))) %>%  
  #   mutate(pp = as.factor(pp)) %>%
  #   drop_na(pp)   
    filter(status == "native") %>%
    dplyr::select(species, lat, long, pp, year) 
  
  #head(table(spp$genus, spp$pp, exclude = NULL))
  glimpse(spp)
  
# raster 
  raster <- raster("Australia/aus_100km_cell_id.grd")
  
# number of records by grid square
  xy <- cbind(spp$long, spp$lat)
  n_tot <- rasterize(xy, raster, fun = function(x,...) length(x))
  n_tot_val <- getValues(n_tot)
  
# assign each point in the dataframe to raster cell
  spp$cell <- raster::extract(raster, xy)
  
# get a list of the cell numbers
  cell.list <- as.numeric(levels(factor(spp$cell)))
  
# number of records per cell --------------------------------------------
# number of records for only cells with records in them
  n_rec <- table(spp$cell)
  nr <- data.frame(cell = as.numeric(names(n_rec)), 
                   n_rec = as.vector(n_rec))
  n_cell_rec_occ <- spp %>%
    group_by(cell) %>%
    summarise(n_rec = n())      
  
# number of records for all cells  
  n_cell_rec_tot <- data.frame(cell = 1:length(n_tot_val), n_tot = n_tot_val)
  
# check the dataframes line up
  ch_rv1 <- full_join(n_cell_rec_occ, n_cell_rec_tot)      
  plot(log(n_tot) ~ log(n_rec), data = ch_rv1) # they agree
  
# Number of species per cell ---------------------------------------------
# number of species per cell
  spp_per_cell <- as.numeric(factor(spp$species))
  n_spp <- rasterize(xy, raster, field = spp_per_cell, fun = function(x,...) {length(unique(na.omit(x))) })
  plot(n_spp)
  
# check this (spp richness per cell)
  check2 <- spp %>%
    group_by(cell) %>%
    summarise(n = length(unique(species))) 
  
  x <- getValues(n_spp)
  rv <- data.frame(cell = 1:length(x), n_tot = x)
  ch_rv <- full_join(check2, rv) # line up all goods
  plot(log(n) ~ log(n_tot), data = ch_rv) # lines up, cool
  
# add number of records, per cell, to the dataframe
  spp <- full_join(spp, nr) %>%
    arrange(cell)

# cell numbers with total records in each
  cell_list <- as.numeric(levels(factor(spp$cell)))
  
# Rarefaction Introduced-only function -------------------------------------
  rarefaction_solo <- function(sp, cutoff) 
  { 
    n <- length(sp)           # number of records           
    n_sp <- table(sp)         # number of records for each species    
    out <- numeric(length(n_sp))# vector to store estimate for each species
    
  # for each species, calculate the expected number of occurrences from n records
    for(i in 1:length(n_sp)) 
    {
      out[i] <- 1 - exp(lchoose((n - n_sp[i]), cutoff) - lchoose(n, cutoff))    
    # use lchoose (i.e. log scale) to avoid problems with big numbers
    }
    
    return(round(sum(out)))
  } # end function 
  
# Run function ----------------------------------------------------------
# matrix to store output
  out_rare <- matrix(nrow = length(cell_list)) # length 1009-1147 (not 2538)
  
# matrix with all cells
  rarefied_rich <- matrix(NA, nrow = length(getValues(raster)), ncol = 8)
  
# Do a few of these  
# Would be great to loop through serveral of these
# I'm jut not that clued up
# cutoff <- 15
# cutoff <- 20
# cutoff <- 25
# cutoff <- 30
# cutoff <- 35
# cutoff <- 40
# cutoff <- 45
# cutoff <- 50
  
  for(j in 1:length(cell_list)) { 
    cell <- dplyr::filter(spp, cell == cell_list[j]) # return only the cells with records in them
    sp <- as.character(cell$species)
    if(cell$n_rec[1] < cutoff) out_rare[j] <- NA 
    else { out_rare[j, ] <- rarefaction_solo(sp, cutoff) 
    }
  }
  
# add the rarefied cells, one cutoff at a time  
# rarefied_rich[cell_list, 1] <- out_rare
# rarefied_rich[cell_list, 2] <- out_rare
# rarefied_rich[cell_list, 3] <- out_rare
# rarefied_rich[cell_list, 4] <- out_rare
# rarefied_rich[cell_list, 5] <- out_rare
# rarefied_rich[cell_list, 6] <- out_rare
# rarefied_rich[cell_list, 7] <- out_rare
# rarefied_rich[cell_list, 8] <- out_rare
  
# View and save -----------------------------------------------------------------------
# Assess geographic coverage via rasters & save + check distribution of cell values with histograms
  est_rich_15 <- setValues(raster, rarefied_rich[, 1])
  plot(est_rich_15) 
  hist(est_rich_15$layer)
  writeRaster(est_rich_15,"C:/Users/s436862/Dropbox/Rarefaction/4. Results/Rarefaction/Rasters/Rarefied int cutoffs/Int rare 15", overwrite = T)
  
  est_rich_20 <- setValues(raster, rarefied_rich[, 2])
  plot(est_rich_20)
  hist(est_rich_20$layer)
  writeRaster(est_rich_20,"C:/Users/s436862/Dropbox/Rarefaction/4. Results/Rarefaction/Rasters/Rarefied int cutoffs/Int rare 20", overwrite = T)
  
  est_rich_25 <- setValues(raster, rarefied_rich[, 3])
  plot(est_rich_25)
  hist(est_rich_25$layer)
  writeRaster(est_rich_25,"C:/Users/s436862/Dropbox/Rarefaction/4. Results/Rarefaction/Rasters/Rarefied int cutoffs/Int rare 25", overwrite = T)
  
  est_rich_30 <- setValues(raster, rarefied_rich[, 4])
  plot(est_rich_30)
  hist(est_rich_30$layer)
  writeRaster(est_rich_30,"C:/Users/s436862/Dropbox/Rarefaction/4. Results/Rarefaction/Rasters/Rarefied int cutoffs/Int rare 30", overwrite = T)
  
  est_rich_35 <- setValues(raster, rarefied_rich[, 5])
  plot(est_rich_35)
  hist(est_rich_35$layer)
  writeRaster(est_rich_35,"C:/Users/s436862/Dropbox/Rarefaction/4. Results/Rarefaction/Rasters/Rarefied int cutoffs/Int rare 35", overwrite = T)
  
  est_rich_40 <- setValues(raster, rarefied_rich[, 6])
  plot(est_rich_40)
  hist(est_rich_40$layer)
  writeRaster(est_rich_40,"C:/Users/s436862/Dropbox/Rarefaction/4. Results/Rarefaction/Rasters/Rarefied int cutoffs/Int rare 40", overwrite = T)
  
  est_rich_45 <- setValues(raster, rarefied_rich[, 7])
  plot(est_rich_45)
  hist(est_rich_45$layer)
  writeRaster(est_rich_45,"C:/Users/s436862/Dropbox/Rarefaction/4. Results/Rarefaction/Rasters/Rarefied int cutoffs/Int rare 45", overwrite = T)
  
  est_rich_50 <- setValues(raster, rarefied_rich[, 8])
  plot(est_rich_50)
  hist(est_rich_50$layer)
  writeRaster(est_rich_50,"C:/Users/s436862/Dropbox/Rarefaction/4. Results/Rarefaction/Rasters/Rarefied int cutoffs/Int rare 50", overwrite = T)
  
# They're all a bit terrible
  
# Save data frame
  rarefied_list <- data.frame(rarefied_rich)
  colnames(rarefied_list) <- c("rare_15", "rare_20", "rare_25", "rare_30", "rare_35", "rare_40", "rare_45", "rare_50")
  
  write.csv(rarefied_list, file = "C:/Users/s436862/Dropbox/Rarefaction/4. Results/Rarefaction/CSV/Int_ONLY multi-cutoffs.csv", row.names = F)
  
# 1.2 SD vs. % occupied for different introduced rarefied cutoffs -----------------
# Standard deviation of cell records vs. number of cells occupied
# We want a high SD, but high record number - let's see how this plays out
  cutoff <- read.csv("C:/Users/s436862/Dropbox/Rarefaction/4. Results/Rarefaction/CSV/Int_ONLY multi-cutoffs.csv", header = T)
  
# SD & CV of each cutoff value  
  cutoff.sd <- sapply(cutoff, sd, na.rm = T)
  
  cv <- function(x) {sd(x, na.rm = T)/mean(x, na.rm = T)}
  cutoff.cv <- sapply(cutoff, cv)

# just going to check this is ok
  cv.test.35 <- sd(cutoff$rare_35, na.rm = T) / mean(cutoff$rare_35, na.rm = T)
  cv.test.20 <- sd(cutoff$rare_20, na.rm = T) / mean(cutoff$rare_20, na.rm = T)
# Es ok :) 
  
# Number of cells with rarefied records
# Not sure how to do one for each row altogether so I shall do manually
  r15 <- sum(table(cutoff$rare_15))
  r20 <- sum(table(cutoff$rare_20))
  r25 <- sum(table(cutoff$rare_25))
  r30 <- sum(table(cutoff$rare_30))
  r35 <- sum(table(cutoff$rare_35))
  r40 <- sum(table(cutoff$rare_40))
  r45 <- sum(table(cutoff$rare_45))
  r50 <- sum(table(cutoff$rare_50))
  cutoff.cell.tot <- rbind(r15, r20, r25, r30, r35, r40, r45, r50)
  
# percentage  
  p15 <- sum(table(cutoff$rare_15))/1003*100
  p20 <- sum(table(cutoff$rare_20))/1003*100
  p25 <- sum(table(cutoff$rare_25))/1003*100
  p30 <- sum(table(cutoff$rare_30))/1003*100
  p35 <- sum(table(cutoff$rare_35))/1003*100
  p40 <- sum(table(cutoff$rare_40))/1003*100
  p45 <- sum(table(cutoff$rare_45))/1003*100
  p50 <- sum(table(cutoff$rare_50))/1003*100
  cutoff.per <- rbind(p15, p20, p25, p30, p35, p40, p45, p50)

# rarefied names
  rarefied <- c(15, 20, 25, 30, 35, 40, 45, 50)
  
# dataframe
  cutoff.com <- cbind(rarefied, cutoff.cell.tot, cutoff.per, cutoff.sd, cutoff.cv)
  colnames(cutoff.com) <- c("rarefied", "cell.total", "cell.percent", "SD", "CV")  
  
# save
  write.csv(cutoff.com, file = "C:/Users/s436862/Dropbox/Rarefaction/4. Results/Rarefaction/CSV/Int_ONLY multi-cutoffs summary stats.csv", row.names = F)
  

# ---------------------------------------------------------------------------------
  
# 2. Rarefaction: Native species -------------------------------------------------
# What is the maximum record-cutoff with the highest coverage for introduced grass species? Well, it's probably 20-30, so I'll try these three for nat for comparison
# Data formatting --------------------------------------------------------------- 
# remember this will need to have the one with pp in the proper script or whereever you take this
  
# species    
  spp <- read.csv("Osborne C3-C4/AVH grass pp.csv", header = T) %>%
  ##  If doing pp stuff inlcude this: 
  #   mutate(pp = ifelse(pp == "C3 & C4", NA, as.character(pp))) %>%  
  #   mutate(pp = as.factor(pp)) %>%
  #   drop_na(pp) %>%
    filter(status == "native") %>%
    dplyr::select(species, lat, long, pp, year) 
  
  head(table(spp$genus, spp$pp, exclude = NULL))
  glimpse(spp)
  
# raster 
  raster <- raster("Australia/aus_100km_cell_id.grd")
  
# number of records by grid square
  xy <- cbind(spp$long, spp$lat)
  n_tot <- rasterize(xy, raster, fun = function(x,...) length(x))
  n_tot_val <- getValues(n_tot)
  
# assign each point in the dataframe to raster cell
  spp$cell <- raster::extract(raster, xy)
  
# get a list of the cell numbers
  cell.list <- as.numeric(levels(factor(spp$cell)))
  
# number of records per cell --------------------------------------------
# number of records for only cells with records in them
  n_rec <- table(spp$cell)
  nr <- data.frame(cell = as.numeric(names(n_rec)), 
                   n_rec = as.vector(n_rec))
  n_cell_rec_occ <- spp %>%
    group_by(cell) %>%
    summarise(n_rec = n())      
  
# number of records for all cells  
  n_cell_rec_tot <- data.frame(cell = 1:length(n_tot_val), n_tot = n_tot_val)
  
# check the dataframes line up
  ch_rv1 <- full_join(n_cell_rec_occ, n_cell_rec_tot)      
  plot(log(n_tot) ~ log(n_rec), data = ch_rv1) # they agree
  
# Number of species per cell ---------------------------------------------
# number of species per cell
  spp_per_cell <- as.numeric(factor(spp$species))
  n_spp <- rasterize(xy, raster, field = spp_per_cell, fun = function(x,...) {length(unique(na.omit(x))) })
  plot(n_spp)
  
# check this (spp richness per cell)
  check2 <- spp %>%
    group_by(cell) %>%
    summarise(n = length(unique(species))) 
  
  x <- getValues(n_spp)
  rv <- data.frame(cell = 1:length(x), n_tot = x)
  ch_rv <- full_join(check2, rv) # line up all goods
  plot(log(n) ~ log(n_tot), data = ch_rv) # lines up, cool
  
# add number of records, per cell, to the dataframe
  spp <- full_join(spp, nr) %>%
    arrange(cell) %>%
    select(cell, species, lat, long, year, pp, n_rec)
  
# cell numbers with total records in each
  cell_list <- as.numeric(levels(factor(spp$cell)))
  
# Rarefaction SOLO function ------------------------------------------------
  rarefaction_solo <- function(sp, cutoff) 
  { 
    n <- length(sp)           # number of records           
    n_sp <- table(sp)         # number of records for each species    
    out <- numeric(length(n_sp))# vector to store estimate for each species
    
  # for each species, calculate the expected number of occurrences from n records
    for(i in 1:length(n_sp)) 
    {
      out[i] <- 1 - exp(lchoose((n - n_sp[i]), cutoff) - lchoose(n, cutoff))    
    # use lchoose (i.e. log scale) to avoid problems with big numbers
    }
    
    return(round(sum(out)))
  } # end function 
  
# Run function ----------------------------------------------------------
# matrix to store output
  out_rare <- matrix(nrow = length(cell_list)) # length 1009-1147 (not 2538)
  
# matrix with all cells
  rarefied_rich <- matrix(NA, nrow = length(getValues(raster)), ncol = 3)
  
# Do a few of these  
# cutoff <- 20
# cutoff <- 25
# cutoff <- 30
  
  for(j in 1:length(cell_list)) { 
    cell <- dplyr::filter(spp, cell == cell_list[j]) # return only the cells with records in them
    sp <- as.character(cell$species)
    if(cell$n_rec[1] <= cutoff) out_rare[j] <- NA 
    else { out_rare[j, ] <- rarefaction_solo(sp, cutoff) 
    }
  }
  
# add the rarefied cells, one cutoff at a time  
# rarefied_rich[cell_list, 1] <- out_rare
# rarefied_rich[cell_list, 2] <- out_rare
# rarefied_rich[cell_list, 3] <- out_rare
  
# View and save -----------------------------------------------------------------------
# Assess geographic coverage via rasters & save + check distribution of cell values with histograms
  est_rich_20 <- setValues(raster, rarefied_rich[, 1])
  plot(est_rich_20) 
  hist(est_rich_20$layer)
  writeRaster(est_rich_20,"C:/Users/s436862/Dropbox/Rarefaction/4. Results/Rarefaction/Rasters/Rarefied nat cutoffs/Nat rare 20", overwrite = T)
  
  est_rich_25 <- setValues(raster, rarefied_rich[, 2])
  plot(est_rich_25)
  hist(est_rich_25$layer)
  writeRaster(est_rich_25,"C:/Users/s436862/Dropbox/Rarefaction/4. Results/Rarefaction/Rasters/Rarefied nat cutoffs/Nat rare 25", overwrite = T)
  
  est_rich_30 <- setValues(raster, rarefied_rich[, 3])
  plot(est_rich_30)
  hist(est_rich_30$layer)
  writeRaster(est_rich_30,"C:/Users/s436862/Dropbox/Rarefaction/4. Results/Rarefaction/Rasters/Rarefied nat cutoffs/Nat rare 30", overwrite = T)
  
# Save data frame
  rarefied_list <- data.frame(rarefied_rich)
  colnames(rarefied_list) <- c("rare_20", "rare_25", "rare_30")
  
  write.csv(rarefied_list, file = "C:/Users/s436862/Dropbox/Rarefaction/4. Results/Rarefaction/CSV/Nat_ONLY multi-cutoffs.csv", row.names = F)
  
# ---------------------------------------------------------------------------------------  
  
# Date created: 31/7
# Last updated:
  
# This is the rarefaction methodology (cutoffs, pp, nat/int), all rolled into to one script.
# The last script (SRE methods) was getting a bit large. 
  
  
# Library -------------------------------------------------------------------------
  library(raster)
  library(oz)
  library(tidyverse)
  library(ggmap)
  library(tidyr)
  library(oz)
  
  rm(list = ls())
  
  setwd("C:/Users/s436862/Dropbox/Climate matching/1. Data files")
  
  
# 3. Rarefied split via pp -----------------------------------------------------------
  rm(list=setdiff(ls(), "native.array")) 
  rm(list=setdiff(ls(), "introduced.array")) 
# Data formatting --------------------------------------------------------------------------- 
# species, raster data ----------------------------------------------------------------
# 'C3 & C4' mixed class into NAs   
  spp <- read.csv("Osborne C3-C4/AVH grass pp.csv", header = T) %>% 
    mutate(pp = ifelse(pp == "C3 & C4", NA, as.character(pp))) %>%  
    mutate(pp = as.factor(pp)) %>%
    drop_na(pp)                                                     
  
  head(table(spp$genus, spp$pp, exclude = NULL))
  glimpse(spp)
  
# raster  
  raster <- raster("Australia/aus_100km_cell_id.grd")
  
# Subset into native and introduced statuses
  spp_n <- filter(spp, status == "native") %>%
    dplyr::select(species, lat, long, pp, year) 
  
  spp_i <- filter(spp, status == "introduced") %>%
    dplyr::select(species, lat, long, pp, year) 
  
 
# Rarefaction set-up -------------------------------------------------------   
# number of records per cell ----------------------------------------------
  spp <- spp_n
  # spp <- spp_i
  
# number of records by grid square
  xy <- cbind(spp$long, spp$lat)
  spp$cell <- raster::extract(raster, xy)
  
# number of records for only cells with records in them
  n_rec <- table(spp$cell)
  nr <- data.frame(cell = as.numeric(names(n_rec)), 
                   n_rec = as.vector(n_rec))
  
  cell_rec_occ <- spp %>%
    group_by(cell) %>%
    summarise(n_rec = n())      
  
# number of records for all cells 
  n_tot <- rasterize(xy, raster, fun = function(x,...) length(x))
  v_tot <- getValues(n_tot)
  cell_rec_tot <- data.frame(cell = 1:length(v_tot), n_tot = v_tot)
  
# check the dataframes line up
  ch_rv1 <- full_join(cell_rec_occ, cell_rec_tot)      
  plot(log(n_tot) ~ log(n_rec), data = ch_rv1) # they agree
  
# Number of species per cell ---------------------------------------------
# number of species per cell
  spp_per_cell <- as.numeric(factor(spp$species))
  n_spp <- rasterize(xy, raster, field = spp_per_cell, fun = function(x,...) {length(unique(na.omit(x))) })
  plot(n_spp)
  
# check this (spp richness per cell)
  check2 <- spp %>%
    group_by(cell) %>%
    summarise(n = length(unique(species))) 
  
  x <- getValues(n_spp)
  rv <- data.frame(cell = 1:length(x), n_tot = x)
  ch_rv <- full_join(check2, rv) # line up all goods
  plot(log(n) ~ log(n_tot), data = ch_rv) # lines up, cool
  
# add number of records, per cell, to the dataframe
  spp2 <- full_join(spp, nr) %>%
    arrange(cell)
    
# cell numbers with total records in each
  cell_list <- as.numeric(levels(factor(spp2$cell)))
  
# Rarefied function  -----------------------------------------------------
# requires as input: 
# sp = a vector of all the records with species names & cell_id
# pp = a vector of photosynthetic pathway (C3 or C4) for each record (though its only genus-level)
# n = the number of samples for rarefaction
  rarefaction_split <- function(sp, pp, n) {
    N <- length(sp)          # number of records
    sp_n <- table(sp)        # number of records for each species
    
  # get the pp for each species in alphabetical order: 1 = C3, 0 = C4
    a <- unique(cbind(sp, pp))
    a <- a[order(a[, 1]), ]
    ne <- ifelse(a[, 2] == "C3", 1, 0)
    
    out <- numeric(length(sp_n))# vector to store estimate for each species
    
  # for each species, calculate the expected number of occurrences from n records
    for(i in 1:length(sp_n)) {
      out[i] <- 1 - exp(lchoose((N - sp_n[i]), n) - lchoose(N, n))    
    # use lchoose (i.e. log scale) to avoid problems with big numbers
    }
  # output estimated total richness, and estimated richness of introduced and native species
    return(c(round(sum(out)), round(sum(out[ne == 1])), round(sum(out[ne == 0]))))
  }
  
# Store output ------------------------------------------------------------------
# matrix to store output: one row for each cell, 3 columns: 1 = total richness, 2 = introduced richness, 3 = native richness
  n_min <- seq(15, 50, 5)
  out_rare <- matrix(nrow = length(cell_list), ncol = 3)
  
# multi-cutoff (array; 2538, 3, 8)
  a <- array(dim = c(length(raster), ncol(out_rare), length(n_min)))
  
# function start
  for (i in 1:length(n_min)) {
    
    rec_no <- n_min[i]
    
    for(j in 1:length(cell_list)) {
      cell <- filter(spp2, cell == cell_list[j])
      if(cell$n_rec[1] < n_min) out_rare[j] <- NA else {
        spp <- as.character(cell$species)
        pp <- as.character(cell$pp)
        out_rare[j, ] <- rarefaction_split(spp, pp, rec_no)
      }
    }
    
    m <- matrix(NA, nrow = length(getValues(raster)), ncol = 3)
    m[cell_list, ] <- out_rare                         
    
    a[,,i] <- m
    
  } # multi-cutoff end 
  
  
# ------------------------------------------------------------------------------------------- 
  
# Save native and introduced arrays
  
# native.array <- a
  sum(native.array[, 1, 4],na.rm = T)
  sum(native.array[, 2, 4],na.rm = T)
  sum(native.array[, 3, 4],na.rm = T)

# introduced.array <- a  
  sum(introduced.array[, 1, 4], na.rm = T)
  sum(introduced.array[, 2, 4], na.rm = T)
  sum(introduced.array[, 3, 4], na.rm = T)
  
# add in column names
  dimnames = list(c("a", "b", "c"),
                  c("d", "e", "f"),
                  c("g", "h", "i"))

  dimnames(native.array)[[2]] <- c("total", "c3", "c4")
  dimnames(native.array)[[3]] <- c("rare.15", "rare.20", "rare.25", "rare.30",
                                   "rare.35", "rare.40", "rare.45", "rare.50")
  
  dimnames(introduced.array)[[2]] <- c("total", "C3", "C4")
  dimnames(introduced.array)[[3]] <- c("rare.15", "rare.20", "rare.25", "rare.30",
                                       "rare.35", "rare.40", "rare.45", "rare.50")
  
  write.csv(native.array, file = "C:/Users/s436862/Dropbox/Rarefaction/4. Results/Rarefaction/CSV/Native multiple cutoff COMPLETE.csv", row.names = F)
  
  write.csv(introduced.array, file = "C:/Users/s436862/Dropbox/Rarefaction/4. Results/Rarefaction/CSV/Introduced multiple cutoff COMPLETE.csv", row.names = F)
# ------------------------------------------------------------------------------------------- 
  
  
# 4. Nat/Int C3/C4/total 15- to 50-record rasters --------------------- 
# Species data
  nat <- read.csv("C:/Users/s436862/Dropbox/Rarefaction/4. Results/Rarefaction/CSV/Native multiple cutoff COMPLETE.csv", header = T)
  int <- read.csv("C:/Users/s436862/Dropbox/Rarefaction/4. Results/Rarefaction/CSV/Introduced multiple cutoff COMPLETE.csv", header = T)
 
# Australian cell of species and efs 
  land.cat <- read.csv("C:/Users/s436862/Dropbox/Climate Matching/1. Data files/EFs/4. EFs complete/Terrestrial categories.csv", header = T)
  
  nat.land <- cbind(land.cat, nat)
  int.land <- cbind(land.cat, int)
  
# subset terrestrial values out 
  nat.terr <- filter(nat.land, cell_category == "terrestrial")
  int.terr <- filter(int.land, cell_category == "terrestrial")
  
  cell.list.x <- nat.terr$cell_id
  cell.list.y <- int.terr$cell_id

# raster 
  aus <- raster("Australia/aus_100km_cell_id.grd")
  
# make a matrix with all missing values
  x <- matrix(NA, nrow = length(getValues(aus)), ncol = 24)
  y <- matrix(NA, nrow = length(getValues(aus)), ncol = 24)

  nat.m <- as.matrix(nat.terr)
  nat.mm <- nat.m[, 3:26]
  class(nat.mm) <- "numeric"
  
  int.m <- as.matrix(int.terr)
  int.mm <- as.numeric(int.m[, 3:26])
  class(int.mm) <- "numeric"
  
# add the occupied cells
  x[cell.list.x, ] <- nat.mm
  y[cell.list.y, ] <- int.mm   

# generate the raster object for each column -------------------------------
# I'll do all of them. It will be easier -_-
# 48 of them..
  
 setwd("C:/Users/s436862/Dropbox/Rarefaction/4. Results/Rarefaction/Rasters/Rarefied rasters complete") 
  
# Nat rarefied 15- to 50-records -------------------------------------------
# 15 - 30 (x rows 1-12)
  r15.n.tot <- setValues(aus, x[, 1])
  writeRaster(r15.n.tot, "r15.n.tot.grd", overwrite = T)
  plot(r15.n.tot)
  r15.n.c3 <- setValues(aus, x[, 2])
  writeRaster(r15.n.c3, "r15.n.c3.grd", overwrite = T)
  r15.n.c4 <- setValues(aus, x[, 3])
  writeRaster(r15.n.c4, "r15.n.c4.grd", overwrite = T)
  
  r20.n.tot <- setValues(aus, x[, 4])
  writeRaster(r20.n.tot, "r20.n.tot.grd", overwrite = T)
  r20.n.c3 <- setValues(aus, x[, 5])
  writeRaster(r20.n.c3, "r20.n.c3.grd", overwrite = T)
  r20.n.c4 <- setValues(aus, x[, 6])
  writeRaster(r20.n.c4, "r20.n.c4.grd", overwrite = T)
  
  r25.n.tot <- setValues(aus, x[, 7])
  writeRaster(r25.n.tot, "r25.n.tot.grd", overwrite = T)
  r25.n.c3 <- setValues(aus, x[, 8])
  writeRaster(r25.n.c3, "r25.n.c3.grd", overwrite = T)
  r25.n.c4 <- setValues(aus, x[, 9])
  writeRaster(r25.n.c4, "r25.n.c4.grd", overwrite = T)
  
  r30.n.tot <- setValues(aus, x[, 10])
  writeRaster(r30.n.tot, "r30.n.tot.grd", overwrite = T)
  r30.n.c3 <- setValues(aus, x[, 11])
  writeRaster(r30.n.c3, "r30.n.c3.grd", overwrite = T)
  r30.n.c4 <- setValues(aus, x[, 12])
  writeRaster(r30.n.c4, "r30.n.c4.grd", overwrite = T)
  
# 30 - 50 (x rows 13 - 24) ----
  r35.n.tot <- setValues(aus, x[, 13])
  writeRaster(r35.n.tot, "r35.n.tot.grd", overwrite = T)
  r35.n.c3 <- setValues(aus, x[, 14])
  writeRaster(r35.n.c3, "r35.n.c3.grd", overwrite = T)
  r35.n.c4 <- setValues(aus, x[, 15])
  writeRaster(r35.n.c4, "r35.n.c4.grd", overwrite = T)
  
  r40.n.tot <- setValues(aus, x[, 16])
  writeRaster(r40.n.tot, "r40.n.tot.grd", overwrite = T)
  r40.n.c3 <- setValues(aus, x[, 17])
  writeRaster(r40.n.c3, "r40.n.c3.grd", overwrite = T)
  r40.n.c4 <- setValues(aus, x[, 18])
  writeRaster(r40.n.c4, "r40.n.c4.grd", overwrite = T)
  
  r45.n.tot <- setValues(aus, x[, 19])
  writeRaster(r45.n.tot, "r45.n.tot.grd", overwrite = T)
  r45.n.c3 <- setValues(aus, x[, 20])
  writeRaster(r45.n.c3, "r45.n.c3.grd", overwrite = T)
  r45.n.c4 <- setValues(aus, x[, 21])
  writeRaster(r45.n.c4, "r45.n.c4.grd", overwrite = T)
  
  r50.n.tot <- setValues(aus, x[, 22])
  writeRaster(r50.n.tot, "r50.n.tot.grd", overwrite = T)
  r50.n.c3 <- setValues(aus, x[, 23])
  writeRaster(r50.n.c3, "r50.n.c3.grd", overwrite = T)
  r50.n.c4 <- setValues(aus, x[, 24])
  writeRaster(r50.n.c4, "r50.n.c4.grd", overwrite = T)
  
# Introduced 15- to 50-records -------------------------------
# 15 - 30 (y rows 1-12)
  r15.i.tot <- setValues(aus, y[, 1])
  writeRaster(r15.i.tot, "r15.i.tot.grd", overwrite = T)
  r15.i.c3 <- setValues(aus, y[, 2])
  writeRaster(r15.i.c3, "r15.i.c3.grd", overwrite = T)
  r15.i.c4 <- setValues(aus, y[, 3])
  writeRaster(r15.i.c4, "r15.i.c4.grd", overwrite = T)
  
  r20.i.tot <- setValues(aus, y[, 4])
  writeRaster(r20.i.tot, "r20.i.tot.grd", overwrite = T)
  r20.i.c3 <- setValues(aus, y[, 5])
  writeRaster(r20.i.c3, "r20.i.c3.grd", overwrite = T)
  r20.i.c4 <- setValues(aus, y[, 6])
  writeRaster(r20.i.c4, "r20.i.c4.grd", overwrite = T)
  
  r25.i.tot <- setValues(aus, y[, 7])
  writeRaster(r25.i.tot, "r25.i.tot.grd", overwrite = T)
  r25.i.c3 <- setValues(aus, y[, 8])
  writeRaster(r25.i.c3, "r25.i.c3.grd", overwrite = T)
  r25.i.c4 <- setValues(aus, y[, 9])
  writeRaster(r25.i.c4, "r25.i.c4.grd", overwrite = T)
  
  r30.i.tot <- setValues(aus, y[, 10])
  writeRaster(r30.i.tot, "r30.i.tot.grd", overwrite = T)
  r30.i.c3 <- setValues(aus, y[, 11])
  writeRaster(r30.i.c3, "r30.i.c3.grd", overwrite = T)
  r30.i.c4 <- setValues(aus, y[, 12])
  writeRaster(r30.i.c4, "r30.i.c4.grd", overwrite = T)
  
# 30 - 50 (y rows 13 - 24) ----
  r35.i.tot <- setValues(aus, y[, 13])
  writeRaster(r35.i.tot, "r35.i.tot.grd", overwrite = T)
  r35.i.c3 <- setValues(aus, y[, 14])
  writeRaster(r35.i.c3, "r35.i.c3.grd", overwrite = T)
  r35.i.c4 <- setValues(aus, y[, 15])
  writeRaster(r35.i.c4, "r35.i.c4.grd", overwrite = T)
  
  r40.i.tot <- setValues(aus, y[, 16])
  writeRaster(r40.i.tot, "r40.i.tot.grd", overwrite = T)
  r40.i.c3 <- setValues(aus, y[, 17])
  writeRaster(r40.i.c3, "r40.i.c3.grd", overwrite = T)
  r40.i.c4 <- setValues(aus, y[, 18])
  writeRaster(r40.i.c4, "r40.i.c4.grd", overwrite = T)
  
  r45.i.tot <- setValues(aus, y[, 19])
  writeRaster(r45.i.tot, "r45.i.tot.grd", overwrite = T)
  r45.i.c3 <- setValues(aus, y[, 20])
  writeRaster(r45.i.c3, "r45.i.c3.grd", overwrite = T)
  r45.i.c4 <- setValues(aus, y[, 21])
  writeRaster(r45.i.c4, "r45.i.c4.grd", overwrite = T)
  
  r50.i.tot <- setValues(aus, y[, 22])
  writeRaster(r50.i.tot, "r50.i.tot.grd", overwrite = T)
  r50.i.c3 <- setValues(aus, y[, 23])
  writeRaster(r50.i.c3, "r50.i.c3.grd", overwrite = T)
  r50.i.c4 <- setValues(aus, y[, 24])
  writeRaster(r50.i.c4, "r50.i.c4.grd", overwrite = T)
  
# -------------------------------------------------------------------
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
# Comparing nat versus int C3 and C4 --------------------------------------
  grass <- read.csv("C:/Users/s436862/Dropbox/Rarefaction/4. Results/Rarefaction/CSV/Rarefied richness nat int C3-4.csv", header = T)
  
  plot(grass$nat_total, grass$int_total)
  
  plot(grass$nat_c3, grass$int_c3)
  
  plot(grass$nat_c4, grass$int_c4)

# a lot of spread, but overall linear-ish






# Extras that Richard did -----------------------------------------------------------   
# compare with raw data                                                   
# number of native species by grid squares
spp_fj$pp <- factor(spp$pp)
poa_c3 <- filter(spp_fj, pp == "C3")
spp <- as.numeric(factor(poa_c3$species))
new_xy <- cbind(poa_c3$long, poa_c3$lat)
n_spp_nat <- rasterize(new_xy, raster, field = spp, fun = function(x,...) {length(unique(na.omit(x))) })

# number of introduced species by grid squares
poa_c4 <- filter(spp_fj, pp == "C4")
spp <- as.numeric(factor(poa_c4$species))
new_xy <- cbind(poa_c4$long, poa_c4$lat)
n_spp_int <- rasterize(new_xy, raster, field = spp, fun = function(x,...) {length(unique(na.omit(x))) })

par(mfrow = c(2, 2), mar = c(1, 1, 1, 1))
plot(log(n_spp_nat))
plot(est_rich_nat)
plot(n_spp_int)
plot(est_rich_int)


par(mfrow = c(2, 2), mar = c(4, 4, 1, 1))
plot(getValues(est_rich) ~ getValues(n_spp))
plot(getValues(est_rich_nat) ~ getValues(n_spp_nat))
plot(getValues(est_rich_int) ~ getValues(n_spp_int))
plot(getValues(est_rich_int) ~ getValues(est_rich_nat))

# place this into Rarefaction 4.0, c&p it; and then do the model selection stuff from my earlier work and what I went through on the camp. Cool. Ridge regression!
# Should be good. 

# ----------------------------------------------------------------------
 



