
# Date created: 25/2/18
# Last updated: 8/6

# Rarefaction estimator v3.0 -------------------------------------------------------
# The aim of this script is to suss out the best method for delivering Nat(c3/c4/total) & Int(c3/c4/total) into the Rarefaction v4.0 script (which itself will have all the other things for the chapter/paper)

# See associated METADATA for more info

# Library ------------------------------------------------------------------
  library(raster)
  library(oz)
  library(tidyverse)
  library(ggmap)
  library(tidyr)
  library(oz)
 
  rm(list = ls())

  setwd("C:/Users/s436862/Dropbox/Climate matching/1. Data files")
  
# 1. Rarefaction solo: Int -----------------------------------------------------------
# What is the maximum record-cutoff with the highest coverage for introduced grass species? 
# Data formatting --------------------------------------------------------------- 
# remember this will need to have the one with pp in the proper script or whereever you take this

# species    
  spp <- read.csv("Osborne C3-C4/AVH grass pp.csv", header = T) %>%
  ##  If doing pp stuff inlcude this: 
  #   mutate(pp = ifelse(pp == "C3 & C4", NA, as.character(pp))) %>%  
  #   mutate(pp = as.factor(pp)) %>%
  #   drop_na(pp)   
      filter(status == "introduced") %>%
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
    n <- length(sp)             # number of records           
    n_sp <- table(sp)           # number of records for each species    
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
    if(cell$n_rec[1] <= cutoff) out_rare[j] <- NA 
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

  
# ---------------------------------------------------------------------------------------
  
# 1. Rarefaction solo: Nat -----------------------------------------------------------
# What is the maximum record-cutoff with the highest coverage for introduced grass species? Well, it's probably 20-30, so I'll try these three for nat for comparison
# Data formatting --------------------------------------------------------------- 
# remember this will need to have the one with pp in the proper script or whereever you take this
  
# species    
  spp <- read.csv("Osborne C3-C4/AVH grass pp.csv", header = T) %>%
    ##  If doing pp stuff inlcude this: 
    #   mutate(pp = ifelse(pp == "C3 & C4", NA, as.character(pp))) %>%  
    #   mutate(pp = as.factor(pp)) %>%
    #   drop_na(pp)   
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
    n <- length(sp)             # number of records           
    n_sp <- table(sp)           # number of records for each species    
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
  
# 2. Rarefaction C3/C4 method -----------------------------------------------------------  
# Do native and introduced seperately, and for each, calculate the proportion of C3-C4
# Important note: because these are genera which are intermediate or known pp's, the data set used for this section will be a different to that calculated elsewhere
  
  setwd("C:/Users/s436862/Dropbox/Climate matching/1. Data files")
  
  rm(list = ls())
  
# Data formatting --------------------------------------------------------------- 
# Note this section is cut and pasted from the above section
# Only the rarefaction function will be different

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
  
# Rarefaction set-up -------------------------------------------------------   
# Subset into native and intrdocued statuses  
  spp_n <- filter(spp, status == "native") %>%
    dplyr::select(species, lat, long, pp, year) 
  
  spp_i <- filter(spp, status == "introduced") %>%
    dplyr::select(species, lat, long, pp, year) 
  
# Set one of these and run the script; repeat for the other ---------------
# spp <- spp_n
# spp <- spp_i
  
# number of records per cell ----------------------------------------------
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
    arrange(cell) %>%
    select(species, pp, year, cell, n_rec)
  
# cell numbers with total records in each
  cell_list <- as.numeric(levels(factor(spp2$cell)))
  
# Rarefied function  -----------------------------------------------------
# requires as input: 
# sp = a vector of all the records with species names & cell_id
# pp = a vector of photosynthetic pathway (C3 or C4) for each record (though its only genus-level)
# n = the number of samples for rarefaction
  rarefaction_split <- function(sp, pp, n) {
    N <- length(sp)            # number of records
    sp_n <- table(sp)          # number of records for each species
    
  # get the pp for each species in alphabetical order: 1 = C3, 0 = C4
    a <- unique(cbind(sp, pp))
    a <- a[order(a[, 1]), ]
    ne <- ifelse(a[, 2] == "C3", 1, 0)
    
    out <- numeric(length(sp_n))  # vector to store estimate for each species

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
  out_rare <- matrix(nrow = length(cell_list), ncol = 3)
  
# loop through all the cells, extract data from dataframe sp, and calculate rarefied richness using n_min as the number of samples for rarefaction
# for cells with less than or equal to n_min records, record as NA
  n_min <- 30 # either 25 or 30; let's go with 30 for now 
  
  for(j in 1:length(cell_list)) {
    cell <- filter(spp2, cell == cell_list[j])
    if(cell$n_rec[1] <= n_min) out_rare[j] <- NA else {
      spp <- as.character(cell$species)
      pp <- as.character(cell$pp)
      out_rare[j, ] <- rarefaction_split(spp, pp, n_min)
    }
  }
  
# put the rarefaction estimates into the dataframe and raster
# need to include the missing cell values as well as the occupied cells
# first make a matrix with all missing values
  x <- matrix(NA, nrow = length(getValues(raster)), ncol = 3)
# add the occupied cells
  x[cell_list, ] <- out_rare
  sum(x[,1], na.rm = T)
  
  sum(x[,1], na.rm = T)
  sum(x[,2], na.rm = T)
  sum(x[,3], na.rm = T)
  
# Save dataframe ------------------------------------------------
  # x_n <- x
  # x_i <- x
  colnames(x_n) <- c("nat_total", "nat_c3", "nat_c4")
  colnames(x_i) <- c("int_total", "int_c3", "int_c4")
  rarefied_rich <- cbind(x_n, x_i)
  
# remove extra-terrestrial cells 
  land_id <- read.csv("C:/Users/s436862/Dropbox/Climate Matching/1. Data files/EFs/3. EFs aggregated/Terrestrial categories.csv")
  
  rare_id <- cbind(land_id, rarefied_rich)
  rare_id2 <- rare_id
  
  rare_id2[rare_id$cell_category != "terrestrial", 3:8] <- NA
  table(rare_id2$cell_category, rare_id2$nat_total) # beautiful
  
# change cells where total = C3 (or C4) thereby the other one (C4 or C3) = 0, to = NA
  rare_id2[rare_id2 == "0"] <- NA
  table(rare_id2$cell_category, rare_id2$nat_total, exclude = F)
  
# right some wrongs  
  rare_id3 <- select(rare_id2, -cell_id, -cell_category)
  
  write.csv(rare_id3, "C:/Users/s436862/Dropbox/Rarefaction/4. Results/Rarefaction/CSV/Rarefied richness nat int C3-4.csv", row.names = F)
  
# Raster generation ----------------------------------------------------------  
  x <- read.csv("C:/Users/s436862/Dropbox/Rarefaction/4. Results/Rarefaction/CSV/Rarefied richness nat int C3-4.csv", header = T)
  
# Native rasters -------------------------------------------------------------  
# total richness  
  nat_est_rich <- setValues(raster, x[, 1])
  plot(nat_est_rich)
  
# C3 richness  
  nat_est_rich_c3 <- setValues(raster, x[, 2])
  plot(nat_est_rich_c3)

# C4 richness  
  nat_est_rich_c4 <- setValues(raster, x[, 3])
  plot(nat_est_rich_c4)

# proportion of C3-C4 species in each cell
  nat_ratio_pp <- setValues(raster, x[, 2]/x[, 3])
  plot(nat_ratio_pp)

# Introduced rasters ---------------------------------------------------------
# total richness  
  int_est_rich <- setValues(raster, x[, 4])
  plot(int_est_rich)
  
# C3 richness  
  int_est_rich_c3 <- setValues(raster, x[, 5])
  plot(int_est_rich_c3)
  
# C4 richness  
  int_est_rich_c4 <- setValues(raster, x[, 6])
  plot(int_est_rich_c4)
  
# proportion of C3-C4 species in each cell
  int_ratio_pp <- setValues(raster, x[, 5]/x[, 6])
  plot(int_ratio_pp)
  
  
# Save rasters ------------------------------------------------------------
# Native
  writeRaster(nat_est_rich, "C:/Users/s436862/Dropbox/Rarefaction/4. Results/Rarefaction/Rasters/Rarefied 30 pp/Nat rare total", overwrite = T)
  writeRaster(nat_est_rich_c3, "C:/Users/s436862/Dropbox/Rarefaction/4. Results/Rarefaction/Rasters/Rarefied 30 pp/Nat rare c3", overwrite = T)
  writeRaster(nat_est_rich_c4, "C:/Users/s436862/Dropbox/Rarefaction/4. Results/Rarefaction/Rasters/Rarefied 30 pp/Nat rare c4", overwrite = T)
  
# Introduced
  writeRaster(int_est_rich, "C:/Users/s436862/Dropbox/Rarefaction/4. Results/Rarefaction/Rasters/Rarefied 30 pp/Int rare total", overwrite = T)
  writeRaster(int_est_rich_c3, "C:/Users/s436862/Dropbox/Rarefaction/4. Results/Rarefaction/Rasters/Rarefied 30 pp/Int rare c3", overwrite = T)
  writeRaster(int_est_rich_c4, "C:/Users/s436862/Dropbox/Rarefaction/4. Results/Rarefaction/Rasters/Rarefied 30 pp/Int rare c4", overwrite = T)
 
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