
# Date created: 30/5
# Last updated: 

# Nat + Int Rarefaction script for building models

# Overall aim: use rarefaction on species richness using herbarium data
# this script will collate all of the data 

# Based on "SRE dataframe 3.0" and "Rarefaction estimator grass_messed with" (and ""Rarefaction estimator grass)
# and Single-scale SRE-DF for how I did the SREs

# Outcomes:
# rarefaction estimates for native and introdueced species, also broken into C3-C4, and done independently; as we have seen, including both was a bit shifty.
# Model and plots will be included as we go, too. 
# these will be variabile selection, horsehoe priors, maybe, and some stuff. She'll be right

  library(raster)
  library(oz)
  library(tidyverse)
  library(ggmap)
  library(tidyr)
  library(oz)
  library(ggcorrplot)
  library(corrplot)
  library(factoextra)
  library(car)
  
  rm(list = ls())
  
  setwd("C:/Users/s436862/Dropbox/Climate matching/1. Data files")
  
# 1. Rarefaction -----------------------------------------------
# Splitting Nat and int on separate analyses to include C3-C4

# 15-record C3-C4 split ----------------------------------------------
# Note this is based on: C3C4.R, which is in rarefaction, and is not very well done, I suspect  
# Required: 
# spp = species record list with lat/long
# raster = land area
# scale = scale by which raster will be converted to (assumed 1-km^2 raster and 10,000-km^2 scale)  
# cutoff = rarefied richness value  
  rm(list = ls())
  
# species data  
  spp <- read.csv("AVH/AVH grass records.csv", header = T) %>%
    dplyr::select(species, lat, long, year, status) 

# raster with appropriate cell size and each cell has a value (=ID)  
  raster <- raster("Australia/aus_100km_cell_id.grd")
 
# spp record lat/longs  
  xy <- cbind(spp$long, spp$lat)
  
# assign each point in the dataframe to raster cell
  spp$cell <- raster::extract(raster, xy)
  
# number of records per cell
  n_rec <- table(spp$cell)
  nr <- data.frame(cell = as.numeric(names(n_rec)), n_rec = as.vector(n_rec))
  
# add number of records, per cell, to the dataframe
# select the columns we need
  sp <- spp %>%
    select(species, status, year, cell)
  
# number of records per cell
  n_rec <- table(sp$cell)
  nr <- data.frame(cell = as.numeric(names(n_rec)), 
                   n_rec = as.vector(n_rec))
  
# add number of records per cell to the dataframe
  sp <- full_join(sp, nr)
  
# get a list of the cell numbers (that actually have records in them)
  cell_list <- as.numeric(levels(factor(sp$cell)))
  
# Split rarefaction function ------------------------------------------------------
# Requires: 
# sp = a vector of all the records with species names & cell_id
# status = a vector of status (c3- or c4-status) for all records in a cell
# n = the number of samples for rarefaction
  rare <- function(sp, status, n) {
    N <- length(sp)      # number of records
    sp_n <- table(sp)    # number of records for each species
    
# get the status for each species in alphabetical order: 1 = introduced, 0 = native
    a <- unique(cbind(sp, status))
    a <- a[order(a[, 1]), ]
    ne <- ifelse(a[, 2] == "introduced", 1, 0)
    
    out <- numeric(length(sp_n))# vector to store estimate for each species
    
# for each species, calculate the expected number of occurrences from n records
    for(i in 1:length(sp_n)) {
      out[i] <- 1 - exp(lchoose((N - sp_n[i]), n) - lchoose(N, n))    
# use lchoose (i.e. log scale) to avoid problems with big numbers
    }
# output estimated total richness, and estimated richness of introduced and native species
    return(c(round(sum(out)), round(sum(out[ne == 1])), round(sum(out[ne == 0]))))
  }

# Run function ----------------------------------------------------------
# matrix to store output: one row for each cell, 3 columns: 1 = total richness, 2 = introduced richness, 3 = native richness
  out_rare <- matrix(nrow = length(cell_list), ncol = 3)
  
# loop through all the cells, extract data from dataframe sp, and calculate rarefied richness using n_min as the number of samples for rarefaction
# for cells with less than or equal to n_min records, record as NA
  n_min <- 15
  
  for(j in 1:length(cell_list)) {
    cell <- filter(sp, cell == cell_list[j])
    if(cell$n_rec[1] <= n_min) out_rare[j] <- NA else {
      spp <- as.character(cell$species)
      status <- as.character(cell$status)
      out_rare[j, ] <- rare(spp, status, n_min)
    }
  }
  
# put the rarefaction estimates into the raster
# need to include the missing cell values as well as the occupied cells
# first make a matrix with all missing values
  x <- matrix(NA, nrow = length(getValues(raster)), ncol = 3)
# add the occupied cells
  x[cell_list, ] <- out_rare
# generate the raster object for total richness  
  est_rich <- setValues(raster, x[, 1])
  plot(est_rich)
  
# generate the raster object for introduced richness  
  est_rich_int <- setValues(raster, x[, 2])
  plot(est_rich_int)
  
# generate the raster object for native richness  
  est_rich_nat <- setValues(raster, x[, 3])
  plot(est_rich_nat)
  
# 
# Generate rarefaction data frame -------------------------------------  
# cell_id | nat_rare | int_rare
  cell_id <- getValues(raster)
  nat_rare <- getValues(est_rich_nat)
  int_rare <- getValues(est_rich_int)
  
  rare_df <- as.data.frame(cbind(cell_id, nat_rare, int_rare))
  
  write.csv(rare_df, file = "C:/Users/s436862/Dropbox/Rarefaction/4. Results/Rarefaction/CSV/Nat_Int rarefaction.csv", row.names = F)
  
# Summary stats on photosynthetic pathways -----------------------------
  spp <- read.csv("C:/Users/s436862/Dropbox/Rarefaction/4. Results/Rarefaction/CSV/Nat_Int rarefaction.csv", header = T)
  
  c3 <- cor(spp$nat_c3, spp$int_c3, use = "complete.obs")
  c4 <- cor(spp$nat_c4, spp$int_c4, use = "complete.obs")
  tot <- cor(spp$nat_total, spp$int_total, use = "complete.obs")
  
  pp.cor <- cbind(tot, c3, c4)
  
  write.csv(pp.cor, file = "C:/Users/s436862/Dropbox/Rarefaction/4. Results/Rarefaction/CSV/C3_4 Nat_Int_total correlations.csv", row.names = F)
  
# ----------------------------------------------------------------------  

# 2. Rarefaction NATIVE ONLY -----------------------------------------------------
# Required ------------------------------------------------------------ 
# spp = species record list with lat/long
# raster = land area
# scale = scale by which raster will be converted to (assumed 1-km^2 raster and 10,000-km^2 scale)# cutoff = rarefied richness value 
  rm(list = ls())
  
  setwd("C:/Users/s436862/Dropbox/Climate matching/1. Data files")
  
  spp <- read.csv("AVH/AVH grass records.csv", header = T) %>%
    dplyr::filter(status == "native")   %>%
    dplyr::select(species, lat, long, year) 
  cutoff <- 15
  raster_agg <- raster("Australia/aus_100km.grd")
 
# number of records by grid squares
  xy <- cbind(spp$long, spp$lat)
# assign each point in the dataframe to raster cell
  spp$cell <- raster::extract(raster_agg, xy)
  
# Total records per cell (check 1)------------------------------------------------
# raster of total records per cell
  n_tot <- rasterize(xy, raster_agg, fun = function(x,...) length(x))
  plot(log10(n_tot))
  length(getValues(n_tot))      
# number of records per cell
  n_rec <- table(spp$cell)
  nr <- data.frame(cell = as.numeric(names(n_rec)), n_rec = as.vector(n_rec))
# Check (that n and o.g. df line up?) -- yes
  check1 <- spp %>%
    group_by(cell) %>%
    summarise(n_rec = n()) # adding this n_rec clause
  x <- getValues(n_tot)
  rv <- data.frame(cell = 1:length(x), n_tot = x)
  ch_rv <- full_join(check1, rv) # they line up if you view this
  plot(log(n_tot) ~ log(n_rec), data = ch_rv) # they're the same
  
# Number of species per cell (check 2) ------------------------------------------  
# number of species per cell
  spp_per_cell <- as.numeric(factor(spp$species))
  n_spp <- rasterize(xy, raster_agg, field = spp_per_cell, fun = function(x,...) {length(unique(na.omit(x))) })
  plot(n_spp)
  
# check this (spp richness per cell)
  check2 <- spp %>%
    group_by(cell) %>%
    summarise(n = length(unique(species))) 
  
  x <- getValues(n_spp)
  rv <- data.frame(cell = 1:length(x), n_tot = x)
  ch_rv <- full_join(check2, rv) # line up all goods
  plot(log(n) ~ log(n_tot), data = ch_rv) # lines up 
  
# add number of records, per cell, to the dataframe
  spp <- full_join(spp, nr) %>%
    arrange(cell) %>%
    select(cell, species, lat, long, year, n_rec)
# cell numbers with total records in each
  cell_list <- as.numeric(levels(factor(spp$cell)))
# Species richness versus number of records per cell (check 3) ------------------
  spp_cell_id <- data.frame(n_spp = getValues(n_spp),
                            n_tot = getValues(n_tot))
  
  ggplot(spp_cell_id, aes(y = n_spp, x = n_tot)) +
    geom_point() +
    geom_abline(intercept = 0, slope = 1) +
    scale_x_log10() +
    scale_y_log10() +
    theme_bw()
  
# Rarefaction NATIVE function ------------------------------------------------
# for nomenclature's sake, I will convert to Richard's naming of things
# so the only things I am changing is the status part & the 'n' names in the function
  sp <- spp %>%
    select(species, year, cell)
  
# number of records per cell
  n_rec <- table(sp$cell)
  nr <- data.frame(cell = as.numeric(names(n_rec)), n_rec = as.vector(n_rec))
  
# add number of records per cell to the dataframe
  sp <- full_join(sp, nr)
  
# get a list of the cell numbers (that actually have records in them)
  cell_list <- as.numeric(levels(factor(sp$cell)))
  
  rarefaction <- function(sp, cutoff) 
  { 
    n <- length(sp)         # number of records           
    n_sp <- table(sp)       # number of records for each species    
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
  out_rare <- matrix(nrow = length(cell_list)) # length 1147 (not 2538)
  cutoff <- 15
  
  for(j in 1:length(cell_list)) { 
    cell <- dplyr::filter(spp, cell == cell_list[j]) # return only the cells with records in them
    sp <- as.character(cell$species)
    if(cell$n_rec[1] <= cutoff) out_rare[j] <- NA 
    else { out_rare[j, ] <- rarefaction(sp, cutoff) 
    }
  }
  
  
# Produce dataframe --------------------------------------  
# need to include the missing cell values as well as the occupied cells
# first make a matrix with all missing values
  rarefied_rich <- matrix(NA, nrow = length(getValues(raster_agg)))
# add the occupied cells
  rarefied_rich[cell_list, ] <- out_rare
# generate the raster object for total richness  
  est_rich <- setValues(raster_agg, rarefied_rich[, 1])
  plot(est_rich)
# looking gooooooooood
  
# Keep only this bit (because otherwise a gives me weird SR values)
  
  rarefaction <- data.frame(rarefied_rich)
  
  write.csv(rarefaction, file = "C:/Users/s436862/Dropbox/Rarefaction/4. Results/Rarefaction/CSV/Nat_30 rarefaction.csv", row.names = F)
# ----------------------------------------------------------------------------

  

