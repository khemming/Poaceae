# Date created: 25/2/18
# Last updated: 

  library(raster)
  library(oz)
  library(tidyverse)
  library(ggmap)
  library(tidyr)
  library(oz)
 
  rm(list = ls())
  
  setwd("C:/Users/s436862/Dropbox/Climate Matching/1. Data files")
  
# species records    
  poa <- read.csv("AVH/AVH grass records.csv", header = T)
  
# Rasterise data --------------------------------------
# can do this in three scales, btw  
  aus <- raster("australia raster/aus.grd")
  aus_100 <- aggregate(aus, fac = 100, fun = mean)
  values(aus_100) <- 1:ncell(aus_100)
  
# number of records by grid squares
  xy <- cbind(poa$long, poa$lat)
  
# assign each point in the dataframe to raster cells of 100 km
  poa$cell_id_100 <- raster:::extract(aus_100, xy) # Richard, you're a genius ...
  poa$cell <- poa$cell_id_100
  aus <- aus_100
  
# raster of total records per cell
  n_tot <- rasterize(xy, aus, fun = function(x,...) length(x))
  plot(log10(n_tot))
  length(getValues(n_tot))
  
# Check (that n and o.g. df line up?) -- yes
  check1 <- poa %>%
        group_by(cell) %>%
        summarise(n_rec = n()) # adding this n_rec clause
  
  x <- getValues(n_tot)
  rv <- data.frame(cell = 1:length(x), n_tot = x)
  ch_rv <- full_join(check1, rv) # they line up if you view this
  plot(log(n_tot) ~ log(n_rec), data = ch_rv)      
  # they're the same
  
# number of species per cell
  spp <- as.numeric(factor(poa$species))
  n_spp <- rasterize(xy, aus, field = spp, fun = function(x,...) {length(unique(na.omit(x))) })
  plot(n_spp)

# check this (spp richness per cell)
  check2 <- poa %>%
        group_by(cell) %>%
        summarise(n = length(unique(species))) 
        
  x <- getValues(n_spp)
  rv <- data.frame(cell = 1:length(x), n_tot = x)
  ch_rv <- full_join(check2, rv) # line up all goods
  plot(log(n) ~ log(n_tot), data = ch_rv)      
  # lines up 


# species richness versus number of records per cell
# (what Andrew and I tried to make)  
  spp_cell_id <- data.frame(n_spp = getValues(n_spp),
                            n_tot = getValues(n_tot))
                        
  ggplot(spp_cell_id, aes(y = n_spp, x = n_tot)) +
    geom_point() +
    geom_abline(intercept = 0, slope = 1) +
    scale_x_log10() +
    scale_y_log10() +
    theme_bw()
    # Cool
  
  
# generating a 'useable' cell-record number table -------------------- 
  
# select the columns we need
  sp <- poa %>%
        select(species, status, year, cell)
  
# number of records per cell
  n_rec <- table(sp$cell)
  nr <- data.frame(cell = as.numeric(names(n_rec)), n_rec = as.vector(n_rec))
  # not sure how you did this ... (amazing)
  
# add number of records per cell to the dataframe
  sp <- full_join(sp, nr)
  # mind = blown
  
# get a list of the cell numbers (that actually have records in them)
  cell_list <- as.numeric(levels(factor(sp$cell)))
  
################################################################################
# function to calculate rarefaction estimate of number of species (Hurlbert, 1971; +/- Chao's Hill numbers)
  
# requires as input: 
# sp = a vector of all the records with species names & cell_id
# status = a vector of status (native or introduced) for all records in a cell
# n = the number of samples for rarefaction
  rare <- function(sp, status, n) {
    N <- length(sp)            # number of records
    sp_n <- table(sp)          # number of records for each species
    
  # get the status for each species in alphabetical order: 1 = introduced, 0 = native
    a <- unique(cbind(sp, status))
    a <- a[order(a[, 1]), ]
    ne <- ifelse(a[, 2] == "introduced", 1, 0)
    
    out <- numeric(length(sp_n))  # vector to store estimate for each species

    # for each species, calculate the expected number of occurrences from n records
    for(i in 1:length(sp_n)) {
      out[i] <- 1 - exp(lchoose((N - sp_n[i]), n) - lchoose(N, n))    
      # use lchoose (i.e. log scale) to avoid problems with big numbers
    }
    # output estimated total richness, and estimated richness of introduced and native species
    return(c(round(sum(out)), round(sum(out[ne == 1])), round(sum(out[ne == 0]))))
  }

################################################################################

# matrix to store output: one row for each cell, 3 columns: 1 = total richness, 2 = introduced richness, 3 = native richness
  out_rare <- matrix(nrow = length(cell_list), ncol = 3)
  
# loop through all the cells, extract data from dataframe sp, and calculate rarefied richness using n_min as the number of samples for rarefaction
# for cells with less than or equal to n_min records, record as NA

  n_min <- 50
  
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
  x <- matrix(NA, nrow = length(getValues(aus)), ncol = 3)
# add the occupied cells
  x[cell_list, ] <- out_rare
# generate the raster object for total richness  
  est_rich <- setValues(aus, x[, 1])
  plot(est_rich)
  
# generate the raster object for introduced richness  
  est_rich_int <- setValues(aus, x[, 2])
  plot(est_rich_int)

# generate the raster object for native richness  
  est_rich_nat <- setValues(aus, x[, 3])
  plot(est_rich_nat)

# proportion of introduced species in each cell
  ratio_ne <- setValues(aus, x[, 2]/x[, 1])
  plot(ratio_ne)
  
  par(mfrow = c(2, 2), mar = c(1, 1, 1, 1))
  plot(est_rich)
  plot(est_rich_nat)
  plot(est_rich_int)
  plot(ratio_ne)
  
# compare with raw data                                                   
# number of native species by grid squares
  poa_nat <- filter(poa, status == "native")
  spp <- as.numeric(factor(poa_nat$species))
  new_xy <- cbind(poa_nat$long, poa_nat$lat)
  n_spp_nat <- rasterize(new_xy, aus, field = spp, fun = function(x,...) {length(unique(na.omit(x))) })

# number of introduced species by grid squares
  poa_int <- filter(poa, status == "introduced")
  spp <- as.numeric(factor(poa_int$species))
  new_xy <- cbind(poa_int$long, poa_int$lat)
  n_spp_int <- rasterize(new_xy, aus, field = spp, fun = function(x,...) {length(unique(na.omit(x))) })

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
  
# next step is it do this for all my data? In proper rasters, actually... 
  # look at the consequences of putting this into a model or two