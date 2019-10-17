# Date created: 14/3/18
# Last updated: 26/3

# Based on "Rarefaction estimator grass_messed with" (and ""Rarefaction estimator grass)
# and Single-scale SRE-DF for how I did the SREs

# next scripts: Rarefaction model (which will include correlation analysis) & Rarefaction raster plots (which will include correlation figures)


  library(raster)
  library(tidyverse)
  library(ggmap)
  library(tidyr)
  library(oz)

  rm(list = ls())
  
  setwd("C:/Users/s436862/Dropbox/Climate Matching/1. Data files")
  
################# Proportion of cells occupied: rarefaction EDA ######################
# Aim: decide where the appropriate ranges are for cell width & rarefaction cutoff points
# These are opposing forces: we want the highest 'cutoff' point (giving the biggest range of relative species richness between cells), but the lowest % of cells excluded, to have a good coverage of Australia
  
# This script is split into two sections because of the aggregate function: first is the 1 km dataframe, and second the 2-250 km one, with their merger at the end
  
  rm(list = ls())
  
  setwd("C:/Users/s436862/Dropbox/Climate Matching/1. Data files")
  
#####################  1km width dataframe #############################
# Aims: ----------------------------------------------------------------
# (1) compute 1 km cell width   
# (2) compute proportion of cells occupied (>0 records) at each width
# (3) compute proportion of cells occupied at different cell-record cutoffs (>25, >50, >100,>250, >500 records) at each width
  
# raster template (to remove sea-related NA values for land-occupied proportions)
  raster <- raster("EFs/EFs cropped/arid.grd")
  
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
  
  
##################### 2-500 km width dataframe ##########################
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
  
# output ----------------------------
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
  write.csv(out_prop, file = "C:/Users/s436862/Dropbox/Climate Matching/4. Results/Grass groups AVH/Multiscale_rare_cell_occupation.csv", row.names = F)

# ------------------------------------------------------------------------------------    
  
######################################################################################
######################### Rarefaction & SREs dataframe ###############################
# Couple species records to grid cell id for each cell width ------------------------
  rm(list = ls())
  
  setwd("C:/Users/s436862/Dropbox/Climate Matching/1. Data files")
  
# Poa species records 
  spp <- read.csv("AVH/AVH grass records.csv", header = T)
  xy <- cbind(spp$long, spp$lat)
  # note: these split into nat-int later, so we don't need Nat/Int categories
  # therefore, getting AVH into this will be a little different, and will require a different script
  
  aus <- raster("Australia raster/aus") # need complete aus coverage
  width <- 100 # chosen from 'Cells occupied %' from plot script based on this
  aus <- aggregate(aus, fac = width, fun = mean)
  values(aus) <- 1:ncell(aus)
  spp$cell <- raster:::extract(aus, xy)
  
# generating a 'useable' cell-record number table -------------------------- 
# select the columns we need
  spz <- spp %>%
    select(species, status, year, cell)
# number of records per cell
  n_rec <- table(spz$cell)
  nr <- data.frame(cell = as.numeric(names(n_rec)), n_rec = as.vector(n_rec))
# add number of records, per cell, to the dataframe
  spz <- full_join(spz, nr) 
# get a list of the cell numbers (that actually have records in them)
  cell_list <- as.numeric(levels(factor(spz$cell)))

  
# Rarefaction function -------------------------------------------------------
# requires as input: 
# sp = vector of records with species names & cell id
# status = vector of status (native or introduced) for all records in a cell
# n = the number of samples we are rarefying to (25, 50, 100 in this case)
  rare <- function(spz, status, n) {
    N <- length(spz)            # number of records
    sp_n <- table(spz)          # number of records for each species
    
    # get the status for each species in alphabetical order: 1 = introduced, 0 = native
    a <- unique(cbind(spz, status))
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
  # got some weird Error that occurs due to naming stuff 9even if environment cleared)
  # if this pops up, close everything else, and rerun

# Rarefacton output matrix ----------------------------------------------
# three columns for each cutoff: 1 = total richness, 2 = introduced richness, 3 = native richness
  out_rare_25 <- matrix(nrow = length(cell_list), ncol = 3)
  out_rare_50 <- matrix(nrow = length(cell_list), ncol = 3)
  out_rare_100 <- matrix(nrow = length(cell_list), ncol = 3)
  
# loop through all the cells, extract data from dataframe sp, and calculate rarefied richness using n_min as the number of samples for rarefaction
# for cells with less than or equal to n_min records, record as NA

  # n <- 25
  # n <- 50 
  # n <- 100
 
  for(j in 1:length(cell_list)) { # cell list = cells that have stuff in them
    cell <- filter(spz, cell == cell_list[j]) # grab records that do that
    if(cell$n_rec[1] <= n) out_rare_25[j] <- NA else {
      spp <- as.character(cell$species)
      status <- as.character(cell$status)
      out_rare_25[j, ] <- rare(spp, status, n)
    }
  } # finish loop
    # Not really sure how to loop through 3x cutoffs; doing it manually  

# Save dataframe --------------------------------------------------  
# bind
  rarefaction_cutoffs <- cbind(out_rare_25, out_rare_50, out_rare_100)
  
# include Aus dummy values to remove offshore islands and to create a raster-appropriate format
  arid1 <- raster("EFs/EFs cropped/arid")
  arid <- aggregate(arid1, fac = 100, fun = mean)
  arid_val <- getValues(arid)
 
# get our species dataframe (with missing values)  
  y <- matrix(NA, nrow = length(getValues(arid)), ncol = 9)
# add the occupied cells; what a guy....
  y[cell_list, ] <- rarefaction_cutoffs
# bind  
  q <- cbind(arid_val, y)

# where arid = NA and any spp cell doesn't, replace with NA
# which is the same as replacing the whole row with NA  
  q <- as.data.frame(q)
  q[is.na(q$arid_val), 2:10] <- NA
  q45 <- q[ , 2:10]
  colnames(q45) <- c("poa_25", "int_25", "nat_25", "poa_50", "int_50", "nat_50", "poa_100", "int_100", "nat_100")
  
# save the rarefaction dataframe  
  write.csv(q45, file = "C:/Users/s436862/Dropbox/Climate Matching/4. Results/Grass groups AVH/Rarefaction/CSV/rarefaction_Poa_Int_Nat_multi_cutoffs.csv", row.names = F) 

  
# Save rasters ---------------------------------------------------------
# Use q45, if generated it before, or load it
  setwd("C:/Users/s436862/Dropbox/Climate Matching/4. Results/Grass groups AVH/Rarefaction")
  y <- q45
  
# Poa (total) richness
# 25  
  poa_25 <- setValues(aus, y[, 1]) # genius
  plot(poa_25)
  writeRaster(poa_25, "Raster/Poa_rare_25", overwrite = T)    
# 50
  poa_50 <- setValues(aus, y[, 4]) # fourth row
  plot(poa_50)
  writeRaster(poa_50, "Raster/Poa_rare_50", overwrite = T) 
# 100
  poa_100 <- setValues(aus, y[, 7]) # seventh row
  plot(poa_100) # very interesting patterns here ... looks very sample-dominated
  writeRaster(poa_100, "Raster/Poa_rare_100", overwrite = T)
  
# native richness  
# 25  
  nat <- setValues(aus, y[, 3])
  plot(nat)
  writeRaster(nat, "Raster/Nat_rare_25", overwrite = T)  
# 50
  nat_50 <- setValues(aus, y[, 6]) # fourth row
  plot(nat_50)
  writeRaster(nat_50, "Raster/Nat_rare_50", overwrite = T) 
# 100
  nat_100 <- setValues(aus, y[, 9]) # seventh row
  plot(nat_100) # very interesting patterns here ... looks very sample-dominated
  writeRaster(nat_100, "Raster/Nat_rare_100", overwrite = T)
  
# introduced richness  
# 25  
  int_25 <- setValues(aus, y[, 2])
  plot(int_25)
  writeRaster(int_25, "Raster/Int_rare_25", overwrite = T)  
# 50
  int_50 <- setValues(aus, y[, 5]) 
  plot(int_50)
  writeRaster(int_50, "Raster/Int_rare_50", overwrite = T) 
# 100
  int_100 <- setValues(aus, y[, 8]) 
  plot(int_100) # very interesting patterns here ... looks very sample-dominated
  writeRaster(int_100, "Raster/Int_rare_100", overwrite = T)

# proportion of introduced species at 50
  ratio_ni <- setValues(aus, y[, 5]/y[, 4])
  plot(ratio_ni)
  writeRaster(ratio_ni, "Raster/IntProp_rare_50", overwrite = T)
 
# ------------------------------------------------------------------------------------  
    
#####################################################################################
########################## Baseline & SREs ##########################################
# SRES: Chao1, iChao1, Jack_2nd, Lanumteang (because of its insensitivity to sample-size)
# and no. of records + actual AVH richness (a)  -------------------------------

  rm(list=(ls()))

  setwd("C:/Users/s436862/Dropbox/Climate Matching/1. Data files")
  
# Species richness function --------------------------------
# x = AVH-type dataframe: species-records, lat/long -- see METADATA for more info  
# v = raster  
# z = cell size you want aggregated to; note this script won't work if you don't aggregate (can change this later if need)
  

  sre_function <- function(x, v, z) 
  {
  
  # putting records into cells    
    xy <- cbind(x$long, x$lat) 
    spp <- as.numeric(factor(x$species))
    raster <- aggregate(v, fact = z, fun = mean)
    
  # total number of records per cell (n)
    n <- rasterize(xy, raster, fun = function(x,...) {length(na.omit(x)) })
    
  # actual richness (a)
    a <- rasterize(xy, raster, field = spp, fun = function(x,...) {length(unique(na.omit(x))) })
  
  # single-, double-, triple- and quadruple-tons
  # (f1, f2, f3 and f4) 
    f1 <- rasterize(xy, raster, field = spp, fun = function(x,...) {length(which(table(x)==1)) })
    f2 <- rasterize(xy, raster, field = spp, fun = function(x,...) {length(which(table(x)==2)) })
    f3 <- rasterize(xy, raster, field = spp, fun = function(x,...) {length(which(table(x)==3)) })
    f4 <- rasterize(xy, raster, field = spp, fun = function(x,...) {length(which(table(x)==4)) })
    
  # retrieve cell values
  # actual record no. + richness
    n <- getValues(n)
    a <- getValues(a)

  # SRE correctives
    f1 <- getValues(f1)
    f2 <- getValues(f2)
    f3 <- getValues(f3)
    f4 <- getValues(f4)
  
  # remove offshore values & replace NAs with zeroes for SRE calculations
    raster_df <- getValues(raster)
    sre <- data.frame(raster_df, n, a, f1, f2, f3, f4)
    sre[is.na(sre$raster), 2:7] <- NA
    sre[is.na(sre)] <- 0
    sum(is.na(sre)) # cool
    
  # split them back up
    n <- sre$n
    a <- sre$a
    f1 <- sre$f1
    f2 <- sre$f2
    f3 <- sre$f3
    f4 <- sre$f4
    
  # chao1 --------------------
    chao1 <- ifelse(f2 == 0, 
             (a + (((n-1) / n) * (f1 * (f1-1)) / (2 * (f2+1)))),
             (a + (((n-1) / n) * ((f1^2)/(2*f2))))
      )
    # 'if' won't handle two conditional statements 
   
  # iChao1 --------------------
    # ichao1_f1 (no f2)   
      ichao1_f1 <- a + (((n-1)/n) * (f1 * (f1-1)) / (2 * (f2+1)))
    
    # ichao1_f2 
      ichao1_f2 <- a + (((n-1)/n) * ((f1^2)/(2*f2)))
    
    # ichao1_f3 (no f4)
      ichao1_f3 <- (a + ((f1^2)/(2*f2))) + 
        ((((n-3) / (4*n)) * (f3/(f4+1))) *
           (ifelse( (f1 - ((n-3) / (2*(n-1))) * (f2*f3/(f4+1))) > 0, 
                    (f1 - ((n-3) / (2*(n-1))) * (f2*f3/(f4+1))), 0 )))
    
    # ichao1_f4
      ichao1_f4 <- (a +  ((f1^2)/(2*f2))) + 
        ((((n-3) / (4*n)) * (f3/f4)) *
           (ifelse( (f1 - ((n-3) / (2*(n-1))) * (f2*f3/f4)) > 0, 
                    (f1 - ((n-3) / (2*(n-1))) * (f2*f3/f4)), 0 )))
    
    # complete ichao1 
    # doing this extremely properly gives the wrong answer, so I am going with less proper way
      ichao1 <-  ifelse(f2 == 0,
                       ichao1_f1,
                       ifelse(f3 == 0,
                              ichao1_f2,
                              ifelse(f4 == 0,
                                     ichao1_f3,
                                     ichao1_f4
                              )))
      
      # technically the better way, but gives Inf values
      # otherwise, all other values are identical, so going with 1st method
      # ichao1b <-  ifelse(f4 == 0,
      #                   ifelse(f3 == 0,
      #                          ifelse(f2 == 0,
      #                                 ichao1_f1,
      #                                 ichao1_f2),
      #                          ichao1_f3),
      #                   ichao1_f4)
                            
         

     
  # Jackknife second order --------------------
    jack_second <- a + 2*f1 - f2
    
  # Lanumteang & Bohning (2011) --------------------
    lanum_f3 <- a + (3*(f1^3)*f3)/(4*(f2^3)) 
    
  # no f3
    lanum_f2 <- a + (3*(f1^3))/(4*(f2^3)) 
    
  # no f2 or f3
    lanum_f1 <- a + (3/4 * (f1*(f1-1)*(f1-2)) / ((f2 + 1) * (f2 + 2) * (f2 + 3)))
    
  # altogether
    lanum <- ifelse(f2 == 0, 
                    lanum_f1,
                    ifelse(f3 == 0, 
                          lanum_f2, 
                          lanum_f3))
    rm(lanum_f3)
    mean(lanum_f1, na.rm = T)
    tail(table(lanum))
    
    funny <- cbind(n, a, f1, f2, f3, f4, lanum)
    colnames(funny) <- c("n", "a", "f1", "f2", "f3", "f4", "lanum")
    
  # ------------------ 
    sre_df <- data.frame(n, a, chao1, ichao1, jack_second, lanum)  
    
    return(sre_df)
    
  } # finish function
  
# run function for Poa, Nat and Int ----------------------------------  
# raster (v)
  v <- raster("EFs/EFS cropped/arid.grd")
# scale/width (z)
  z <- 100
  
# species records (x) + script + saving each clas
# Poa  
  x1 <- read.csv("AVH/AVH grass records.csv", header = T)
  sre_df_poa <- sre_function(x1, v, z) 
  write.csv(sre_df_poa, file = "C:/Users/s436862/Dropbox/Climate Matching/4. Results/Grass groups AVH/Rarefaction/CSV/Poa_SRE.csv", row.names = F)
# Nat  
  x2 <- read.csv("AVH/AVH grass records.csv", header = T) %>%
    dplyr::filter(status == "native")
  sre_df_nat <- sre_function(x2, v, z) 
  write.csv(sre_df_nat, file = "C:/Users/s436862/Dropbox/Climate Matching/4. Results/Grass groups AVH/Rarefaction/CSV/Nat_SRE.csv", row.names = F)
# Int
  x3 <- read.csv("AVH/AVH grass records.csv", header = T) %>%
    dplyr::filter(status == "introduced")
  sre_df_int <- sre_function(x3, v, z) 
  write.csv(sre_df_int, file = "C:/Users/s436862/Dropbox/Climate Matching/4. Results/Grass groups AVH/Rarefaction/CSV/Int_SRE.csv", row.names = F)
  
# save -------------------------------------  
  poa <- read.csv("C:/Users/s436862/Dropbox/Climate Matching/4. Results/Grass groups AVH/Rarefaction/CSV/Poa_SRE.csv", header = T)  
  nat <- read.csv("C:/Users/s436862/Dropbox/Climate Matching/4. Results/Grass groups AVH/Rarefaction/CSV/Nat_SRE.csv", header = T) 
  int <- read.csv("C:/Users/s436862/Dropbox/Climate Matching/4. Results/Grass groups AVH/Rarefaction/CSV/Int_SRE.csv", header = T) 
  
 
  
# SRE raster generation -----------------------------------------------
  setwd("C:/Users/s436862/Dropbox/Climate Matching/4. Results/Grass groups AVH/Rarefaction")
  
# dummy raster   
  aus <- raster("C:/Users/s436862/Dropbox/Climate Matching/1. Data files/Australia raster/aus") 
  width <- 100 
  aus <- aggregate(aus, fac = width, fun = mean)
  
# Poa rasters ------------------------------------------
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
  
 
# ----------------------------------------------------------------------------------------  
  
################################# Extras ######################################
# Poa SR at 50- and 200-km scales --------------------------------------------
  setwd("C:/Users/s436862/Dropbox/Climate Matching/1. Data files")
  
  poa <- read.csv("AVH/AVH grass records.csv", header = T)
  xy <- cbind(poa$long, poa$lat) 
  spp <- as.numeric(factor(poa$species))
  
  aus <- raster("Australia raster/aus")
  
# 50-km  
  width <- 50 
  aus1 <- aggregate(aus, fac = width, fun = mean)
  poa_50 <- rasterize(xy, aus1, field = spp, fun = function(x,...) {length(unique(na.omit(x))) })
  plot(poa_50)
  writeRaster(poa_50,"C:/Users/s436862/Dropbox/Climate Matching/4. Results/Grass groups AVH/Rarefaction/Raster/Poa_50km", overwrite = T)
  
# 200-km  
  width <- 200 
  aus2 <- aggregate(aus, fac = width, fun = mean)
  poa_200 <- rasterize(xy, aus2, field = spp, fun = function(x,...) {length(unique(na.omit(x))) })
  plot(poa_200)
  writeRaster(poa_200,"C:/Users/s436862/Dropbox/Climate Matching/4. Results/Grass groups AVH/Rarefaction/Raster/Poa_200km", overwrite = T)  
  
  
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
  
  setwd("C:/Users/s436862/Dropbox/Climate Matching/1. Data files")
  
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
  write.csv(ef_v_na, file = "C:/Users/s436862/Dropbox/Climate Matching/4. Results/Grass groups AVH/Rarefaction/CSV/EFs.csv", row.names = F)
  
# ----------------------------------------------------------------------------------------  
  
################################### SAC #################################################
# Species accumulation curves ----------------------------------------------------
# produced from "Why assymptotic estimators don't work" and beerbayes, actually. No. 3.   
  
# First I wat to make a plot of all the cells' communitites -- SR x record no.
# (then circle 4x comms I want to check out, using PP)
# 4x communitites in a 'z' type shape; basically, two high-records cells, of similar SR; and two low-record cells, of similar SR, as well.  
  
  rm(list = ls())
  
  setwd("C:/Users/s436862/Dropbox/Climate Matching/1. Data files")
  
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
  #ggsave("C:/Users/s436862/Dropbox/Climate Matching/4. Results/Grass groups AVH/Rarefaction/Graphs/Poa SR x Rec no.jpeg", plot = last_plot(), scale = 1, dpi = 500, device = "jpeg")  
  
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
  ggsave("C:/Users/s436862/Dropbox/Climate Matching/4. Results/Grass groups AVH/Rarefaction/Graphs/SAC_HT.jpeg", plot = last_plot(), scale = 1, dpi = 500, device = "jpeg")  
  
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
  ggsave("C:/Users/s436862/Dropbox/Climate Matching/4. Results/Grass groups AVH/Rarefaction/Graphs/SAC_HR.jpeg", plot = last_plot(), scale = 1, dpi = 500, device = "jpeg")  
  
  
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
  ggsave("C:/Users/s436862/Dropbox/Climate Matching/4. Results/Grass groups AVH/Rarefaction/Graphs/SAC_LR.jpeg", plot = last_plot(), scale = 1, dpi = 500, device = "jpeg")  
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
  ggsave("C:/Users/s436862/Dropbox/Climate Matching/4. Results/Grass groups AVH/Rarefaction/Graphs/SAC_LT.jpeg", plot = last_plot(), scale = 1, dpi = 500, device = "jpeg")    

# Ratio of f1:f2 across Australia raster -----------------------------------------
# let's make two things:
# (1) raster of ratio of f1:f2;
# (2) correlation between ratio & record no.
#     would this work? I am not sure. Maybe not as a raster, but the values, surely  
  
  library(car)
  
  setwd("C:/Users/s436862/Dropbox/Climate Matching/1. Data files")
  
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
  
  writeRaster(ratio, "C:/Users/s436862/Dropbox/Climate Matching/4. Results/Grass groups AVH/Rarefaction/Raster/F1-F2 ratio", overwrite = T)
  
  arid <- raster("C:/Users/s436862/Dropbox/Climate Matching/1. Data files/EFs/EFs cropped/arid")
  
  a_v <- getValues(a)
  ratio_v <- getValues(ratio)
  a_ra <- cbind(a_v, ratio_v)
  
  a_ratio_cor <- cor(a_ra, method = "pearson", use = "complete.obs")
  cor(ef, method = "pearson", use ="complete.obs") # not sure why you're not playing ball
  
  