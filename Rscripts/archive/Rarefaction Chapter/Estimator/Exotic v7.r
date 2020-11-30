  
# Date created: 31/7/18
# Last updated: 12/3/19
  
# update from v5: we are now using ALA grass data produced from 'ALA cleaned data' script
# this script is based off Richard's version (v1.R) and secondly v5 of this script
  
# library -------------------------------------------------------------------------
  library(tidyverse)
  library(raster)
  library(rgdal)
  
  setwd("C:/Users/s436862/Dropbox/Rarefaction/Data files")
  
  rm(list = ls())
  
# 1. data and exploration  -----------------------------------------------------------------------------
# poaceae
  dat <- readRDS("ALA/2019 ALA master data/master grass data.rds") %>%
    dplyr::select(-sub.family) %>% 
    filter(status == "exotic")
  
# australia 
  raster <- raster("Australia/aus 100 km v2.grd")
  plot(raster)
  
# 1.1 number of records per cell across Australia --------------------------------------------
  xy <- cbind(dat$longitude, dat$latitude)
  
# assign raster cell to each point
  dat$cell <- raster::extract(raster, xy)
  
# total records per cell
  n_tot <- rasterize(xy, raster, fun = function(x, ...) length(x))
  plot(log10(n_tot))
  
# record-coverage check
  ch <- dat %>%
    group_by(cell) %>%
    summarise(n_rec = n())
  
  x <- getValues(n_tot) 
  rv <- data.frame(cell = 1:length(x), n_tot = x)
  ch.rv <- full_join(ch, rv)
  plot(log(n_tot) ~ log(n_rec), data = ch.rv)
  
# 1.3 number of species per cell across Australia --------------------------------------------
  spp <- as.numeric(factor(dat$species))  
  n_spp <- rasterize(xy, raster, field = spp, fun = function(x, ...) {length(unique(na.omit(x))) })
  plot(n_spp)
  
# species richness check
  ch <- dat %>%
    group_by(cell) %>%
    summarise(n = length(unique(species)))
  
  x <- getValues(n_spp)  
  rv <- data.frame(cell = 1:length(x), n_tot = x)  
  ch.rv <- full_join(ch, rv)  
  plot(log(n) ~ log(n_tot), data = ch.rv)  
  
# record vs species richness
  all.val <- data.frame(n_spp = getValues(n_spp),
                        n_tot = getValues(n_tot))
  
  ggplot(all.val, aes(y = n_spp, x = n_tot)) +
    geom_point() +
    geom_abline(intercept = 0, slope = 1) +
    scale_x_log10() +
    scale_y_log10() +
    theme_bw() # not quite assymptoting, eh ... (but looks grand)
  
# 2. rarefaction -------------------------------------------------------------
# 2.1 rarefaction set-up -----------------------------------------------------
# records per cell
  n_rec <- table(dat$cell)
  nr <- data.frame(cell = as.numeric(names(n_rec)), n_rec = as.vector(n_rec))
  
# add number of records per cell to the dataframe
  dat <- full_join(dat, nr) %>%
    arrange(cell) %>%
    dplyr::select(species, pp, year, cell, n_rec)
  
# list of all the cells with spp data in them
  cell.list <- as.numeric(levels(factor(dat$cell)))
  
# 2.2 rarefaction function ---------------------------------------------------
# requires as input: sp = a vector of the species names corresponding to all records in a cell
#                    key = a vector of a key that partitions total rarefied richness for a given cell (i.e. pp (C3/C4) or family (A - E))
#                    n = subsample size
  
  rare_pp <- function(sp, key, n) {
    N <- length(sp)         # number of records
    sp_n <- table(sp)       # number of records for each species
    
  # get the key for each species in alphabetical order; in this case 1 = C3, 2 = c4
    a <- unique(cbind(sp, key))
    a <- a[order(a[, 1]), ]
    a_key <- ifelse(a[, 2] == "C3", 1, 0)
    
  # so far: 
  # status = key
  # ne = a_key
    
    out <- numeric(length(sp_n)) # vector to store estimate for each species
    
  # for each species, calculate the expected nymber of occurrences from n records
    for(i in 1:length(sp_n)) {
      out[i] <- 1 - exp(lchoose((N - sp_n[i]), n) - lchoose(N, n))# use lchoose (i.e. log scale) to avoid problems with tabling big numbers
    }
  # output estimated total richness, and richness of key groups; in this case, pp C3 & C4
    return(c(sum(out), sum(out[a_key == 1]), sum(out[a_key == 0])))
    
  }
  
# 2.3 run rarefaction function ------------------------------------------------  
# matrix to store function output: row for each cell, column for total, C3, C4 richness
  n.min <- seq(10, 50, 5)
  out.rare <- matrix(nrow = length(cell.list), ncol = 3)
  
# multi-cutoff (array; 2538, 3, 9)
  a <- array(dim = c(length(raster), ncol(out.rare), length(n.min)))
  
  for(i in 1:length(n.min)) {
    
    rec.no <- n.min[i]
    
    for(j in 1:length(cell.list)) {
      cell <- filter(dat, cell == cell.list[j])
      if(cell$n_rec[1] < n.min) out.rare[j] <- NA else {
        spp <- as.character(cell$species)
        key <- as.character(cell$pp)          
        out.rare[j, ] <- rare_pp(spp, key, rec.no)
      }
    }
    
    m <- matrix(NA, nrow = length(getValues(raster)), ncol = 3)
    m[cell.list, ] <- out.rare                         
    
    a[,,i] <- m
    
  } # fun end
  
# 2.4 generate array ------------------------------------------------
  poa.array <- a
  
# add in column names
  dimnames(poa.array)[[2]] <- c("total", "C3", "C4")
  dimnames(poa.array)[[3]] <- c("rare.10", "rare.15", "rare.20", "rare.25", "rare.30",
                                "rare.35", "rare.40", "rare.45", "rare.50")
  poa.df <- as.data.frame(poa.array)  
  
# remove oceanic cells -----------------------------------------------------------
  cell.cat <- read.csv("Australia/aus 100 km v2.csv", header = T) %>%
    dplyr::select(-prop.cover, -cell.category.v1)
  poa.land <- cbind(cell.cat, poa.df)
  poa.terr <- filter(poa.land, cell.category.v2 == "land")
  
# generate list of occupied cells
  cell.list.x <- poa.terr$cell.id
  
# make a matrix with all missing values (to get from 1003 cells to 2538)
  x <- matrix(NA, nrow = length(raster), ncol = 27)
  
  poa.m <- as.matrix(poa.terr)
  poa.mm <- poa.m[, 3:29]
  class(poa.mm) <- "numeric"
  
# add the occupied cells
  x[cell.list.x, ] <- poa.mm
  
  colnames(x) <- colnames(poa.df)
  
# save data frames
  write.csv(x, "C:/Users/s436862/Dropbox/Rarefaction/Results/Rarefaction/CSV/Rarefied exotic richness 10 to 50 records.csv", row.names = F)
  
# 3. generate rasters ----------------------------------------------------------
  setwd("C:/Users/s436862/Dropbox/Rarefaction/Results/Rarefaction/Rasters/Rarefied rasters") 
  
  r10.e.tot <- setValues(raster, x[, 1])
  writeRaster(r10.e.tot, "r10.e.tot.grd", overwrite = T)
  r10.e.c3 <- setValues(raster, x[, 2])
  writeRaster(r10.e.c3, "r10.e.c3.grd", overwrite = T)
  r10.e.c4 <- setValues(raster, x[, 3])
  writeRaster(r10.e.c4, "r10.e.c4.grd", overwrite = T)
  
  r15.e.tot <- setValues(raster, x[, 4])
  writeRaster(r15.e.tot, "r15.e.tot.grd", overwrite = T)
  r15.e.c3 <- setValues(raster, x[, 5])
  writeRaster(r15.e.c3, "r15.e.c3.grd", overwrite = T)
  r15.e.c4 <- setValues(raster, x[, 6])
  writeRaster(r15.e.c4, "r15.e.c4.grd", overwrite = T)
  
  r20.e.tot <- setValues(raster, x[, 7])
  writeRaster(r20.e.tot, "r20.e.tot.grd", overwrite = T)
  r20.e.c3 <- setValues(raster, x[, 8])
  writeRaster(r20.e.c3, "r20.e.c3.grd", overwrite = T)
  r20.e.c4 <- setValues(raster, x[, 9])
  writeRaster(r20.e.c4, "r20.e.c4.grd", overwrite = T)
  
  r25.e.tot <- setValues(raster, x[, 10])
  writeRaster(r25.e.tot, "r25.e.tot.grd", overwrite = T)
  r25.e.c3 <- setValues(raster, x[, 11])
  writeRaster(r25.e.c3, "r25.e.c3.grd", overwrite = T)
  r25.e.c4 <- setValues(raster, x[, 12])
  writeRaster(r25.e.c4, "r25.e.c4.grd", overwrite = T)
  
  r30.e.tot <- setValues(raster, x[, 13])
  writeRaster(r30.e.tot, "r30.e.tot.grd", overwrite = T)
  r30.e.c3 <- setValues(raster, x[, 14])
  writeRaster(r30.e.c3, "r30.e.c3.grd", overwrite = T)
  r30.e.c4 <- setValues(raster, x[, 15])
  writeRaster(r30.e.c4, "r30.e.c4.grd", overwrite = T)
  
# 35 - 50 (exotic rows 16 - 27)
  r35.e.tot <- setValues(raster, x[, 16])
  writeRaster(r35.e.tot, "r35.e.tot.grd", overwrite = T)
  r35.e.c3 <- setValues(raster, x[, 17])
  writeRaster(r35.e.c3, "r35.e.c3.grd", overwrite = T)
  r35.e.c4 <- setValues(raster, x[, 18])
  writeRaster(r35.e.c4, "r35.e.c4.grd", overwrite = T)
  
  r40.e.tot <- setValues(raster, x[, 19])
  writeRaster(r40.e.tot, "r40.e.tot.grd", overwrite = T)
  r40.e.c3 <- setValues(raster, x[, 20])
  writeRaster(r40.e.c3, "r40.e.c3.grd", overwrite = T)
  r40.e.c4 <- setValues(raster, x[, 21])
  writeRaster(r40.e.c4, "r40.e.c4.grd", overwrite = T)
  
  r45.e.tot <- setValues(raster, x[, 22])
  writeRaster(r45.e.tot, "r45.e.tot.grd", overwrite = T)
  r45.e.c3 <- setValues(raster, x[, 23])
  writeRaster(r45.e.c3, "r45.e.c3.grd", overwrite = T)
  r45.e.c4 <- setValues(raster, x[, 24])
  writeRaster(r45.e.c4, "r45.e.c4.grd", overwrite = T)
  
  r50.e.tot <- setValues(raster, x[, 25])
  writeRaster(r50.e.tot, "r50.e.tot.grd", overwrite = T)
  r50.e.c3 <- setValues(raster, x[, 26])
  writeRaster(r50.e.c3, "r50.e.c3.grd", overwrite = T)
  r50.e.c4 <- setValues(raster, x[, 27])
  writeRaster(r50.e.c4, "r50.e.c4.grd", overwrite = T)
  
# -------------------------------------------------------------------------------  