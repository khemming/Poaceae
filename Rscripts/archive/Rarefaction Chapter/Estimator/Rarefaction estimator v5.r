
# Date created: 31/7/18
# Last updated: 5/2/19

# This is the rarefaction methodology (rarefied cutoffs, photosynthetic pathway (pp), nat/exo), all rolled into to one script.
# The last script (SRE methods) was getting a bit large. 

# We have: 
# 1.0 Rarefaction, split for native/exotic status, and photosynthetic pathway, at a variety of rarefied richnesses (15-50 records)
# 1.1 Rasters of each of the 48 derived richness estimates
# 2.0 Rarefaction using total records for 10-50 records
# 2.1 Correlating 10-50 and percentage coverage

# and added from version 5:
# 3.0 sampling effort correlations
# I.e. how record #, species richness, Chao1 and rarefaction correlation to record number at 100-km scale
# 3.4 rarefaction at 15-record scale, for pp-tagged native and exotic species

# 0. Library -------------------------------------------------------------------------
  library(raster)
  library(oz)
  library(ggmap)
  library(tidyr)
  library(oz)
  library(dplyr)
  
  rm(list = ls())
  
  setwd("C:/Users/s436862/Dropbox/Rarefaction/Data files")
  
# 1.0 Rarefaction: 15-50 records, photosynthetic pathway, native/exotic richnesses ----------------------
  # rm(list=setdiff(ls(), "native.array")) 
  # rm(list=setdiff(ls(), "introduced.array")) 
  
# Data formatting --------------------------------------------------------------------------- 
  spp <- read.csv("Osborne C3-C4/AVH grass pp.csv", header = T) %>%
                  filter(pp == "C3" | pp == "C4") # removing 'unknown' and 'mixed' pp classes
 
  head(table(spp$species, spp$pp, exclude = NULL))
  glimpse(spp)
  
# -------------------------------------------------------------------------
# 1.01 Side quest: Calculating records used in this study (prior to rarefaction cut-off) --------------------
# This is therefore plotting the number of native and exotic records that we have C3/C4 tags for, than aren't excluded from the 1003-terrestrial cutoff bit. 
# This will involve rasterising raw records for native & exotic's C3/C4s.

# raster  
  raster <- raster("Australia/aus_100km_cell_id.grd")

# Subset into native and introduced statuses
  spp.n <- filter(spp, status == "native") %>%
    dplyr::select(species, lat, long, pp, year) 

  xy <- cbind(spp.n$long, spp.n$lat)
  n.tot <- rasterize(xy, raster, fun = function(x,...) length(x))
  plot(n.tot)
  n.tot.val <- getValues(n.tot)
  
  spp.i <- filter(spp, status == "introduced") %>%
    dplyr::select(species, lat, long, pp, year) 
  
  xy <- cbind(spp.i$long, spp.i$lat)
  i.tot <- rasterize(xy, raster, fun = function(x,...) length(x))
  plot(i.tot)
  i.tot.val <- getValues(i.tot)
  
# Exclude oceanic cells  
  cell.cat <- read.csv("C:/Users/s436862/Dropbox/Climate Matching/1. Data files/EFs/CSV/Terrestrial land categories.csv", header = T)
  
  poa.rec.tot <- cbind(cell.cat, n.tot.val, i.tot.val) %>%
    filter(cell.cat == "terrestrial")
  
# Sum  
  sum(poa.rec.tot$n.tot.val, na.rm = T) # 196668
  sum(poa.rec.tot$i.tot.val, na.rm = T) # 45181

# -------------------------------------------------------------------------
# Back to rarefaction -----------------------------------------------------  
# raster  
  raster <- raster("Australia/aus_100km_cell_id.grd")
  
# Subset into native and introduced statuses
  spp_n <- filter(spp, status == "native") %>%
    dplyr::select(species, lat, long, pp, year) 
  
  spp_i <- filter(spp, status == "introduced") %>%
    dplyr::select(species, lat, long, pp, year)  
  
  # spp <- spp_n
  # spp <- spp_i

# Rarefaction set-up -------------------------------------------------------   
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
  
  
# ---------------------------------------------------------------------- 

# Save native and introduced arrays
  # native.array <- a
  # introduced.array <- a
  
  sum(native.array[, 1, 4],na.rm = T)
  sum(native.array[, 2, 4],na.rm = T)
  sum(native.array[, 3, 4],na.rm = T)
  
  sum(introduced.array[, 1, 4], na.rm = T)
  sum(introduced.array[, 2, 4], na.rm = T)
  sum(introduced.array[, 3, 4], na.rm = T)
  
# add in column names
  dimnames = list(c("a", "b", "c"),
                  c("d", "e", "f"),
                  c("g", "h", "i"))

  dimnames(native.array)[[2]] <- c("total", "C3", "C4")
  dimnames(native.array)[[3]] <- c("rare.15", "rare.20", "rare.25", "rare.30",
                                   "rare.35", "rare.40", "rare.45", "rare.50")
  
  dimnames(introduced.array)[[2]] <- c("total", "C3", "C4")
  dimnames(introduced.array)[[3]] <- c("rare.15", "rare.20", "rare.25", "rare.30",
                                       "rare.35", "rare.40", "rare.45", "rare.50")
  
# Note: these have too many values; need to subset out offshore cells within dataframe for use in models etc. E.g. below for raster synthesis

# -----------------------------------------------------------------------------------
  
# 1.2 Generate all 48 rasters -------------------------------------------------------------------- 
# We need to read in species data, extract out Australia-only cells that fit within the bounds of the shapefile, then plug those into individual rasters from our Aus template.
  
# Data ------------------------  
# Species data
  nat.c <- read.csv("C:/Users/s436862/Dropbox/Rarefaction/4. Results/Rarefaction/CSV/Native multiple cutoff COMPLETE.csv", header = T)
  int.c <- read.csv("C:/Users/s436862/Dropbox/Rarefaction/4. Results/Rarefaction/CSV/Introduced multiple cutoff COMPLETE.csv", header = T)
  
# Land/ocean & cell values
  cell.cat <- read.csv("C:/Users/s436862/Dropbox/Climate Matching/1. Data files/EFs/CSV/Terrestrial land categories.csv", header = T)
 
# Raster template
  aus <- raster("C:/Users/s436862/Dropbox/Climate matching/1. Data files/Australia/aus_100km_cell_id.grd")
  
# Removing off-shore cells --------------  
# Subset terrestrial cells out of data frames  
  nat.land <- cbind(cell.cat, nat.c)
  int.land <- cbind(cell.cat, int.c)
  
  nat.terr <- filter(nat.land, cell.cat == "terrestrial")
  int.terr <- filter(int.land, cell.cat == "terrestrial")
  
# generate list of occupied cells
  cell.list.x <- nat.terr$cell.id
  cell.list.y <- int.terr$cell.id
  
# make a matrix with all missing values (to get from 1003 cells to 2538)
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
  setwd("C:/Users/s436862/Dropbox/Rarefaction/4. Results/Rarefaction/Rasters/Rarefied rasters complete") 
  
# Nat rarefied 15- to 50-records 
# 15 - 30 (x rows 1-12)
  r15.n.tot <- setValues(aus, x[, 1])
  writeRaster(r15.n.tot, "r15.n.tot.grd", overwrite = T)
  plot(r15.n.tot)
  r15.n.c3 <- setValues(aus, x[, 2])
  writeRaster(r15.n.c3, "r15.n.c3.grd", overwrite = T)
  plot(r15.n.c3)
  r15.n.c4 <- setValues(aus, x[, 3])
  writeRaster(r15.n.c4, "r15.n.c4.grd", overwrite = T)
  plot(r15.n.c4)
  
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
  
# 30 - 50 (nat rows 13 - 24) ----
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
  
# 30 - 50 (int rows 13 - 24) ----
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
  
# -----------------------------------------------------------------------  
  
# 2.0 Rarefaction using total records for 15-50 records -----------------
# Aim: which is the best cutoff? We'll calculate several (15 - 50 records) for all species records (i.e. antive and exotic combined) and correlate them with each other in the next section
  
  # Data formatting -----------------------------------------------------------------
  # species    
  spp <- read.csv("Osborne C3-C4/AVH grass pp.csv", header = T) %>%
    filter(pp == "C3" | pp == "C4") %>% # removing 'unknown' and 'mixed' pp classes
    dplyr::select(species, lat, long, pp, year) 
  
  # raster 
  raster <- raster("Australia/aus_100km_cell_id.grd")
  
  # number of records by grid square
  xy <- cbind(spp$long, spp$lat)
  n_tot <- rasterize(xy, raster, fun = function(x,...) length(x))
  n_tot_val <- getValues(n_tot)
  
  # assign each point in the dataframe to raster cell
  spp$cell <- raster::extract(raster, xy)
  
  # get a list of the cell numbers
  cell_list <- as.numeric(levels(factor(spp$cell)))
  
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
  
  # Rarefaction function ----------------------------------------------
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
  # Including 10, but really testing 15 - 50 cut off values (n_min)
  n_min <- seq(10, 50, 5)
  
  # matrix to store rarefied cells' output, for a single cut off 
  out_rare <- matrix(NA, nrow = length(cell_list), ncol = 1) # length 1003
  
  # matrix with all cell info (including NAs), for a single cut off
  a <- matrix(NA, nrow = length(raster), ncol = 9)
  
  # function start
  for (i in 1:length(n_min)) {
    
    rec_no <- n_min[i]
    
    for(j in 1:length(cell_list)) { 
      cell <- dplyr::filter(spp, cell == cell_list[j]) # return only the cells with records in them
      sp <- as.character(cell$species)
      if(cell$n_rec[1] < n_min) out_rare[j] <- NA 
      else {out_rare[j, ] <- rarefaction_solo(sp, rec_no) 
      }
    }
    
    m <- matrix(NA, nrow = length(getValues(raster)), ncol = 1)
    m[cell_list, ] <- out_rare                         
    
    a[,i] <- m
    
  } # multi-cutoff end 
  
  rare.rich.cutoff <- a
  
  # View -----------------------------------------------------------------------
  est_rich_10 <- setValues(raster, rare.rich.cutoff[, 1])
  plot(est_rich_10)
  est_rich_15 <- setValues(raster, rare.rich.cutoff[, 2])
  plot(est_rich_15)
  est_rich_20 <- setValues(raster, rare.rich.cutoff[, 3])
  plot(est_rich_20)
  est_rich_25 <- setValues(raster, rare.rich.cutoff[, 4])
  plot(est_rich_25)
  est_rich_30 <- setValues(raster, rare.rich.cutoff[, 5])
  plot(est_rich_30)
  est_rich_35 <- setValues(raster, rare.rich.cutoff[, 6])
  plot(est_rich_35)
  est_rich_40 <- setValues(raster, rare.rich.cutoff[, 7])
  plot(est_rich_40)
  est_rich_45 <- setValues(raster, rare.rich.cutoff[, 8])
  plot(est_rich_45)
  est_rich_50 <- setValues(raster, rare.rich.cutoff[, 9])
  plot(est_rich_50)
  
  
  # Data frame
  rarefied_list <- data.frame(rare.rich.cutoff)
  colnames(rarefied_list) <- c("rare_10", "rare_15", "rare_20", "rare_25", "rare_30", "rare_35", "rare_40", "rare_45", "rare_50")
  
  
  # Removing off-shore cells ------------------------------------------------
  # Land/ocean & cell values
  cell.cat <- read.csv("C:/Users/s436862/Dropbox/Climate Matching/1. Data files/EFs/CSV/Terrestrial land categories.csv", header = T)
  
  # Subset terrestrial cells out of data frames  
  poa.land <- cbind(cell.cat, rarefied_list)
  poa.terr <- filter(poa.land, cell.cat == "terrestrial")
  
  # generate list of occupied cells
  cell.list.x <- poa.terr$cell.id
  
  # make a matrix with all missing values (to get from 1003 cells to 2538)
  x <- matrix(NA, nrow = length(getValues(raster)), ncol = 9)
  
  poa.m <- as.matrix(poa.terr)
  poa.mm <- poa.m[, 3:11]
  class(poa.mm) <- "numeric"
  
  # add the occupied cells
  cell.list.x <- poa.terr$cell.id
  x[cell.list.x, ] <- poa.mm
  
  colnames(x) <- c("rare_10", "rare_15", "rare_20", "rare_25", "rare_30", "rare_35", "rare_40", "rare_45", "rare_50")
  # save  
  write.csv(x, file = "C:/Users/s436862/Dropbox/Rarefaction/4. Results/Rarefaction/CSV/Poa 10 to 50 rarefied richness cut offs.csv", row.names = F)
  
# 2.1 Correlating 10-50 and percentage coverage -------------------------------------------------------- 
  # Standard deviation of cell records vs. number of cells occupied (Not sure if need)
  # We want a high SD, but high record number - let's see how this plays out
  cutoff <- read.csv("C:/Users/s436862/Dropbox/Rarefaction/4. Results/Rarefaction/CSV/Poa 10 to 50 rarefied richness cut offs.csv", header = T)
  
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
  r10 <- sum(table(cutoff$rare_10))
  r15 <- sum(table(cutoff$rare_15))
  r20 <- sum(table(cutoff$rare_20))
  r25 <- sum(table(cutoff$rare_25))
  r30 <- sum(table(cutoff$rare_30))
  r35 <- sum(table(cutoff$rare_35))
  r40 <- sum(table(cutoff$rare_40))
  r45 <- sum(table(cutoff$rare_45))
  r50 <- sum(table(cutoff$rare_50))
  cutoff.cell.tot <- rbind(r10, r15, r20, r25, r30, r35, r40, r45, r50)
  
  # percentage  
  p10 <- sum(table(cutoff$rare_10))/1003*100
  p15 <- sum(table(cutoff$rare_15))/1003*100
  p20 <- sum(table(cutoff$rare_20))/1003*100
  p25 <- sum(table(cutoff$rare_25))/1003*100
  p30 <- sum(table(cutoff$rare_30))/1003*100
  p35 <- sum(table(cutoff$rare_35))/1003*100
  p40 <- sum(table(cutoff$rare_40))/1003*100
  p45 <- sum(table(cutoff$rare_45))/1003*100
  p50 <- sum(table(cutoff$rare_50))/1003*100
  cutoff.per <- rbind(p10, p15, p20, p25, p30, p35, p40, p45, p50)
  
  # correlation matrix
  # 50 cut off 
  co.50 <- select(cutoff, rare_50)
  
  # other cutoffs (co)  
  co.others <- select(cutoff, rare_10, rare_15, rare_20, rare_25, rare_30,
                      rare_35, rare_40, rare_45, rare_50)  
  
  # correlation matrix
  total.cor <- cor(co.others, co.50, use = "complete.obs")  
  
  # rarefied names
  rarefied <- c(10, 15, 20, 25, 30, 35, 40, 45, 50)
  
  # dataframe
  cutoff.com <- cbind(rarefied, cutoff.cell.tot, cutoff.per, cutoff.sd, cutoff.cv, total.cor)
  colnames(cutoff.com) <- c("rarefied", "cell.total", "cell.percent", "SD", "CV", "correlation.w.50")  
  
  # save
  write.csv(cutoff.com, file = "C:/Users/s436862/Dropbox/Rarefaction/4. Results/Rarefaction/CSV/Poa 10 to 50 rarefied richness cut off summary stats.csv", row.names = F)

   
  # ---------------------------------------------------------------------------------  

  

  
# 3.0 Sampling effort calculations -------------------------------------------------
# Species richness, Chao1 estimator, and rarefaction correlated against sampling effort.
# Ideally, this should show the efficacy of rarefaction at standardising species richness for sampling effort. 
# Data -------------------------------------------------------------------------
  spp <- read.csv("Osborne C3-C4/AVH grass pp.csv", header = T) %>%
    filter(pp == "C3" | pp == "C4") # removing 'unknown' and 'mixed' pp classes
  
  
# 3.1 Calculate total record number ---------------------------------------------  
# Template of Australia
  raster <- raster("Australia/aus_100km_cell_id.grd")
  
# number of records by grid square
  xy <- cbind(spp$long, spp$lat)
  spp$cell <- raster::extract(raster, xy)
  
# Total records for every cell
  rec.tot <- rasterize(xy, raster, fun = function(x,...) length(x))
  plot(rec.tot)
  
# 3.2 Calculate species richness ---------------------------------------------------
# number of species per cell
  spp_per_cell <- as.numeric(factor(spp$species))
  spp.rich <- rasterize(xy, raster, field = spp_per_cell, fun = function(x,...) {length(unique(na.omit(x))) })
  plot(spp.rich)
  
  # 3.3 Chao1 species richness estimation ---------------------------------------------
  # For Chao1 estimation: singletons, doubletons 
  f1 <- rasterize(xy, raster, field = spp_per_cell, fun = function(x,...) {length(which(table(x)==1)) })
  f2 <- rasterize(xy, raster, field = spp_per_cell, fun = function(x,...) {length(which(table(x)==2)) })
  
  # Convert to dataframe for calcuations
  n <- getValues(rec.tot) # total number of records per cell
  a <- getValues(spp.rich) # species richness per cell
  f1 <- getValues(f1) # species with only one record in each cell
  f2 <- getValues(f2) # speices with only two "                 "
  
  # Replace NAs with zeroes for SRE calculations
  raster_df <- getValues(raster)
  sre <- data.frame(raster_df, n, a, f1, f2)
  sre[is.na(sre)] <- 0
  sum(is.na(sre)) # cool
  
  # split them back up
  n <- sre$n
  a <- sre$a
  f1 <- sre$f1
  f2 <- sre$f2
  
  # Chao1 
  chao1.df <- ifelse(f2 == 0, 
                     (a + (((n-1) / n) * (f1 * (f1-1)) / (2 * (f2+1)))),
                     (a + (((n-1) / n) * ((f1^2)/(2*f2))))
  ) # 'if' won't handle two conditional statements  
  
  chao1 <- setValues(raster, chao1.df)
  plot(chao1)
  
  
  # Can easily do ACE estimation if need be, but I think this will suffice  
  
  
  
# 3.4 Rarefaction to 15 records not splitting for photosynthetic pathway -----------------------------------------------  
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
  
# Rarefaction (no PP split) function -------------------------------------
  rarefaction_solo <- function(sp, cutoff) 
  { 
    n <- length(sp)       # number of records           
    n_sp <- table(sp)     # number of records for each species    
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
  rarefied_rich <- matrix(NA, nrow = length(getValues(raster)), ncol = 1)
  
# Cut off subsample
  cutoff <- 15
  
  for(j in 1:length(cell_list)) { 
    cell <- dplyr::filter(spp, cell == cell_list[j]) # return only the cells with records in them
    sp <- as.character(cell$species)
    if(cell$n_rec[1] < cutoff) out_rare[j] <- NA 
    else { out_rare[j, ] <- rarefaction_solo(sp, cutoff) 
    }
  }
  
# add the rarefied cells, one cutoff at a time  
  rarefied_rich[cell_list, 1] <- out_rare
  
  
# Plot
  est_rich_15 <- setValues(raster, rarefied_rich[, 1])
  plot(est_rich_15) 
  
  
# Removing off-shore cells -----------------------------------------------------------  
# Land/ocean & cell values
  cell.cat <- read.csv("C:/Users/s436862/Dropbox/Climate Matching/1. Data files/EFs/CSV/Terrestrial land categories.csv", header = T)
  
# Group and filter sampling effort (se)
  se <- cbind(cell.cat, n, a, chao1.df, rarefied_rich)
  se.terr <- filter(se, cell.cat == "terrestrial")
  
# generate list of occupied cells
  cell.list.x <- se.terr$cell.id
  
# make a matrix with all missing values (to get from 1003 cells to 2538)
  x <- matrix(NA, nrow = length(getValues(raster)), ncol = 4)
  
  se.m <- as.matrix(se.terr)
  se.mm <- se.m[, 3:6]
  class(se.mm) <- "numeric"
  
# add the occupied cells
  x[cell.list.x, ] <- se.mm
  
# Column names
  colnames(x) <- c("records", "sr", "chao1", "rarefied")
  
# Total records raster
  tot.rec.raster <- setValues(raster, x[,1])
  plot(tot.rec.raster)
  writeRaster(tot.rec.raster, "C:/Users/s436862/Dropbox/Rarefaction/4. Results/Rarefaction/Rasters/Sample effort comparisons/Total records.grd")
  
# Species richness raster
  sr.raster <- setValues(raster, x[,2])
  plot(sr.raster)
  writeRaster(rec.tot, "C:/Users/s436862/Dropbox/Rarefaction/4. Results/Rarefaction/Rasters/Sample effort comparisons/Species richness.grd")
  
# Chao1 raster
  chao1.raster <- setValues(raster, x[,3])
  plot(chao1.raster)
  writeRaster(rec.tot, "C:/Users/s436862/Dropbox/Rarefaction/4. Results/Rarefaction/Rasters/Sample effort comparisons/Chao1 species richness.grd")
  
# Rarefied richness raster
  rarefied.raster <- setValues(raster, x[,4])
  plot(rarefied.raster)
  writeRaster(rarefied.raster, "C:/Users/s436862/Dropbox/Rarefaction/4. Results/Rarefaction/Rasters/Sample effort comparisons/Rarefied to 15 species richness.grd")
  
  
# 3.5 Sample effort comparisons ----------------------------------------------------------
  cor <- cor(x, use = "complete.obs")
  
  write.csv(cor, "C:/Users/s436862/Dropbox/Rarefaction/4. Results/Rarefaction/CSV/Sampling effort correlation matrix.csv", row.names = T)
  
  
  
  
  
  
  
  
  # -------------------------------------------------------------------------------------
  
  
  
  
  
  
  
  
  
  
  
