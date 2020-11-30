
# Date created: 31/7
# Last updated:

# This is the rarefaction methodology (cutoffs, pp, nat/int), all rolled into to one script.
# The last script (SRE methods) was getting a bit large. 


# Library -------------------------------------------------------------------------
  library(raster)
  library(oz)
  library(ggmap)
  library(tidyr)
  library(oz)
  
  rm(list = ls())
  
  setwd("C:/Users/s436862/Dropbox/Climate matching/1. Data files")

  
# Rarefied split via pp function v.automatic (Reduce size + notations) ----------------------
  rm(list=setdiff(ls(), "native.array")) 
  rm(list=setdiff(ls(), "introduced.array")) 
# Data formatting --------------------------------------------------------------------------- 
# Poa data ----------------------------------------------------------------
# mixed class into NAs -------------------------------------------
# spp <- read.csv("Osborne C3-C4/AVH grass pp.csv", header = T) %>% 
#   mutate(pp = ifelse(pp == "C3 & C4", NA, as.character(pp))) %>%  
#   mutate(pp = as.factor(pp)) %>%
#   drop_na(pp)                                                     
# -------------------------------------------------------------------------

spp <- read.csv("Osborne C3-C4/AVH grass pp.csv", header = T) %>%
                filter(pp == "C3" | pp == "C4") # removing 'unknown' and 'mixed' pp classes
 
head(table(spp$genus, spp$pp, exclude = NULL)) # why does it keep unknown and mixed? There are none?
glimpse(spp)

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
  
  
# ------------------------------------------------------------------------------------------- 
  
# Save native and introduced arrays
  
  native.array <- a
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
  
  write.csv(native.array, file = "C:/Users/s436862/Dropbox/Rarefaction/4. Results/Rarefaction/CSV/Native multiple cutoff COMPLETE.csv", row.names = F)
  
  write.csv(introduced.array, file = "C:/Users/s436862/Dropbox/Rarefaction/4. Results/Rarefaction/CSV/Introduced multiple cutoff COMPLETE.csv", row.names = F)
  
# -----------------------------------------------------------------------------------
  
  
  
  
# Rarefied split via pp function v.automatic (DID NOT WORK) ---------------------------------
# Set function ------------------------------------------------------------------
  rarefaction.pp <- function(sp, pp, n) {
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
  } # function end
  
# Required data --------------------------------------------------------------- 
# (1) Raster: 
# defining extent and resolution of species records

# (2) Spp df, columns:  
# Species records (species)
# Photosynthetic pathway (pp)
# Lat/long (lat + long)
# Year collected (year)  
  
# (3) Cutoff 
# Sequence of cell record numbers to rarefy to; also acts as the minimum cell record number for inclusion for rarefaction estimate
  
# (4) Here we are repeating this for natives and for introduced, thus be aware of who you're using and what is being saved
  
# Spp  
  spp <- read.csv("Osborne C3-C4/AVH grass pp.csv", header = T) %>% 
    mutate(pp = ifelse(pp == "C3 & C4", NA, as.character(pp))) %>%  
    mutate(pp = as.factor(pp)) %>%
    drop_na(pp)                                                     
  
# Natives  
  spp_n <- filter(spp, status == "native")
  spp <- spp_n

# Introduced
  spp_i <- filter(spp, status == "introduced")
  #spp <- spp_i
  
# raster  
  raster <- raster("Australia/aus_100km_cell_id.grd")
  
# Cutoff
  rec <- seq(15, 50, 5) 
  
# Set data up for rarefaction function ------------------------------------------------------
# number of records per cell ----------------------------------------------
# number of records by grid square
  xy <- cbind(spp$long, spp$lat)
  spp$cell <- raster::extract(raster, xy)

# number of records for only cells with records in them
  n_rec <- table(spp$cell)
  nr <- data.frame(cell = as.numeric(names(n_rec)), 
                   n_rec = as.vector(n_rec))

# add number of records, per cell, to the dataframe
  spp_cell <- full_join(spp, nr) %>%
     arrange(cell) %>%
     select(species, pp, year, cell, n_rec)

# cell numbers with total records in each
  cell_list <- as.numeric(levels(factor(spp_cell$cell)))
  

# Set function outputs -----------------------------------------------------------------------
# rarefaction (3-col matrix)
  out_rare <- matrix(nrow = length(cell_list), ncol = 3)
  
# multi-cutoff (array; 2538, 3, 8)
  a <- array(dim = c(length(raster), ncol(out_rare), length(rec)))
  
# -------------------------------------------------------------------------------------------
# Run function: natives -------------------------------------------------------------------
  for (i in 1:length(rec)) {
    
    rec_no <- rec[i]
    
# Rarefaction 
    for(j in 1:length(cell_list)) {
      cell <- filter(spp_cell, cell == cell_list[j])
      if(cell$n_rec[1] <= rec_no) out_rare[j] <- NA else {
        spp <- as.character(cell$species)
        pp <- as.character(cell$pp)
        out_rare[j, ] <- rarefaction.pp(spp, pp, rec_no)
      }
    }
    
    
# rarefaction matrix with all cells (not just those with records in them)
     m <- matrix(NA, nrow = length(getValues(raster)), ncol = 3)
     m[cell_list, ] <- out_rare                         
     
     a[,,i] <- m
      
    } # multi-cutoff end

# ------------------------------------------------------------------------------------------- 
  
# Run function: introduced -------------------------------------------------------------------
  for (i in 1:length(rec)) {
    
    rec_no <- rec[i]
    
    # Rarefaction 
    for(j in 1:length(cell_list)) { 
      cell <- filter(spp_cell, cell == cell_list[j])      
      if(cell$n_rec[1] <= rec_no) out_rare[j] <- NA else {
        spp <- as.character(cell$species)
        pp <- as.character(cell$pp)
        out_rare[j, ] <- rarefaction.pp(spp, pp, rec_no)
      } # ifelse end
    } # rare end (he he)
    
    # rarefaction matrix with all cells (not just those with records in them)
    m <- matrix(NA, nrow = length(getValues(raster)), ncol = 3)
    m[cell_list, ] <- out_rare                         
    
    a[,,i] <- m
    
  } # multi-cutoff end
  
  introduced.array <- a
  
# ------------------------------------------------------------------------------------------- 
  
  
  
# Rarefied split via pp function v. manual --------------------------------------------------
# Set function ------------------------------------------------------------------
  rare <- function(sp, pp, n) {
    N <- length(sp)            # number of records
    sp_n <- table(sp)          # number of records for each species
    
    # get the status for each species in alphabetical order: 1 = introduced, 0 = native
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
  } # function end
  
# Required data --------------------------------------------------------------- 
  # (1) Raster: 
  # defining extent and resolution of species records
  
  # (2) Spp df, columns:  
  # Species records (species)
  # Photosynthetic pathway (pp)
  # Lat/long (lat + long)
  # Year collected (year)  
  
  # (3) Cutoff 
  # Sequence of cell record numbers to rarefy to; also acts as the minimum cell record number for inclusion for rarefaction estimate
  
  # Spp  
  spp <- read.csv("Osborne C3-C4/AVH grass pp.csv", header = T) %>% 
    mutate(pp = ifelse(pp == "C3 & C4", NA, as.character(pp))) %>%  
    mutate(pp = as.factor(pp)) %>%
    drop_na(pp)                                                     
  
  # raster  
  raster <- raster("Australia/aus_100km_cell_id.grd")
  
  # Cutoff
  # rec <- seq(15, 50, 5) 
  rec <- 30
  
  # Subset into native and introdcued statuses
  spp_n <- filter(spp, status == "native") %>%
    dplyr::select(species, lat, long, pp, year) 
  
  spp_i <- filter(spp, status == "introduced") %>%
    dplyr::select(species, lat, long, pp, year)    
  
  spp <- spp_n
  # spp <- spp_i
  
  
  
# Set data up for rarefaction function ------------------------------------------------------
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
  
# Number of species per cell ---------------------------------------------
  check2 <- spp %>%
    group_by(cell) %>%
    summarise(n = length(unique(species))) 
  
  # add number of records, per cell, to the dataframe
  spp_cell <- full_join(spp, nr) %>%
    arrange(cell) %>%
    select(species, pp, year, cell, n_rec)
  
  # cell numbers with total records in each
  cell_list <- as.numeric(levels(factor(spp_cell$cell)))
  
  
  
# Run function ---------------------------------------------------------------------------- 
  out_rare <- matrix(nrow = length(cell_list), ncol = 3)
  
  for(j in 1:length(cell_list)) {
    cell <- filter(spp_cell, cell == cell_list[j])
    if(cell$n_rec[1] <= rec) out_rare[j] <- NA else {
      spp <- as.character(cell$species)
      pp <- as.character(cell$pp)
      out_rare[j, ] <- rarefaction.pp(spp, pp, rec)
    }
  } 
  
  
  m <- matrix(NA, nrow = length(getValues(raster)), ncol = 3)
  
  m[cell_list, ] <- out_rare 
  
  
  