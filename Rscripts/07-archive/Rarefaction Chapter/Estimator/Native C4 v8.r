# Date created: 31/7/18
# Last updated: 12/3/19

# update from v6: rarefying C4 and C3 separetely (rather than relative proportions) using the 'independent' rarefaction function (see Rarefaction v5 for original version of equations)

# library -------------------------------------------------------------------------
library(tidyverse)
library(raster)
library(rgdal)

setwd("C:/Users/s436862/Dropbox/Rarefaction/Data files")

rm(list = ls())

# 1. data exploration  -----------------------------------------------------------------------------
# poaceae
dat <- readRDS("ALA/2019 ALA master data/master grass data.rds") %>%
  dplyr::select(-sub.family) %>% 
  filter(status == "native")

dat.C4 <- dat %>% filter(pp == "C4")

# C4 as trial run  
dat <- dat.C4
# australia 
raster <- raster("Australia/aus 100 km v2.grd")
plot(raster)

# 2. Rarefaction to 15 records independent (not splitting for photosynthetic pathway) ----------------------  
# number of records by grid square
xy <- cbind(dat$longitude, dat$latitude)
n_tot <- rasterize(xy, raster, fun = function(x,...) length(x))
n_tot_val <- getValues(n_tot)

# assign each point in the dataframe to raster cell
dat$cell <- raster::extract(raster, xy)

# get a list of the cell numbers
cell.list <- as.numeric(levels(factor(dat$cell)))

# number of records per cell --------------------------------------------
# number of records for only cells with records in them
n_rec <- table(dat$cell)
nr <- data.frame(cell = as.numeric(names(n_rec)), 
                 n_rec = as.vector(n_rec))
n_cell_rec_occ <- dat %>%
  group_by(cell) %>%
  summarise(n_rec = n())      

# number of records for all cells  
n_cell_rec_tot <- data.frame(cell = 1:length(n_tot_val), n_tot = n_tot_val)

# check the dataframes line up
ch_rv1 <- full_join(n_cell_rec_occ, n_cell_rec_tot)      
plot(log(n_tot) ~ log(n_rec), data = ch_rv1) # they agree

# Number of species per cell ---------------------------------------------
# number of species per cell
spp_per_cell <- as.numeric(factor(dat$species))
n_spp <- rasterize(xy, raster, field = spp_per_cell, fun = function(x,...) {length(unique(na.omit(x))) })
plot(n_spp)

# check this (spp richness per cell)
check2 <- dat %>%
  group_by(cell) %>%
  summarise(n = length(unique(species))) 

x <- getValues(n_spp)
rv <- data.frame(cell = 1:length(x), n_tot = x)
ch_rv <- full_join(check2, rv) # line up all goods
plot(log(n) ~ log(n_tot), data = ch_rv) # lines up, cool

# add number of records, per cell, to the dataframe
dat <- full_join(dat, nr) %>%
  arrange(cell)

# cell numbers with total records in each
cell_list <- as.numeric(levels(factor(dat$cell)))

# Rarefaction (no PP split) function -------------------------------------
rarefaction_independent <- function(sp, cutoff) 
{ 
  n <- length(sp)               # number of records           
  n_sp <- table(sp)             # number of records for each species    
  out <- numeric(length(n_sp))  # vector to store estimate for each species
  
  # for each species, calculate the expected number of occurrences from n records
  for(i in 1:length(n_sp)) 
  {
    out[i] <- 1 - exp(lchoose((n - n_sp[i]), cutoff) - lchoose(n, cutoff))    
    # use lchoose (i.e. log scale) to avoid problems with big numbers
  }
  
  return(sum(out))
} # end function 

# Run function ----------------------------------------------------------
# matrix to store output
out_rare <- matrix(nrow = length(cell_list)) # length 1009-1147 (not 2538)

# matrix with all cells
rarefied_rich <- matrix(NA, nrow = length(getValues(raster)), ncol = 1)

# Cut off subsample
cutoff <- 15

for(j in 1:length(cell_list)) { 
  cell <- dplyr::filter(dat, cell == cell_list[j]) # return only the cells with records in them
  sp <- as.character(cell$species)
  if(cell$n_rec[1] < cutoff) out_rare[j] <- NA 
  else { out_rare[j, ] <- rarefaction_independent(sp, cutoff) 
  }
}

# add the rarefied cells, one cutoff at a time  
rarefied_rich[cell_list, 1] <- out_rare


# plot
est_rich_15 <- setValues(raster, rarefied_rich[, 1])
plot(est_rich_15) 

est_rich <- mask(est_rich_15, raster)
plot(est_rich)

poa.df <- data.frame(getValues(est_rich))

# save raster (we'll make them into a df later)
writeRaster(est_rich, "C:/Users/s436862/Dropbox/Rarefaction/Results/Rarefaction/Rasters/Rarefied richness independent/Native C4 rich", overwrite = T)

# <15 record native C4 cells
C4_rec <- mask(n_tot, raster)
plot(log(C4_rec))

# save raster                                                                                      
writeRaster(C4_rec, "C:/Users/s436862/Dropbox/Rarefaction/Results/Rarefaction/Rasters/Rarefied richness independent/Native C4 rec", overwrite = T)

# -----------------------------------------------------