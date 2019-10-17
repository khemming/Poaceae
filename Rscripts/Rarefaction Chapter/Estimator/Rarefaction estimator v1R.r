
  library(raster)
  library(oz)
  library(tidyverse)
  library(ggmap)
  library(tidyr)

           
########################################
  rm(list = ls())
  
  setwd("C:/Users/s436862/Dropbox/Climate Matching/1. Data files")
  
# read in AVH grass record data
  poa <- read.csv("AVH/AVH grass records.csv", header = T)
  dim(poa)
  glimpse(poa)
  
# may not be necessary:

# look for potential duplicated herbarium records
# defined as the same species in the same location (lat and long to 2 decimal places) in the same year
#  dup <- paste(poa$species, round(poa$lat, 2), round(poa$long, 2), poa$year)
#  poa.nodup <- poa[duplicated(dup) == FALSE, ]
#  poa.nodup$species <- factor(poa.nodup$species)
#  poa <- poa.nodup
  
# locations of all records
  ggplot(poa, aes(y = lat, x = long)) +
    geom_point(size = 0.1) +
    theme_nothing()

    
################################################################
# make a raster
  l1 <- floor(min(poa$long))
  l2 <- ceiling(max(poa$long))
  l3 <- floor(min(poa$lat))
  l4 <- ceiling(max(poa$lat))
  
  nr <- length(seq(l3, l4, by = 1))
  nc <- length(seq(l1, l2, by = 1))
  
  aus2 <- raster(nrows = nr, ncols = nc, 
                 xmn = l1, xmx = l2, 
                 ymn = l3, ymx = l4,
                 vals = 1:(nr*nc))

# number of records by grid squares
  xy <- cbind(poa$long, poa$lat)

# assign each point in the dataframe to raster cells
  poa$cell <- raster:::extract(aus2, xy)

# total records per cell
  n_tot <- rasterize(xy, aus2, fun = function(x,...) length(x))
  plot(log10(n_tot))
  length(getValues(n_tot))
  
# check this
  ch <- poa %>%
        group_by(cell) %>%
        summarise(n_rec = n())
        
  x <- getValues(n_tot)
  rv <- data.frame(cell = 1:length(x), n_tot = x)
  ch.rv <- full_join(ch, rv)
  plot(log(n_tot) ~ log(n_rec), data = ch.rv)      
  
# number of species per cell
  spp <- as.numeric(factor(poa$species))
  n_spp <- rasterize(xy, aus2, field = spp, fun = function(x,...) {length(unique(na.omit(x))) })
  plot(n_spp)

# check this
  ch <- poa %>%
        group_by(cell) %>%
        summarise(n = length(unique(species))) 
        
  x <- getValues(n_spp)
  rv <- data.frame(cell = 1:length(x), n_tot = x)
  ch.rv <- full_join(ch, rv)
  plot(log(n) ~ log(n_tot), data = ch.rv)      


# species versus number of records per cell
  all.val <- data.frame(n_spp = getValues(n_spp),
                        n_tot = getValues(n_tot))
                        
  ggplot(all.val, aes(y = n_spp, x = n_tot)) +
    geom_point() +
    geom_abline(intercept = 0, slope = 1) +
    scale_x_log10() +
    scale_y_log10() +
    theme_bw()
    
################################################################################
# select the columns we need
  sp <- poa %>%
        select(species, status, year, cell)
  
# number of records per cell
  n.rec <- table(sp$cell)
  nr <- data.frame(cell = as.numeric(names(n.rec)), n.rec = as.vector(n.rec))
  
# add number of records per cell to the dataframe
  sp <- full_join(sp, nr)
  
# get a list of the cell numbers
  cell.list <- as.numeric(levels(factor(sp$cell)))
  
################################################################################
# function to calculate rarefaction estimate of number of species
# requires as input: sp = a vector of the species names corresponding to all records in a cell
#                    status = a vector of status (native or exotic) for all records in a cell
#                    n = the number of samples for rarefaction
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
      out[i] <- 1 - exp(lchoose((N - sp_n[i]), n) - lchoose(N, n))    # use lchoose (i.e. log scale) to avoid problems with big numbers
    }
    # output estimated total richness, and estimated richness of introduced and native species
    return(c(round(sum(out)), round(sum(out[ne == 1])), round(sum(out[ne == 0]))))
  }

################################################################################

# matrix to store output: one row for each cell, 3 columns: 1 = total richness, 2 = introduced richness, 3 = native richness
  out.rare <- matrix(nrow = length(cell.list), ncol = 3)
  
# loop through all the cells, extract data from dataframe sp, and calculate rarefied richness using n.min as the number of samples for rarefaction
# for cells with less than or equal to n.min records, record as NA

  n.min <- 50
  
  for(j in 1:length(cell.list)) {
    cell <- filter(sp, cell == cell.list[j])
    if(cell$n.rec[1] <= n.min) out.rare[j] <- NA else {
      spp <- as.character(cell$species)
      status <- as.character(cell$status)
      out.rare[j, ] <- rare(spp, status, n.min)
    }
  }
  
# put the rarefaction estimates into the raster
# need to include the missing cell values as well as the occupied cells
# first make a matrix with all missing values
  x <- matrix(NA, nrow = length(getValues(aus2)), ncol = 3)
# add the occupied cells
  x[cell.list, ] <- out.rare
# generate the raster object for total richness  
  est_rich <- setValues(aus2, x[, 1])
  plot(est_rich)
  
# generate the raster object for introduced richness  
  est_rich_int <- setValues(aus2, x[, 2])
  plot(est_rich_int)

# generate the raster object for native richness  
  est_rich_nat <- setValues(aus2, x[, 3])
  plot(est_rich_nat)

# proportion of introduced species in each cell
  ratio.ne <- setValues(aus2, x[, 2]/x[, 1])
  plot(ratio.ne)
  
  par(mfrow = c(2, 2), mar = c(1, 1, 1, 1))
  plot(est_rich)
  plot(est_rich_nat)
  plot(est_rich_int)
  plot(ratio.ne)
  
# compare with raw data                                                   
# number of native species by grid squares
  poa.nat <- filter(poa, status == "native")
  spp <- as.numeric(factor(poa.nat$species))
  new.xy <- cbind(poa.nat$long, poa.nat$lat)
  n_spp_nat <- rasterize(new.xy, aus2, field = spp, fun = function(x,...) {length(unique(na.omit(x))) })

# number of introduced species by grid squares
  poa.int <- filter(poa, status == "introduced")
  spp <- as.numeric(factor(poa.int$species))
  new.xy <- cbind(poa.int$long, poa.int$lat)
  n_spp_int <- rasterize(new.xy, aus2, field = spp, fun = function(x,...) {length(unique(na.omit(x))) })

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
  
