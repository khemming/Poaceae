
  library(tidyverse)
  library(ggmap)
  library(raster)
  library(SpadeR)
  
########################################
  
  setwd("C:/Users/s436862/Dropbox/Climate Matching/1. Data files")
  
# read in AVH grass record data
  poa <- read.csv("AVH/AVH grass records.txt", header = T, sep = "\t")
  dim(poa)
  glimpse(poa)
         
# number of records per species
  poa.nr  <- group_by(poa, species) %>%
             summarise(n_rec = n())
             
# number of species
  n_spp <- nrow(poa.nr)
  
# species list
  list_spp <- poa.nr$species 
             
################################################################
# create a raster 
  aus2 <- raster(xmn = 112,
                 xmx = 156,
                 ymn = -46,
                 ymx = -9.1,
                 resolution = c(0.6, 0.6))

# number of records by grid squares
  xy <- cbind(poa$long, poa$lat)
  n_tot <- rasterize(xy, aus2, fun = function(x,...) length(x))
  plot(log10(n_tot))
  
# create a matrix to store the species x grid cell data
  out <- matrix(nrow = n_spp, ncol = dim(aus2)[1] * dim(aus2)[2])
  
# loop through each species and compute the number of observations per cell
# save this to the matrix

  e <- extent(aus2)
  for(i in 1:n_spp) {
    sub.poa <- filter(poa, species == list_spp[i])
    sub.xy <- cbind(sub.poa$long, sub.poa$lat)
    sub.n_tot <- rasterize(sub.xy, aus2, fun = function(x,...) length(x))
    out[i, ] <- extract(sub.n_tot, e)
  }
  
# convert NAs into zeros
  out <- ifelse(is.na(out), 0, out)
  
  
########################################  

# SpadeR examples
  abu <- ChaoSpeciesData$Abu
  sum(abu[,1]) # [row,column]
  # summed the column
  ChaoSpecies(abu, datatype = "abundance", k = 10, conf = 0.95)
  # value = 1996; sweet as
  
# ok, so checking that it did the first column in 'out'
  out.l <- ChaoSpecies(out[,2], datatype = "abundance", k = 10, conf = 0.95)
  sum(out[,1:100])
  # it didn't -- it did the first row
  # weird
  
  