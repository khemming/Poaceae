# Date: 23/11
# Last modified: 

# Date of first-record in Australia

# do this for native C3, c4 and exotic C3 and C4s

# Library ------------------------------------------------------------------
library(dplyr)
library(tidyr)

rm(list = ls())

setwd("C:/Users/s436862/Dropbox/Rarefaction/Results/Rarefaction")

# Data ----------------------------------------------------------------   
# Select relevant columns
  poa <- read.csv("C:/Users/s436862/Dropbox/Climate Matching/Data files/Osborne C3-C4/AVH grass pp.csv", header = T) %>% 
         select(species, year, status, pp)

  head(poa)  

# Filter records by earliest year of each species
  poa_yr <- group_by(poa, species, year)
  head(poa_yr)
  
# take unique rows of first year. Or top row?
  poa_un <- unique(poa, incomparables = F)
  head(poa_un) 

# count each speciess as a factor, and steal first observation of it??
# Take first row of each species' observation
  tn <- levels(factor(poa_un$species))
  
  sub <- filter(poa, year == min(year))
  
  i <- 1
  
  for(i in 1:length(tn)) { # put this in after we know it works
    
    sub.dat <- (filter(poa_un, species == tn[i]))
    
  }
  
# Long to wide format?!
  poa_sp <- spread(poa_un, key = first_occ, value = year)
  
  