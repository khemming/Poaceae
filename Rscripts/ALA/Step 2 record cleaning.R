########################################################
# cleaning step 2: cleaning missing ALA data
########################################################
# date created: 17/5
# last updated: 

# aim ------------------------------------------------------------
# remove records with missing data: species, hybrids, year, location data, and duplicates etc.

# library --------------------------------------------------------
  library(tidyverse)
  library(data.table)
  
  rm(list = ls())
  
  setwd("C:/Users/s436862/Dropbox/Poaceae/Data files")
  
# data --------------------------------------------------------------------
# read in RDS file (note this was downloaded as CSV and I converted it to save space)
  ala.raw <- readRDS("ALA/Supplied data/records-2019-03-01.rds")
  
# required columns listed in header csv
  ala.header <- read.csv("ALA/Supplied data/records-2019-03-01 headings.csv", header = T) %>%
    dplyr::select(Required)
  ala.header <- as.vector(t(ala.header))
  ala.header.pos <- which(ala.header == "yes")
  
# select columns  
  ala <- ala.raw[, ..ala.header.pos]
  ala <- data.frame(ala)
  
# select relvant columns and change column names 
  ala <- dplyr::select(ala, species, genus, year, taxonRank, coordinateUncertaintyInMeters, decimalLatitude, decimalLongitude, recordID, recordedBy)
  colnames(ala) <- c("species", "genus", "year", "taxonRank", "coordinateUncertainty", "latitude", "longitude", "recordID", "recorder")
  
# filter Na values
  ala2 <- ala %>% tidyr::drop_na("species", "genus", "latitude", "longitude", "year")
  
# filtering species: missing (blank), genera, 'forms', 'variety' and subspecies
  ala2a <- filter(ala2, grepl(" ", species))
  ala2b <- filter(ala2a, !grepl("form", taxonRank))
  ala2c <- filter(ala2b, !grepl("variety", taxonRank))
  ala2d <- filter(ala2c, !genus == "")
  
# remove incorrect year (where year = 0)
  table(ala2d$year, exclude = F)
  ala3 <- filter(ala2d, !year == "0")
  table(ala3$year, exclude = F)
  
# remove records with coordiantes that are incorrect, uncertain (NA) and with large uncertainties (above 10 km radius)
  ala4 <- filter(ala3, coordinateUncertainty <= 10000 & !coordinateUncertainty <= 0)
  table(ala4$coordinateUncertainty)
  
# remove duplicates 
# round lat/longs to ~1-km (2dp)
  ala4$latitude <- round(ala4$latitude, digits = 2)
  ala4$longitude <- round(ala4$longitude, digits = 2)
# find unique (distinct) records 
  ala5 <- ala4 %>% distinct(species, year, latitude, longitude, .keep_all = TRUE) 
  
# list of species
  spp.list <- levels(as.factor(ala5$species))
  length(spp.list)
  spp.list.df <- data.frame(spp.list) # 1445
  
# save
  saveRDS(ala5, "ALA/Generated data from cleaning steps/step 2 ALA records filtered by accuracy.rds")
  
