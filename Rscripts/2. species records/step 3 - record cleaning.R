
# library --------------------------------------------------------
  library(raster)
  library(tidyverse)
  library(data.table)

  rm(list = ls())
  
# data --------------------------------------------------------------------
# Australia coordinates 
  aus <- raster("Data files/Australia/Australia 1104.grd")
  
# read in RDS file (note this was downloaded as CSV and I converted it to save space)
  ala.raw <- readRDS("Data files/ALA/Supplied data/records-2019-03-01.rds")
  
# required columns listed in header csv
  ala.header <- read.csv("Data files/ALA/Supplied data/records-2019-03-01 headings.csv", header = T) %>%
    dplyr::select(Required)
  ala.header <- as.vector(t(ala.header))
  ala.header.pos <- which(ala.header == "yes")
  
# select columns  
  ala <- ala.raw[, ..ala.header.pos]
  ala <- data.frame(ala)
  
# select relevant columns and change column names 
  ala <- dplyr::select(ala, species, genus, year, taxonRank, coordinateUncertaintyInMeters, decimalLatitude, decimalLongitude, recordID, recordedBy)
  colnames(ala) <- c("species", "genus", "year", "taxonRank", "coordinateUncertainty", "latitude", "longitude", "recordID", "recorder")
  
# filter steps --------------------------------------------------------
# crop records ----------------------------------------------------
  xy <- cbind(ala$longitude, ala$latitude)
  cell <- raster::extract(aus, xy)
  
  table(cell, exclude = NULL)
  
  ala$aus_record <- ifelse(!is.na(cell), "yes", "no")
  table(ala$aus_record, exclude = NULL)
  
  ala2 <- ala %>% 
          filter(aus_record == "yes") %>%
          droplevels()
          
# missing values
  ala3 <- ala2 %>% tidyr::drop_na("species", "genus", "latitude", "longitude", "year")
  
# missing (blank), genera, 'forms', 'variety' and subspecies
  table(ala3$taxonRank, exclude = F)
  ala3a <- filter(ala3, grepl(" ", species))
  ala3b <- filter(ala3a, !grepl("form", taxonRank))
  ala3c <- filter(ala3b, !grepl("variety", taxonRank))
  ala3d <- filter(ala3c, !genus == "")
  table(ala3d$taxonRank, exclude = F)
  
# incorrect year (where year = 0)
  table(ala3d$year, exclude = F)
  ala4 <- filter(ala3d, !year == "0")
  table(ala4$year, exclude = F)
  
# records with coordiantes that are incorrect, uncertain (NA) and with large uncertainties (above 10 km radius)
  ala5 <- filter(ala4, coordinateUncertainty <= 10000 & !coordinateUncertainty <= 0)
  table(ala5$coordinateUncertainty, exclude = F)
  
# duplicates 
# round lat/longs to ~1-km (2dp)
  ala5$latitude <- round(ala5$latitude, digits = 2)
  ala5$longitude <- round(ala5$longitude, digits = 2)
# find unique (distinct) records 
  ala6 <- ala5 %>% distinct(species, year, latitude, longitude, .keep_all = TRUE) 
  
# list of species
  spp.list <- levels(as.factor(ala6$species))
  length(spp.list)
  spp.list.df <- data.frame(spp.list) # 1445
  
# save
  saveRDS(ala6, "Data files/ALA/cleaning steps/step 2 - ALA records filtered by accuracy.rds")
  
# -----------------------------------------------------------
  
  