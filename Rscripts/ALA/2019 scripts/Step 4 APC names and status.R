########################################################
# cleaning step 4: finalise names and origin
########################################################
# date created: 5/6
# last updated: 

# aim ------------------------------------------------------------
# update names and origin based on Rachel (aka Randall), and add in pp 

# library --------------------------------------------------------
  library(tidyverse)
  library(data.table)
  
  rm(list = ls())
  
  setwd("C:/Users/s436862/Dropbox/Poaceae/Data files")


# step 1: filter ALA spp. names with APC accepted names ------------------------------------  
# ALA data thus far
  rm(list = ls())
  ala <- readRDS("ALA/Generated data from cleaning steps/Step 3 updated pp tribes.rds") 
  colnames(ala)     

# APC names
  apc <- read.csv("ALA/Generated data from cleaning steps/Step 1 ALA names.csv", strip.white = T) %>%
    dplyr::select(name, naturalised) 

# Sue's data
  sue <- read.csv("ALA/Supplied data/6.AVH_grass-SB-endemics for KH.csv", fileEncoding="UTF-8-BOM")
  head(sue)
  table(sue$endemic)
  
# step 1: match ALA records with apc species names & status --------------------------------
# ala species names
  ala$species <- as.factor(ala$species)
  ala.names <- data.frame(species = as.character(levels(ala$species)), ala = 1) 
  
# remove duplicate names
  apc.dup <- distinct(apc, name, .keep_all = T)
 
# apc status (native/exotic)
  apc$name <- as.factor(apc$name) 
  apc.names <- data.frame(species = as.character(levels(apc.dup$name)), 
                          apc = 1, 
                          apc.status = table(apc.dup$name, apc.dup$naturalised)[, 1]) 
  
# match ALA and APC names to form statuses
  allnames <- merge(ala.names, apc.names, by = "species", all = T)
  allnames <- mutate(allnames, ala = ifelse(is.na(ala) == T, 0, 1),
                     apc = ifelse(is.na(apc) == T, 0, 1),
                     tot = ala + apc)
  
# take only names which have both ALA and APC (i.e. tot = 2)  
  matching.names <- filter(allnames, tot == 2)

# subset ala records based on ALA/APC names
  ala$species <- as.character(ala$species)
  apc.names$species <- as.character(apc.names$species)
  ala.apc.names <- filter(ala, ala$species %in% matching.names$species)
  
# add native status to ala data
  apc.named.ala.records <- left_join(ala.apc.names, apc.names, by = "species")
  
  table(apc.named.ala.records$apc.status, exclude = F)
  
# step 2: doublecheck APC tags with Sue's list --------------------------
# species data
  apc.spp <- distinct(apc.named.ala.records, species, .keep_all = T) %>%
            select(species, apc.status)
  table(apc.spp$apc.status, exclude = F)  

  table(sue$endemic, exclude = F)
  
  apc.sue.spp <- left_join(apc.spp, sue, by = c("species"))  
  table(apc.sue.spp$apc.status, apc.sue.spp$endemic, exclude = F)
  
  
# final lists -------------------------------------------------
# species list with updated status -- assumes NAs were exotic species
  spp.list <- spp3 %>%
    mutate(., status = ifelse(new.status == 1, "native", "exotic")) %>%
    dplyr::select(species, status)
  spp.list$species <- as.character(spp.list$species)
  
# add to ALA records
  ala.final <- left_join(apc.named.ala.records, spp.list, by = "species")
  glimpse(ala.final)
  
  table(ala.final$status, exclude = T)
  nas <- filter(ala.final, is.na(status) == T)
  
# final check
  table(kh8$status, exclude = F)
  kh8a <- kh8 %>% tidyr::drop_na("status")
  

  
# load data here from updated name and tribes list -----------------  
# remove records who're outside of continental Australia
# raster template
  rm(list = ls())
  raster <- raster("Australia/aus 100 km v2.grd")
  
# which cells I want to keep
  cell.id <-  read.csv("C:/Users/s436862/Dropbox/Poaceae/Results/EVs/CSV/100 km EFs scaled.csv", header = T) %>%
    mutate(cell = cell.id) %>%
    dplyr::select(cell.category.v2, cell)
  
# spp records  
  dat <- readRDS("ALA/Generated data from cleaning steps/APC Randall Sue Kyle cleaned ALA Poaceae.rds") 
  
# remove cells which don't have a 'land' cell category via cell.id ----
# extract records by cell
  xy <- cbind(dat$longitude, dat$latitude)
  dat$cell <- raster::extract(raster, xy)
  
# attach cell.id and filter  
  dat.a <- left_join(dat, cell.id)
  dat.b <- filter(dat.a, cell.category.v2 == "land")
  
# plot check
  xy.b <- cbind(dat.b$longitude, dat.b$latitude)
  dat.br <- rasterize(xy.b, raster, fun = function(x,...) {length(unique(na.omit(x))) })
  plot(dat.br)
  plot(raster)
  
# checks
  table(dat.b$pp, exclude = F)
  table(dat.b$sub.family, exclude = F)
  table(dat.b$status, exclude = F)
# all good
  dat.c <- distinct(dat.b, genus, sub.family)
  table(dat.c$genus, exclude = F)
  table(dat.c$species, exclude = F)
# all good
  
# 7. save master copy data ---------------------------------------------  
  saveRDS(dat.b, "ALA/ALA master data/master grass data.rds")
  
# ----------------------------------------------------------------------------------
  
  