########################################################
# cleaning step 3: APC names and origin
########################################################
# date created: 5/6
# last updated: 27/6 

# aim ------------------------------------------------------------
# update names and origin (native/exotic) based on APC species list

# library --------------------------------------------------------
  library(tidyverse)
  library(data.table)
  
  rm(list = ls())
  
  setwd("C:/Users/s436862/Dropbox/Poaceae/Data files")


# step 1: filter ALA spp. names with APC accepted names ------------------------------------  
# ALA data thus far
  ala <- readRDS("ALA/Generated data from cleaning steps/step 2 ALA records filtered by accuracy.rds") 
  colnames(ala)     

# APC names
  apc <- read.csv("ALA/Generated data from cleaning steps/step 1 ALA names & status.csv", strip.white = T) %>%
    dplyr::select(name, naturalised) 

# step 1: match ALA records with APC species names & status --------------------------------
# ala species names
  ala$species <- as.factor(ala$species)
  ala.names <- data.frame(species = as.character(levels(ala$species)), ala = 1) 
  
# ------------------------------------------------------------------------------------------  
# note: manual ALA name change; genus removed ----------------------------------------------
# name change
  ala$species[ala$species == "Neurachne alopecuroides"] <- "Neurachne alopecuroidea"
  table(ala$species == "Neurachne alopecuroidea")
  table(ala$species == "Neurachne alopecuroides")
  
# removing Cynochloris genus because it is a hybrid: http://www.theplantlist.org/1.1/browse/A/Poaceae/Cynochloris/ & http://ausgrass2.myspecies.info/content/x-cynochloris-reynoldensis
  ala <- filter(ala, genus != "Cynochloris")
  table(ala$genus == "Cynochloris")
# ------------------------------------------------------------------------------------------
  
# remove duplicate APC names
  apc.dup <- distinct(apc, name, .keep_all = T)
 
# apc status (native/exotic)
  apc$name <- as.factor(apc$name) 
  apc.names <- data.frame(species = as.character(levels(apc.dup$name)), 
                          apc = 1, 
                          apc.status = table(apc.dup$name, apc.dup$naturalised)[, 1]) 
  
# match ALA to APC names
  allnames <- merge(ala.names, apc.names, by = "species", all = T)
  allnames <- mutate(allnames, ala = ifelse(is.na(ala) == T, 0, 1),
                     apc = ifelse(is.na(apc) == T, 0, 1),
                     tot = ala + apc)
  matching.names <- filter(allnames, tot == 2)

# filter ALA records for approrpiate names
  ala$species <- as.character(ala$species)
  apc.names$species <- as.character(apc.names$species)
  ala.apc.names <- filter(ala, ala$species %in% matching.names$species)
  

# step 2: origin (status) ------------------------------------------------
  apc.named.ala.records <- left_join(ala.apc.names, apc.names, by = "species")
  
# checks
  table(apc.named.ala.records$apc.status, exclude = F)
  table(apc.named.ala.records$apc, exclude = F)

# load Sue's script here!?!?!??!  
  
  
# step 3: save ---------------------------------------------------------------
  apc.final <- apc.named.ala.records %>% dplyr::select(-apc) 
  
  saveRDS(apc.final, "ALA/Generated data from cleaning steps/step 3 ALA records APC species names and status.rds")
  
# -----------------------------------------------------------------------------
  
  