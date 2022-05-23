

# aim ------------------------------------------------------------
# update names and origin (native/exotic) based on APC species list

# library ---------------------------------------------------------------
  library(tidyverse)
  library(data.table)
  
  rm(list = ls())
  
# data --------------------------------------------------------------------------  
# ALA data thus far
  ala <- readRDS("Data files/ALA/cleaning steps/step 2 - ALA records filtered by accuracy.rds") 
  colnames(ala)     

# APC names
  apc <- read.csv("Data files/ALA/cleaning steps/step 1 - ALA names & status.csv", strip.white = T) %>%
    dplyr::select(name, naturalised) 

# match ALA records with APC species names & status ------------------------------------
# ala species names
  ala$species <- as.factor(ala$species)
  ala.names <- data.frame(species = as.character(levels(ala$species)), ala = 1) 
  
# manual removal of genus -------------------------------
# removing Cynochloris genus because it is a hybrid: http://www.theplantlist.org/1.1/browse/A/Poaceae/Cynochloris/ & http://ausgrass2.myspecies.info/content/x-cynochloris-reynoldensis
  ala <- filter(ala, genus != "Cynochloris")
  table(ala$genus == "Cynochloris")
# -----------------------------------------------------------
  
# remove duplicate APC names
  apc.dup <- distinct(apc, name, .keep_all = T)
 
# apc status (native/nonnative)
  apc$name <- as.factor(apc$name) 
  apc.names <- data.frame(species = as.character(levels(as.factor(apc.dup$name))), 
                          apc = 1, 
                          apc.status = table(apc.dup$name, apc.dup$naturalised)[, 1]) 
  
# match ALA to APC names
  allnames <- merge(ala.names, apc.names, by = "species", all = T)
  allnames <- mutate(allnames, ala = ifelse(is.na(ala) == T, 0, 1),
                     apc = ifelse(is.na(apc) == T, 0, 1),
                     tot = ala + apc)
  matching.names <- filter(allnames, tot == 2)

# filter ALA records for appropriate names
  ala$species <- as.character(ala$species)
  apc.names$species <- as.character(apc.names$species)
  ala.apc.names <- filter(ala, ala$species %in% matching.names$species)
  

# join ALA and APC names
  apc.named.ala.records <- left_join(ala.apc.names, apc.names, by = "species")
  
# checks
  table(apc.named.ala.records$apc.status, exclude = F)
  table(apc.named.ala.records$apc, exclude = F)

# save ---------------------------------------------------------------
  apc.final <- apc.named.ala.records %>% dplyr::select(-apc) 
  
  saveRDS(apc.final, "Data files/ALA/cleaning steps/step 3 - ALA records APC species names and status.rds")
  
# -----------------------------------------------------------------------------
  
  