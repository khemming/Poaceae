#############################################################
# photosynthetic pathway & tribe allocation
############################################################
# date created: 8/5
# last updated: 

# aim ------------------------------------------------------
# each species needs a photosynthetic pathway (C3/C4)
# I will use Sue's recommendation of Watson (http://www.delta-intkey.com/)
# and Osborne et al. 2014, to assign genera that are missing.
# library -----------------------------------------------------
  library(tidyverse)
  library(data.table)
  
  rm(list = ls())
  
  setwd("C:/Users/s436862/Dropbox/Rarefaction/Data files")

# data --------------------------------------------------------  
# ala thus far
  ala <- readRDS("ALA/Generated data from cleaning steps/APC Randall Sue cleaned ALA Poaceae.rds")
  ala.gen <- distinct(ala, genus)
  ala.spp <- distinct(ala, species)
  
# Sue's pp data
  sb.spp <- read.csv("Sue/6.AVH_grass-SB-endemics for KH.csv")
  colnames(sb.spp) <- c("species", "genus", "pp", "sub.family", "endemic")
  sb <- distinct(sb.spp, genus, .keep_all = T) %>%
    select(genus, pp, sub.family)
  colnames(sb) <- c("genus", "w.pp", "sub.family")

# Osborne's pp data   
  os <- read.csv("Osborne C3-C4/c3-c4.csv", header = T)
  colnames(os) <- c("o.pp", "genus")
  table(os$o.pp) # note there are 'C3 and C4's and 'unknown' categories

# panicum spp. data
  panicum <- read.csv("Osborne C3-C4/Panicum.csv", header = T) %>%
            dplyr::select(ï..Species, Pathway)
  colnames(panicum) <- c("species", "pani.pp")
  head(panicum)
  
# assess coverage of pp from both sources ---------------------
# merge
  genera <- left_join(ala.gen, os, by = "genus") %>%
            left_join(., sb, by = "genus")
  head(genera) 
  table(genera$pp, exclude = F)
  table(genera$sub.family, exclude = F)
    
# notes -------------------------------------------------------
# I am taking Osborne's C3/C4 values over Watson's
# where Osborne is 'C3 & C4' or 'Na', I will take Watson's
# where there is no data, I will manually look up from Watson
# -------------------------------------------------------------
  table(genera$o.pp, genera$w.pp, exclude = F) # only Panicum is disbuted
  
  genera.c3 <- mutate(genera, final.pp = ifelse(w.pp == "C3"| o.pp == "C3", 1, 0)) %>%
               filter(., final.pp == 1) %>%
               mutate(final.pp = "C3")
  
  genera.c4 <- mutate(genera, final.pp = ifelse(w.pp == "C4"| o.pp == "C4", 1, 0)) %>%
                filter(., final.pp == 1) %>%
                mutate(final.pp = "C4")
              
  genera2 <- rbind(genera.c3, genera.c4)
  
# put back in genera which neither had (and were therefore excluded)
  genera3 <- left_join(genera, genera2)
  # there is an extra row now (238 vs 237), and it's not unique within genera3 nor different to genera (??)
  
# manually updating genera with no pp --------------------------  
# notes --------------------------------------------------------
# these are not represented within Sue's Watson list or Osborne
# and will be taken from Watson's site: https://www.delta-intkey.com/
# --------------------------------------------------------------  
  ala2 <- ala
  
  gen3 <- genera3 %>% mutate(., pp = final.pp) %>%
                      dplyr::select(genus, pp, sub.family)
# Lophopyrum
  gen3[gen3$genus == "Lophopyrum", "pp"] <- "C3"
  gen3[gen3$genus == "Lophopyrum", "sub.family"] <- "C3"
  
# Thinopyrum
  gen3[gen3$genus == "Thinopyrum", "pp"] <- "C3"
  gen3[gen3$genus == "Thinopyrum", "sub.family"] <- "C3"
  
# Avellinia
  gen3[gen3$genus == "Avellinia", "pp"] <- "C3"
  gen3[gen3$genus == "Avellinia", "sub.family"] <- "C3"
  
# Megathyrsus (synonym for Panicum maximum)
  gen3 <- filter(gen3, genus != "Megathyrsus")
  ala2[ala2$genus == "Megathyrsus", "genus"] <- "Panicum"
  ala2[ala2$species == "Megathyrsus maximus", "species"] <- "Panicum maximum"
  
# Molineriella (synonym for Periballia)
  gen3[gen3$genus == "Molineriella", "genus"] <- "Periballia"
  gen3[gen3$genus == "Periballia", "sub.family"] <- "C3"
  gen3[gen3$genus == "Periballia", "pp"] <- "C3"
  
  ala2[ala2$genus == "Molineriella", "genus"] <- "Periballia"
  ala2[ala2$species == "Molineriella minuta", "species"] <- "Periballia minuta"
  
# Elytrigia  
  gen3[gen3$genus == "Elytrigia", "pp"] <- "C3"
  gen3[gen3$genus == "Elytrigia", "sub.family"] <- "C3"
  
# Amelichloa (synonym for Nassella, but still recognised spp.)
  gen3[gen3$genus == "Amelichloa", "pp"] <- "C3"
  gen3[gen3$genus == "Amelichloa", "sub.family"] <- "C3"
  
# Steinchisma  
  gen3[gen3$genus == "Steinchisma", "pp"] <- "intermediate"
  gen3[gen3$genus == "Steinchisma", "sub.family"] <- "Paniceae"
  
# Moorochloa
  gen3[gen3$genus == "Moorochloa", "pp"] <- "C4"
  gen3[gen3$genus == "Moorochloa", "sub.family"] <- "Paniceae"
  
# Zuloagaea
  gen3[gen3$genus == "Zuloagaea", "pp"] <- "C4"
  gen3[gen3$genus == "Zuloagaea", "sub.family"] <- "Paniceae" 
  
# Jarava
  gen3[gen3$genus == "Jarava", "pp"] <- "C3"
  gen3[gen3$genus == "Jarava", "sub.family"] <- "C3" 
  
# update leftover tribes ----------------------------------
# where C3 --> C3,
# and we'll look up C4s on Watson's site
  gen3[gen3$pp == "C3", "sub.family"] <- "C3"
  table(gen3$sub.family, exclude = F)
  
# Triodia
  gen3[gen3$genus == "Triodia", "sub.family"] <- "Chloridoideae"  
  
# Cynodon
  gen3[gen3$genus == "Cynodon", "sub.family"] <- "Chloridoideae" 
  
# Andropogon
  gen3[gen3$genus == "Andropogon", "sub.family"] <- "Andropogoneae"
  
# Tragus  
  gen3[gen3$genus == "Tragus", "sub.family"] <- "Chloridoideae"
  
# Eleusine  
  gen3[gen3$genus == "Eleusine", "sub.family"] <- "Chloridoideae"  
  
# Axonopus
  gen3[gen3$genus == "Axonopus", "sub.family"] <- "Paniceae" 
  
# Melinis
  gen3[gen3$genus == "Melinis", "sub.family"] <- "Paniceae"
  
# Spartina
  gen3[gen3$genus == "Spartina", "sub.family"] <- "Chloridoideae"  
  
# Desmostachya  
  gen3[gen3$genus == "Desmostachya", "sub.family"] <- "Chloridoideae" 
  
# Eustachys
  gen3[gen3$genus == "Eustachys", "sub.family"] <- "Chloridoideae"   
  
# Saccharum 
  gen3[gen3$genus == "Saccharum", "sub.family"] <- "Andropogoneae" 
  
# Dinebra
  gen3[gen3$genus == "Dinebra", "sub.family"] <- "Chloridoideae"   
  
# Zea
  gen3[gen3$genus == "Zea", "sub.family"] <- "Andropogoneae"    
  
# Miscanthus
  gen3[gen3$genus == "Miscanthus", "sub.family"] <- "Andropogoneae" 
  
# Schmidtia
  gen3[gen3$genus == "Schmidtia", "sub.family"] <- "Chloridoideae"  
  
# Microstegium
  gen3[gen3$genus == "Microstegium", "sub.family"] <- "Andropogoneae" 
  
# Polytrias
  gen3[gen3$genus == "Polytrias", "sub.family"] <- "Andropogoneae"  
  
# Sorghastrum 
  gen3[gen3$genus == "Sorghastrum", "sub.family"] <- "Andropogoneae"    
  
# Crypsis
  gen3[gen3$genus == "Crypsis", "sub.family"] <- "Chloridoideae"  
  
  
# update ala ----------------------------------------------
  ala.g <- left_join(ala2, gen3, by = "genus")
  table(ala.g$status)
  table(ala.g$pp)
  
# updating Panicum, by species, from Osborne --------------
  ala.pan <- filter(ala.g, genus == "Panicum") %>%
            dplyr::select(species, genus) %>%
            distinct(species, .keep_all = T)
  head(ala.pan)
  
  ala.pan.pp <- left_join(ala.pan, panicum, by = "species")
  head(ala.pan.pp)
  
  ala.pan.pp$pani.pp[is.na(ala.pan.pp$pani.pp)] <- "C4"
  head(ala.pan.pp)
  pan <- ala.pan.pp
  colnames(pan) <- c("species", "genus", "pp")
  
# insert back into ala (this is tricky)
# isolate Panicum and remove 'wrong' pp
  ala.p <- filter(ala.g, genus == "Panicum") %>%
            select(-pp)

# join with updated list
  ala.p2 <- left_join(ala.p, pan, by = c("genus", "species"))  
  table(ala.p2$pp)
  
# append
  ala.no.p <- filter(ala.g, genus != "Panicum")
  ala.sorted <- rbind(ala.p2, ala.no.p)
  table(ala.sorted$pp, ala.sorted$genus == "Panicum")

# ------------------------------------------------------------------  
  
# endemism ---------------------------------------------------------  
# 
  

  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  