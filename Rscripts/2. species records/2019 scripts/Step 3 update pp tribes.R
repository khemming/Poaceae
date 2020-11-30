########################################################
# update names, origin, pp & tribes w Sue's data
########################################################
# date created: 11/6
# last updated: 

# aim ------------------------------------------------------------
# update: (1) names, (2) origin, (3) tribe, based Sue's manual additions
# Also (4) photosynthetic pathway: C3/C4
# much of this is manually added

# this script fits in between step 2 - 3 in the scheme of ALA things, but when I make all that into one script (if I do) this will sit serarate, and its results will be loaded when required, but not all this text in the script itself (because there's too much and it's pretty messy looking)


# library --------------------------------------------------------
  library(tidyverse)
  library(data.table)
  
  setwd("C:/Users/s436862/Dropbox/Poaceae/Data files")
  
# data ---------------------------------------------------------------
# ala species
  ala <- readRDS("ALA/Generated data from cleaning steps/Step 2 ALA records.rds") 
  
# Sue's tribe etc. data
  sue <- read.csv("ALA/Supplied data/6.AVH_grass-SB-endemics for KH.csv", fileEncoding="UTF-8-BOM")
  head(sue)
  table(sue$endemic)
  
# Osborne's pp data   
  os <- read.csv("Osborne C3-C4/c3-c4.csv", fileEncoding="UTF-8-BOM", header = T)
  colnames(os) <- c("o.pp", "genus")
  table(os$o.pp) # note there are 'C3 and C4's and 'unknown' categories
  
# panicum pp spp. data [assigned by spp.]
  panicum <- read.csv("Osborne C3-C4/Panicum.csv", fileEncoding="UTF-8-BOM", header = T) %>%
    dplyr::select(Species, Pathway)
  colnames(panicum) <- c("species", "pani.pp")
  head(panicum)  

# --------------------------------------------------------------------------------
  
# step 1: name change for species, that Sue noted --------------------------------
  ala$species[ala$species == "Neurachne alopecuroides"] <- "Neurachne alopecuroidea"
  table(ala$species == "Neurachne alopecuroidea")
  table(ala$species == "Neurachne alopecuroides")
  
# species list
  ala2 <- ala %>% distinct(., species, .keep_all = T) %>%
    dplyr::select(species, genus)
  head(ala2)
  
# step 2: separate endemic and native species column -----------------------------
# merge lists
  spp <- left_join(ala2, sue, by = c("species", "genus")) 
  head(spp)
  table(spp$endemic)
  
  spp2 <- mutate(spp, status = ifelse(endemic == "exotic", "exotic", "native"))
  table(spp2$status)
  

# step 3: assign photosynthetic pathway -----------------------------------------
# merge
  spp3 <- left_join(spp2, os, by = "genus")
  head(spp3) 
  table(spp3$o.pp, exclude = F)
  
# do they disagree on any pp's?
  table(spp3$o.pp, spp3$pp, exclude = F) # only Panicum is disbuted, which is fine (see below)
  
  spp.c3 <- mutate(spp3, final.pp = ifelse(pp == "C3"| o.pp == "C3", 1, 0)) %>%
    filter(., final.pp == 1) %>%
    mutate(final.pp = "C3")
  
  spp.c4 <- mutate(spp3, final.pp = ifelse(pp == "C4"| o.pp == "C4", 1, 0)) %>%
    filter(., final.pp == 1) %>%
    mutate(final.pp = "C4")
  
  spp4 <- rbind(spp.c3, spp.c4)
  
  table(spp4$final.pp, exclude = F)
  
# put back in genera which neither had (and were therefore excluded)
  spp5 <- left_join(spp3, spp4)
  table(spp5$final.pp, exclude = F) # there is an extra row now (1561 vs 1560), and it's not unique nor different (??)
  
# updating spp with no pp by genus --------------------------  
# manually taken from Watson's site: https://www.delta-intkey.com/

  ala6 <- spp5 %>% mutate(., pp = final.pp) %>%
    dplyr::select(-final.pp, -o.pp)
  
# Lophopyrum
  ala6[ala6$genus == "Lophopyrum", "pp"] <- "C3"
  ala6[ala6$genus == "Lophopyrum", "sub.family"] <- "C3"
  
# Thinopyrum
  ala6[ala6$genus == "Thinopyrum", "pp"] <- "C3"
  ala6[ala6$genus == "Thinopyrum", "sub.family"] <- "C3"
  
# Avellinia
  ala6[ala6$genus == "Avellinia", "pp"] <- "C3"
  ala6[ala6$genus == "Avellinia", "sub.family"] <- "C3"
  
# Megathyrsus (synonym for Panicum maximum)
  ala6 <- filter(ala6, genus != "Megathyrsus")
  ala2[ala2$genus == "Megathyrsus", "genus"] <- "Panicum"
  ala2[ala2$species == "Megathyrsus maximus", "species"] <- "Panicum maximum"
  
# Molineriella (synonym for Periballia)
  ala6[ala6$genus == "Molineriella", "genus"] <- "Periballia"
  ala6[ala6$genus == "Periballia", "sub.family"] <- "C3"
  ala6[ala6$genus == "Periballia", "pp"] <- "C3"
  
  ala[ala$genus == "Molineriella", "genus"] <- "Periballia"
  ala[ala$species == "Molineriella minuta", "species"] <- "Periballia minuta"
  
# Elytrigia  
  ala6[ala6$genus == "Elytrigia", "pp"] <- "C3"
  ala6[ala6$genus == "Elytrigia", "sub.family"] <- "C3"
  
# Amelichloa (synonym for Nassella, but still recognised spp.)
  ala6[ala6$genus == "Amelichloa", "pp"] <- "C3"
  ala6[ala6$genus == "Amelichloa", "sub.family"] <- "C3"
  
# Steinchisma  
  ala6[ala6$genus == "Steinchisma", "pp"] <- "intermediate"
  ala6[ala6$genus == "Steinchisma", "sub.family"] <- "Paniceae"
  
# Moorochloa
  ala6[ala6$genus == "Moorochloa", "pp"] <- "C4"
  ala6[ala6$genus == "Moorochloa", "sub.family"] <- "Paniceae"
  
# Zuloagaea
  ala6[ala6$genus == "Zuloagaea", "pp"] <- "C4"
  ala6[ala6$genus == "Zuloagaea", "sub.family"] <- "Paniceae" 
  
# Jarava
  ala6[ala6$genus == "Jarava", "pp"] <- "C3"
  ala6[ala6$genus == "Jarava", "sub.family"] <- "C3" 
  
# update leftover tribes ----------------------------------
# where C3 --> C3,
# and we'll look up C4s on Watson's site
  ala6[ala6$pp == "C3", "sub.family"] <- "C3"
  table(ala6$pp, ala6$sub.family, exclude = F)
  
# Triodia
  ala6[ala6$genus == "Triodia", "sub.family"] <- "Chloridoideae"  
  
# Cynodon
  ala6[ala6$genus == "Cynodon", "sub.family"] <- "Chloridoideae" 
  
# Andropogon
  ala6[ala6$genus == "Andropogon", "sub.family"] <- "Andropogoneae"
  
# Tragus  
  ala6[ala6$genus == "Tragus", "sub.family"] <- "Chloridoideae"
  
# Eleusine  
  ala6[ala6$genus == "Eleusine", "sub.family"] <- "Chloridoideae"  
  
# Axonopus
  ala6[ala6$genus == "Axonopus", "sub.family"] <- "Paniceae" 
  
# Melinis
  ala6[ala6$genus == "Melinis", "sub.family"] <- "Paniceae"
  
# Spartina
  ala6[ala6$genus == "Spartina", "sub.family"] <- "Chloridoideae"  
  
# Desmostachya  
  ala6[ala6$genus == "Desmostachya", "sub.family"] <- "Chloridoideae" 
  
# Eustachys
  ala6[ala6$genus == "Eustachys", "sub.family"] <- "Chloridoideae"   
  
# Saccharum 
  ala6[ala6$genus == "Saccharum", "sub.family"] <- "Andropogoneae" 
  
# Dinebra
  ala6[ala6$genus == "Dinebra", "sub.family"] <- "Chloridoideae"   
  
# Zea
  ala6[ala6$genus == "Zea", "sub.family"] <- "Andropogoneae"    
  
# Miscanthus
  ala6[ala6$genus == "Miscanthus", "sub.family"] <- "Andropogoneae" 
  
# Schmidtia
  ala6[ala6$genus == "Schmidtia", "sub.family"] <- "Chloridoideae"  
  
# Microstegium
  ala6[ala6$genus == "Microstegium", "sub.family"] <- "Andropogoneae" 
  
# Polytrias
  ala6[ala6$genus == "Polytrias", "sub.family"] <- "Andropogoneae"  
  
# Sorghastrum 
  ala6[ala6$genus == "Sorghastrum", "sub.family"] <- "Andropogoneae"    
  
# Crypsis
  ala6[ala6$genus == "Crypsis", "sub.family"] <- "Chloridoideae"  
  
  
# updating Panicum, by species, from Osborne --------------
  ala.pan <- filter(ala6, genus == "Panicum") %>%
    dplyr::select(species, genus) %>%
    distinct(species, .keep_all = T)
  head(ala.pan)
  
  ala.pan.pp <- left_join(ala.pan, panicum, by = "species")
  head(ala.pan.pp)
  
  ala.pan.pp$pani.pp[is.na(ala.pan.pp$pani.pp)] <- "C4"
  head(ala.pan.pp)
  pan <- ala.pan.pp
  colnames(pan) <- c("species", "genus", "pp")
  head(pan)  

# insert back into ala (this is tricky)
# isolate Panicum and remove 'wrong' pp
  ala.p <- filter(ala6, genus == "Panicum") %>%
    dplyr::select(-pp)
  
# join with updated list
  ala.p2 <- left_join(ala.p, pan, by = c("genus", "species"))  
  table(ala.p2$pp)
  
# append
  ala.no.p <- filter(ala6, genus != "Panicum")
  ala.append <- rbind(ala.p2, ala.no.p) %>%
    group_by(species) %>%
    arrange(species) %>%
    droplevels()
  head(ala.append)
  table(ala.append$sub.family, exclude = F)
  
# step 4: grand merge -----------------------------------------------------------
  ala.update <- left_join(ala, ala.append, by = c("species", "genus"))
  
# checks 
  head(ala.update)
  table(ala.update$pp, ala.update$genus == "Panicum", exclude = F)
  table(ala.update$pp, exclude = F)
  table(ala.update$species == "Neurachne alopecuroidea", exclude = F)
  
# only keep ala.update
  #rm(list=setdiff(ls(), "ala.update"))
  saveRDS(ala.update, "ALA/Generated data from cleaning steps/Step 3 updated pp tribes.rds")
  
# ------------------------------------------------------------------  
  
  
 
  
  