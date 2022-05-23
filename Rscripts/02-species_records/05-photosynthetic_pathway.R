

# library -----------------------------------------------------------
  library(tidyverse)
  library(raster)
  
  rm(list = ls())

# data ---------------------------------------------------------------
# ala species
  ala <- readRDS("Data files/ALA/cleaning steps/step 3 - ALA records APC species names and status.RDS") 

# Australia coordinates 
  aus <- raster("Data files/Australia/Australia 1104.grd")
  
# Osborne's pp data   
  os <- read.csv("Data files/Osborne C3-C4/c3-c4.csv", fileEncoding = "UTF-8-BOM", header = T)
  colnames(os) <- c("pp", "genus")
  table(os$pp) # note there are 'C3 and C4's and 'unknown' categories

# Osborne et al. (2014) panicum data (assigned by spp. not genus)
  os_panicum <- read.csv("Data files/Osborne C3-C4/Panicum.csv", fileEncoding = "UTF-8-BOM", header = T) %>%
    dplyr::select(Species, Pathway)
  colnames(os_panicum) <- c("species", "pani.pp")
  head(os_panicum)  

# Megathyrsus maxmimus is a synonym for Panicum maximum
  ala[ala$genus == "Megathyrsus", "genus"] <- "Panicum"
  ala[ala$species == "Megathyrsus maximus", "species"] <- "Panicum maximum"  
  
# assign pathways by genus using Osborne et al. (2014)-------------------
  gen <- distinct(ala, genus) %>%
    arrange(., genus)
  
  gen2 <- left_join(gen, os, by = "genus")
  
  table(gen2$pp, exclude = NULL) # tidy up these
  
# assign remaining genera using Watson: https://www.delta-intkey.com
# note: there are 23 NAs and a few which are both C3 and C4
# if Watson can't resolve these, we will exclude from study
  gen2_na <- filter(gen2, is.na(gen2$pp) == T)
  gen2_na
  gen2_bothpp <- filter(gen2, pp == "C3 & C4")
  gen2_bothpp
  
# C3
  c3 <- c("Achnatherum",
          "Amelichloa",   # reference: https://www.delta-intkey.com/grass/www/nassella.htm
          "Amphibromus",
          "Anthosachne",  # reference: https://www.delta-intkey.com/grass/www/elymus.htm
          "Austrostipa",
          "Australopyrum",
          "Avellinia",
          "Chascolytrum",
          "Deyeuxia",
          "Dichanthelium",
          "Elytrigia",
          "Hookerochloa",
          "Jarava",
          "Lachnagrostis",
          "Lophopyrum",
          "Periballia",
          "Microlaena",
          "Molineriella",
          "Neurachne",
          "Saxipoa", # no data on PP; genus recently moved from Poa, which is C3, so assuming that's conserved
          "Sylvipoa", # no data on PP; genus recently moved from Poa, which is C3, so assuming that's conserved
          "Sasaella",
          "Tetrarrhena",
          "Thinopyrum",
          "Walwhalleya",
          "Zotovia")
# C4
  c4 <- c("Aristida",
          "Diplachne",
          "Eragrostis",
          "Moorochloa",
          "Panicum",
          "Pseudopogonatherum",
          "Thellungia",
          "Zuloagaea")
  
# update gen2
  c3_index <- (gen2$genus %in% c3)
  gen2$pp[c3_index]<- "C3"
  
  c4_index <- (gen2$genus %in% c4)
  gen2$pp[c4_index] <- "C4"
  
# merge genera and pathways
  ala_pp <- left_join(ala, gen2, by = "genus") %>%
    droplevels()
  head(ala_pp)
  table(ala_pp$pp, exclude = F)
  
# remove remaining unresolved pathways
# remove intermediate (C3 & C4) genera 
  table(gen2$pp)
  both <- gen2 %>% filter(pp == "C3 & C4")
  both
  ala_pp2 <- ala_pp %>% filter(genus != "Alloteropsis",
                               genus != "Steinchisma") %>%
                        droplevels()
  
  table(ala_pp2$pp, exclude = F)
   
# update Panicum by species ---------------------------------------------------------
  gen_pan <- filter(ala_pp2, genus == "Panicum") %>%
    dplyr::select(species, genus) %>%
    distinct(species, .keep_all = T)
  head(gen_pan)
  
  gen_pan_pp <- left_join(gen_pan, os_panicum, by = "species")
  head(gen_pan_pp)
  
# label species with no pathway   
  gen_pan_pp$pani.pp[is.na(gen_pan_pp$pani.pp)] <- "remove"
  head(gen_pan_pp)
  pan <- gen_pan_pp
  colnames(pan) <- c("species", "genus", "pp")
  head(pan)  

# isolate Panicum
  ala_pani <- filter(ala_pp2, genus == "Panicum") %>%
    dplyr::select(-pp)
  
# join with updated list
  ala_pani2 <- left_join(ala_pani, pan, by = c("genus", "species"))  
  table(ala_pani2$pp)
  
# append into larger data frame
  ala_no_pani <- filter(ala_pp2, genus != "Panicum")
  ala2 <- rbind(ala_pani2, ala_no_pani) %>%
    group_by(species) %>%
    arrange(species) %>%
    droplevels()
  table(ala2$pp)
  
# checks
  ala3 <- ala2 %>% filter(pp != "remove")
  table(ala3$pp, exclude = F)
  
# save excluded Panicum species  
  excluded_panicum <- ala3 %>% filter(pp == "remove") %>%
                      distinct(species)
  
  write.csv(excluded_panicum, "Results/csv/Panicum species excluded.csv", row.names = F)

# change status column 
  ala4 <- ala3 %>% mutate(status = ifelse(apc.status == 1, "native", "nonnative")) %>% 
            dplyr::select(-apc.status)
  table(ala4$status, exclude = F)
  
# Neurachne munroi is the only C4 Neurachne
  c4_neur <- "Neurachne munroi"
  c3_neur_index <- (ala4$species %in% c4_neur)
  ala4$pp[c3_neur_index]<- "C4"

# crop records (again) ---------------------------------------
  xy <- cbind(ala4$longitude, ala4$latitude)
  cell <- raster::extract(aus, xy)
  
  table(cell, exclude = NULL)
  
  ala4$aus_record <- ifelse(!is.na(cell), "yes", "no")
  table(ala4$aus_record, exclude = NULL)
  
  ala5 <- ala4 %>% filter(aus_record == "yes") %>%
          droplevels()
  
  table(ala5$aus_record, exclude = NULL)
  
# save master copy
  saveRDS(ala5, "Data files/ALA/master data/master grass records.rds")
  saveRDS(ala5, "C:/Users/s436862/Dropbox/Projects/Sue Bryceson/data/ALA records/raw grass records.rds")
  
# ------------------------------------------------------------------------------  