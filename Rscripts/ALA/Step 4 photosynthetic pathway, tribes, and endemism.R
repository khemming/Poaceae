########################################################
# add photosynthetic pathway, tribes; update names and origin
########################################################
# date created: 11/6
# last updated: 27/6

# aim ------------------------------------------------------------
# update: (1) names, (2) origin, (3) tribe, based Sue's manual additions
# Also (4) photosynthetic pathway: C3/C4
# much of this is manually added


# library --------------------------------------------------------
  library(tidyverse)
  library(data.table)
  
  rm(list = ls())

  setwd("C:/Users/s436862/Dropbox/Poaceae/Data files")
  
# data ---------------------------------------------------------------
# ala species
  ala <- readRDS("ALA/Generated data from cleaning steps/step 3 ALA records APC species names and status.rds") 
  
# Sue's tribe etc. data
  sue <- read.csv("ALA/Supplied data/6.AVH_grass-SB-endemics for KH.csv", fileEncoding = "UTF-8-BOM")
  head(sue)
  table(sue$endemic)
  
# Osborne's pp data   
  os <- read.csv("Osborne C3-C4/c3-c4.csv", fileEncoding = "UTF-8-BOM", header = T)
  colnames(os) <- c("o.pp", "genus")
  table(os$o.pp) # note there are 'C3 and C4's and 'unknown' categories

# their panicum data (assigned by spp. not genus)
  panicum <- read.csv("Osborne C3-C4/Panicum.csv", fileEncoding = "UTF-8-BOM", header = T) %>%
    dplyr::select(Species, Pathway)
  colnames(panicum) <- c("species", "pani.pp")
  head(panicum)  


# step 1: origin update --------------------------------
# Sue's list is more accurate than the APC one, so I will look for discrepencies between the APC and her list and update with Sue's if necessary
# Cenchrus purpurascens is being changed to native in APC status
  ala[ala$species == "Cenchrus purpurascens", "apc.status"] <- 1 
  
# Megathyrsus (synonym for Panicum maximum)
  ala <- filter(ala, genus != "Megathyrsus")
  ala[ala$genus == "Megathyrsus", "genus"] <- "Panicum"
  ala[ala$species == "Megathyrsus maximus", "species"] <- "Panicum maximum"
  
# Molineriella (synonym for Periballia)
  ala[ala$genus == "Molineriella", "genus"] <- "Periballia"
  ala[ala$species == "Molineriella minuta", "species"] <- "Periballia minuta"
  
# apc species list
  apc.spp <- ala %>% distinct(., species, .keep_all = T)
  
# Sue species data
  sue.spp <- sue %>% distinct(., species, .keep_all = T)
  
  spp1 <- left_join(apc.spp, sue.spp, by = c("species", "genus"))  
  
  table(spp1$apc.status, spp1$endemic, exclude = F)
  

  
# step 2: assign photosynthetic pathway -----------------------------------------
# as well as Obsorne, Sue also has pp data, from Watson
# since we're using that source anyways, I am going to fill any gaps that Obsorne has with Sue's list
  sue.pp <- sue %>% dplyr::select(genus, pp) %>%
            distinct(genus, .keep_all = T) %>%
            rename(s.pp = pp)

  ala.gen <- ala %>% distinct(genus)
  
# merge
  gen.pp1 <- left_join(ala.gen, os, by = "genus")
  gen.pp2 <- left_join(gen.pp1, sue.pp, by = "genus")
  head(gen.pp2) 
  
  table(gen.pp2$o.pp, gen.pp2$s.pp, exclude = F)
  

# add C3 and C4 separately 
  gen.c3 <- mutate(gen.pp2, final.pp = ifelse(s.pp == "C3"| o.pp == "C3", 1, 0)) %>%
    filter(., final.pp == 1) %>%
    mutate(final.pp = "C3")
  
  gen.c4 <- mutate(gen.pp2, final.pp = ifelse(s.pp == "C4"| o.pp == "C4", 1, 0)) %>%
    filter(., final.pp == 1) %>%
    mutate(final.pp = "C4")
  
  gen4 <- rbind(gen.c3, gen.c4)
  
  table(gen4$final.pp, exclude = F)
  
# merge with species list
  spp2 <- left_join(spp1, gen4, by = "genus") %>%
          dplyr::select(-o.pp, -s.pp, -pp)
  table(spp2$final.pp, exclude = F)

  spp3 <- spp2 %>% rename(pp = final.pp)
  
# updating 16 spp with no pp by genus --------------------------  
# manually taken from Watson's site: https://www.delta-intkey.com/
  ala6 <- spp3
  
  gen.missing.pp <- filter(spp3, is.na(pp) == T) %>% 
                    dplyr::select(genus) %>%
                    distinct()
 
# 1) Lophopyrum
  ala6[ala6$genus == "Lophopyrum", "pp"] <- "C3"
  ala6[ala6$genus == "Lophopyrum", "sub.family"] <- "C3"
  
# 2) Thinopyrum
  ala6[ala6$genus == "Thinopyrum", "pp"] <- "C3"
  ala6[ala6$genus == "Thinopyrum", "sub.family"] <- "C3"
  
# 3) Avellinia
  ala6[ala6$genus == "Avellinia", "pp"] <- "C3"
  ala6[ala6$genus == "Avellinia", "sub.family"] <- "C3"
  
# 4) Molineriella (synonym for Periballia)
  ala6[ala6$genus == "Periballia", "sub.family"] <- "C3"
  ala6[ala6$genus == "Periballia", "pp"] <- "C3"

  
# 5) Elytrigia  
  ala6[ala6$genus == "Elytrigia", "pp"] <- "C3"
  ala6[ala6$genus == "Elytrigia", "sub.family"] <- "C3"
  
# 6) Amelichloa (synonym for Nassella, but still recognised spp.)
  ala6[ala6$genus == "Amelichloa", "pp"] <- "C3"
  ala6[ala6$genus == "Amelichloa", "sub.family"] <- "C3"
  
# 7) Steinchisma  
  ala6[ala6$genus == "Steinchisma", "pp"] <- "intermediate"
  ala6[ala6$genus == "Steinchisma", "sub.family"] <- "Paniceae"
  
# 8) Moorochloa
  ala6[ala6$genus == "Moorochloa", "pp"] <- "C4"
  ala6[ala6$genus == "Moorochloa", "sub.family"] <- "Paniceae"
  
# 9) Zuloagaea
  ala6[ala6$genus == "Zuloagaea", "pp"] <- "C4"
  ala6[ala6$genus == "Zuloagaea", "sub.family"] <- "Paniceae" 
  
# 10) Jarava
  ala6[ala6$genus == "Jarava", "pp"] <- "C3"
  ala6[ala6$genus == "Jarava", "sub.family"] <- "C3" 
  
# check
  table(ala6$pp, exclude = F)
  na.pp <- filter(ala6, is.na(pp == T))
  
# step 3: update tribes ----------------------------------
  table(ala6$sub.family, exclude = F)

# C3 spp are C3 tribes
  ala6[ala6$pp == "C3", "sub.family"] <- "C3"
  table(ala6$pp, ala6$sub.family, exclude = F)
  
# and we'll look up C4s on Watson's site: https://www.delta-intkey.com/
  missing.c4.tribes <- filter(ala6, is.na(sub.family) == T) %>% 
                    dplyr::select(genus, sub.family) %>%
                    distinct(genus)

# do this by tribe for convenience    

# Andropogoneae
  and <- c("Andropogon",
           "Bothriochloa",
           "Chrysopogon",
           "Coix",
           "Cymbopogon",
           "Dichanthium",
           "Hyparrhenia",
           "Ischaemum",
           "Microstegium",
           "Miscanthus",
           "Polytrias",
           "Sorghastrum",
           "Saccharum",
           "Schizachyrium",
           "Sorghum",
           "Thaumastochloa",
           "Themeda",
           "Zea"
           )
  and.index <- (ala6$genus %in% and)
  ala6$sub.family[and.index]<- "Andropogoneae"
  
# Chloridoideae
  chl <- c("Chloris",
           "Crypsis",
           "Cynodon",
           "Dactyloctenium",
           "Desmostachya",
           "Dinebra",
           "Eleusine",
           "Eragrostis",
           "Eustachys",
           "Perotis",
           "Schmidtia",
           "Spartina",
           "Sporobolus",
           "Tragus",
           "Triodia",
           "Zoysia"
           )
  
  chl.index <- (ala6$genus %in% chl)
  ala6$sub.family[chl.index] <- "Chloridoideae"
  
  table(ala6$sub.family, exclude = F)
  
# Paniceae
  pan <- c("Axonopus", 
          "Cenchrus",
          "Digitaria",
          "Echinochloa",
          "Eriochloa",
          "Melinis",
          "Panicum",
          "Paspalidium",
          "Paspalum",
          "Pseudoraphis",
          "Rottboellia",
          "Setaria",
          "Spinifex",
          "Stenotaphrum",
          "Urochloa"
          )
  pan.index <- (ala6$genus %in% pan)
  ala6$sub.family[pan.index] <- "Paniceae"
  

# not using but put in anyway: Aristidoideae
  ari <- "Aristida"
  ari.index <- (ala6$genus %in% ari)
  ala6$sub.family[ari.index] <- "Aristidoideae"
  
# check 
  table(ala6$sub.family, exclude = F) 
  pp.nas <- filter(ala6, is.na(sub.family) == T) 
  
# step 4: update Panicum ------------------------------------
# by species, from Osborne & Watson 
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

# insert back into ala 
# isolate Panicum and remove 'wrong' pp
  ala.p <- filter(ala6, genus == "Panicum") %>%
    dplyr::select(-pp)
  
# join with updated list
  ala.p2 <- left_join(ala.p, pan, by = c("genus", "species"))  
  table(ala.p2$pp)
  
# append
  ala.no.p <- filter(ala6, genus != "Panicum")
  ala7 <- rbind(ala.p2, ala.no.p) %>%
    group_by(species) %>%
    arrange(species) %>%
    droplevels()
  head(ala7)
  
# checks
  table(ala7$sub.family, exclude = F)
  table(ala7$pp, exclude = F)
  
# step 4: grand merge -----------------------------------------------------------
# remove unnecessary columns
  ala8 <- dplyr::select(ala7, species, genus, pp, sub.family, endemic)
  
  ala9 <- left_join(ala, ala8, by = c("species", "genus"))
  
# checks 
  head(ala9)
  table(ala9$pp, ala9$genus == "Panicum", exclude = F)
  
  na.pp <- filter(ala9, is.na(pp) == T) %>%
            distinct(species, .keep_all = T)
  
  table(ala9$pp, exclude = F)
  table(ala9$endemic, exclude = F)
  
  no.end <- filter(ala9, is.na(endemic) == T) %>%
            distinct(species, .keep_all = T)
  
  table(ala9$species == "Neurachne alopecuroidea", exclude = F)
  
# change status column 
  ala10 <- ala9 %>% mutate(status = ifelse(apc.status == 1, "native", "exotic"))
  table(ala10$status, exclude = F)
  table(ala9$apc.status, exclude = F)
  
  ala11 <- dplyr::select(ala10, -apc.status)
  
# save (my) master copy
  saveRDS(ala11, "ALA/ALA master data/master grass records.rds")
  
# ------------------------------------------------------------------------------  

  
  