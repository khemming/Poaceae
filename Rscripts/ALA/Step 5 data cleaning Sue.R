###############################################################################
# Step 5 ALA data for Sue 
###############################################################################
# date created: 4/7
# last updated: 



# aim ------------------------------------------------------------
# here I will subset "my" master data based on Sue's requirements:
# (1) no exotic species
# (2) no Triodia genus
# (3) all species need endemic/native status

# note: Sue has sent me an update of some species from my list missing endemic status, a few that APC identified as native are in act exotic. I am keeping this so for replicability reasons. Whereas in Sue's data I will note the changes and the new source (which is Randall as a source.)


# library --------------------------------------------------------
  setwd("C:/Users/s436862/Dropbox/Poaceae/Data files")
  
  library(tidyverse)
  library(data.table)
  
  rm(list = ls())


# data ------------------------------------------------------------------------
# master data
  dat.all <- readRDS("ALA/ALA master data/master grass records.rds")

  updated.spp <- read.csv("ALA/Supplied data/Sue spp endemics.csv") %>%
                  dplyr::select(species, endemic)
  
# step 1: remove incorrect species 
# misspelling: Deyeuxia mckiei
  dat.all[dat.all$species == "Deyeuxia mckiei", "species"] <- "Deyeuxia mackiei"
  
# unnamed and hybrids: Spinifex x alterniflorus, Hymenachne x calamitosa, Lachnagrostis x punicea, & Lachnagrostis x contracta
  dat.all2 <- filter(dat.all, species != "Spinifex alterniflorus" &
                             species != "Hymenachne calamitosa" & 
                             species != "Lachnagrostis punicea" & 
                             species != "Lachnagrostis contracta")
  
# Triodia genus
  dat.all3 <- filter(dat.all2, genus != "Triodia")
  
# exotic species
  dat.all4 <- filter(dat.all3, status != "exotic")
  
# step 2: correct endemism column in species list 
  dat.spp <- distinct(dat.all4, species, .keep_all = T) %>%
              dplyr::select(species, endemic) 

# take out all species missing endemic status 
  dat.spp2 <- drop_na(dat.spp) 

  dat.spp3 <- rbind(dat.spp2, updated.spp) # puts them back in (same spp. no. as dat.spp)
  
  
# step 3: merge endemism into record data frame 
  dat.all5 <- dplyr::select(dat.all4, -endemic)
  
  dat.all6 <- left_join(dat.all5, dat.spp3, by = "species")  

# checks
  table(dat.all6$endemic, exclude = F)

  na <- filter(dat.all6, is.na(endemic) == T)  %>%
        distinct(species)
  
  
# step 4: save
  write.csv(dat.all6, "Sue/Master grass data/master native grass records.csv", row.names = F)
  
# -----------------------------------------------------------------------------------
  
  
  

