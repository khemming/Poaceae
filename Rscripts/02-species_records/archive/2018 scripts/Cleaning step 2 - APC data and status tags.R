# date created: 22/1

# Read in APC data and classify as native/naturalised -----------------------------


# Library --------------------------------------------------------
  library(dplyr)
  
  rm(list = ls())
  
  setwd("C:/Users/s436862/Dropbox/Rarefaction/Data files")

# updated distribution fields
# note ------------------------------------------------------------------------------
# I updated the list we made in he previous script by individually tagging each state tag as naturalised or native, in their own columns.
# I excluded tags based on if they were: ambiguous, thought to be extinct, two offshore islands (Lord Howe [LH] and MAcquarie [MI], leaving the other islands ot be removed later via their location-tags).
# -----------------------------------------------------------------------------------
  c <- read.csv("AVH/Generated data from cleaning steps/Distribution fields updated.csv", header = T)
  
  exo <- matrix(0, nrow = nrow(c), ncol = ncol(c))
  nat <- matrix(0, nrow = nrow(c), ncol = ncol(c))
  
# create vectors to speed things up
  field <- c$field
  naturalised <- c$naturalised
  native <- c$native
  
# identiy whether each species is listed as native or naturalised from the fields
  exo <- rowSums(apply(c, c(1, 2), function(x) naturalised[which(field == x)]))
  nat <- rowSums(apply(c, c(1, 2), function(x) native[which(field == x)]))
  
# now combine these to determine exclusively naturalised species (i.e. no native entry)
  exc_nat <- ifelse(nat == 0 & exo > 0, 1, 0) 
  
# number of naturalised species
  sum(exc_nat) # 3,000 exotics
  
# native species that have naturalised elsewhere in Australia
  native_nat <- ifelse(nat == 1 & exo > 0, 1, 0) 
  sum(native_nat) # 150 natives that have adventured outwards
  
# list each species as native/naturalised (0/1)
  dat.final <- mutate(dat, naturalised = exc_nat, native_naturalised = native_nat) %>%
    dplyr:::select(name, naturalised, native_naturalised, apc_name, apc_familia, familia, author, authority, genus, species) %>%
    arrange(familia, name) # so naturalised = exotic and native_naturalised = aventurous native; we assume all else are native
  
# output
  write.csv(dat.final, "AVH/Generated data from cleaning steps/APC names native naturalised.csv")
  
  
  