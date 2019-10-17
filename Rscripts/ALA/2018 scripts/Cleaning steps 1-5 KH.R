
# Date created: 5/2/19
# Last modified: 

# The aim of this script is doublecheck Richard's cleaning step 1-5 scripts
# I copied what he did in each script and wnt through it myself.
# The data he used are all here; where I manually changed things, see the METADATA (i.e. native/naturalised tags)

# 0. Library --------------------------------------------------------
  library(dplyr)
  
  rm(list = ls())
  
  setwd("C:/Users/s436862/Dropbox/Rarefaction/Data files")
  

  
# 1. Assess AVH code -----------------------------------------------------
# This section is based off Richard's Step 1-6 code in 'Cleaning AVH RD'
  
# 1.1 Create distribution codes for APC data ---------------------------
# function to trim leading and trailing white space
  trim <- function( x ) {
    gsub("(^[[:space:]]+|[[:space:]]+$)", "", x)
  }  # not sure what this does
  
# APC name data
  apc <- read.csv("AVH/nslsimplename-2015-06-25-2021.csv")
  glimpse(apc)
  
# include only the APC names
  dat <- filter(apc, rank == "Species" & classifications %in% c("[APC, APNI]", "[APNI, APC]"))
  
  a <- dat$apc_distribution
  b <- strsplit(as.character(a), split = ",")
  n_col <- unlist(lapply(b, function(x) length(x)))
  
  c <- matrix("", nrow = nrow(dat), ncol = max(n_col))
  for(i in 1:length(a)) if(n_col[i] > 0) c[i, 1:n_col[i]] <- b[[i]]
  
# trim leading and trailing white space
  c <- apply(c, 2, function(x) trim(x))
  table(c) # all of the tags you can have (i.e. WA, VIC, VIC naturalised, etc.)
  
  write.csv(table(c), "c:\\users\\s429217\\onedrive\\data\\avh\\distribution fields out.csv")# ***
  
# 1.2 Read in APC data and classify as native/naturalised -----------------------------
# Notes -------------------------------------------------------------------------------
# *** = Prior to this step Richard (and I double-checked) the field codes [APC classification] for any 'naturalised' tag which indicates non-native to that area.
# I removed two islands (Lord Howe Island [LHI/HI] and Maquarie [MI]; see MEATADATA for more info)
# And I removed all tags that mentioned the word 'extinct'.
# -------------------------------------------------------------------------------------
# native/naturalised codes
  distr <- read.csv("AVH/distribution fields updated KH.csv")
  distr <- mutate(distr, native = ifelse(is.na(native) == T, 0, 1),
                  naturalised = ifelse(is.na(naturalised) == T, 0, 1))
  
  names(distr) <- c("id", "field", "native", "naturalised", "freq")                       
  
# check method
  write.csv(distr, "AVH/Distribution fields updated KH.csv", row.names = F)
  
# identify species classified as native and naturalised (exotic)
# and native species that have naturalised in Australia
  exo <- matrix(0, nrow = nrow(c), ncol = ncol(c))
  nat <- matrix(0, nrow = nrow(c), ncol = ncol(c))
  
# create vectors to speed things up
  field <- distr$field
  naturalised <- distr$naturalised
  native <- distr$native
 
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
  write.csv(dat.final, "AVH/APC names native naturalised.csv")
  
# 1.3 Read in AVH data and output native/naturalised records ----------------------------------------  
# here we want to tag each species in the AVH data as nativ or exotic, and make sure that the names fit between them. I.e. that AVH names are ocnsistent with APC. (If not, I will cull them.)
  
# read in AVH data
  avh <- read.table("AVH/AVH data extracted.dat", sep="\t", header=T, strip.white = T)
  dim(avh)
  
# list of species
  spp.list <- levels(avh$Species...matched)
  length(spp.list) # 25,913 species
  
# read in APC names
  apc <- read.csv("AVH/APC names native naturalised.csv", strip.white = T)
  apc.nat <- select(apc, name, naturalised)
  sum(apc.nat$naturalised) 
  length(apc$naturalised[apc$naturalised == 0]) # 23,618 species
  
# compare and correct names --------------------------------------------------------------
# there is a wee issue that comes down the line of some species have an " x " in the middle of their species epithet. I am going to remove this now from the AVH data and APC names
  avh$Species...matched <- gsub(" x ", " ", avh$Species...matched)
  
# check problem with " x " in avh names
  dplyr::filter(avh, grepl(" x ", Species...matched)) # sweet
 
# check in APC data
  apc.nat$name <- gsub(" x ", " ", apc.nat$name)
  dplyr::filter(avh, grepl(" x ", Species...matched)) # sweet
 
# extract names from avh and apc
  avh$Species...matched <- as.factor(avh$Species...matched)
  avh.names <- data.frame(sci.name = as.character(levels(avh$Species...matched)), avh = 1)  
  
  apc.nat$name <- as.factor(apc.nat$name)
  apc.names <- data.frame(sci.name = as.character(levels(apc.nat$name)), apc = 1, native = table(apc.nat$name, apc.nat$naturalised)[, 1])
  
# where names match, code 1, where they don't, code 0
  allnames <- merge(avh.names, apc.names, by = "sci.name", all = T) # 29622 (vs. APC's 25913)
  allnames <- mutate(allnames, avh = ifelse(is.na(avh) == T, 0, 1),
                     apc = ifelse(is.na(apc) == T, 0, 1),
                     tot = avh + apc)
  
# take only names which have both AVH and APC (i.e. to = 2)  
  matching.names <- filter(allnames, tot == 2)
  
# subset avh based on these names
  avh$Species...matched <- as.character(avh$Species...matched)
  apc.names$sci.name <- as.character(apc.names$sci.name)
  
  avh.apc.names <- filter(avh, avh$Species...matched %in% apc.names$sci.name)
  
# add native status to avh data
  colnames(apc.names) <- c("Species...matched", "apc", "native")
  apc.status <- select(apc.names, Species...matched, native)
  avh.stat <- left_join(avh.apc.names, apc.status, by = "Species...matched")
  
# save all native & exotic species in AVH data that meet APC naming criteria
  write.csv(avh.stat, "AVH/2019 master data/AVH records.csv", row.names = F)
  

# 1.4 removing duplicates ---------------------------------------------------------
# Gallagher 2016: removing non-unique names and latitude and longitude value combinations
  avh <- read.csv("AVH/2019 master data/AVH records.csv", header = T) %>% 
    mutate(id = 1:nrow(avh))
  
# select information duplicates across columns; label with row ID
  #dat <- select(avh, Species...matched, Year...parsed, Latitude...processed, Longitude...processed) %>%
    #mutate(id = 1:nrow(dat))
  
# find unique records 
  dat.dist <- avh %>% distinct(Species...matched, Year...parsed, Latitude...processed, Longitude...processed, .keep_all = TRUE) %>%
    select(-id)
  
# find duplicate records
  #dat.dupl <- anti_join(dat, dat.dist, by = "id")
  
# I don't know why i went through that all ... distinct did it all for me?
# anyways, I will save the new data frame without the duplicate records in it
  
# 1.5 save outputs ------------------------------------------------------------------
# avh mastercopy
  write.csv(dat.dist, "AVH/2019 master data/AVH records.csv", header = T)
  
# all grasses -- note I changed column names and filtered some out here but not for the master copy (above) 
  avh.grass <- filter(dat.dist, Family...matched == "Poaceae") 
  
  colnames(avh.grass) <- c("scientific.name", "matched.scientific.name", "order",
                           "family", "genus", "species", "latitude",
                           "longitude", "IBRA.region", "state", "year",
                           "taxon.identification.issue", "inferred.duplicate.Record", 
                           "name.not.in.national.checklists", "native")
  avh.grass.kh <- select(avh.grass, species, family, genus, latitude, longitude, year, native) %>%
    group_by(species)
  
  sum(table(avh.grass.kh$species)) # 244,141 records
  length(unique(avh.grass.kh$species)) # 1392 spp.
  
  write.csv(avh.grass.kh, "AVH/AVH grass records.csv", row.names = F)  

  

  
  
