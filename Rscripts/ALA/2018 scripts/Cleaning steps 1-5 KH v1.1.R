
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
# note: I did not run this code; I received its output as 'distribution fields out'
# function to trim leading and trailing white space
  trim <- function( x ) {
    gsub("(^[[:space:]]+|[[:space:]]+$)", "", x)
  }  # not sure what this does

# APC name data
  apc <- read.csv("AVH/Supplied data/nslsimplename-2015-06-25-2021.csv")
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

  write.csv(table(c), "AVH/Generated data from cleaning steps/distribution fields out.csv")

# -----------------------------------------------
  
# 1.2 Read in APC data and classify as native/naturalised -----------------------------
# Notes -------------------------------------------------------------------------------
# *** = Richard, and I double-checked, the field codes [APC classification] for any 'naturalised' tag which indicates non-native to that area.
# I removed two islands (Lord Howe Island [LHI/HI] and Maquarie [MI]; see MEATADATA for more info)
# And I removed all tags that mentioned the word 'extinct'
# Tags with 'cultivated' or whatever were removed
# -------------------------------------------------------------------------------------
# native/naturalised codes
  distr <- read.csv("AVH/Generated data from cleaning steps/distribution fields updated KH.csv") # *** see notes above
  distr <- mutate(distr, native = ifelse(is.na(native) == T, 0, 1),
                  naturalised = ifelse(is.na(naturalised) == T, 0, 1))
  
  names(distr) <- c("id", "field", "native", "naturalised", "freq")                       
  
# check method
  write.csv(distr, "AVH/Generated data from cleaning steps/distribution fields updated KH.csv", row.names = F)
  
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
  write.csv(dat.final, "AVH/Generated data from cleaning steps/APC names native naturalised.csv")
  
# 1.3 Read in AVH data and output native/naturalised records ----------------------------------------  
# here we want to tag each species in the AVH data as nativ or exotic, and make sure that the names fit between them. I.e. that AVH names are consistent with APC. (If not, I will cull them.)

# this is in two parts: first, I do it with Richard's method: using APC names
# second, verify this using Randall's list, donated by Rachel Gallagher
  
# part 1 -----------------------------------------------------------------------
# read in AVH data
  avh <- read.table("AVH/Supplied data/AVH data extracted.dat", sep="\t", header=T, strip.white = T)
  dim(avh)
  
# list of species
  spp.list <- levels(avh$Species...matched)
  length(spp.list) # 25,913 species
  
# read in APC names
  apc <- read.csv("AVH/Generated data from cleaning steps/APC names native naturalised.csv", strip.white = T)
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
  
# part 2 --------------------------------------------------------------------------
# Using Rachel/Randall for the rest of the naturalised status tagging
# my data
  kh <- avh.stat
  kh.spp <- distinct(kh, Species...matched, .keep_all = T) %>%
    mutate(., native.kh = native) %>%
    select(-native)
  
# Rachel/Randall's
  rg <- read.csv("AVH/Supplied data/Australia_plant_Census.csv", header = T)
  rg.spp <- distinct(rg, Taxon, .keep_all = T) %>%
    select(., Taxon, native, nativeAndNaturalised, potentiallyNativeAndNaturalised) %>%
    mutate(., native.rg = ifelse(native == T, 1, 0)) %>%
    select(-native)
  
# merge the two lists on common species 
  kh.rg.spp <- inner_join(kh.spp, rg.spp, by = c("Species...matched" = "Taxon"))
  
# we lost some species joining them -- what are these? Let's have a look
  kh.rg.anti <- anti_join(kh.spp, rg.spp, by = c("Species...matched" = "Taxon"))
  
# get the missing species back
  kh.rg.spp.2 <- full_join(kh.rg.spp, kh.rg.anti)
  
# check: Rachel's list has 'nativeAndNAturalised' tag, which I think are actually native for my purposes
# I want to check if there are overlaps here
  n.nat <- filter(kh.rg.spp.2, nativeAndNaturalised == T & native.rg == 0) # 27 species fit this
  n.nat2 <- filter(kh.rg.spp.2, nativeAndNaturalised == T & native.kh == 0) # 0 of mine do. Cool.
  
# RG identified one grass that is native and naturalised, put is down as exotic (Aristida jerichoensis)
# manually changed it
  kh.rg.spp.2[1462, c(15, 18)] <- 1
  
# now on row "potentiallyNativeAndNAturalised"
  n.nat3 <- filter(kh.rg.spp.2, potentiallyNativeAndNaturalised == T & native.rg == 0) # 2 species fit this
  n.nat4 <- filter(kh.rg.spp.2, potentiallyNativeAndNaturalised == T & native.kh == 0) # 0 of mine do. Cool.
  # these aren't poaceae, so I don't care
  
# where I have put native and Rachel has put exotic, I want to take Rachel's value. These will be the spp. that slipped the APC net, and were picked up by Randall's list.
  # where I have put exotic and Rachel native, I will have a look at the prevalance
  # prevalance
  nat.exo <- filter(kh.rg.spp.2, native.kh == 0 & native.rg == 1) # there are no Poaceae, good
  
# slipped thorugh spp. 
  exo.nat <- filter(kh.rg.spp.2, native.kh == 1 & native.rg == 0) %>% 
    filter(., Family...matched == "Poaceae") # 25 Poaceae here
  
# update native.kh
  exo.nat$native.kh <- 0
  
# update spp. list
  kh.rg.spp.2[match(exo.nat$Species...matched, kh.rg.spp.2$Species...matched), ] <- exo.nat # manually checked with a changed spp. (Aristida behriana) and it is now an exotic
  spp.list <- kh.rg.spp.2 %>%
    mutate(., native = native.kh) %>%
    select(Species...matched, native)
  
# update avh list
  kh2 <- kh %>% select(-native)
  kh3 <- left_join(kh2, spp.list, by = "Species...matched")
  
# save it
  write.csv(kh3, "AVH/Generated data from cleaning steps/AVH status matched records.csv", row.names = F)
  
# 1.4 removing duplicates ---------------------------------------------------------
# removing non-unique species, year and latitude and longitude combinations rounded to ~1-km
# note: the commented-out parts are there if you want to look at which records have been kept and which have been reomoved. (Just change avh -> dat in 'find unique (distinct) records' code.)
  avh <- read.csv("AVH/Generated data from cleaning steps/AVH status matched records.csv", header = T) %>% 
    mutate(id = 1:nrow(.))
         
# round lat/longs to ~1-km (2dp)
  avh$Latitude...processed <- round(avh$Latitude...processed, digits = 2)
  avh$Longitude...processed <- round(avh$Longitude...processed, digits = 2)
  
# identify duplicates based on same year, species and lat/long; label each spp. record with unique ID
  #dat <- select(avh, Species...matched, Year...parsed, Latitude...processed, Longitude...processed) %>% 
         #mutate(id = 1:nrow(.))
  
# find unique (distinct) records 
  dat.dist <- avh %>% distinct(Species...matched, Year...parsed, Latitude...processed, Longitude...processed, .keep_all = TRUE) %>%
    select(-id)
  
# find duplicate records
  #dat.dupl <- anti_join(dat, dat.dist, by = "id")
  
# save outputs 
# avh mastercopy
  write.csv(dat.dist, "AVH/2019 master data/AVH records.csv", row.names = F)
  
# all grasses -- note I changed column names and filtered some out here but not for the master copy (above) 
  avh.grass <- filter(dat.dist, Family...matched == "Poaceae") 
  
  colnames(avh.grass) <- c("scientific.name", 
                           "matched.scientific.name", 
                           "order",
                           "family", 
                           "genus", 
                           "species", 
                           "latitude",
                           "longitude", 
                           "IBRA.region", 
                           "state", 
                           "year",
                           "taxon.identification.issue", 
                           "inferred.duplicate.Record", 
                           "name.not.in.national.checklists", 
                           "native")
  
  avh.grass.kh <- select(avh.grass, species, family, genus, latitude, longitude, year, native) %>%
    group_by(species)
  
  sum(table(avh.grass.kh$species)) # 225,320 records
  length(unique(avh.grass.kh$species)) # 1392 spp.
  
  write.csv(avh.grass.kh, "AVH/2019 master data/AVH grass records.csv", row.names = F)  

  
# ----------------------------------------------------------------------------

