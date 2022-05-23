# Read in AVH data and Rachel's Randell list of also native/exotic specoies tags and output native/naturalised records ----------------------------------------  
# here we want to tag each species in the AVH data as nativ or exotic, and make sure that the names fit between them. I.e. that AVH names are consistent with APC. (If not, I will cull them.)

# this is in two parts: first, I do it with Richard's method: using APC names
# second, verify this using Randall's list, donated by Rachel Gallagher

# Library --------------------------------------------------------
  library(dplyr)
  
  rm(list = ls())
  
  setwd("C:/Users/s436862/Dropbox/Rarefaction/Data files")

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
  apc.names <- data.frame(sci.name = as.character(levels(apc.nat$name)), 
                          apc = 1, 
                          native = table(apc.nat$name, apc.nat$naturalised)[, 1])

  apc.names.test1 <- data.frame(native = table(apc.nat$name, apc.nat$naturalised)[, 1]) # somehow this gives us '4' on some species...
  apc.names.test2 <- data.frame(table(apc.nat$name, apc.nat$naturalised))
  apc.names.test3 <- data.frame(table(apc.nat$name)) # somehow there are double ups here ...
  
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

  avh.stat.fil <- filter(avh.stat, native == 2) # why are there 15 k records for which there is >1 native? IT'S DICHOTOMOUS
  
  
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
  
  
  
  