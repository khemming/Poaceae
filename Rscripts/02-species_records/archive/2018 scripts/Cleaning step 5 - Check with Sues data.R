

# The aim of this script is to compare the list made by myself and Richard with Sue's reccomendations, which is all in "3. Final species lists w tribes.xlsx"

# We will be checking:
# How exotic species have native statuses
# Why the photosynthetic types are different, and updating my selection criteria with a different source
# She's added 3 species (with 1300 records), so i will look at where these came from
# Removed a genus?
# Put species into Tribes <-- very important bit; do this a little later

# Library --------------------------------------------------------
  library(dplyr)
  
  rm(list = ls())
  
  setwd("C:/Users/s436862/Dropbox/Rarefaction/Data files")


# data --------------------------------------------------------------
  avh <- read.csv("AVH/2019 master data/AVH records.csv")
  
  g <- filter(avh, native == 0)
  h <- filter(g, Family...matched == "Poaceae")
    
    
  apc <- read.csv("AVH/Supplied data/nslsimplename-2015-06-25-2021.csv")

  grass <- read.csv("AVH/2019 master data/AVH grass records.csv") %>%
                  select(species, genus, native)
  
# species list
  spp <- distinct(dat, .keep_all = T)

# genus list
  gen <- select(dat, genus, native) %>% 
          distinct(.keep_all = T)
  
# taxa of interest identified in Sue's list --------------------------------------------
# first, let's look at the natives that may in fact bre introduced

# Chrysopogon aciculatus 
  chry <- filter(spp, species == "Chrysopogon aciculatus") # good -- it's not native :) 
  
# Cymbopogon martinii
  cymb <- filter(spp, species == "Cymbopogon martinii") # bad -- says native

# how many records of this are there?
  cymb.rec <- filter(avh, Species...matched == "Cymbopogon martinii") # only 3...

# APC have any other tags than just none?
  cymb.apc <- filter(apc, name == "Cymbopogon martinii")
  
# No action: Amphibromus archeri -- says is introduced, pretty sure is native ----------
# https://vicflora.rbg.vic.gov.au/flora/taxon/35666223-8023-4c3d-860e-36547e17dcbd

  
  
  
  
  
  
  
  
  
# Not found?: Anthosachne -- AKA Elymus ------------------------------------------------
# Can't find records of either synonym
  anthosachne <- filter(gen, gen == "Anthosachne") # no records
  elymus <- filter(gen, gen == "Elymus")
  
  anth <- filter(spp, genus == "Anthosachne") # none in original data?
  ely <- filter(spp, genus == "Elymus") 
  
  anth.n <- filter(nat.gen, genus == "Anthosachne") # none?

# Might be in her data and not mine, will check Richard's APC 

# Remove: Calyptochloa sp. Charters Towers (E.J.Thompson+ CHA554) -------------------
caly <- filter(spp, genus == "Calyptochloa")
# indeed: Species == Calyptochloa sp. Charters Towers (E.J.Thompson+ CHA554)
# will need to remove these records


# Incorrect status: Isachne minutula ------------------------------------------------
# From what I can see, it is naturalised
https://biodiversity.org.au/nsl/services/rest/node/apni/2908095

# Incorrect status: Koeleria macrantha-----------------------------------------------
# Wiki says it's North American
https://en.wikipedia.org/wiki/Koeleria_macrantha

# Incorrect status: Lolium persicum -------------------------------------------------
http://inspection.gc.ca/plants/seeds/testing-grading/seeds-identification/lolium-persicum/eng/1476284178870/1476284179275

# Remove: Micraira sp. Purnululu (M.D.Barrett and R.L.Barrett 1507) -----------------  
micr <- filter(spp, genus == "Micraira")
# will need to remove these records

# Incorrect status: Oryza officinalis ----------------------------------------------  
oryz <- filter(spp, species == "Oryza officinalis")
# Definitely introduced: http://tropical.theferns.info/viewtropical.php?id=Oryza+officinalis  

# Remove/incorrect status: Phyllostachys pubescens ----------------------------------  
# This is bamboo -- it's a commercial species originally from China
# To include? Only three occurrences, all same date, similar area
phyl <- filter(spp, species == "Phyllostachys pubescens")

# Remove: Rytidosperma sp. Goomalling (A.G.Gunness et al. OAKP 10/63) ---------------
ryti <- filter(spp, species == "Rytidosperma sp. Goomalling (A.G.Gunness et al. OAKP 10/63)")


# Incorrect status: Acrachne racemosa ----------------------------------------------
# Native to Zimbabwe
https://www.zimbabweflora.co.zw/speciesdata/species.php?species_id=105350

# C3 -> C4: all of Aristida -------------------------------------------------------
# Should probably do this

# Remove: Aristida sp. Upper Fergusson (C.R.Michell 3717) -------------------------
aris <- filter(spp, species == "Aristida sp. Upper Fergusson (C.R.Michell 3717)")

# No action: Chrysopogon aciculatus -----------------------------------------------
# Might be introduced?: http://ausgrass2.myspecies.info/content/chrysopogon-aciculatus
# Apprently not: https://npgsweb.ars-grin.gov/gringlobal/taxonomydetail.aspx?id=10501

# Status change: Cymbopogon martinii---------------------------------------------
# non-native
https://en.wikipedia.org/wiki/Cymbopogon_martinii

# Status change: Cynodon dactylon ------------------------------------------------ 
# Non-native
https://en.wikipedia.org/wiki/Cynodon_dactylon

# Potentially actually native: Digitaria bicornis ------------------------------
# Notes from webpage:  "The species is native to tropical Asia extending into Australia"
https://www.cabi.org/ISC/datasheet/120117

# Status change: Digitaria violascens --------------------  
# Introduced
http://ausgrass2.myspecies.info/content/digitaria-violascens  

# Remove: Dimeria sp. Mosquito Point (J.R.Clarkson+ 9994) ------------------
dime.a <- filter(spp, species == "Dimeria sp. Mosquito Point (J.R.Clarkson+ 9994)")
# 7 records
# Remove: Dimeria sp. Salvator Rosa (R.J.Fensham RJF3643) ------------------  
dime.b <- filter(spp, species == "Dimeria sp. Salvator Rosa (R.J.Fensham RJF3643)")
# 1 record
