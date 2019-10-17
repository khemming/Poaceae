

# removing duplicates and saving final copies ---------------------------------------------------------
# removing non-unique species, year and latitude and longitude combinations rounded to ~1-km
# note: the commented-out parts are there if you want to look at which records have been kept and which have been reomoved. (Just change avh -> dat in 'find unique (distinct) records' code.)

# Library --------------------------------------------------------
library(dplyr)

rm(list = ls())

setwd("C:/Users/s436862/Dropbox/Rarefaction/Data files")


# data
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