# Data created: 11/1/19
# Last modified: 

# v1 --------------------------------------------------------------------------------
# The purpose of this document is to explore the data for explicit use in hSDM modelling exotic grasses of Australia's global and national distributions.

# See METADATA for more info

# Aim-------------------------------------------------------------------
# To assess the data in terms of scale, record distributions, species richness distributions, species-species associations, environmental variable correlates, for input into hSDM script for running data on that.

# 0. Library -----------------------------------------------------------
  library(dplyr)
  library(raster)
  library(car)
  library(ggplot2)
  library(rgdal)
  library(data.table)

  rm(list = ls())

  setwd("C:/Users/s436862/Dropbox/Climate matching/Data files")
  
# 1.0 Global distribution of Australia's current 335 exotic grasses ---------------
# The aim of this section is to plot the records of Australia's 335 exotic species form the downlaoded GBIF data  across a multitude of spatial scales. (Somethin glike 1-km to 300-km.)

# 1.1 Filtering master data ----------------------------------------------------------
# Here I want only the exotic species of Australia, so I will subset these out
  spp <- fread("GBIF/GBIF Poaceae 11.1.19 master.csv", header = T, quote = "") %>%
          dplyr::select(species,
                        family,
                        genus,
                        taxonRank,
                        scientificName,
                        countryCode,
                        decimalLatitude,
                        decimalLongitude,
                        basisOfRecord,
                        recordNumber)
  
  head(spp)
  
# Aus's exotic spp
  aus.exo <- read.csv("Osborne C3-C4/AVH grass pp.csv", header = T) %>%
    filter(status == "introduced")
  
  aus.exo1 <- unique(aus.exo$species)  
  
  exo <- droplevels(aus.exo1) # drop unused levels (i.e. natives)  
  exo.df <- as.data.frame(exo)
  
# filter all rows that are the 335 exotic species 
  spp2 <- filter(spp, species %in% exo) # What. A. Function.
  
  spp2.list <- sort(unique(spp2$species)) # 290 of the 335 species are contained in the GBIF data. Seems a little low.
  
# save new spp file
  write.csv(spp2, "GBIF/GBIF Poaceae v.Australias exotic species.csv", row.names = F)
  
# 1.2 Global record coverage -----------------------------------------------------
# Using Multiscale SRE DFs [Rarefaction > Rscripts > 2017] as template
  
# Data ----------------------------------------------------------------------------  
# species
  spp <- fread("GBIF/GBIF Poaceae v.Australias exotic species.csv", header = T) %>%
    dplyr::select(species, decimalLatitude, decimalLongitude)
  
# raster template
  raster <- raster("EVs/Raw EVs/arid.grd") # aridity is as good as any
  ras.ag <- aggregate(raster, fact = 200, fun = mean)
  plot(ras.ag) # took 4 minutes
               # EcoCloud took 15-20
               # CoESRA took ~10-11
               # Sad.
  
# shapefile
  wrld.shp <- readOGR("Global shapefile/TM_WORLD_BORDERS-0.3.shp")
  projection(wrld.shp) <- "+proj=utm +zone=48 +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
  
# Rasterise spp coordinates for plotting ---------------------------------
  xy <- cbind(spp$decimalLongitude, spp$decimalLatitude)
  sps <- as.numeric(factor(spp$species))
  
# number of records (n.rec)
  n.rec <- rasterize(xy, ras.ag, fun = function(x,...) {length(na.omit(x)) })
  plot(n.rec)
  
  n.rec.crop <- mask(n.rec, wrld.shp) # weird cells cropped/masked by global shapefile
  plot(log(n.rec.crop))
  
  
# species richness (sr)
  sr <- rasterize(xy, ras.ag, field = sps, fun = function(x,...) {length(unique(na.omit(x))) })  
  plot(log(sr))
  
  sr.crop <- mask(sr, wrld.shp)
  plot(log(sr.crop))
  
  

    
    