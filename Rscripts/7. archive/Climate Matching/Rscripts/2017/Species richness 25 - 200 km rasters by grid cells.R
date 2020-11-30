# Want a dataframe off Poa, Nat, Int & All_AVH richness over a variety of spatial scales
# scales of:
# 25, 50, 75, 100, 125, 150, 175, 200 km cell widths

# long format (so 50, 75 etc. are categorical labels for every grid cell value for each of Poa, Nat, etc.)


  rm(list = ls())
  
  library(ggplot2) 
  library(rasterVis) # gplot function
  library(ggmap)
  library(ggthemes)
  library(gplots)
  library(tidyr)
  library(raster)
  library(rgdal)
  library(maptools)
  library(dplyr)
  
  setwd("C:/Users/s436862/Dropbox/Climate Matching/1. Data files")
  
  
  raster1 <- raster("EFs/EFs cropped/arid.grd") 
  raster <- aggregate(raster1, fact = 100, fun = mean)
  oz <- borders("world", region = "Australia")
  
##### Poa species richness
  poa <- read.table("AVH/AVH grass records.txt", header = T, sep = "\t")
  
# Native
  nat <- filter(poa, status == "native")
  xy <- cbind(nat$long, nat$lat)
  spp <- as.numeric(factor(nat$species))
# Richness
  native <- rasterize(xy, raster, field = spp, fun = function(x,...) {length(unique(na.omit(x))) })
  plot(native)
  
  
# Introduced
  int <- filter(poa, status == "introduced")
  xy <- cbind(int$long, int$lat)
  spp <- as.numeric(factor(int$species))
  # Richness
  introduced <- rasterize(xy, raster, field = spp, fun = function(x,...) {length(unique(na.omit(x))) })
  plot(introduced)               

  
  ratio <- (native) / (native + introduced)

  
# GGplot time  
# native
  q <- gplot(richness) + 
       theme_classic()+
       geom_raster(aes(fill = value)) +
       facet_wrap(~ variable) +
       scale_fill_gradient(low = "white",
                           high = "red",
                           na.value="grey") +
       coord_equal()
                   
  p <- q + guides(fill=guide_legend(title="richness")) +
           xlab("Lat") + 
           ylab("Long") +
           oz + 
           xlim(110, 160) +
           ylim(-45, -10)
                   
  print(p)
                   
  ggsave("C:/Users/s436862/Dropbox/Climate Matching/4. Results/Poa/Native richness @ 100 km.jpeg", plot = last_plot(), scale = 1, device = "jpeg")
                   
