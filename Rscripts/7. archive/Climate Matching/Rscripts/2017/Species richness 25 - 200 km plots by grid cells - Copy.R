# Poa Species -----------------------------------

# Histogram and "survey effort" for 50, 100 & 150 km scales to see what's up


rm(list = ls())

library(ggplot2)
library(ggmap)
library(tidyr)
library(raster)
library(rgdal)
library(maptools)
library(dplyr)


setwd("C:/Users/s436862/Dropbox/Climate Matching/1. Data files/")

### data  
# raster template
b <- raster("EFs/EFs cropped/arid.grd")

# Poa
poa <- read.csv("AVH/AVH grass records.csv", header = T) %>%
  dplyr::select(species, lat, long)

p <- poa
xy <- cbind(p$long, p$lat)
spp <- as.numeric(factor(p$species))



### run this 3x, one with each width
  width <- 50
  #width <- 100
  #width <- 150

  raster <- aggregate(b, fact = width, fun = mean)
  
  # actual richness (a)
    a <- rasterize(xy, raster, field = spp, fun = function(x,...) {length(unique(na.omit(x))) })
  # number of records (n)
    n <- rasterize(xy, raster, fun = function(x,...) {length(na.omit(x)) })
  # chao (c)
    sing <- rasterize(xy, raster, field = spp, fun = function(x,...) {length(which(table(x)==1)) })
    doub <- rasterize(xy, raster, field = spp, fun = function(x,...) {length(which(table(x)==2)) })
  # reference for extraterrestrial NA removal
    b1 <- getValues(raster)
  # actual richness
    a1 <- getValues(a)
  # chao correctives
    c_sing <- getValues(sing)
    c_doub <- getValues(doub)
    n1 <- getValues(n)
  # dataframe
    df_na <- data.frame(a1, b1, c_sing, c_doub, n1)
  df <- df_na[!is.na(b1), ] 
  # doubleton correction 
    df$c1 <- ifelse(is.na(df$c_doub) == FALSE,
                  (ifelse(df$c_doub == 0, 
                          df$a1 + ((df$c_sing * (df$c_sing - 1)) / (2 * (df$c_doub + 1))),   
                          df$a1 + ((df$c_sing ^ 2) / (2 * df$c_doub)))),
                  NA)
  
### Anything and everything else
  # variance (v)
    df$v1 <-  df$c_doub * ((0.5 * ((df$c_sing / df$c_doub) ^ 2)) + ((df$c_sing/df$c_doub) ^ 3) + (0.25 * ((df$c_sing/df$c_doub) ^ 4) ))
  # SD
    df$sd1 <- sqrt(df$v1)
    
  # CI (Chao, 1987)
    df$ci1 <- df$c1 + (df$sd1 * 1.96)
    df$ci2 <- df$c1 - (df$sd1 * 1.96)
##############  
 
# ran the above and did each scale in step (didn't take long)
  scale50 <- na.exclude(df$c1)  
  ci_pos50 <- na.exclude(df$ci1) 
  ci_neg50 <- na.exclude(df$ci2)
  survey50 <- scale50/50
  
  scale100 <- na.exclude(df$c1)  
  ci_pos100 <- na.exclude(df$ci1) 
  ci_neg100 <- na.exclude(df$ci2)
  survey100 <- scale100/100  
  
  scale150 <- na.exclude(df$c1)  
  ci_pos150 <- na.exclude(df$ci1) 
  ci_neg150 <- na.exclude(df$ci2)
  survey150 <- scale150/150  
  
  scale <- as.matrix(scale50, ci_pos50, ci_neg50, survey50,
                     scale100, ci_pos100, ci_neg100, survey100,
                     scale150, ci_pos150, ci_neg150, survey150
                     )
  
  write.csv(scale, "C:/Users/s436862/Dropbox/Climate Matching/4. Results/Poa/Ideal scales/histograms and survey effort.csv")
  
  
# histogram of the corrected data  
  histo <- ggplot(data = scale, aes(x = scale50)) +
    geom_col(size = 0.5) +
    geom_point(size = 2) +
    #scale_y_continuous(breaks = seq(0.55, 0.8, 0.1)) +
    #scale_x_continuous(breaks = seq(50, 200, 20)) +
    labs(x = "cell width (km)",
         y = "number of grid cells") +
    theme(axis.title = element_text(size = 14)
    )

jpeg("C:/Users/s436862/Dropbox/Climate Matching/4. Results/Poa/Ideal scales/number of grid cells.jpeg", width = 16, height = 10, units = 'cm', res = 300)
histo
dev.off()

