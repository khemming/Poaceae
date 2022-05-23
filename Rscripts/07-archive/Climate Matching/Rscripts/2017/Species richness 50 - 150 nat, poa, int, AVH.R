# Poa Species -----------------------------------

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

# being cheeky and short-cutting this to get cell coverage for native and introduced species
# (1) Poa
poa <- read.csv("AVH/AVH grass records.csv", header = T)
#p <-dplyr::select(poa, species, lat, long)

#xy <- cbind(p$long, p$lat)
#spp <- as.numeric(factor(p$species))

# (2) Native
poa_nat <- dplyr::filter(poa, status == "native")
xy <- cbind(poa_nat$long, poa_nat$lat)
spp <- as.numeric(factor(poa_nat$species))

# (3) Introduced
poa_int <- filter(poa, status == "introduced")
xy <- cbind(poa_int$long, poa_int$lat)
spp <- as.numeric(factor(poa_int$species))

#width <- 100

### loop function
ras <- function(width){
  
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
  df$v1 <-  df$c_doub * ((0.5 * ((df$c_sing / df$c_doub) ^ 2)) + ((df$c_sing/df$c_doub)^3) + (0.25 * ((df$c_sing/df$c_doub) ^ 4) ))
  
  # cv
  df$cv1 <- sqrt(df$v1) / df$c1 * 100
  
  # sd
  df$sd1 <- sqrt(df$v1)
  
  # CI: SD
  df$ci1 <- df$c1 + df$sd1
  df$ci2 <- df$c1 - df$sd1
  
  # percentage cover (i.e. % of grid cells with >0 records)  
  df$total <- length(df$a1)
  df$sumna <- sum(is.na(df$a1))
  df$coverage <- (df$total - df$sumna) / df$total * 100
  
  # remove lingering NAs & bind
  a_mean <- mean(df$a1, na.rm = TRUE) 
  c_mean <- mean(df$c1, na.rm = TRUE) 
  var <- mean(df$v1, na.rm = TRUE) 
  cv <- mean(df$cv1, na.rm = TRUE)
  sd <- mean(df$sd1, na.rm = TRUE)
  ci_pos <- mean(df$ci1, na.rm = TRUE)
  ci_neg <- mean(df$ci2, na.rm = TRUE)
  ratio <- a_mean / c_mean
  cover <- mean(df$coverage, na.rm = TRUE)
  cell_no <- length(b1)
  
  res <- as.data.frame(cbind(a_mean, c_mean, var, cv, sd, ci_pos, ci_neg, ratio, cover, cell_no))
  
  return(res)
  
}  #finish function

####

# scale range (1 - 250 km)
width <- 2 ^ seq(1, 8, 0.5)

out <- matrix(nrow = length(width), ncol = 11)

for(i in 1:length(width)) {
  out[i, 1] <- width[i]
  out[i, 2:11] <- as.numeric(ras(width[i]))
} 

ideal_poa <- out

colnames(ideal_poa) <- c("cell width (km)", "actual mean", "corrected mean", "variance", "coefficient of variation", "standard deviation", "confidence interval positive", "confidence interval negative", "ratio", "coverage", "number of grid cells")
ideal_poa <- as.data.frame(ideal_poa)

# csv
write.csv(ideal_poa, "C:/Users/s436862/Dropbox/Climate Matching/4. Results/Poa/Ideal scales/Introduced poa chao estimates.csv")

# for GGplot    
  ideal_poa_gg <- ideal_poa
  colnames(ideal_poa_gg) <- c("width", "a_mean", "c_mean", "var", "cv", "sd", "ci_pos", "ci_neg", "ratio", "cover", "cell_no")
ideal_poa_gg <- as.data.frame(ideal_poa_gg)

 
# coverage -- % of cells with >0 records
cover <- ggplot(data = ideal_poa_gg, aes(x = width, y = cover)) +
  geom_line(size = 0.5) +
  geom_point(size = 2) +
  #scale_y_continuous(breaks = seq(0.55, 0.8, 0.1)) +
  #scale_x_continuous(breaks = seq(50, 200, 20)) +
  labs(x = "cell width (km)",
       y = "cell coverage (%)") +
  theme(axis.title = element_text(size = 14)
  )

jpeg("C:/Users/s436862/Dropbox/Climate Matching/4. Results/Poa/Ideal scales/Introduced cell coverage.jpeg", width = 16, height = 10, units = 'cm', res = 300)
cover
dev.off()
