### Actual/corrected species richness code

###########################################

 
  
  library(ggplot2)
  library(ggmap)
  library(tidyr)
  library(raster)
  library(rgdal)
  library(maptools)
  library(dplyr)
  
# note: wd change  
  setwd("C:\\Users\\s429217\\Dropbox\\Aus pop and aridity\\corrected_actual_poa_spp_rich_ratio")
  
#### data  
  # raster template
  b <- raster("arid.grd")
  
# Poa
  poa <- read.csv("AVH native records.csv", header = T)
  poa <- dplyr::select(poa, Species...matched, Latitude...processed, Longitude...processed)
  names(poa) <- c("species", "lat", "long")
  
  p <- poa
  xy <- cbind(p$long, p$lat)
  spp <- as.numeric(factor(p$species))
  

#### loop      
  ras <- function(width){
    
    raster <- aggregate(b, fact = width, fun = mean)
    
    # actual richness (a)
    a <- rasterize(xy, raster, field = spp, fun = function(x,...) {length(unique(na.omit(x))) })
    
    # chao (c)
    n_spp <- rasterize(xy, raster, field = spp, fun = function(x,...) {length(unique(na.omit(x))) })
    sing <- rasterize(xy, raster, field = spp, fun = function(x,...) {length(which(table(x)==1)) })
    doub <- rasterize(xy, raster, field = spp, fun = function(x,...) {length(which(table(x)==2)) })
    # bias corrected; use when no doubletons
    c <- n_spp + ((sing * (sing - 1)) / (2 * (doub + 1)))
    
    # df
    b1 <- getValues(raster)
    a1 <- getValues(a)
    c1 <- getValues(c)
    # discriminate between NA & zero values
    df_na <- data.frame(a1, b1, c1)
    df <- df_na[!is.na(b1), ] 
    
    # generate ratio of a/c ("completeness index")
    a_mean <- mean(df$a1, na.rm = TRUE)
    a_variance <- var(df$a1, na.rm = TRUE)
    c_mean <- mean(df$c1, na.rm = TRUE)
    c_variance <- var(df$c1, na.rm = TRUE)
    
    res <- cbind(a_mean, a_variance, c_mean, c_variance)
    colnames(res) <- c("a_mean", "a_variance", "c_mean", "c_variance")
    rownames(res) <- "raster"
    res <- as.data.frame(res)
    
    res <- mutate(res, ratio = a_mean/c_mean)
    
    
    return(res)
  }  #finish function

####
  
  test_width <- 2 ^ (seq(1, 10, 0.5))
  out <- matrix(nrow = length(test_width), ncol = 6)
  
  for(i in 1:length(test_width)) {
    out[i, 1] <- test_width[i]
    out[i, 2:6] <- as.numeric(ras(test_width[i]))
  }
  
  plot(out[, 6] ~ out[, 1])
  plot(out[, 6] ~ log10(out[, 1]))

