

# AVH raster script: codes and plots
# rasters of a, n, Chao1, jost C @ 50, 100 and 200 km

# Note: how to make the raster legends the same?
  rm(list = ls())
  
  library(raster)
  library(ggmap)
  library(tidyr)
  library(rgdal)
  library(maptools)
  library(ggplot2)
  library(dplyr)
  library(rasterVis)
  
  setwd("C:/Users/s436862/Dropbox/Climate Matching/1. Data files")

# 50 km-------------------------------------------------
# data ------------------------------------------------
  rm(list = ls())
  
  avh <- read.csv("AVH/AVH native records.csv", header = T) %>%
    select(species, lat, long)
  
  xy <- cbind(avh$long, avh$lat)
  spp <- as.numeric(factor(avh$species))
  
# raster template & Aus outline (shapefile)
  b <- raster("EFs/EFs cropped/arid.grd")
  oz <- borders("world", region = "Australia")
  
# scale    
  width <- 50
  
# Chao1 and baseline [actual] SRE rasters & functions-------------------------------------------------
# aggregate original raster
  raster <- aggregate(b, fact = width, fun = mean)
  
# actual richness (a)
  a <- rasterize(xy, raster, field = spp, fun = function(x,...) {length(unique(na.omit(x))) })
  
# number of records (n)
  n <- rasterize(xy, raster, fun = function(x,...) {length(na.omit(x)) })
  
# singletons etc.; going to refer these as f1, f2, f3 and f4 
  f1 <- rasterize(xy, raster, field = spp, fun = function(x,...) {length(which(table(x)==1)) })
  f2 <- rasterize(xy, raster, field = spp, fun = function(x,...) {length(which(table(x)==2)) })
  f3 <- rasterize(xy, raster, field = spp, fun = function(x,...) {length(which(table(x)==3)) })
  f4 <- rasterize(xy, raster, field = spp, fun = function(x,...) {length(which(table(x)==4)) })
  chao1 <- stack(a, n, f1, f2, f3, f4)
  
# overlay is the key function for raster maths here
  
# Chao1 function  
# this is bias-correcting for when no f2's  
  chao1_sre <- function(x) { overlay(a, n, f1, f2, fun = function(a, n, f1, f2) {
  ifelse(f2 == 0, 
         (a + (((n-1) / n) * (f1 * (f1-1)) / (2 * (f2+1)))),
         (a + (((n-1) / n) * ((f1^2)/(2*f2))))
    )
    } # finish function
  )
  } # finish function
  
  chao1 <- chao1_sre(ichao1)
  plot(chao1)
  # worked :) 
# actual/Chao1_C
  actual_C <- a/chao1
  plot(actual_C)
  
# Chao + Jost Coverage (jost_C) 
# includes f2 bias-correction
  jost_C <- function(x) { overlay(a, n, f1, f2, fun = function(a, n, f1, f2) { ifelse(f2 > 0,
                        1 - ((f1/n) * (((n-1)*f1) / (((n-1)*f1) + 2*f2))), 
                        1 - ((f1/n) * ((n-1)*(f1-1) / (((n-1)*(f1-1)) + 2))) )
  } # finish function
  )
  } # finish function
  
  jost_C <- jost_C(ichao1)
  plot(jost_C)
  
# save: actual (a), chao1, and Jost C
  writeRaster(a, "C:/Users/s436862/Dropbox/Climate Matching/4. Results/AVH/50 km scale/baseline_SR_50km.grd", prj = T, overwrite = T)
  writeRaster(chao1, "C:/Users/s436862/Dropbox/Climate Matching/4. Results/AVH/50 km scale/chao1_50km.grd", prj = T, overwrite = T)
  writeRaster(actual_C, "C:/Users/s436862/Dropbox/Climate Matching/4. Results/AVH/50 km scale/actual_C_50km.grd", prj = T, overwrite = T)
  writeRaster(jost_C, "C:/Users/s436862/Dropbox/Climate Matching/4. Results/AVH/50 km scale/jost_C_50km.grd", prj = T, overwrite = T)
  
# iChao1 equations. Note: not using this measure anympore, but keeping equations b/c pride ------------    
  # (a) chao1 when no doubletons   
  chao1_no_f2 <- function(x) { overlay(a, n, f1, f2, fun = function(a, n, f1, f2) {
    (a + (((n-1) / n) * (f1 * (f1-1)) / (2 * (f2+1))))
  } # finish function
  )
  } # finish function
  
  chao_nd <- chao1_no_f2(ichao1)
  plot(chao_nd)
  # worked :) 
  
  
  # (b) chao1 with doubltons
  chao1_w_f2 <- function(x) { overlay(a, n, f1, f2, fun = function(a, n, f1, f2) {
    a + (((n-1) / n) * ((f1^2)/(2*f2)))
  } # finish function  
  )
  } # finish function
  
  chao_yd <- chao1_w_f2(ichao1)
  plot(chao_yd)
  # worked :)
  
  # (c) ichao1 when no quadrupletons (f4's)
  ichao1_no_f4 <- function(x) { overlay(a, n, f1, f2, f3, f4, fun = function(a, n, f1, f2, f3, f4) {
    ((a + (((n-1) / n) * ((f1^2)/(2*f2)))) + 
       ((((n-3) / (4*n)) * (f3/(f4+1))) *
          (ifelse( (f1 - ((n-3) / (2*(n-1))) * (f2*f3/(f4+1))) > 0, 
                   (f1 - ((n-3) / (2*(n-1))) * (f2*f3/(f4+1))), 0 ))))
  } # finish function
  )
  } # finish function
  
  chao_nq <- ichao1_no_f4(ichao1)
  plot(chao_nq)
  # worked :)
  
  # (d) ichao1 with quadrupletons (f4's)
  ichao1_w_f4 <- function(x) { overlay(a, n, f1, f2, f3, f4, fun = function(a, n, f1, f2, f3, f4) {
    ((a + (((n-1) / n) * ((f1^2)/(2*f2)))) + 
       ((((n-3) / (4*n)) * (f3/f4)) *
          (ifelse( (f1 - ((n-3) / (2*(n-1))) * (f2*f3/f4)) > 0, 
                   (f1 - ((n-3) / (2*(n-1))) * (f2*f3/f4)), 0 ))))
    
  } # finish function
  )
  } # finish function
  
  chao_yq <- ichao1_w_f4(ichao1)
  plot(chao_yq)
  # worked :)
  
  # (a-d) complete ichao1 estimation -- this sohlud work, we can build off this below script
  # alright, this was hard -- the chao1 ifelse worked, but not when I added the ifelse for the other scripts
  # ok, alst ditch effort: doing it step-wise, and then I am giving up. Cool.  
  
  ichao1_sre_fn <- function(x) { overlay(a, n, f1, f2, f3, f4, fun = function(a, n, f1, f2, f3, f4) {
    ifelse(f2 == 0, 
           (a + (((n-1) / n) * (f1 * (f1-1)) / (2 * (f2+1)))), # no dbltons
           ifelse(f3 == 0, 
                  (a + (((n-1) / n) * ((f1^2)/(2*f2)))), # yes dobltons
                  ifelse(f4 == 0, 
                         ((a + (((n-1) / n) * ((f1^2)/(2*f2)))) + 
                            ((((n-3) / (4*n)) * (f3/(f4+1))) *
                               (ifelse( (f1 - ((n-3) / (2*(n-1))) * (f2*f3/(f4+1))) > 0, 
                                        (f1 - ((n-3) / (2*(n-1))) * (f2*f3/(f4+1))), 0 )))), # no quads (this line and the three above it)
                         
                         ((a + (((n-1) / n) * ((f1^2)/(2*f2)))) + 
                            ((((n-3) / (4*n)) * (f3/f4)) *
                               (ifelse( (f1 - ((n-3) / (2*(n-1))) * (f2*f3/f4)) > 0, 
                                        (f1 - ((n-3) / (2*(n-1))) * (f2*f3/f4)), 0 )))) # yes quads ("               ")
                  ))
    )
  } # finish function
  )
  } # finish function
  
  ichao1_raster <- ichao1_sre_fn(ichao1)
  plot(ichao1_raster)
  # wooooooooooooooooooooooooooooooooooooooooooooooooooooooow
  
  
  # save: actual (a), ichao1 (ichao1), and just the OG chao1
  writeRaster(a, "C:/Users/s436862/Dropbox/Climate Matching/4. Results/AVH/50 km scale/baseline_SR_50km.grd", prj = T, overwrite = T)
  writeRaster(ichao1_raster, "C:/Users/s436862/Dropbox/Climate Matching/4. Results/AVH/50 km scale/ichao1_50km.grd", prj = T, overwrite = T)
  writeRaster(chao1, "C:/Users/s436862/Dropbox/Climate Matching/4. Results/AVH/50 km scale/chao1_50km.grd", prj = T, overwrite = T)
  
  
  
# Plot Chao1  ----------------------------------------------------------
  chao1_ras <- raster("C:/Users/s436862/Dropbox/Climate Matching/4. Results/AVH/50 km scale/chao1_50km.grd")
  
  q <- gplot(chao1_ras) + 
    theme_classic()+
    geom_raster(aes(fill = value)) +
    scale_fill_gradient(low = 'white',
                        high = 'red',
                        na.value="grey") +
    coord_equal()
  
  p <- q + guides(fill=guide_legend(title="Chao1 SR")) +
    xlab("Long") + 
    ylab("Lat") +
    oz + 
    xlim(110, 160) +
    ylim(-50, -5)
  
  print(p)
  
  # save: chao1
  ggsave("C:/Users/s436862/Dropbox/Climate Matching/4. Results/AVH/50 km scale/50km chao1 SR.jpeg", plot = last_plot(), scale = 1, dpi = 300, device = "jpeg")
  
# Plot actual (henceforth 'baseline') ----------------------------------------------------------  
  baseline_ras <- raster("C:/Users/s436862/Dropbox/Climate Matching/4. Results/AVH/50 km scale/baseline_SR_50km.grd")
  
  q <- gplot(baseline_ras) + 
    theme_classic()+
    geom_raster(aes(fill = value)) +
    scale_fill_gradient(low = 'white',
                        high = 'red',
                        na.value="grey") +
    coord_equal()
  
  p <- q + guides(fill=guide_legend(title="Baseline (AVH) SR")) +
    xlab("Long") + 
    ylab("Lat") +
    oz + 
    xlim(110, 160) +
    ylim(-50, -5)
  
  print(p)
  
  # save: baseline
  ggsave("C:/Users/s436862/Dropbox/Climate Matching/4. Results/AVH/50 km scale/50km baseline SR.jpeg", plot = last_plot(), scale = 1, dpi = 300, device = "jpeg")
  
  
# Plot Jost_C ----------------------------------------------------------  
  jost_C <- raster("C:/Users/s436862/Dropbox/Climate Matching/4. Results/AVH/50 km scale/jost_C_50km.grd")
  
  q <- gplot(jost_C) + 
    theme_classic()+
    geom_raster(aes(fill = value)) +
    scale_fill_gradient(low = 'white',
                        high = 'red',
                        na.value="grey") +
    coord_equal()
  
  p <- q + guides(fill=guide_legend(title="Jost Coverage (C)")) +
    xlab("Long") + 
    ylab("Lat") +
    oz + 
    xlim(110, 160) +
    ylim(-50, -5)
  
  print(p)
  
  # save: ratio
  ggsave("C:/Users/s436862/Dropbox/Climate Matching/4. Results/AVH/50 km scale/50 km Jost coverage.jpeg", plot = last_plot(), scale = 1, dpi = 300, device = "jpeg")
  
 
  
  
# Plot actual/Chao1_C ----------------------------------------------------------  
  actual_C <- raster("C:/Users/s436862/Dropbox/Climate Matching/4. Results/AVH/50 km scale/actual_C_50km.grd")
  
  q <- gplot(actual_C) + 
    theme_classic()+
    geom_raster(aes(fill = value)) +
    scale_fill_gradient(low = 'white',
                        high = 'red',
                        na.value="grey") +
    coord_equal()
  
  p <- q + guides(fill=guide_legend(title="baseline coverage")) +
    xlab("Long") + 
    ylab("Lat") +
    oz + 
    xlim(110, 160) +
    ylim(-50, -5)
  
  print(p)
  
  # save: ratio
  ggsave("C:/Users/s436862/Dropbox/Climate Matching/4. Results/AVH/50 km scale/50 km baseline coverage.jpeg", plot = last_plot(), scale = 1, dpi = 300, device = "jpeg")
  
  
  
  
# -------------------------------------------------  
  
  
# 100 km-------------------------------------------------
# data ------------------------------------------------
  rm(list = ls())
  
  avh <- read.csv("AVH/AVH native records.csv", header = T) %>%
    select(species, lat, long)
  
  xy <- cbind(avh$long, avh$lat)
  spp <- as.numeric(factor(avh$species))
  
  # raster template & Aus outline (shapefile)
  b <- raster("EFs/EFs cropped/arid.grd")
  oz <- borders("world", region = "Australia")
  
  # scale    
  width <- 100
  
  # Chao1 and baseline [actual] SRE rasters & functions-------------------------------------------------
  # aggregate original raster
  raster <- aggregate(b, fact = width, fun = mean)
  
  # actual richness (a)
  a <- rasterize(xy, raster, field = spp, fun = function(x,...) {length(unique(na.omit(x))) })
  
  # number of records (n)
  n <- rasterize(xy, raster, fun = function(x,...) {length(na.omit(x)) })
  
  # singletons etc.; going to refer these as f1, f2, f3 and f4 
  f1 <- rasterize(xy, raster, field = spp, fun = function(x,...) {length(which(table(x)==1)) })
  f2 <- rasterize(xy, raster, field = spp, fun = function(x,...) {length(which(table(x)==2)) })
  f3 <- rasterize(xy, raster, field = spp, fun = function(x,...) {length(which(table(x)==3)) })
  f4 <- rasterize(xy, raster, field = spp, fun = function(x,...) {length(which(table(x)==4)) })
  chao1 <- stack(a, n, f1, f2, f3, f4)
  
  # overlay is the key function for raster maths here
  
  # Chao1 function  
  # this is bias-correcting for when no f2's  
  chao1_sre <- function(x) { overlay(a, n, f1, f2, fun = function(a, n, f1, f2) {
    ifelse(f2 == 0, 
           (a + (((n-1) / n) * (f1 * (f1-1)) / (2 * (f2+1)))),
           (a + (((n-1) / n) * ((f1^2)/(2*f2))))
    )
  } # finish function
  )
  } # finish function
  
  chao1 <- chao1_sre(ichao1)
  plot(chao1)
  # worked :) 
  # actual/Chao1_C
  actual_C <- a/chao1
  plot(actual_C)
  
  # Chao + Jost Coverage (jost_C) 
  # includes f2 bias-correction
  jost_C <- function(x) { overlay(a, n, f1, f2, fun = function(a, n, f1, f2) { ifelse(f2 > 0,
                                                                                      1 - ((f1/n) * (((n-1)*f1) / (((n-1)*f1) + 2*f2))), 
                                                                                      1 - ((f1/n) * ((n-1)*(f1-1) / (((n-1)*(f1-1)) + 2))) )
  } # finish function
  )
  } # finish function
  
  jost_C <- jost_C(ichao1)
  plot(jost_C)
  
  # save: actual (a), chao1, and Jost C
  writeRaster(a, "C:/Users/s436862/Dropbox/Climate Matching/4. Results/AVH/100 km scale/baseline_SR_100km.grd", prj = T, overwrite = T)
  writeRaster(chao1, "C:/Users/s436862/Dropbox/Climate Matching/4. Results/AVH/100 km scale/chao1_100km.grd", prj = T, overwrite = T)
  writeRaster(actual_C, "C:/Users/s436862/Dropbox/Climate Matching/4. Results/AVH/100 km scale/actual_C_100km.grd", prj = T, overwrite = T)
  writeRaster(jost_C, "C:/Users/s436862/Dropbox/Climate Matching/4. Results/AVH/100 km scale/jost_C_100km.grd", prj = T, overwrite = T)
  
  # iChao1 equations. Note: not using this measure anympore, but keeping equations b/c pride ------------    
  # (a) chao1 when no doubletons   
  chao1_no_f2 <- function(x) { overlay(a, n, f1, f2, fun = function(a, n, f1, f2) {
    (a + (((n-1) / n) * (f1 * (f1-1)) / (2 * (f2+1))))
  } # finish function
  )
  } # finish function
  
  chao_nd <- chao1_no_f2(ichao1)
  plot(chao_nd)
  # worked :) 
  
  
  # (b) chao1 with doubltons
  chao1_w_f2 <- function(x) { overlay(a, n, f1, f2, fun = function(a, n, f1, f2) {
    a + (((n-1) / n) * ((f1^2)/(2*f2)))
  } # finish function  
  )
  } # finish function
  
  chao_yd <- chao1_w_f2(ichao1)
  plot(chao_yd)
  # worked :)
  
  # (c) ichao1 when no quadrupletons (f4's)
  ichao1_no_f4 <- function(x) { overlay(a, n, f1, f2, f3, f4, fun = function(a, n, f1, f2, f3, f4) {
    ((a + (((n-1) / n) * ((f1^2)/(2*f2)))) + 
       ((((n-3) / (4*n)) * (f3/(f4+1))) *
          (ifelse( (f1 - ((n-3) / (2*(n-1))) * (f2*f3/(f4+1))) > 0, 
                   (f1 - ((n-3) / (2*(n-1))) * (f2*f3/(f4+1))), 0 ))))
  } # finish function
  )
  } # finish function
  
  chao_nq <- ichao1_no_f4(ichao1)
  plot(chao_nq)
  # worked :)
  
  # (d) ichao1 with quadrupletons (f4's)
  ichao1_w_f4 <- function(x) { overlay(a, n, f1, f2, f3, f4, fun = function(a, n, f1, f2, f3, f4) {
    ((a + (((n-1) / n) * ((f1^2)/(2*f2)))) + 
       ((((n-3) / (4*n)) * (f3/f4)) *
          (ifelse( (f1 - ((n-3) / (2*(n-1))) * (f2*f3/f4)) > 0, 
                   (f1 - ((n-3) / (2*(n-1))) * (f2*f3/f4)), 0 ))))
    
  } # finish function
  )
  } # finish function
  
  chao_yq <- ichao1_w_f4(ichao1)
  plot(chao_yq)
  # worked :)
  
  # (a-d) complete ichao1 estimation -- this sohlud work, we can build off this below script
  # alright, this was hard -- the chao1 ifelse worked, but not when I added the ifelse for the other scripts
  # ok, alst ditch effort: doing it step-wise, and then I am giving up. Cool.  
  
  ichao1_sre_fn <- function(x) { overlay(a, n, f1, f2, f3, f4, fun = function(a, n, f1, f2, f3, f4) {
    ifelse(f2 == 0, 
           (a + (((n-1) / n) * (f1 * (f1-1)) / (2 * (f2+1)))), # no dbltons
           ifelse(f3 == 0, 
                  (a + (((n-1) / n) * ((f1^2)/(2*f2)))), # yes dobltons
                  ifelse(f4 == 0, 
                         ((a + (((n-1) / n) * ((f1^2)/(2*f2)))) + 
                            ((((n-3) / (4*n)) * (f3/(f4+1))) *
                               (ifelse( (f1 - ((n-3) / (2*(n-1))) * (f2*f3/(f4+1))) > 0, 
                                        (f1 - ((n-3) / (2*(n-1))) * (f2*f3/(f4+1))), 0 )))), # no quads (this line and the three above it)
                         
                         ((a + (((n-1) / n) * ((f1^2)/(2*f2)))) + 
                            ((((n-3) / (4*n)) * (f3/f4)) *
                               (ifelse( (f1 - ((n-3) / (2*(n-1))) * (f2*f3/f4)) > 0, 
                                        (f1 - ((n-3) / (2*(n-1))) * (f2*f3/f4)), 0 )))) # yes quads ("               ")
                  ))
    )
  } # finish function
  )
  } # finish function
  
  ichao1_raster <- ichao1_sre_fn(ichao1)
  plot(ichao1_raster)
  # wooooooooooooooooooooooooooooooooooooooooooooooooooooooow
  
  
  # save: actual (a), ichao1 (ichao1), and just the OG chao1
  writeRaster(a, "C:/Users/s436862/Dropbox/Climate Matching/4. Results/AVH/100 km scale/baseline_SR_100km.grd", prj = T, overwrite = T)
  writeRaster(ichao1_raster, "C:/Users/s436862/Dropbox/Climate Matching/4. Results/AVH/100 km scale/ichao1_100km.grd", prj = T, overwrite = T)
  writeRaster(chao1, "C:/Users/s436862/Dropbox/Climate Matching/4. Results/AVH/100 km scale/chao1_100km.grd", prj = T, overwrite = T)
  
  
  
  # Plot Chao1  ----------------------------------------------------------
  chao1_ras <- raster("C:/Users/s436862/Dropbox/Climate Matching/4. Results/AVH/100 km scale/chao1_100km.grd")
  
  q <- gplot(chao1_ras) + 
    theme_classic()+
    geom_raster(aes(fill = value)) +
    scale_fill_gradient(low = 'white',
                        high = 'red',
                        na.value="grey") +
    coord_equal()
  
  p <- q + guides(fill=guide_legend(title="Chao1 SR")) +
    xlab("Long") + 
    ylab("Lat") +
    oz + 
    xlim(110, 160) +
    ylim(-50, -5)
  
  print(p)
  
  # save: chao1
  ggsave("C:/Users/s436862/Dropbox/Climate Matching/4. Results/AVH/100 km scale/100km chao1 SR.jpeg", plot = last_plot(), scale = 1, dpi = 300, device = "jpeg")
  
  # Plot actual (henceforth 'baseline') ----------------------------------------------------------  
  baseline_ras <- raster("C:/Users/s436862/Dropbox/Climate Matching/4. Results/AVH/100 km scale/baseline_SR_100km.grd")
  
  q <- gplot(baseline_ras) + 
    theme_classic()+
    geom_raster(aes(fill = value)) +
    scale_fill_gradient(low = 'white',
                        high = 'red',
                        na.value="grey") +
    coord_equal()
  
  p <- q + guides(fill=guide_legend(title="Baseline (AVH) SR")) +
    xlab("Long") + 
    ylab("Lat") +
    oz + 
    xlim(110, 160) +
    ylim(-50, -5)
  
  print(p)
  
  # save: baseline
  ggsave("C:/Users/s436862/Dropbox/Climate Matching/4. Results/AVH/100 km scale/100km baseline SR.jpeg", plot = last_plot(), scale = 1, dpi = 300, device = "jpeg")
  
  
  # Plot Jost_C ----------------------------------------------------------  
  jost_C <- raster("C:/Users/s436862/Dropbox/Climate Matching/4. Results/AVH/100 km scale/jost_C_100km.grd")
  
  q <- gplot(jost_C) + 
    theme_classic()+
    geom_raster(aes(fill = value)) +
    scale_fill_gradient(low = 'white',
                        high = 'red',
                        na.value="grey") +
    coord_equal()
  
  p <- q + guides(fill=guide_legend(title="Jost Coverage (C)")) +
    xlab("Long") + 
    ylab("Lat") +
    oz + 
    xlim(110, 160) +
    ylim(-50, -5)
  
  print(p)
  
  # save: ratio
  ggsave("C:/Users/s436862/Dropbox/Climate Matching/4. Results/AVH/100 km scale/100 km Jost coverage.jpeg", plot = last_plot(), scale = 1, dpi = 300, device = "jpeg")
  
  
  
  
  # Plot actual/Chao1_C ----------------------------------------------------------  
  actual_C <- raster("C:/Users/s436862/Dropbox/Climate Matching/4. Results/AVH/100 km scale/actual_C_100km.grd")
  
  q <- gplot(actual_C) + 
    theme_classic()+
    geom_raster(aes(fill = value)) +
    scale_fill_gradient(low = 'white',
                        high = 'red',
                        na.value="grey") +
    coord_equal()
  
  p <- q + guides(fill=guide_legend(title="baseline coverage")) +
    xlab("Long") + 
    ylab("Lat") +
    oz + 
    xlim(110, 160) +
    ylim(-50, -5)
  
  print(p)
  
  # save: ratio
  ggsave("C:/Users/s436862/Dropbox/Climate Matching/4. Results/AVH/100 km scale/100 km baseline coverage.jpeg", plot = last_plot(), scale = 1, dpi = 300, device = "jpeg")
  
  
  
  
# -------------------------------------------------  
  
  
# 200 km-------------------------------------------------
# data ------------------------------------------------
  rm(list = ls())
  
  avh <- read.csv("AVH/AVH native records.csv", header = T) %>%
    select(species, lat, long)
  
  xy <- cbind(avh$long, avh$lat)
  spp <- as.numeric(factor(avh$species))
  
  # raster template & Aus outline (shapefile)
  b <- raster("EFs/EFs cropped/arid.grd")
  oz <- borders("world", region = "Australia")
  
  # scale    
  width <- 200
  
# Chao1 and baseline [actual] SRE rasters & functions-------------------------------------------------
  # aggregate original raster
  raster <- aggregate(b, fact = width, fun = mean)
  
  # actual richness (a)
  a <- rasterize(xy, raster, field = spp, fun = function(x,...) {length(unique(na.omit(x))) })
  
  # number of records (n)
  n <- rasterize(xy, raster, fun = function(x,...) {length(na.omit(x)) })
  
  # singletons etc.; going to refer these as f1, f2, f3 and f4 
  f1 <- rasterize(xy, raster, field = spp, fun = function(x,...) {length(which(table(x)==1)) })
  f2 <- rasterize(xy, raster, field = spp, fun = function(x,...) {length(which(table(x)==2)) })
  f3 <- rasterize(xy, raster, field = spp, fun = function(x,...) {length(which(table(x)==3)) })
  f4 <- rasterize(xy, raster, field = spp, fun = function(x,...) {length(which(table(x)==4)) })
  chao1 <- stack(a, n, f1, f2, f3, f4)
  
  # overlay is the key function for raster maths here
  
  # Chao1 function  
  # this is bias-correcting for when no f2's  
  chao1_sre <- function(x) { overlay(a, n, f1, f2, fun = function(a, n, f1, f2) {
    ifelse(f2 == 0, 
           (a + (((n-1) / n) * (f1 * (f1-1)) / (2 * (f2+1)))),
           (a + (((n-1) / n) * ((f1^2)/(2*f2))))
    )
  } # finish function
  )
  } # finish function
  
  chao1 <- chao1_sre(ichao1)
  plot(chao1)
  # worked :) 
  # actual/Chao1_C
  actual_C <- a/chao1
  plot(actual_C)
  
  # Chao + Jost Coverage (jost_C) 
  # includes f2 bias-correction
  jost_C <- function(x) { overlay(a, n, f1, f2, fun = function(a, n, f1, f2) { ifelse(f2 > 0,
                                                                                      1 - ((f1/n) * (((n-1)*f1) / (((n-1)*f1) + 2*f2))), 
                                                                                      1 - ((f1/n) * ((n-1)*(f1-1) / (((n-1)*(f1-1)) + 2))) )
  } # finish function
  )
  } # finish function
  
  jost_C <- jost_C(ichao1)
  plot(jost_C)
  
  # save: actual (a), chao1, and Jost C
  writeRaster(a, "C:/Users/s436862/Dropbox/Climate Matching/4. Results/AVH/200 km scale/baseline_SR_200km.grd", prj = T, overwrite = T)
  writeRaster(chao1, "C:/Users/s436862/Dropbox/Climate Matching/4. Results/AVH/200 km scale/chao1_200km.grd", prj = T, overwrite = T)
  writeRaster(actual_C, "C:/Users/s436862/Dropbox/Climate Matching/4. Results/AVH/200 km scale/actual_C_200km.grd", prj = T, overwrite = T)
  writeRaster(jost_C, "C:/Users/s436862/Dropbox/Climate Matching/4. Results/AVH/200 km scale/jost_C_200km.grd", prj = T, overwrite = T)
  
# iChao1 equations. Note: not using this measure anympore, but keeping equations b/c pride ------------    
# (a) chao1 when no doubletons   
  chao1_no_f2 <- function(x) { overlay(a, n, f1, f2, fun = function(a, n, f1, f2) {
    (a + (((n-1) / n) * (f1 * (f1-1)) / (2 * (f2+1))))
  } # finish function
  )
  } # finish function
  
  chao_nd <- chao1_no_f2(ichao1)
  plot(chao_nd)
  # worked :) 
  
  
  # (b) chao1 with doubltons
  chao1_w_f2 <- function(x) { overlay(a, n, f1, f2, fun = function(a, n, f1, f2) {
    a + (((n-1) / n) * ((f1^2)/(2*f2)))
  } # finish function  
  )
  } # finish function
  
  chao_yd <- chao1_w_f2(ichao1)
  plot(chao_yd)
  # worked :)
  
  # (c) ichao1 when no quadrupletons (f4's)
  ichao1_no_f4 <- function(x) { overlay(a, n, f1, f2, f3, f4, fun = function(a, n, f1, f2, f3, f4) {
    ((a + (((n-1) / n) * ((f1^2)/(2*f2)))) + 
       ((((n-3) / (4*n)) * (f3/(f4+1))) *
          (ifelse( (f1 - ((n-3) / (2*(n-1))) * (f2*f3/(f4+1))) > 0, 
                   (f1 - ((n-3) / (2*(n-1))) * (f2*f3/(f4+1))), 0 ))))
  } # finish function
  )
  } # finish function
  
  chao_nq <- ichao1_no_f4(ichao1)
  plot(chao_nq)
  # worked :)
  
  # (d) ichao1 with quadrupletons (f4's)
  ichao1_w_f4 <- function(x) { overlay(a, n, f1, f2, f3, f4, fun = function(a, n, f1, f2, f3, f4) {
    ((a + (((n-1) / n) * ((f1^2)/(2*f2)))) + 
       ((((n-3) / (4*n)) * (f3/f4)) *
          (ifelse( (f1 - ((n-3) / (2*(n-1))) * (f2*f3/f4)) > 0, 
                   (f1 - ((n-3) / (2*(n-1))) * (f2*f3/f4)), 0 ))))
    
  } # finish function
  )
  } # finish function
  
  chao_yq <- ichao1_w_f4(ichao1)
  plot(chao_yq)
  # worked :)
  
  # (a-d) complete ichao1 estimation -- this sohlud work, we can build off this below script
  # alright, this was hard -- the chao1 ifelse worked, but not when I added the ifelse for the other scripts
  # ok, alst ditch effort: doing it step-wise, and then I am giving up. Cool.  
  
  ichao1_sre_fn <- function(x) { overlay(a, n, f1, f2, f3, f4, fun = function(a, n, f1, f2, f3, f4) {
    ifelse(f2 == 0, 
           (a + (((n-1) / n) * (f1 * (f1-1)) / (2 * (f2+1)))), # no dbltons
           ifelse(f3 == 0, 
                  (a + (((n-1) / n) * ((f1^2)/(2*f2)))), # yes dobltons
                  ifelse(f4 == 0, 
                         ((a + (((n-1) / n) * ((f1^2)/(2*f2)))) + 
                            ((((n-3) / (4*n)) * (f3/(f4+1))) *
                               (ifelse( (f1 - ((n-3) / (2*(n-1))) * (f2*f3/(f4+1))) > 0, 
                                        (f1 - ((n-3) / (2*(n-1))) * (f2*f3/(f4+1))), 0 )))), # no quads (this line and the three above it)
                         
                         ((a + (((n-1) / n) * ((f1^2)/(2*f2)))) + 
                            ((((n-3) / (4*n)) * (f3/f4)) *
                               (ifelse( (f1 - ((n-3) / (2*(n-1))) * (f2*f3/f4)) > 0, 
                                        (f1 - ((n-3) / (2*(n-1))) * (f2*f3/f4)), 0 )))) # yes quads ("               ")
                  ))
    )
  } # finish function
  )
  } # finish function
  
  ichao1_raster <- ichao1_sre_fn(ichao1)
  plot(ichao1_raster)
  # wooooooooooooooooooooooooooooooooooooooooooooooooooooooow
  
  
  # save: actual (a), ichao1 (ichao1), and just the OG chao1
  writeRaster(a, "C:/Users/s436862/Dropbox/Climate Matching/4. Results/AVH/200 km scale/baseline_SR_200km.grd", prj = T, overwrite = T)
  writeRaster(ichao1_raster, "C:/Users/s436862/Dropbox/Climate Matching/4. Results/AVH/200 km scale/ichao1_200km.grd", prj = T, overwrite = T)
  writeRaster(chao1, "C:/Users/s436862/Dropbox/Climate Matching/4. Results/AVH/200 km scale/chao1_200km.grd", prj = T, overwrite = T)
  
  
  
# Plot Chao1  ----------------------------------------------------------
  chao1_ras <- raster("C:/Users/s436862/Dropbox/Climate Matching/4. Results/AVH/200 km scale/chao1_200km.grd")
  
  q <- gplot(chao1_ras) + 
    theme_classic()+
    geom_raster(aes(fill = value)) +
    scale_fill_gradient(low = 'white',
                        high = 'red',
                        na.value="grey") +
    coord_equal()
  
  p <- q + guides(fill=guide_legend(title="Chao1 SR")) +
    xlab("Long") + 
    ylab("Lat") +
    oz + 
    xlim(110, 160) +
    ylim(-50, -5)
  
  print(p)
  
  # save: chao1
  ggsave("C:/Users/s436862/Dropbox/Climate Matching/4. Results/AVH/200 km scale/200km chao1 SR.jpeg", plot = last_plot(), scale = 1, dpi = 300, device = "jpeg")
  
# Plot actual (henceforth 'baseline') ----------------------------------------------------------  
  baseline_ras <- raster("C:/Users/s436862/Dropbox/Climate Matching/4. Results/AVH/200 km scale/baseline_SR_200km.grd")
  
  q <- gplot(baseline_ras) + 
    theme_classic()+
    geom_raster(aes(fill = value)) +
    scale_fill_gradient(low = 'white',
                        high = 'red',
                        na.value="grey") +
    coord_equal()
  
  p <- q + guides(fill=guide_legend(title="Baseline (AVH) SR")) +
    xlab("Long") + 
    ylab("Lat") +
    oz + 
    xlim(110, 160) +
    ylim(-50, -5)
  
  print(p)
  
  # save: baseline
  ggsave("C:/Users/s436862/Dropbox/Climate Matching/4. Results/AVH/200 km scale/200km baseline SR.jpeg", plot = last_plot(), scale = 1, dpi = 300, device = "jpeg")
  
  
# Plot Jost_C ----------------------------------------------------------  
  jost_C <- raster("C:/Users/s436862/Dropbox/Climate Matching/4. Results/AVH/200 km scale/jost_C_200km.grd")
  
  q <- gplot(jost_C) + 
    theme_classic()+
    geom_raster(aes(fill = value)) +
    scale_fill_gradient(low = 'white',
                        high = 'red',
                        na.value="grey") +
    coord_equal()
  
  p <- q + guides(fill=guide_legend(title="Jost Coverage (C)")) +
    xlab("Long") + 
    ylab("Lat") +
    oz + 
    xlim(110, 160) +
    ylim(-50, -5)
  
  print(p)
  
  # save: ratio
  ggsave("C:/Users/s436862/Dropbox/Climate Matching/4. Results/AVH/200 km scale/200 km Jost coverage.jpeg", plot = last_plot(), scale = 1, dpi = 300, device = "jpeg")
  
  
  
  
# Plot actual/Chao1_C ----------------------------------------------------------  
  actual_C <- raster("C:/Users/s436862/Dropbox/Climate Matching/4. Results/AVH/200 km scale/actual_C_200km.grd")
  
  q <- gplot(actual_C) + 
    theme_classic()+
    geom_raster(aes(fill = value)) +
    scale_fill_gradient(low = 'white',
                        high = 'red',
                        na.value="grey") +
    coord_equal()
  
  p <- q + guides(fill=guide_legend(title="baseline coverage")) +
    xlab("Long") + 
    ylab("Lat") +
    oz + 
    xlim(110, 160) +
    ylim(-50, -5)
  
  print(p)
  
  # save: ratio
  ggsave("C:/Users/s436862/Dropbox/Climate Matching/4. Results/AVH/200 km scale/200 km baseline coverage.jpeg", plot = last_plot(), scale = 1, dpi = 300, device = "jpeg")
  
  
  
  
# -------------------------------------------------  
  
  
  
  
  
  
  
  
  
