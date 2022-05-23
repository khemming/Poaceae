
# Date created: 18/4
# Last modified: 12/3/19 (updated using outputs from rarefaction v6)

# Rarefaction Paper rasters and other plots

# Based on: 'Rarefaction df + rasters' and 'Rarefaction plots' and Engemann's results flow

# v3 = has lots of things cut out of it that aren't totally necessary for the paper; see version 2 for a complete set.

# Library --------------------------------------------------------------
  setwd("C:/Users/s436862/Dropbox/Rarefaction")
  
  library(RColorBrewer)
  library(raster)
  library(ggmap)
  library(rgdal)
  library(ggplot2)
  library(dplyr)
  library(rasterVis)
  library(maptools)
  library(ggthemes)
  library(ggThemeAssist)
  library(gplots)
  library(tidyr)
  library(forcats)

  rm(list = ls())
  
# 1. raster plot function ----------------------------------------------------------------  
# notes --------------------------------------------------------------------------------
# copy and past 'Raster plot function' section at the beginning of each new chapter. 
# alter that one as needed for that chapter's section. 
# When you want to run multiple plots of similar design within a chapter, simply specify the new required inputs as appropriate   
# plot look is based on Engemann's paper, except where it isn't
# --------------------------------------------------------------------------------------- 
# 1.1 generic raster plot function & notes -----------------------------------------------
#  Requires:
# (1) raster
# (2) legend [title] (e.g. 'record number', 'species richness')
# (3) file save name (e.g. Graphs/Native SR 50-km.jpeg)  [needs full extension]

# raster    
  aus <- raster("C:/Users/s436862/Dropbox/Climate Matching/Data files/Australia/aus_100km")

# function   
  eng_ras <- function (raster, legend, save)  
    
  {
# AUS border + NA fill
    oz1 <- borders("world", region = "Australia", fill = "grey60", bg = "white")
    oz2 <- borders(database = "world", regions = "Australia", colour = "black")
    
# Colour palette for legend
    colour <- rev(brewer.pal(11, "Spectral"))
# display.brewer.all() 
# (for more info)
    
# Plot
    q <- gplot(raster) + 
      theme_map()+
      oz1 +
      geom_raster(aes(fill = value)) +
      scale_fill_gradientn(colours = colour, 
                           limits = c(0, 300),
                           space = "Lab",
                           na.value = "transparent",
                           guide = "colourbar",
                           name = legend) + 
      coord_equal() +
      coord_cartesian(xlim = c(112, 155), ylim = c(-45, -7), expand = F) +
      theme(legend.justification = "centre",
            legend.position = "right",
            aspect.ratio = 0.88) +
      oz2
    print(q)
    
    ggsave(save, plot = last_plot(), scale = 1, dpi = 500, device = "jpeg")
    
  } # finish function
  
  
# --------------------------------------------------------------------------------------------
  
# 1.2 Rarefaction 15- to 50- records, Nat/Int, total/C3/C4 -----------------------------------
# Required -----------------------------------------------------------------------------------
# (1) raster
# (2) legend [title] (e.g. record number or species richness)
# (3) file save name (e.g. Graphs/Native SR 50-km.jpeg)  

# note: standardised legend, therefore 15- and 50- records will appear similar (rather than having the scale at 50-records for each plot, which would show magnitude decreases, we're instead showing relative differences in patterns)
  
# Rarefaction function ---------------------------------------------------------
# function   
  eng_ras <- function (raster, save)  
    
  {
# AUS border + NA fill
    oz1 <- borders("world", region = "Australia", fill = "grey60", bg = "white")
    oz2 <- borders(database = "world", regions = "Australia", colour = "black")
    
# Colour palette for legend
    colour <- rev(brewer.pal(11, "Spectral"))
# display.brewer.all() 
# (for more info)
    
# Plot
    q <- gplot(raster) + 
      theme_map()+
      oz1 +
      geom_raster(aes(fill = value)) +
      scale_fill_gradientn(colours = colour, 
                           space = "Lab",
                           na.value = "transparent",
                           guide = "colourbar",
                           name = "Standardised\nrichness",
                           limits = c(0, 1)) + 
      coord_equal() +
      coord_cartesian(xlim = c(112, 155), ylim = c(-45, -7), expand = F) +
      theme(legend.justification = "centre",
            legend.position = "right",
            aspect.ratio = 0.88) +
      oz2
    
    print(q)
    
    ggsave(save, plot = last_plot(), scale = 1, dpi = 500, device = "jpeg")
    
  } # finish function
  
# Data ------------------------------------------------
  setwd("C:/Users/s436862/Dropbox/Rarefaction/Results/Rarefaction/Rasters/Rarefied rasters complete")
  
  current.list <- list.files(pattern = ".grd")
  names <- gsub(pattern = "\\.grd$", "", current.list)
  
  c.stack <- stack(current.list)
  names(c.stack) <- names

  list2env(setNames(unstack(c.stack), names(c.stack)), .GlobalEnv)
    
# Run function ------------------------------------------------
  setwd("C:/Users/s436862/Dropbox/Rarefaction/Results/Rarefaction/Graphs/Rarefied plots complete")

# r15.i.c3
  eng_ras(r15.i.c3/15, "r15.i.c3.jpeg")
# r15.i.c4  
  eng_ras(r15.i.c4/15, "r15.i.c4.jpeg")
# r15.i.tot 
  eng_ras(r15.i.tot/15, "r15.i.tot.jpeg")

# r15.n.c3  
  eng_ras(r15.n.c3/15, "r15.n.c3.jpeg")
# r15.n.c4  
  eng_ras(r15.n.c4/15, "r15.n.c4.jpeg")
# r15.n.tot 
  eng_ras(r15.n.tot/15, "r15.n.tot.jpeg")

# r20.i.c3  
  eng_ras(r20.i.c3/20, "r20.i.c3.jpeg")
# r20.i.c4  
  eng_ras(r20.i.c4/20, "r20.i.c4.jpeg")
# r20.i.tot
  eng_ras(r20.i.tot/20, "r20.i.tot.jpeg")

# r20.n.c3 
  eng_ras(r20.n.c3/20, "r20.n.c3.jpeg")
# r20.n.c4  
  eng_ras(r20.n.c4/20, "r20.n.c4.jpeg")
# r20.n.tot 
  eng_ras(r20.n.tot/20, "r20.n.tot.jpeg")

# r25.i.c3  
  eng_ras(r25.i.c3/25, "r25.i.c3.jpeg")
# r25.i.c4  
  eng_ras(r25.i.c4/25, "r25.i.c4.jpeg")
# r25.i.tot 
  eng_ras(r25.i.tot/25, "r25.i.tot.jpeg")

# r25.n.c3  
  eng_ras(r25.n.c3/25, "r25.n.c3.jpeg")
# r25.n.c4  
  eng_ras(r25.n.c4/25, "r25.n.c4.jpeg")
# r25.n.tot
  eng_ras(r25.n.tot/25, "r25.n.tot3.jpeg")

# r30.i.c3  
  eng_ras(r30.i.c3/30, "r30.i.c3.jpeg")
# r30.i.c4  
  eng_ras(r30.i.c4/30, "r30.i.c4.jpeg")
# r30.i.tot 
  eng_ras(r30.i.tot/30, "r30.i.tot.jpeg")

# r30.n.c3  
  eng_ras(r30.n.c3/30, "r30.n.c3.jpeg")
# r30.n.c4  
  eng_ras(r30.n.c4/30, "r30.n.c4.jpeg")
# r30.n.tot 
  eng_ras(r30.n.tot/30, "r30.n.tot.jpeg")

# r35.i.c3  
  eng_ras(r35.i.c3/35, "r35.i.c3.jpeg")
# r35.i.c4  
  eng_ras(r35.i.c4/35, "r35.i.c4.jpeg")
# r35.i.tot
  eng_ras(r35.i.tot/35, "r35.i.tot.jpeg")

# r35.n.c3  
  eng_ras(r35.n.c3/35, "r35.n.c3.jpeg")
# r35.n.c4  
  eng_ras(r35.n.c4/35, "r35.n.c4.jpeg")
# r35.n.tot 
  eng_ras(r35.n.tot/35, "r35.n.tot.jpeg")

# r40.i.c3  
  eng_ras(r40.i.c3/40, "r40.i.c3.jpeg")
# r40.i.c4  
  eng_ras(r40.i.c4/40, "r40.i.c4.jpeg")
# r40.i.tot 
  eng_ras(r40.i.tot/40, "r40.i.tot.jpeg")

# r40.n.c3  
  eng_ras(r40.n.c3/40, "r40.n.c3.jpeg")
# r40.n.c4  
  eng_ras(r40.n.c4/40, "r40.n.c4.jpeg")
# r40.n.tot
  eng_ras(r40.n.tot/40, "r40.n.tot.jpeg")
  
# r45.i.c3  
  eng_ras(r45.i.c3/45, "r45.i.c3.jpeg")
# r45.i.c4  
  eng_ras(r45.i.c4/45, "r45.i.c4.jpeg")
# r45.i.tot 
  eng_ras(r45.i.tot/45, "r45.i.tot.jpeg")

# r45.n.c3  
  eng_ras(r45.n.c3/45, "r45.n.c3.jpeg")
# r45.n.c4  
  eng_ras(r45.n.c4/45, "r45.n.c4.jpeg")
# r45.n.tot 
  eng_ras(r45.n.tot/45, "r45.n.tot.jpeg")

# r50.i.c3  
  eng_ras(r50.i.c3/50, "r50.i.c3.jpeg")
# r50.i.c4  
  eng_ras(r50.i.c4/50, "r50.i.c4.jpeg")
# r50.i.tot
  eng_ras(r50.i.tot/50, "r50.i.tot.jpeg")
 
# r50.n.c3
  eng_ras(r50.n.c3/50, "r50.n.c3.jpeg")
# r50.n.c4 
  eng_ras(r50.n.c4/50, "r50.n.c4.jpeg")
# r50.n.tot
  eng_ras(r50.n.tot/50, "r50.n.tot.jpeg")
  
 
  
  
  
# 1.3 Rarefaction N/E x PP correlations --------------------- ----------------
# How do c3/c4 exotic and native species correlate?
# Data -----------------------------------------------------------------------
  setwd("C:/Users/s436862/Dropbox/Rarefaction/Results/Rarefaction")
  
# Spp data   
  nat <- read.csv("CSV/Native multiple cutoff COMPLETE.csv", header = T) %>%
    dplyr::select(total.rare.15, C3.rare.15, C4.rare.15)
  colnames(nat) <- c("nat.tot", "nat.c3", "nat.c4")
  
  int <- read.csv("CSV/Introduced multiple cutoff COMPLETE.csv", header = T) %>%
    dplyr::select(total.rare.15, C3.rare.15, C4.rare.15)
  colnames(int) <- c("int.tot", "int.c3", "int.c4")
  
# Subset Aus-only cells
  cell.cat <- read.csv("C:/Users/s436862/Dropbox/Climate Matching/Data files/EFs/CSV/Terrestrial land categories.csv", header = T)
  
# Subset terrestrial cells out of data frames  
  poa.land <- cbind(cell.cat, nat, int)
  poa.terr <- filter(poa.land, cell.cat == "terrestrial") %>%
    dplyr::select(-cell.cat, -cell.id)
    

# Scatter plot function ---------------------------------------------------------
# Requires:
# df: complete df of all the efs and spp
# xcol: spp1
# ycol: spp2
# xlab: spp1 name
# ylab: spp2 name
# save: exact save locale
# ggThemeAssistGadget(plot1) <- test this first 
#                               (and then # out for actual script)
  
# Function 
  plot_fun <- function(df, xcol, ycol, xlab, ylab, save){
    df <- select(df, x = one_of(xcol), y = one_of(ycol))
    
    n <- ggplot(data = df, aes(x, y)) +
      geom_point(size = 1.5) +
      theme_bw() + 
      labs(x = xlab,
           y = ylab) +
      theme(axis.title = element_text(size = 12)) + 
      geom_smooth(method = "lm", se = FALSE)
    
  #ggThemeAssistGadget(n) 
    
    
  m <- n + theme(panel.grid.major = element_line(linetype = "blank"), 
    panel.grid.minor = element_line(linetype = "blank"), 
    panel.background = element_rect(fill = NA))
  
# save
    ggsave(save, plot = last_plot(), dpi = 500, device = "jpeg")
    
    return(m)
  } # finish script
  
# Run function ---------------------------------------------------------  
# Requires in this order:
# df: complete df of all the efs and spp
# xcol: enviro factor
# ycol: spp
# xlab: spp name
# ylab: ef name
# save: exact save locale 

# Total -------------------------------------------------------------
  df <- poa.terr
  xcol <- "nat.tot"
  ycol <- "int.tot"
  xlab <- "Native total richness"
  ylab <- "Introduced total richness"
  save <- "Graphs/Nat vs. Int/Total native-introduced richness.jpeg" 
  
  tot <- plot_fun(df, 
                 xcol,
                 ycol,
                 xlab,
                 ylab,
                 save)
  tot
  
# C3 (nat vs. int) -----------------------------------------------------
  df <- poa.terr
  xcol <- "nat.c3"
  ycol <- "int.c3"
  xlab <- "Native-C3 richness"
  ylab <- "Introduced-C3 richness"
  save <- "Graphs/Nat vs. Int/C3 native-introduced richness.jpeg" 
  
  c3 <- plot_fun(df, 
           xcol,
           ycol,
           xlab,
           ylab,
           save)
  c3
  
# C4 (nat vs. int) ----------------------------------------------------
  df <- spp
  xcol <- "nat.c4"
  ycol <- "int.c4"
  xlab <- "Native-C4 richness"
  ylab <- "Introduced-C4 richness"
  save <- "Graphs/Nat vs. Int/C4 native-introduced richness.jpeg" 
  
  c4 <- plot_fun(df, 
           xcol,
           ycol,
           xlab,
           ylab,
           save)
  c4
  
# Correlation estimates ----------------------------------------------  
  poa.corr <- cor(poa.terr, use = "complete.obs")
  write.csv(poa.corr, "CSV/15-record rarefaction Nat-Int x PP correlation matrix.csv", row.names = T)
# --------------------------------------------------------------------------------------------
  
# 1.4 15-record rarefaction histograms Nat/Int, tot/C3/C4 ---------------------------------------
# Data ---------------------------------------------------------------- 
  setwd("C:/Users/s436862/Dropbox/Rarefaction/Results/Rarefaction")
# We have two species data frames: nat and int
# These have teh two rarefied records, and all three pp pathways
  nat <- read.csv("CSV/Native multiple cutoff COMPLETE.csv", header = T) %>%
    dplyr::select(total.rare.15, C3.rare.15, C4.rare.15)
  colnames(nat) <- c("n.tot", "n.c3", "n.c4")
  head(nat)
  
  int <- read.csv("CSV/Introduced multiple cutoff COMPLETE.csv", header = T) %>%
    dplyr::select(total.rare.15, C3.rare.15, C4.rare.15)
  colnames(int) <- c("i.tot", "i.c3", "i.c4")
  
# histograms -------
  setwd("C:/Users/s436862/Dropbox/Rarefaction/Results/Rarefaction/Graphs")
# Native total  
  jpeg("Rarefaction 15-record histograms/Native total.jpeg", width = 13, height = 10, units = "cm", res = 500)
  hist(nat$n.tot)
  hist(v.last.m)
dev.off()
# c3  
  jpeg("Rarefaction 15-record histograms/Native C3.jpeg", width = 13, height = 10, units = "cm", res = 500)
  hist(nat$n.c3)
  dev.off()
# c4
  jpeg("Rarefaction 15-record histograms/Native C4.jpeg", width = 13, height = 10, units = "cm", res = 500)
  hist(nat$n.c4)
  dev.off()
  
# Introduced  
  jpeg("Rarefaction 15-record histograms/Introduced total.jpeg", width = 13, height = 10, units = "cm", res = 500)
  hist(int$i.tot)
  dev.off()
# c3  
  jpeg("Rarefaction 15-record histograms/Introduced C3.jpeg", width = 13, height = 10, units = "cm", res = 500)
  hist(int$i.c3)
  dev.off()
# c4
  jpeg("Rarefaction 15-record histograms/Introduced C4.jpeg", width = 13, height = 10, units = "cm", res = 500)
  hist(int$i.c4)
  dev.off()
  
# --------------------------------------------------------------------------------------------
  
  
# 3. Native and Exotic Records and richness through scale (30/10) ----------------------------
# record number and richness for total, C3 and C4, plotted etc. And then correlations between these to emphesise spatial biases in AVH data 
  
# 3.1 Native records ---------------------------------------------------------------------------------
# Record-number per cell rasters at 10- 50-, 100-, 200- & 300-km ------------------
# Data -------------------------------------------------------------------------------
  setwd("C:/Users/s436862/Dropbox/Climate matching/Data files")
    
# Species 
  x <- read.csv("Osborne C3-C4/AVH grass pp.csv", header = T) %>%
    filter(status == "native")
  
# raster (1-km scale)
  v <- raster("C:/Users/s436862/Dropbox/Climate Matching/Data files/Australia/aus")
  
# put records into cells    
  xy <- cbind(x$long, x$lat) 
  spp <- as.numeric(factor(x$species))
  
# scale/width (v)
  v10 <- aggregate(v, fact = 10, fun = mean)
  v20 <- aggregate(v, fact = 20, fun = mean)
  v50 <- aggregate(v, fact = 50, fun = mean)
  v100 <- aggregate(v, fact = 100, fun = mean)
  v200 <- aggregate(v, fact = 200, fun = mean)
  v300 <- aggregate(v, fact = 300, fun = mean)
  
# create raster for each km class
  n10 <- rasterize(xy, v10, fun = function(x,...) {length(na.omit(x)) })
  n20 <- rasterize(xy, v20, fun = function(x,...) {length(na.omit(x)) })
  n50 <- rasterize(xy, v50, fun = function(x,...) {length(na.omit(x)) })
  n100 <- rasterize(xy, v100, fun = function(x,...) {length(na.omit(x)) })
  n200 <- rasterize(xy, v200, fun = function(x,...) {length(na.omit(x)) })
  n300 <- rasterize(xy, v300, fun = function(x,...) {length(na.omit(x)) })
  
# Mask cells who's centres are contained in the continent
  aus_shp <- readOGR("C:/Users/s436862/Dropbox/Climate Matching/Data files/Australia/australia_shapefile.shp")
  
  n10 <- mask(n10, aus_shp)
  plot(n10)
  n20 <- mask(n20, aus_shp)
  plot(n20)
  n50 <- mask(n50, aus_shp)
  plot(n50)
  n100 <- mask(n100, aus_shp)
  plot(n100)
  n200 <- mask(n200, aus_shp)
  plot(n200)
  n300 <- mask(n300, aus_shp)
  plot(n300)
  
# shift to dataframe  
  n10_n <- getValues(n10)
  n20_n <- getValues(n20)
  n50_n <- getValues(n50)
  n100_n <- getValues(n100)
  n200_n <- getValues(n200)
  n300_n <- getValues(n300)
  
# repeat for arid raster (basis for removing offshore cells)
  v10_n <- getValues(v10)
  v20_n <- getValues(v20)
  v50_n <- getValues(v50)
  v100_n <- getValues(v100)
  v200_n <- getValues(v200)
  v300_n <- getValues(v300)
  
# save rasters  
  setwd("C:/Users/s436862/Dropbox/Rarefaction/Results/Rarefaction/Rasters/Raw records multiscale")
  
  writeRaster(n10, "Native 10-km", overwrite = T)
  writeRaster(n20, "Native 20-km", overwrite = T)
  writeRaster(n50, "Native 50-km", overwrite = T)
  writeRaster(n100, "Native 100-km", overwrite = T)
  writeRaster(n200, "Native 200-km", overwrite = T)
  writeRaster(n300, "Native 300-km", overwrite = T)
  
# Raster plot function ---------------------------------------------------------------
# same as previous

  setwd("C:/Users/s436862/Dropbox/Rarefaction/Results/Rarefaction")

  eng_ras <- function (raster, legend, save)  
  
{
# AUS border
  oz1 <- borders("world", region = "Australia", fill = "grey60")
  oz2 <- borders("world", region = "Australia", colour = "black")
 
# Colour palette for legend
  colour <- rev(brewer.pal(11, "Spectral"))
  # display.brewer.all() 
  # (for more info)

# Plot
  q <- gplot(raster) + 
    theme_map()+
    oz1 +
  geom_raster(aes(fill = value)) +
    scale_fill_gradientn(colours = colour, 
                         space = "Lab",
                         na.value = "transparent",
                         guide = "colourbar",
                         name = legend) + 
    coord_equal() +
    coord_cartesian(xlim = c(112, 155), ylim = c(-45, -7), expand = F) +
    theme(legend.justification = "centre",
          legend.position = "right",
          aspect.ratio = 0.88) +
    oz2
  print(q)
  
  ggsave(save, plot = last_plot(), scale = 1, dpi = 500, device = "jpeg")
  
} # finish function
# 10-km plot ----------------------------------------------  
  raster <- raster("Rasters/Raw records multiscale/Native 10-km")
  legend <- "Record number"
  save <- "Graphs/Raw records multiscale/Native 10-km.jpeg"
  
  eng_ras(raster, legend, save)
  
# 20-km plot ----------------------------------------------  
  raster <- raster("Rasters/Raw records multiscale/Native 20-km")
  legend <- "Record number"
  save <- "Graphs/Raw records multiscale/Native 20-km.jpeg"
  
  eng_ras(raster, legend, save)
  
  
# 50-km plot ------------------------------------------------
  raster <- raster("Rasters/Raw records multiscale/Native 50-km")
  legend <- "Record number"
  save <- "Graphs/Raw records multiscale/Native 50-km.jpeg"
  
  eng_ras(raster, legend, save)
  
# 100-km plot ----------------------------------------------  
  raster <- raster("Rasters/Raw records multiscale/Native 100-km")
  legend <- "Record number"
  save <- "Graphs/Raw records multiscale/Native 100-km.jpeg"
  
  eng_ras(raster, legend, save)
  
# 200-km plot ----------------------------------------------  
  raster <- raster("Rasters/Raw records multiscale/Native 200-km")
  legend <- "Record number"
  save <- "Graphs/Raw records multiscale/Native 200-km.jpeg"
  
  eng_ras(raster, legend, save)
  
  
# 300-km plot ----------------------------------------------  
  raster <- raster("Rasters/Raw records multiscale/Native 300-km")
  legend <- "Record number"
  save <- "Graphs/Raw records multiscale/Native 300-km.jpeg"
  
  eng_ras(raster, legend, save)
  
  
# -------------------------------------------------------------------------------------
  
# 3.2 Native species richness -----------------------------------------------------
# Richness per cell rasters at 10- 50-, 100-, 200- & 300-km ------------------
# Data -------------------------------------------------------------------------------
  setwd("C:/Users/s436862/Dropbox/Climate matching/Data files")
  
# Species 
  x <- read.csv("Osborne C3-C4/AVH grass pp.csv", header = T) %>%
    filter(status == "native")
  
# raster (1-km scale)
  v <- raster("C:/Users/s436862/Dropbox/Climate Matching/Data files/Australia/aus")
  
# put records into cells    
  xy <- cbind(x$long, x$lat) 
  spp <- as.numeric(factor(x$species))
  
# scale/width (v)
  v10 <- aggregate(v, fact = 10, fun = mean)
  v20 <- aggregate(v, fact = 20, fun = mean)
  v50 <- aggregate(v, fact = 50, fun = mean)
  v100 <- aggregate(v, fact = 100, fun = mean)
  v200 <- aggregate(v, fact = 200, fun = mean)
  v300 <- aggregate(v, fact = 300, fun = mean)
  
# create raster for each km class
  n10 <- rasterize(xy, v10, field = spp, fun = function(x,...) {length(unique(na.omit(x))) })
  n20 <- rasterize(xy, v20, field = spp, fun = function(x,...) {length(unique(na.omit(x))) })
  n50 <- rasterize(xy, v50, field = spp, fun = function(x,...) {length(unique(na.omit(x))) })
  n100 <- rasterize(xy, v100, field = spp, fun = function(x,...) {length(unique(na.omit(x))) })
  n200 <- rasterize(xy, v200, field = spp, fun = function(x,...) {length(unique(na.omit(x))) })
  n300 <- rasterize(xy, v300, field = spp, fun = function(x,...) {length(unique(na.omit(x))) })
  
# Mask cells who's centres are contained in the continent
  aus_shp <- readOGR("C:/Users/s436862/Dropbox/Climate Matching/Data files/Australia/australia_shapefile.shp")
  
  n10 <- mask(n10, aus_shp)
  plot(n10)
  n20 <- mask(n20, aus_shp)
  plot(n20)
  n50 <- mask(n50, aus_shp)
  plot(n50)
  n100 <- mask(n100, aus_shp)
  plot(n100)
  n200 <- mask(n200, aus_shp)
  plot(n200)
  n300 <- mask(n300, aus_shp)
  plot(n300)
  
# shift to dataframe  
  n10_n <- getValues(n10)
  n20_n <- getValues(n20)
  n50_n <- getValues(n50)
  n100_n <- getValues(n100)
  n200_n <- getValues(n200)
  n300_n <- getValues(n300)
  
# repeat for arid raster (basis for removing offshore cells)
  v10_n <- getValues(v10)
  v20_n <- getValues(v20)
  v50_n <- getValues(v50)
  v100_n <- getValues(v100)
  v200_n <- getValues(v200)
  v300_n <- getValues(v300)
  
# save rasters  
  setwd("C:/Users/s436862/Dropbox/Rarefaction/Results/Rarefaction/Rasters/Raw species richness multiscale")
  
  writeRaster(n10, "Native 10-km", overwrite = T)
  writeRaster(n20, "Native 20-km", overwrite = T)
  writeRaster(n50, "Native 50-km", overwrite = T)
  writeRaster(n100, "Native 100-km", overwrite = T)
  writeRaster(n200, "Native 200-km", overwrite = T)
  writeRaster(n300, "Native 300-km", overwrite = T)
  
# Raster plot function ---------------------------------------------------------------
# same as previous
  
  setwd("C:/Users/s436862/Dropbox/Rarefaction/Results/Rarefaction")
  
  eng_ras <- function (raster, legend, save)  
    
  {
  # AUS border
    oz1 <- borders("world", region = "Australia", fill = "grey60")
    oz2 <- borders("world", region = "Australia", colour = "black")
    
  # Colour palette for legend
    colour <- rev(brewer.pal(11, "Spectral"))
  # display.brewer.all() 
  # (for more info)
    
  # Plot
    q <- gplot(raster) + 
      theme_map()+
      oz1 +
      geom_raster(aes(fill = value)) +
      scale_fill_gradientn(colours = colour, 
                           space = "Lab",
                           na.value = "transparent",
                           guide = "colourbar",
                           name = legend) + 
      coord_equal() +
      coord_cartesian(xlim = c(112, 155), ylim = c(-45, -7), expand = F) +
      theme(legend.justification = "centre",
            legend.position = "right",
            aspect.ratio = 0.88) +
      oz2
    print(q)
    
    ggsave(save, plot = last_plot(), scale = 1, dpi = 500, device = "jpeg")
    
  } # finish function
# 10-km plot ----------------------------------------------  
  raster <- raster("Rasters/Raw records multiscale/Native 10-km")
  legend <- "Species richness"
  save <- "Graphs/Raw species richness multiscale/Native 10-km.jpeg"
  
  eng_ras(raster, legend, save)
  
  
# 20-km plot ----------------------------------------------  
  raster <- raster("Rasters/Raw species richness multiscale/Native 20-km")
  legend <- "Species richness"
  save <- "Graphs/Raw species richness multiscale/Native 20-km.jpeg"
  
  eng_ras(raster, legend, save)
  
  
# 50-km plot ------------------------------------------------
  raster <- raster("Rasters/Raw species richness multiscale/Native 50-km")
  legend <- "Species richness"
  save <- "Graphs/Raw species richness multiscale/Native 50-km.jpeg"
  
  eng_ras(raster, legend, save)
  
# 100-km plot ----------------------------------------------  
  raster <- raster("Rasters/Raw species richness multiscale/Native 100-km")
  legend <- "Species richness"
  save <- "Graphs/Raw species richness multiscale/Native 100-km.jpeg"
  
  eng_ras(raster, legend, save)
  
# 200-km plot ----------------------------------------------  
  raster <- raster("Rasters/Raw species richness multiscale/Native 200-km")
  legend <- "Species richness"
  save <- "Graphs/Raw species richness multiscale/Native 200-km.jpeg"
  
  eng_ras(raster, legend, save)
  
  
# 300-km plot ----------------------------------------------  
  raster <- raster("Rasters/Raw species richness multiscale/Native 300-km")
  legend <- "Species richness"
  save <- "Graphs/Raw species richness multiscale/Native 300-km.jpeg"
  
  eng_ras(raster, legend, save)
  
  
# -------------------------------------------------------------------------------------  
  
# 3.3 Exotic (10/9) -------------------------------------------------------------------
# Record-number per cell rasters at 10- 50-, 100-, 200- & 300-km ----------------------
# Data --------------------------------------------------------------------------------
  rm(list = ls())
  setwd("C:/Users/s436862/Dropbox/Climate matching/Data files")
  
# Species 
  x <- read.csv("Osborne C3-C4/AVH grass pp.csv", header = T) %>%
    filter(status == "introduced")
  
# raster (1-km scale)
  v <- raster("C:/Users/s436862/Dropbox/Climate Matching/Data files/Australia/aus")
  
# put records into cells    
  xy <- cbind(x$long, x$lat) 
  spp <- as.numeric(factor(x$species))
  
# scale/width (v)
  v10 <- aggregate(v, fact = 10, fun = mean)
  v20 <- aggregate(v, fact = 20, fun = mean)
  v50 <- aggregate(v, fact = 50, fun = mean)
  v100 <- aggregate(v, fact = 100, fun = mean)
  v200 <- aggregate(v, fact = 200, fun = mean)
  v300 <- aggregate(v, fact = 300, fun = mean)
  
# create raster for each km class
  n10 <- rasterize(xy, v10, fun = function(x,...) {length(na.omit(x)) })
  n20 <- rasterize(xy, v20, fun = function(x,...) {length(na.omit(x)) })
  n50 <- rasterize(xy, v50, fun = function(x,...) {length(na.omit(x)) })
  n100 <- rasterize(xy, v100, fun = function(x,...) {length(na.omit(x)) })
  n200 <- rasterize(xy, v200, fun = function(x,...) {length(na.omit(x)) })
  n300 <- rasterize(xy, v300, fun = function(x,...) {length(na.omit(x)) })
  
# Mask cells who's centres are contained in the continent
  aus_shp <- readOGR("C:/Users/s436862/Dropbox/Climate Matching/Data files/Australia/australia_shapefile.shp")
  
  n10 <- mask(n10, aus_shp)
  plot(n10)
  n20 <- mask(n20, aus_shp)
  plot(n20)
  n50 <- mask(n50, aus_shp)
  plot(n50)
  n100 <- mask(n100, aus_shp)
  plot(n100)
  n200 <- mask(n200, aus_shp)
  plot(n200)
  n300 <- mask(n300, aus_shp)
  plot(n300)
  
# shift to dataframe  
  n10_n <- getValues(n10)
  n20_n <- getValues(n20)
  n50_n <- getValues(n50)
  n100_n <- getValues(n100)
  n200_n <- getValues(n200)
  n300_n <- getValues(n300)
  
# repeat for arid raster (basis for removing offshore cells)
  v10_n <- getValues(v10)
  v20_n <- getValues(v20)
  v50_n <- getValues(v50)
  v100_n <- getValues(v100)
  v200_n <- getValues(v200)
  v300_n <- getValues(v300)
  
# save rasters  
  setwd("C:/Users/s436862/Dropbox/Rarefaction/Results/Rarefaction/Rasters/Raw records multiscale")
  
  writeRaster(n10, "Introduced 10-km", overwrite = T)
  writeRaster(n20, "Introduced 20-km", overwrite = T)
  writeRaster(n50, "Introduced 50-km", overwrite = T)
  writeRaster(n100, "Introduced 100-km", overwrite = T)
  writeRaster(n200, "Introduced 200-km", overwrite = T)
  writeRaster(n300, "Introduced 300-km", overwrite = T)
  
# Raster plot function ---------------------------------------------------------------
# same as previous
  
  setwd("C:/Users/s436862/Dropbox/Rarefaction/Results/Rarefaction")
  
  eng_ras <- function (raster, legend, save)  
    
  {
  # AUS border
    oz1 <- borders("world", region = "Australia", fill = "grey60")
    oz2 <- borders("world", region = "Australia", colour = "black")
    
  # Colour palette for legend
    colour <- rev(brewer.pal(11, "Spectral"))
  # display.brewer.all() 
  # (for more info)
    
  # Plot
    q <- gplot(raster) + 
      theme_map()+
      oz1 +
      geom_raster(aes(fill = value)) +
      scale_fill_gradientn(colours = colour, 
                           space = "Lab",
                           na.value = "transparent",
                           guide = "colourbar",
                           name = legend) + 
      coord_equal() +
      coord_cartesian(xlim = c(112, 155), ylim = c(-45, -7), expand = F) +
      theme(legend.justification = "centre",
            legend.position = "right",
            aspect.ratio = 0.88) +
      oz2
    print(q)
    
    ggsave(save, plot = last_plot(), scale = 1, dpi = 500, device = "jpeg")
    
  } # finish function
# 10-km plot ----------------------------------------------  
  raster <- raster("Rasters/Raw records multiscale/Introduced 10-km")
  legend <- "Record number"
  save <- "Graphs/Raw records multiscale/Introduced 10-km.jpeg"
  
  eng_ras(raster, legend, save)
  
# 10-km log-transformed plot
  raster <- log(n10)
  legend <- "Record number"
  save <- "Graphs/Raw records multiscale/Introduced 10-km log-transformed.jpeg"
  
  eng_ras(raster, legend, save)
  
# 20-km plot ----------------------------------------------  
  raster <- raster("Rasters/Raw records multiscale/Introduced 20-km")
  legend <- "Record number"
  save <- "Graphs/Raw records multiscale/Introduced 20-km.jpeg"
  
  eng_ras(raster, legend, save)
  
  
# 50-km plot ------------------------------------------------
  raster <- raster("Rasters/Raw records multiscale/Introduced 50-km")
  legend <- "Record number"
  save <- "Graphs/Raw records multiscale/Introduced 50-km.jpeg"
  
  eng_ras(raster, legend, save)
  
# 100-km plot ----------------------------------------------  
  raster <- raster("Rasters/Raw records multiscale/Introduced 100-km")
  legend <- "Record number"
  save <- "Graphs/Raw records multiscale/Introduced 100-km.jpeg"
  
  eng_ras(raster, legend, save)
  
# 200-km plot ----------------------------------------------  
  raster <- raster("Rasters/Raw records multiscale/Introduced 200-km")
  legend <- "Record number"
  save <- "Graphs/Raw records multiscale/Introduced 200-km.jpeg"
  
  eng_ras(raster, legend, save)
  
  
# 300-km plot ----------------------------------------------  
  raster <- raster("Rasters/Raw records multiscale/Introduced 300-km")
  legend <- "Record number"
  save <- "Graphs/Raw records multiscale/Introduced 300-km.jpeg"
  
  eng_ras(raster, legend, save)
  
  
# -------------------------------------------------------------------------------------
  
# 3.4 Exotic species richness -----------------------------------------------------
# Richness per cell rasters at 10- 50-, 100-, 200- & 300-km ------------------
# Data -------------------------------------------------------------------------------
  setwd("C:/Users/s436862/Dropbox/Climate matching/Data files")
  
# Species 
  x <- read.csv("Osborne C3-C4/AVH grass pp.csv", header = T) %>%
    filter(status == "introduced")
  
# raster (1-km scale)
  v <- raster("C:/Users/s436862/Dropbox/Climate Matching/Data files/Australia/aus")
  
# put records into cells    
  xy <- cbind(x$long, x$lat) 
  spp <- as.numeric(factor(x$species))
  
# scale/width (v)
  v10 <- aggregate(v, fact = 10, fun = mean)
  v20 <- aggregate(v, fact = 20, fun = mean)
  v50 <- aggregate(v, fact = 50, fun = mean)
  v100 <- aggregate(v, fact = 100, fun = mean)
  v200 <- aggregate(v, fact = 200, fun = mean)
  v300 <- aggregate(v, fact = 300, fun = mean)
  
# create raster for each km class
  n10 <- rasterize(xy, v10, field = spp, fun = function(x,...) {length(unique(na.omit(x))) })
  n20 <- rasterize(xy, v20, field = spp, fun = function(x,...) {length(unique(na.omit(x))) })
  n50 <- rasterize(xy, v50, field = spp, fun = function(x,...) {length(unique(na.omit(x))) })
  n100 <- rasterize(xy, v100, field = spp, fun = function(x,...) {length(unique(na.omit(x))) })
  n200 <- rasterize(xy, v200, field = spp, fun = function(x,...) {length(unique(na.omit(x))) })
  n300 <- rasterize(xy, v300, field = spp, fun = function(x,...) {length(unique(na.omit(x))) })
  
# Mask cells who's centres are contained in the continent
  aus_shp <- readOGR("C:/Users/s436862/Dropbox/Climate Matching/Data files/Australia/australia_shapefile.shp")
  
  n10 <- mask(n10, aus_shp)
  plot(n10)
  n20 <- mask(n20, aus_shp)
  plot(n20)
  n50 <- mask(n50, aus_shp)
  plot(n50)
  n100 <- mask(n100, aus_shp)
  plot(n100)
  n200 <- mask(n200, aus_shp)
  plot(n200)
  n300 <- mask(n300, aus_shp)
  plot(n300)
  
# shift to dataframe  
  n10_n <- getValues(n10)
  n20_n <- getValues(n20)
  n50_n <- getValues(n50)
  n100_n <- getValues(n100)
  n200_n <- getValues(n200)
  n300_n <- getValues(n300)
  
# repeat for arid raster (basis for removing offshore cells)
  v10_n <- getValues(v10)
  v20_n <- getValues(v20)
  v50_n <- getValues(v50)
  v100_n <- getValues(v100)
  v200_n <- getValues(v200)
  v300_n <- getValues(v300)
  
# save rasters  
  setwd("C:/Users/s436862/Dropbox/Rarefaction/Results/Rarefaction/Rasters/Raw species richness multiscale")
  
  writeRaster(n10, "Introduced 10-km", overwrite = T)
  writeRaster(n20, "Introduced 20-km", overwrite = T)
  writeRaster(n50, "Introduced 50-km", overwrite = T)
  writeRaster(n100, "Introduced 100-km", overwrite = T)
  writeRaster(n200, "Introduced 200-km", overwrite = T)
  writeRaster(n300, "Introduced 300-km", overwrite = T)
  
# Raster plot function ---------------------------------------------------------------
# same as previous
  
  setwd("C:/Users/s436862/Dropbox/Rarefaction/Results/Rarefaction")
  
  eng_ras <- function (raster, legend, save)  
    
  {
  # AUS border
    oz1 <- borders("world", region = "Australia", fill = "grey60")
    oz2 <- borders("world", region = "Australia", colour = "black")
    
  # Colour palette for legend
    colour <- rev(brewer.pal(11, "Spectral"))
  # display.brewer.all() 
  # (for more info)
    
  # Plot
    q <- gplot(raster) + 
      theme_map()+
      oz1 +
      geom_raster(aes(fill = value)) +
      scale_fill_gradientn(colours = colour, 
                           space = "Lab",
                           na.value = "transparent",
                           guide = "colourbar",
                           name = legend) + 
      coord_equal() +
      coord_cartesian(xlim = c(112, 155), ylim = c(-45, -7), expand = F) +
      theme(legend.justification = "centre",
            legend.position = "right",
            aspect.ratio = 0.88) +
      oz2
    print(q)
    
    ggsave(save, plot = last_plot(), scale = 1, dpi = 500, device = "jpeg")
    
  } # finish function
# 10-km plot ----------------------------------------------  
  raster <- raster("Rasters/Raw records multiscale/Introduced 10-km")
  legend <- "Species richness"
  save <- "Graphs/Raw species richness multiscale/Introduced 10-km.jpeg"
  
  eng_ras(raster, legend, save)
  
  
# 20-km plot ----------------------------------------------  
  raster <- raster("Rasters/Raw species richness multiscale/Introduced 20-km")
  legend <- "Species richness"
  save <- "Graphs/Raw species richness multiscale/Introduced 20-km.jpeg"
  
  eng_ras(raster, legend, save)
  
  
# 50-km plot ------------------------------------------------
  raster <- raster("Rasters/Raw species richness multiscale/Introduced 50-km")
  legend <- "Species richness"
  save <- "Graphs/Raw species richness multiscale/Introduced 50-km.jpeg"
  
  eng_ras(raster, legend, save)
  
# 100-km plot ----------------------------------------------  
  raster <- raster("Rasters/Raw species richness multiscale/Introduced 100-km")
  legend <- "Species richness"
  save <- "Graphs/Raw species richness multiscale/Introduced 100-km.jpeg"
  
  eng_ras(raster, legend, save)
  
# 200-km plot ----------------------------------------------  
  raster <- raster("Rasters/Raw species richness multiscale/Introduced 200-km")
  legend <- "Species richness"
  save <- "Graphs/Raw species richness multiscale/Introduced 200-km.jpeg"
  
  eng_ras(raster, legend, save)
  
  
# 300-km plot ----------------------------------------------  
  raster <- raster("Rasters/Raw species richness multiscale/Introduced 300-km")
  legend <- "Species richness"
  save <- "Graphs/Raw species richness multiscale/Introduced 300-km.jpeg"
  
  eng_ras(raster, legend, save)
  
  
# ------------------------------------------------------------------------------------- 
  
# 3.5 Total (native + exotic) records ---------------------------------------------------------------------------------
# Record-number per cell rasters at 10- 50-, 100-, 200- & 300-km ------------------
# Data -------------------------------------------------------------------------------
  setwd("C:/Users/s436862/Dropbox/Climate matching/Data files")
  
# Species 
  x <- read.csv("Osborne C3-C4/AVH grass pp.csv", header = T) 
  
# raster (1-km scale)
  v <- raster("C:/Users/s436862/Dropbox/Climate Matching/Data files/Australia/aus")
  
# put records into cells    
  xy <- cbind(x$long, x$lat) 
  spp <- as.numeric(factor(x$species))
  
# scale/width (v)
  v10 <- aggregate(v, fact = 10, fun = mean)
  v20 <- aggregate(v, fact = 20, fun = mean)
  v50 <- aggregate(v, fact = 50, fun = mean)
  v100 <- aggregate(v, fact = 100, fun = mean)
  v200 <- aggregate(v, fact = 200, fun = mean)
  v300 <- aggregate(v, fact = 300, fun = mean)
  
# create raster for each km class
  n10 <- rasterize(xy, v10, fun = function(x,...) {length(na.omit(x)) })
  n20 <- rasterize(xy, v20, fun = function(x,...) {length(na.omit(x)) })
  n50 <- rasterize(xy, v50, fun = function(x,...) {length(na.omit(x)) })
  n100 <- rasterize(xy, v100, fun = function(x,...) {length(na.omit(x)) })
  n200 <- rasterize(xy, v200, fun = function(x,...) {length(na.omit(x)) })
  n300 <- rasterize(xy, v300, fun = function(x,...) {length(na.omit(x)) })
  
# Mask cells who's centres are contained in the continent
  aus_shp <- readOGR("C:/Users/s436862/Dropbox/Climate Matching/Data files/Australia/australia_shapefile.shp")
  
  n10 <- mask(n10, aus_shp)
  plot(n10)
  n20 <- mask(n20, aus_shp)
  plot(n20)
  n50 <- mask(n50, aus_shp)
  plot(n50)
  n100 <- mask(n100, aus_shp)
  plot(n100)
  n200 <- mask(n200, aus_shp)
  plot(n200)
  n300 <- mask(n300, aus_shp)
  plot(n300)
  
# shift to dataframe  
  n10_n <- getValues(n10)
  n20_n <- getValues(n20)
  n50_n <- getValues(n50)
  n100_n <- getValues(n100)
  n200_n <- getValues(n200)
  n300_n <- getValues(n300)
  
# repeat for arid raster (basis for removing offshore cells)
  v10_n <- getValues(v10)
  v20_n <- getValues(v20)
  v50_n <- getValues(v50)
  v100_n <- getValues(v100)
  v200_n <- getValues(v200)
  v300_n <- getValues(v300)
  
# save rasters  
  setwd("C:/Users/s436862/Dropbox/Rarefaction/Results/Rarefaction/Rasters/Raw records multiscale")
  
  writeRaster(n10, "Poa 10-km", overwrite = T)
  writeRaster(n20, "Poa 20-km", overwrite = T)
  writeRaster(n50, "Poa 50-km", overwrite = T)
  writeRaster(n100, "Poa 100-km", overwrite = T)
  writeRaster(n200, "Poa 200-km", overwrite = T)
  writeRaster(n300, "Poa 300-km", overwrite = T)
  
# Raster plot function ---------------------------------------------------------------
# same as previous
  
  setwd("C:/Users/s436862/Dropbox/Rarefaction/Results/Rarefaction")
  
  eng_ras <- function (raster, legend, save)  
    
  {
  # AUS border
    oz1 <- borders("world", region = "Australia", fill = "grey60")
    oz2 <- borders("world", region = "Australia", colour = "black")
    
  # Colour palette for legend
    colour <- rev(brewer.pal(11, "Spectral"))
  # display.brewer.all() 
  # (for more info)
    
  # Plot
    q <- gplot(raster) + 
      theme_map()+
      oz1 +
      geom_raster(aes(fill = value)) +
      scale_fill_gradientn(colours = colour, 
                           space = "Lab",
                           na.value = "transparent",
                           guide = "colourbar",
                           name = legend) + 
      coord_equal() +
      coord_cartesian(xlim = c(112, 155), ylim = c(-45, -7), expand = F) +
      theme(legend.justification = "centre",
            legend.position = "right",
            aspect.ratio = 0.88) +
      oz2
    print(q)
    
    ggsave(save, plot = last_plot(), scale = 1, dpi = 500, device = "jpeg")
    
  } # finish function
# 10-km plot ----------------------------------------------  
  raster <- raster("Rasters/Raw records multiscale/Poa 10-km")
  legend <- "Record number"
  save <- "Graphs/Raw records multiscale/Poa 10-km.jpeg"
  
  eng_ras(raster, legend, save)
  
  
# 20-km plot ----------------------------------------------  
  raster <- raster("Rasters/Raw records multiscale/Poa 20-km")
  legend <- "Record number"
  save <- "Graphs/Raw records multiscale/Poa 20-km.jpeg"
  
  eng_ras(raster, legend, save)
  
  
# 50-km plot ------------------------------------------------
  raster <- raster("Rasters/Raw records multiscale/Poa 50-km")
  legend <- "Record number"
  save <- "Graphs/Raw records multiscale/Poa 50-km.jpeg"
  
  eng_ras(raster, legend, save)
  
# 100-km plot ----------------------------------------------  
  raster <- raster("Rasters/Raw records multiscale/Poa 100-km")
  legend <- "Record number"
  save <- "Graphs/Raw records multiscale/Poa 100-km.jpeg"
  
  eng_ras(raster, legend, save)
  
# 200-km plot ----------------------------------------------  
  raster <- raster("Rasters/Raw records multiscale/Poa 200-km")
  legend <- "Record number"
  save <- "Graphs/Raw records multiscale/Poa 200-km.jpeg"
  
  eng_ras(raster, legend, save)
  
  
# 300-km plot ----------------------------------------------  
  raster <- raster("Rasters/Raw records multiscale/Poa 300-km")
  legend <- "Record number"
  save <- "Graphs/Raw records multiscale/Poa 300-km.jpeg"
  
  eng_ras(raster, legend, save)
  
  
# -------------------------------------------------------------------------------------
  
# 3.6 Total (native + exotic) species richness -----------------------------------------------------
# Richness per cell rasters at 10- 50-, 100-, 200- & 300-km ------------------
# Data -------------------------------------------------------------------------------
  setwd("C:/Users/s436862/Dropbox/Climate matching/Data files")
  
# Species 
  x <- read.csv("Osborne C3-C4/AVH grass pp.csv", header = T)
  
# raster (1-km scale)
  v <- raster("C:/Users/s436862/Dropbox/Climate Matching/Data files/Australia/aus")
  
# put records into cells    
  xy <- cbind(x$long, x$lat) 
  spp <- as.numeric(factor(x$species))
  
# scale/width (v)
  v10 <- aggregate(v, fact = 10, fun = mean)
  v20 <- aggregate(v, fact = 20, fun = mean)
  v50 <- aggregate(v, fact = 50, fun = mean)
  v100 <- aggregate(v, fact = 100, fun = mean)
  v200 <- aggregate(v, fact = 200, fun = mean)
  v300 <- aggregate(v, fact = 300, fun = mean)
  
# create raster for each km class
  n10 <- rasterize(xy, v10, field = spp, fun = function(x,...) {length(unique(na.omit(x))) })
  n20 <- rasterize(xy, v20, field = spp, fun = function(x,...) {length(unique(na.omit(x))) })
  n50 <- rasterize(xy, v50, field = spp, fun = function(x,...) {length(unique(na.omit(x))) })
  n100 <- rasterize(xy, v100, field = spp, fun = function(x,...) {length(unique(na.omit(x))) })
  n200 <- rasterize(xy, v200, field = spp, fun = function(x,...) {length(unique(na.omit(x))) })
  n300 <- rasterize(xy, v300, field = spp, fun = function(x,...) {length(unique(na.omit(x))) })
  
# Mask cells who's centres are contained in the continent
  aus_shp <- readOGR("C:/Users/s436862/Dropbox/Climate Matching/Data files/Australia/australia_shapefile.shp")
  
  n10 <- mask(n10, aus_shp)
  plot(n10)
  n20 <- mask(n20, aus_shp)
  plot(n20)
  n50 <- mask(n50, aus_shp)
  plot(n50)
  n100 <- mask(n100, aus_shp)
  plot(n100)
  n200 <- mask(n200, aus_shp)
  plot(n200)
  n300 <- mask(n300, aus_shp)
  plot(n300)
  
# shift to dataframe  
  n10_n <- getValues(n10)
  n20_n <- getValues(n20)
  n50_n <- getValues(n50)
  n100_n <- getValues(n100)
  n200_n <- getValues(n200)
  n300_n <- getValues(n300)
  
# repeat for arid raster (basis for removing offshore cells)
  v10_n <- getValues(v10)
  v20_n <- getValues(v20)
  v50_n <- getValues(v50)
  v100_n <- getValues(v100)
  v200_n <- getValues(v200)
  v300_n <- getValues(v300)
  
# save rasters  
  setwd("C:/Users/s436862/Dropbox/Rarefaction/Results/Rarefaction/Rasters/Raw species richness multiscale")
  
  writeRaster(n10, "Poa 10-km", overwrite = T)
  writeRaster(n20, "Poa 20-km", overwrite = T)
  writeRaster(n50, "Poa 50-km", overwrite = T)
  writeRaster(n100, "Poa 100-km", overwrite = T)
  writeRaster(n200, "Poa 200-km", overwrite = T)
  writeRaster(n300, "Poa 300-km", overwrite = T)
  
# Raster plot function ---------------------------------------------------------------
# same as previous
  
  setwd("C:/Users/s436862/Dropbox/Rarefaction/Results/Rarefaction")
  
  eng_ras <- function (raster, legend, save)  
    
  {
  # AUS border
    oz1 <- borders("world", region = "Australia", fill = "grey60")
    oz2 <- borders("world", region = "Australia", colour = "black")
    
  # Colour palette for legend
    colour <- rev(brewer.pal(11, "Spectral"))
  # display.brewer.all() 
  # (for more info)
    
  # Plot
    q <- gplot(raster) + 
      theme_map()+
      oz1 +
      geom_raster(aes(fill = value)) +
      scale_fill_gradientn(colours = colour, 
                           space = "Lab",
                           na.value = "transparent",
                           guide = "colourbar",
                           name = legend) + 
      coord_equal() +
      coord_cartesian(xlim = c(112, 155), ylim = c(-45, -7), expand = F) +
      theme(legend.justification = "centre",
            legend.position = "right",
            aspect.ratio = 0.88) +
      oz2
    print(q)
    
    ggsave(save, plot = last_plot(), scale = 1, dpi = 500, device = "jpeg")
    
  } # finish function
# 10-km plot ----------------------------------------------  
  raster <- raster("Rasters/Raw records multiscale/Poa 10-km")
  legend <- "Species richness"
  save <- "Graphs/Raw species richness multiscale/Poa 10-km.jpeg"
  
  eng_ras(raster, legend, save)
  
  
# 20-km plot ----------------------------------------------  
  raster <- raster("Rasters/Raw species richness multiscale/Poa 20-km")
  legend <- "Species richness"
  save <- "Graphs/Raw species richness multiscale/Poa 20-km.jpeg"
  
  eng_ras(raster, legend, save)
  
  
# 50-km plot ------------------------------------------------
  raster <- raster("Rasters/Raw species richness multiscale/Poa 50-km")
  legend <- "Species richness"
  save <- "Graphs/Raw species richness multiscale/Poa 50-km.jpeg"
  
  eng_ras(raster, legend, save)
  
# 100-km plot ----------------------------------------------  
  raster <- raster("Rasters/Raw species richness multiscale/Poa 100-km")
  legend <- "Species richness"
  save <- "Graphs/Raw species richness multiscale/Poa 100-km.jpeg"
  
  eng_ras(raster, legend, save)
  
# 200-km plot ----------------------------------------------  
  raster <- raster("Rasters/Raw species richness multiscale/Poa 200-km")
  legend <- "Species richness"
  save <- "Graphs/Raw species richness multiscale/Poa 200-km.jpeg"
  
  eng_ras(raster, legend, save)
  
  
# 300-km plot ----------------------------------------------  
  raster <- raster("Rasters/Raw species richness multiscale/Poa 300-km")
  legend <- "Species richness"
  save <- "Graphs/Raw species richness multiscale/Poa 300-km.jpeg"
  
  eng_ras(raster, legend, save)

  
# 3.7 Record-richness correlations ----------------------------------------------------
# Data --------------------------------------------------------------------------------
# Total record and richness rasters to a dataframe
  setwd("C:/Users/s436862/Dropbox/Rarefaction/Results/Rarefaction")
  
# 10-km  
  sr.10 <- getValues(raster("Rasters/Raw species richness multiscale/Poa 10-km"))
  rec.10 <- getValues(raster("Rasters/Raw records multiscale/Poa 10-km"))
  
# 20-km  
  sr.20 <- getValues(raster("Rasters/Raw species richness multiscale/Poa 20-km"))
  rec.20 <- getValues(raster("Rasters/Raw records multiscale/Poa 20-km"))
  
# 50-km  
  sr.50 <- getValues(raster("Rasters/Raw species richness multiscale/Poa 50-km"))
  rec.50 <- getValues(raster("Rasters/Raw records multiscale/Poa 50-km"))
  
# 100-km  
  sr.100 <- getValues(raster("Rasters/Raw species richness multiscale/Poa 100-km"))
  rec.100 <- getValues(raster("Rasters/Raw records multiscale/Poa 100-km"))
  
# 200-km  
  sr.200 <- getValues(raster("Rasters/Raw species richness multiscale/Poa 200-km"))
  rec.200 <- getValues(raster("Rasters/Raw records multiscale/Poa 200-km"))
  
# 300-km  
  sr.300 <- getValues(raster("Rasters/Raw species richness multiscale/Poa 300-km"))
  rec.300 <- getValues(raster("Rasters/Raw records multiscale/Poa 300-km"))
  
  
# Raster template
  v <- raster("C:/Users/s436862/Dropbox/Climate Matching/Data files/EFs/Rasters 1-km Aus-cropped/arid")

# Shapefile
  oz <- readOGR("C:/Users/s436862/Dropbox/Climate Matching/Data files/Australia/australia_shapefile.shp")
  
# scaled & cropped
  v.10 <- aggregate(v, fact = 10, fun = mean)
  v.10.m <- mask(v.10, oz)
  plot(v.10.m)
  v.10.c <- getValues(v.10.m)
  
  v.20 <- aggregate(v, fact = 20, fun = mean)
  v.20.m <- mask(v.20, oz)
  plot(v.20.m)
  v.20.c <- getValues(v.20.m)
  
  v.50 <- aggregate(v, fact = 50, fun = mean)
  v.50.m <- mask(v.50, oz)
  plot(v.50.m)
  v.50.c <- getValues(v.50.m)
  
  v.100 <- aggregate(v, fact = 100, fun = mean)
  v.100.m <- mask(v.100, oz)
  plot(v.100.m)
  v.100.c <- getValues(v.100.m)
  
  v.200 <- aggregate(v, fact = 200, fun = mean)
  v.200.m <- mask(v.200, oz)
  plot(v.200.m)
  v.200.c <- getValues(v.200.m)
  
  v.300 <- aggregate(v, fact = 300, fun = mean)
  v.300.m <- mask(v.300, oz)
  plot(v.300.m)
  v.300.c <- getValues(v.300.m)
  
# Cropped
  sr.10.c <- as.data.frame(cbind(v.10.c, sr.10)) %>%
    filter(!is.na(v.10.c)) %>%
    select(-v.10.c)
  
  sr.20.c <- as.data.frame(cbind(v.20.c, sr.20)) %>%
    filter(!is.na(v.20.c)) %>%
    select(-v.20.c)
  
  sr.50.c <- as.data.frame(cbind(v.50.c, sr.50)) %>%
    filter(!is.na(v.50.c)) %>%
    select(-v.50.c)
  
  sr.100.c <- as.data.frame(cbind(v.100.c, sr.100)) %>%
    filter(!is.na(v.100.c)) %>%
    select(-v.100.c)
  
  sr.200.c <- as.data.frame(cbind(v.200.c, sr.200)) %>%
    filter(!is.na(v.200.c)) %>%
    select(-v.200.c)
  
  sr.300.c <- as.data.frame(cbind(v.300.c, sr.300)) %>%
    filter(!is.na(v.300.c)) %>%
    select(-v.300.c)

# Community measures ------------------------------------------------------
# Correlations
  cor.10 <- cor(rec.10, sr.10, use = "na.or.complete")
  cor.20 <- cor(rec.20, sr.20, use = "na.or.complete")
  cor.50 <- cor(rec.50, sr.50, use = "na.or.complete")
  cor.100 <- cor(rec.100, sr.100, use = "na.or.complete")
  cor.200 <- cor(rec.200, sr.200, use = "na.or.complete")
  cor.300 <- cor(rec.300, sr.300, use = "na.or.complete")
  
# Range (max and min) species richness
  range.10 <- max(as.matrix(sr.10.c)) - min(as.matrix(sr.10.c))
  
  range.20 <- max(as.matrix(sr.20.c)) - min(as.matrix(sr.20.c))
  
  range.50 <- max(as.matrix(sr.50.c)) - min(as.matrix(sr.50.c))
  
  range.100 <- max(as.matrix(sr.100.c)) - min(as.matrix(sr.100.c))
  
  range.200 <- max(as.matrix(sr.200.c)) - min(as.matrix(sr.200.c))
  
  range.300 <- max(as.matrix(sr.300.c)) - min(as.matrix(sr.300.c))
  
# Coefficient of variation: SD/mean * 100
  cv.10 <- sd(as.matrix(sr.10.c))/mean(as.matrix(sr.10.c))*100
  cv.20 <- sd(as.matrix(sr.20.c))/mean(as.matrix(sr.20.c))*100
  cv.50 <- sd(as.matrix(sr.50.c))/mean(as.matrix(sr.50.c))*100
  cv.100 <- sd(as.matrix(sr.100.c))/mean(as.matrix(sr.100.c))*100
  cv.200 <- sd(as.matrix(sr.200.c))/mean(as.matrix(sr.200.c))*100
  cv.300 <- sd(as.matrix(sr.300.c))/mean(as.matrix(sr.300.c))*100
  
  
# Convert NAs to zeroes for other community measures
  sr.10.c[is.na(sr.10.c)] <- 0 
  sr.20.c[is.na(sr.20.c)] <- 0 
  sr.50.c[is.na(sr.50.c)] <- 0 
  sr.100.c[is.na(sr.100.c)] <- 0 
  sr.200.c[is.na(sr.200.c)] <- 0 
  sr.300.c[is.na(sr.300.c)] <- 0 
  
  sum(is.na(sr.10.c)) # beautiful
  
# Total cells
  tc.sr.10 <- nrow(sr.10.c)
  tc.sr.20 <- nrow(sr.20.c)
  tc.sr.50 <- nrow(sr.50.c)
  tc.sr.100 <- nrow(sr.100.c)
  tc.sr.200 <- nrow(sr.200.c)
  tc.sr.300 <- nrow(sr.300.c)
  
# Percent occupied (>0 SR)
  po.10 <- sum(sr.10.c > 0) / tc.sr.10 * 100 
  po.20 <- sum(sr.20.c > 0) / tc.sr.20 * 100 
  po.50 <- sum(sr.50.c > 0) / tc.sr.50 * 100 
  po.100 <- sum(sr.100.c > 0) / tc.sr.100 * 100 
  po.200 <- sum(sr.200.c > 0) / tc.sr.200 * 100 
  po.300 <- sum(sr.300.c > 0) / tc.sr.300 * 100 
  
# Save as data frame
  df <- matrix(nrow = 6, ncol = 5)
  colnames(df) <- c("cell.num", "coverage", "range", "cv", "cor")
  df <- data.frame(df)
  
  
# 4.1 Proportion of native cells occupied --------------------------------------------
# Aim -----------------------------------------------------------------------------
# decide where the appropriate ranges are for cell width & rarefaction cutoff points
# These are opposing forces: we want the highest 'cutoff' point (giving the biggest range of relative species richness between cells), but the lowest % of cells excluded, to have a good coverage of Australia
  
# This script is split into two sections because of the aggregate function: first is the 1 km dataframe, and second the 2-250 km one, with their merger at the end
  
  rm(list = ls())
  
  setwd("C:/Users/s436862/Dropbox/Rarefaction/Data files")
  
# 1.1 Record coverage across scales -------------------------------------  
# Note: add in 'mean correlation between cells' record# and HII at each cell width  
# 1.2 1km width dataframe ----------------------------------------------
# Aims: ----------------------------------------------------------------
# (1) compute 1 km cell width   
# (2) compute proportion of cells occupied (>0 records) at each width
# (3) compute proportion of cells occupied at different cell-record cutoffs (>25, >50, >100,>250, >500 records) at each width
# (4) compute the correlation between average record number and HII score for each scale

# Data ----------------------------------------------------
  setwd("C:/Users/s436862/Dropbox/Climate matching/Data files")
  
# Species 
  x <- read.csv("AVH/AVH grass records.csv", header = T) %>%
    dplyr::filter(status == "native")
  
# raster
  raster <- raster("EFs/3. EFs cropped bin/lu.grd")
  
# put records into cells    
  xy <- cbind(x$long, x$lat) 
  spp <- as.numeric(factor(x$species))
  
# Shapefile for masking oceanic/island values
  aus_shp <- readOGR("C:/Users/s436862/Dropbox/Climate Matching/Data files/Australia/australia_shapefile.shp")
  
# Cell-proportion function -------------------------
  prop <- function(width) 
  {
    
# aggregate
    raster_agg <- aggregate(raster, fact = width, fun = mean)
    
# number of records per cell (n)
    n <- rasterize(xy, raster_agg, fun = function(x,...) {length(na.omit(x)) })
    
    n.s <- mask(n, aus_shp)
    
    n <- n.s # I am too lazy to globally change
    
# double-check: use an EF to do the same thing
    b <- getValues(raster_agg)
    n_val <- getValues(n) 
    
    n_rec_na <- data.frame(b, n_val)
    n_rec_b <- n_rec_na[!is.na(b), ] 
    n_rec <- n_rec_b[ ,2]
    
# convert NAs to zeroes
    n_rec[is.na(n_rec)] <- 0 
    sum(is.na(n_rec)) # beautiful
    
# community estimates
# total cells
    total_cells <- length(n_rec)
# percent_occupied
    prop_occ <- sum(n_rec > 0) / total_cells * 100
# percent with 1 - 10 records
    one_ten <- sum(n_rec >= 25) / total_cells * 100
    fifty <- sum(n_rec >= 50) / total_cells * 100
    one_hundred <- sum(n_rec >= 100) / total_cells * 100
    two_fifty <- sum(n_rec >= 250) / total_cells * 100
    five_hundred <- sum(n_rec >= 500) / total_cells * 100 
    
# dataframe (8 cols)
    res <- as.data.frame(cbind(mean(n_rec), total_cells, prop_occ, twenty_five, fifty, one_hundred, two_fifty, five_hundred))                                                                                         
    return(res)                                                                    
    
  }#finish function
  
# Output ----------------------------
# scale range (2 - 250 km)
  width <- c(2, 3, 4, 6, 8, 12, 16, 22, 32, 50, 75, 100, 125, 175, 250)
  
  out <- matrix(nrow = length(width), ncol = 9) # res cols +1
  
  for(i in 1:length(width)) 
  {
    out[i, 1] <- width[i]
    out[i, 2:9] <- as.numeric(prop(width[i]))
  }
  
  out_multi_km <- out
  
  colnames(out_multi_km) <- c("cell_width", "mean_rec", "total_cells", "all_records", "twenty_five", "fifty", "one_hundred", "two_fifty", "five_hundred")   
# total: 8 + 1
  
# merge dataframes
  out_prop <- rbind(out_1km, out_multi_km)
  
# save 
  write.csv(out_prop, file = "C:/Users/s436862/Dropbox/Rarefaction/Results/Grass groups AVH/Rarefaction/CSV/Multiscale_rare_cell_occupation.csv", row.names = F)  
  
  
  
# ------------------------------------------------------------------   
  
  
  
# 4.0 Model coefficients --------------------------------------
# Nat/int rarefied to 15-records: total, C3 and C4 richness (from Rarefaction chapter model v5 [atm; 16/10])   
# Load model workspace (from Rarefaction chapter model v5)
  setwd("C:/Users/s436862/Dropbox/Rarefaction")
  load("Results/Rarefaction/Rdata/rarefaction_chapter_models.RData")

# C3-C4 ----------------------------------------
# Data frame with all photosynthetic classes in it
  levels(c3.rich$coef) <- c("PCOLDQ", "PWARMQ", "AMT", "TS", "ARID", "PEWC", "TH", "HII")
   
  levels(c4.rich$coef) <- c("PCOLDQ", "PWARMQ", "AMT", "TS", "ARID", "PEWC", "TH", "HII")
  
  levels(total.rich$coef) <- c("PCOLDQ", "PWARMQ", "AMT", "TS", "ARID", "PEWC", "TH", "HII")
 
# C3 -------------------------------------------------------------------
  h <- ggplot(c3.rich, aes(x = coef, color = status)) +
    theme_classic() +
    scale_color_manual(labels = c("Exotic", "Native"), values = c("blue", "red")) +
    geom_hline(aes(yintercept = 0),
               color = "black", size = 0.6) +
    labs(title = "", x = "Environmental variables", y = "Estimate", color = "grey100") +
    geom_point(aes(y = mean), size = 5, position = position_dodge(width = 1)) +
    geom_errorbar(aes(ymin = mean - lower.ci, ymax = mean + upper.ci),
                  size = 1, width = 0, position = position_dodge(width = 1)) +
    annotate("segment", x = -Inf, xend = Inf, y = -Inf, yend = -Inf) +
    annotate("segment", x = -Inf, xend = -Inf,y = -Inf, yend = Inf) + 
    annotate("text", x = 7, y = -5.5, label = "Adj. r2 = exotic 0.84, native 0.88")
  
  #ggThemeAssistGadget(h)
  
  h + theme(axis.text = element_text(size = 12, colour = "gray0"), 
            axis.text.x = element_text(size = 12), 
            axis.text.y = element_text(size = 12), 
            axis.title = element_text(size = 14),
            legend.text = element_text(size = 14), 
            legend.title = element_blank(), 
            axis.ticks.length = unit(0.2, "cm")) 
  
  ggsave("Results/Rarefaction/Graphs/Coefficients/C3 coefficients.jpeg", plot = last_plot(), scale = 1, dpi = 500, device = "jpeg")
  
# C4 -------------------------------------------------------------------
  i <- ggplot(c4.rich, aes(x = coef, color = status)) +
    theme_classic() +
    scale_color_manual(labels = c("Exotic", "Native"), values = c("blue", "red")) +
    geom_hline(aes(yintercept = 0),
               color = "black", size = 0.6) +
    labs(title = "", x = "Environmental variables", y = "Estimate", color = "grey100") +
    geom_point(aes(y = mean), size = 5, position = position_dodge(width = 1)) +
    geom_errorbar(aes(ymin = mean - lower.ci, ymax = mean + upper.ci),
                  size = 1, width = 0, position = position_dodge(width = 1)) +
    annotate("segment", x = -Inf, xend = Inf, y = -Inf, yend = -Inf) +
    annotate("segment", x = -Inf, xend = -Inf,y = -Inf, yend = Inf) +
    annotate("text", x = 7, y = 6, label = "Adj. r2 = exotic 0.69, native 0.82")
  
  i + theme(axis.text = element_text(size = 12, colour = "gray0"), 
            axis.text.x = element_text(size = 12), 
            axis.text.y = element_text(size = 12), 
            axis.title = element_text(size = 14),
            legend.text = element_text(size = 14), 
            legend.title = element_blank(), 
            axis.ticks.length = unit(0.2, "cm"))
  
  ggsave("Results/Rarefaction/Graphs/Coefficients/C4 coefficients.jpeg", plot = last_plot(), scale = 1, dpi = 500, device = "jpeg")  
  
# Total Nat-Int ------------------------------------
  j <- ggplot(total.rich, aes(x = coef, color = status)) +
    theme_classic() +
    scale_color_manual(labels = c("Exotic", "Native"), values = c("blue", "red")) +
    geom_hline(aes(yintercept = 0),
               color = "black", size = 0.6) +
    labs(title = "", x = "Environmental variables", y = "Estimate", color = "grey100") +
    geom_point(aes(y = mean), size = 5, position = position_dodge(width = 1)) +
    geom_errorbar(aes(ymin = mean - lower.ci, ymax = mean + upper.ci),
                  size = 1, width = 0, position = position_dodge(width = 1)) +
    annotate("segment", x = -Inf, xend = Inf, y = -Inf, yend = -Inf) +
    annotate("segment", x = -Inf, xend = -Inf,y = -Inf, yend = Inf) + 
    annotate("text", x = 7, y = -2, label = "Adj. r2 = exotic 0.58, native 0.29")
  
  j + theme(axis.text = element_text(size = 12, colour = "gray0"), 
            axis.text.x = element_text(size = 12), 
            axis.text.y = element_text(size = 12), 
            axis.title = element_text(size = 14),
            legend.text = element_text(size = 14), 
            legend.title = element_blank(), 
            axis.ticks.length = unit(0.2, "cm"))
  
  ggsave("Results/Rarefaction/Graphs/Coefficients/Total coefficients.jpeg", plot = last_plot(), scale = 1, dpi = 500, device = "jpeg")  
  
  
  
  

# ------------------------------------------------------------------------------------------    
  
# 5.0 Predicted distribution plots --------------------------------------------  
# Raster data
  setwd("Rasters/Predicted distributions")
  
  current.list <- list.files(pattern = ".grd")
  names <- gsub(pattern = "\\.grd$", "", current.list)
  
  c.stack <- stack(current.list)
  names(c.stack) <- names
  
  list2env(setNames(unstack(c.stack), names(c.stack)), .GlobalEnv)
  
# Raster plot function ---------------------------------------------------------------
# We are truncating over- and udner-predicted values tto between 0 and 15; therefore I will still scale it to 0 and 1, but can change this if need be.
  setwd("C:/Users/s436862/Dropbox/Rarefaction/Results/Rarefaction")
  
# function --------------------------------------------  
  eng_ras <- function (raster, legend, save)  
    
  {
    # AUS border
    oz1 <- borders("world", region = "Australia", fill = "grey60")
    oz2 <- borders("world", region = "Australia", colour = "black")
    
    # Colour palette for legend
    colour <- rev(brewer.pal(11, "Spectral"))
    # display.brewer.all() 
    # (for more info)
    
    # Plot
    q <- gplot(raster) + 
      theme_map()+
      oz1 +
      geom_raster(aes(fill = value)) +
      scale_fill_gradientn(colours = colour, 
                           space = "Lab",
                           limits = c(0, 1),
                           na.value = "transparent",
                           guide = "colourbar",
                           name = legend) + 
      coord_equal() +
      coord_cartesian(xlim = c(112, 155), ylim = c(-45, -7), expand = F) +
      theme(legend.justification = "centre",
            legend.position = "right",
            aspect.ratio = 0.88) +
      oz2
    print(q)
    
    ggsave(save, plot = last_plot(), scale = 1, dpi = 500, device = "jpeg")
    
  } # finish function


# 5.1 Predicted plots ----------------------------------------------  
# For each plot, set any cell values <0 --> 0 and >15 -> 15
  
# Total native -- turncating upper values to 15
  t.n <- raster("Rasters/Predicted distributions/n.tot.predicted.grd")
  t.n.calc <- calc(t.n, fun = function(x) {x[x>15] <- 15; return(x)})/15
  plot(t.n.calc)
 
  raster <- t.n.calc
  legend <- "Species richness"
  save <- "Graphs/Predicted distributions/Native total.jpeg"
  
  eng_ras(raster, legend, save)
  
# Total exotic 
  t.e <- raster("Rasters/Predicted distributions/e.tot.predicted.grd")
  t.e.calc <- calc(t.e, fun = function(x) {x[x>15] <- 15; return(x)})/15
  
  raster <- t.e.calc
  legend <- "Species richness"
  save <- "Graphs/Predicted distributions/Exotic total.jpeg"
  
  eng_ras(raster, legend, save)
  
# C3 native -- truncate lower and uppee values (0-15 -> 0-1) 
  c3.n <- raster("Rasters/Predicted distributions/n.c3.predicted.grd")
  c3.n.calc <- calc(c3.n, fun = function(x) {x[x<0] <- 0; x[x>15] <- 15; return(x)})/15
  
  raster <- c3.n.calc
  legend <- "Species richness"
  save <- "Graphs/Predicted distributions/Native C3.jpeg"
  
  eng_ras(raster, legend, save)
  
# C3 exotic 
  c3.e <- raster("Rasters/Predicted distributions/e.c3.predicted.grd")
  c3.e.calc <- calc(c3.e, fun = function(x) {x[x<0] <- 0; x[x>15] <- 15; return(x)})/15
  
  raster <- c3.e.calc
  legend <- "Species richness"
  save <- "Graphs/Predicted distributions/Exotic C3.jpeg"
  
  eng_ras(raster, legend, save)
  
# C4 native
  c4.n <- raster("Rasters/Predicted distributions/n.c4.predicted.grd")
  c4.n.calc <- calc(c4.n, fun = function(x) {x[x<0] <- 0; x[x>15] <- 15; return(x)})/15
  
  raster <- c4.n.calc
  legend <- "Species richness"
  save <- "Graphs/Predicted distributions/Native C4.jpeg"
  
  eng_ras(raster, legend, save)
  
# C4 exotic
  c4.e <- raster("Rasters/Predicted distributions/e.c4.predicted.grd")
  c4.e.calc <- calc(c4.e, fun = function(x) {x[x<0] <- 0; x[x>15] <- 15; return(x)})/15
  
  raster <- c4.e.calc
  legend <- "Species richness"
  save <- "Graphs/Predicted distributions/Exotic C4.jpeg"
  
  eng_ras(raster, legend, save) 

# 5.2 Predicted native - predicted exotic plots ------------------------------------  
# Plots to describe the potential for Australia to support higher numbers of species richness than it does currently, using that lovely template supplied by the natives

# Total "potential" (pot)
  t.pot <- t.n.calc - t.e.calc
  raster <- t.pot
  legend <- "Species richness"
  save <- "Graphs/Predicted distributions/Potential distribution total.jpeg"
  
  eng_ras(raster, legend, save) 
  
# C3 potential 
  c3.pot <- c3.n.calc - c3.e.calc
# values 0< constrained to 0 again 
  c3.pot.calc <- calc(c3.pot, fun = function(x) {x[x<0] <- 0; return(x)})
  raster <- c3.pot.calc
  legend <- "Species richness"
  save <- "Graphs/Predicted distributions/Potential distribution C3.jpeg"
  
  eng_ras(raster, legend, save) 
  
# C4 potential 
  c4.pot <- c4.n.calc - c4.e.calc
# values 0< constrained to 0 again 
  c4.pot.calc <- calc(c4.pot, fun = function(x) {x[x<0] <- 0; return(x)})
  raster <- c4.pot.calc
  legend <- "Species richness"
  save <- "Graphs/Predicted distributions/Potential distribution C4.jpeg"
  
  eng_ras(raster, legend, save) 
  
  
# 5.3 Observed/predicted scatterplots ------------------------------------------
# How these two correlate, specifically the observed versus predicted richness
  setwd("C:/Users/s436862/Dropbox/Rarefaction")
  
# Species data ------------------------------------------------------------
# o = observed, p = predicted; e = exotic, n = native; C3/C4 = pp
  
# Observed species distributions
  o.n.c3 <- raster("Results/Rarefaction/Rasters/Rarefied rasters/r15.n.c3.grd")
  plot(o.n.c3)
  o.n.c3.df <- getValues(o.n.c3)
  
  o.e.c3 <- raster("Results/Rarefaction/Rasters/Rarefied rasters/r15.i.c3.grd")
  plot(o.e.c3)
  o.e.c3.df <- getValues(o.e.c3)
  
  o.n.c4 <- raster("Results/Rarefaction/Rasters/Rarefied rasters/r15.n.c4.grd")
  plot(o.n.c4)
  o.n.c4.df <- getValues(o.n.c4)
  
  o.e.c4 <- raster("Results/Rarefaction/Rasters/Rarefied rasters/r15.i.c4.grd")
  plot(o.e.c4)
  o.e.c4.df <- getValues(o.e.c4)
  
# Predicted species distributions
  setwd("C:/Users/s436862/Dropbox/Rarefaction/Results/Rarefaction/Rasters/Predicted distributions")
   
  p.n.c3 <- raster("n.c3.predicted.grd")
  plot(p.n.c3)
  p.n.c3.df <- getValues(p.n.c3)
  
  p.e.c3 <- raster("e.c3.predicted.grd")
  plot(p.e.c3)
  p.e.c3.df <- getValues(p.e.c3)
  
  p.n.c4 <- raster("n.c4.predicted.grd")
  plot(p.n.c4)
  p.n.c4.df <- getValues(p.n.c4)
  
  p.e.c4 <- raster("e.c4.predicted.grd")
  plot(p.e.c4)
  p.e.c4.df <- getValues(p.e.c4)

# merge 
  spp <- cbind(o.n.c3.df, o.n.c4.df, o.e.c3.df, o.e.c4.df,
               p.n.c3.df, p.n.c4.df, p.e.c3.df, p.e.c4.df)
  
  spp.df <- data.frame(spp)
  
# truncating values below zero and above 15  
  trunc <- function(x) {x[x<0] <- 0; x[x>15] <- 15; return(x)} 
  spp.df.trunc <- trunc(spp.df) # that worked??
    
# Scatterplot -------------------------------------------------------
# Interested in how observed native correlates with predicted exotic, but will plot all combinations
  setwd("C:/Users/s436862/Dropbox/Rarefaction/Results/Rarefaction/Graphs/Predicted scatterplots")

# Scatterlot function ---------------------------------------------------------------  
# Requires: x, y, xlab, ylab, save
  scat_fun <- function (x, y, xlab, ylab, save)  {
    a <- ggplot(aes(x = x, y = y), data = spp.df.trunc) +
      geom_point(size = 1.5) +
      theme_bw() + 
      labs(x = xlab,
           y = ylab) +
      theme(axis.title = element_text(size = 14)) +
      geom_abline(slope = 1, intercept = 0, size = 1)
    ggsave(save, plot = last_plot(), dpi = 500, device = "jpeg")
    
    a
  } # function end
  
  ggplot(nmmaps, aes(temp, death))+geom_point(color="firebrick")+
    stat_smooth(method="lm", se=FALSE)
  
# Native obs C3 vs. native pred C3 --------------------------------------------------
  scat_fun(o.n.c3.df, 
           p.n.c3.df, 
           "Observed native C3", 
           "Predicted native C3", 
           "Observed native C3-Predicted native C3.jpeg")
  
# Native obs C4 vs. native pred C4 
  scat_fun(o.n.c4.df, 
           p.n.c4.df, 
           "Observed native C4", 
           "Predicted native C4", 
           "Observed native C4-Predicted native C4.jpeg")
  
# Native obs C3 vs. exotic pred C3
  scat_fun(o.n.c3.df, 
           p.e.c3.df, 
           "Observed native C3", 
           "Predicted exotic C3", 
           "Observed native C3-Predicted exotic C3.jpeg")
  
  
# Native obs C4 vs. exotic pred C4
  scat_fun(o.n.c4.df, 
           p.e.c4.df, 
           "Observed native C4", 
           "Predicted exotic C4", 
           "Observed native C4-Predicted exotic C4.jpeg") 
  
# Native pred C4 vs. exotic pred C3
  scat_fun(p.n.c3.df, 
           p.e.c3.df, 
           "Predicted native C3", 
           "Predicted exotic C3", 
           "Predicted native C3-Predicted exotic C3.jpeg") 
  
# Native pred C4 vs. exotic pred C4
  scat_fun(p.n.c4.df, 
           p.e.c4.df, 
           "Predicted native C4", 
           "Predicted exotic C4", 
           "Predicted native C4-Predicted exotic C4.jpeg") 
  

  
