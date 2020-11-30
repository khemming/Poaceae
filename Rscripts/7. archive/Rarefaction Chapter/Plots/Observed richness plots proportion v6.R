
# Date created: 18/4
# Last modified: 12/3/19 (updated using outputs from rarefaction v7)

# Rarefaction Paper rasters and other plots

# Based on: 'Rarefaction df + rasters' and 'Rarefaction plots' and Engemann's results flow

# v4 = cut a lot out -- see v2 for complete set.

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
  
# 2.0 Observed rarefaction 10- to 50- records x Nat/Exo x total/C3/C4 ---------------------------------
# required -----------------------------------------------------------------------------------
# (1) raster
# (2) file save name (e.g. Graphs/Native SR 50-km.jpeg)  
# (3) title: will pop up in the top left hand corner of the plot

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
                           limits = c(0, 1), # update
                           space = "Lab",
                           na.value = "transparent",
                           guide = "colourbar",
                           name = "Relative\nrichness") + 
      coord_equal() +
      coord_cartesian(xlim = c(112, 155), ylim = c(-45, -7), expand = F) +
      theme(legend.justification = "centre",
            legend.position = "right",
            aspect.ratio = 0.88) +
      oz2
    print(q)
    
    ggsave(save, plot = last_plot(), scale = 1, dpi = 500, device = "jpeg")
    
  } # finish function
    
# data ------------------------------------------------
  setwd("C:/Users/s436862/Dropbox/Rarefaction/Results/Rarefaction/Rasters/Rarefied rasters")
  
  current.list <- list.files(pattern = ".grd")
  names <- gsub(pattern = "\\.grd$", "", current.list)
  
  c.stack <- stack(current.list)
  names(c.stack) <- names

  list2env(setNames(unstack(c.stack), names(c.stack)), .GlobalEnv)
    
# Run function ------------------------------------------------
# required -----------------------------------------------------------------------------------
# (1) raster
# (2) file save name (e.g. Graphs/Native SR 50-km.jpeg)  
# (3) title: will pop up in the top left hand corner of the plot
  
  setwd("C:/Users/s436862/Dropbox/Rarefaction/Results/Rarefaction/Graphs/Rarefied richness")
  
# r10.e.c3
  eng_ras(r10.e.c3/10, "r10.e.c3.jpeg")
# r10.e.c4  
  eng_ras(r10.e.c4/10, "r10.e.c4.jpeg")
# r10.e.tot
  eng_ras(r10.e.tot/10, "r10.e.tot.jpeg")
  
# r10.n.c3  
  eng_ras(r10.n.c3/10, "r10.n.c3.jpeg")
# r10.n.c4  
  eng_ras(r10.n.c4/10, "r10.n.c4.jpeg")
# r10.n.tot 
  eng_ras(r10.n.tot/10, "r10.n.tot.jpeg")
  
# r15.e.c3
  eng_ras(r15.e.c3/15, "r15.e.c3.jpeg")
# r15.e.c4  
  eng_ras(r15.e.c4/15, "r15.e.c4.jpeg")
# r15.e.tot 
  eng_ras(r15.e.tot/15, "r15.e.tot.jpeg")

# r15.n.c3  
  eng_ras(r15.n.c3/15, "r15.n.c3.jpeg")
# r15.n.c4  
  eng_ras(r15.n.c4/15, "r15.n.c4.jpeg")
# r15.n.tot 
  eng_ras(r15.n.tot/15, "r15.n.tot.jpeg")

# r20.e.c3  
  eng_ras(r20.e.c3/20, "r20.e.c3.jpeg")
# r20.e.c4  
  eng_ras(r20.e.c4/20, "r20.e.c4.jpeg")
# r20.e.tot
  eng_ras(r20.e.tot/20, "r20.e.tot.jpeg")

# r20.n.c3 
  eng_ras(r20.n.c3/20, "r20.n.c3.jpeg")
# r20.n.c4  
  eng_ras(r20.n.c4/20, "r20.n.c4.jpeg")
# r20.n.tot 
  eng_ras(r20.n.tot/20, "r20.n.tot.jpeg")

# r25.e.c3  
  eng_ras(r25.e.c3/25, "r25.e.c3.jpeg")
# r25.e.c4  
  eng_ras(r25.e.c4/25, "r25.e.c4.jpeg")
# r25.e.tot 
  eng_ras(r25.e.tot/25, "r25.e.tot.jpeg")

# r25.n.c3  
  eng_ras(r25.n.c3/25, "r25.n.c3.jpeg")
# r25.n.c4  
  eng_ras(r25.n.c4/25, "r25.n.c4.jpeg")
# r25.n.tot
  eng_ras(r25.n.tot/25, "r25.n.tot3.jpeg")

# r30.e.c3  
  eng_ras(r30.e.c3/30, "r30.e.c3.jpeg")
# r30.e.c4  
  eng_ras(r30.e.c4/30, "r30.e.c4.jpeg")
# r30.e.tot 
  eng_ras(r30.e.tot/30, "r30.e.tot.jpeg")

# r30.n.c3  
  eng_ras(r30.n.c3/30, "r30.n.c3.jpeg")
# r30.n.c4  
  eng_ras(r30.n.c4/30, "r30.n.c4.jpeg")
# r30.n.tot 
  eng_ras(r30.n.tot/30, "r30.n.tot.jpeg")

# r35.e.c3  
  eng_ras(r35.e.c3/35, "r35.e.c3.jpeg")
# r35.e.c4  
  eng_ras(r35.e.c4/35, "r35.e.c4.jpeg")
# r35.e.tot
  eng_ras(r35.e.tot/35, "r35.e.tot.jpeg")

# r35.n.c3  
  eng_ras(r35.n.c3/35, "r35.n.c3.jpeg")
# r35.n.c4  
  eng_ras(r35.n.c4/35, "r35.n.c4.jpeg")
# r35.n.tot 
  eng_ras(r35.n.tot/35, "r35.n.tot.jpeg")

# r40.e.c3  
  eng_ras(r40.e.c3/40, "r40.e.c3.jpeg")
# r40.e.c4  
  eng_ras(r40.e.c4/40, "r40.e.c4.jpeg")
# r40.e.tot 
  eng_ras(r40.e.tot/40, "r40.e.tot.jpeg")

# r40.n.c3  
  eng_ras(r40.n.c3/40, "r40.n.c3.jpeg")
# r40.n.c4  
  eng_ras(r40.n.c4/40, "r40.n.c4.jpeg")
# r40.n.tot
  eng_ras(r40.n.tot/40, "r40.n.tot.jpeg")
  
# r45.e.c3  
  eng_ras(r45.e.c3/45, "r45.e.c3.jpeg")
# r45.e.c4  
  eng_ras(r45.e.c4/45, "r45.e.c4.jpeg")
# r45.e.tot 
  eng_ras(r45.e.tot/45, "r45.e.tot.jpeg")

# r45.n.c3  
  eng_ras(r45.n.c3/45, "r45.n.c3.jpeg")
# r45.n.c4  
  eng_ras(r45.n.c4/45, "r45.n.c4.jpeg")
# r45.n.tot 
  eng_ras(r45.n.tot/45, "r45.n.tot.jpeg")

# r50.e.c3  
  eng_ras(r50.e.c3/50, "r50.e.c3.jpeg")
# r50.e.c4  
  eng_ras(r50.e.c4/50, "r50.e.c4.jpeg")
# r50.e.tot
  eng_ras(r50.e.tot/50, "r50.e.tot.jpeg")
 
# r50.n.c3
  eng_ras(r50.n.c3/50, "r50.n.c3.jpeg")
# r50.n.c4 
  eng_ras(r50.n.c4/50, "r50.n.c4.jpeg")
# r50.n.tot
  eng_ras(r50.n.tot/50, "r50.n.tot.jpeg")
  
 
# ----------------------------------------------------------------  

# 3. 'Solo' raster plots (v8 rarefaction scripts) --------------------------
# required -----------------------------------------------------------------------------------
# (1) raster
# (2) file save name (e.g. Graphs/Native SR 50-km.jpeg)  
# (3) title: will pop up in the top left hand corner of the plot
  
# Rarefaction function ---------------------------------------------------------
# function   
  rm(list = ls())
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
                           limits = c(0, 1), # update
                           space = "Lab",
                           na.value = "transparent",
                           guide = "colourbar",
                           name = "Relative\nrichness") + 
      coord_equal() +
      coord_cartesian(xlim = c(112, 155), ylim = c(-45, -7), expand = F) +
      theme(legend.justification = "centre",
            legend.position = "right",
            aspect.ratio = 0.88) +
      oz2
    print(q)
    
    ggsave(save, plot = last_plot(), scale = 1, dpi = 500, device = "jpeg")
    
  } # finish function
  
# data ------------------------------------------------
  setwd("C:/Users/s436862/Dropbox/Rarefaction/Results/Rarefaction/Rasters/Rarefied richness solo")
  
  current.list <- list.files(pattern = ".grd")
  names <- gsub(pattern = "\\.grd$", "", current.list)
  
  c.stack <- stack(current.list)
  names(c.stack) <- names
  
  list2env(setNames(unstack(c.stack), names(c.stack)), .GlobalEnv)
  
# default raster ------------------------------------------------
# notes ---------------------------------------------------------
# I assume that for true absences of C3, C4.rec >=15 and C3.rec = Na/0
# but it's Na for when                   C4.rec >=15 and 0 < C3.rec < 15
# and still an Na for when               C4.rec < 15 and 0 < C3.rec < 15
# (and vise versa with C3 >= 15, etc.)
# ---------------------------------------------------------------
# converting Nas to zeroes (see notes for details)
  ras <- as.data.frame(c.stack, na.rm = F)
  
  ras$Native.C3.rich[ras$Native.C4.rec >= 15 & is.na(ras$Native.C3.rec) == T] <- 0
  ras$Native.C4.rich[ras$Native.C3.rec >= 15 & is.na(ras$Native.C4.rec) == T] <- 0
  
  ras$Exotic.C3.rich[ras$Exotic.C4.rec >= 15 & is.na(ras$Exotic.C3.rec) == T] <- 0
  ras$Exotic.C4.rich[ras$Exotic.C3.rec >= 15 & is.na(ras$Exotic.C4.rec) == T] <- 0
  
# raster template
  raster <- raster("C:/Users/s436862/Dropbox/Rarefaction/Data files/Australia/aus 100 km v2.grd")
  plot(raster)

# save 
  write.csv(ras, "C:/Users/s436862/Dropbox/Rarefaction/Results/Rarefaction/CSV/Rarefied 15 richness and record number pp SOLO.csv", row.names = F)
  
# Run function ------------------------------------------------
# required -----------------------------------------------------------------------------------
# (1) raster
# (2) file save name (e.g. Graphs/Native SR 50-km.jpeg)  
# (3) title: will pop up in the top left hand corner of the plot
  
  setwd("C:/Users/s436862/Dropbox/Rarefaction/Results/Rarefaction/Graphs/Rarefied richness solo")
  
# native c3
  nat.c3 <- setValues(raster, ras$Native.C3.rich)
  eng_ras(nat.c3/15, "Native.C3.jpeg")

# native c4 
  nat.c4 <- setValues(raster, ras$Native.C4.rich)
  eng_ras(nat.c4/15, "Native.C4.jpeg")
  
# exotic c3
  exo.c3 <- setValues(raster, ras$Exotic.C3.rich)
  eng_ras(exo.c3/15, "Exotic.C3.jpeg")
  
# exotic c4 
  exo.c4 <- setValues(raster, ras$Exotic.C4.rich)
  eng_ras(exo.c4/15, "Exotic.C4.jpeg")
  
# 4. scatterplots of rarefied (15 records) richness -------------------------------
# correlating the distributions between photosynthetic pathways with origin (i.e. nat/exo x C3/C4)
# and also between SOLO rarefaction and porportion C3/C4

  
# 4.1 proportion of C3/C4 with origin ------------------------------  
# run 2. code for rasters
# data --------------------------------------------------------------
  n.rich <- read.csv("C:/Users/s436862/Dropbox/Rarefaction/Results/Rarefaction/CSV/Rarefied native richness 10 to 50 records.csv") %>%
    dplyr::select(total.rare.15, C3.rare.15, C4.rare.15)
  e.rich <- read.csv("C:/Users/s436862/Dropbox/Rarefaction/Results/Rarefaction/CSV/Rarefied exotic richness 10 to 50 records.csv") %>%
    dplyr::select(total.rare.15, C3.rare.15, C4.rare.15)
  
  rare.rich <- cbind(n.rich, e.rich)
  colnames(rare.rich) <- c("n.tot", "n.c3", "n.c4",
                           "e.tot", "e.c3", "e.c4")  
  
# scatterlot function ---------------------------------------------------------------  
# C3-C3
  xlab <- "native C3"
  ylab <- "exotic C3" 
  save <- "Results/Rarefaction/Graphs/Scatterplots/Observed/Native C3-exotic C3.jpeg"
  
  a <- ggplot(aes(x = n.c3, y = e.c3), data = rare.rich) +
      geom_point(size = 1.5) +
      theme_bw() + 
      labs(x = xlab,
           y = ylab) +
      theme(axis.title = element_text(size = 14)) +
      geom_abline(slope = 1, intercept = 0, size = 1)
    
    ggsave(save, plot = last_plot(), dpi = 500, scale = 1, device = "jpeg")
    
  a

# C4-C4    
  xlab <- "native C4"
  ylab <- "exotic C4" 
  save <- "Results/Rarefaction/Graphs/Scatterplots/Observed/Native C4-exotic C4.jpeg"
  
  b <- ggplot(aes(x = n.c4, y = e.c4), data = rare.rich) +
    geom_point(size = 1.5) +
    theme_bw() + 
    labs(x = xlab,
         y = ylab) +
    theme(axis.title = element_text(size = 14)) +
    geom_abline(slope = 1, intercept = 0, size = 1)
  
  ggsave(save, plot = last_plot(), dpi = 500, scale = 1, device = "jpeg")
  
  b
  
  c4.c4.cor <- cor(rare.rich$n.c4, rare.rich$e.c4, use = "complete.obs") # still 0.81??
  
# nat:c3-c4  
  xlab <- "native C3"
  ylab <- "native C4" 
  save <- "Results/Rarefaction/Graphs/Scatterplots/Observed/Native C3-native C4.jpeg"
  
  c <- ggplot(aes(x = n.c3, y = n.c4), data = rare.rich) +
    geom_point(size = 1.5) +
    theme_bw() + 
    labs(x = xlab,
         y = ylab) +
    theme(axis.title = element_text(size = 14)) +
    geom_abline(slope = -1, intercept = 11, size = 1)
  
  ggsave(save, plot = last_plot(), dpi = 500, scale = 1, device = "jpeg")
  
  c
  
  c4.c4.cor <- cor(rare.rich$n.c4, rare.rich$e.c4, use = "complete.obs") # still 0.81??
  
# exotic:c3-c4
  xlab <- "exotic C3"
  ylab <- "exotic C4" 
  save <- "Results/Rarefaction/Graphs/Scatterplots/Observed/Exotic C3-exotic C4.jpeg"
  
  d <- ggplot(aes(x = e.c3, y = e.c4), data = rare.rich) +
    geom_point(size = 1.5) +
    theme_bw() + 
    labs(x = xlab,
         y = ylab) +
    theme(axis.title = element_text(size = 14)) +
    geom_abline(slope = -1, intercept = 9.5, size = 1)
  
  ggsave(save, plot = last_plot(), dpi = 500, scale = 1, device = "jpeg")

  d
  
# total-total
  xlab <- "native total"
  ylab <- "exotic toal" 
  save <- "Results/Rarefaction/Graphs/Scatterplots/Observed/Native total-exotic total.jpeg"
  
  e <- ggplot(aes(x = n.tot, y = e.tot), data = rare.rich) +
    geom_point(size = 1.5) +
    theme_bw() + 
    labs(x = xlab,
         y = ylab) +
    theme(axis.title = element_text(size = 14)) +
    geom_abline(slope = 1, intercept = -2, size = 1)
  
  ggsave(save, plot = last_plot(), dpi = 500, scale = 1, device = "jpeg")
  
  e

# 4.2 Porportion of C3/C4 versus independent C3/C4 ----------------------------------
# notes ----------------------------------------------------------------------
# I have tried a new method for partioning C3 and C4 richness
# here I evaluate it against previous method
# previous method dealt with proportion (hence 'prop') and new is sepearate ('solo')    
# -----------------------------------------------------------------------------
# data --------------------------------------------------------------
  setwd("C:/Users/s436862/Dropbox/Rarefaction/Results/Rarefaction")
  
# C3/C4 proportions
  n.rich <- read.csv("CSV/Rarefied native richness 10 to 50 records.csv") %>%
    dplyr::select(C3.rare.15, C4.rare.15)
  e.rich <- read.csv("CSV/Rarefied exotic richness 10 to 50 records.csv") %>%
    dplyr::select(C3.rare.15, C4.rare.15)
  
# C3/C4 solo
  solo <- read.csv("CSV/Rarefied 15 richness and record number pp SOLO.csv") %>%
    dplyr::select(Native.C3.rich, Native.C4.rich, 
                  Exotic.C3.rich, Exotic.C4.rich)
  
  comparison <- cbind(n.rich, e.rich, solo)
  colnames(comparison) <- c("n.c3.prop", "n.c4.prop", "e.c3.prop", "e.c4.prop",
                            "n.c3.solo", "n.c4.solo", "e.c3.solo", "e.c4.solo")  
  
# scatter plots -----------------------------------------------------------------
# nat C3-C3   
  xlab <- "Native C3 (proportion)"
  ylab <- "Native C3 (independent)" 
  save <- "Graphs/Scatterplots/Proportion vs. solo/Native C3-native C3.jpeg"
  
  b <- ggplot(aes(x = n.c3.prop, y = n.c3.solo), data = comparison) +
    geom_point(size = 1.5) +
    theme_bw() + 
    labs(x = xlab,
         y = ylab) +
    theme(axis.title = element_text(size = 14)) +
    geom_abline(slope = 1, intercept = 0, size = 1)
  
  ggsave(save, plot = last_plot(), dpi = 500, scale = 1, device = "jpeg")
  
  b
  
  n.c3.cor <- cor(comparison$n.c3.prop, comparison$n.c3.solo, use = "complete.obs") # 0.79
  
# nat C4-C4   
  xlab <- "Native C4 (proportion)"
  ylab <- "Native C4 (independent)" 
  save <- "Graphs/Scatterplots/Proportion vs. solo/Native C4-native C4.jpeg"
  
  c <- ggplot(aes(x = n.c4.prop, y = n.c4.solo), data = comparison) +
    geom_point(size = 1.5) +
    theme_bw() + 
    labs(x = xlab,
         y = ylab) +
    theme(axis.title = element_text(size = 14)) +
    geom_abline(slope = 1, intercept = 0, size = 1)
  
  ggsave(save, plot = last_plot(), dpi = 500, scale = 1, device = "jpeg")
  
  c
  
  n.c4.cor <- cor(comparison$n.c4.prop, comparison$n.c4.solo, use = "complete.obs") # 0.94
  
# exo C3-C3   
  xlab <- "Exotic C3 (proportion)"
  ylab <- "Exotic C3 (independent)" 
  save <- "Graphs/Scatterplots/Proportion vs. solo/Exotic C3-exotic C3.jpeg"
  
  c <- ggplot(aes(x = e.c3.prop, y = e.c3.solo), data = comparison) +
    geom_point(size = 1.5) +
    theme_bw() + 
    labs(x = xlab,
         y = ylab) +
    theme(axis.title = element_text(size = 14)) +
    geom_abline(slope = 1, intercept = 0, size = 1)
  
  ggsave(save, plot = last_plot(), dpi = 500, scale = 1, device = "jpeg")
  
  c
  
  e.c3.cor <- cor(comparison$e.c3.prop, comparison$e.c3.solo, use = "complete.obs") # 0.85  
  
  
# exo C4-C4   
  xlab <- "Exotic C4 (proportion)"
  ylab <- "Exotic C4 (independent)" 
  save <- "Graphs/Scatterplots/Proportion vs. solo/Exotic C4-exotic C4.jpeg"
  
  c <- ggplot(aes(x = e.c4.prop, y = e.c4.solo), data = comparison) +
    geom_point(size = 1.5) +
    theme_bw() + 
    labs(x = xlab,
         y = ylab) +
    theme(axis.title = element_text(size = 14)) +
    geom_abline(slope = 1, intercept = 0, size = 1)
  
  ggsave(save, plot = last_plot(), dpi = 500, scale = 1, device = "jpeg")
  
  c
  
  e.c4.cor <- cor(comparison$e.c4.prop, comparison$e.c4.solo, use = "complete.obs") # 0.74  
  
  
# --------------------------------------------------------------------------  

  
  
# 5. Model coefficients --------------------------------------
# Nat/int rarefied to 15-records: total, C3 and C4 richness (from Rarefaction chapter model v5 [atm; 16/10])   
# Load model workspace (from Rarefaction chapter model v5)
  rm(list = ls())
  setwd("C:/Users/s436862/Dropbox/Rarefaction")
  load("Results/Rarefaction/Rdata/Proportion_rarefaction_model_coefficient_values.RData")

# EV order rearrange: to do so, change at the beginning of 'Raster Chapter model v7[current]' script   
# environmental variable labels
  ev.levels <- c("pcoldq", "pwarmq", "amt", "ts", "arid", "pewc", "th", "hii")
  
  ev.labels <- c("Winter \nrainfall", 
               "Summer \nrainfall", 
               "Annual mean \ntemperature", 
               "Temperature \nseasonality", 
               "Aridity", 
               "Soil water \navailability", 
               "Topographic \nheterogeneity", 
               "Human \nactivity")

# reorder levels in spp dfs to match EV labels
  total.rich$plot.names <- factor(ev.labels, ordered = is.ordered(ev.labels))
  total.rich$plot.names <- fct_inorder(total.rich$plot.names)
  
  c3.rich$plot.names <- factor(ev.labels, ordered = is.ordered(ev.labels))
  c3.rich$plot.names <- fct_inorder(c3.rich$plot.names)
  
  c4.rich$plot.names <- factor(ev.labels, ordered = is.ordered(ev.labels))
  c4.rich$plot.names <- fct_inorder(c4.rich$plot.names)
  
# Total -----------------------------------------------------
# total adjusted r2: x = 7, y = 1.8, label = "Adj. r2 = exotic 0.29, native 0.31"
  k <- ggplot(total.rich, aes(x = plot.names, color = status)) +
    theme_classic() +
    scale_color_manual(labels = c("Exotic", "Native"), values = c("blue", "red")) +
    geom_hline(aes(yintercept = 0),
               color = "black", size = 0.6) +
    labs(title = "", x = "Environmental variables", y = "Parameter estimate", color = "grey100") +
    geom_point(aes(y = estimate), size = 4, position = position_dodge(width = 0.6)) +
    geom_errorbar(aes(ymin = lower.ci, ymax = upper.ci),
                  size = 1, width = 0, position = position_dodge(width = 0.6)) +
    annotate("segment", x = -Inf, xend = Inf, y = -Inf, yend = -Inf) +
    annotate("segment", x = -Inf, xend = -Inf,y = -Inf, yend = Inf) + 
    annotate("text", x = 7, y = 1.8, label = "Adj. R2 = exotic 0.29, native 0.31")
  
  #ggThemeAssistGadget(h)
  
  k + theme(axis.text = element_text(size = 12, colour = "gray0"),
            axis.text.x = element_text(size = 12, angle = 45, hjust = 1), 
            axis.text.y = element_text(size = 12), 
            axis.title = element_text(size = 16),
            legend.text = element_text(size = 14), 
            legend.title = element_blank(), 
            axis.ticks.length = unit(0.2, "cm")) 
  
  ggsave("Results/Rarefaction/Graphs/Coefficients/Total coefficients.jpeg", plot = last_plot(), scale = 1, dpi = 500, device = "jpeg")
  
  
# C3 -------------------------------------------------------------------
# c3 adjusted r2 & position: x = 7, y = 1.3, label = "Adj. r2 = exotic 0.82, native 0.89"
  i <- ggplot(c3.rich, aes(x = plot.names, color = status)) +
    theme_classic() +
    scale_color_manual(labels = c("Exotic", "Native"), values = c("blue", "red")) +
    geom_hline(aes(yintercept = 0),
               color = "black", size = 0.6) +
    labs(title = "", x = "Environmental variables", y = "Parameter estimate", color = "grey100") +
    geom_point(aes(y = estimate), size = 5, position = position_dodge(width = 0.6)) +
    geom_errorbar(aes(ymin = lower.ci, ymax = upper.ci),
                  size = 1, width = 0, position = position_dodge(width = 0.6)) +
    annotate("segment", x = -Inf, xend = Inf, y = -Inf, yend = -Inf) +
    annotate("segment", x = -Inf, xend = -Inf,y = -Inf, yend = Inf) + 
    annotate("text", x = 7, y = 1.3, label = "Adj. R2 = exotic 0.82, native 0.89")
  
  #ggThemeAssistGadget(h)
  
  i + theme(axis.text = element_text(size = 12, colour = "gray0"),
            axis.text.x = element_text(size = 12, angle = 45, hjust = 1), 
            axis.text.y = element_text(size = 12), 
            axis.title = element_text(size = 16),
            legend.text = element_text(size = 14), 
            legend.title = element_blank(), 
            axis.ticks.length = unit(0.2, "cm")) 
  
  ggsave("Results/Rarefaction/Graphs/Coefficients/C3 coefficients.jpeg", plot = last_plot(), scale = 1, dpi = 500, device = "jpeg")
  
# C4 ---------------------------------------------------------------
# c4 position and r2: x = 7, y = 3.1, label = "Adj. r2 = exotic 0.72, native 0.83"
  j <- ggplot(c4.rich, aes(x = plot.names, color = status)) +
    theme_classic() +
    scale_color_manual(labels = c("Exotic", "Native"), values = c("blue", "red")) +
    geom_hline(aes(yintercept = 0),
               color = "black", size = 0.6) +
    labs(title = "", x = "Environmental variables", y = "Parameter estimate", color = "grey100") +
    geom_point(aes(y = estimate), size = 5, position = position_dodge(width = 0.6)) +
    geom_errorbar(aes(ymin = lower.ci, ymax = upper.ci),
                  size = 1, width = 0, position = position_dodge(width = 0.6)) +
    annotate("segment", x = -Inf, xend = Inf, y = -Inf, yend = -Inf) +
    annotate("segment", x = -Inf, xend = -Inf,y = -Inf, yend = Inf) + 
    annotate("text", x = 7, y = 2.9, label = "Adj. R2 = exotic 0.72, native 0.83")
  
  #ggThemeAssistGadget(h)
  
  j + theme(axis.text = element_text(size = 12, colour = "gray0"),
            axis.text.x = element_text(size = 12, angle = 45, hjust = 1), 
            axis.text.y = element_text(size = 12), 
            axis.title = element_text(size = 16),
            legend.text = element_text(size = 14), 
            legend.title = element_blank(), 
            axis.ticks.length = unit(0.2, "cm")) 
  
  ggsave("Results/Rarefaction/Graphs/Coefficients/C4 coefficients.jpeg", plot = last_plot(), scale = 1, dpi = 500, device = "jpeg")
  


# -------------------------------------------------------------------------    
  
