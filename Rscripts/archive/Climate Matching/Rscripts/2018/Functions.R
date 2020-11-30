
# Date created: 25/7/2018
# Last updated: 

# The purpose of this is to note each functuion I create, with a little title so I can search for it more easily than trolling through random scripts
# E.g. scatterplot, rarefaction, etc.

# Remove all but one thing from your workspace ----------------------------
# Saves memory
# remove everything but output in environment to do next section
  rm(list=setdiff(ls(), "thing_you_want_to_keep")) 


# Scatterplot: x and y in same data frame ------------------------------------
# Example data ------------------------------------------------
# Native C3 rarefied richness vs. annual precipitation both contained within the same dataframe

# Required packages
  library(RColorBrewer)
  library(raster)
  library(ggmap)
  library(tidyr)
  library(rgdal)
  library(ggplot2)
  library(dplyr)
  library(rasterVis)
  library(maptools)
  library(ggthemes)
  library(ggThemeAssist)
  library(gplots)


# Here, we are comparing rarefied C3-native and -introduced richness  
  setwd("C:/Users/s436862/Dropbox/Rarefaction/4. Results/Rarefaction")
# raster 
  aus <- raster("C:/Users/s436862/Dropbox/Climate Matching/1. Data files/Australia/aus_100km_cell_id")
# spp data   
  nat_c3 <- raster("Rasters/Rarefied 30 pp/Nat rare C3")
  int_c3 <- raster("Rasters/Rarefied 30 pp/Int rare C3")
  
#   
  
# Scatter plot function ------------------------------------------------------
# Requires:
# df: complete df of all the spp categories
# xcol: ef
# ycol: spp
# xlab: ef name
# ylab: spp2 name
# save: exact save locale
# ggThemeAssistGadget(plot1)
  
# Function to plot combinations of columns & rows
  plot_fun <- function(df, xcol, ycol, xlab, ylab, save){
    df <- select(df, x = one_of(xcol), y = one_of(ycol))
    
    n <- ggplot(data = df, aes(x, y)) +
      geom_point(size = 1.5) +
      theme_bw() + 
      labs(x = ylab,
           y = xlab) +
      theme(axis.title = element_text(size = 14)) + 
      geom_smooth(method = "lm", se = FALSE)
    
    # ggThemeAssistGadget(n) 
    
    n + theme(axis.ticks = element_line(size = 1), 
              panel.grid.major = element_line(linetype = "blank"), 
              panel.grid.minor = element_line(linetype = "blank"), 
              axis.text = element_text(size = 12), 
              panel.background = element_rect(fill = NA))
    
    
    # save
    ggsave(save, plot = last_plot(), dpi = 500, device = "jpeg")
    
    return(n)
  } # finish script
  

  
# Example use: Nat-C3 vs AP --------------------------------------------------
# Note: it will save this someone, so be aware of that 
  
# Requires in this order:
# df: complete df of all the efs and spp
# xcol: enviro factor
# ycol: spp
# xlab: spp name
# ylab: ef name
# save: exact save locale   df <- spp_ef
  xcol <- "ap"
  ycol <- "nat_c3"
  xlab <- "Native-C3 richness"
  ylab <- "Annual precipitation"
  save <- "4. Results/Rarefaction/Graphs/Single variate scatter plots/Nat C3-AP.jpeg" 
  
# Nat-C3 ~ AP
  natc3.ap <- lm(nat_c3 ~ ap, data = spp_ef)
  summary(natc3.ap)
  
  plot_fun(df, 
           xcol,
           ycol,
           xlab,
           ylab,
           save)
  
# ---------------------------------------------------------------------

  
# Turn values into NAs using dplyr ------------------------------------
  
