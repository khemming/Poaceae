#########################################################
# iNEXT raster plots
#########################################################

# aim -------------------------------------------------------------------
# produce: C3 and C4 x
#          native and non-native x
#          observed and predicted and spread potential plots

# library --------------------------------------------------------------
  library(gplots)
  library(RColorBrewer)
  library(raster)
  library(ggmap)
  library(rgdal)
  library(rasterVis)
  library(maptools)
  library(ggthemes)
  library(ggThemeAssist)
  library(gplots)
  library(tidyverse)
  library(forcats)
  library(maps)

# 1. iNEXT plots ----------------------------------------------------------
# 1.1 raster maps ---------------------------------------------------------
# function -- remember legend (leg) requirements
  eng_ras <- function (raster, save, leg)  
    
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
                           limits = leg, # update
                           space = "Lab",
                           na.value = "transparent",
                           guide = "colourbar",
                           name = "Species\nrichness") + 
      coord_equal() +
      coord_cartesian(xlim = c(112, 155), ylim = c(-45, -7), expand = F) +
      theme(legend.justification = "centre",
            legend.position = "right",
            aspect.ratio = 0.88) +
      oz2
    print(q)
    
    ggsave(save, plot = last_plot(), scale = 1, dpi = 500, device = "jpeg")
    
  } # finish function
  
# run plots ---------------------------------------------------------------------
# data ------------------------------------------------
  setwd("C:/Users/s436862/Dropbox/Poaceae/Results/iNEXT/Rasters/observed 15 rec 0.8 cov warn removed")
  
  current.list <- list.files(pattern = ".grd")
  names <- gsub(pattern = "\\.grd$", "", current.list)
  
  c.stack <- stack(current.list)
  names(c.stack) <- names
  
  list2env(setNames(unstack(c.stack), names(c.stack)), .GlobalEnv)# required 
# function ---------------------------------------------------------------------
# (1) raster
# (2) file save name (e.g. Graphs/Native SR 50-km.jpeg)  
# (3) title: will pop up in the top left hand corner of the plot
  
  setwd("C:/Users/s436862/Dropbox/Poaceae/Results/iNEXT/Graphs/observed 15 rec 0.8 cov warn removed")
# native tot
  eng_ras(n.tot, "Native.tot.jpeg", c(1, cellStats(n.tot, stat = 'max', na.rm = T)))
# native c3
  eng_ras(n.c3, "Native.C3.jpeg", c(1, cellStats(n.c3, stat = 'max', na.rm = T)))
# native c4 
  eng_ras(n.c4, "Native.C4.jpeg", c(1, cellStats(n.c4, stat = 'max', na.rm = T)))
  
# native tot
  eng_ras(e.tot, "Exotic.tot.jpeg", c(1, cellStats(n.tot, stat = 'max', na.rm = T))) # n.tot is much higher 
# exotic c3
  eng_ras(e.c3, "Exotic.C3.jpeg", c(1, cellStats(n.c3, stat = 'max', na.rm = T))) # n max is higher
# exotic c4 
  eng_ras(e.c4, "Exotic.C4.jpeg", c(1, cellStats(n.c4, stat = 'max', na.rm = T))) # n max is incredibly higher
  


# -------------------------------------------------------------------------    
  
# 4.1 Predicted distribution plots --------------------------------------------  
# Raster data
  rm(list = ls())
  
  setwd("C:/Users/s436862/Dropbox/Poaceae/Results/iNEXT/Rasters/predicted 15 rec 0.8 cov warn removed")
  
  current.list <- list.files(pattern = ".grd")
  names <- gsub(pattern = "\\.grd$", "", current.list)
  
  c.stack <- stack(current.list)
  names(c.stack) <- names
  
  list2env(setNames(unstack(c.stack), names(c.stack)), .GlobalEnv)
  
# Raster plot function ---------------------------------------------------------------
# function --------------------------------------------  
  eng_ras <- function (raster, leg, save)  
    
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
                           limits = leg,
                           na.value = "transparent",
                           guide = "colourbar",
                           name = "Species \nrichness") + 
      coord_equal() +
      coord_cartesian(xlim = c(112, 155), ylim = c(-45, -7), expand = F) +
      theme(legend.justification = "centre",
            legend.position = "right",
            aspect.ratio = 0.88) +
      oz2
    print(q)
    
    ggsave(save, plot = last_plot(), scale = 1, dpi = 500, device = "jpeg")
    
  } # finish function
  
  
# predicted plots ----------------------------------------------  
  setwd("C:/Users/s436862/Dropbox/Poaceae/Results/iNEXT")
  
# total native 
  n.tot <- raster("Rasters/predicted 15 rec 0.8 cov warn removed/n.tot.predicted.grd")
  n.tot.calc <- calc(n.tot, fun = function(x) {x[x<0] <- 0; return(x)})
  plot(n.tot.calc)
  
  raster <- n.tot.calc
  leg <- c(0, cellStats(n.tot.calc, stat = 'max', na.rm = T))
  save <- "Graphs/predicted 15 rec 0.8 cov warn removed/Native total.jpeg"
  
  eng_ras(raster, leg, save)
  
# total exotic 
  e.tot <- raster("Rasters/predicted 15 rec 0.8 cov warn removed/e.tot.predicted.grd")
  plot(e.tot)
  
  raster <-e.tot
  leg <- c(0, cellStats(n.tot, stat = 'max', na.rm = T)) # native total legend scale
  save <- "Graphs/predicted 15 rec 0.8 cov warn removed/Exotic total.jpeg"
  
  eng_ras(raster, leg, save)
  
# C3 native
  n.c3 <- raster("Rasters/predicted 15 rec 0.8 cov warn removed/n.c3.predicted.grd")
# negative values constrained to zero
  n.c3.calc <- calc(n.c3, fun = function(x) {x[x<0] <- 0; return(x)})
  plot(n.c3.calc)
  n.c3.calc
  
  raster <- n.c3.calc
  leg <- c(0, cellStats(n.c3.calc, stat = 'max', na.rm = T)) 
  save <- "Graphs/predicted 15 rec 0.8 cov warn removed/Native C3.jpeg"
  
  eng_ras(raster, leg, save)
  
# C3 exotic 
  e.c3 <- raster("Rasters/predicted 15 rec 0.8 cov warn removed/e.c3.predicted.grd")
# negative values constrained to zero
  e.c3.calc <- calc(e.c3, fun = function(x) {x[x<0] <- 0; return(x)})
  plot(e.c3.calc)
  e.c3.calc
  
  raster <- e.c3.calc
  leg <- c(0, cellStats(n.c3.calc, stat = 'max', na.rm = T)) # native max
  save <- "Graphs/predicted 15 rec 0.8 cov warn removed/Exotic C3.jpeg"
  
  eng_ras(raster, leg, save)
  
# C4 native
  n.c4 <- raster("Rasters/predicted 15 rec 0.8 cov warn removed/n.c4.predicted.grd")
# negative values constrained to zero
  n.c4.calc <- calc(n.c4, fun = function(x) {x[x<0] <- 0; return(x)})
  plot(n.c4.calc)
  n.c4.calc
  
  raster <- n.c4.calc
  leg <- c(0, cellStats(n.c4.calc, stat = 'max', na.rm = T)) 
  save <- "Graphs/predicted 15 rec 0.8 cov warn removed/Native C4.jpeg"
  
  eng_ras(raster, leg, save)
  
# C4 exotic
  e.c4 <- raster("Rasters/predicted 15 rec 0.8 cov warn removed/e.c4.predicted.grd")
  e.c4 # no negative values
  
  raster <- e.c4
  leg <- c(0, cellStats(n.c4.calc, stat = 'max', na.rm = T)) 
  save <- "Graphs/predicted 15 rec 0.8 cov warn removed/Exotic C4.jpeg"
  
  eng_ras(raster, leg, save)
  

# 4.2 potential plots ---------------------------------------------------
# aim: updated (30/5/19): am using observed exotic against predcited native
  
# data: run 4.1 section, above
# data -------------------------------------------------------------------  
  e.tot.ob <- raster("Rasters/observed 15 rec 0.8 cov warn removed/e.tot.grd")
  e.c3.ob <- raster("Rasters/observed 15 rec 0.8 cov warn removed/e.c3.grd")
  e.c4.ob <- raster("Rasters/observed 15 rec 0.8 cov warn removed/e.c4.grd")
  
  n.tot.pr <- n.tot.predicted
  n.c3.pr <- n.c3.predicted
  n.c4.pr <- n.c4.predicted
  
# Na <- 0 for exotics
# (raster(x) - raster(Na) = raster(Na), where I actually want x back)
  e.tot.ob.calc <- calc(e.tot.ob, fun = function(x) {x[is.na(x) == T] <- 0; return(x)})
  plot(e.tot.ob.calc)
  e.c3.ob.calc <- calc(e.c3.ob, fun = function(x) {x[is.na(x) == T] <- 0; return(x)})
  plot(e.c3.ob.calc)
  e.c4.ob.calc <- calc(e.c4.ob, fun = function(x) {x[is.na(x) == T] <- 0; return(x)})
  plot(e.c4.ob.calc)

# caluclate difference 
# total
  p.tot <- n.tot.calc - e.tot.ob.calc
  p.tot # negative values
  plot(p.tot)
  p.tot.calc <- calc(p.tot, fun = function(x) {x[x<0] <- 0; return(x)})
  plot(p.tot.calc)
# c3
  p.c3 <- n.c3.calc - e.c3.ob.calc
  p.c3 # negative values
  plot(p.c3)
  p.c3.calc <- calc(p.c3, fun = function(x) {x[x<0] <- 0; return(x)})
  plot(p.c3.calc)
# c4 
  p.c4 <- n.c4.calc - e.c4.ob.calc
  p.c4 # negative values
  plot(p.c4)
  p.c4.calc <- calc(p.c4, fun = function(x) {x[x<0] <- 0; return(x)})
  plot(p.c4.calc)
  
  
# raster plots & save --------------------------------------------------------
# total
# legend relative to native total
  leg <- c(0, cellStats(n.tot.calc, stat = 'max', na.rm = T))
  save <- "Graphs/predicted 15 rec 0.8 cov warn removed/Potential total.jpeg"
  
  eng_ras(p.tot.calc, leg, save)
 
# c3 
# legend relative to native c3
  leg <- c(0, cellStats(n.c3.calc, stat = 'max', na.rm = T)) 
  save <- "Graphs/predicted 15 rec 0.8 cov warn removed/Potential C3.jpeg"
  
  eng_ras(p.c3.calc, leg, save)

# c4 
# legend relative to native c4
  leg <- c(0, cellStats(n.c4.calc, stat = 'max', na.rm = T)) 
  save <- "Graphs/predicted 15 rec 0.8 cov warn removed/Potential C4.jpeg"
  
  eng_ras(p.c4.calc, leg, save) 
# --------------------------------------------------------  