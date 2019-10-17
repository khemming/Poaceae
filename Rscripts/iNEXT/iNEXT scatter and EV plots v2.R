#########################################################
# iNEXT rarefaction plots
#########################################################
# date created: 30/4/19
# last modified: 4/5 (went from pilot to "15rec 0.8coverage warn removed" data)

# aim -------------------------------------------------------------------
# produce all the results plots
# any summary stats or supplementary figures will be in another script

# library --------------------------------------------------------------
  setwd("C:/Users/s436862/Dropbox/Poaceae")

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

# data ------------------------------------------------
  setwd("C:/Users/s436862/Dropbox/Poaceae/Results/iNEXT/Rasters/observed 15 rec 0.8 cov warn removed")
  
  current.list <- list.files(pattern = ".grd")
  names <- gsub(pattern = "\\.grd$", "", current.list)
  
  c.stack <- stack(current.list)
  names(c.stack) <- names
  
  list2env(setNames(unstack(c.stack), names(c.stack)), .GlobalEnv)# required 

  rich <- as.data.frame(getValues(c.stack))
  
   setwd("C:/Users/s436862/Dropbox/Poaceae/Results/iNEXT/Graphs/scatterplots")  

# scatterlots ------------------------------------  
# total-total ------------------------------------
# labels  
  xlab <- "Total native richness"
  ylab <- "Total non-native richness" 
  save <- "Native total-exotic total.jpeg"

# correlation score to use  
  tot.cor <- round(cor(rich$n.tot, rich$e.tot, use = "complete.obs", method = "spearman"), 2)
  tot.cor
  
# scale limits
  max(rich$n.tot, na.rm = T)
  max(rich$e.tot, na.rm = T)
  
# C3 plot
  ggplot(aes(x = n.tot, y = e.tot), data = rich) +
    geom_point(shape = "circle", size = 1) +
    theme_bw() + 
    theme(panel.border = element_blank(),
          #axis.title.x = element_blank(),
          #axis.title.y = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), 
          axis.line = element_line(colour = "black", size = 1),
          axis.text.x = element_text(colour = "black", size = 18),
          axis.text.y = element_text(colour = "black", size = 18),
          axis.ticks = element_blank()) +
    scale_x_continuous(breaks = c(40, 80, 120), 
                       limits = c(1, max(rich$n.tot, na.rm = T)), 
                       expand = c(0, 3)) +
    scale_y_continuous(breaks = c(40, 80, 120), 
                       limits = c(1, max(rich$n.tot, na.rm = T)), 
                       expand = c(0, 3)) +
    geom_abline(intercept = 0, slope = 1, size = 1) +
    annotate("text", x = 140, y = 60, label = "r = 0.23", size = 5) +
    labs(x = xlab,
         y = ylab) +
      theme(axis.title = element_text(size = 18))
    
  ggsave(save, plot = last_plot(), dpi = 500, scale = 1, device = "jpeg")
# ----------------------------------------------------------------  
# C3-C3 ----------------------------------------------------------
# labels  
  xlab <- "Native C3 richness"
  ylab <- "Non-native C3 richness" 
  save <- "C3 total-C3 total.jpeg"

# correlation score to use  
  c3.cor <- round(cor(rich$n.c3, rich$e.c3, use = "complete.obs", method = "spearman"), 2)
  c3.cor
  
# scale limits
  max(rich$n.c3, na.rm = T)
  max(rich$e.c3, na.rm = T)

# C3 plot
  ggplot(aes(x = n.c3, y = e.c3), data = rich) +
    geom_point(shape = "circle", size = 1) +
    theme_bw() + 
    theme(panel.border = element_blank(),
          #axis.title.x = element_blank(),
          #axis.title.y = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), 
          axis.line = element_line(colour = "black", size = 1),
          axis.text.x = element_text(colour = "black", size = 18),
          axis.text.y = element_text(colour = "black", size = 18),
          axis.ticks = element_blank()) +
    scale_x_continuous(breaks = c(15, 30, 45), 
                       limits = c(1, max(rich$n.c3, na.rm = T)), 
                       expand = c(0, 3)) +
    scale_y_continuous(breaks = c(15, 30, 45), 
                       limits = c(1, max(rich$n.c3, na.rm = T)), 
                       expand = c(0, 3)) +
    geom_abline(intercept = 0, slope = 1, size = 1) +
    annotate("text", x = 50, y = 45, label = "r = 0.69", size = 5) +
    labs(x = xlab,
         y = ylab) +
    theme(axis.title = element_text(size = 18))
  
  ggsave(save, plot = last_plot(), dpi = 500, scale = 1, device = "jpeg")

# C4-C4 ---------------------------------------------------    
# labels  
  xlab <- "Native C4 richness"
  ylab <- "Non-native C4 richness" 
  save <- "C4 total-C4 total.jpeg"
  
# correlation score to use  
  c4.cor <- round(cor(rich$n.c4, rich$e.c4, use = "complete.obs", method = "spearman"), 2)
  c4.cor
  
# scale limits
  max(rich$n.c4, na.rm = T)
  max(rich$e.c4, na.rm = T)
  
# C4 plot
  ggplot(aes(x = n.c4, y = e.c4), data = rich) +
    geom_point(shape = "circle", size = 1) +
    theme_bw() + 
    theme(panel.border = element_blank(),
          #axis.title.x = element_blank(),
          #axis.title.y = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), 
          axis.line = element_line(colour = "black", size = 1),
          axis.text.x = element_text(colour = "black", size = 18),
          axis.text.y = element_text(colour = "black", size = 18),
          axis.ticks = element_blank()) +
    scale_x_continuous(breaks = c(50, 100, 150), 
                       limits = c(1, max(rich$n.c4, na.rm = T)),
                       expand = c(0, 3)) +
    scale_y_continuous(breaks = c(50, 100, 150), 
                       limits = c(1, max(rich$n.c4, na.rm = T)),
                       expand = c(0, 3)) +
    geom_abline(intercept = 0, slope = 1, size = 1) +
    annotate("text", x = 140, y = 60, label = "r = 0.36", size = 5) +
    labs(x = xlab,
         y = ylab) +
    theme(axis.title = element_text(size = 18))
  
  ggsave(save, plot = last_plot(), dpi = 500, scale = 1, device = "jpeg")
  
# native c3-c4 --------------------------------------------------------- 
# labels    
  xlab <- "Native C4 richness"
  ylab <- "Native C3 richness" 
  save <- "Native C3-native C4.jpeg"

# correlation score to use  
  n.c34.cor <- round(cor(rich$n.c3, rich$n.c4, use = "complete.obs", method = "spearman"), 2)
  n.c34.cor
  
# scale limits
  max(rich$n.c4, na.rm = T)
  max(rich$n.c3, na.rm = T)
  
# plot
  ggplot(aes(x = n.c4, y = n.c3), data = rich) +
    geom_point(shape = "circle", size = 1) +
    theme_bw() + 
    theme(panel.border = element_blank(),
          #axis.title.x = element_blank(),
          #axis.title.y = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), 
          axis.line = element_line(colour = "black", size = 1),
          axis.text.x = element_text(colour = "black", size = 18),
          axis.text.y = element_text(colour = "black", size = 18),
          axis.ticks = element_blank()) +
    scale_x_continuous(breaks = c(40, 80, 120), 
                       limits = c(1, max(rich$n.c4, na.rm = T)),
                       expand = c(0, 2)) +
    scale_y_continuous(breaks = c(40, 80, 120), 
                       limits = c(1, max(rich$n.c4, na.rm = T)),
                       expand = c(0, 2)) +
    geom_abline(intercept = 0, slope = 1, size = 1) +
    annotate("text", x = 140, y = 60, label = "r = -0.38", size = 5) +
    labs(x = xlab,
         y = ylab) +
    theme(axis.title = element_text(size = 18))
  
  ggsave(save, plot = last_plot(), dpi = 500, scale = 1, device = "jpeg")
  
  
# Non-native C3-C4 ------------------------------------------------------
# labels      
  xlab <- "Non-native C4"
  ylab <- "Non-native C3" 
  save <- "Exotic C3-exotic C4.jpeg"

# correlation score to use    
  c3.c4.cor <- round(cor(rich$e.c4, rich$e.c3, use = "complete.obs", method = "spearman"), 2) # 
  c3.c4.cor
  
# scale limits
  max(rich$e.c4, na.rm = T)
  max(rich$e.c3, na.rm = T)
  
# plot
  ggplot(aes(x = e.c4, y = e.c3), data = rich) +
    geom_point(shape = "circle", size = 1) +
    theme_bw() + 
    theme(panel.border = element_blank(),
          #axis.title.x = element_blank(),
          #axis.title.y = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), 
          axis.line = element_line(colour = "black", size = 1),
          axis.text.x = element_text(colour = "black", size = 18),
          axis.text.y = element_text(colour = "black", size = 18),
          axis.ticks = element_blank()) +
    scale_x_continuous(breaks = c(15, 30, 45), 
                       limits = c(1, max(rich$e.c4, na.rm = T)),
                       expand = c(0, 2)) +
    scale_y_continuous(breaks = c(15, 30, 45), 
                       limits = c(1, max(rich$e.c4, na.rm = T)),
                       expand = c(0, 2)) +
    geom_abline(intercept = 0, slope = 1, size = 1) +
    annotate("text", x = 45, y = 40, label = "r = 0.19", size = 5) +
    labs(x = xlab,
         y = ylab) +
    theme(axis.title = element_text(size = 18))
  
  ggsave(save, plot = last_plot(), dpi = 500, scale = 1, device = "jpeg")
  
# -------------------------------------------------------------------
 
  



# ---------------------------------------------------------------------------------    
  
# 4.1 Predicted distribution plots ------------------------------------------------  
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