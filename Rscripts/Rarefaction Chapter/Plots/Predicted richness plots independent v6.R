
# Date created: 18/4
# Last modified: 29/4 (updated using outputs from rarefaction v7)
# v6 = using different rarefaction methodology [independent] to calculate C3-C4 richnesses

# v4 = cut a lot out -- see v3 for more, which itself has lots of things cut out of it that aren't totally necessary for the paper; see version 2 for a complete set.
# v5 = probably cut more out


# library --------------------------------------------------------------
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
  
# 1. predicted distribution plots --------------------------------------------  
# Raster data
  setwd("C:/Users/s436862/Dropbox/Rarefaction/Results/Rarefaction/Rasters/Predicted distributions independent method")
  
  current.list <- list.files(pattern = ".grd")
  names <- gsub(pattern = "\\.grd$", "", current.list)
  
  c.stack <- stack(current.list)
  names(c.stack) <- names
  
  list2env(setNames(unstack(c.stack), names(c.stack)), .GlobalEnv)
  
# Raster plot function ---------------------------------------------------------------
# We are truncating over- and under-predicted values to between 0 and 15; therefore I will still scale it to 0 and 1, but can change this if need be.
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
  setwd("C:/Users/s436862/Dropbox/Rarefaction/Results/Rarefaction")

# For each plot, set any cell values <0 --> 0 and >15 -> 15
 # Total native -- turncating upper values to 15
  t.n <- raster("Rasters/Predicted distributions independent method/n.tot.predicted.grd")
  t.n.calc <- calc(t.n, fun = function(x) {x[x>15] <- 15; return(x)})/15
  plot(t.n.calc)
 
  raster <- t.n.calc
  legend <- "Standardised \nrichness"
  save <- "Graphs/Predicted distributions independent method/Native total.jpeg"
  
  eng_ras(raster, legend, save)
  
# Total exotic 
  t.e <- raster("Rasters/Predicted distributions independent method/e.tot.predicted.grd")
  t.e.calc <- calc(t.e, fun = function(x) {x[x>15] <- 15; return(x)})/15
  raster <- t.e.calc
  legend <- "Standardised \nrichness"
  save <- "Graphs/Predicted distributions independent method/Exotic total.jpeg"
  
  eng_ras(raster, legend, save)
  
# C3 native -- truncate lower and uppee values (0-15 -> 0-1) 
  c3.n <- raster("Rasters/Predicted distributions independent method/n.c3.predicted.grd")
  c3.n.calc <- calc(c3.n, fun = function(x) {x[x<0] <- 0; x[x>15] <- 15; return(x)})/15
  
  raster <- c3.n.calc
  legend <- "Standardised \nrichness"
  save <- "Graphs/Predicted distributions independent method/Native C3.jpeg"
  
  eng_ras(raster, legend, save)
  
# C3 exotic 
  c3.e <- raster("Rasters/Predicted distributions independent method/e.c3.predicted.grd")
  c3.e.calc <- calc(c3.e, fun = function(x) {x[x<0] <- 0; x[x>15] <- 15; return(x)})/15
  
  raster <- c3.e.calc
  legend <- "Standardised \nrichness"
  save <- "Graphs/Predicted distributions independent method/Exotic C3.jpeg"
  
  eng_ras(raster, legend, save)
  
# C4 native
  c4.n <- raster("Rasters/Predicted distributions independent method/n.c4.predicted.grd")
  c4.n.calc <- calc(c4.n, fun = function(x) {x[x<0] <- 0; x[x>15] <- 15; return(x)})/15
  
  raster <- c4.n.calc
  legend <- "Standardised \nrichness"
  save <- "Graphs/Predicted distributions independent method/Native C4.jpeg"
  
  eng_ras(raster, legend, save)
  
# C4 exotic
  c4.e <- raster("Rasters/Predicted distributions independent method/e.c4.predicted.grd")
  c4.e.calc <- calc(c4.e, fun = function(x) {x[x<0] <- 0; x[x>15] <- 15; return(x)})/15
  
  raster <- c4.e.calc
  legend <- "Standardised \nrichness"
  save <- "Graphs/Predicted distributions independent method/Exotic C4.jpeg"
  
  eng_ras(raster, legend, save) 

# 5.3 Observed/predicted scatterplots ------------------------------------------
# Species data
# o = observed, p = predicted; e = exotic, n = native; C3/C4 = pp
  
# Observed species distributions
  o.n.c3 <- raster("Rasters/Rarefied richness independent/Native C3 rich.grd")
  plot(o.n.c3)
  o.n.c3.df <- getValues(o.n.c3)
  
  o.e.c3 <- raster("Rasters/Rarefied richness independent/Exotic C3 rich.grd")
  plot(o.e.c3)
  o.e.c3.df <- getValues(o.e.c3)
  
  o.n.c4 <- raster("Rasters/Rarefied richness independent/Native C4 rich.grd")
  plot(o.n.c4)
  o.n.c4.df <- getValues(o.n.c4)
  
  o.e.c4 <- raster("Rasters/Rarefied richness independent/Exotic C4 rich.grd")
  plot(o.e.c4)
  o.e.c4.df <- getValues(o.e.c4)
  
# Predicted species distributions
  p.n.c3 <- raster("Rasters/Predicted distributions independent method/n.c3.predicted.grd")
  plot(p.n.c3)
  p.n.c3.df <- getValues(p.n.c3)
  
  p.e.c3 <- raster("Rasters/Predicted distributions independent method/e.c3.predicted.grd")
  plot(p.e.c3)
  p.e.c3.df <- getValues(p.e.c3)
  
  p.n.c4 <- raster("Rasters/Predicted distributions independent method/n.c4.predicted.grd")
  plot(p.n.c4)
  p.n.c4.df <- getValues(p.n.c4)
  
  p.e.c4 <- raster("Rasters/Predicted distributions independent method/e.c4.predicted.grd")
  plot(p.e.c4)
  p.e.c4.df <- getValues(p.e.c4)

# merge 
  spp <- cbind(o.n.c3.df, o.n.c4.df, o.e.c3.df, o.e.c4.df,
               p.n.c3.df, p.n.c4.df, p.e.c3.df, p.e.c4.df)
  
  spp.df <- data.frame(spp)
  
# truncating values below zero and above 15  
  trunc <- function(x) {x[x<0] <- 0; x[x>15] <- 15; return(x)} 
  spp.df.trunc <- trunc(spp.df) # that worked??
    
# Scatterplot 
# Interested in how observed native correlates with predicted exotic, but will plot all combinations
  setwd("C:/Users/s436862/Dropbox/Rarefaction/Results/Rarefaction/Graphs/Scatterplots/Predicted independent method")

# Scatterlot function ---------------------------------------------------------------  
  colour <- rev(brewer.pal(11, "Spectral"))
  scale_fill_gradientn(colours = colour, 
                       space = "Lab",
                       limits = c(0, 15),
                       na.value = "transparent",
                       guide = "colourbar") 
    
# Requires: x, y, xlab, ylab, save
  scat_fun <- function (x, y, xlab, ylab, save)  {
    a <- ggplot(aes(x = x, y = y), data = spp.df.trunc) +
      geom_point(size = 1.5) +
      theme_bw() + 
      labs(x = xlab,
           y = ylab) +
      theme(axis.title = element_text(size = 14)) +
      geom_abline(slope = 1, intercept = 0, size = 1)
     
    ggsave(save, plot = last_plot(), dpi = 500, scale = 1, device = "jpeg")
  
  } # function end
  

# testing testing   
     a <- ggplot(aes(x = x, y = y, colour = colour), data = spp.df.trunc) +
          theme_minimal() +
          geom_point(size = 1.5)
     a
     a + scale_color_gradient(low="blue", high="red")
    
    ggsave(save, plot = last_plot(), dpi = 500, scale = 1, device = "jpeg")
    
  
  x <- o.n.c3.df 
  y <- p.n.c3.df
  xlab <- "Observed native C3"
  ylab <- "Predicted native C3" 
  save <- "Observed native C3-Predicted native C3.jpeg"
# -----------------------------------
# Native obs C3 vs. native pred C3 
  scat_fun(spp.df.trunc$o.n.c3.df, 
           spp.df.trunc$p.n.c3.df, 
           "Observed native C3", 
           "Predicted native C3", 
           "Observed native C3-Predicted native C3.jpeg")
  
# Native obs C4 vs. native pred C4 
  scat_fun(spp.df.trunc$o.n.c4.df,
           spp.df.trunc$p.n.c4.df, 
           "Observed native C4", 
           "Predicted native C4", 
           "Observed native C4-Predicted native C4.jpeg")
  
# Native obs C3 vs. exotic pred C3
  scat_fun(spp.df.trunc$o.n.c3.df, 
           spp.df.trunc$p.e.c3.df, 
           "Observed native C3", 
           "Predicted exotic C3", 
           "Observed native C3-Predicted exotic C3.jpeg")
  
  
# Native obs C4 vs. exotic pred C4
  scat_fun(spp.df.trunc$o.n.c4.df, 
           spp.df.trunc$p.e.c4.df, 
           "Observed native C4", 
           "Predicted exotic C4", 
           "Observed native C4-Predicted exotic C4.jpeg") 
  
# Native pred C4 vs. exotic pred C3
  scat_fun(spp.df.trunc$p.n.c3.df, 
           spp.df.trunc$p.e.c3.df, 
           "Predicted native C3", 
           "Predicted exotic C3", 
           "Predicted native C3-Predicted exotic C3.jpeg") 
  
# Native pred C4 vs. exotic pred C4
  scat_fun(spp.df.trunc$p.n.c4.df, 
           spp.df.trunc$p.e.c4.df, 
           "Predicted native C4", 
           "Predicted exotic C4", 
           "Predicted native C4-Predicted exotic C4.jpeg") 
  

# 5.4 potential exotic distributions ---------------------------------------  
# not the right method ---------------------------------------------------------
  # Plots to describe the potential for Australia to support higher numbers of species richness than it does currently, using that lovely template supplied by the natives
  # # Total "potential" (pot)
  #   t.e <- raster("Rasters/Rarefied richness independent/r15.e.tot.grd")/15
  #   t.e1 <- calc(t.e, fun = function(x) {x[is.na(x)] <- 0; return(x)})
  #   plot(t.e1)
  #   t.e.calc <- calc(t.e1, fun = function(x) {x[x<0] <- 0; return(x)})
  #   plot(t.e.calc)
  #   t.pot <- t.n.calc - t.e.calc
  #   plot(t.n.calc)
  #   plot(t.e.calc)
  #   plot(t.pot)
  #   t.pot2 <- calc(t.pot, fun = function(x) {x[x<0] <- 0; return(x)})
  #   plot(t.pot2)
  #   raster <- t.pot2
  #   legend <- "Standardised \nrichness"
  #   save <- "Graphs/Predicted distributions independent method/Potential distribution total.jpeg"
  #   
  #   eng_ras(raster, legend, save) 
  #   
  # # C3 potential 
  #   c3.e <- raster("Rasters/Rarefied richness independent/r15.e.c3.grd")/15
  #   c3.e1 <- calc(c3.e, fun = function(x) {x[is.na(x)] <- 0; return(x)})
  #   plot(c3.e1)
  #   c3.e.calc <- calc(c3.e1, fun = function(x) {x[x<0] <- 0; return(x)})
  #   c3.pot <- c3.n.calc - c3.e.calc
  #   plot(c3.n.calc)
  #   plot(c3.e.calc)
  #   plot(c3.pot)
  #   c3.pot2 <- calc(c3.pot, fun = function(x) {x[x<0] <- 0; return(x)})
  #   plot(c3.pot2)
  #   raster <- c3.pot2
  #   legend <- "Standardised \nrichness"
  #   save <- "Graphs/Predicted distributions independent method/Potential distribution C3.jpeg"
  #   
  #   eng_ras(raster, legend, save) 
  #   
  # # C4 potential 
  #   c4.e <- raster("Rasters/Rarefied richness independent/r15.e.c4.grd")/15
  #   c4.e1 <- calc(c4.e, fun = function(x) {x[is.na(x)] <- 0; return(x)})
  #   plot(c4.e1)
  #   c4.e.calc <- calc(c4.e1, fun = function(x) {x[x<0] <- 0; return(x)})
  #   plot(c4.e.calc)
  #   c4.pot <- c4.n.calc - c4.e.calc
  #   plot(c4.n.calc)
  #   plot(c4.e.calc)
  #   plot(c4.pot)
  #   c4.pot2 <- calc(c4.pot, fun = function(x) {x[x<0] <- 0; return(x)})
  #   plot(c4.pot2)
  #   raster <- c4.pot2
  #   legend <- "Standardised \nrichness"
  #   save <- "Graphs/Predicted distributions independent method/Potential distribution C4.jpeg"
  #   
  #   eng_ras(raster, legend, save) 
# ---------------------------------------------------------------------------------  
# the right method:
  setwd("C:/Users/s436862/Dropbox/Rarefaction/Results/Rarefaction")
# total ---------------------------------------------------
# exotic
  plot(t.e)
  p.e.tot.calc <- calc(t.e, fun = function(x) {x[x<0] <- 0; x[x>15] <- 15; return(x)})/15
  plot(p.e.tot.calc)
  
# native
  p.n.tot.calc <- calc(t.n, fun = function(x) {x[x<0] <- 0; x[x>15] <- 15; return(x)})/15
  plot(p.n.tot.calc)
  
  potential <- p.n.tot.calc - p.e.tot.calc
  pot.calc <- calc(potential, fun = function(x) {x[x<0] <- 0; return(x)})
  
  
  raster <- pot.calc
  legend <- "Standardised \nrichness"
  save <- "Graphs/Predicted distributions independent method/Total poential.jpeg"
  
  eng_ras(raster, legend, save) 

# C3 ------------------------------------------------------
# exotic
  plot(p.e.c3)
  p.e.c3.calc <- calc(p.e.c3, fun = function(x) {x[x<0] <- 0; x[x>15] <- 15; return(x)})/15
  plot(p.e.c3.calc)
  
# native
  p.n.c3.calc <- calc(p.n.c3, fun = function(x) {x[x<0] <- 0; x[x>15] <- 15; return(x)})/15
  plot(p.n.c3.calc)
  
  potential <- p.n.c3.calc - p.e.c3.calc
  pot.calc <- calc(potential, fun = function(x) {x[x<0] <- 0; return(x)})
  
  
  raster <- pot.calc
  legend <- "Standardised \nrichness"
  save <- "Graphs/Predicted distributions independent method/C3 potential.jpeg"
  
  eng_ras(raster, legend, save) 

# C4 ------------------------------------------------------
# exotic
  plot(p.e.c4)
  p.e.c4.calc <- calc(p.e.c4, fun = function(x) {x[x<0] <- 0; x[x>15] <- 15; return(x)})/15
  plot(p.e.c4.calc)
  
# native
  p.n.c4.calc <- calc(p.n.c4, fun = function(x) {x[x<0] <- 0; x[x>15] <- 15; return(x)})/15
  plot(p.n.c4.calc)
  
  potential <- p.n.c4.calc - p.e.c4.calc
  pot.calc <- calc(potential, fun = function(x) {x[x<0] <- 0; return(x)})
  
  
  raster <- pot.calc
  legend <- "Standardised \nrichness"
  save <- "Graphs/Predicted distributions independent method/C4 potential.jpeg"
  
  eng_ras(raster, legend, save) 
  