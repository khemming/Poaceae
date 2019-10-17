
# Date created: 18/4
# Last modified: 12/3/19 (updated using outputs from rarefaction v7)

# Rarefaction Paper rasters and other plots

# Based on: 'Rarefaction df + rasters' and 'Rarefaction plots' and Engemann's results flow

# v4 = cut a lot out -- see v3 for more, which itself has lots of things cut out of it that aren't totally necessary for the paper; see version 2 for a complete set.

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
  
# 1.2 rarefaction 10- to 50- records x Nat/Exo x total/C3/C4 ---------------------------------
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
  
# 2. summary statistics on rarefaction subsamples -------------------------------------------------------------  
# 2.1 native + exotic correlation matrix on subsamples ----------------------------------------------------------
  rm(list = ls())
  
  nat <- read.csv("Results/Rarefaction/CSV/Rarefied native richness 10 to 50 records.csv", header = T)
  exo <- read.csv("Results/Rarefaction/CSV/Rarefied exotic richness 10 to 50 records.csv", header = T)
  
  poa <- cbind(nat, exo)
  
  cor <- cor(poa, use = "complete.obs")
  write.csv(cor, "Results/Rarefaction/CSV/Rarefied richness 10 to 50 record correlation matrix.csv", row.names = T)
  
# 2.2 native percentage of cells occupied ---------------------------------------------------  
# rarefied richness data
  nat <- read.csv("Results/Rarefaction/CSV/Rarefied native richness 10 to 50 records.csv", header = T)
  
# cell categories
  aus.cells <- read.csv("C:/Users/s436862/Dropbox/Rarefaction/Data files/Australia/aus 100-km.csv", header = T)
  land.cells <- filter(aus.cells, cell.cat == "land") # 1133
  
# land cells
  dat.cells <- cbind(aus.cells, nat)
  dat.cells.f <- filter(dat.cells, cell.cat == "land")
  
# nas to zero for percentage estimates
  dat.cells.f[is.na(dat.cells.f)] <- 0
  dat.cells.e <- dat.cells.f[,4:ncol(dat.cells.f)]
  
# the amount of cells occupied for each column
  d <- dat.cells.e
  k <- length(dat.cells.e[,1])
  c3.10 <- sum(d$C3.rare.10 > 0)/k*100
  
  ff <- function(x) (sum(x > 0))/k*100
  
  
  d.p <- apply(d, MARGIN = 2, ff)
  dd.p <- data.frame(as.list(d.p))
  
  write.csv(dd.p, "Results/Rarefaction/CSV/Percent of cells occupied native.csv", row.names = F)
  
# --------------------------------------------------------------------------------------  
# 2.3 exotic percentage of cells occupied ---------------------------------------------------  
# rarefied richness data
  exo <- read.csv("Results/Rarefaction/CSV/Rarefied exotic richness 10 to 50 records.csv", header = T)
  
# cell categories
  aus.cells <- read.csv("C:/Users/s436862/Dropbox/Rarefaction/Data files/Australia/aus 100-km.csv", header = T)
  land.cells <- filter(aus.cells, cell.cat == "land") # 1133
  
# land cells
  dat.cells <- cbind(aus.cells, exo)
  dat.cells.f <- filter(dat.cells, cell.cat == "land")
  
# nas to zero for percentage estimates
  dat.cells.f[is.na(dat.cells.f)] <- 0
  dat.cells.e <- dat.cells.f[,4:ncol(dat.cells.f)]
  
# the amount of cells occupied for each column
  d <- dat.cells.e
  k <- length(dat.cells.e[,1])
  c3.10 <- sum(d$C3.rare.10 > 0)/k*100
  
  ff <- function(x) (sum(x > 0))/k*100
  
  
  d.p <- apply(d, MARGIN = 2, ff)
  dd.p <- data.frame(as.list(d.p))
  
  write.csv(dd.p, "Results/Rarefaction/CSV/Percent of cells occupied exotic.csv", row.names = F)
  
# --------------------------------------------------------------------------------------    
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
  
  o.e.c3 <- raster("Results/Rarefaction/Rasters/Rarefied rasters/r15.e.c3.grd")
  plot(o.e.c3)
  o.e.c3.df <- getValues(o.e.c3)
  
  o.n.c4 <- raster("Results/Rarefaction/Rasters/Rarefied rasters/r15.n.c4.grd")
  plot(o.n.c4)
  o.n.c4.df <- getValues(o.n.c4)
  
  o.e.c4 <- raster("Results/Rarefaction/Rasters/Rarefied rasters/r15.e.c4.grd")
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
  

  
