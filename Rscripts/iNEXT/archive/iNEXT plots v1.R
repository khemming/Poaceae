#########################################################
# iNEXT rarefaction plots
#########################################################
# date created: 30/4/19
# last modified: 

# Rarefaction Paper rasters and other plots using data generated from using the 'independent' photosynthetic pathway method

# library --------------------------------------------------------------
  setwd("C:/Users/s436862/Dropbox/Rarefaction")
  
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
  
# 2. iNEXT rarefied richness (pilot script rasters) --------------------------
# required ------------------------------------------------------------------------
# (1) raster
# (2) file save name (e.g. Graphs/Native SR 50-km.jpeg)  
# (3) title: will pop up in the top left hand corner of the plot
  
# Rarefaction function w legend ---------------------------------------------------------
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
                           limits = c(0, leg), # update
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
  
# data ------------------------------------------------
  setwd("C:/Users/s436862/Dropbox/Rarefaction/Results/Rarefaction/Rasters/iNEXT/Pilot")
  
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
  
  setwd("C:/Users/s436862/Dropbox/Rarefaction/Results/Rarefaction/Graphs/iNEXT/Pilot")
# native tot
  eng_ras(n.tot, "Native.tot.jpeg", cellStats(n.tot, stat = 'max', na.rm = T))
# native c3
  eng_ras(n.c3, "Native.C3.jpeg", cellStats(n.c3, stat = 'max', na.rm = T))
# native c4 
  eng_ras(n.c4, "Native.C4.jpeg", cellStats(n.c4, stat = 'max', na.rm = T))
  
# native tot
  eng_ras(e.tot, "Exotic.tot.jpeg", cellStats(n.tot, stat = 'max', na.rm = T)) # n.tot is much higher 
# exotic c3
  eng_ras(e.c3, "Exotic.C3.jpeg", cellStats(n.c3, stat = 'max', na.rm = T)) # n max is higher
# exotic c4 
  eng_ras(e.c4, "Exotic.C4.jpeg", cellStats(n.c4, stat = 'max', na.rm = T)) # n max is incredibly higher
  
# 3. iNEXT (pilot) raster scatterplots ------------------------------  

# data --------------------------------------------------------------
  setwd("C:/Users/s436862/Dropbox/Rarefaction/Results/Rarefaction/Rasters/iNEXT/Pilot")
  
  current.list <- list.files(pattern = ".grd")
  names <- gsub(pattern = "\\.grd$", "", current.list)
  
  c.stack <- stack(current.list)
  names(c.stack) <- names
  
  list2env(setNames(unstack(c.stack), names(c.stack)), .GlobalEnv)
  
  rare.rich <- as.data.frame(c.stack, na.rm = F)
  
  
# scatterlot function ---------------------------------------------------------------  
# setwd()
  setwd("C:/Users/s436862/Dropbox/Rarefaction/Results/Rarefaction/Graphs/iNEXT/Pilot")

# total-total
  xlab <- "Native total"
  ylab <- "Exotic total" 
  save <- "Native total-exotic total.jpeg"
  
  tot.cor <- cor(rare.rich$n.tot, rare.rich$e.tot, use = "complete.obs") # 0.34
  
  a <- ggplot(aes(x = n.tot, y = e.tot), data = rare.rich) +
    geom_point(size = 1.5) +
    theme_bw() + 
    labs(x = xlab,
         y = ylab) +
    theme(axis.title = element_text(size = 14)) +
    geom_abline(intercept = 0, slope = 1, size = 1) +
    annotate("text", x = 130, y = 60, label = "r = 0.27", size = 6)
    
  
  ggsave(save, plot = last_plot(), dpi = 500, scale = 1, device = "jpeg")
  
  a
  
# C3-C3
  xlab <- "Native C3"
  ylab <- "Exotic C3" 
  save <- "Native C3-exotic C3.jpeg"
  
  c3.cor <- round(cor(rare.rich$n.c3, rare.rich$e.c3, use = "complete.obs"), 2) # 0.55
  
  b <- ggplot(aes(x = n.c3, y = e.c3), data = rare.rich) +
    geom_point(size = 1.5) +
    theme_bw() + 
    labs(x = xlab,
         y = ylab) +
    theme(axis.title = element_text(size = 14)) +
    geom_abline(intercept = 0, slope = 1, size = 1) +
    annotate("text", x = 40, y = 35, label = "r = 0.55", size = 6)
  
  
  ggsave(save, plot = last_plot(), dpi = 500, scale = 1, device = "jpeg")
  
  b

# C4-C4    
  xlab <- "Native C4"
  ylab <- "Exotic C4" 
  save <- "Native C4-exotic C4.jpeg"
  
  c4.cor <- round(cor(rare.rich$n.c4, rare.rich$e.c4, use = "complete.obs"), 2) # 0.51
  
  c <- ggplot(aes(x = n.c4, y = e.c4), data = rare.rich) +
    geom_point(size = 1.5) +
    theme_bw() + 
    labs(x = xlab,
         y = ylab) +
    theme(axis.title = element_text(size = 14)) +
    geom_abline(intercept = 0, slope = 1, size = 1) +
    annotate("text", x = 125, y = 48, label = "r = 0.51", size = 6)
  
  
  ggsave(save, plot = last_plot(), dpi = 500, scale = 1, device = "jpeg")
  
  c
  
# nat:c3-c4  
  xlab <- "native C3"
  ylab <- "native C4" 
  save <- "Graphs/Scatterplots/Observed independent method/Native C3-native C4.jpeg"
  
  c <- ggplot(aes(x = n.c3, y = n.c4), data = rare.rich) +
    geom_point(size = 1.5) +
    theme_bw() + 
    labs(x = xlab,
         y = ylab) +
    theme(axis.title = element_text(size = 14)) +
    geom_abline(slope = -1, intercept = 13, size = 1)
  
  ggsave(save, plot = last_plot(), dpi = 500, scale = 1, device = "jpeg")
  
  c
  
  c4.c4.cor <- cor(rare.rich$n.c4, rare.rich$e.c4, use = "complete.obs") # still 0.81??
  
# exotic:c3-c4
  xlab <- "exotic C3"
  ylab <- "exotic C4" 
  save <- "Graphs/Scatterplots/Observed independent method/Exotic C3-exotic C4.jpeg"
  
  d <- ggplot(aes(x = e.c3, y = e.c4), data = rare.rich) +
    geom_point(size = 1.5) +
    theme_bw() + 
    labs(x = xlab,
         y = ylab) +
    theme(axis.title = element_text(size = 14)) +
    geom_abline(slope = -1, intercept = 9.5, size = 1)
  
  ggsave(save, plot = last_plot(), dpi = 500, scale = 1, device = "jpeg")

  d
  
  c3.c4.cor <- cor(rare.rich$e.c3, rare.rich$e.c4, use = "complete.obs") # still -.13

  
# -------------------------------------------------------------------
  
  
# 4. Independent vs. iNEXT scatterplots ----------------------------------
# data --------------------------------------------------------------
  setwd("C:/Users/s436862/Dropbox/Rarefaction/Results/Rarefaction")
  
# C3/C4 proportions
  n.rich <- read.csv("CSV/Rarefied native richness 10 to 50 records.csv") %>%
    dplyr::select(C3.rare.15, C4.rare.15)
  e.rich <- read.csv("CSV/Rarefied exotic richness 10 to 50 records.csv") %>%
    dplyr::select(C3.rare.15, C4.rare.15)
  
# C3/C4 independent
  independent <- read.csv("CSV/Rarefied 15 richness and record number pp independent.csv") %>%
    dplyr::select(Native.C3.rich, Native.C4.rich, 
                  Exotic.C3.rich, Exotic.C4.rich)
  
  comparison <- cbind(n.rich, e.rich, independent)
  colnames(comparison) <- c("n.c3.prop", "n.c4.prop", "e.c3.prop", "e.c4.prop",
                            "n.c3.independent", "n.c4.independent", "e.c3.independent", "e.c4.independent")  
  
# scatter plots -----------------------------------------------------------------
# nat C3-C3   
  xlab <- "Native C3 (proportion)"
  ylab <- "Native C3 (independent)" 
  save <- "Graphs/Scatterplots/Proportion vs. independent/Native C3-native C3.jpeg"
  
  b <- ggplot(aes(x = n.c3.prop, y = n.c3.independent), data = comparison) +
    geom_point(size = 1.5) +
    theme_bw() + 
    labs(x = xlab,
         y = ylab) +
    theme(axis.title = element_text(size = 14)) +
    geom_abline(slope = 1, intercept = 0, size = 1)
  
  ggsave(save, plot = last_plot(), dpi = 500, scale = 1, device = "jpeg")
  
  b
  
  n.c3.cor <- cor(comparison$n.c3.prop, comparison$n.c3.independent, use = "complete.obs") # 0.79
  
# nat C4-C4   
  xlab <- "Native C4 (proportion)"
  ylab <- "Native C4 (independent)" 
  save <- "Graphs/Scatterplots/Proportion vs. independent/Native C4-native C4.jpeg"
  
  c <- ggplot(aes(x = n.c4.prop, y = n.c4.independent), data = comparison) +
    geom_point(size = 1.5) +
    theme_bw() + 
    labs(x = xlab,
         y = ylab) +
    theme(axis.title = element_text(size = 14)) +
    geom_abline(slope = 1, intercept = 0, size = 1)
  
  ggsave(save, plot = last_plot(), dpi = 500, scale = 1, device = "jpeg")
  
  c
  
  n.c4.cor <- cor(comparison$n.c4.prop, comparison$n.c4.independent, use = "complete.obs") # 0.94
  
# exo C3-C3   
  xlab <- "Exotic C3 (proportion)"
  ylab <- "Exotic C3 (independent)" 
  save <- "Graphs/Scatterplots/Proportion vs. independent/Exotic C3-exotic C3.jpeg"
  
  c <- ggplot(aes(x = e.c3.prop, y = e.c3.independent), data = comparison) +
    geom_point(size = 1.5) +
    theme_bw() + 
    labs(x = xlab,
         y = ylab) +
    theme(axis.title = element_text(size = 14)) +
    geom_abline(slope = 1, intercept = 0, size = 1)
  
  ggsave(save, plot = last_plot(), dpi = 500, scale = 1, device = "jpeg")
  
  c
  
  e.c3.cor <- cor(comparison$e.c3.prop, comparison$e.c3.independent, use = "complete.obs") # 0.85  
  
  
# exo C4-C4   
  xlab <- "Exotic C4 (proportion)"
  ylab <- "Exotic C4 (independent)" 
  save <- "Graphs/Scatterplots/Proportion vs. independent/Exotic C4-exotic C4.jpeg"
  
  c <- ggplot(aes(x = e.c4.prop, y = e.c4.independent), data = comparison) +
    geom_point(size = 1.5) +
    theme_bw() + 
    labs(x = xlab,
         y = ylab) +
    theme(axis.title = element_text(size = 14)) +
    geom_abline(slope = 1, intercept = 0, size = 1)
  
  ggsave(save, plot = last_plot(), dpi = 500, scale = 1, device = "jpeg")
  
  c
  
  e.c4.cor <- cor(comparison$e.c4.prop, comparison$e.c4.independent, use = "complete.obs") # 0.74  
  
  
# --------------------------------------------------------------------------  

  
  
# 5. iNEXT model coefficients --------------------------------------
# Nat/int rarefied to 15-records: total, C3 and C4 richness (from Rarefaction chapter model v5 [atm; 16/10])   
# Load model workspace (from Rarefaction chapter model v5)
  rm(list = ls())
  setwd("C:/Users/s436862/Dropbox/Rarefaction")
  load("Results/Rarefaction/Rdata/Independent_rarefaction_model_coefficient_values.RData")

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
  c3.rich$plot.names <- factor(ev.labels, ordered = is.ordered(ev.labels))
  c3.rich$plot.names <- fct_inorder(c3.rich$plot.names)
  
  c4.rich$plot.names <- factor(ev.labels, ordered = is.ordered(ev.labels))
  c4.rich$plot.names <- fct_inorder(c4.rich$plot.names)
  
# C3 -------------------------------------------------------------------
# c3 adjusted r2 & position: x = 7, y = 1.5, label = "Adj. r2 = exotic 0.76, native 0.74"
  i <- ggplot(c3.rich, aes(x = plot.names, color = status)) +
    theme_classic() +
    scale_color_manual(labels = c("Exotic", "Native"), values = c("blue", "red")) +
    geom_hline(aes(yintercept = 0),
               color = "black", size = 0.6) +
    labs(title = "", x = "Environmental and anthropogenic variables", y = "Parameter estimate", color = "grey100") +
    geom_point(aes(y = estimate), size = 5, position = position_dodge(width = 0.6)) +
    geom_errorbar(aes(ymin = lower.ci, ymax = upper.ci),
                  size = 1, width = 0, position = position_dodge(width = 0.6)) +
    annotate("segment", x = -Inf, xend = Inf, y = -Inf, yend = -Inf) +
    annotate("segment", x = -Inf, xend = -Inf,y = -Inf, yend = Inf) + 
    annotate("text", x = 7, y = 1.5, label = "Adj. r2 = exotic 0.76, native 0.74")
  
  #ggThemeAssistGadget(h)
  
  i + theme(axis.text = element_text(size = 12, colour = "gray0"),
            axis.text.x = element_text(size = 12, angle = 45, hjust = 1), 
            axis.text.y = element_text(size = 12), 
            axis.title = element_text(size = 16),
            legend.text = element_text(size = 14), 
            legend.title = element_blank(), 
            axis.ticks.length = unit(0.2, "cm")) 
  
  ggsave("Results/Rarefaction/Graphs/Coefficients/Independent C3 coefficients.jpeg", plot = last_plot(), scale = 1, dpi = 500, device = "jpeg")
  
# C4 ---------------------------------------------------------------
# c4 position and r2: x = 7, y = 3.1, label = "Adj. r2 = exotic 0.35, native 0.71"
  j <- ggplot(c4.rich, aes(x = plot.names, color = status)) +
    theme_classic() +
    scale_color_manual(labels = c("Exotic", "Native"), values = c("blue", "red")) +
    geom_hline(aes(yintercept = 0),
               color = "black", size = 0.6) +
    labs(title = "", x = "Environmental and anthropogenic variables", y = "Parameter estimate", color = "grey100") +
    geom_point(aes(y = estimate), size = 5, position = position_dodge(width = 0.6)) +
    geom_errorbar(aes(ymin = lower.ci, ymax = upper.ci),
                  size = 1, width = 0, position = position_dodge(width = 0.6)) +
    annotate("segment", x = -Inf, xend = Inf, y = -Inf, yend = -Inf) +
    annotate("segment", x = -Inf, xend = -Inf,y = -Inf, yend = Inf) + 
    annotate("text", x = 7, y = 2.9, label = "Adj. r2 = exotic 0.35, native 0.71")
  
  #ggThemeAssistGadget(h)
  
  j + theme(axis.text = element_text(size = 12, colour = "gray0"),
            axis.text.x = element_text(size = 12, angle = 45, hjust = 1), 
            axis.text.y = element_text(size = 12), 
            axis.title = element_text(size = 16),
            legend.text = element_text(size = 14), 
            legend.title = element_blank(), 
            axis.ticks.length = unit(0.2, "cm")) 
  
  ggsave("Results/Rarefaction/Graphs/Coefficients/Independent C4 coefficients.jpeg", plot = last_plot(), scale = 1, dpi = 500, device = "jpeg")
  


# -------------------------------------------------------------------------    
  
