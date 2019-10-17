#########################################################
# iNEXT raster plots
#########################################################
# date created: 30/4/19
# last modified: 9/10 name change

# aim -------------------------------------------------------------------
# produce all the different raster plots

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

# 1. iNEXT rarefied richness (v2 script) plots --------------------------
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
  
# 2.2. raster scatterplots -------------------------------  
# scatterlot function ------------------------------------  
# data 
  rare.rich <- as.data.frame(getValues(c.stack))

# setwd()
  setwd("C:/Users/s436862/Dropbox/Poaceae/Results/iNEXT/Graphs/observed 15 rec 0.8 cov warn removed")

# total-total
  xlab <- "Native total"
  ylab <- "Exotic total" 
  save <- "Native total-exotic total.jpeg"
  
  tot.cor <- round(cor(rare.rich$n.tot, rare.rich$e.tot, use = "complete.obs", method = "spearman"), 2)
  tot.cor
  
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
  
  c3.cor <- round(cor(rare.rich$n.c3, rare.rich$e.c3, use = "complete.obs", method = "spearman"), 2)
  c3.cor
  
  b <- ggplot(aes(x = n.c3, y = e.c3), data = rare.rich) +
    geom_point(size = 1.5) +
    theme_bw() + 
    labs(x = xlab,
         y = ylab) +
    theme(axis.title = element_text(size = 14)) +
    geom_abline(intercept = 0, slope = 1, size = 1) +
    annotate("text", x = 40, y = 35, label = "r = 0.67", size = 6)
  
  
  ggsave(save, plot = last_plot(), dpi = 500, scale = 1, device = "jpeg")
  
  b

# C4-C4    
  xlab <- "Native C4"
  ylab <- "Exotic C4" 
  save <- "Native C4-exotic C4.jpeg"
  
  c4.cor <- round(cor(rare.rich$n.c4, rare.rich$e.c4, use = "complete.obs", method = "spearman"), 2) # 0.51
  c4.cor
  
  c <- ggplot(aes(x = n.c4, y = e.c4), data = rare.rich) +
    geom_point(size = 1.5) +
    theme_bw() + 
    labs(x = xlab,
         y = ylab) +
    theme(axis.title = element_text(size = 14)) +
    geom_abline(intercept = 0, slope = 1, size = 1) +
    annotate("text", x = 135, y = 48, label = "r = 0.48", size = 6)
  
  
  ggsave(save, plot = last_plot(), dpi = 500, scale = 1, device = "jpeg")
  
  c
  
# nat:c3-c4  
  xlab <- "native C3"
  ylab <- "native C4" 
  save <- "Native C3-native C4.jpeg"
  
  c3.c4.cor <- round(cor(rare.rich$n.c4, rare.rich$n.c3, use = "complete.obs", method = "spearman"), 2) # 
  c3.c4.cor
  
  d <- ggplot(aes(x = n.c4, y = n.c3), data = rare.rich) +
    geom_point(size = 1.5) +
    theme_bw() + 
    labs(x = xlab,
         y = ylab) +
    theme(axis.title = element_text(size = 14)) +
    #geom_abline(slope = -1, intercept = 45, size = 1) +
    annotate("text", x = 115, y = 48, label = "r = -0.41", size = 6)
  
  ggsave(save, plot = last_plot(), dpi = 500, scale = 1, device = "jpeg")
  
  d
  
  
# exotic:c3-c4
  xlab <- "Exotic C3"
  ylab <- "Exotic C4" 
  save <- "Exotic C3-exotic C4.jpeg"
  
  c3.c4.cor <- round(cor(rare.rich$e.c4, rare.rich$e.c3, use = "complete.obs", method = "spearman"), 2) # 
  c3.c4.cor
  
  ggplot(aes(x = e.c4, y = e.c3), data = rare.rich) +
    geom_point(size = 1.5) +
    theme_bw() + 
    labs(x = xlab,
         y = ylab) +
    theme(axis.title = element_text(size = 14)) +
    #geom_abline(slope = -1, intercept = 45, size = 1) +
    annotate("text", x = 45, y = 40, label = "r = 0.20", size = 6)
  
  ggsave(save, plot = last_plot(), dpi = 500, scale = 1, device = "jpeg")
  
# -------------------------------------------------------------------
 
  
# 3. iNEXT model coefficients --------------------------------------
# Load model workspace (from iNEXT model v1)
  rm(list = ls())
  setwd("C:/Users/s436862/Dropbox/Poaceae")
  load("Results/iNEXT/Rdata/model_coefficients.RData")

# EV order rearrange: to do so, change at the beginning of iNEXT model v_ script   
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
  tot.rich$plot.names <- factor(ev.labels, ordered = is.ordered(ev.labels))
  tot.rich$plot.names <- fct_inorder(tot.rich$plot.names)
  
  c3.rich$plot.names <- factor(ev.labels, ordered = is.ordered(ev.labels))
  c3.rich$plot.names <- fct_inorder(c3.rich$plot.names)
  
  c4.rich$plot.names <- factor(ev.labels, ordered = is.ordered(ev.labels))
  c4.rich$plot.names <- fct_inorder(c4.rich$plot.names)
  
# total -------------------------------------------------------------------
# tot adjusted r2 & position: x = 1, y = 15, label = "Adj. r2 = exotic 0.46, native 0.50"
  n <- ggplot(tot.rich, aes(x = plot.names, color = status)) +
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
    annotate("text", x = 7, y = 15, label = "Adj. r2 = exotic 0.46, native 0.50")
  
  #ggThemeAssistGadget(h)
  
  n + theme(axis.text = element_text(size = 12, colour = "gray0"),
            axis.text.x = element_text(size = 12, angle = 45, hjust = 1), 
            axis.text.y = element_text(size = 12), 
            axis.title = element_text(size = 16),
            legend.text = element_text(size = 14), 
            legend.title = element_blank(), 
            axis.ticks.length = unit(0.2, "cm")) 
  
  ggsave("Results/iNEXT/Graphs/observed 15 rec 0.8 cov warn removed/Total coefficients.jpeg", plot = last_plot(), scale = 1, dpi = 500, device = "jpeg")
  
  
# C3 -------------------------------------------------------------------
# c3 adjusted r2 & position: x = 7, y = 5, label = "Adj. r2 = exotic 0.40, native 0.74"
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
    annotate("text", x = 7, y = 5, label = "Adj. r2 = exotic 0.40, native 0.74")
  
  #ggThemeAssistGadget(h)
  
  i + theme(axis.text = element_text(size = 12, colour = "gray0"),
            axis.text.x = element_text(size = 12, angle = 45, hjust = 1), 
            axis.text.y = element_text(size = 12), 
            axis.title = element_text(size = 16),
            legend.text = element_text(size = 14), 
            legend.title = element_blank(), 
            axis.ticks.length = unit(0.2, "cm")) 
  
  ggsave("Results/iNEXT/Graphs/observed 15 rec 0.8 cov warn removed/C3 coefficients.jpeg", plot = last_plot(), scale = 1, dpi = 500, device = "jpeg")
  
# C4 ---------------------------------------------------------------
# c4 position and r2: x = 7, y = 20, label = "Adj. r2 = exotic 0.41, native 0.57"
  
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
    annotate("text", x = 7, y = 20, label = "Adj. r2 = exotic 0.41, native 0.57")
  
  #ggThemeAssistGadget(h)
  
  j + theme(axis.text = element_text(size = 12, colour = "gray0"),
            axis.text.x = element_text(size = 12, angle = 45, hjust = 1), 
            axis.text.y = element_text(size = 12), 
            axis.title = element_text(size = 16),
            legend.text = element_text(size = 14), 
            legend.title = element_blank(), 
            axis.ticks.length = unit(0.2, "cm")) 
  
  ggsave("Results/iNEXT/Graphs/observed 15 rec 0.8 cov warn removed/C4 coefficients.jpeg", plot = last_plot(), scale = 1, dpi = 500, device = "jpeg")
  


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