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
  library(tidyverse)
  library(forcats)
  library(maps)

  rm(list = ls())

# data -------------------------------------------------------------------
  setwd("C:/Users/s436862/Dropbox/Poaceae/Results/iNEXT/Rasters/proportional species richness")
  
  current.list <- list.files(pattern = ".grd")
  names <- gsub(pattern = "\\.grd$", "", current.list)
  
  c.stack <- stack(current.list)
  names(c.stack) <- names
  
  list2env(setNames(unstack(c.stack), names(c.stack)), .GlobalEnv)# required 
  
  setwd("C:/Users/s436862/Dropbox/Poaceae")
  
# raster map -------------------------------------------------------------
  ras_v4 <- function (title, save, raster, scale)  
    
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
      theme_map() +
      ggtitle(title) +
      oz1 +
      geom_raster(aes(fill = value)) +
      scale_fill_gradientn(colours = colour, 
                           limits = scale,                               
                           space = "Lab",
                           na.value = "transparent",
                           guide = "colourbar",
                           name = "Relative\nspecies\nrichness") + 
      coord_equal() +
      coord_cartesian(xlim = c(112, 155), ylim = c(-45, -7), expand = F) +
      theme(legend.justification = "right",
            legend.position = "right",
            legend.key.size = unit(1, "cm"),
            legend.key.width = unit(1,"cm"),
            legend.title = element_text(size = 16),
            legend.text = element_text(size = 14),
            aspect.ratio = 0.88,
            plot.title = element_text(size = 26, face = "bold")
      ) +
      oz2
    print(q)
    
    ggsave(save, plot = last_plot(), scale = 1, dpi = 500, device = "jpeg")
    
  } # finish function
  
# function ---------------------------------------------------------------------
# required: title, save, raster, legend scale
  
# observed ---------------------------------------------------------------------
# required: title, save, raster, scale
# native total 
  ras_v4("Observed total native richness",
         "Results/iNEXT/Graphs/observed.total.native.richness.jpeg", 
         obs.nat.tot, 
         c(0, cellStats(obs.nat.tot, stat = 'max', na.rm = T)))
  
# native C3 
  ras_v4("Observed C3 native richness",
         "Results/iNEXT/Graphs/observed.C3.native.richness.jpeg", 
         obs.nat.c3, 
         c(0, cellStats(obs.nat.c3, stat = 'max', na.rm = T)))
  
# native C4 
  ras_v4("Observed C4 native richness",
         "Results/iNEXT/Graphs/observed.C4.native.richness.jpeg", 
         obs.nat.c4, 
         c(0, cellStats(obs.nat.c4, stat = 'max', na.rm = T)))
  
# non-native total 
  ras_v4("Observed non-native total richness",
         "Results/iNEXT/Graphs/observed.total.non.native.richness.jpeg", 
         obs.nonnat.tot, 
         c(0, cellStats(obs.nonnat.tot, stat = 'max', na.rm = T)))
  
# non-native C3 
  ras_v4("Observed non-native C3 richness",
         "Results/iNEXT/Graphs/observed.C3.non.native.richness.jpeg", 
         obs.nonnat.c3, 
         c(0, cellStats(obs.nonnat.c3, stat = 'max', na.rm = T)))
  
# non-native C4 
  ras_v4("Observed non-native C4 richness",
         "Results/iNEXT/Graphs/observed.C4.non.native.richness.jpeg", 
         obs.nonnat.c4, 
         c(0, cellStats(obs.nat.c4, stat = 'max', na.rm = T)))
  
# predicted -----------------------------------------------------------------
# native total 
  ras_v4("Predicted native total richness",
         "Results/iNEXT/Graphs/predicted.total.native.richness.jpeg", 
         pred.nat.tot, 
         c(0, cellStats(pred.nat.tot, stat = 'max', na.rm = T)))
  
# native C3 
  ras_v4("Predicted native C3 richness",
         "Results/iNEXT/Graphs/predicted.C3.native.richness.jpeg", 
         pred.nat.c3, 
         c(0, cellStats(pred.nat.c3, stat = 'max', na.rm = T)))
  
# native C4 
  ras_v4("Predicted native C4 richness",
         "Results/iNEXT/Graphs/predicted.C4.native.richness.jpeg", 
         pred.nat.c4, 
         c(0, cellStats(pred.nat.c4, stat = 'max', na.rm = T)))
  
# potential richness ------------------------------------------------------------------  
# non-native total 
  ras_v4("Potential non-native total richness",
         "Results/iNEXT/Graphs/potential.total.non.native.richness.jpeg", 
         total.nonnat.potential.relative.richness, 
         c(0, cellStats(total.nonnat.potential.relative.richness, stat = 'max', na.rm = T)))
  
# non-native C3 
  ras_v4("Potential non-native C3 richness",
         "Results/iNEXT/Graphs/potential.C3.non.native.richness.jpeg", 
         C3.nonnat.potential.relative.richness, 
         c(0, cellStats(C3.nonnat.potential.relative.richness, stat = 'max', na.rm = T)))
  
# non-native C4 
  ras_v4("Potential non-native C4 richness",
         "Results/iNEXT/Graphs/potential.C4.non.native.richness.jpeg", 
         C4.nonnat.potential.relative.richness, 
         c(0, cellStats(C4.nonnat.potential.relative.richness, stat = 'max', na.rm = T)))

# ------------------------------------------------------------------------------------