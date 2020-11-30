#########################################################
# absolute richness maps
#########################################################

# note -------------------------------------------------
# native and non-native plotted on same scale
# e.g. look at C4
# change if required

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
  setwd("Results/rasters/iNEXT")
  
  current.list <- list.files(pattern = ".grd")
  names <- gsub(pattern = "\\.grd$", "", current.list)
  c.stack <- stack(current.list)
  names(c.stack) <- names
  list2env(setNames(unstack(c.stack), names(c.stack)), .GlobalEnv)# required 
  
  setwd("C:/Users/s436862/Dropbox/Poaceae")
  
# raw maps (v6) ---------------------------------------------------------
  ras_v5 <- function (title, raster, sr_scale, save)  
  {
    # AUS border + NA fill
    oz1 <- borders("world", region = "Australia", fill = "grey60")
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
                           limits = sr_scale,                             
                           space = "Lab",
                           na.value = "transparent",
                           guide = "colourbar",
                           name = "Species\nrichness") + 
      coord_equal() +
      coord_cartesian(xlim = c(112, 155), ylim = c(-45, -7), expand = F) +
      theme(legend.justification = "right",
            legend.position = "right",
            legend.key.size = unit(1, "cm"),
            legend.key.width = unit(1,"cm"),
            legend.title = element_text(size = 14),
            legend.text = element_text(size = 12),
            aspect.ratio = 0.88,
            plot.title = element_text(size = 20, face = "bold", hjust = 0.1, vjust = -0.5)
      ) +
      oz2
    ggsave(save, plot = last_plot(), scale = 1, dpi = 500, device = "jpeg")
    
    return(q)
  } 
  
# run function -------------------------------------------------------------------
# ras_v5(title, raster, sr_scale, save)  
  
# native
  ras_v5("Native total",
         native_total,
         c(0, cellStats(native_total, stat = 'max', na.rm = T)),
         "Results/maps/iNEXT/native total.jpeg")
  
  ras_v5("Native C3",
         native_C3,
         c(0, cellStats(native_C3, stat = 'max', na.rm = T)),
         "Results/maps/iNEXT/native C3.jpeg")
  
  ras_v5("Native C4",
         native_C4,
         c(0, cellStats(native_C4, stat = 'max', na.rm = T)),
         "Results/maps/iNEXT/native C4.jpeg")
  
# non-native
  ras_v5("Non-native total",
         nonnative_total,
         c(0, cellStats(native_total, stat = 'max', na.rm = T)),
         "Results/maps/iNEXT/nonnative total.jpeg")
  
  ras_v5("Non-native C3",
         nonnative_C3,
         c(0, cellStats(native_C3, stat = 'max', na.rm = T)),
         "Results/maps/iNEXT/nonnative C3.jpeg")
  
  ras_v5("Non-native C4",
         nonnative_C4,
         c(0, cellStats(native_C4, stat = 'max', na.rm = T)),
         "Results/maps/iNEXT/nonnative C4.jpeg")

# -------------------------------------------------------------------------------  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  