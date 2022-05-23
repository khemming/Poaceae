#########################################################
# scatter plots for native and non-native regressions for absolute richness
#########################################################


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

  rm(list = ls())

# data -----------------------------------------------------------------
# iNEXT species richness (absolute richness)
  setwd("Results/rasters/iNEXT")
  
  current.list <- list.files(pattern = ".grd")
  names <- gsub(pattern = "\\.grd$", "", current.list)
  
  c.stack <- stack(current.list)
  names(c.stack) <- names
  
  list2env(setNames(unstack(c.stack), names(c.stack)), .GlobalEnv)# required 

  spp <- as.data.frame(getValues(c.stack))
  
  setwd("C:/Users/s436862/Dropbox/Poaceae")
  
# scatterlots ----------------------------------------------------------  
# total richness  ------------------------------------------------------
  x <- spp$native_total
  y <- spp$nonnative_total
  
  x_lab <- "Total native richness"
  y_lab <- "Total non-native richness"
  
  cor <- cor(x, y, use = "complete.obs", method = "spearman")
  cor_lab <- paste0("r = ", sprintf("%.2f", round(cor, digits = 2)))
  
# plot
  ggplot(aes(x = x, y = y), data = spp) +
    geom_point(shape = "circle", size = 1.5) +
    theme_bw() + 
    theme(panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), 
          axis.title = element_text(size = 18),
          axis.line = element_line(colour = "black", size = 1),
          axis.text.x = element_text(colour = "black", size = 14),
          axis.text.y = element_text(colour = "black", size = 14),
          axis.ticks = element_line(size = 1)) +
  # scale_x_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1), 
  #                    expand = c(0, 0.05),
  #                    limits = c(0, 1)) +
  # scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1),
  #                    limits = c(0, 1),
  #                    expand = c(0, 0.05)) +
    annotate("text", x = 0.95*max(x, na.rm = T), y = 0.95*max(y, na.rm = T), 
             label = cor_lab, size = 5) +
    labs(x = x_lab,
         y = y_lab)
  
  ggsave("Results/correlation plots/Total.jpeg", plot = last_plot(), dpi = 500, scale = 1, device = "jpeg")
  

# C3 richness  ---------------------------------------------------
# total richness  ------------------------------------------------------
  x <- spp$native_C3
  y <- spp$nonnative_C3
  
  x_lab <- "C3 native richness"
  y_lab <- "C3 non-native richness"
  
  cor <- cor(x, y, use = "complete.obs", method = "spearman")
  cor_lab <- paste0("r = ", sprintf("%.2f", round(cor, digits = 2)))
  
# plot
  ggplot(aes(x = x, y = y), data = spp) +
    geom_point(shape = "circle", size = 1.5) +
    theme_bw() + 
    theme(panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), 
          axis.title = element_text(size = 18),
          axis.line = element_line(colour = "black", size = 1),
          axis.text.x = element_text(colour = "black", size = 14),
          axis.text.y = element_text(colour = "black", size = 14),
          axis.ticks = element_line(size = 1)) +
  # scale_x_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1), 
  #                    expand = c(0, 0.05),
  #                    limits = c(0, 1)) +
  # scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1),
  #                    limits = c(0, 1),
  #                    expand = c(0, 0.05)) +
    annotate("text", x = 0.95*max(x, na.rm = T), y = 0.95*max(y, na.rm = T), 
             label = cor_lab, size = 5) +
    labs(x = x_lab,
         y = y_lab)
  
  ggsave("Results/correlation plots/C3.jpeg", plot = last_plot(), dpi = 500, scale = 1, device = "jpeg")
  
# C4 richness  ---------------------------------------------------
# total richness  ------------------------------------------------------
  x <- spp$native_C4
  y <- spp$nonnative_C4
  
  x_lab <- "C4 native richness"
  y_lab <- "C4 non-native richness"
  
  cor <- cor(x, y, use = "complete.obs", method = "spearman")
  cor_lab <- paste0("r = ", sprintf("%.2f", round(cor, digits = 2)))
  
# plot
  ggplot(aes(x = x, y = y), data = spp) +
    geom_point(shape = "circle", size = 1.5) +
    theme_bw() + 
    theme(panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), 
          axis.title = element_text(size = 18),
          axis.line = element_line(colour = "black", size = 1),
          axis.text.x = element_text(colour = "black", size = 14),
          axis.text.y = element_text(colour = "black", size = 14),
          axis.ticks = element_line(size = 1)) +
  # scale_x_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1), 
  #                    expand = c(0, 0.05),
  #                    limits = c(0, 1)) +
  # scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1),
  #                    limits = c(0, 1),
  #                    expand = c(0, 0.05)) +
    annotate("text", x = 0.95*max(x, na.rm = T), y = 0.95*max(y, na.rm = T), 
             label = cor_lab, size = 5) +
    labs(x = x_lab,
         y = y_lab)
  
  ggsave("Results/correlation plots/C4.jpeg", plot = last_plot(), dpi = 500, scale = 1, device = "jpeg")
  
# ----------------------------------------------------------  