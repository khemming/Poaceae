#########################################################
# scatter plots for native and non-native regressions
#########################################################


# aim -------------------------------------------------------------------
# plots: (1) total-total 
#        (2) C3-C4 
#        (3) C4-C4
#        (4) Native C3-C4
#        (5) Non-native C3-C4

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

# data ------------------------------------------------
  setwd("C:/Users/s436862/Dropbox/Poaceae/Results/iNEXT/Rasters/observed")
  
  current.list <- list.files(pattern = ".grd")
  names <- gsub(pattern = "\\.grd$", "", current.list)
  
  c.stack <- stack(current.list)
  names(c.stack) <- names
  
  list2env(setNames(unstack(c.stack), names(c.stack)), .GlobalEnv)# required 

  rich <- as.data.frame(getValues(c.stack))
  
   setwd("C:/Users/s436862/Dropbox/Poaceae/Results/iNEXT/Graphs/scatterplots")  

# scatterlots ------------------------------------  
# (1) total-total ------------------------------------
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


# (2) C3-C3 ----------------------------------------------------------
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

# (3) C4-C4 ---------------------------------------------------    
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
  
# (4) native c3-c4 --------------------------------------------------------- 
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
  
  
# (5) Non-native C3-C4 ------------------------------------------------------
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
 
  
