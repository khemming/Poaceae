

# library ------------------------------------------------
  library(ggThemeAssist)
  library(raster)
  library(RColorBrewer)
  library(tidyverse)
  
  rm(list = ls())

# data ----------------------------------------------------------------------
  setwd("C:/Users/s436862/Dropbox/Poaceae/Results/simulated results")
  
  current.list <- list.files(pattern = ".grd")
  names <- gsub(pattern = ".grd$", "", current.list)
  c_stack <- stack(current.list)
  names(c_stack) <- names
  list2env(setNames(unstack(c_stack), names(c_stack)), .GlobalEnv)
  
  spp <- as.data.frame(c_stack, na.rm = F)
  glimpse(spp)
  
  setwd("C:/Users/s436862/Dropbox/Poaceae")

# scatterlot function -----------------------------------------------------------  
# note: expand might need tweaking
  corr_fun <- function(dat, title, xlab, ylab, cor, label, save){
    q <- ggplot(aes(x = x, y = y), data = dat) +
      geom_point(shape = "circle", size = 2) +
      geom_abline(intercept = 0, slope = 1, size = 1, linetype = "dashed") +
      theme_bw() + 
      labs(x = xlab, y = ylab) +
      theme(panel.border = element_blank(),
            plot.title = element_text(size = 20),
            axis.title.x = element_text(size = 20),
            axis.title.y = element_text(size = 20),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(), 
            axis.line = element_line(colour = "black", size = 1),
            axis.text.x = element_text(colour = "black", size = 18),
            axis.text.y = element_text(colour = "black", size = 18),
            axis.ticks.length = unit(0.25, "cm"),
            axis.ticks = element_line(colour = "black", size = 1)) +
      scale_x_continuous(breaks = c(0, 0.5, 1), 
                         limits = c(0, 1), 
                         #expand = c(0, 0.05)
                         ) +
      scale_y_continuous(breaks = c(0, 0.5,  1), 
                         limits = c(0, 1), 
                         #expand = c(0, 0.05)
                         ) +
      annotate("text", x = 0.15, y = 1, label = label, size = 8) 
    ggsave(save, plot = last_plot(), dpi = 500, width = 12, height = 11, units = "cm", device = "jpeg")  
    return(q)
  }

# -----------------------------------------------------------------------  
  
# Complete invasion of nonnative richness ---------------------------------
# data
  x <- getValues(native.complete)
  y <- getValues(nonnative.complete)
  dat <- data.frame(cbind(x, y))
# labels
  title <- "Completed nonnative invasion"
  xlab <- "Native richness (scaled)"
  ylab <- "Exotic richness (scaled)" 
# correlation
  cor <- sprintf("%.2f", round(cor(x, y, use = "complete.obs", method = "spearman"), 2))
  label <- paste0("r = ", cor)
# save
  save <- paste0("Results/simulated results/", title, ".jpeg")
# run  
  corr_fun(dat, title, xlab, ylab, cor, label, save)
 
# Nonnative invasion potential -------------------------------------------------
# data
  x <- getValues(native.complete)
  y <- getValues(nonnative.incomplete)
  dat <- data.frame(cbind(x, y))
# labels
  title <- "Exotic invasion potential"
  xlab <- "Native richness (scaled)"
  ylab <- "Exotic richness (scaled)" 
# correlation
  cor <- sprintf("%.2f", round(cor(x, y, use = "complete.obs", method = "spearman"), 2))
  label <- paste0("r = ", cor)
# save
  save <- paste0("Results/simulated results/", title, ".jpeg")
# run  
  corr_fun(dat, title, xlab, ylab, cor, label, save)

# No nonnative invasion potential --------------------------------------------------
# data
  x <- getValues(native.complete)
  y <- getValues(nonnative.no.correlation)
  dat <- data.frame(cbind(x, y))
# labels
  title <- "No nonnative invasion potential"
  xlab <- "Native richness (scaled)"
  ylab <- "Exotic richness (scaled)" 
# correlation
  cor <- sprintf("%.2f", round(cor(x, y, use = "complete.obs", method = "spearman"), 2))
  label <- paste0("r = ", cor)
# save
  save <- paste0("Results/simulated results/", title, ".jpeg")
# run  
  corr_fun(dat, title, xlab, ylab, cor, label, save)

# ---------------------------------------------------------------------------------    
  
  