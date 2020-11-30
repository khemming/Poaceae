

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
# observed 
  setwd("C:/Users/s436862/Dropbox/Poaceae/Results/rasters/scaled")
  current.list <- list.files(pattern = ".grd")
  names_long <- gsub(pattern = "\\.grd$", "", current.list)
  ob.stack <- stack(current.list)
  names(ob.stack) <- names_long
  list2env(setNames(unstack(ob.stack), names(ob.stack)), .GlobalEnv)
  
# predicted  
  setwd("C:/Users/s436862/Dropbox/Poaceae/Results/rasters/predicted")
  current.list <- list.files(pattern = ".grd")
  names_long <- gsub(pattern = "\\.grd$", "", current.list)
  pr.stack <- stack(current.list)
  names(pr.stack) <- names_long
  list2env(setNames(unstack(pr.stack), names(pr.stack)), .GlobalEnv)
  
  setwd("C:/Users/s436862/Dropbox/Poaceae")
  
# EDA: normality -------------------------------------------------------
  hist(native_C3_predicted) # skewed
  hist(native_C4_predicted) # fairly normal
  hist(native_total_predicted) # skewed
  
  
  
# scatterlot function -----------------------------------------------------------  
# plot aesthetics

# note: expand might need tweaking
  corr_fun <- function(dat, title, xlab, ylab, cor, label, save) {
   q <- ggplot(aes(x = x, y = y), data = dat) +
      geom_point(shape = "circle", size = 2) +
      geom_abline(intercept = 0, slope = 1, size = 1, linetype = "dashed") +
      theme_bw() + 
      labs(x = xlab, y = ylab) +
      theme(panel.border = element_blank(),
            plot.title = element_text(size = 22),
            axis.title.x = element_text(size = 18),
            axis.title.y = element_text(size = 18),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(), 
            axis.line = element_line(colour = "black", size = 1),
            axis.text.x = element_text(colour = "black", size = 16),
            axis.text.y = element_text(colour = "black", size = 16),
            axis.ticks.length = unit(0.25, "cm"),
            axis.ticks = element_line(colour = "black", size = 1)) +
      scale_x_continuous(breaks = c(0, 0.5, 1), 
        limits = c(0, 1), 
        expand = c(0, 0.05)) +
      scale_y_continuous(breaks = c(0, 0.5,  1), 
        limits = c(0, 1), 
        expand = c(0, 0.05)) +
      annotate("text", x = 0.13, y = 1, label = label, size = 8) 
      ggsave(save, plot = last_plot(), dpi = 500, width = 13, height = 12, units = "cm", device = "jpeg")  
    q
    return(q)
  }
# -----------------------------------------------------------------------  
  
# C3 native observed x
#    nonnative observed ------------------------------------------------
# data
  x <- getValues(native_C3)
  y <- getValues(nonnative_C3)
  dat <- data.frame(cbind(x, y))
# labels
  title <- "C3 native-nonnative correlation"
  xlab <- "Observed native richness (scaled)"
  ylab <- "Nonnative richness (scaled)" 
# correlation
  cor <- sprintf("%.2f", cor(x, y, use = "complete.obs", method = "spearman"), 2)
  label <- paste0("r = ", cor)
# save
  save <- paste0("Results/correlation plots/", title, ".jpeg")
# run  
  corr_fun(dat, title, xlab, ylab, cor, label, save)


# C4 native observed x
#    nonnative observed ------------------------------------------------
# data
  x <- getValues(native_C4)
  y <- getValues(nonnative_C4)
  dat <- data.frame(cbind(x, y))
# labels
  title <- "C4 native-nonnative correlation"
  xlab <- "Observed native richness (scaled)"
  ylab <- "Nonnative richness (scaled)" 
# correlation
  cor <- sprintf("%.2f", cor(x, y, use = "complete.obs", method = "spearman"), 2)
  label <- paste0("r = ", cor)
# save
  save <- paste0("Results/correlation plots/", title, ".jpeg")
  
# run  
  corr_fun(dat, title, xlab, ylab, cor, label, save)

# ---------------------------------------------------------------------------  
  
# total native observed x
#    nonnative observed ------------------------------------------------
# data
  x <- getValues(native_total)
  y <- getValues(nonnative_total)
  dat <- data.frame(cbind(x, y))
# labels
  title <- "Native-nonnative total correlation"
  xlab <- "Native richness (scaled)"
  ylab <- "Nonnative richness (scaled)" 
# correlation
  cor <- sprintf("%.2f", cor(x, y, use = "complete.obs", method = "spearman"), 2)
  label <- paste0("r = ", cor)
# save
  save <- paste0("Results/correlation plots/", title, ".jpeg")
  
# run  
  corr_fun(dat, title, xlab, ylab, cor, label, save)
# ---------------------------------------------------------------------------
 
# supplementary figures -----------------------------------------------------     
# C3 native observed x
#    native predicted ----------------------------------------------------
# data
  x <- getValues(native_C3)
  y <- getValues(native_C3_predicted)
  dat <- data.frame(cbind(x, y))
# labels
  title <- "Native C3 observed-predicted correlation"
  xlab <- "Observed C3 richness (scaled)"
  ylab <- "Predicted C3 richness (scaled)" 
# correlation
  cor <- sprintf("%.2f", round(cor(x, y, use = "complete.obs", method = "spearman"), 2))
  label <- paste0("r = ", cor)
# save
  save <- paste0("Results/correlation plots/", title, ".jpeg")

# run  
  corr_fun(dat, title, xlab, ylab, cor, label, save)
# ---------------------------------------------------------------------------
  
# C4 native observed x
#    native predicted ----------------------------------------------------
# data
  x <- getValues(native_C4)
  y <- getValues(native_C4_predicted)
  dat <- data.frame(cbind(x, y))
# labels
  title <- "Native C4 observed-predicted correlation"
  xlab <- "Observed C4 richness (scaled)"
  ylab <- "Predicted C4 richness (scaled)" 
# correlation
  cor <- sprintf("%.2f", round(cor(x, y, use = "complete.obs", method = "spearman"), 2))
  label <- paste0("r = ", cor)
# save
  save <- paste0("Results/correlation plots/", title, ".jpeg")
  
# run  
  corr_fun(dat, title, xlab, ylab, cor, label, save)
  
# --------------------------------------------------------------------------
  
# total native observed x
#       native predicted ----------------------------------------------------
# data
  x <- getValues(native_total)
  y <- getValues(native_total_predicted)
  dat <- data.frame(cbind(x, y))
# labels
  title <- "Native total observed-predicted correlation"
  xlab <- "Observed richness (scaled)"
  ylab <- "Predicted richness (scaled)" 
# correlation
  cor <- sprintf("%.2f", round(cor(x, y, use = "complete.obs", method = "spearman"), 2))
  label <- paste0("r = ", cor)
# save
  save <- paste0("Results/correlation plots/", title, ".jpeg")
  
# run  
  corr_fun(dat, title, xlab, ylab, cor, label, save)
  
  
# C3 native observed x
# C4                   ----------------------------------------------------
# data
  x <- getValues(native_C3)
  y <- getValues(native_C4)
  dat <- data.frame(cbind(x, y))
# labels
  title <- "Native C3- C4-observed correlation"
  xlab <- "Observed C3 richness (scaled)"
  ylab <- "Observed C4 richness (scaled)" 
# correlation
  cor <- sprintf("%.2f", round(cor(x, y, use = "complete.obs", method = "spearman"), 2))
  label <- paste0("r = ", cor)
# save
  save <- paste0("Results/correlation plots/", title, ".jpeg")
  
# run  
  corr_fun(dat, title, xlab, ylab, cor, label, save)
  
# C3 native predicted x
# C4                    ----------------------------------------------------
# data
  x <- getValues(native_C3_predicted)
  y <- getValues(native_C4_predicted)
  dat <- data.frame(cbind(x, y))
# labels
  title <- "Native C3- C4-predicted correlation"
  xlab <- "Predicted C3 richness (scaled)"
  ylab <- "Predicted C4 richness (scaled)" 
# correlation
  cor <- sprintf("%.2f", round(cor(x, y, use = "complete.obs", method = "spearman"), 2))
  label <- paste0("r = ", cor)
# save
  save <- paste0("Results/correlation plots/", title, ".jpeg")
  
# run  
  corr_fun(dat, title, xlab, ylab, cor, label, save)
  
# C3 nonnative predicted x
#              observed       ---------------------------------------------
# data
  x <- getValues(nonnative_C3_predicted)
  y <- getValues(nonnative_C3)
  dat <- data.frame(cbind(x, y))
# labels
  title <- "Nonnative C3 observed x predicted correlation"
  xlab <- "Predicted C3 richness (scaled)"
  ylab <- "Observed C3 richness (scaled)" 
# correlation
  cor <- sprintf("%.2f", round(cor(x, y, use = "complete.obs", method = "spearman"), 2))
  label <- paste0("r = ", cor)
# save
  save <- paste0("Results/correlation plots/", title, ".jpeg")
  
# run  
  corr_fun(dat, title, xlab, ylab, cor, label, save)
  
# C4 nonnative predicted x
#              observed       ---------------------------------------------
# data
  x <- getValues(nonnative_C4_predicted)
  y <- getValues(nonnative_C4)
  dat <- data.frame(cbind(x, y))
# labels
  title <- "Nonnative C4 observed x predicted correlation"
  xlab <- "Predicted C4 richness (scaled)"
  ylab <- "Observed C4 richness (scaled)" 
# correlation
  cor <- sprintf("%.2f", round(cor(x, y, use = "complete.obs", method = "spearman"), 2))
  label <- paste0("r = ", cor)
# save
  save <- paste0("Results/correlation plots/", title, ".jpeg")
  
# run  
  corr_fun(dat, title, xlab, ylab, cor, label, save)
  
  
# --------------------------------------------------------------------------