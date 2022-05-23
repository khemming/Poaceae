# library ----------------------------------
  library(tidyverse)
  library(ggThemeAssist)
  library(raster)
  library(gplots)
  library(RColorBrewer)
  library(ggmap)
  library(rgdal)
  library(rasterVis)
  library(maptools)
  library(ggthemes)
  library(forcats)
  library(maps)
  library(ggsn)
  
  rm(list = ls())

# data ---------------------------------
# shape file
  oz <- readOGR("Data files/Australia/Australia shapefile.shp")
  plot(oz)

# Human influence
  hii <- read.csv("Results/csv/predictor variables 2538.csv") %>% 
         dplyr::select(hii)
 
# observed richness
  current.list <- list.files(path = "Results/rasters/scaled",
                             pattern = ".grd", full.names = T)
  names <- gsub(pattern = "Results/rasters/scaled/|.grd$", "", current.list)
  c.stack <- stack(current.list)
  names(c.stack) <- names
  list2env(setNames(unstack(c.stack), names(c.stack)), .GlobalEnv)
  
# predicted richness
  current.list <- list.files(path = "Results/rasters/predicted",
                             pattern = ".grd", full.names = T)
  names <- gsub(pattern = "Results/rasters/predicted/|.grd$", "", current.list)
  c.stack <- stack(current.list)
  names(c.stack) <- names
  list2env(setNames(unstack(c.stack), names(c.stack)), .GlobalEnv)
  
# calculate negative invasion potential -------------------------
# requires: 
# predicted raster = native predicted
# observed raster  = non-native observed
# save_name        = status, pathway, richness category
  rr_pot <- function(pred_raster, obs_raster, save_name) {
    
    obs_raster[is.na(obs_raster[])] <- 0 
    rr_pot1 <- pred_raster - obs_raster
    rr_pot2 <- calc(rr_pot1, fun = function(x) {x[x>0] <- NA; return(x)})
    rasterfile <- paste0("Results/rasters/negative invasion potential/", save_name, ".grd")
    writeRaster(rr_pot2, rasterfile, overwrite = T)
    return(plot(rr_pot2))
  }
  
# exotic total negative invasion potential (nip)
  rr_pot(native_total_predicted, nonnative_total, "tot_nip")

# exotic C3 
  rr_pot(native_C3_predicted, nonnative_C3, "C3_nip")
  
# exotic C4 
  rr_pot(native_C4_predicted, nonnative_C4, "C4_nip")
  
# invasion potentital --------------------------------------------  
# maintains scaled legend to 0 and 1
  po_v10 <- function(raster, title, lims, sr_breaks, legend_title){
    
# spatial points dataframe
    raster_spdf <- as(raster, "SpatialPixelsDataFrame")
    raster_df <- as.data.frame(raster_spdf)
    colnames(raster_df) <- c("value", "x", "y")
    
# species richness colours
    colr <- rev(brewer.pal(11, "Spectral"))
    
 # save
    save_txt <- paste0("Results/maps/negative invasion potential/", title, ".jpeg")
    
# plot
    q <- ggplot() +
      ggtitle(title) +
      geom_polygon(data = oz, aes(x = long, y = lat, group = group),
                   fill = "grey60") +
      geom_tile(data = raster_df, aes(x = x, y = y, fill = value)) +
      geom_polygon(data = oz, colour = "grey1",
                   aes(x = long, y = lat, group = group), fill = NA, size = 0.7) +
      scale_fill_gradientn(colours = rev(colr),
                           limits = lims,
                           breaks = sr_breaks,
                           labels = sr_breaks,
                           space = "Lab",
                           name = legend_title) +
      coord_fixed(ratio = 38/33, xlim = c(112, 155), ylim = c(-45, -7)) +
      theme_map() +
      theme(legend.direction = "vertical",
            legend.justification = "right",
            legend.position = "right",
            legend.key.size = unit(18, "cm"),
            legend.key.width = unit(1,"cm"),
            legend.title = element_text(size = 16),
            legend.text = element_text(size = 12),
            legend.box.spacing = unit(0.01, "cm"),
            plot.title = element_text(size = 20, face = "bold", hjust = 0.1, vjust = -0.5)) +
      guides(fill = guide_colorbar(barheight = unit(5, "cm")))
    
    plot(q)
    
    ggsave(save_txt, plot = last_plot(), width = 15, height = 15, units = "cm", dpi = 500, device = "jpeg")
  } # finish
  
# invasion potential maps -------------------
# total
  po_v10(raster <- raster("Results/rasters/negative invasion potential/tot_nip.grd"),
         title <- "Total exotic NIP",
         lims <- c(-0.25, 0),
         sr_breaks <- c(-0.25, -0.1, 0),
         legend_title <- "Scaled\nlog-\nspecies\nrichness")
# C3  
  po_v10(raster <- raster("Results/rasters/negative invasion potential/C3_nip.grd"),
         title <- "C3 exotic NIP",
         lims <- c(-0.54, 0),
         sr_breaks <- c(-0.5, -0.25, 0),
         legend_title <- "Scaled\nlog-\nspecies\nrichness")
# C4
  po_v10(raster <- raster("Results/rasters/negative invasion potential/C4_nip.grd"),
         title <- "C4 exotic NIP",
         lims <- c(-0.50, 0),
         sr_breaks <- c(-0.5, -0.25, 0),
         legend_title <- "Scaled\nlog-\nspecies\nrichness")
  
# correlation with HII --------------------------------------
  df <- data.frame(hii,
                   tot_nip = getValues(raster("Results/rasters/negative invasion potential/tot_nip.grd")),
                   C3_nip = getValues(raster("Results/rasters/negative invasion potential/C3_nip.grd")),
                   C4_nip = getValues(raster("Results/rasters/negative invasion potential/C4_nip.grd")))

  head(df)  

  hii_tot <- cor(df$hii, abs(df$tot_nip), use = "complete.obs", method = "pearson")
  hii_tot

  hii_C3 <- cor(df$hii, df$C3_nip, use = "complete.obs", method = "pearson")
  hii_C3
  
  hii_C4 <- cor(df$hii, abs(df$C4_nip), use = "complete.obs", method = "pearson")
  hii_C4
  
# correlation scatter plots -------------------------
# note: expand might need tweaking
  corr_fun <- function(dat, title, xlab, ylab, cor, label, save) {
    q <- ggplot(aes(x = x, y = y), data = dat) +
      geom_point(shape = "circle", size = 2) +
    #geom_abline(intercept = 0, slope = 1, size = 1, linetype = "dashed") +
      theme_bw() + 
      labs(x = xlab, y = ylab) +
      theme(panel.border = element_blank(),
            plot.title = element_text(size = 22),
            axis.title.x = element_text(size = 18),
            axis.title.y = element_text(size = 18),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(), 
            axis.line = element_line(colour = "black", size = 1),
            axis.text.x = element_text(colour = "black", size = 18),
            axis.text.y = element_text(colour = "black", size = 18),
            axis.ticks.length = unit(0.25, "cm"),
            axis.ticks = element_line(colour = "black", size = 1)) +
      scale_x_continuous() +
      scale_y_continuous(breaks = sr_breaks, 
                         limits = lims, 
                         expand = c(0, 0.05)) +
      annotate("text", x = 3, y = -0.5, label = label, size = 8) 
    ggsave(save, plot = last_plot(), dpi = 500, width = 13, height = 12, units = "cm", device = "jpeg")  
    q
    return(q)
  }
# -----------------------------------------------------------------------  
  
# total NIP ------------------------------------------------
# data
  dat <- df
  dat$x <- df$hii
  dat$y <- df$tot_nip
# labels
  title <- "Total NIP and human influence"
  xlab <- "Human influence index (scaled)"
  ylab <- "Negative invasion potential (scaled)" 
# correlation
  cor <- sprintf("%.2f", cor(df$hii, df$tot_nip, use = "complete.obs", method = "spearman"), 2)
  label <- paste0("r = ", cor)
# save
  save <- paste0("Results/correlation plots/", title, ".jpeg")
# run  
  corr_fun(dat, title, xlab, ylab, cor, label, save)
  
# C3 NIP ------------------------------------------------
# data
  dat <- df
  dat$x <- df$hii
  dat$y <- df$C3_nip
# labels
  title <- "C3 NIP and human influence"
  xlab <- "Human influence index (scaled)"
  ylab <- "Negative invasion potential (scaled)" 
# correlation
  cor <- sprintf("%.2f", cor(df$hii, df$C3_nip, use = "complete.obs", method = "spearman"), 2)
  label <- paste0("r = ", cor)
# save
  save <- paste0("Results/correlation plots/", title, ".jpeg")
# run  
  corr_fun(dat, title, xlab, ylab, cor, label, save)
  
# C4 NIP ------------------------------------------------
  # data
  dat <- df
  dat$x <- df$hii
  dat$y <- df$C4_nip
  # labels
  title <- "C4 NIP and human influence"
  xlab <- "Human influence index (scaled)"
  ylab <- "Negative invasion potential (scaled)" 
  # correlation
  cor <- sprintf("%.2f", cor(df$hii, df$C4_nip, use = "complete.obs", method = "spearman"), 2)
  label <- paste0("r = ", cor)
  # save
  save <- paste0("Results/correlation plots/", title, ".jpeg")
  # run  
  corr_fun(dat, title, xlab, ylab, cor, label, save)
  
# ---------------------------------------  
  
  