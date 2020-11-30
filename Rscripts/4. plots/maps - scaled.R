
# library ------------------------------------------------
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
  
# data ---------------------------------------------------
# shape file
  oz <- readOGR("Data files/Australia/Australia shapefile.shp")
  plot(oz)
  
# iNEXT richness for scale bar
  setwd("C:/Users/s436862/Dropbox/Poaceae/Results/rasters/iNEXT")
  current.list <- list.files(pattern = ".grd")
  inext_names <- gsub(pattern = "\\.grd$", "", current.list)
  inext <- stack(current.list)
  names(inext) <- inext_names
  list2env(setNames(unstack(inext), names(inext)), .GlobalEnv)
  st_scale <- stack(native_C3, native_C4, native_total, 
                    nonnative_C3, nonnative_C4, nonnative_total)
  
# observed 
  setwd("C:/Users/s436862/Dropbox/Poaceae/Results/rasters/scaled")
  current.list <- list.files(pattern = ".grd")
  names_long <- gsub(pattern = "\\.grd$", "", current.list)
  ob.stack <- stack(current.list)
  names(ob.stack) <- names_long
  
# predicted  
  setwd("C:/Users/s436862/Dropbox/Poaceae/Results/rasters/predicted")
  current.list <- list.files(pattern = ".grd")
  names_long <- gsub(pattern = "\\.grd$", "", current.list)
  pr.stack <- stack(current.list)
  names(pr.stack) <- names_long
 
# potential  
  setwd("C:/Users/s436862/Dropbox/Poaceae/Results/rasters/potential")
  current.list <- list.files(pattern = ".grd")
  names_long <- gsub(pattern = "\\.grd$", "", current.list)
  po.stack <- stack(current.list)
  names(po.stack) <- names_long
 
  setwd("C:/Users/s436862/Dropbox/Poaceae")
  
# scale bar --------------------------------------------------------------
# iNEXT species richness to scale map legends
  st_scale[[1]]
  st_scale[[2]]
  st_scale[[3]]
  st_scale[[4]]
  st_scale[[5]]
  st_scale[[6]]
  
  leg_lab <- list(c(55,  30, 10,  5, 1),  # native C3
                  c(160, 80, 40, 10, 1),  # native C4
                  c(150, 70, 30, 10, 1),  # total 
                  c(40,  20, 10,  5, 1),  # nn C3
                  c(50,  25, 10,  5, 1),  # nn C4
                  c(75,  50, 20, 10, 1))  # nn total

# map requirements v10 ---------------------------------------------------------------  
# raster = raster
# title = plot and save title
# sr_breaks = the raw species richness legend labels (i.e. leg_lab)
# sr_max = max species richness
# legend_title = legend units 
  
# observed maps v10 ---------------------------------------------------------
  obs_v10 <- function(raster, title, sr_breaks, sr_max, legend_title){
    
  # scale breaks and labels
    log_sr <- log(sr_breaks)
    scale_sr <- log_sr/max(log_sr)
    
    log_mx <- log(cellStats(sr_max, "max", na.rm = T))
    scale_mx <- log_mx/max(log_sr)
    
    leg_scale <- scale_sr / scale_mx
      
  # spatial points dataframe 
    raster_spdf <- as(raster, "SpatialPixelsDataFrame")
    raster_df <- as.data.frame(raster_spdf)
    colnames(raster_df) <- c("value", "x", "y")
  
  # species richness colours
    colr <- rev(brewer.pal(11, "Spectral"))
  # save 
    save_txt <- paste0("results/maps/scaled/", title, ".jpeg")  
    
  # plot
    q <- ggplot() +  
      ggtitle(title) +
      geom_polygon(data = oz, aes(x = long, y = lat, group = group),
                   fill = "grey60") +
      geom_tile(data = raster_df, aes(x = x, y = y, fill = value)) +             
      geom_polygon(data = oz, colour = "grey1", 
                   aes(x = long, y = lat, group = group), fill = NA, size = 0.7) + 
      scale_fill_gradientn(colours = colr, 
                           limits = c(min(leg_scale), max(leg_scale)),            
                           breaks = leg_scale, 
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

# observed maps
  for (i in 1:length(names(ob.stack))) {
    
    raster <- ob.stack[[i]]
    title <- paste0(names(ob.stack)[i], "_observed")
    sr_breaks <- leg_lab[[i]]
    sr_max <- st_scale[[i]]
    legend_title <- "Species\nrichness"
    
    obs_v10(raster, title, sr_breaks, sr_max, legend_title)
  }
  
# predicted maps v10 ---------------------------------------------------------
# adding north and scale bars
  pred_v10 <- function(raster, title, sr_breaks, sr_max, legend_title){
    
  # scale breaks and labels
    log_sr <- log(sr_breaks)
    scale_sr <- log_sr/max(log_sr)
    
    log_mx <- log(cellStats(sr_max, "max", na.rm = T))
    scale_mx <- log_mx/max(log_sr)
    
    leg_scale <- scale_sr / scale_mx
    
  # spatial points dataframe 
    raster_spdf <- as(raster, "SpatialPixelsDataFrame")
    raster_df <- as.data.frame(raster_spdf)
    colnames(raster_df) <- c("value", "x", "y")
  
  # km scale bar  
    km_scale <- raster_df %>% rename(lat = y, long = x)
    km_pos <- as.vector(data.frame(x = 130, 
                                   y = -40))
    
  # species richness colours
    colr <- rev(brewer.pal(11, "Spectral"))
  # save 
    save_txt <- paste0("results/maps/scaled/", title, ".jpeg")  
    
  # plot
    q <- ggplot() +  
      ggtitle(title) +
      geom_polygon(data = oz, aes(x = long, y = lat, group = group),
                   fill = "grey60") +
      geom_tile(data = raster_df, aes(x = x, y = y, fill = value)) +             
      geom_polygon(data = oz, colour = "grey1", 
                   aes(x = long, y = lat, group = group), fill = NA, size = 0.7) + 
      scale_fill_gradientn(colours = colr, 
                           limits = c(min(leg_scale), max(leg_scale)),            
                           breaks = leg_scale, 
                           labels = sr_breaks,
                           space = "Lab",
                           name = legend_title) +
      coord_fixed(ratio = 38/33, xlim = c(112, 155), ylim = c(-45, -7)) +
      ggsn::scalebar(km_scale, dist = 500, dist_unit = "km",  st.size = 4, st.dist = 0.05, st.bottom = T, height = 0.05, transform = T, model = 'WGS84', anchor = km_pos) +
      north(data = km_scale, symbol = 1, anchor = c(y = -37, x = 139),
            x.min = 115, x.max = 150, y.min = 0, y.max = -7, scale = 0.15) +
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
  
# predicted maps 
  for (i in 1:length(names(pr.stack))) {
    
    raster <- pr.stack[[i]]
    title <- paste0(names(pr.stack)[i])
    sr_breaks <- leg_lab[[i]]
    sr_max <- st_scale[[i]]
    legend_title <- "Species\nrichness"
    
    pred_v10(raster, title, sr_breaks, sr_max, legend_title)
    
  }
  
# invasion potentital -------------------------------------------------------  
# maintains scaled legend to 0 and 1
  po_v10 <- function(raster, title, sr_breaks, legend_title){
    
  # spatial points dataframe
    raster_spdf <- as(raster, "SpatialPixelsDataFrame")
    raster_df <- as.data.frame(raster_spdf)
    colnames(raster_df) <- c("value", "x", "y")
    
  # species richness colours
    colr <- rev(brewer.pal(11, "Spectral"))
  
  # save
    save_txt <- paste0("results/maps/scaled/", title, ".jpeg")
    
  # plot
    q <- ggplot() +
      ggtitle(title) +
      geom_polygon(data = oz, aes(x = long, y = lat, group = group),
                   fill = "grey60") +
      geom_tile(data = raster_df, aes(x = x, y = y, fill = value)) +
      geom_polygon(data = oz, colour = "grey1",
                   aes(x = long, y = lat, group = group), fill = NA, size = 0.7) +
      scale_fill_gradientn(colours = colr,
                           limits = c(0, 1),
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
  
  
# invasion potential maps
  for (i in 1:length(names(po.stack))) {
    
    raster <- po.stack[[i]]
    title <- paste0(names(po.stack)[i])
    sr_breaks <- c(0, 0.5, 1)
    legend_title <- "Scaled\nlog-\nspecies\nrichness"
    
    po_v10(raster, title, sr_breaks, legend_title)
    
  }
  
# ---------------------------------------------------------------------------
  

  
  