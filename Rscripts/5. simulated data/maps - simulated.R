


# library --------------------------------------------------------------
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
  library(tidyverse)

  rm(list = ls())

# data -----------------------------------------------------------------
# shape file
  oz <- readOGR("Data files/Australia/Australia shapefile.shp")
  plot(oz)
  
# rasters 
  setwd("Results/simulated results")
  current.list <- list.files(pattern = ".grd")
  names_long <- gsub(pattern = "\\.grd$", "", current.list)
  ob.stack <- stack(current.list)
  names(ob.stack) <- names_long
  list2env(setNames(unstack(ob.stack), names(ob.stack)), .GlobalEnv)
  
  setwd("C:/Users/s436862/Dropbox/Poaceae")
  
# scaled maps (v9) ---------------------------------------------------------
  ras_v9 <- function(raster, title){
    
  # raster to spatial points dataframe 
    raster_spdf <- as(raster, "SpatialPixelsDataFrame")
    raster_df <- as.data.frame(raster_spdf)
    colnames(raster_df) <- c("value", "x", "y")
  # pretty colours
    colr <- rev(brewer.pal(11, "Spectral"))
  # save 
    save_txt <- paste0("Results/simulated results/", title, ".jpeg")
  # plot
    q <- ggplot() +  
      ggtitle(title) +
      geom_polygon(data = oz, aes(x = long, y = lat, group = group), 
                   fill = "grey60") +                                               
      geom_tile(data = raster_df, aes(x = x, y = y, fill = value)) +             
      geom_polygon(data = oz, colour = "grey1", 
                   aes(x = long, y = lat, group = group), fill = NA, size = 0.5) + 
      scale_fill_gradientn(colours = colr, 
                           limits = c(0, 1),                             
                           breaks = c(0, 0.5, 1),
                           space = "Lab",
                           name = "Scaled\nrichness") +
      coord_fixed(ratio = 38/33, xlim = c(112, 155), ylim = c(-45, -7)) +
      theme_map() +
      theme(legend.direction = "vertical",
            legend.justification = "right",
            legend.position = "right",
            legend.key.size = unit(12, "cm"),
            legend.key.width = unit(1,"cm"),
            legend.title = element_text(size = 16),
            legend.text = element_text(size = 14),
            legend.box.spacing = unit(0.1, "cm"),
            plot.title = element_text(size = 20, face = "bold", hjust = 0.1, vjust = -0.5)) +
      guides(fill = guide_colorbar(barheight = unit(5, "cm"))) 
    plot(q)
    
    ggsave(save_txt, plot = last_plot(), width = 12, height = 12, units = "cm", dpi = 500, device = "jpeg")
  } # finish function
  
# maps ------------------------------------------------------------  
# native complete
  ras_v9(native.complete,
         "Native complete")
# nonnative complete
  ras_v9(nonnative.complete,
         "nonnative complete")
# nonnative incomplete
  ras_v9(nonnative.incomplete,
         "nonnative incomplete")
# invasion potential marginal
  ras_v9(marginal.invasion.potential,
         "Marginal invasion potential")
# invasion potential substantial
  ras_v9(substantial.invasion.potential,
         "Substantial invasion potential")
# no correlation nonnative
  ras_v9(nonnative.no.correlation,
         "nonnative no correlation")
  
  
# start here
# blank invasion potential -----------------------------------------------------
# raster to spatial points dataframe 
  raster <- calc(nonnative.incomplete, fun = function(x) {x[x>1500] <- NA; return(x)})
  raster_spdf <- as(raster, "SpatialPixelsDataFrame")
  raster_df <- as.data.frame(raster_spdf)
  colnames(raster_df) <- c("value", "x", "y")
  
# colours
  colr <- rev(brewer.pal(11, "Spectral"))

# scale bar
  limit_set <- c(cellStats(native.complete, stat = 'min', na.rm = T), 
                 cellStats(native.complete, stat = 'max', na.rm = T))
  break_set <- c(cellStats(native.complete, stat = 'min', na.rm = T) + 0.05, 
                 cellStats(native.complete, stat = 'max', na.rm = T)- 0.05)
# save
  save <- "unknown invasion potential"

# plot
  ggplot() +  
    ggtitle(title) +
    geom_polygon(data = oz, aes(x = long, y = lat, group = group), 
                 fill = "grey60") +                                               
    geom_tile(data = raster_df, aes(x = x, y = y, fill = NA)) +             
    geom_polygon(data = oz, colour = "grey1", 
                 aes(x = long, y = lat, group = group), fill = NA, size = 0.5) + 
    scale_fill_gradientn(colours = colr, 
                         limits = c(0, 1),                             
                         breaks = c(0, 0.5, 1),
                         space = "Lab",
                         name = "Scaled\nrichness") +
    coord_fixed(ratio = 38/33, xlim = c(112, 155), ylim = c(-45, -7)) +
    theme_map() +
    theme(legend.direction = "vertical",
          legend.justification = "right",
          legend.position = "right",
          legend.key.size = unit(12, "cm"),
          legend.key.width = unit(1,"cm"),
          legend.title = element_text(size = 16),
          legend.text = element_text(size = 14),
          legend.box.spacing = unit(0.1, "cm"),
          plot.title = element_text(size = 20, face = "bold", hjust = 0.1, vjust = -0.5)) +
    guides(fill = guide_colorbar(barheight = unit(5, "cm"))) 
  save_txt <- paste0("Results/simulated results/", save, ".jpeg")
 
  ggsave(save_txt, plot = last_plot(), scale = 1, dpi = 500, device = "jpeg")
  

# ------------------------------------------------------------------