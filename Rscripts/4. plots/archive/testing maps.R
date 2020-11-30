
##########################################################
# scaled species richness maps
##########################################################
  
# scope --------------------------------------------------
# observed, predicted and potential species richness maps
  
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
  library(oz)
  library(rgeos)

  
  rm(list = ls())
  
# data ---------------------------------------------------
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
  
# potential  
  setwd("C:/Users/s436862/Dropbox/Poaceae/Results/rasters/potential")
  current.list <- list.files(pattern = ".grd")
  names_long <- gsub(pattern = "\\.grd$", "", current.list)
  po.stack <- stack(current.list)
  names(po.stack) <- names_long
  list2env(setNames(unstack(po.stack), names(po.stack)), .GlobalEnv)
  
  setwd("C:/Users/s436862/Dropbox/Poaceae")
  
# scaled maps (v5) ---------------------------------------------------------
  
  
  
  
# plot raster with levelplot --------------------------------------------- 
  library(sf)
  library(rgdal)
  library(rasterVis)
  library(RColorBrewer)
  
# shapefile
  aus <- readOGR("C:/Users/s436862/Downloads/STE11aAust.shp")
  plot(aus)
# simplify
  oz <- ms_simplify(aus, keep = .01, # proportion of points
                         weighting = 0.7) # smoothing index
  plot(oz)
  
# colour palette  
  #colr <- rev(brewer.pal(11, "Spectral")) # mine doesn't work?
  #colr <- colorRampPalette(brewer.pal(11, 'RdYlBu'))
# so I will change theirs
#   colr <- colorRampPalette(rev(brewer.pal(11, "Spectral")))
#   
# # plot so far ....  
#   levelplot(raster,
#             margin=FALSE,                       # suppress marginal graphics
#             colorkey=list(
#               space='bottom',                   # plot legend at bottom
#               labels=list(at=seq(0, 1, 0.2), font=4)      # legend ticks and labels 
#             ),    
#             par.settings=list(
#               axis.line=list(col='transparent') # suppress axes and legend outline
#             ),
#             scales=list(draw=FALSE),            # suppress axis labels
#             col.regions=colr,                   # colour ramp
#             at=seq(0, 1, len=10)) +          
#     layer(sp.polygons(oz, lwd=1.5)) # add shapefile with sf
  
# ggplot method -------------------------------------------------------------
  library(ggplot2)
  library(raster)
  library(rasterVis)
  library(rgdal)
  library(grid)
  library(scales)
  library(viridis)  # better colors for everyone
  library(ggthemes) # theme_map()
  library(rgdal)
  library(scales)
  library(ggThemeAssist)
  

  oz <- readOGR("Data files/Australia/Australia shapefile.shp")
# raster  
  raster <- raster("Results/rasters/scaled/native_C4.grd")
  raster_spdf <- as(raster, "SpatialPixelsDataFrame")
  raster_df <- as.data.frame(raster_spdf)
  colnames(raster_df) <- c("value", "x", "y")
 
# colours
  colr <- rev(brewer.pal(11, "Spectral"))
  
# plot
  ggplot() +  
    geom_polygon(data = oz, aes(x = long, y = lat, group = group), 
                 fill = "grey60") + # grey interior
    geom_tile(data=raster_df, aes(x = x, y = y, fill = value)) + # raster cells
    geom_polygon(data = oz, colour = "grey1", 
                 aes(x = long, y = lat, group = group), fill = NA, size=0.5) + # border
    scale_fill_gradientn(colours = colr, 
                         limits = c(0,1),                             
                         breaks = c(0, 0.25, 0.5, 0.75, 1),
                         labels = c("0", "0.25", "0.5", "0.75", "1"),
                         space = "Lab",
                         name = "Relative\nspecies\nrichness",
         guide_colourbar(label.theme = NULL, # should remove legend ticks -- not sure why it deson't
                         ticks = F,
                         ticks.colour = "white",
                         ticks.linewidth = 0.5)
    ) +
    coord_fixed(xlim = c(112, 155), ylim = c(-45, -7), expand = F) +
    theme_map() +
    theme(legend.justification = "right",
          legend.position = "right",
          legend.key.size = unit(1, "cm"),
          legend.key.width = unit(0.8,"cm"),
          legend.title = element_text(size = 14),
          legend.text = element_text(size = 14),
          legend.box.spacing = unit(0.1, "cm"),
          plot.title = element_text(size = 20, face = "bold", hjust = 0.1, vjust = -0.5))
  
 # ---------------------------------------------------------- 
  
  
  
  
  
  
  ?write.csv
  
  
  
  
  
  
  
  
  
  
  
  
  
  oz_states <- ozmaps::ozmap_states
  crs(oz_states) <- "+proj=utm +zone=48 +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
  oz_states <- ozmap_data("states")
  ggplot(oz_states) +
    geom_sf(fill = NA, color = "gray1", size = 0.5) +
    # these 2 lines just clean up appearance
    theme_void() +
    coord_sf()
  
  colour <- rev(brewer.pal(11, "Spectral"))
  levelplot(raster) +           # colour ramp breaks
    layer(sp.polygons(aus_states, lwd=1))
  
  
  
  
  
  
  
  
  ggplot(oz_states) + 
    geom_sf() + 
    coord_sf() +
    gplot(raster)
  
  
  plot(sf_oz)
  geom_ploygon(aus)
  raster <- nonnative_C3
  plot(raster)
  ozmap(add = T)
  
  # AUS border + NA fill
    oz1 <- borders("world", region = "Australia", fill = "grey60")
    oz2 <- borders(database = "world", regions = "Australia", colour = "black")
    
  # Colour palette for legend
    colour <- rev(brewer.pal(11, "Spectral"))

  # Plot
    q <- ggplot(oz_states) +
      gplot(raster) + 
      theme_map() +
      ggtitle(title) +
      geom_raster(aes(fill = value)) +
      scale_fill_gradientn(colours = colour, 
                           limits = c(0, 1),                               
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
            legend.title = element_text(size = 14),
            legend.text = element_text(size = 12),
            aspect.ratio = 0.88,
            plot.title = element_text(size = 20, face = "bold", hjust = 0.1, vjust = -0.5)
      ) +
      oz2

   p
   
   ggsave(save, plot = last_plot(), scale = 1, dpi = 500, device = "jpeg")
   return(q)
  } 
  
# richness plots --------------------------------------------------------
# observed maps
  for (i in 1:length(names(ob.stack))) {
    
    title <- paste0(names(ob.stack)[i], "_observed")
    save <- paste0("Results/maps/scaled/", names(ob.stack)[i], ".jpeg")
    raster <- ob.stack[[i]]
    
    ras_v5(title, save, raster)
    
  }
  
# predicted maps
  for (i in 1:length(names(pr.stack))) {
    
    title <- paste0(names(pr.stack)[i], "_predicted")
    save <- paste0("Results/maps/predicted/", names(pr.stack)[i], ".jpeg")
    raster <- pr.stack[[i]]
    
    ras_v5(title, save, raster)
    
  }
  
# potential maps
  for (i in 1:length(names(po.stack))) {
    
    title <- paste0(names(po.stack)[i], "_potential")
    save <- paste0("Results/maps/potential/", names(po.stack)[i], ".jpeg")
    raster <- po.stack[[i]]
    
    ras_v5(title, save, raster)
    
  }
  
# ---------------------------------------------------------------------------
  