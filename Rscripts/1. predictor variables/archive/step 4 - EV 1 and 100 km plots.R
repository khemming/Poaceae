
########################################################################################
# step four: plot 100 km rasters
########################################################################################

# library ---------------------------------------------------------------------
  library(RColorBrewer)
  library(raster)
  library(ggmap)
  library(tidyr)
  library(rgdal)
  library(ggplot2)
  library(dplyr)
  library(rasterVis)
  library(maptools)
  library(ggthemes)
  
  rm(list = ls())
  setwd("C:/Users/s436862/Dropbox/Rarefaction/Results")

# EFs at 100 km scale ---------------------------------------------------- 
# EFs ------------------------------------------------------------------  
  setwd("C:/Users/s436862/Dropbox/Rarefaction/Results/EVs/Rasters/100 km")
  
  current.list <- list.files(pattern = ".grd")
  names <- gsub(pattern = "\\.grd$", "", current.list)
  
  c.stack <- stack(current.list)
  names(c.stack) <- names
  
  list2env(setNames(unstack(c.stack), names(c.stack)), .GlobalEnv)

# Plot function --------------------------------------------------------
#  Requires:
# (1) raster
# (2) legend [title] (e.g. record number or species richness)
# (3) file save name (e.g. Graphs/Native SR 50-km.jpeg)  [needs full extension]

# function   
  ef_plot <- function (raster, legend, save)  
  {
    pewc <- raster("C:/Users/s436862/Dropbox/Rarefaction/Results/EVs/Rasters/100 km/pewc.grd")
    raster <- crop(raster, pewc)
  # AUS border + NA fill
  # oz1 <- borders("world", region = "Australia", fill = "grey50")
    oz2 <- borders("world", region = "Australia", colour = "black")
    
  # Colour palette for legend
    colour <- rev(brewer.pal(11, "Spectral"))
  # Plot
    q <- gplot(raster) + 
      theme_map()+
      geom_raster(aes(fill = value)) +
      scale_fill_gradientn(colours = colour, 
                           space = "Lab",
                           na.value = "white",
                           guide = "colourbar",
                           name = legend
      ) + 
      coord_equal() +
      coord_cartesian(xlim = c(112, 155), ylim = c(-45, -7), expand = F) +
      theme(legend.justification = "centre",
            legend.position = "right",
            aspect.ratio = 0.88) +
      oz2
    print(q)
    
    ggsave(save, plot = last_plot(), device = "jpeg", scale = 1, dpi = 500)
    
  } # finish function
  
# Seems legit  
  
  
# EF plots -----------------------------------------------------  
# Requires: Raster: EF raster 
#           legend: what it's long name is 
#           save: complete save location (C/Users etc.)
  setwd("C:/Users/s436862/Dropbox/Rarefaction/Results")
# ---------------------------------------------------------------  
# hii
  raster <- hii
  legend <- "Human \ninfluence \nindex"
  save <- "EVs/Graphs/100 km/hii.jpeg"
  ef_plot(raster, legend, save)
  
# arid
  raster <- arid
  legend <- "Aridity"
  save <- "EVs/Graphs/100 km/aridity.jpeg"
  ef_plot(raster, legend, save)
  
# rz    
  raster <- rz
  legend <- "water \navailability \nroot zone"
  save <- "EVs/Graphs/100 km/rz.jpeg"
  ef_plot(raster, legend, save)
  
# st  
  raster <- st
  legend <- "Water \navailability \nsoil texture"
  save <- "EVs/Graphs/100 km/st.jpeg"
  ef_plot(raster, legend, save)
  
# elev    
  raster <- elev
  legend <- "Elevation"
  save <- "EVs/Graphs/100 km/elev.jpeg"
  ef_plot(raster, legend, save)
  
# evap  
  raster <- pet
  legend <- "Potential \nevapo-transpiration"
  save <- "EVs/Graphs/100 km/pet.jpeg"
  ef_plot(raster, legend, save)
  
# pawc    
  raster <- pawc
  legend <- "Plant \navailable \nwater \ncapacity"
  save <- "EVs/Graphs/100 km/pawc.jpeg"
  ef_plot(raster, legend, save)
  
# pewc    
  raster <- pewc
  legend <- "Plant \nextractable \nwater \ncapacity"
  save <- "EVs/Graphs/100 km/pewc.jpeg"
  ef_plot(raster, legend, save)  
  
# amt  
  raster <- amt
  legend <- "Annual \nmean \ntemperature"
  save <- "EVs/Graphs/100 km/amt.jpeg"
  ef_plot(raster, legend, save)
  
# mdr   
  raster <- mdr
  legend <- "Mean \ndiurnal \nrange"
  save <- "EVs/Graphs/100 km/mdr.jpeg"
  ef_plot(raster, legend, save)
  
# iso  
  raster <- iso
  legend <- "Isothermality"
  save <- "EVs/Graphs/100 km/iso.jpeg"
  ef_plot(raster, legend, save)
  
# ts
  raster <- ts
  legend <- "Temperature \nseasonality"
  save <- "EVs/Graphs/100 km/ts.jpeg"
  ef_plot(raster, legend, save)
  
# twarmm 
  raster <- twarmm
  legend <- "Temperature \nwarmest month"
  save <- "EVs/Graphs/100 km/twarmm.jpeg"
  ef_plot(raster, legend, save)
  
# tcoldm
  raster <- tcoldm
  legend <- "Temperature \ncoldest month"
  save <- "EVs/Graphs/100 km/tcoldm.jpeg"
  ef_plot(raster, legend, save)
  
# tar
  raster <- tar
  legend <- "Annual \ntemperature \nrange"
  save <- "EVs/Graphs/100 km/tar.jpeg"
  ef_plot(raster, legend, save)
  
# twetq
  raster <- twetq
  legend <- "Temperature \nwettest qtr."
  save <- "EVs/Graphs/100 km/twetq.jpeg"
  ef_plot(raster, legend, save)
  
# tdryq
  raster <- tdryq
  legend <- "Temperature \ndriest qtr."
  save <- "EVs/Graphs/100 km/tdryq.jpeg"
  ef_plot(raster, legend, save)
  
# twarmq
  raster <- twarmq
  legend <- "Temperature \nwarmest qtr."
  save <- "EVs/Graphs/100 km/twarmq.jpeg"
  ef_plot(raster, legend, save)
  
# tcoldq
  raster <- tcoldq
  legend <- "Temperature \ncoldest qtr."
  save <- "EVs/Graphs/100 km/tcoldq.jpeg"
  ef_plot(raster, legend, save)
  
# ap
  raster <- ap
  legend <- "Annual \nprecipitation"
  save <- "EVs/Graphs/100 km/ap.jpeg"
  ef_plot(raster, legend, save)
  
# pwetm
  raster <- pwetm
  legend <- "Precipitation \nwettest \nmonth"
  save <- "EVs/Graphs/100 km/pwetm.jpeg"
  ef_plot(raster, legend, save)
  
# pdrym
  raster <- pdrym
  legend <- "Precipitation \ndriest \nmonth"
  save <- "EVs/Graphs/100 km/pdrym.jpeg"
  ef_plot(raster, legend, save)
  
# ps
  raster <- ps
  legend <- "Precipitation \nseasonality"
  save <- "EVs/Graphs/100 km/ps.jpeg"
  ef_plot(raster, legend, save)
  
# pwetq
  raster <- ap
  legend <- "Precipitation \nwettest qtr."
  save <- "EVs/Graphs/100 km/pwetq.jpeg"
  ef_plot(raster, legend, save)
  
# pdryq
  raster <- pdryq
  legend <- "Precipitation \ndriest qtr."
  save <- "EVs/Graphs/100 km/pdryq.jpeg"
  ef_plot(raster, legend, save)
  
# pwarmq
  raster <- pwarmq
  legend <- "Precipitation \nwarmest qtr."
  save <- "EVs/Graphs/100 km/pwarmq.jpeg"
  ef_plot(raster, legend, save)
  
# pcoldq
  raster <- pcoldq
  legend <- "Precipitation \ncoldest qtr."
  save <- "EVs/Graphs/100 km/pcoldq.jpeg"
  ef_plot(raster, legend, save)
  
# th
  raster <- th
  legend <- "Topographic \nheterogeniety"
  save <- "EVs/Graphs/100 km/th.jpeg"
  ef_plot(raster, legend, save)  
  
# clay
  raster <- clay
  legend <- "Clay (%)"
  save <- "EVs/Graphs/100 km/clay.jpeg"
  ef_plot(raster, legend, save) 
  
# EFs at 1 km scale ---------------------------------------------------- 
# EFs ------------------------------------------------------------------  
  setwd("C:/Users/s436862/Dropbox/Rarefaction/Results/EVs/Rasters/1 km")
  
  current.list <- list.files(pattern = ".grd")
  names <- gsub(pattern = "\\.grd$", "", current.list)
  
  c.stack <- stack(current.list)
  names(c.stack) <- names
  
  list2env(setNames(unstack(c.stack), names(c.stack)), .GlobalEnv)
  
  # Plot function --------------------------------------------------------
  #  Requires:
  # (1) raster
  # (2) legend [title] (e.g. record number or species richness)
  # (3) file save name (e.g. Graphs/Native SR 50-km.jpeg)  [needs full extension]
  
  # function   
  ef_plot <- function (raster, legend, save)  
  {
    # AUS border + NA fill
    # oz1 <- borders("world", region = "Australia", fill = "grey50")
    oz2 <- borders("world", region = "Australia", colour = "black")
    
    # Colour palette for legend
    colour <- rev(brewer.pal(11, "Spectral"))
    # Plot
    q <- gplot(raster) + 
      theme_map()+
      geom_raster(aes(fill = value)) +
      scale_fill_gradientn(colours = colour, 
                           space = "Lab",
                           na.value = "white",
                           guide = "colourbar",
                           name = legend
      ) + 
      coord_equal() +
      coord_cartesian(xlim = c(112, 155), ylim = c(-45, -7), expand = F) +
      theme(legend.justification = "centre",
            legend.position = "right",
            aspect.ratio = 0.88) +
      oz2
    print(q)
    
    ggsave(save, plot = last_plot(), device = "jpeg", scale = 1, dpi = 500)
    
  } # finish function
  
  # Seems legit  
  
  
  # EF plots -----------------------------------------------------  
  # Requires: Raster: EF raster 
  #           legend: what it's long name is 
  #           save: complete save location (C/Users etc.)
  setwd("C:/Users/s436862/Dropbox/Rarefaction/Results")
  # ---------------------------------------------------------------  
  # hii
  raster <- hii
  legend <- "Human \ninfluence \nindex"
  save <- "EVs/Graphs/1 km/hii.jpeg"
  ef_plot(raster, legend, save)
  
  # arid
  raster <- arid
  legend <- "Aridity"
  save <- "EVs/Graphs/1 km/aridity.jpeg"
  ef_plot(raster, legend, save)
  
  # rz    
  raster <- rz
  legend <- "water \navailability \nroot zone"
  save <- "EVs/Graphs/1 km/rz.jpeg"
  ef_plot(raster, legend, save)
  
  # st  
  raster <- st
  legend <- "Water \navailability \nsoil texture"
  save <- "EVs/Graphs/1 km/st.jpeg"
  ef_plot(raster, legend, save)
  
  # elev    
  raster <- elev
  legend <- "Elevation"
  save <- "EVs/Graphs/1 km/elev.jpeg"
  ef_plot(raster, legend, save)
  
  # evap  
  raster <- pet
  legend <- "Potential \nevapo-transpiration"
  save <- "EVs/Graphs/1 km/pet.jpeg"
  ef_plot(raster, legend, save)
  
  # pawc    
  raster <- pawc
  legend <- "Plant \navailable \nwater \ncapacity"
  save <- "EVs/Graphs/1 km/pawc.jpeg"
  ef_plot(raster, legend, save)
  
  # pewc    
  raster <- pewc
  legend <- "Plant \nextractable \nwater \ncapacity"
  save <- "EVs/Graphs/1 km/pewc.jpeg"
  ef_plot(raster, legend, save)  
  
  # amt  
  raster <- amt
  legend <- "Annual \nmean \ntemperature"
  save <- "EVs/Graphs/1 km/amt.jpeg"
  ef_plot(raster, legend, save)
  
  # mdr   
  raster <- mdr
  legend <- "Mean \ndiurnal \nrange"
  save <- "EVs/Graphs/1 km/mdr.jpeg"
  ef_plot(raster, legend, save)
  
  # iso  
  raster <- iso
  legend <- "Isothermality"
  save <- "EVs/Graphs/1 km/iso.jpeg"
  ef_plot(raster, legend, save)
  
  # ts
  raster <- ts
  legend <- "Temperature \nseasonality"
  save <- "EVs/Graphs/1 km/ts.jpeg"
  ef_plot(raster, legend, save)
  
  # twarmm 
  raster <- twarmm
  legend <- "Temperature \nwarmest month"
  save <- "EVs/Graphs/1 km/twarmm.jpeg"
  ef_plot(raster, legend, save)
  
  # tcoldm
  raster <- tcoldm
  legend <- "Temperature \ncoldest month"
  save <- "EVs/Graphs/1 km/tcoldm.jpeg"
  ef_plot(raster, legend, save)
  
  # tar
  raster <- tar
  legend <- "Annual \ntemperature \nrange"
  save <- "EVs/Graphs/1 km/tar.jpeg"
  ef_plot(raster, legend, save)
  
  # twetq
  raster <- twetq
  legend <- "Temperature \nwettest qtr."
  save <- "EVs/Graphs/1 km/twetq.jpeg"
  ef_plot(raster, legend, save)
  
  # tdryq
  raster <- tdryq
  legend <- "Temperature \ndriest qtr."
  save <- "EVs/Graphs/1 km/tdryq.jpeg"
  ef_plot(raster, legend, save)
  
  # twarmq
  raster <- twarmq
  legend <- "Temperature \nwarmest qtr."
  save <- "EVs/Graphs/1 km/twarmq.jpeg"
  ef_plot(raster, legend, save)
  
  # tcoldq
  raster <- tcoldq
  legend <- "Temperature \ncoldest qtr."
  save <- "EVs/Graphs/1 km/tcoldq.jpeg"
  ef_plot(raster, legend, save)
  
  # ap
  raster <- ap
  legend <- "Annual \nprecipitation"
  save <- "EVs/Graphs/1 km/ap.jpeg"
  ef_plot(raster, legend, save)
  
  # pwetm
  raster <- pwetm
  legend <- "Precipitation \nwettest \nmonth"
  save <- "EVs/Graphs/1 km/pwetm.jpeg"
  ef_plot(raster, legend, save)
  
  # pdrym
  raster <- pdrym
  legend <- "Precipitation \ndriest \nmonth"
  save <- "EVs/Graphs/1 km/pdrym.jpeg"
  ef_plot(raster, legend, save)
  
  # ps
  raster <- ps
  legend <- "Precipitation \nseasonality"
  save <- "EVs/Graphs/1 km/ps.jpeg"
  ef_plot(raster, legend, save)
  
  # pwetq
  raster <- ap
  legend <- "Precipitation \nwettest qtr."
  save <- "EVs/Graphs/1 km/pwetq.jpeg"
  ef_plot(raster, legend, save)
  
  # pdryq
  raster <- pdryq
  legend <- "Precipitation \ndriest qtr."
  save <- "EVs/Graphs/1 km/pdryq.jpeg"
  ef_plot(raster, legend, save)
  
  # pwarmq
  raster <- pwarmq
  legend <- "Precipitation \nwarmest qtr."
  save <- "EVs/Graphs/1 km/pwarmq.jpeg"
  ef_plot(raster, legend, save)
  
  # pcoldq
  raster <- pcoldq
  legend <- "Precipitation \ncoldest qtr."
  save <- "EVs/Graphs/1 km/pcoldq.jpeg"
  ef_plot(raster, legend, save)
  
  # th
  raster <- th
  legend <- "Topographic \nheterogeniety"
  save <- "EVs/Graphs/1 km/th.jpeg"
  ef_plot(raster, legend, save) 
  
  # clay
  raster <- clay
  legend <- "Clay (%)"
  save <- "EVs/Graphs/1 km/clay.jpeg"
  ef_plot(raster, legend, save) 
  
  
  
  
  
  
  
  
    