# mapping stuff
  setwd("C:/Users/s436862/Dropbox/Poaceae")
  
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
  
  library(rmapshaper)
  library(oz)
  
# using oz package ---------------------------------------------------
  plot(n.tot)
  oz(states = T, coast = T, xlim = c(112, 155), ylim = c(-45, -7), add = T) # doesn't seem to work, eh
  oz2 <- 
  plot(ozzy)
  k <- oz()
  plot(k)
  
  
# try another way -------------------------------------------------------
  oz.df <- load("C:/Users/s436862/Dropbox/Poaceae/Data files/Australia/ozdata.rda")
  head(ozdata)
  qplot(long, lat, data = ozdata, geom = "polygon", fill = state, main = "States of Australia") + 
        coord_equal()
  
  qplot(long, lat, data = subset(ozdata, border == "coast"), geom = "path", fill = state, main = "Coastline of Australia") + coord_equal()
  
  qplot(long, lat, data = subset(ozdata, state == "WA"), geom = "polygon", fill = I("blue"), main = "WA only") + coord_equal()
  
  ggplot() + geom_polygon(data = ozdata, aes(x = long, y = lat), colour = "black", fill = NA) +
    theme_void()
  
  
  # use this link too: https://www.r-spatial.org/r/2018/10/25/ggplot2-sf-2.html
    
# first step, plot the shapefile ---------------------------------------------------
  oz <- readOGR("C:/Users/s436862/Dropbox/Poaceae/Data files/Australia/australia_shapefile.shp")
  plot(oz)
  plot(oz@data$REGION==1)
# simply outline
  aus2 <- geojsonio::geojson_sp(aus1)
  aus3 <- ms_simplify(aus1, keep = 0.5)
  
# try and plot states/territories
  map <- ggplot() + geom_polygon(data = aus1, aes(x = long, y = lat, group = group), colour = "black", fill = NA) +
    theme_void()
  map

  proj <- "+proj=utm +zone=48 +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
  
  oz1 <- borders("world", region = "Australia", fill = "grey60", bg = "white")
  oz2 <- borders(database = "world", regions = "Australia", colour = "black")
  
  map(regions = "Australia", 
      fill = TRUE, 
      col = "grey60", 
      bg = "white", 
      xlim = c(112, 155), ylim = c(-45, -7), 
      interior = TRUE, 
      resolution = 0
      #, projection = proj
      ) 
  
  
   plot(oz_map)
  # Colour palette for legend
  colour <- rev(brewer.pal(11, "Spectral"))
  # display.brewer.all() 
  # (for more info)
  
  # Plot
  gplot(n.c4) + 
    theme_map()+
    oz1 +
    geom_raster(aes(fill = value)) +
    scale_fill_gradientn(colours = colour, 
                         limits = c(0, 150),
                         space = "Lab",
                         na.value = "transparent",
                         guide = "colourbar",
                         name = "Species \nrichness") + 
    coord_equal() +
    coord_cartesian(xlim = c(112, 155), ylim = c(-45, -7), expand = F) +
    theme(legend.justification = "centre",
          legend.position = "right",
          aspect.ratio = 0.88) +
    oz2
  print(q)
  
  
  
  
  
  
  
  
  
  
  
  
  
  