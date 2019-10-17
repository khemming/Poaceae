
# Date created: 18/4
# Last modified: 17/7

# Rarefaction Paper rasters and associated plots

  setwd("C:/Users/s436862/Dropbox/Rarefaction")
  
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
  library(gplot2)
    library(ggThemeAssist)
  
  rm(list = ls())
  
# 1. Raster plot function ---------------------------------------------------------------------  
# Details -------------------------------------------------------------------------------------
# Copy and past 'Raster plot function' section at the beginning of each new chapter. Globally alter it as needed for that chapter. 
# When you want to run multiple plots of similar design within chapter, simply specify the new 'required' inputs as appropriate   
# Plot look is based on Engemann's paper 
  
# 1.1 Raster plot function --------------------------------------------------------------------------------
#  Requires:
# (1) raster
# (2) legend [title] (e.g. record number or species richness)
# (3) file save name (e.g. Graphs/Native SR 50-km.jpeg)  [needs full extension]

# raster    
  aus <- raster("C:/Users/s436862/Dropbox/Climate Matching/1. Data files/Australia/aus_100km")


# function   
  eng_ras <- function (raster, legend, save)  
    
  {
    # AUS border + NA fill
    oz1 <- borders("world", region = "Australia", fill = "grey50", bg = "white")
    oz2 <- borders(database = "world", regions = "Australia", colour = "black")
    
    # Colour palette for legend
    colour <- rev(brewer.pal(11, "Spectral"))
    # display.brewer.all() 
    # (for more info)
    
    # Plot
    q <- gplot(raster) + 
      theme_map()+
      oz1 +
      geom_raster(aes(fill = value)) +
      scale_fill_gradientn(colours = colour, 
                           limits = c(0, 300),
                           space = "Lab",
                           na.value = "transparent",
                           guide = "colourbar",
                           name = legend) + 
      coord_equal() +
      coord_cartesian(xlim = c(112, 155), ylim = c(-45, -7), expand = F) +
      theme(legend.justification = "centre",
            legend.position = "right",
            aspect.ratio = 0.88) +
      oz2
    print(q)
    
    ggsave(save, plot = last_plot(), scale = 1, dpi = 500, device = "jpeg")
    
  } # finish function
  
  
# --------------------------------------------------------------------------------------------
  
# 2. Species richness for Nat and Int at 100-km ----------------------------------------------
# Required -----------------------------------------------------------------------------------
# (1) raster
# (2) legend [title] (e.g. record number or species richness)
# (3) file save name (e.g. Graphs/Native SR 50-km.jpeg)  
# Note: save needs to have complete extension and file type
# within this plot loads oz (Australia border) and colour (palette 'Spectral')  
# Note: might want just nat, depending on context of paper 
  
# 2.1 Native plot ------------------------------------------------
  nat <- raster("4. Results/Rarefaction/Rasters/Nat_a")
# the NAs in this raster are zeroes, so I am returning them to whence they came  
  fun <- function(x) { x[x==0] <- NA; return(x)  }
  nat2 <- calc(nat, fun)

# specificy function inputs    
  legend <- "Species richness"
  saveN <- "4. Results/Rarefaction/Graphs/Nat_a.jpeg"
  
  eng_ras(nat2, legend, saveN)
  
# 2.2 Introduced plot ----------------------------------------------  
  int <- raster("4. Results/Rarefaction/Rasters/Int_a")
  # the NAs in this raster are zeroes, so I am returning them to whence they came  
  fun <- function(x) { x[x==0] <- NA; return(x)  }
  int2 <- calc(int, fun)
  
  # specificy function inputs    
  legend <- "Species richness"
  saveI <- "4. Results/Rarefaction/Graphs/Int_a.jpeg"
  
  eng_ras(int2, legend, saveI)
  
  
# --------------------------------------------------------------------------------------------
  
# 2. Record-number per-cell rasters at 50-, 100- and 200-km and HII ------------------
# Data -------------------------------------------------------------------------------
  setwd("C:/Users/s436862/Dropbox/Rarefaction/1. Data files")

# Species 
  x <- read.csv("AVH/AVH grass records.csv", header = T) %>%
    dplyr::filter(status == "native")
# raster
  v <- raster("EFs/EFS cropped/arid.grd")
# put records into cells    
  xy <- cbind(x$long, x$lat) 
  spp <- as.numeric(factor(x$species))
# scale/width (v)
  v50 <- aggregate(v, fact = 50, fun = mean)
  v100 <- aggregate(v, fact = 100, fun = mean)
  v200 <- aggregate(v, fact = 200, fun = mean)

# create raster for each km class
  n50 <- rasterize(xy, v50, fun = function(x,...) {length(na.omit(x)) })
  n100 <- rasterize(xy, v100, fun = function(x,...) {length(na.omit(x)) })
  n200 <- rasterize(xy, v200, fun = function(x,...) {length(na.omit(x)) })
  
# shift to dataframe  
  n50_n <- getValues(n50)
  n100_n <- getValues(n100)
  n200_n <- getValues(n200)
  
# repeat for arid raster (basis for removing offshore cells)
  v50_n <- getValues(v50)
  v100_n <- getValues(v100)
  v200_n <- getValues(v200)
  
# merge km-pairs; convert Native n values to arid NAs
  df50 <- data.frame(n50_n, v50_n)
  df50[is.na(df50$v50_n), 1] <- NA  
  
  df100 <- data.frame(n100_n, v100_n)
  df100[is.na(df100$v100_n), 1] <- NA  
  
  df200 <- data.frame(n200_n, v200_n)
  df200[is.na(df200$v200_n), 1] <- NA  
  
# generate rasters  
  values(n50) <- df50$n50_n
  plot(n50)
  values(n100) <- df100$n100_n
  plot(n100)
  values(n200) <- df200$n200_n
  plot(n200)
  
# save rasters  
  setwd("C:/Users/s436862/Dropbox/Rarefaction/4. Results")
  
  writeRaster(n50, "Grass groups AVH/Rarefaction/Rasters/Native n 50km", overwrite = T)
  
  writeRaster(n100, "Grass groups AVH/Rarefaction/Rasters/Native n 100km", overwrite = T)
  
  writeRaster(n200, "Grass groups AVH/Rarefaction/Rasters/Native n 200km", overwrite = T)  
  
# Raster plot function ---------------------------------------------------------------
# same as previous

  setwd("C:/Users/s436862/Dropbox/Rarefaction/4. Results/Rarefaction")

  eng_ras <- function (raster, legend, save)  
  
{
# AUS border
  oz1 <- borders("world", region = "Australia", fill = "grey50")
  oz2 <- borders("world", region = "Australia", colour = "black")
 
# Colour palette for legend
  colour <- rev(brewer.pal(11, "Spectral"))
  # display.brewer.all() 
  # (for more info)

# Plot
  q <- gplot(raster) + 
    theme_map()+
    oz1 +
  geom_raster(aes(fill = value)) +
    scale_fill_gradientn(colours = colour, 
                         space = "Lab",
                         na.value = "transparent",
                         guide = "colourbar",
                         name = legend) + 
    coord_equal() +
    coord_cartesian(xlim = c(112, 155), ylim = c(-45, -7), expand = F) +
    theme(legend.justification = "centre",
          legend.position = "right",
          aspect.ratio = 0.88) +
    oz2
  print(q)
  
  ggsave(save, plot = last_plot(), scale = 1, dpi = 500, device = "jpeg")
  
} # finish function

# 2.1 50-km plot ------------------------------------------------
  raster <- raster("Rasters/Native n 50km")
  legend <- "Record number"
  save <- "Graphs/Nat n 50 km.jpeg"
  
  eng_ras(raster, legend, save)
  
# 2.2 100-km plot ----------------------------------------------  
  raster <- raster("Rasters/Native n 100km")
  legend <- "Record number"
  save <- "Graphs/Nat n 100 km.jpeg"
  
  eng_ras(raster, legend, save)
  
# 2.3 200-km plot ----------------------------------------------  
  raster <- raster("Rasters/Native n 200km")
  legend <- "Record number"
  save <- "Graphs/Nat n 200 km.jpeg"
  
  eng_ras(raster, legend, save)
  
# 2.4 HII ----------------------------------------------
  setwd("C:/Users/s436862/Dropbox/Rarefaction")
  
  hii <- raster("1. Data files/EFs/EFs cropped/hii")
  plot(hii)
  
  oz <- borders("world", region = "Australia", xlim = c(112, 155), ylim = c(-45, -7))
  
  theme_set(theme_map())
  gplot(hii) + geom_raster(aes(fill = value)) +
    facet_wrap(~ variable) +
    scale_fill_gradient(low = 'white', 
                        high = 'black',
                        na.value = "transparent",
                        name = "") +
    coord_equal() +
    theme(legend.justification = "centre",
          legend.position = "right") +
    oz
  
  ggsave(save, plot = last_plot(), scale = 1, dpi = 500, device = "jpeg")
  
  ggsave("4. Results/Rarefaction/Graphs/HII.jpeg", plot = last_plot(), scale = 1, dpi = 500, device = "jpeg")  
# -------------------------------------------------------------------------------------
  
# 3. SRE plots at 100-km --------------------------------------------------------------
# Aim ---------------------------------------------------------------------------------  
# So here I want to show how each look at 100-km (our ideal scale) compare with each other
# Raster plot functions #3a-b -------------------------------------------------------------
# Requires:
# (1) raster
# (2) legend [title] (e.g. record number or species richness)
# (3) file save name (e.g. Graphs/Native SR 50-km.jpeg)  [needs full extension]
# Data
# species richness estimators
  sre <- read.csv("C:/Users/s436862/Dropbox/Rarefaction/4. Results/Rarefaction/CSV/sre.csv", header = T)
  
# raster    
  aus <- raster("C:/Users/s436862/Dropbox/Rarefaction/1. Data files/Australia raster/aus")
  aus <- aggregate(aus, fac = 100, fun = mean)
  
# function for extrap. SREs ---------------------------------------------------
  eng_ras_3a <- function (raster, legend, save)  
  {
    # AUS border + NA fill
    oz1 <- borders("world", region = "Australia", fill = "grey50")
    oz2 <- borders("world", region = "Australia", colour = "black")
    
    # Colour palette for legend
    colour <- rev(brewer.pal(11, "Spectral"))
    # display.brewer.all() 
    # (for more info)
    
    # Plot
    q <- gplot(raster) + 
      theme_map()+
      oz1 +
      geom_raster(aes(fill = value)) +
      scale_fill_gradientn(colours = colour, 
                           limits = c(0, 330), # max SRE values is 330 (Jack 2nd)
                           space = "Lab",
                           na.value = "transparent",
                           guide = "colourbar",
                           name = legend) + 
      coord_equal() +
      coord_cartesian(xlim = c(112, 155), ylim = c(-45, -7), expand = F) +
      theme(legend.justification = "centre",
            legend.position = "right",
            aspect.ratio = 0.88) +
      oz2
    print(q)
    
    ggsave(save, plot = last_plot(), scale = 1, dpi = 500, device = "jpeg")
    
  } # finish function    
  
# function for rarefaction SRE--------------------------------------------  
  eng_ras_3b <- function (raster, legend, save)  
  {
    # AUS border + NA fill
    oz1 <- borders("world", region = "Australia", fill = "grey50")
    oz2 <- borders("world", region = "Australia", colour = "black")
    
    # Colour palette for legend
    colour <- rev(brewer.pal(11, "Spectral"))
    # display.brewer.all() 
    # (for more info)
    
    # Plot
    q <- gplot(raster) + 
      theme_map()+
      oz1 +
      geom_raster(aes(fill = value)) +
      scale_fill_gradientn(colours = colour, 
                           limits = c(0, 50), # max SRE value is 50
                           space = "Lab",
                           na.value = "transparent",
                           guide = "colourbar",
                           name = legend) + 
      coord_equal() +
      coord_cartesian(xlim = c(112, 155), ylim = c(-45, -7), expand = F) +
      theme(legend.justification = "centre",
            legend.position = "right",
            aspect.ratio = 0.88) +
      oz2
    print(q)
    
    ggsave(save, plot = last_plot(), scale = 1, dpi = 500, device = "jpeg")
    
  } # finish function  
  
# 3.1 Raw -------------------------------------------------------------------
  setwd("C:/Users/s436862/Dropbox/Rarefaction/4. Results/Rarefaction/Graphs")

  oz <- borders("world", region = "Australia", colour = "black")
  
# set function elements   
  raw <- setValues(aus, sre$a)
  #raw_d <- alignExtent(raw, oz2, snap = "in")
  plot(raw)
  
  legend <- "Raw"
  save <- "SR-100km raw.jpeg"
  raw_test <- raw
  
  
# run function    
  eng_ras_3a(x, legend, save)
  
  
  legend <- "Raw"
  save <- "SR-100km raw.jpeg"
  raw_test <- raw
# run function    
  eng_ras_3a(raw, legend, save)
  
# 3.2 Chao1 -------------------------------------------------------------------
# set function elements   
  chao1 <- setValues(aus, sre$chao1)
  plot(chao1)
  legend <- "Chao1"
  save <- "SR-100km Chao1.jpeg"

# run function  
  eng_ras_3a(chao1, legend, save)
  
# 3.3 ACE -------------------------------------------------------------------
# set function elements   
  ace <- setValues(aus, sre$ace)
  plot(ace)
  legend <- "ACE"
  save <- "SR-100km ACE.jpeg"
  
# run function  
  eng_ras_3a(ace, legend, save)  
  
# 3.3 jack -------------------------------------------------------------------
# set function elements   
  jack <- setValues(aus, sre$jack)
  plot(jack)
  legend <- "jack"
  save <- "SR-100km jack.jpeg"
  
# run function  
  eng_ras_3a(jack, legend, save)    
  
  
# 3.3 Rarefaction -------------------------------------------------------------------
# set function elements   
  rarefaction <- setValues(aus, sre$rarefied_rich)
  plot(rarefaction)
  legend <- "rarefied"
  save <- "SR-100km rarefied.jpeg"
  
# run function  
  eng_ras_3b(rarefaction, legend, save) # interesting ....
  
  
  
  
  
  
  
# 3.4 trying to plot smooth edges ----------------------------------------------
# test 1 ----------------------------------------------------------------------  
# define the outside square of the shapefile
  coords <- matrix(c(quadrat@xmin-20, quadrat@ymin-20, 
                     quadrat@xmin-20, quadrat@ymax+20,
                     quadrat@xmax+20, quadrat@ymax+20,
                     quadrat@xmax+20, quadrat@ymin-20,
                     quadrat@xmin-20, quadrat@ymin-20),
                   ncol = 2, byrow = T)
  
# turn it into a shape (polygon)
  p1 <- Polygon(coords, hole = T)
  plot(p1)
# give it x and y lat and longs
  ps1<- SpatialPolygons(list(Polygons(list(p1), ID = "a")), proj4string = CRS(proj4string(raw)))
  
  aus_hole <- gDifference(ps1, aus)
  plot(aus_hole, col='red', pbg='white')
  
  plot(raw, axes=F, box =F, legend=F)
  plot(aus, add = T)
  plot(aus_hole, add = T, col="white")
  
  #aus_shp <- shapefile("C:/Users/s436862/Dropbox/Climate Matching/1. Data files/Shapefiles Australia/australia_shapefile.shp")
  
  proj4string(aus_hole) <- "+proj=utm +zone=48 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"
  
  #aus_hole_spdf <- SpatialPolygons(aus_hole, pO = )
  
  q <- gplot(raw) + 
    theme_map()+
    geom_raster(aes(fill = value)) +
    scale_fill_gradientn(colours = colour, 
                         limits = c(0, 330), # max SRE values is 330 (Jack 2nd)
                         space = "Lab",
                         na.value = "transparent",
                         guide = "colourbar",
                         name = legend) + 
    coord_equal() +
    coord_cartesian(xlim = c(112, 155), ylim = c(-45, -7), expand = F) +
    theme(legend.justification = "centre",
          legend.position = "right",
          aspect.ratio = 0.88) + 
    oz + 
    
    geom_path(mapping = NULL, data = aus_hole)
  
  print(q)
  
  #ggsave(save, plot = last_plot(), scale = 1, dpi = 500, device = "jpeg")
  
  
  
  
  
  
  
# ------------------------------------------------------------------------------
# test 2 (actually works) -----------------------------------------------------------------------
  setwd("C:/Users/s436862/Dropbox/Rarefaction/4. Results/Rarefaction/Graphs")
  
  oz <- borders("world", region = "Australia", colour = "black")
  aus_shp <- readOGR("C:/Users/s436862/Dropbox/Climate Matching/1. Data files/Shapefiles Australia/australia_shapefile.shp")
  
  
# set function elements   
  raw <- setValues(aus, sre$a)
  plot(raw)
  raw_crop <- mask(raw, aus_shp)
  plot(raw_crop)
  plot(aus_shp, add = T)
  
  legend <- "Raw"
  save <- "SR-100km raw.jpeg"
  raw_test <- raw
  
  
# run function    
  eng_ras_3a(x, legend, save)
  
  
  legend <- "Raw"
  save <- "SR-100km raw.jpeg"
  raw_test <- raw
# run function    
  eng_ras_3a(raw, legend, save)
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
# ------------------------------------------------------------------------------
  
# 4. Native and introdcued plots using Chao1 1 and Rarefaction -----------------
# Aim ---------------------------------------------------------------------------------  
# So here I want to show how each look at 100-km (our ideal scale) compare with each other  
# Raster plot functions #4a-b -------------------------------------------------------------
# Requires:
# (1) raster
# (2) legend [title] (e.g. record number or species richness)
# (3) file save name (e.g. Graphs/Native SR 50-km.jpeg)  [needs full extension]

# function for extrap. SREs ---------------------------------------------------
  eng_ras_3a <- function (raster, legend, save)  
  {
  # AUS border + NA fill
    oz1 <- borders("world", region = "Australia", fill = "grey50")
    oz2 <- borders("world", region = "Australia", colour = "black")
    
  # Colour palette for legend
    colour <- rev(brewer.pal(11, "Spectral"))
  # display.brewer.all() 
  # (for more info)
    
  # Plot
    q <- gplot(raster) + 
      theme_map()+
      oz1 +
      geom_raster(aes(fill = value)) +
      scale_fill_gradientn(colours = colour, 
                           limits = c(0, 310), # max SRE values is 302 (Chao1)
                           space = "Lab",
                           na.value = "transparent",
                           guide = "colourbar",
                           name = legend) + 
      coord_equal() +
      coord_cartesian(xlim = c(112, 155), ylim = c(-45, -7), expand = F) +
      theme(legend.justification = "centre",
            legend.position = "right",
            aspect.ratio = 0.88) +
      oz2
    print(q)
    
    ggsave(save, plot = last_plot(), scale = 1, dpi = 500, device = "jpeg")
    
  } # finish function    
  
# function for rarefaction SRE--------------------------------------------  
  eng_ras_3b <- function (raster, legend, save)  
  {
  # AUS border + NA fill
    oz1 <- borders("world", region = "Australia", fill = "grey50")
    oz2 <- borders("world", region = "Australia", colour = "black")
    
  # Colour palette for legend
    colour <- rev(brewer.pal(11, "Spectral"))
    # display.brewer.all() 
    # (for more info)
    
  # Plot
    q <- gplot(raster) + 
      theme_map()+
      oz1 +
      geom_raster(aes(fill = value)) +
      scale_fill_gradientn(colours = colour, 
                           limits = c(0, 50), # max SRE value is 50
                           space = "Lab",
                           na.value = "transparent",
                           guide = "colourbar",
                           name = legend) + 
      coord_equal() +
      coord_cartesian(xlim = c(112, 155), ylim = c(-45, -7), expand = F) +
      theme(legend.justification = "centre",
            legend.position = "right",
            aspect.ratio = 0.88) +
      oz2
    print(q)
    
    ggsave(save, plot = last_plot(), scale = 1, dpi = 500, device = "jpeg")
    
  } # finish function  
  
# 4.1 Data manipulation -------------------------------------------------------------------
  setwd("C:/Users/s436862/Dropbox/Rarefaction/4. Results/Rarefaction")

# 
# create sre dataframe
  sre <- cbind(cell, n, a, chao1, ace, jack_second, rarefaction, arid)  
  
  # where arid = NA, we want the sre's to also be NA  
  # (conviniently removes ocean and off-shore island cells)  
  sre[is.na(sre$arid), 2:7] <- NA
  
  
  nat_raw <- raster("Rasters/Nat_a")
  plot(nat_raw)
  
  raw_df <- getValues(nat_raw)
  raw_df <- data.frame(raw_df)
  nat_chao <- raster("Rasters/Nat_chao1")
  plot(nat_chao)
  
  nat_rare <- raster("Rasters/Nat_rare_50")
  plot(nat_rare)
  
  
  
  
  
  oz <- borders("world", region = "Australia", colour = "black")
  
# Nat raw -------------------------------------------------------------------   
# set function elements
  legend <- "Nat raw"
  save <- "Graphs/SR-100km raw.jpeg"
  raw_test <- raw
# run function    
  eng_ras_3a(nat_raw, legend, save)
  
  
  legend <- "Raw"
  save <- "SR-100km raw.jpeg"
  raw_test <- raw
# run function    
  eng_ras_3a(raw, legend, save)
  
# 4.2 Chao1 -------------------------------------------------------------------
# set function elements   
  chao1 <- setValues(aus, sre$chao1)
  plot(chao1)
  legend <- "Chao1"
  save <- "SR-100km Chao1.jpeg"
  
# run function  
  eng_ras_3a(chao1, legend, save)
  
# 4.3 Rarefaction -------------------------------------------------------------------
# set function elements   
  rarefaction <- setValues(aus, sre$rarefied_rich)
  plot(rarefaction)
  legend <- "rarefied"
  save <- "SR-100km rarefied.jpeg"
  
# run function  
  eng_ras_3b(rarefaction, legend, save) # interesting ....
  
  
  
  
  
  
  
  
  
  
  
  
# ------------------------------------------------------------------------------  
  
# GGplot 2 theme assist ----------------------------------------------------------------- 
  library(gplot2)
  
  library(ggThemeAssist)
  
  plot1 <- ggplot(data=dat, aes(x=long, y=lat)) + geom_point(aes(col=dat$bamboo, size=dat$prop.aban)) # or whatever your plot is
  
  ggThemeAssistGadget(plot1) # makes an easy-to-use interface and then returns the code that configures your plot how you like it!
  
  # ---------------------------------------------------------------------------