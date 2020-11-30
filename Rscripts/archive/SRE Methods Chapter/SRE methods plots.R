
# Date created: 18/4
# Last modified: 17/7

# Rarefaction Paper rasters and associated plots


# Library -------------------------------------------------------------------------------------
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
  
# 5. Rarefaction multiple cutoffs ----------------------------------------------
# 5.1 Introduced summary statst: SD vs cell coverage ---------------------
  co <- read.csv("4. Results/Rarefaction/CSV/Int_ONLY multi-cutoffs summary stats.csv", header = T)
  
# CV-Cell coverage correlation plot -------------------------------------------- 
  plot(co$CV, co$cell.percent)
  
  q <- ggplot(co, aes(x = cell.percent, y = CV)) + 
    geom_text(aes(label = rarefied), hjust = -0.5, vjust = 0) +
    geom_line() +
    geom_point() +
    labs(x="Cells occupied (%)", y = "CV") +
    expand_limits(x = c(22, 43)) +
    theme_classic()
q
  
  #ggThemeAssistGadget(q)
  
  ggsave("4. Results/Rarefaction/Graphs/Int_ONLY multi-cutoffs %occupied x CV.jpeg", plot = last_plot(), scale = 1, dpi = 500, device = "jpeg")
# Double Y-axis Plot ---------------------------------------------------------------  
# y axis  
  p <- ggplot(co, aes(x = rarefied))
  
# x axis 1: cells occupied (%)
  p <- p + geom_line(aes(y = cell.percent, colour = "Cells occupied (%)"))
  
# x axis 2: SD [transformed to match roughly the range of the cell data (== /5)]
  p <- p + geom_line(aes(y = SD*7, colour = "SD"))
  
# now adding the secondary axis, following the example in the help file ?scale_y_continuous
# and, very important, reverting the above transformation
  p <- p + scale_y_continuous(sec.axis = sec_axis(~./7, name = "SD"))
  
# modifying colours and theme options
  p <- p + scale_colour_manual(values = c("red", "blue"))
  p <- p + labs(y = "Cells occupied (%)",
                x = "Rarefied richness",
                colour = "Parameter")
  p <- p + theme(legend.position = c(0.7, 0.9))
  p
  
 
  p <- p + theme(plot.subtitle = element_text(vjust = 1), 
      plot.caption = element_text(vjust = 1), 
      panel.grid.major = element_line(linetype = "blank"), 
      panel.grid.minor = element_line(linetype = "blank"), 
      axis.text = element_text(size = 11), 
      panel.background = element_rect(fill = NA))
  #ggThemeAssistGadget(p)

  p <- p + theme(panel.grid.major = element_line(linetype = "solid"), 
      panel.background = element_rect(fill = "gray95"))
  p

  ggsave("4. Results/Rarefaction/Graphs/Int_ONLY multi-cutoffs summary stats.jpeg", plot = last_plot(), scale = 1, dpi = 500, device = "jpeg")

# 5.2 Rarefaction multi-cutoff (50- X 45- ... 15-record) summary statistics -----
# Native records ----------------------------------------------------------------
  tot.50 <- read.csv("4. Results/Rarefaction/CSV/Native multiple cutoff COMPLETE.csv", header = T) %>%
    select(total.rare.50)
# other cutoffs (co)  
  tot.co <- read.csv("4. Results/Rarefaction/CSV/Native multiple cutoff COMPLETE.csv", header = T) %>%
    select(total.rare.15, total.rare.20, total.rare.25, total.rare.30,
           total.rare.35, total.rare.40, total.rare.45)  
# correlation matrix
  total.cor <- cor(tot.co, tot.50, use = "complete.obs")  
  
# C3 records -------------------------------------------------------------------
  c3.50 <- read.csv("4. Results/Rarefaction/CSV/Native multiple cutoff COMPLETE.csv", header = T) %>%
    select(C3.rare.50)
# other cutoffs (co)  
  c3.co <- read.csv("4. Results/Rarefaction/CSV/Native multiple cutoff COMPLETE.csv", header = T) %>%
    select(C3.rare.15, C3.rare.20, C3.rare.25, C3.rare.30,
           C3.rare.35, C3.rare.40, C3.rare.45)  
# correlation matrix
  c3.cor <- cor(c3.co, c3.50, use = "complete.obs")  
  
# C4 records -------------------------------------------------------------------
  c4.50 <- read.csv("4. Results/Rarefaction/CSV/Native multiple cutoff COMPLETE.csv", header = T) %>%
    select(C4.rare.50)
# other cutoffs (co)  
  c4.co <- read.csv("4. Results/Rarefaction/CSV/Native multiple cutoff COMPLETE.csv", header = T) %>%
    select(C4.rare.15, C4.rare.20, C4.rare.25, C4.rare.30,
           C4.rare.35, C4.rare.40, C4.rare.45)  
# correlation matrix
  c4.cor <- cor(c4.co, c4.50, use = "complete.obs")  
  
# complete native correlation matrix  
  nat.cor <- cbind(total.cor, c3.cor, c4.cor)
  
# Introduced species -----------------------------------------------------------
# total records ----------------------------------------------------------------
  tot.50 <- read.csv("4. Results/Rarefaction/CSV/Introduced multiple cutoff COMPLETE.csv", header = T) %>%
    select(total.rare.50)
# other cutoffs (co)  
  tot.co <- read.csv("4. Results/Rarefaction/CSV/Introduced multiple cutoff COMPLETE.csv", header = T) %>%
    select(total.rare.15, total.rare.20, total.rare.25, total.rare.30,
           total.rare.35, total.rare.40, total.rare.45)  
# correlation matrix
  total.cor <- cor(tot.co, tot.50, use = "complete.obs")  
  
# C3 records -------------------------------------------------------------------
  c3.50 <- read.csv("4. Results/Rarefaction/CSV/Introduced multiple cutoff COMPLETE.csv", header = T) %>%
    select(C3.rare.50)
# other cutoffs (co)  
  c3.co <- read.csv("4. Results/Rarefaction/CSV/Introduced multiple cutoff COMPLETE.csv", header = T) %>%
    select(C3.rare.15, C3.rare.20, C3.rare.25, C3.rare.30,
           C3.rare.35, C3.rare.40, C3.rare.45)  
# correlation matrix
  c3.cor <- cor(c3.co, c3.50, use = "complete.obs")  
  
# C4 records -------------------------------------------------------------------
  c4.50 <- read.csv("4. Results/Rarefaction/CSV/Introduced multiple cutoff COMPLETE.csv", header = T) %>%
    select(C4.rare.50)
# other cutoffs (co)  
  c4.co <- read.csv("4. Results/Rarefaction/CSV/Introduced multiple cutoff COMPLETE.csv", header = T) %>%
    select(C4.rare.15, C4.rare.20, C4.rare.25, C4.rare.30,
           C4.rare.35, C4.rare.40, C4.rare.45)  
# correlation matrix
  c4.cor <- cor(c4.co, c4.50, use = "complete.obs")  
  
# complete native correlation matrix  
  int.cor <- cbind(total.cor, c3.cor, c4.cor)
  
# Save ----------------------------------------------------------------------------------
  complete.cor <- cbind(nat.cor, int.cor)
  colnames(complete.cor) <- c("nat.total", "nat.c3", "nat.c4", "int.total", "int.c3", "int.c4")
  
  write.csv(complete.cor, file = "4. Results/Rarefaction/CSV/Multiple cutoff correlations COMPLETE.csv")

   
# 5.3 Rarefaction multi-cutoff (50- X 15-record) plots -------------------------
# Aim --------------------------------------------------------------------------
# The aim of this is to assess which cutoff is best for overall rarefaction.
# This will be done via correlations between 50 (the ideal cutoff) and the worst resolution data (15)
  
# Not sure how I will split these up yet... we'll see
# Plots:
# Status (Nat/Int)
# Pp (total/C3/C4)
  
  setwd("C:/Users/s436862/Dropbox/Rarefaction")
  
# Spp data
  nat <- read.csv("4. Results/Rarefaction/CSV/Native multiple cutoff COMPLETE.csv", header = T)
  int <- read.csv("4. Results/Rarefaction/CSV/Introduced multiple cutoff COMPLETE.csv", header = T)
 
  setwd("C:/Users/s436862/Dropbox/Rarefaction/4. Results/Rarefaction/Graphs/Multi-cutoff rarefaction correlations")
  
# Scatterplot function -------------------------------------------------------
# Requires:
# df: complete df of all the spp categories
# xcol: X
# ycol: Y
# xlab: X name
# ylab: Y name
# save: exact save locale (note yo wd())
  ggplotRegression <- function(dat, x, y, xlab, ylab, save){
    
    fml <- paste(y, "~", x)
    
    fit <- lm(fml, dat)
    
    ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) + 
      geom_point() +
      stat_smooth(method = "lm", col = "red", se = F) +
      theme(axis.title = element_text(size = 14)) +
      labs(x = ylab,
           y = xlab) +
      labs(title = paste("Adj R2 = ", signif(summary(fit)$adj.r.squared, 3),
                         "Intercept =", signif(fit$coef[[1]],3 ),
                         " Slope =", signif(fit$coef[[2]], 3),
                         " P =", signif(summary(fit)$coef[2,4], 5)))
    
    ggsave(save, plot = last_plot(), dpi = 500, device = "jpeg")
   
      }
 
# Nat -----------------------------------------------------------------------------------  
# total ---------------------------------------------------------------
  df <- nat
  x <- "total.rare.50"
  y <- "total.rare.15"
  xlab <- "15-record"
  ylab <- "50-record"
  save <- "Total native 50-15-record correlation.jpeg" 
  
  ggplotRegression(df, x, y, xlab, ylab, save)
  

# c3 ---------------------------------------------------------------
  df <- nat
  x <- "C3.rare.50"
  y <- "C3.rare.15"
  xlab <- "15-record"
  ylab <- "50-record"
  save <- "C3 native 50-15-record correlation.jpeg" 
  
  ggplotRegression(df, x, y, xlab, ylab, save)
  
  
# c4 ---------------------------------------------------------------
  df <- int
  x <- "C4.rare.50"
  y <- "C4.rare.15"
  xlab <- "15-record"
  ylab <- "50-record"
  save <- "C4 native 50-15-record correlation.jpeg" 
  
  ggplotRegression(df, x, y, xlab, ylab, save)
  
  
# Int -----------------------------------------------------------------------------------
# total ---------------------------------------------------------------
  df <- int
  x <- "total.rare.50"
  y <- "total.rare.15"
  xlab <- "15-record"
  ylab <- "50-record"
  save <- "Total introduced 50-15-record correlation.jpeg" 
  
  ggplotRegression(df, x, y, xlab, ylab, save)
  
# c3 ---------------------------------------------------------------
  df <- int
  x <- "C3.rare.50"
  y <- "C3.rare.15"
  xlab <- "15-record"
  ylab <- "50-record"
  save <- "C3 introduced 50-15-record correlation.jpeg" 
  
  ggplotRegression(df, x, y, xlab, ylab, save)
  
  
# c4 ---------------------------------------------------------------
  df <- nat
  x <- "C4.rare.50"
  y <- "C4.rare.15"
  xlab <- "15-record"
  ylab <- "50-record"
  save <- "C4 introduced 50-15-record correlation.jpeg" 
  
  ggplotRegression(df, x, y, xlab, ylab, save)
  
  
 
# --------------------------------------------------------------------------------------- 

# Stolen from Rarefaction cahpter plots -------------------------------------------------
# May or may not hav ebene useful
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
  
  
  
  # ------------------------------------------------------------------------------  
  
  # GG Theme assist --------------------------------------------------------------
  # Notes on two above packages 
  plot1 <- ggplot(data=dat, aes(x=long, y=lat)) + geom_point(aes(col=dat$bamboo, size=dat$prop.aban)) # or whatever your plot is
  
  ggThemeAssistGadget(plot1) # makes an easy-to-use interface and then returns the code that configures your plot how you like it
  # --------------------------------------------------------------------  
  
  
  
# GGplot 2 theme assist ----------------------------------------------------------------- 
  library(gplot2)
  
  library(ggThemeAssist)
  
  plot1 <- ggplot(data=dat, aes(x=long, y=lat)) + geom_point(aes(col=dat$bamboo, size=dat$prop.aban)) # or whatever your plot is
  
  ggThemeAssistGadget(plot1) # makes an easy-to-use interface and then returns the code that configures your plot how you like it!
  
  # ---------------------------------------------------------------------------