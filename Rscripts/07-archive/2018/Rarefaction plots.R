# Date created: 14/3/18
# Last updated: 20/3

####################### Raster rarefication plots ################################
# Raster plots for rarefaction dataframe on Poa, Native and Introduced species
# Need to update for changing scale and cutoff points (atm it's 100 km and 50, respectively)

# From the results of the cels occupied figure, I have decided to compare all at 100 km
# Specifically: (1) 25, (2) 50, & (3) 100 record-cutoffs; (4) Chao1 (5) baseline and (6) Lanum
# And doing this for (a) Poa (b) native (c) introduced distributions for a selection of the above

# Based on: Rarefaction df + rasters & Nat SR chao1 C rasters

# Concurrent with: rarefaction models (yet to be completed)

  rm(list = ls())
  
  library(raster)
  library(ggmap)
  library(tidyr)
  library(rgdal)
  library(maptools)
  library(ggplot2)
  library(dplyr)
  library(rasterVis)
  
  setwd("C:/Users/s436862/Dropbox/Rarefaction/4. Results/Grass groups AVH/Rarefaction")
  
# Australian border
  oz <- borders(database = "world", region = "Australia")
  
######################### Community coverage #########################  
# Community coverage across scale and cell-record cutoffs ------------
  cell_prop <- read.csv("CSV/Multiscale_rare_cell_occupation.csv")
  
# long data format
  cell_prop_long <- select(cell_prop, cell_width, all_records, twenty_five, fifty, one_hundred, two_fifty, five_hundred)%>%
    gather(Cutoff_val, value, -cell_width)
  
# ggplot
  comm_plot <- ggplot(data = cell_prop_long, aes(x = cell_width, y = value, colour = Cutoff_val)) +
    geom_line(size = 0.5) +
    geom_point(size = 2) +
    theme_bw() +
    labs(x = "Cell width (km)",
         y = "Cells occupied (%)") +
    theme(axis.title = element_text(size = 14)
    )
  comm_plot
  
# save
  ggsave("Graphs/Cell proportions occupied rarefied cutoffs.jpeg", plot = last_plot(), scale = 1, dpi = 500, device = "jpeg")
  
# --------------------------------------------------------------------  
    

############################# Rarefied ###############################
# Rarefied Poa (total) richness -------------------------
  poa_rare <- raster("Raster/Poa_rare_50.grd") 
  # arid <- raster("C:/Users/s436862/Dropbox/Climate Matching/1. Data files/EFs/EFs cropped/arid")
  # arid <- aggregate(arid, fac = 100, fun = mean)
  # plot(arid)
  
  q <- gplot(poa_rare) + 
    theme_classic()+
    geom_raster(aes(fill = value)) +
    scale_fill_gradient(low = 'white',
                        high = 'red',
                        na.value="grey",
                        limits = c(1,50)) +
    coord_equal() +
    labs(title = "Rarefaction 50 records",
         x = "Long",
         y = "Lat",
         fill = "RarePoa") +
    oz +
    coord_cartesian(xlim = c(112, 155), ylim = c(-45, -7), expand = F) +
    theme(aspect.ratio = 0.88)
  
  print(q)

  ggsave("Graphs/Poa_rare_50.jpeg", plot = last_plot(), scale = 1, dpi = 500, device = "jpeg")
  
# Rarefied introduced richness -------------------------
  int_rare <- raster("Raster/Int_rare_50.grd")
  
  q <- gplot(int_rare) + 
    theme_classic()+
    geom_raster(aes(fill = value)) +
    scale_fill_gradient(low = 'white',
                        high = 'red',
                        na.value="grey",
                        limits = c(1,50)) +
    coord_equal() +
    labs(title = "Rarefaction 50 records",
         x = "Long",
         y = "Lat",
         fill = "RareIntroduced") +
    oz +
    coord_cartesian(xlim = c(112, 155), ylim = c(-45, -7), expand = F) +
    theme(aspect.ratio = 0.88)
  
  print(q)
  
  ggsave("Graphs/Int_rare_50.jpeg", plot = last_plot(), scale = 1, dpi = 500, device = "jpeg")
  
# Rarefied native richness -------------------------------
  nat_rare <- raster("Raster/Nat_rare_50.grd")
  
  q <- gplot(nat_rare) + 
    theme_classic()+
    geom_raster(aes(fill = value)) +
    scale_fill_gradient(low = 'white',
                        high = 'red',
                        na.value="grey",
                        limits = c(1,50)) +
    coord_equal() +
    labs(title = "Rarefaction 50 records",
         x = "Long",
         y = "Lat",
         fill = "RareNative") +
    oz +
    coord_cartesian(xlim = c(112, 155), ylim = c(-45, -7), expand = F) +
    theme(aspect.ratio = 0.88)
  
  print(q)
  
  ggsave("Graphs/Nat_rare_50.jpeg", plot = last_plot(), scale = 1, dpi = 500, device = "jpeg")

# Rarefied proportion of Int:total -----------------------
  NI_rare <- raster("Raster/IntProp_rare_50.grd")
  
  q <- gplot(NI_rare) + 
    theme_classic()+
    geom_raster(aes(fill = value)) +
    scale_fill_gradient(low = 'white',
                        high = 'red',
                        na.value="grey",
                        limits = c(0,1)) +
    coord_equal() +
    labs(title = "Rarefaction 50 records",
         x = "Long",
         y = "Lat",
         fill = "Introduced proportion") +
    oz +
    coord_cartesian(xlim = c(112, 155), ylim = c(-45, -7), expand = F) +
    theme(aspect.ratio = 0.88)
  
  print(q)
  
  ggsave("Graphs/IntProp_baseline.jpeg", plot = last_plot(), scale = 1, dpi = 500, device = "jpeg")

# -------------------------------------------------------------------    
############################# Baseline rasters ################################  
# Poa richness ----------------------------------  
  actual <- raster("Raster/Poa_a.grd")
  
  q <- gplot(actual) + 
    theme_classic()+
    geom_raster(aes(fill = value)) +
    scale_fill_gradient(low = 'white',
                        high = 'red',
                        na.value="grey",
                        limits = c(1,300)) +
    coord_equal() +
    labs(title = "100 km",
         x = "Long",
         y = "Lat",
         fill = "Poa SR") +
    oz +
    coord_cartesian(xlim = c(112, 155), ylim = c(-45, -7), expand = F) +
    theme(aspect.ratio = 0.88)
  
  print(q)
  
  ggsave("Graphs/Poa_a.jpeg", plot = last_plot(), scale = 1, dpi = 500, device = "jpeg")

# Native richness ----------------------------------  
  nat <- raster("Raster/Nat_a.grd")
  
  q <- gplot(nat) + 
    theme_classic()+
    geom_raster(aes(fill = value)) +
    scale_fill_gradient(low = 'white',
                        high = 'red',
                        na.value="grey",
                        limits = c(1,200)) + # cuts out a few at this scale, but better richness differences witht his legend
    coord_equal() +
    labs(title = "100 km",
         x = "Long",
         y = "Lat",
         fill = "Native SR") +
    oz +
    coord_cartesian(xlim = c(112, 155), ylim = c(-45, -7), expand = F) +
    theme(aspect.ratio = 0.88)
  
  print(q)
  
  ggsave("Graphs/Nat_a.jpeg", plot = last_plot(), scale = 1, dpi = 500, device = "jpeg")  
  
 
# Introduced richness ----------------------------------  
  int <- raster("Raster/Int_a.grd")
  
  q <- gplot(int) + 
    theme_classic()+
    geom_raster(aes(fill = value)) +
    scale_fill_gradient(low = 'white',
                        high = 'red',
                        na.value="grey",
                        limits = c(1,200)) + # cuts out a few at this scale, but better richness differences witht his legend
    coord_equal() +
    labs(title = "100 km",
         x = "Long",
         y = "Lat",
         fill = "Introduced SR") +
    oz +
    coord_cartesian(xlim = c(112, 155), ylim = c(-45, -7), expand = F) +
    theme(aspect.ratio = 0.88)
  
  print(q)
  
  ggsave("Graphs/Int_a.jpeg", plot = last_plot(), scale = 1, dpi = 500, device = "jpeg")  
  
  
# Baseline proportion of Int:total -----------------------
  NI_bl <- raster("Raster/Int_proportion_baseline.grd")
  
  q <- gplot(NI_bl) + 
    theme_classic()+
    geom_raster(aes(fill = value)) +
    scale_fill_gradient(low = 'white',
                        high = 'red',
                        na.value="grey",
                        limits = c(0,1)) +
    coord_equal() +
    labs(title = "Introduced:total proportion",
         x = "Long",
         y = "Lat",
         fill = "Intro prop baseline") +
    oz +
    coord_cartesian(xlim = c(112, 155), ylim = c(-45, -7), expand = F) +
    theme(aspect.ratio = 0.88)
  
  print(q)
  
  ggsave("Graphs/IntProp_baseline.jpeg", plot = last_plot(), scale = 1, dpi = 500, device = "jpeg")
# --------------------------------------------------------------------

######################## Jackknife Second-order rasters ######################### 
# Jack Poa ----------------------------------  
  j_poa <- raster("Raster/Poa_jack2nd.grd")
  
  q <- gplot(j_poa) + 
    theme_classic()+
    geom_raster(aes(fill = value)) +
    scale_fill_gradient(low = 'white',
                        high = 'red',
                        na.value="grey",
                        limits = c(1,300)) +
    coord_equal() +
    labs(title = "100 km",
         x = "Long",
         y = "Lat",
         fill = "Jackknife Poa SR") +
    oz +
    coord_cartesian(xlim = c(112, 155), ylim = c(-45, -7), expand = F) +
    theme(aspect.ratio = 0.88)
  
  print(q)
  
  ggsave("Graphs/Poa_jack.jpeg", plot = last_plot(), scale = 1, dpi = 500, device = "jpeg")
  
# Jack Nat ----------------------------------  
  j_nat <- raster("Raster/Nat_jack2nd.grd")
  
  q <- gplot(j_nat) + 
    theme_classic()+
    geom_raster(aes(fill = value)) +
    scale_fill_gradient(low = 'white',
                        high = 'red',
                        na.value="grey",
                        limits = c(1,300)) +
    coord_equal() +
    labs(title = "100 km",
         x = "Long",
         y = "Lat",
         fill = "Jackknife Nat SR") +
    oz +
    coord_cartesian(xlim = c(112, 155), ylim = c(-45, -7), expand = F) +
    theme(aspect.ratio = 0.88)
  
  print(q)
  
  ggsave("Graphs/Nat_jackknife.jpeg", plot = last_plot(), scale = 1, dpi = 500, device = "jpeg")
  
# Jack int ----------------------------------  
  j_int <- raster("Raster/Int_jack2nd.grd")
  
  q <- gplot(j_int) + 
    theme_classic()+
    geom_raster(aes(fill = value)) +
    scale_fill_gradient(low = 'white',
                        high = 'red',
                        na.value="grey",
                        limits = c(1,300)) +
    coord_equal() +
    labs(title = "100 km",
         x = "Long",
         y = "Lat",
         fill = "Jackknife Int SR") +
    oz +
    coord_cartesian(xlim = c(112, 155), ylim = c(-45, -7), expand = F) +
    theme(aspect.ratio = 0.88)
  
  print(q)
  
  ggsave("Graphs/Int_jackknife.jpeg", plot = last_plot(), scale = 1, dpi = 500, device = "jpeg")
  
# --------------------------------------------------------------------     
############################## Chao1 rasters ################################## 
# Chao1 Poa ----------------------------------  
  c_poa <- raster("Raster/Poa_chao1.grd")
  
  q <- gplot(c_poa) + 
    theme_classic()+
    geom_raster(aes(fill = value)) +
    scale_fill_gradient(low = 'white',
                        high = 'red',
                        na.value="grey",
                        limits = c(1,300)) +
    coord_equal() +
    labs(title = "100 km",
         x = "Long",
         y = "Lat",
         fill = "Chao1 Poa SR") +
    oz +
    coord_cartesian(xlim = c(112, 155), ylim = c(-45, -7), expand = F) +
    theme(aspect.ratio = 0.88)
  
  print(q)
  
  ggsave("Graphs/Poa_chao1.jpeg", plot = last_plot(), scale = 1, dpi = 500, device = "jpeg")
  
# Chao1 Nat ----------------------------------  
  c_nat <- raster("Raster/Nat_chao1.grd")
  
  q <- gplot(c_nat) + 
    theme_classic()+
    geom_raster(aes(fill = value)) +
    scale_fill_gradient(low = 'white',
                        high = 'red',
                        na.value="grey",
                        limits = c(1,300)) +
    coord_equal() +
    labs(title = "100 km",
         x = "Long",
         y = "Lat",
         fill = "Chao1 Nat SR") +
    oz +
    coord_cartesian(xlim = c(112, 155), ylim = c(-45, -7), expand = F) +
    theme(aspect.ratio = 0.88)
  
  print(q)
  
  ggsave("Graphs/Nat_chao1.jpeg", plot = last_plot(), scale = 1, dpi = 500, device = "jpeg")
  
# Chao1 int ----------------------------------  
  c_int <- raster("Raster/Int_chao1.grd")
  
  q <- gplot(c_int) + 
    theme_classic()+
    geom_raster(aes(fill = value)) +
    scale_fill_gradient(low = 'white',
                        high = 'red',
                        na.value="grey",
                        limits = c(1,300)) +
    coord_equal() +
    labs(title = "100 km",
         x = "Long",
         y = "Lat",
         fill = "Chao1 int SR") +
    oz +
    coord_cartesian(xlim = c(112, 155), ylim = c(-45, -7), expand = F) +
    theme(aspect.ratio = 0.88)
  
  print(q)
  
  ggsave("Graphs/Int_chao1.jpeg", plot = last_plot(), scale = 1, dpi = 500, device = "jpeg")
# Chao1 proportion of Int:total -----------------------
  NI_c <- raster("Raster/Int_proportion_Chao1.grd")
  
  q <- gplot(NI_c) + 
    theme_classic()+
    geom_raster(aes(fill = value)) +
    scale_fill_gradient(low = 'white',
                        high = 'red',
                        na.value="grey",
                        limits = c(0,1)) +
    coord_equal() +
    labs(title = "Introduced:total proportion",
         x = "Long",
         y = "Lat",
         fill = "Int prop Chao1") +
    oz +
    coord_cartesian(xlim = c(112, 155), ylim = c(-45, -7), expand = F) +
    theme(aspect.ratio = 0.88)
  
  print(q)
  
  ggsave("Graphs/IntProp_chao1.jpeg", plot = last_plot(), scale = 1, dpi = 500, device = "jpeg")  
# --------------------------------------------------------------------  
  
############################## iChao1 ################################## 
# ichao1 Poa ----------------------------------  
  ic_poa <- raster("Raster/Poa_ichao1.grd")
  
  q <- gplot(ic_poa) + 
    theme_classic()+
    geom_raster(aes(fill = value)) +
    scale_fill_gradient(low = 'white',
                        high = 'red',
                        na.value="grey",
                        limits = c(1,300)) + # guess keep all these the same
    coord_equal() +
    labs(title = "100 km",
         x = "Long",
         y = "Lat",
         fill = "iChao1 Poa SR") +
    oz +
    coord_cartesian(xlim = c(112, 155), ylim = c(-45, -7), expand = F) +
    theme(aspect.ratio = 0.88)
  
  print(q)
  
  ggsave("Graphs/Poa_ichao1.jpeg", plot = last_plot(), scale = 1, dpi = 500, device = "jpeg")
  
# ichao1 Nat ----------------------------------  
  ic_nat <- raster("Raster/Nat_ichao1.grd")
  
  q <- gplot(ic_nat) + 
    theme_classic()+
    geom_raster(aes(fill = value)) +
    scale_fill_gradient(low = 'white',
                        high = 'red',
                        na.value="grey",
                        limits = c(1,300)) +
    coord_equal() +
    labs(title = "100 km",
         x = "Long",
         y = "Lat",
         fill = "ichao1 nat SR") +
    oz +
    coord_cartesian(xlim = c(112, 155), ylim = c(-45, -7), expand = F) +
    theme(aspect.ratio = 0.88)
  
  print(q)
  
  ggsave("Graphs/Nat_ichao1.jpeg", plot = last_plot(), scale = 1, dpi = 500, device = "jpeg")
  
# ichao1 Int ----------------------------------  
  ic_int <- raster("Raster/Int_ichao1.grd")
  
  q <- gplot(ic_int) + 
    theme_classic()+
    geom_raster(aes(fill = value)) +
    scale_fill_gradient(low = 'white',
                        high = 'red',
                        na.value="grey",
                        limits = c(1,300)) +
    coord_equal() +
    labs(title = "100 km",
         x = "Long",
         y = "Lat",
         fill = "ichao1 int SR") +
    oz +
    coord_cartesian(xlim = c(112, 155), ylim = c(-45, -7), expand = F) +
    theme(aspect.ratio = 0.88)
  
  print(q)
  
  ggsave("Graphs/Int_ichao1.jpeg", plot = last_plot(), scale = 1, dpi = 500, device = "jpeg")
# ---------------------------------------------------------------------

############################## Lanum ################################## 
# Lanum Poa ----------------------------------  
  l_poa <- raster("Raster/Poa_lanum.grd")
  
  q <- gplot(l_poa) + 
    theme_classic()+
    geom_raster(aes(fill = value)) +
    scale_fill_gradient(low = 'white',
                        high = 'red',
                        na.value="grey",
                        limits = c(1,500)) + # this need to be much higher ...
    coord_equal() +
    labs(title = "100 km",
         x = "Long",
         y = "Lat",
         fill = "Lanum Poa SR") +
    oz +
    coord_cartesian(xlim = c(112, 155), ylim = c(-45, -7), expand = F) +
    theme(aspect.ratio = 0.88)
  
  print(q)
  
  ggsave("Graphs/Poa_lanum.jpeg", plot = last_plot(), scale = 1, dpi = 500, device = "jpeg")
  
# lanum Nat ----------------------------------  
  l_nat <- raster("Raster/Nat_lanum.grd")
  
  q <- gplot(l_nat) + 
    theme_classic()+
    geom_raster(aes(fill = value)) +
    scale_fill_gradient(low = 'white',
                        high = 'red',
                        na.value="grey",
                        limits = c(1,500)) +
    coord_equal() +
    labs(title = "100 km",
         x = "Long",
         y = "Lat",
         fill = "lanum nat SR") +
    oz +
    coord_cartesian(xlim = c(112, 155), ylim = c(-45, -7), expand = F) +
    theme(aspect.ratio = 0.88)
  
  print(q)
  
  ggsave("Graphs/Nat_lanum.jpeg", plot = last_plot(), scale = 1, dpi = 500, device = "jpeg")
  
# lanum Int ----------------------------------  
  l_int <- raster("Raster/Int_lanum.grd")
  
  q <- gplot(l_int) + 
    theme_classic()+
    geom_raster(aes(fill = value)) +
    scale_fill_gradient(low = 'white',
                        high = 'red',
                        na.value="grey",
                        limits = c(1,500)) +
    coord_equal() +
    labs(title = "100 km",
         x = "Long",
         y = "Lat",
         fill = "lanum int SR") +
    oz +
    coord_cartesian(xlim = c(112, 155), ylim = c(-45, -7), expand = F) +
    theme(aspect.ratio = 0.88)
  
  print(q)
  
  ggsave("Graphs/Int_lanum.jpeg", plot = last_plot(), scale = 1, dpi = 500, device = "jpeg")
# ---------------------------------------------------------------------  
  
############################# Extra rasters ##################################
# Poa 50 km ----------------------------------------------------------  
  poa_50km <- raster("Raster/Poa_50km.grd")
  
  q <- gplot(poa_50km) + 
    theme_classic()+
    geom_raster(aes(fill = value)) +
    scale_fill_gradient(low = 'white',
                        high = 'red',
                        na.value="grey",
                        limits = c(1,300)) +
    coord_equal() +
    labs(title = "50 km",
         x = "Long",
         y = "Lat",
         fill = "Poa SR (50km)") +
    oz +
    coord_cartesian(xlim = c(112, 155), ylim = c(-45, -7), expand = F) +
    theme(aspect.ratio = 0.88)
  
  print(q)
  
  ggsave("Graphs/Poa_50km.jpeg", plot = last_plot(), scale = 1, dpi = 500, device = "jpeg")
  
# Poa 200 km ----------------------------------------------------------  
  poa_200km <- raster("Raster/Poa_200km.grd")
  
  q <- gplot(poa_200km) + 
    theme_classic()+
    geom_raster(aes(fill = value)) +
    scale_fill_gradient(low = 'white',
                        high = 'red',
                        na.value="grey",
                        limits = c(1,350)) +
    coord_equal() +
    labs(title = "200 km",
         x = "Long",
         y = "Lat",
         fill = "Poa SR (200km)") +
    oz +
    coord_cartesian(xlim = c(112, 155), ylim = c(-45, -7), expand = F) +
    theme(aspect.ratio = 0.88)
  
  print(q)
  
  ggsave("Graphs/Poa_200km.jpeg", plot = last_plot(), scale = 1, dpi = 500, device = "jpeg")
  
# Poa rarefied to 25 records -------------------------
  poa_rare_25 <- raster("Raster/Poa_rare_25.grd")    
  q <- gplot(poa_rare_25) + 
    theme_classic()+
    geom_raster(aes(fill = value)) +
    scale_fill_gradient(low = 'white',
                        high = 'red',
                        na.value="grey",
                        limits = c(1,25)) +
    coord_equal() +
    labs(title = "Rarefaction 25 records",
         x = "Long",
         y = "Lat",
         fill = "Rare Poa") +
    oz +
    coord_cartesian(xlim = c(112, 155), ylim = c(-45, -7), expand = F) +
    theme(aspect.ratio = 0.88)
  
  print(q)
  
  ggsave("Graphs/Poa_rare_25.jpeg", plot = last_plot(), scale = 1, dpi = 500, device = "jpeg")
  
# Nat rarefied to 25 records -------------------------
  nat_rare_25 <- raster("Raster/Nat_rare_25.grd")    
  q <- gplot(nat_rare_25) + 
    theme_classic()+
    geom_raster(aes(fill = value)) +
    scale_fill_gradient(low = 'white',
                        high = 'red',
                        na.value="grey",
                        limits = c(1,25)) +
    coord_equal() +
    labs(title = "Rarefaction 25 records",
         x = "Long",
         y = "Lat",
         fill = "Rare Native") +
    oz +
    coord_cartesian(xlim = c(112, 155), ylim = c(-45, -7), expand = F) +
    theme(aspect.ratio = 0.88)
  
  print(q)
  
  ggsave("Graphs/Nat_rare_25.jpeg", plot = last_plot(), scale = 1, dpi = 500, device = "jpeg")
  
# Int rarefied to 25 records -------------------------
  int_rare_25 <- raster("Raster/Int_rare_25.grd")    
  q <- gplot(int_rare_25) + 
    theme_classic()+
    geom_raster(aes(fill = value)) +
    scale_fill_gradient(low = 'white',
                        high = 'red',
                        na.value="grey",
                        limits = c(1,25)) +
    coord_equal() +
    labs(title = "Rarefaction 25 records",
         x = "Long",
         y = "Lat",
         fill = "Rare Introduced") +
    oz +
    coord_cartesian(xlim = c(112, 155), ylim = c(-45, -7), expand = F) +
    theme(aspect.ratio = 0.88)
  
  print(q)
  
  ggsave("Graphs/Int_rare_25.jpeg", plot = last_plot(), scale = 1, dpi = 500, device = "jpeg")
  
# Poa rarefied to 100 records -------------------------
  poa_rare_100 <- raster("Raster/Poa_rare_100.grd")    
  q <- gplot(poa_rare_100) + 
    theme_classic()+
    geom_raster(aes(fill = value)) +
    scale_fill_gradient(low = 'white',
                        high = 'red',
                        na.value="grey",
                        limits = c(1,100)) +
    coord_equal() +
    labs(title = "Rarefaction 100 records",
         x = "Long",
         y = "Lat",
         fill = "Rare Poa") +
    oz +
    coord_cartesian(xlim = c(112, 155), ylim = c(-45, -7), expand = F) +
    theme(aspect.ratio = 0.88)
  
  print(q)
  
  ggsave("Graphs/Poa_rare_100.jpeg", plot = last_plot(), scale = 1, dpi = 500, device = "jpeg")
  
# Ratio of f1:f2 
  ratio <- raster("Raster/F1-F2 ratio")    
  q <- gplot(ratio) + 
    theme_classic()+
    geom_raster(aes(fill = value)) +
    scale_fill_gradient(low = 'white',
                        high = 'red',
                        na.value="grey",
                        limits = c(0,8)) + # tricky
    coord_equal() +
    labs(title = "Ratio of single- and double-tons",
         x = "Long",
         y = "Lat",
         fill = "F1:f2 ratio") +
    oz +
    coord_cartesian(xlim = c(112, 155), ylim = c(-45, -7), expand = F) +
    theme(aspect.ratio = 0.88)
  
  print(q)
  
  ggsave("Graphs/F1-F2 ratio 0-8.jpeg", plot = last_plot(), scale = 1, dpi = 500, device = "jpeg")
# --------------------------------------------------------------------    
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  