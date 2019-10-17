
# AVH plots at 50, 100 and 200 km cell widths

  library(rasterVis)
  library(ggplot2)
  library(ggmap)
  library(tidyr)
  library(raster)
  library(rgdal)
  library(maptools)
  library(dplyr)
  
  
  setwd("C:/Users/s436862/Dropbox/Climate Matching/4. Results")

# 50 km data -------------------------------------------------

rm(list = ls())

avh_50 <- read.csv("AVH/50 km scale/50km SREs AVH.csv")

# mean intensity (survey effort vs. C) -------------------
  mi_se <- ggplot(data = avh_50, aes(x = mean_intensity, y = jost_ratio)) +
    geom_point(size = 1) +
    labs(x = "records km-2",
         y = "C") +
  theme_bw() +
    theme(axis.title = element_text(size = 14)
    )
# save
  ggsave("AVH/50 km scale/50km C vs mean intensity AVH.jpeg", plot = last_plot(), scale = 1, dpi = 300, device = "jpeg")
  
# iChao1 vs. Chao1 ----------------------------------------
  ichao1_chao1 <- ggplot(data = avh_50, aes(x = chao1, y = ichao1)) +
    geom_point(size = 1) +
    labs(x = "chao1",
         y = "iChao1") +
    theme(axis.title = element_text(size = 14)
    )
  
# save: ratio
  ggsave("AVH/50 km scale/ichao1 v chao1 AVH.jpeg", plot = last_plot(), scale = 1, dpi = 300, device = "jpeg")
  
# histogram of ratio (C) --------------------------------------
  histo_50 <- ggplot(data = avh_50, aes(x = C)) +
    geom_histogram(binwidth = 0.05) +
    theme_grey() +
    labs(x = "sampling completedness",
         y = "frequency") +
    theme(axis.title = element_text(size = 14)
    )
  histo_50
  
# save: ratio
  ggsave("AVH/50 km scale/ratio histogram AVH.jpeg", plot = last_plot(), scale = 1, dpi = 300, device = "jpeg")
# histogram of ratio (Jost C) --------------------------------------
  histo_jost_50 <- ggplot(data = avh_50, aes(x = jost_ratio)) +
    geom_histogram(binwidth = 0.05) +
    labs(x = "Jost Coverage (C)",
         y = "frequency") +
   theme(axis.title = element_text(size = 14) +
           theme_bw()
    )
  histo_jost_50
  
  # save: ratio
  ggsave("AVH/50 km scale/Jost C ratio histogram AVH.jpeg", plot = last_plot(), scale = 1, dpi = 300, device = "jpeg")
# --------------------------------------
  
# 100 km data -------------------------------------------------
  
  rm(list = ls())
  
  avh_100 <- read.csv("AVH/100 km scale/100km SREs AVH.csv")
  
# mean intensity (survey effort vs. C) -------------------
  mi_se <- ggplot(data = avh_100, aes(x = mean_intensity, y = jost_ratio)) +
    geom_point(size = 1) +
    labs(x = "records km-2",
         y = "C") +
    theme_bw() +
    theme(axis.title = element_text(size = 14)
    )
  # save
  ggsave("AVH/100 km scale/100km C vs mean intensity AVH.jpeg", plot = last_plot(), scale = 1, dpi = 300, device = "jpeg")
  
# iChao1 vs. Chao1 ----------------------------------------
  ichao1_chao1 <- ggplot(data = avh_100, aes(x = chao1, y = ichao1)) +
    geom_point(size = 1) +
    labs(x = "chao1",
         y = "iChao1") +
    theme(axis.title = element_text(size = 14)
    )
  
  # save: ratio
  ggsave("AVH/100 km scale/ichao1 v chao1 AVH.jpeg", plot = last_plot(), scale = 1, dpi = 300, device = "jpeg")
  
  
  
  
  
  
  
  
  
  
# histogram of ratio (C) --------------------------------------
  histo_100 <- ggplot(data = avh_100, aes(x = C)) +
    geom_histogram(binwidth = 0.05) +
    theme_grey() +
    labs(x = "sampling completedness",
         y = "frequency") +
    theme(axis.title = element_text(size = 14)
    )
  histo_100
  
  # save: ratio
  ggsave("AVH/100 km scale/ratio histogram AVH.jpeg", plot = last_plot(), scale = 1, dpi = 300, device = "jpeg")
# histogram of ratio (Jost C) --------------------------------------
  histo_jost_100 <- ggplot(data = avh_100, aes(x = jost_ratio)) +
    geom_histogram(binwidth = 0.05) +
    theme_grey() +
    labs(x = "Jost Coverage (C)",
         y = "frequency") +
    theme(axis.title = element_text(size = 14)
    )
  histo_50
  
  # save: ratio
  ggsave("AVH/100 km scale/Jost C ratio histogram AVH.jpeg", plot = last_plot(), scale = 1, dpi = 300, device = "jpeg")
# --------------------------------------
  
# 200 km data -------------------------------------------------
  
  rm(list = ls())
  
  avh_200 <- read.csv("AVH/200 km scale/200km SREs AVH.csv")
  
# mean intensity (survey effort vs. C) -------------------
  mi_se <- ggplot(data = avh_200, aes(x = mean_intensity, y = jost_ratio)) +
    geom_point(size = 1) +
    labs(x = "records km-2",
         y = "C") +
  theme_bw() +
    theme(axis.title = element_text(size = 14)
    )
  # save
  ggsave("AVH/200 km scale/200km C vs mean intensity AVH.jpeg", plot = last_plot(), scale = 1, dpi = 300, device = "jpeg")
  
# iChao1 vs. Chao1 ----------------------------------------
  ichao1_chao1 <- ggplot(data = avh_200, aes(x = chao1, y = ichao1)) +
    geom_point(size = 1) +
    labs(x = "chao1",
         y = "iChao1") +
    theme(axis.title = element_text(size = 14)
    )
  
  # save: ratio
  ggsave("AVH/200 km scale/ichao1 v chao1 AVH.jpeg", plot = last_plot(), scale = 1, dpi = 300, device = "jpeg")
  
  
# histogram of ratio (C) --------------------------------------
  histo_200 <- ggplot(data = avh_200, aes(x = C)) +
    geom_histogram(binwidth = 0.05) +
    theme_grey() +
    labs(x = "sampling completedness",
         y = "frequency") +
    theme(axis.title = element_text(size = 14)
    )
  histo_200
  
  # save: ratio
  ggsave("AVH/200 km scale/ratio histogram AVH.jpeg", plot = last_plot(), scale = 1, dpi = 300, device = "jpeg")
# histogram of ratio (Jost C) --------------------------------------
  histo_jost_200 <- ggplot(data = avh_200, aes(x = jost_ratio)) +
    geom_histogram(binwidth = 0.05) +
    theme_grey() +
    labs(x = "Jost Coverage (C)",
         y = "frequency") +
    theme(axis.title = element_text(size = 14)
    )
  histo_50
  
  # save: ratio
  ggsave("AVH/200 km scale/Jost C ratio histogram AVH.jpeg", plot = last_plot(), scale = 1, dpi = 300, device = "jpeg") 
# --------------------------------------
  
  
  