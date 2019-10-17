
#  Single-scale plots 18/1/18 ------------------------------------------

  rm(list = ls())
  
  library(ggplot2)
  library(ggmap)
  library(tidyr)
  library(raster)
  library(rgdal)
  library(maptools)
  library(dplyr)

  setwd("C:/Users/s436862/Dropbox/Climate Matching/1. Data files/")

  poa <- read.csv("C:/Users/s436862/Dropbox/Climate Matching/4. Results/Poa/100 km scale/single-scale SRE dataframes.csv")
  
# (Single-scale) SRE-record no. Pearson correlations --------------------------

# # gather relevant data; change names for legend  
#   poa_cor <- dplyr::select(poa, a_cor, ichao_cor, chao1_cor, jack1st_cor, jack2nd_cor, lanum_cor)
#   colnames(poa_cor) <- c("a", "ichao1", "chao1", "jack1st", "jack2nd", "lanum")
# 
# # long data format 
#   poa_clong <- select(poa_cor, a, n_ichao1_cor, n_chao1_cor, n_jack1st_cor, n_jack2nd_cor, n_lanum_cor)%>%
#     gather(sample_sre_correlation, value, -cell_width)
#   # ggplot
#   a_sre_correlation <- ggplot(data = poa_long, aes(x = cell_width, y = value, colour = sample_sre_correlation)) +
#     geom_line(size = 0.5) +
#     geom_point(size = 2) +
#     theme_bw() + 
#     labs(x = "cell width (km)",
#          y = "correlation (Pearson's %)") +
#     theme(axis.title = element_text(size = 14))
#   # save
#   ggsave("AVH/EDA/n_SREs pearsons correlations AVH.jpeg", plot = last_plot(), scale = 1, dpi = 300, device = "jpeg")
  
  
  
  
  
    
# SREs ----------------------------------------  
  # long data format
  nat_long_sre <- select(nat, cell_width, a, chao1_sre, ichao1_sre, jack1st, jack2nd)%>%
    gather(sre, value, -cell_width)
  
  # ggplot
  sres <- ggplot(data = nat_long_sre, aes(x = cell_width, y = value, colour = sre)) +
    geom_line(size = 0.5) +
    geom_point(size = 2) +
    #facet_wrap(~sre) +
    #geom_linerange(aes(ymax = chao1_sd, ymin = chao1sd), size = 0.3) +
    #scale_y_continuous(limits = c(20,200)) +
    #scale_x_continuous(breaks = seq(50, 200, 20)) +
    labs(x = "cell width (km)",
         y = "Species Richness") +
    theme(axis.title = element_text(size = 14)
    )
  # save
  jpeg("C:/Users/s436862/Dropbox/Climate Matching/4. Results/Poa/Multiscales/Native/SREs NAT.jpeg", width = 16, height = 10, units = 'cm', res = 300)
  sres
  dev.off()
  
# coverage -------------------------------------------------------------
  coverage <- ggplot(data = nat, aes(x = cell_width, y = coverage)) +
    geom_line(size = 0.5) +
    geom_point(size = 2) +
    #scale_y_continuous(breaks = seq(0.55, 0.8, 0.1)) +
    #scale_x_continuous(breaks = seq(50, 200, 20)) +
    labs(x = "cell width (km)",
         y = "coverage (%)") +
    theme(axis.title = element_text(size = 14)
    )
  
  jpeg("C:/Users/s436862/Dropbox/Climate Matching/4. Results/Poa/Multiscales/Native/coverage NAT.jpeg", width = 16, height = 10, units = 'cm', res = 300)
  coverage
  dev.off()
 
# community coverages -------------------------------------------------------------
  # long data format
  nat_long_comm <- select(nat, cell_width, zero, v.low, low, medium, high, v.high)%>%
    gather(community_size, value, -cell_width)
  # ggplot
  comm_cov <- ggplot(data = nat_long_comm, aes(x = cell_width, y = value, colour = community_size)) +
    geom_line(size = 0.5) +
    geom_point(size = 2) +
    #scale_y_continuous(breaks = seq(0.55, 0.8, 0.1)) +
    #scale_x_continuous(breaks = seq(50, 200, 20)) +
    labs(x = "cell width (km)",
         y = "cells occupied (%)") +
    theme(axis.title = element_text(size = 14)
    )
  # save
  jpeg("C:/Users/s436862/Dropbox/Climate Matching/4. Results/Poa/Multiscales/Native/community coverages NAT.jpeg", width = 16, height = 10, units = 'cm', res = 300)
  comm_cov
  dev.off()
  
# Chao1:iChao1 ratio ----------------------------------------
  actual_ichao1_ratio <- ggplot(data = nat, aes(x = cell_width, y = a/ichao1_sre)) +
    geom_line(size = 0.5) +
    geom_point(size = 2) +
    #geom_linerange(aes(ymax = ci_pos, ymin = ci_neg), size = 0.3) +
    #scale_y_continuous(limits = c(0,1)) +
    #scale_x_continuous(breaks = seq(50, 200, 20)) +
    labs(x = "cell width (km)",
         y = "Actual:iChao1 ratio") +
    theme(axis.title = element_text(size = 14)
    )
  
  jpeg("C:/Users/s436862/Dropbox/Climate Matching/4. Results/Poa/Multiscales/Native/actualchao1 ratio NAT.jpeg", width = 16, height = 10, units = 'cm', res = 300)
  actual_ichao1_ratio
  dev.off()
  
# iChao1 vs. Chao1 ----------------------------------------
  ichao1_chao1 <- ggplot(data = nat, aes(x = chao1_sre, y = (ichao1_sre))) +
    geom_line(size = 0.5) +
    geom_point(size = 2) +
    #geom_linerange(aes(ymax = ci_pos, ymin = ci_neg), size = 0.3) +
    #scale_y_continuous(limits = c(20,200)) +
    #scale_x_continuous(breaks = seq(50, 200, 20)) +
    labs(x = "chao1",
         y = "iChao1") +
    theme(axis.title = element_text(size = 14)
    )
  
  jpeg("C:/Users/s436862/Dropbox/Climate Matching/4. Results/Poa/Multiscales/Native/ichao1 v chao1 NAT.jpeg", width = 16, height = 10, units = 'cm', res = 300)
  ichao1_chao1
  dev.off()
  
  
# Chao1_Chiu_SD ----------------------------------------  
  sd <- ggplot(data = nat, aes(x = cell_width, y = chao1_chiu_sd)) +
    geom_line(size = 0.5) +
    geom_point(size = 2) +
    #geom_linerange(aes(ymax = chao1_sd, ymin = chao1sd), size = 0.3) +
    #scale_y_continuous(limits = c(20,200)) +
    #scale_x_continuous(breaks = seq(50, 200, 20)) +
    labs(x = "cell width (km)",
         y = "Chao1 SD") +
    theme(axis.title = element_text(size = 14)
    )
  
  jpeg("C:/Users/s436862/Dropbox/Climate Matching/4. Results/Poa/Multiscales/Native/Chao1 SD NAT.jpeg", width = 16, height = 10, units = 'cm', res = 300)
  sd
  dev.off()

  
# mean intensity (SD) in recordskm-2 in cells with at least one records  ----------------------------------------   
# Soberon, 2007
# In english, mean sample intenisty of sampled areas, and sd of it for different scales
  
  mi <- ggplot(data = nat, aes(x = cell_width, y = chao1_chiu_sd)) +
    geom_line(size = 0.5) +
    geom_point(size = 2) +
    #geom_linerange(aes(ymax = chao1_sd, ymin = chao1sd), size = 0.3) +
    #scale_y_continuous(limits = c(20,200)) +
    #scale_x_continuous(breaks = seq(50, 200, 20)) +
    labs(x = "cell width (km)",
         y = "Chao1 SD") +
    theme(axis.title = element_text(size = 14)
    )
  
  jpeg("C:/Users/s436862/Dropbox/Climate Matching/4. Results/Poa/Multiscales/Native/Chao1 SD NAT.jpeg", width = 16, height = 10, units = 'cm', res = 300)
  sd
  dev.off()
 
  
  
  
  

  
