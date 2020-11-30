
#  Multiscale SR plots 2/11 ------------------------------------------

  rm(list = ls())
  
  library(ggplot2)
  library(ggmap)
  library(tidyr)
  library(raster)
  library(rgdal)
  library(maptools)
  library(dplyr)

  setwd("C:/Users/s436862/Dropbox/Climate Matching/4. Results/")

  
  
# _________________ Native grasses _________________
  nat <- read.csv("Grass groups AVH/Multiscales/Nat SRE DF.csv")
  
# SREs ----------------------------------------  
# long data format
  nat_long_sre <- select(nat, cell_width, a, chao1, ichao1, jack1st, jack2nd, lanum)%>%
    gather(sre, value, -cell_width)
  chao_ci <- select(nat, cell_width, upper_ci, lower_ci) %>%
    mutate(sre = "chao1")
  nat2 <- left_join(nat_long_sre, chao_ci)
  
# ggplot
  sres <- ggplot(data = nat2, aes(x = cell_width, y = value, colour = sre)) +
    geom_line(size = 0.5) +
    geom_point(size = 2) +
    geom_errorbar(aes(ymax = upper_ci, ymin = lower_ci), size = 0.3) +
    labs(x = "Cell width (km)",
         y = "Mean species richness") +
    theme_bw() +
    theme(axis.title = element_text(size = 14))
  sres
  
# save
  ggsave("Grass groups AVH/Multiscales/Nat SRE-scale plot.jpeg", plot = last_plot(), scale = 1, dpi = 300, device = "jpeg")
  
  
  
# (Multi-scale) SRE-record# correlations --------------------------
# changle names
  nat_cor <- select(nat, cell_width, n_a_cor, n_ichao1_cor, n_chao1_cor, n_jack1st_cor, n_jack2nd_cor, n_lanum_cor)
  colnames(nat_cor) <- c("cell_width", "a", "ichao1", "chao1", "jack1st", "jack2nd", "lanum")

# long data format
  nat_eda_long <- select(nat_cor, cell_width, a, ichao1, chao1, jack1st, jack2nd, lanum)%>%
    gather(sample_sre_correlation, value, -cell_width)

# ggplot
  a_sre_correlation <- ggplot(data = nat_eda_long, aes(x = cell_width, y = value, colour = sample_sre_correlation)) +
    geom_line(size = 0.5) +
    geom_point(size = 2) +
    theme_bw() + 
    labs(x = "Cell width (km)",
         y = "Correlation (Pearson's)") +
    theme(axis.title = element_text(size = 14))

# save
  ggsave("Grass groups AVH/Multiscales/Nat SRE-pearson correlations plot.jpeg", plot = last_plot(), scale = 1, dpi = 300, device = "jpeg")
  
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
 
  
  
  
  
  
  
  
  
  
  # nat_long <- select(nat, cell_width, zero, v.low, low, medium, high, v.high)%>%
  #   gather(community_size, value, -cell_width) # comm_no/sre = key (factor for each sre), value = ... value, -cellwidth cos we don't need that shiz
  # 
  # # reverse that
  # nat_wide <- spread(nat_long, sre, value, fill = NA)
  
  # Note: any missing values in any of the sre's, can pass 'fill' in (deafult is NA)
  
  # (turn these into functions when I have Poa sorted as best ap)
  

  
