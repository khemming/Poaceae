

# AVH EDA

  rm(list = ls())
  
  library(rasterVis)
  library(ggplot2)
  library(ggmap)
  library(tidyr)
  library(raster)
  library(rgdal)
  library(maptools)
  library(dplyr)
  
  
  setwd("C:/Users/s436862/Dropbox/Climate Matching/4. Results")


# dataframe
  avh_eda <- read.csv("AVH/EDA/multiscale EDA AVH.csv")

# (Multi-scale) SRE-record# correlations --------------------------
# long data format
  avh_eda_long <- select(avh_eda, cell_width, n_a_cor, n_ichao1_cor, n_chao1_cor, n_jack1st_cor, n_jack2nd_cor, n_lanum_cor)%>%
    gather(sample_sre_correlation, value, -cell_width)
# ggplot
  a_sre_correlation <- ggplot(data = avh_eda_long, aes(x = cell_width, y = value, colour = sample_sre_correlation)) +
    geom_line(size = 0.5) +
    geom_point(size = 2) +
    theme_bw() + 
    labs(x = "cell width (km)",
         y = "correlation (Pearson's %)") +
    theme(axis.title = element_text(size = 14))
# save
  ggsave("AVH/EDA/n_SREs pearsons correlations AVH.jpeg", plot = last_plot(), scale = 1, dpi = 300, device = "jpeg")
 

  
# SREs ----------------------------------------  
# long data format
  avh_long_sre <- select(avh_eda, cell_width, a, chao1, ichao1, jack1st, jack2nd, lanum)%>%
    gather(sre, value, -cell_width)
  chao_ci <- select(avh_eda, cell_width, upper_ci, lower_ci) %>%
    mutate(sre = "chao1")
  avh2 <- left_join(avh_long_sre, chao_ci)
  
# ggplot
  sres <- ggplot(data = avh2, aes(x = cell_width, y = value, colour = sre)) +
    geom_line(size = 0.5) +
    geom_point(size = 2) +
    geom_errorbar(aes(ymax = upper_ci, ymin = lower_ci), size = 0.3) +
    labs(x = "cell width (km)",
         y = "Mean species richness") +
    theme_bw() +
    theme(axis.title = element_text(size = 14))
  sres
# save
  ggsave("AVH/EDA/SREs AVH.jpeg", plot = last_plot(), scale = 1, dpi = 300, device = "jpeg")
  
# community coverages -------------------------------------------------------------
# long data format
  avh_long_comm <- select(avh_eda, cell_width, zero, one_10, eleven_50, fifty_one_200, two_hundred_500, five_hundred_plus)%>%
    gather(record_no, value, -cell_width)
  
# note: change legend titesl for record_no from low, med high, etc. to actual numbers
  
# ggplot
  comm_cov <- ggplot(data = avh_long_comm, aes(x = cell_width, y = value, colour = record_no)) +
    geom_line(size = 0.5) +
    geom_point(size = 2) +
    theme_bw() +
    #scale_y_continuous(breaks = seq(0.55, 0.8, 0.1)) +
    #scale_x_continuous(breaks = seq(50, 200, 20)) +
    labs(x = "cell width (km)",
         y = "Percentage of cells") +
    theme(axis.title = element_text(size = 14)
    )
# save
  ggsave("AVH/EDA/community coverage AVH.jpeg", plot = last_plot(), scale = 1, dpi = 300, device = "jpeg")
  
# community coverages project 1 (8/12/17)-------------------------------------------------------------
# long data format
  avh_long_comm1 <- select(avh_eda, cell_width, zero, ten, hundred, thousand, five_thousand, ten_thousand)%>%
    gather(record_no, value, -cell_width)
  
# ggplot
  comm_cov2 <- ggplot(data = avh_long_comm1, aes(x = cell_width, y = value, colour = record_no)) +
    geom_line(size = 0.5) +
    geom_point(size = 2) +
    theme_bw() +
    #scale_y_continuous(breaks = seq(0.55, 0.8, 0.1)) +
    #scale_x_continuous(breaks = seq(50, 200, 20)) +
    labs(x = "cell width (km)",
         y = "Percentage of total cells") +
    theme(axis.title = element_text(size = 14)
    )
  comm_cov2
  # save
  ggsave("AVH/EDA/community coverage AVH2.jpeg", plot = last_plot(), scale = 1, dpi = 300, device = "jpeg")
  
# actual:iChao1 ratio ----------------------------------------
  a_ichao1 <- ggplot(data = avh_eda, aes(x = cell_width, y = a/ichao1)) +
    geom_line(size = 0.5) +
    geom_point(size = 2) +
    theme_bw() +
    labs(x = "cell width (km)",
         y = "mean actual:iChao1 ratio") +
    theme(axis.title = element_text(size = 14)
    )
  
  ggsave("AVH/EDA/actual_chao1 ratio AVH.jpeg", plot = last_plot(), scale = 1, dpi = 300, device = "jpeg")
  
# Jost (C) ratio ----------------------------------------
  jost_ratio <- ggplot(data = avh_eda, aes(x = cell_width, y = jost_C)) +
    geom_line(size = 0.5) +
    geom_point(size = 2) +
    theme_bw() +
    #geom_linerange(aes(ymax = ci_pos, ymin = ci_neg), size = 0.3) +
    #scale_y_continuous(limits = c(0,1)) +
    #scale_x_continuous(breaks = seq(50, 200, 20)) +
    labs(x = "cell width (km)",
         y = "Jost ratio") +
    theme(axis.title = element_text(size = 14)
    )
# save  
  ggsave("AVH/EDA/jost C AVH.jpeg", plot = last_plot(), scale = 1, dpi = 300, device = "jpeg")
  
# iChao1 vs. Chao1 ----------------------------------------
  ichao1_chao1 <- ggplot(data = avh_eda, aes(x = chao1, y = ichao1)) +
    geom_line(size = 0.5) +
    geom_point(size = 2) +
    #geom_linerange(aes(ymax = ci_pos, ymin = ci_neg), size = 0.3) +
    #scale_y_continuous(limits = c(20,200)) +
    #scale_x_continuous(breaks = seq(50, 200, 20)) +
    labs(x = "chao1",
         y = "iChao1") +
    theme(axis.title = element_text(size = 14)
    )
  
  ggsave("AVH/EDA/ichao1 v chao1 AVH.jpeg", plot = last_plot(), scale = 1, dpi = 300, device = "jpeg")
  
  
# Chao1_Chiu_SE ----------------------------------------  
  se <- ggplot(data = avh_eda, aes(x = cell_width, y = chiu_se)) +
    geom_line(size = 0.5) +
    geom_point(size = 2) +
    theme_bw() +
    #geom_linerange(aes(ymax = chao1_sd, ymin = chao1sd), size = 0.3) +
    #scale_y_continuous(limits = c(20,200)) +
    #scale_x_continuous(breaks = seq(50, 200, 20)) +
    labs(x = "cell width (km)",
         y = "Chao1 SE") +
    theme(axis.title = element_text(size = 14)
    )
  
  ggsave("AVH/EDA/Chao1 SE AVH.jpeg", plot = last_plot(), scale = 1, dpi = 300, device = "jpeg")
  

  
  
# Survey ?? que ----------------------------------------
  survey_effort <- ggplot(data = avh_eda, aes(x = cell_width, y = a/chao1)) +
    geom_line(size = 0.5) +
    geom_point(size = 2) +
    #geom_linerange(aes(ymax = ci_pos, ymin = ci_neg), size = 0.3) +
    #scale_y_continuous(limits = c(0,1)) +
    #scale_x_continuous(breaks = seq(50, 200, 20)) +
    labs(x = "cell width (km)",
         y = "mean actual:iChao1 ratio") +
    theme(axis.title = element_text(size = 14)
    )
  
  jpeg("AVH/EDA/actual_chao1 ratio AVH.jpeg", width = 16, height = 10, units = 'cm', res = 300)
  a_ichao1
  dev.off()
# ----------------------------
  
  
  
  
  
  
  
  
  
  
  
  
  