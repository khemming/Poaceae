

# library --------------------------------------------------------------
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
  
  rm(list = ls())
  

  
# data ------------------------------------------------------------------------
  nat <- data.frame(pv =       c("Temperature", "Precipitation", "Human\nimpact"),
                    ci_low =   c(-25,     15,      -5),
                    estimate = c(-20,     20,       0),
                    ci_high =  c(-15,     25,       5))
  
  nnat_sim <- data.frame(pv =       c("Temperature", "Precipitation", "Human\nimpact"),
                         ci_low =   c(-24,    16,       -4),
                         estimate = c(-19,    21,        1),
                         ci_high =  c(-14,    26,        6))
  fct_inorder(rev(nat$pv))
  fct_inorder(rev(nnat_sim$pv))
  
  
# native only -----------------------------------------------------------------------
  ggplot(nat, aes(y = pv), width = 0.1) +
    theme_classic() +
    geom_vline(aes(xintercept = 0), colour = "black", size = 0.9, linetype = "dashed") +
    geom_point(data = nat, aes(x = estimate), colour = "blue", 
               size = 7, ) +
    geom_errorbarh(data = nat, aes(xmin = ci_low, xmax = ci_high), 
                   size = 2, height = 0, colour = "blue") +
    geom_rect(xmin = -50, xmax = 31,
              ymin = 2.7, ymax = 3.3, 
              size = 0.5, fill = "red", alpha = 0.05) +
    geom_rect(xmin = -50, xmax = 31,
              ymin = 1.7, ymax = 2.3, 
              size = 0.5, fill = "blue", alpha = 0.05) +
    geom_rect(xmin = -50, xmax = 31,
              ymin = 0.7, ymax = 1.3, 
              size = 0.5, fill = "grey1", alpha = 0.05) +
    labs(x = "",
         y = "",
         title = "") +
    scale_x_continuous(limits = c(-25, 25),
                       breaks = c(-18, 0, 18),
                       labels = c("Negative", 0, "Positive")) +
    theme(axis.title = element_blank(),
          plot.title = element_text(size = 20),
          axis.line = element_line(colour = "black", size = 1),
          axis.text.x = element_text(colour = "black", size = 18), # , vjust = 68
          axis.title.x = element_blank(),
          axis.text.y = element_text(colour = "black", size = 18),
          axis.ticks.length = unit(0.25, "cm"),
          axis.ticks.y = element_line(colour = "black", size = 1),
          axis.ticks.x = element_blank())
  
  ggsave("Results/simulated results/native coefficients.jpeg", plot = last_plot(), width = 12, height = 10, units = "cm", dpi = 500, device = "jpeg")
  
  
# similar relationships -------------------------------------------------
# data
  nat <- data.frame(pv =       c("Temperature", "Precipitation", "Human\nimpact"),
                    ci_low =   c(-25,           15,              -5),
                    estimate = c(-20,           20,               0),
                    ci_high =  c(-15,           25,               5))
  
  nnat_sim <- data.frame(pv =   c("Temperature", "Precipitation", "Human\nimpact"),
                     ci_low =   c(-24,           16,               -4),
                     estimate = c(-19,           21,                1),
                     ci_high =  c(-14,           26,                6))
  fct_inorder(rev(nat$pv))
  fct_inorder(rev(nnat_sim$pv))
  
# plot
  ggplot(nat, aes(y = pv), width = 0.1) +
    theme_classic() +
    geom_vline(aes(xintercept = 0),colour = "black", size = 0.9, linetype = "dashed") +
    geom_point(data = nat, aes(x = estimate), colour = "blue", 
                                              size = 7, position = position_nudge(y = -0.11)) +
               geom_errorbarh(data = nat, aes(xmin = ci_low, xmax = ci_high), 
               size = 2, height = 0, colour = "blue", position = position_nudge(y = -0.11)) +
    geom_point(data = nnat_sim, aes(x = estimate), colour = "red", 
                                               size = 7, position = position_nudge(y = 0.11)) +
               geom_errorbarh(data = nnat_sim, aes(xmin = ci_low, xmax = ci_high), 
               size = 2, height = 0, colour = "red", position = position_nudge(y = 0.11)) +
    geom_rect(xmin = -50, xmax = 31,
              ymin = 2.7, ymax = 3.3, 
              size = 0.5, fill = "red", alpha = 0.05) +
    geom_rect(xmin = -50, xmax = 31,
              ymin = 1.7, ymax = 2.3, 
              size = 0.5, fill = "blue", alpha = 0.05) +
    geom_rect(xmin = -50, xmax = 31,
              ymin = 0.7, ymax = 1.3, 
              size = 0.5, fill = "grey1", alpha = 0.05) +
    labs(x = "Mean estimate",
       y = "") +
    scale_x_continuous(limits = c(-30, 30)) +
    theme(axis.ticks.length = unit(0.25, "cm"),
          axis.ticks.y = element_line(colour = "black", size = 1),
          axis.ticks.x = element_blank(),
          legend.title = element_text(size = 18),
          legend.position = "right", 
          axis.title = element_blank(),
          plot.title = element_text(size = 22, face = "bold"),
          axis.line = element_line(colour = "black", size = 1),
          axis.text.x = element_blank(),
          axis.text.y = element_text(colour = "black", size = 18))
  
    ggsave("Results/simulated results/similar coefficients.jpeg", plot = last_plot(), width = 12, height = 10, units = "cm", dpi = 500, device = "jpeg")
  
# disimilar relationships ------------------------------------------------
# data
  nat <- data.frame(pv =       c("Temperature", "Precipitation", "Human\nimpact"),
                    ci_low =   c(-25,   15,      -5),
                    estimate = c(-20,   20,       0),
                    ci_high =  c(-15,   25,       5))
  nnat_dissim <- data.frame(pv =  c("Temperature", "Precipitation", "Human\nimpact"),
                       ci_low =   c(-5,     -15,       20),
                       estimate = c(-0,     -20,       25),
                       ci_high =  c( 5,     -25,       30))
  fct_inorder(rev(nat$pv))
  fct_inorder(rev(nnat_dissim$pv)) 
  
  ggplot(nat, aes(y = pv), width = 0.1) +
    theme_classic() +
    geom_vline(aes(xintercept = 0),colour = "black", size = 0.9, linetype = "dashed") +
    geom_point(data = nat, aes(x = estimate), colour = "blue", 
                                                   size = 7, position = position_nudge(y = -0.11)) +
    geom_errorbarh(data = nat, aes(xmin = ci_low, xmax = ci_high), 
                   size = 2, height = 0, colour = "blue", position = position_nudge(y = -0.11)) +
    geom_point(data = nnat_dissim, aes(x = estimate), colour = "red", 
                                                   size = 7, position = position_nudge(y = 0.11)) +
    geom_errorbarh(data = nnat_dissim, aes(xmin = ci_low, xmax = ci_high), 
                   size = 2, height = 0, colour = "red", position = position_nudge(y = 0.11)) +
    labs(x = "Mean estimate",
         y = "") +
    geom_rect(xmin = -50, xmax = 31,
              ymin = 2.7, ymax = 3.3, 
              size = 0.5, fill = "red", alpha = 0.05) +
    geom_rect(xmin = -50, xmax = 31,
              ymin = 1.7, ymax = 2.3, 
              size = 0.5, fill = "blue", alpha = 0.05) +
    geom_rect(xmin = -50, xmax = 31,
              ymin = 0.7, ymax = 1.3, 
              size = 0.5, fill = "grey1", alpha = 0.05) +
    scale_x_continuous(limits = c(-30, 30)) +
    theme(axis.ticks.length = unit(0.25, "cm"),
          axis.ticks.y = element_line(colour = "black", size = 1),
          axis.ticks.x = element_blank(),
          legend.title = element_text(size = 18),
          legend.position = "right", 
          axis.title = element_blank(),
          plot.title = element_text(size = 22, face = "bold"),
          axis.line = element_line(colour = "black", size = 1),
          axis.text.x = element_blank(),
          axis.text.y = element_text(colour = "black", size = 18))
  
  
    ggsave("Results/simulated results/disimilar coefficients.jpeg", plot = last_plot(), width = 12, height = 10, units = "cm", dpi = 500, device = "jpeg")
# ---------------------------------------------------------------------