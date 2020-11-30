
#################################################################
# coefficients plots 
#################################################################

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

# data -----------------------------------------------------------------------
  load("Data files/rdata/models.RData")
  
  plot_names <- c("Intercept", 
                  "Human\nactivity",
                  "Topographic\nheterogeneity",
                  "Soil water\navailability",
                  "Winter\nrainfall",          
                  "Summer\nrainfall",
                  "Temperature\nseasonality",  
                  "Aridity",
                  "Annual mean\ntemperature",
                  "Proportion\ncover")

# wrangle model data for plotting --------------------------------------------
  model_df <- function(model_i){
  model_i$plot_names <- plot_names
  model_df <- model_i %>% mutate(plot_names = factor(plot_names, levels = plot_names),
                                 pv = rownames(model_i),
                                 estimate = est.) %>%
                      slice(2:9) %>%
                      droplevels() %>%
               dplyr::select(plot_names, pv, lower, estimate, upper)
  return(model_df)
  }
  
# natives  
  n_c3 <- model_df(n_c3_i)
  n_c4 <- model_df(n_c4_i)
  n_tot <- model_df(n_tot_i)
# non-natives  
  nn_c3 <- model_df(nn_c3_i)
  nn_c4 <- model_df(nn_c4_i)
  nn_tot <- model_df(nn_tot_i)
  
# plot function ----------------------------------------------------------------------
  coef_v1 <- function(title, native, nonnative, x_limits, x_labels){
    
    q <- ggplot(native, aes(y = plot_names), width = 0.1) +
          theme_classic() +
          geom_vline(aes(xintercept = 0),colour = "black", size = 0.9, linetype = "dashed") +
          geom_point(data = n_c3, aes(x = estimate), colour = "blue", 
                     size = 6, position = position_nudge(y = -0.15)) +
          geom_errorbarh(data = n_c3, aes(xmin = lower, xmax = upper), 
                         size = 2, height = 0.1, colour = "blue", position = position_nudge(y = -0.15)) +
          geom_point(data = nonnative, aes(x = estimate), colour = "red",  
                     size = 6, position = position_nudge(y = 0.15)) +
          geom_errorbarh(data = nn_c3, aes(xmin = lower, xmax = upper), 
                         size = 2, height = 0.1, colour = "red", position = position_nudge(y = 0.15)) +
          labs(x = "Mean estimate",
               y = "Variable") +
          scale_x_continuous(limits = x_limits,
                             breaks = x_labels,
                             labels = x_labels) +
          scale_color_manual("Legend Title", limits = x_limits, 
                             values = c("red","blue")) +
          guides(colour = guide_legend(override.aes = list(pch = c(16, 21), 
                             fill = c("red", "blue")))) +
          theme(legend.title = element_text(size = 18),
                legend.position = "right", 
                axis.title = element_text(size = 18, face = "bold"),
                plot.title = element_text(size = 22, face = "bold"),
                axis.line = element_line(colour = "black", size = 1.5),
                axis.text.x = element_text(colour = "black", size = 18),
                axis.text.y = element_text(colour = "black", size = 14))
   q
    
    save <- paste0("Results/simulated results/", title, ".jpeg")
    ggsave(save, plot = last_plot(), scale = 1, dpi = 500, device = "jpeg")
    return(q) 
    
  } # coef_v1 end

# plots --------------------------------------------------------------------  
# coef_v1(title,    native, nonnative, 
#         x_limits, x_labels)
 
# C3
  coef_v1("C3 coefficients", n_c3, nn_c3, 
          c(-0.25, 0.25),    c(round(seq(-0.3, 0.3, 0.1), digits = 2)))
  
# C4
  coef_v1("C4 coefficients", n_c4, nn_c4, 
    c(-0.4, 0.4),    c(round(seq(-0.3, 0.3, 0.1), digits = 2)))
  
# total
  coef_v1("Total richness coefficients", n_tot, nn_tot, 
    c(-0.25, 0.25),    c(round(seq(-0.3, 0.3, 0.1), digits = 2)))
  
# ----------------------------------------------------------------------------  
    
