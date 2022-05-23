
# library ----------------------------------------------------------------
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
  r2 <- read.csv("Results/csv/r2.csv")

# wrangle data --------------------------------------------------------------  
# long format with both native and nonnative, ordered correctly
  plot_names <- c("Human\nimpact",
                  "Topographic\nheterogeneity",
                  "Soil water\navailability",
                  "Winter\nrainfall",          
                  "Summer\nrainfall",
                  "Temperature\nseasonality",  
                  "Aridity",
                  "Annual mean\ntemperature")
    
  status <- rep(c("Native", "Exotic"), each = 10)

# function
  model_df <- function(nat_model, nnat_model){
    nat <- nat_model %>% 
           mutate(pv = rownames(nat_model),
                  status = "Native") %>%
           slice(2:9)
    nnat <- nnat_model %>% 
            mutate(pv = rownames(nat_model),
            status = "Exotic") %>%
            slice(2:9)
    long_df <- bind_rows(nat, nnat) %>%
               mutate(estimate = est.,
                      status = as.factor(status),
                      plot_names = factor(rep(plot_names, 2), levels = plot_names)) %>%
        dplyr::select(plot_names, pv, lower, estimate, upper, status)
  return(long_df)
  }
  
# run   
  c3 <- model_df(ci_ls[["native_C3"]], ci_ls[["nonnative_C3"]])
  c4 <- model_df(ci_ls[["native_C4"]], ci_ls[["nonnative_C4"]])
  tot <- model_df(ci_ls[["native_total"]], ci_ls[["nonnative_total"]])

# plot function ----------------------------------------------------------------------
  coef_v1 <- function(title, dat, x_limits, x_labels){ 
  q <-  ggplot(dat, aes(y = plot_names, colour = status)) +
         geom_vline(aes(xintercept = 0),
                       colour = "black", 
                       size = 0.9, 
                       linetype = "dashed") +
        geom_point(aes(x = estimate, shape = status), size = 4, position = position_dodge(width = 0.55)) +
        geom_errorbarh(aes(xmin = lower, xmax = upper),
                       size = 1, height = 0, position = position_dodge(width = 0.55)) +
        scale_x_continuous(limits = x_limits,
                           breaks = x_labels,
                           labels = x_labels) +
        theme_classic() +
        scale_colour_manual(labels = c("Exotic", "Native"),
                            values = c("red", "blue")) + 
        scale_shape_manual(labels = c("Exotic", "Native"),
                           values = c(19, 19)) +
        
        labs(colour = "Status",
             shape = "Status",
             x = "Mean estimate",
             y = "") +
        theme(legend.title = element_text(size = 16),
              legend.text = element_text(size = 16),
              legend.position = "bottom", 
              axis.title = element_text(size = 16),
              plot.title = element_text(size = 22),
              axis.line = element_line(colour = "black", size = 0.9),
              axis.text.x = element_text(colour = "black", size = 14),
              axis.text.y = element_text(colour = "black", size = 14),
              axis.ticks.length = unit(0.25, "cm"),
              axis.ticks = element_line(colour = "black", size = 1),
              plot.caption = element_text(size = 14))
      
    save <- paste0("Results/coefficient plots/", title, ".jpeg")
    ggsave(save, plot = last_plot(), height = 12, width = 15, units = "cm", dpi = 500, device = "jpeg")
    return(q) 
    
  } # coef_v1 end

# plots --------------------------------------------------------------------  
# coef_v1 <- function(title, dat, x_limits, x_labels)
 
# C3
  coef_v1("C3 coefficients", c3, 
          c(-0.23, 0.15), 
          c(round(seq(-0.2, 0.1, 0.1), digits = 2)))
  
# C4
  coef_v1("C4 coefficients", c4, 
          c(-0.23, 0.15), 
          c(round(seq(-0.2, 0.1, 0.1), digits = 2)))
  
# total
  coef_v1("Total richness coefficients", tot,  
          c(-0.23, 0.15), 
          c(round(seq(-0.3, 0.1, 0.1), digits = 2)))
  
# ----------------------------------------------------------------------------  
    
