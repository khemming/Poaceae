
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

# data -----------------------------------------------------------------
  load("Data files/rdata/model_data.RData")
  
# transform model data for plotting ------------------------------------
# data frame
  m_df_proto <- data.frame(variable = rep(c("hii",    "th", "pewc", "pcoldq", 
                                            "pwarmq", "ts", "arid", "amt"), 2))
  m_df <- m_df_proto %>% mutate(status = rep(c("native", "non_native"), each = 8),
                                estimate = NA,
                                lower_ci = NA,
                                upper_ci = NA)
# order predictor variables for plotting
  m_df$plot_names <- as.factor(rep(c("Human\nactivity",
                                     "Topographic\nheterogeneity",
                                     "Soil water\navailability",
                                     "Winter\nrainfall",          
                                     "Summer\nrainfall",
                                     "Temperature\nseasonality",  
                                     "Aridity",
                                     "Annual mean\ntemperature"), 2))
  m_df$plot_names <- fct_inorder(m_df$plot_names)
  levels(m_df$plot_names)
  
  
# function 
  coef_matrix <- function(df, nat_model, nonnat_model) {
    
  # assign coefficients 
    df[df$status == "native", "estimate"] <- nat_model$coefficients[2:9]
    df[df$status == "non_native", "estimate"] <- nonnat_model$coefficients[2:9]
  
  # assign confidence intervals 
    df[df$status == "native", "lower_ci"] <- confint(nat_model)[2:9, 1]
    df[df$status == "non_native", "lower_ci"] <- confint(nonnat_model)[2:9, 1]
    
    df[df$status == "native", "upper_ci"] <- confint(nat_model)[2:9, 2]
    df[df$status == "non_native", "upper_ci"] <- confint(nonnat_model)[2:9, 2]
    
    return(df)
  }

# run function 
# total
  df_tot <- coef_matrix(m_df, m_nt, m_nnt)
# total
  df_c3 <- coef_matrix(m_df, m_nc3, m_nnc3)
# total
  df_c4 <- coef_matrix(m_df, m_nc4, m_nnc4)
  
# # reorder predictor variable factor levels
#   pv_ordered <- c("hii",    "th", "pewc", "pcoldq", 
#                   "pwarmq", "ts", "arid", "amt")
#   
#   df_tot$variable <- factor(pv_ordered, ordered = is.ordered(pv_ordered))
#   df_tot$variable <- fct_inorder(df_tot$variable)
#   levels(df_tot$variable)
#   
#   df_c3$variable <- factor(pv_ordered, ordered = is.ordered(pv_ordered))
#   df_c3$variable <- fct_inorder(df_c3$variable)
#   levels(df_c3$variable)
#   
#   df_c4$variable <- factor(pv_ordered, ordered = is.ordered(pv_ordered))
#   df_c4$variable <- fct_inorder(df_c4$variable)
#   levels(df_c4$variable)

# plots ----------------------------------------------------------------------
# tot -------------------------------------------------------------------------
# r2 terms
  r2_n <- summary(m_nt)$adj.r.squared
  r2_n_text <- paste0("Native\n(adj. r2 = ", sprintf("%.2f", round(r2_n, digits = 2)), ")")
  r2_nn <- summary(m_nnt)$adj.r.squared
  r2_nn_text <- paste0("Non-native\n(adj. r2 = ", sprintf("%.2f", round(r2_nn, digits = 2)), ")")
  
# plot
  ggplot(df_tot, aes(y = plot_names, colour = status)) +
        theme_classic() +
        scale_colour_manual(labels = c(r2_n_text, r2_nn_text), 
                            values = c("blue", "red")) +
        geom_vline(aes(xintercept = 0),
                   colour = "black", 
                   size = 0.6, 
                   linetype = "dashed") +
        labs(x = "Mean estimate",
             y = "") +
        geom_point(aes(x = estimate), size = 4) +
        geom_errorbarh(aes(xmin = lower_ci, xmax = upper_ci),
                       size = 1, height = 0.1) +
        scale_x_continuous(limits = c(-0.2, 0.2),
                           breaks = c(seq(-0.2, 0.2, 0.1))) +
        theme(legend.title = element_text(size = 14),
              legend.position = "bottom", 
              axis.title = element_text(size = 14),
              plot.title = element_text(size = 22),
              axis.text.x = element_text(colour = "black", size = 12),
              axis.text.y = element_text(colour = "black", size = 12))
  
  ggsave("Results/coefficient plots/Total.jpeg", plot = last_plot(), scale = 1, dpi = 500, device = "jpeg")
  
# C3 -------------------------------------------------------------------------
# r2 terms
  r2_n <- summary(m_nc3)$adj.r.squared
  r2_n_text <- paste0("Native\n(adj. r2 = ", sprintf("%.2f", round(r2_n, digits = 2)), ")")
  r2_nn <- summary(m_nnc3)$adj.r.squared
  r2_nn_text <- paste0("Non-native\n(adj. r2 = ", sprintf("%.2f", round(r2_nn, digits = 2)), ")")
  
# plot
  ggplot(df_c3, aes(y = plot_names, colour = status)) +
    theme_classic() +
    scale_colour_manual(labels = c(r2_n_text, r2_nn_text), 
                        values = c("blue", "red")) +
    geom_vline(aes(xintercept = 0),
               colour = "black", 
               size = 0.6, 
               linetype = "dashed") +
    labs(x = "Mean estimate",
         y = "") +
    geom_point(aes(x = estimate), size = 4) +
    geom_errorbarh(aes(xmin = lower_ci, xmax = upper_ci),
                   size = 1, height = 0.1) +
    scale_x_continuous(limits = c(-0.2, 0.2),
                       breaks = c(seq(-0.2, 0.2, 0.1))) +
    theme(legend.title = element_text(size = 14),
          legend.position = "bottom", 
          axis.title = element_text(size = 14),
          plot.title = element_text(size = 22),
          axis.text.x = element_text(colour = "black", size = 12),
          axis.text.y = element_text(colour = "black", size = 12))
  
  ggsave("Results/coefficient plots/C3.jpeg", plot = last_plot(), scale = 1, dpi = 500, device = "jpeg")
  
  
# C4 -------------------------------------------------------------------------
# r2 terms
  r2_n <- summary(m_nc4)$adj.r.squared
  r2_n_text <- paste0("Native\n(adj. r2 = ", sprintf("%.2f", round(r2_n, digits = 2)), ")")
  r2_nn <- summary(m_nnc4)$adj.r.squared
  r2_nn_text <- paste0("Non-native\n(adj. r2 = ", sprintf("%.2f", round(r2_nn, digits = 2)), ")")
  
# plot
  ggplot(df_c4, aes(y = plot_names, colour = status)) +
    theme_classic() +
    scale_colour_manual(labels = c(r2_n_text, r2_nn_text), 
                        values = c("blue", "red")) +
    geom_vline(aes(xintercept = 0),
               colour = "black", 
               size = 0.6, 
               linetype = "dashed") +
    labs(x = "Mean estimate",
         y = "") +
    geom_point(aes(x = estimate), size = 4) +
    geom_errorbarh(aes(xmin = lower_ci, xmax = upper_ci),
                   size = 1, height = 0.1) +
    scale_x_continuous(limits = c(-0.27, 0.25),
                       breaks = c(seq(-0.2, 0.3, 0.1))) +
    theme(legend.title = element_text(size = 14),
          legend.position = "bottom", 
          axis.title = element_text(size = 14),
          plot.title = element_text(size = 22),
          axis.text.x = element_text(colour = "black", size = 12),
          axis.text.y = element_text(colour = "black", size = 12))
  
    ggsave("Results/coefficient plots/C4.jpeg", plot = last_plot(), scale = 1, dpi = 500, device = "jpeg")
  