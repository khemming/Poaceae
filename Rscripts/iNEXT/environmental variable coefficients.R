#################################################################
# iNEXT model coefficients --------------------------------------
#################################################################

# aim -------------------------------------------------------------------
# produce plots showing how native and non-native species richness relate to environmental variables for:
# (1) C3 
# (2) C4 
# (3) total 


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

# load  workspace from iNEXT model (script) -------------------------------------
  load("Results/iNEXT/Rdata/model_coefficients.RData")
   
# environmental variable labels
# note: to rearrange the order of how the EVs appear in the plot, change at the beginning of iNEXT model  script -- not here  
  ev.levels <- c("pcoldq", "pwarmq", "amt", "ts", "arid", "pewc", "th", "hii")
  ev.labels <- c("Winter \nrainfall", 
                 "Summer \nrainfall", 
                 "Annual mean \ntemperature", 
                 "Temperature \nseasonality", 
                 "Aridity", 
                 "Soil water \navailability", 
                 "Topographic \nheterogeneity", 
                 "Human \nactivity")
  
# reorder levels in spp dfs to match EV labels
  c3.rich$plot.names <- factor(ev.labels, ordered = is.ordered(ev.labels))
  c3.rich$plot.names <- fct_inorder(c3.rich$plot.names)
  
  c4.rich$plot.names <- factor(ev.labels, ordered = is.ordered(ev.labels))
  c4.rich$plot.names <- fct_inorder(c4.rich$plot.names)
  
  tot.rich$plot.names <- factor(ev.labels, ordered = is.ordered(ev.labels))
  tot.rich$plot.names <- fct_inorder(tot.rich$plot.names)
  

# C3 -------------------------------------------------------------------
# C3 adjusted r2 & position: x = 7, y = 5, label = "Adj. r2 = non-native 0.40, native 0.74"
  i <- ggplot(c3.rich, aes(x = plot.names, color = status)) +
    theme_classic() +
    scale_color_manual(labels = c("Non-native", "Native"), values = c("blue", "red")) +
    geom_hline(aes(yintercept = 0),
               color = "black", size = 0.6) +
    labs(title = "", x = "Environmental and anthropogenic variables", y = "Parameter estimate", color = "grey100") +
    geom_point(aes(y = estimate), size = 5, position = position_dodge(width = 0.6)) +
    geom_errorbar(aes(ymin = lower.ci, ymax = upper.ci),
                  size = 1, width = 0, position = position_dodge(width = 0.6)) +
    annotate("segment", x = -Inf, xend = Inf, y = -Inf, yend = -Inf) +
    annotate("segment", x = -Inf, xend = -Inf,y = -Inf, yend = Inf) + 
    annotate("text", x = 7, y = 5, label = "Adj. r2 = non-native 0.40, native 0.74")
  
  #ggThemeAssistGadget(h)
  
  i + theme(axis.text = element_text(size = 12, colour = "gray0"),
            axis.text.x = element_text(size = 12, angle = 45, hjust = 1), 
            axis.text.y = element_text(size = 12), 
            axis.title = element_text(size = 16),
            legend.text = element_text(size = 14), 
            legend.title = element_blank(), 
            axis.ticks.length = unit(0.2, "cm")) 
  
  ggsave("Results/iNEXT/Graphs/observed 15 rec 0.8 cov warn removed/C3 coefficients.jpeg", plot = last_plot(), scale = 1, dpi = 500, device = "jpeg")
  
  
  
# C4 ---------------------------------------------------------------
# c4 position and r2: x = 7, y = 20, label = "Adj. r2 = non-native 0.41, native 0.57"
  
  j <- ggplot(c4.rich, aes(x = plot.names, color = status)) +
    theme_classic() +
    scale_color_manual(labels = c("Non-native", "Native"), values = c("blue", "red")) +
    geom_hline(aes(yintercept = 0),
               color = "black", size = 0.6) +
    labs(title = "", x = "Environmental and anthropogenic variables", y = "Parameter estimate", color = "grey100") +
    geom_point(aes(y = estimate), size = 5, position = position_dodge(width = 0.6)) +
    geom_errorbar(aes(ymin = lower.ci, ymax = upper.ci),
                  size = 1, width = 0, position = position_dodge(width = 0.6)) +
    annotate("segment", x = -Inf, xend = Inf, y = -Inf, yend = -Inf) +
    annotate("segment", x = -Inf, xend = -Inf,y = -Inf, yend = Inf) + 
    annotate("text", x = 7, y = 20, label = "Adj. r2 = non-native 0.41, native 0.57")
  
  #ggThemeAssistGadget(h)
  
  j + theme(axis.text = element_text(size = 12, colour = "gray0"),
            axis.text.x = element_text(size = 12, angle = 45, hjust = 1), 
            axis.text.y = element_text(size = 12), 
            axis.title = element_text(size = 16),
            legend.text = element_text(size = 14), 
            legend.title = element_blank(), 
            axis.ticks.length = unit(0.2, "cm")) 
  
  ggsave("Results/iNEXT/Graphs/observed 15 rec 0.8 cov warn removed/C4 coefficients.jpeg", plot = last_plot(), scale = 1, dpi = 500, device = "jpeg")
  
# total -------------------------------------------------------------------
# tot adjusted r2 & position: x = 1, y = 15, label = "Adj. r2 = non-native 0.46, native 0.50"
  n <- ggplot(tot.rich, aes(x = plot.names, color = status)) +
    theme_classic() +
    scale_color_manual(labels = c("Non-native", "Native"), values = c("blue", "red")) +
    geom_hline(aes(yintercept = 0),
               color = "black", size = 0.6) +
    labs(title = "", x = "Environmental and anthropogenic variables", y = "Parameter estimate", color = "grey100") +
    geom_point(aes(y = estimate), size = 5, position = position_dodge(width = 0.6)) +
    geom_errorbar(aes(ymin = lower.ci, ymax = upper.ci),
                  size = 1, width = 0, position = position_dodge(width = 0.6)) +
    annotate("segment", x = -Inf, xend = Inf, y = -Inf, yend = -Inf) +
    annotate("segment", x = -Inf, xend = -Inf,y = -Inf, yend = Inf) + 
    annotate("text", x = 7, y = 15, label = "Adj. r2 = non-native 0.46, native 0.50")
  
  
  
# testing phase ----------------------------------------------------------------------
# still to do: 
# re-order EVs (either reverse, or biggest to smallest -- left to right)
# change the x and y labels to black text
# move the adj. r2 label over; make the 2->^2
# increase text size (ggThemeAssist)

# C3
  ggplot(c3.rich, aes(y = plot.names, colour = status)) +
         theme_classic() +
         scale_colour_manual(labels = c("Non-native", "Native"), values = c("grey30", "grey60")) +
         geom_vline(aes(xintercept = 0),
                    colour = "black", size = 0.6, linetype = "dashed") +
         labs(tital = "", x = "Parameter estimate", y = "Environmental and anthropogenic variables") +
         geom_point(aes(x = estimate), size = 4) +
         geom_errorbarh(aes(xmin = lower.ci, xmax = upper.ci),
                    size = 1, height = 0.1) +
         annotate("text", y = 7.5, x = 6, label = "Adj. r2 = non-native 0.46,\nnative 0.50")
  
  ggsave("Results/iNEXT/Graphs/observed/C3 coefficients.jpeg", plot = last_plot(), scale = 1, dpi = 500, device = "jpeg")
  
# C4  
  ggplot(c4.rich, aes(y = plot.names, colour = status)) +
    theme_classic() +
    scale_colour_manual(labels = c("Non-native", "Native"), values = c("grey30", "grey60")) +
    geom_vline(aes(xintercept = 0),
               colour = "black", size = 0.6, linetype = "dashed") +
    labs(tital = "", x = "Parameter estimate", y = "Environmental and anthropogenic variables") +
    geom_point(aes(x = estimate), size = 4) +
    geom_errorbarh(aes(xmin = lower.ci, xmax = upper.ci),
                   size = 1, height = 0.1) +
    annotate("text", y = 7.5, x = 15, label = "Adj. r2 = non-native 0.46,\nnative 0.50")
  
  ggsave("Results/iNEXT/Graphs/observed/C4 coefficients.jpeg", plot = last_plot(), scale = 1, dpi = 500, device = "jpeg")
  