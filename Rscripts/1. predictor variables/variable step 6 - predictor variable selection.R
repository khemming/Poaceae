
# library ---------------------------------------------------------------------
  library(RColorBrewer)
  library(purrr)
  library(ggthemes)
  library(corrplot)
  library(factoextra)
  library(tidyverse)
  
  rm(list = ls()) 

# data -----------------------------------------------------------------------
  dat <- read.csv("Results/csv/predictor variables 2538.csv", header = T) %>%
         select(-cell_id, -lat, -long, -cell_category, -proportion_cover)
 
# variable selection  
  vs <- dat %>% 
        dplyr::select(pcoldq, pwarmq, amt, ts, arid, pewc, th, hii)
  
# correlation matrix -------------------------------------------------------
  pcor <- round(cor(dat, method = "pearson"), 2)
  write.csv(pcor, "Results/csv/predictor variable correlation matrix.csv")  
  
# reduced EVs on account of variable selection 
  pcor_vs <- round(cor(vs, method = "pearson"), 2)
  write.csv(pcor_vs, "Results/csv/variable selection correlation matrix.csv")  
  

  
# PCA ------------------------------------------------------------------------------
# complete EV data set
  res.pca <- prcomp(dat, scale = F)
  
  fviz_pca_var(res.pca,
               col.var = "contrib", # Color by contributions to the PC
               gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
               repel = TRUE)  + # Avoid text overlapping
               theme_classic() +
               labs(title = "All variables PCA") +
               xlab("Dimension 1 (35.0%)") +
               ylab("Dimension 2 (28.9%)") +
               theme(axis.ticks.length = unit(0.25, "cm"),
                     axis.ticks = element_line(colour = "black", size = 1),
                     axis.title = element_blank(),
                     plot.title = element_text(size = 20),
                     axis.title.x = element_text(size = 12),
                     axis.title.y = element_text(size = 12),
                     axis.line = element_line(colour = "black", size = 1),
                     axis.text.x = element_text(colour = "black", size = 12),
                     axis.text.y = element_text(colour = "black", size = 12))
  
  ggsave("Results/correlation plots/PCA - all vars.jpeg", plot = last_plot(), dpi = 500, width = 14, height = 13, units = "cm", device = "jpeg") 
  

# variable selection data set
  res.pca2 <- prcomp(vs, scale = F)
  
  fviz_pca_var(res.pca2,
               col.var = "contrib", # Color by contributions to the PC
               gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
               repel = TRUE)  + # Avoid text overlapping
    theme_classic() +
    labs(title = "Selected variables PCA") +
    xlab("Dimension 1 (48.1%)") +
    ylab("Dimension 2 (26.6%)") +
    theme(axis.ticks.length = unit(0.25, "cm"),
          axis.ticks = element_line(colour = "black", size = 1),
          axis.title = element_blank(),
          plot.title = element_text(size = 20),
          axis.title.x = element_text(size = 12),
          axis.title.y = element_text(size = 12),
          axis.line = element_line(colour = "black", size = 1),
          axis.text.x = element_text(colour = "black", size = 12),
          axis.text.y = element_text(colour = "black", size = 12))
  
  ggsave("Results/correlation plots/PCA - vs vars.jpeg", plot = last_plot(), dpi = 500, width = 14, height = 13, units = "cm", device = "jpeg") 
  
# ------------------------------------------------------------------