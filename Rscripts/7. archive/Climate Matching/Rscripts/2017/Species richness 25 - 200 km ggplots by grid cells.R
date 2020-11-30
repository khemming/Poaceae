# Poa Species -----------------------------------

# histogram of the corrected data  
  histo <- ggplot(data = scale, aes(x = scale50)) +
    geom_col(size = 0.5) +
    geom_point(size = 2) +
    #scale_y_continuous(breaks = seq(0.55, 0.8, 0.1)) +
    #scale_x_continuous(breaks = seq(50, 200, 20)) +
    labs(x = "cell width (km)",
         y = "number of grid cells") +
    theme(axis.title = element_text(size = 14)
    )

jpeg("C:/Users/s436862/Dropbox/Climate Matching/4. Results/Poa/Ideal scales/number of grid cells.jpeg", width = 16, height = 10, units = 'cm', res = 300)
histo
dev.off()

