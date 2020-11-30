# LARGE SCALE

# Divided into two almost identical sections: Poa species and All Native Species of AVH data

# The scales of the section are termed 'Large', and are between 1 and 1000 km (that is, the cell width is that -- square that for cell area)
# (The sister script is also identical, 'All scales,' and is between 50 - 200 km)

# Below is calculated all the scale related things I can think of; and it's pretty easy to add more.
# (though add to Poa Ideal, as that is the reference script and function)



# Poa Species ----------------------------------------------------

  library(ggplot2)
  library(ggmap)
  library(tidyr)
  library(raster)
  library(rgdal)
  library(maptools)
  library(dplyr)
  
  rm(list= ls())
  
  setwd("C:/Users/s436862/Dropbox/Climate Matching/1. Data files/")
  
### data  
# raster template
  b <- raster("EFs/EFs cropped/arid.grd")
  
# Poa
  poa <- read.csv("AVH/AVH grass records.csv", header = T) %>%
    dplyr::select(species, lat, long)
  
  p <- poa
  xy <- cbind(p$long, p$lat)
  spp <- as.numeric(factor(p$species))

  #width <- 64
  
### loop function
  ras <- function(width){
    
    raster <- aggregate(b, fact = width, fun = mean)
    
    # actual richness (a)
    a <- rasterize(xy, raster, field = spp, fun = function(x,...) {length(unique(na.omit(x))) })
    # number of records (n)
    n <- rasterize(xy, raster, fun = function(x,...) {length(na.omit(x)) })
    # chao (c)
    sing <- rasterize(xy, raster, field = spp, fun = function(x,...) {length(which(table(x)==1)) })
    doub <- rasterize(xy, raster, field = spp, fun = function(x,...) {length(which(table(x)==2)) })
    
    # reference for extraterrestrial NA removal
    b1 <- getValues(raster)
    
    # actual richness
    a1 <- getValues(a)
    
    
    # chao correctives
    c_sing <- getValues(sing)
    c_doub <- getValues(doub)
    n1 <- getValues(n)
    
    # dataframe
    df_na <- data.frame(a1, b1, c_sing, c_doub, n1)
    df <- df_na[!is.na(b1), ] 
    
    
    # sample size correction
    df$c1 <- ifelse(is.na(df$c_doub) == FALSE,
                    (ifelse(df$c_doub == 0, 
                            df$a1 + ((df$c_sing * (df$c_sing - 1)) / (2 * (df$c_doub + 1)) * ((df$n1 - 1) / df$n1)),   
                            df$a1 + (((df$c_sing ^ 2) / (2 * df$c_doub)) * ((df$n1 - 1) / df$n1)))),
                    NA)
    
    ### Anything and everything I wish to calculate
    # variance (v)
    df$v1 <-  df$c_doub * ((0.5 * ((df$c_sing / df$c_doub) ^ 2)) + ((df$c_sing/df$c_doub)^3) + (0.25 * ((df$c_sing/df$c_doub) ^ 4) ))
    
    # cv
    df$cv1 <- sqrt(df$v1) / df$c1 * 100
    
    # sd
    df$sd1 <- sqrt(df$v1)
    
    # CI: N +/-1.96 * sd (Chao, 1987)
    df$ci1 <- df$c1 + (1.96 * df$sd1)
    df$ci2 <- df$c1 - (1.96 * df$sd1)
    
    # percentage cover (i.e. % of grid cells with >0 records)  
    df$total <- length(df$a1)
    df$sumna <- sum(is.na(df$a1))
    
    df$coverage <- (df$total - df$sumna) / df$total
    
    
    # remove lingering NAs & bind
    a_mean <- mean(df$a1, na.rm = TRUE) 
    c_mean <- mean(df$c1, na.rm = TRUE) 
    var <- mean(df$v1, na.rm = TRUE) 
    cv <- mean(df$cv1, na.rm = TRUE)
    sd <- mean(df$sd1, na.rm = TRUE)
    ci_pos <- mean(df$ci1, na.rm = TRUE)
    ci_neg <- mean(df$ci2, na.rm = TRUE)
    ratio <- a_mean / c_mean
    cover <- mean(df$coverage, na.rm = TRUE)
    cell_no <- length(b1)
    
    res <- as.data.frame(cbind(a_mean, c_mean, var, cv, sd, ci_pos, ci_neg, ratio, cover, cell_no))
    
    return(res)
    
  }  #finish function


  width <- 2 ^ (seq(1, 10, 0.5))
  out <- matrix(nrow = length(width), ncol = 10)
  
  out <- matrix(nrow = length(width), ncol = 11)
  
  for(i in 1:length(width)) {
    out[i, 1] <- width[i]
    out[i, 2:11] <- as.numeric(ras(width[i]))
  } 
  
  lrg_poa <- out
  
  colnames(lrg_poa) <- c("cell width (km)", "actual mean", "corrected mean", "variance", "coefficient of variation", "standard deviation", "confidence interval positive", "confidence interval negative", "ratio", "coverage", "number of grid cells")
  lrg_poa <- as.data.frame(lrg_poa)
  
  # run all that once, and then I won't have to again if I do other scripts with 'out' in them
 
  # csv
  write.csv(lrg_poa, "C:/Users/s436862/Dropbox/Climate Matching/4. Results/Poa/All scales/chao estimates.csv")
  
  
  ### for GGplot    
  l_p_gg <- lrg_poa
  colnames(l_p_gg) <- c("width", "a_mean", "c_mean", "var", "cv", "sd", "ci_pos", "ci_neg", "ratio", "cover", "cell_no")
  out_gg <- as.data.frame(l_p_gg)
  
  # number of grid cells (Australia is divided into)
  # log cos big grids are nasty
  cell_no <- ggplot(data = l_p_gg, aes(x = width, y = log(cell_no))) +
    geom_line(size = 0.5) +
    geom_point(size = 2) +
    #scale_y_continuous(breaks = seq(0.55, 0.8, 0.1)) +
    #scale_x_continuous(breaks = seq(50, 200, 20)) +
    labs(x = "cell width (km)",
         y = "log transformed number of grid cells") +
    theme(axis.title = element_text(size = 14)
    )
  
  jpeg("C:/Users/s436862/Dropbox/Climate Matching/4. Results/Poa/All scales/number of grid cells.jpeg", width = 16, height = 10, units = 'cm', res = 300)
  cell_no
  dev.off()
  
  
  # coverage -- % of cells with >0 records
  cover <- ggplot(data = l_p_gg, aes(x = width, y = cover)) +
    geom_line(size = 0.5) +
    geom_point(size = 2) +
    #scale_y_continuous(breaks = seq(0.55, 0.8, 0.1)) +
    #scale_x_continuous(breaks = seq(50, 200, 20)) +
    labs(x = "cell width (km)",
         y = "cell coverage") +
    theme(axis.title = element_text(size = 14)
    )
  
  jpeg("C:/Users/s436862/Dropbox/Climate Matching/4. Results/Poa/All scales/cell coverage.jpeg", width = 16, height = 10, units = 'cm', res = 300)
  cover
  dev.off()
  
  # actual:corrected SR ratio
  ratio <- ggplot(data = l_p_gg, aes(x = width, y = ratio)) +
    geom_line(size = 0.5) +
    geom_point(size = 2) +
    #scale_y_continuous(limits = c(0.5, 0.8)) +
    #scale_x_continuous(breaks = seq(50, 200, 20)) +
    labs(x = "cell width (km)",
         y = "actual:Chao1 species richness ratio") +
    theme(axis.title = element_text(size = 14)
    )
  
  jpeg("C:/Users/s436862/Dropbox/Climate Matching/4. Results/Poa/All scales/corrected vs actual ratio.jpeg", width = 16, height = 10, units = 'cm', res = 300)
  ratio
  dev.off()
  
  
  # corrected mean, with error bars = calculated CIs  
  mean <- ggplot(data = l_p_gg, aes(x = width, y = c_mean)) +
    geom_line(size = 0.5) +
    geom_point(size = 2) +
    geom_linerange(aes(ymax = ci_pos, ymin = ci_neg), size = 0.3) +
    #scale_y_continuous(limits = c(20,200)) +
    #scale_x_continuous(breaks = seq(50, 200, 20)) +
    labs(x = "cell width (km)",
         y = "Chao1 corrected species richness") +
    theme(axis.title = element_text(size = 14)
    )
  
  jpeg("C:/Users/s436862/Dropbox/Climate Matching/4. Results/Poa/All scales/corrected speces richness.jpeg", width = 16, height = 10, units = 'cm', res = 300)
  mean
  dev.off()
  
  
  # variance
  var <- ggplot(data = l_p_gg, aes(x = width, y = var)) +
    geom_line(size = 0.5) +
    geom_point(size = 2) +
    #scale_y_continuous(limits = c(20,200)) +
    #scale_x_continuous(breaks = seq(50, 200, 20)) +
    labs(x = "cell width (km)",
         y = "Chao1 variance") +
    theme(axis.title = element_text(size = 14)
    )
  
  jpeg("C:/Users/s436862/Dropbox/Climate Matching/4. Results/Poa/All scales/chao1 variance.jpeg", width = 16, height = 10, units = 'cm', res = 300)
  var
  dev.off()
  
  
  # coefficient of variation
  cv <- ggplot(data = l_p_gg, aes(x = width, y = cv)) +
    geom_line(size = 0.5) +
    geom_point(size = 2) +
    #scale_y_continuous(limits = c(20,200)) +
    #scale_x_continuous(breaks = seq(50, 200, 20)) +
    labs(x = "cell width (km)",
         y = "Chao1 coefficient of variation") +
    theme(axis.title = element_text(size = 14)
    )
  
  jpeg("C:/Users/s436862/Dropbox/Climate Matching/4. Results/Poa/All scales/chao1 coefficient of variation.jpeg", width = 16, height = 10, units = 'cm', res = 300)
  cv
  dev.off()
  
  # standard deviation
  sd <- ggplot(data = l_p_gg, aes(x = width, y = sd)) +
    geom_line(size = 0.5) +
    geom_point(size = 2) +
    #scale_y_continuous(limits = c(20,200)) +
    #scale_x_continuous(breaks = seq(50, 200, 20)) +
    labs(x = "cell width (km)",
         y = "Chao1 standard deviation") +
    theme(axis.title = element_text(size = 14)
    )
  
  jpeg("C:/Users/s436862/Dropbox/Climate Matching/4. Results/Poa/All scales/chao1 standard deviation.jpeg", width = 16, height = 10, units = 'cm', res = 300)
  sd
  dev.off()
  
  # plotting cv vs. ratio
  cv_ratio <- ggplot(data = l_p_gg, aes(x = cv, y = ratio)) +
    geom_line(size = 0.5) +
    geom_point(size = 2) +
    #scale_y_continuous(limits = c(20,200)) +
    #scale_x_continuous(breaks = seq(50, 200, 20)) +
    labs(x = "Chao1 coefficient of variation",
         y = "actual:Chao1 ratio") +
    theme(axis.title = element_text(size = 14)
    )
  
  jpeg("C:/Users/s436862/Dropbox/Climate Matching/4. Results/Poa/All scales/Chao cv against cor.act ratio.jpeg", width = 16, height = 10, units = 'cm', res = 300)
  cv_ratio
  dev.off()

  
# All Species ----------------------------------------------------
  
  
  library(ggplot2)
  library(ggmap)
  library(tidyr)
  library(raster)
  library(rgdal)
  library(maptools)
  library(dplyr)
  
  rm(list = ls())
  
  setwd("C:/Users/s436862/Dropbox/Climate Matching/1. Data files/")
  
### data  
# raster template
  b <- raster("EFs/EFs cropped/arid.grd")
  
# All native species
  avh <- read.csv("AVH/AVH native records.csv", header = T) %>%
    dplyr::select(species, lat, long)
  
  xy <- cbind(avh$long, avh$lat)
  spp <- as.numeric(factor(avh$species))
  
  #width <- 64
  
### loop function
  ras <- function(width){
    
    raster <- aggregate(b, fact = width, fun = mean)
    
    # actual richness (a)
    a <- rasterize(xy, raster, field = spp, fun = function(x,...) {length(unique(na.omit(x))) })
    # number of records (n)
    n <- rasterize(xy, raster, fun = function(x,...) {length(na.omit(x)) })
    # chao (c)
    sing <- rasterize(xy, raster, field = spp, fun = function(x,...) {length(which(table(x)==1)) })
    doub <- rasterize(xy, raster, field = spp, fun = function(x,...) {length(which(table(x)==2)) })
    
    # reference for extraterrestrial NA removal
    b1 <- getValues(raster)
    
    # actual richness
    a1 <- getValues(a)
    
    
    # chao correctives
    c_sing <- getValues(sing)
    c_doub <- getValues(doub)
    n1 <- getValues(n)
    
    # dataframe
    df_na <- data.frame(a1, b1, c_sing, c_doub, n1)
    df <- df_na[!is.na(b1), ] 
    
    
    # sample size correction
    df$c1 <- ifelse(is.na(df$c_doub) == FALSE,
                    (ifelse(df$c_doub == 0, 
                            df$a1 + ((df$c_sing * (df$c_sing - 1)) / (2 * (df$c_doub + 1)) * ((df$n1 - 1) / df$n1)),   
                            df$a1 + (((df$c_sing ^ 2) / (2 * df$c_doub)) * ((df$n1 - 1) / df$n1)))),
                    NA)
    
    ### Anything and everything I wish to calculate
    # variance (v)
    df$v1 <-  df$c_doub * ((0.5 * ((df$c_sing / df$c_doub) ^ 2)) + ((df$c_sing/df$c_doub)^3) + (0.25 * ((df$c_sing/df$c_doub) ^ 4) ))
    
    # cv
    df$cv1 <- sqrt(df$v1) / df$c1 * 100
    
    # sd
    df$sd1 <- sqrt(df$v1)
    
    # CI: N +/-1.96 * sd (Chao, 1987)
    df$ci1 <- df$c1 + (1.96 * df$sd1)
    df$ci2 <- df$c1 - (1.96 * df$sd1)
    
    # percentage cover (i.e. % of grid cells with >0 records)  
    df$total <- length(df$a1)
    df$sumna <- sum(is.na(df$a1))
    
    df$coverage <- (df$total - df$sumna) / df$total
    
    
    # remove lingering NAs & bind
    a_mean <- mean(df$a1, na.rm = TRUE) 
    c_mean <- mean(df$c1, na.rm = TRUE) 
    var <- mean(df$v1, na.rm = TRUE) 
    cv <- mean(df$cv1, na.rm = TRUE)
    sd <- mean(df$sd1, na.rm = TRUE)
    ci_pos <- mean(df$ci1, na.rm = TRUE)
    ci_neg <- mean(df$ci2, na.rm = TRUE)
    ratio <- a_mean / c_mean
    cover <- mean(df$coverage, na.rm = TRUE)
    cell_no <- length(b1)
    
    res <- as.data.frame(cbind(a_mean, c_mean, var, cv, sd, ci_pos, ci_neg, ratio, cover, cell_no))
    
    return(res)
    
  }  #finish function
  
####
  
  width <- 2 ^ (seq(1, 10, 0.5))
  out <- matrix(nrow = length(width), ncol = 11)
  
  for(i in 1:length(width)) {
    out[i, 1] <- width[i]
    out[i, 2:11] <- as.numeric(ras(width[i]))
  } 
  
  lrg_all <- out
  
  colnames(lrg_all) <- c("cell width (km)", "actual mean", "corrected mean", "variance", "coefficient of variation", "standard deviation", "confidence interval positive", "confidence interval negative", "ratio", "coverage", "number of grid cells")
  lrg_all <- as.data.frame(lrg_all)
  
  # run all that once, and then I won't have to again if I do other scripts with 'out' in them
  
  
  # csv
  write.csv(lrg_all, "C:/Users/s436862/Dropbox/Climate Matching/4. Results/Poa/All scales/chao estimates.csv")
  
  
### for GGplot    
  l_a_gg <- lrg_all
  colnames(l_a_gg) <- c("width", "a_mean", "c_mean", "var", "cv", "sd", "ci_pos", "ci_neg", "ratio", "cover", "cell_no")
  out_gg <- as.data.frame(l_a_gg)
  
  # number of grid cells (Australia is divided into)
  # log cos big grids are nasty
  cell_no <- ggplot(data = l_a_gg, aes(x = width, y = log(cell_no))) +
    geom_line(size = 0.5) +
    geom_point(size = 2) +
    #scale_y_continuous(breaks = seq(0.55, 0.8, 0.1)) +
    #scale_x_continuous(breaks = seq(50, 200, 20)) +
    labs(x = "cell width (km)",
         y = "log transformed number of grid cells") +
    theme(axis.title = element_text(size = 14)
    )
  
  jpeg("C:/Users/s436862/Dropbox/Climate Matching/4. Results/All AVH/All scales/number of grid cells.jpeg", width = 16, height = 10, units = 'cm', res = 300)
  cell_no
  dev.off()
  
  
  # coverage -- % of cells with >0 records
  cover <- ggplot(data = l_a_gg, aes(x = width, y = cover)) +
    geom_line(size = 0.5) +
    geom_point(size = 2) +
    #scale_y_continuous(breaks = seq(0.55, 0.8, 0.1)) +
    #scale_x_continuous(breaks = seq(50, 200, 20)) +
    labs(x = "cell width (km)",
         y = "cell coverage") +
    theme(axis.title = element_text(size = 14)
    )
  
  jpeg("C:/Users/s436862/Dropbox/Climate Matching/4. Results/All AVH/All scales/cell coverage.jpeg", width = 16, height = 10, units = 'cm', res = 300)
  cover
  dev.off()
  
  # actual:corrected SR ratio
  ratio <- ggplot(data = l_a_gg, aes(x = width, y = ratio)) +
    geom_line(size = 0.5) +
    geom_point(size = 2) +
    #scale_y_continuous(limits = c(0.5, 0.8)) +
    #scale_x_continuous(breaks = seq(50, 200, 20)) +
    labs(x = "cell width (km)",
         y = "actual:Chao1 species richness ratio") +
    theme(axis.title = element_text(size = 14)
    )
  
  jpeg("C:/Users/s436862/Dropbox/Climate Matching/4. Results/All AVH/All scales/corrected vs actual ratio.jpeg", width = 16, height = 10, units = 'cm', res = 300)
  ratio
  dev.off()
  
  
  # corrected mean, with error bars = calculated CIs  
  mean <- ggplot(data = l_a_gg, aes(x = width, y = c_mean)) +
    geom_line(size = 0.5) +
    geom_point(size = 2) +
    geom_linerange(aes(ymax = ci_pos, ymin = ci_neg), size = 0.3) +
    #scale_y_continuous(limits = c(20,200)) +
    #scale_x_continuous(breaks = seq(50, 200, 20)) +
    labs(x = "cell width (km)",
         y = "Chao1 corrected species richness") +
    theme(axis.title = element_text(size = 14)
    )
  
  jpeg("C:/Users/s436862/Dropbox/Climate Matching/4. Results/All AVH/All scales/corrected speces richness.jpeg", width = 16, height = 10, units = 'cm', res = 300)
  mean
  dev.off()
  
  
  # variance
  var <- ggplot(data = l_a_gg, aes(x = width, y = var)) +
    geom_line(size = 0.5) +
    geom_point(size = 2) +
    #scale_y_continuous(limits = c(20,200)) +
    #scale_x_continuous(breaks = seq(50, 200, 20)) +
    labs(x = "cell width (km)",
         y = "Chao1 variance") +
    theme(axis.title = element_text(size = 14)
    )
  
  jpeg("C:/Users/s436862/Dropbox/Climate Matching/4. Results/All AVH/All scales/chao1 variance.jpeg", width = 16, height = 10, units = 'cm', res = 300)
  var
  dev.off()
  
  
  # coefficient of variation
  cv <- ggplot(data = l_a_gg, aes(x = width, y = cv)) +
    geom_line(size = 0.5) +
    geom_point(size = 2) +
    #scale_y_continuous(limits = c(20,200)) +
    #scale_x_continuous(breaks = seq(50, 200, 20)) +
    labs(x = "cell width (km)",
         y = "Chao1 coefficient of variation") +
    theme(axis.title = element_text(size = 14)
    )
  
  jpeg("C:/Users/s436862/Dropbox/Climate Matching/4. Results/All AVH/All scales/chao1 coefficient of variation.jpeg", width = 16, height = 10, units = 'cm', res = 300)
  cv
  dev.off()
  
  # standard deviation
  sd <- ggplot(data = l_a_gg, aes(x = width, y = sd)) +
    geom_line(size = 0.5) +
    geom_point(size = 2) +
    #scale_y_continuous(limits = c(20,200)) +
    #scale_x_continuous(breaks = seq(50, 200, 20)) +
    labs(x = "cell width (km)",
         y = "Chao1 standard deviation") +
    theme(axis.title = element_text(size = 14)
    )
  
  jpeg("C:/Users/s436862/Dropbox/Climate Matching/4. Results/All AVH/All scales/chao1 standard deviation.jpeg", width = 16, height = 10, units = 'cm', res = 300)
  sd
  dev.off()
  
  # plotting cv vs. ratio
  cv_ratio <- ggplot(data = l_a_gg, aes(x = cv, y = ratio)) +
    geom_line(size = 0.5) +
    geom_point(size = 2) +
    #scale_y_continuous(limits = c(20,200)) +
    #scale_x_continuous(breaks = seq(50, 200, 20)) +
    labs(x = "Chao1 coefficient of variation",
         y = "actual:Chao1 ratio") +
    theme(axis.title = element_text(size = 14)
    )
  
  jpeg("C:/Users/s436862/Dropbox/Climate Matching/4. Results/All AVH/All scales/Chao cv against cor.act ratio.jpeg", width = 16, height = 10, units = 'cm', res = 300)
  cv_ratio
  dev.off()
 



  
# Poa, coverage (% of grid cells with records in them) --------------------

  rm(list = ls())

  library(ggplot2)
  library(ggmap)
  library(tidyr)
  library(raster)
  library(rgdal)
  library(maptools)
  library(dplyr)
  
  setwd("C:/Users/s436862/Dropbox/Climate Matching/1. Data files/")
  
### data  
# raster template
  b <- raster("EFs/EFs cropped/arid.grd")
  
# Poa
  poa <- read.csv("AVH/AVH grass records.csv", header = T) %>%
    dplyr::select(species, lat, long)
  
  p <- poa
  xy <- cbind(p$long, p$lat)
  spp <- as.numeric(factor(p$species))
  
  #width <- 64
  
### loop function
  ras <- function(width){
    
    raster <- aggregate(b, fact = width, fun = mean)
    
  # actual richness (a)
    a <- rasterize(xy, raster, field = spp, fun = function(x,...) {length(unique(na.omit(x))) })
    
  # reference for extraterrestrial NA removal
    b1 <- getValues(raster)
    
  # actual richness
    a1 <- getValues(a)
    
    
  # dataframe
    df_na <- data.frame(a1, b1)
    df <- df_na[!is.na(b1), ] 
    
    
  # percentage of squares with records
    # we want the num of rows which != NA, and divide them by all rows
    table(df$a1) 
    
    df$total <- length(df$a1)
    df$sumna <- sum(is.na(df$a1))
    
    df$coverage <- (df$total - df$sumna) / df$total
    
    coverage <- mean(df$coverage)
    
    return(coverage)
  }  #finish function
  
####
  
  width <- 2 ^ (seq(1, 10, 0.5))
  out <- matrix(nrow = length(width), ncol = 1)
  
  for(i in 1:length(width)) {
    out[i, 1] <- width[i]
    out[i, 1] <- as.numeric(ras(width[i]))
  }
  
  plot(out ~ width, xlab = "cell width (km)", ylab = "coverage (%)")
  
  jpeg(filename = "C:/Users/s436862/Dropbox/Climate Matching/4. Results/Poa/percent coverage.jpeg", height = 15, width = 15, units = "cm", res = 300)
  plot(out ~ width, xlab = "cell width (km)", ylab = "coverage (%)")
  dev.off()
  
  write.csv(out, "C:/Users/s436862/Dropbox/Climate Matching/4. Results/Poa/percentcoverage.csv")
  


# AVH, percent coverage ---------------------------------------------------


  library(ggplot2)
  library(ggmap)
  library(tidyr)
  library(raster)
  library(rgdal)
  library(maptools)
  library(dplyr)
  
  
  setwd("C:/Users/s436862/Dropbox/Climate Matching/1. Data files/")
  
### data  
# raster template
  b <- raster("EFs/EFs cropped/arid.grd")
  
# All AVH native species
  avh <- read.csv("AVH/AVH native records.csv", header = T) %>%
    dplyr::select(species, lat, long)
  
  xy <- cbind(avh$long, avh$lat)
  spp <- as.numeric(factor(avh$species))
  
  #width <- 2
  
### loop function
  ras <- function(width){
    
    raster <- aggregate(b, fact = width, fun = mean)
    
  # actual richness (a)
    a <- rasterize(xy, raster, field = spp, fun = function(x,...) {length(unique(na.omit(x))) })
    
  # reference for extraterrestrial NA removal
    b1 <- getValues(raster)
    
  # actual richness
    a1 <- getValues(a)
    
    
  # dataframe
    df_na <- data.frame(a1, b1)
    df <- df_na[!is.na(b1), ] 
    
    
  # percentage of squares with records
  # we want the num of rows which != NA, and divide them by all rows
    table(df$a1) 
    
    df$total <- length(df$a1)
    df$na <- sum(is.na(df$a1))
    
    coverage <- (total - na) / total
    
    
    
    return(coverage)
  }  #finish function
  
####
  
  width <- 2 ^ (seq(1, 10, 0.5))
  out <- matrix(nrow = length(width), ncol = 1)
  
  for(i in 1:length(width)) {
    out[i, 1] <- width[i]
    out[i, 1] <- as.numeric(ras(width[i]))
  }
  
  plot(out ~ width, xlab = "cell width (km)", ylab = "coverage (%)")
  
  jpeg(filename = "C:/Users/s436862/Dropbox/Climate Matching/4. Results/All AVH/percent coverage.jpeg", height = 15, width = 15, units = "cm", res = 300)
  plot(out ~ width, xlab = "cell width (km)", ylab = "coverage (%)")
  dev.off()
  
  write.csv(out, "C:/Users/s436862/Dropbox/Climate Matching/4. Results/All AVH/percent coverage.csv")
  

# Poa, Chao:act vs. sampling intensity ------------------------------------
# going to do this over ... hmm.
# well they did it over: 43, 176, 350 and 700 cell sizes.   

  # will try first 10, 50, 150, 500
  # that's actually quite complicated, gonna just do 100, and rerun and do like 150 perhaps
  
  library(ggplot2)
  library(ggmap)
  library(tidyr)
  library(raster)
  library(rgdal)
  library(maptools)
  library(dplyr)
  
  
  setwd("C:/Users/s436862/Dropbox/Climate Matching/1. Data files/")
  
  ### data  
  # raster template
  b <- raster("EFs/EFs cropped/arid.grd")
  
  # Poa species
  p <- read.csv("AVH/AVH grass records.csv", header = T) %>%
    dplyr::select(species, lat, long)
  
  xy <- cbind(p$long, p$lat)
  spp <- as.numeric(factor(p$species))
  
  #width <- 50
  
  ### loop function
  ras <- function(width){
    
    raster <- aggregate(b, fact = width, fun = mean)
    
    # actual richness (a)
      a <- rasterize(xy, raster, field = spp, fun = function(x,...) {length(unique(na.omit(x))) })
    # number of records (n)
      n <- rasterize(xy, raster, fun = function(x,...) {length(na.omit(x)) })
    # chao (c)
      sing <- rasterize(xy, raster, field = spp, fun = function(x,...) {length(which(table(x)==1)) })
      doub <- rasterize(xy, raster, field = spp, fun = function(x,...) {length(which(table(x)==2)) })
    
    # reference for extraterrestrial NA removal
      b1 <- getValues(raster)
    
    # actual richness
      a1 <- getValues(a)
    
    
    # chao correctives
      c_sing <- getValues(sing)
      c_doub <- getValues(doub)
      n1 <- getValues(n)
    
    # dataframe
      df_na <- data.frame(a1, b1, c_sing, c_doub, n1)
      df <- df_na[!is.na(b1), ] 
    
    
    # sample size correction
      df$c1 <- ifelse(is.na(df$c_doub) == FALSE,
                    (ifelse(df$c_doub == 0, 
                            df$a1 + ((df$c_sing * (df$c_sing - 1)) / (2 * (df$c_doub + 1)) * ((df$n1 - 1) / df$n1)),   
                            df$a1 + (((df$c_sing ^ 2) / (2 * df$c_doub)) * ((df$n1 - 1) / df$n1)))),
                    NA)
    
    ### what do I need to calculate?
      # ratio
      a_mean <- mean(df$a1, na.rm = TRUE) 
      c_mean <- mean(df$c1, na.rm = TRUE) 
      ratio <- a_mean / c_mean
      
      # the sheer amount of records / area (how many 1 x km^2's we have) 
      # call thius, sampling intensity
      # might need to do a log scale
      
      int <- na.omit(df$n1 / width)
    
      res <- as.data.frame(cbind(ratio, int))
     
    return(res)
  }  #finish function
  
  ####
  
  width <- c(10, 50, 150, 500)
  out <- matrix(nrow = length(width), ncol = 3)
  
  for(i in 1:length(width)) {
    out[i, 3] <- width[i]
    out[i, 3] <- as.numeric(ras(width[i]))
  }
  
  lrg_all <- out
  
  colnames(lrg_all) <- c("cell width (km)", "actual mean", "corrected mean", "variance", "coefficient of variation", "standard deviation", "confidence interval positive", "confidence interval negative", "ratio", "coverage", "number of grid cells")
  lrg_poa <- as.data.frame(lrg_all)
  
  # run all that once, and then I won't have to again if I do other scripts with 'out' in them
  
  

  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  