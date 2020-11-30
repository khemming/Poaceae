
# Date created: 3/8/18
# Updated: 26/4 
# using independent rarefaction method, comparing to previous method of proportions of C3/C4

# aim ------------------------------------------------------------------------
# model the total, C3 and C4 richness for 15-rarefied richness at 100 km cell size for native and exotic grasses, using two methods: the previous (proportion) way, and the 'solo' (independent) results

# library ------------------------------------------------------------------
  library(dplyr)
  library(broom)
  library(magrittr)
  library(raster)

  rm(list = ls())
  
  setwd("C:/Users/s436862/Dropbox/Rarefaction/Results")
  
# data ----------------------------------------------------------------   
# environmental data (retained from varaible selection process)
# note this is also the order these pop up in the coefficient plots, so vary it here if need be
  vs.evs <- c("cell.category.v2", "prop.cover", "pcoldq", "pwarmq", "amt", "ts", "arid", "pewc", "th", "hii")

# select from wider EV list
  evs <- read.csv("EVs/CSV/100 km EFs scaled.csv", header = T) %>%
            dplyr::select(vs.evs) %>%
            mutate(prop.cover = prop.cover/100) # convert prop cover from 0 - 1 

# rarefied data (generated from 'Plot' script)
  spp <- read.csv("Rarefaction/CSV/Rarefied 15 richness and record number pp independent.csv", header = T) %>%
         dplyr::select(Native.C3.rich, Native.C4.rich, 
                       Exotic.C3.rich, Exotic.C4.rich)
   
  colnames(spp) <- c("n.c3", "n.c4", 
                     "e.c3", "e.c4")
  head(spp)
 
# bind spp and EV data and subset to terrestrial only cells   
  spp.ev <- cbind(evs, spp) %>%
    filter(cell.category.v2 == "land") %>%
    dplyr::select(-cell.category.v2)
  
# models --------------------------------------------------------------
# native C3  
  m.n.c3 <- lm(n.c3 ~ pcoldq + pwarmq + amt + ts + arid + pewc + th + hii + prop.cover, data = spp.ev)
  summary(m.n.c3) 
  m.n.c3.sum <- tidy(m.n.c3)
  m.n.c3.ci <- confint(m.n.c3)

# native C4
  m.n.c4 <- lm(n.c4 ~ pcoldq + pwarmq + amt + ts + arid + pewc + th + hii + prop.cover, data = spp.ev)
  summary(m.n.c4) 
  m.n.c4.sum <- tidy(m.n.c4)
  m.n.c4.ci <- confint(m.n.c4)
   
# exotic C3  
  m.e.c3 <- lm(e.c3 ~ pcoldq + pwarmq + amt + ts + arid + pewc + th + hii + prop.cover, data = spp.ev)
  summary(m.e.c3) 
  m.e.c3.sum <- tidy(m.e.c3)
  m.e.c3.ci <- confint(m.e.c3)

# exotic C4
  m.e.c4 <- lm(e.c4 ~ pcoldq + pwarmq + amt + ts + arid + pewc + th + hii + prop.cover, data = spp.ev)
  summary(m.e.c4) 
  m.e.c4.sum <- tidy(m.e.c4)
  m.e.c4.ci <- confint(m.e.c4)
  
# coefficient dataframe --------------------------------------------
# note we are dropping prop.cover
# Status (n/e) | coefficient (evs) | mean | lower.ci | upper.ci
  spp.ev.mat <- matrix(ncol = 5, nrow = 16)  
  spp.ev.mat[,1] <- rep(c("native", "exotic"), each = 8)
  spp.ev.mat[,2] <- rep(vs.evs[3:10], 2)
  colnames(spp.ev.mat) <- c("status", "coef", "estimate", "lower.ci", "upper.ci")

# One for each total, C3 and C4 richness (i.e. native and exotic will be plotted together) 
  c3.rich <- spp.ev.mat
  c4.rich <- spp.ev.mat
  
 # insert coeffficents & CIs --------------------------------
# 2:9 excludes the intercept and prop.cover terms
# C3 native   
  c3.rich[1:8, 3] <- m.n.c3.sum$estimate[2:9]
  c3.rich[1:8, 4:5] <- m.n.c3.ci[2:9, ]
# C3 exotic 
  c3.rich[9:16, 3] <- m.e.c3.sum$estimate[2:9]
  c3.rich[9:16, 4:5] <- m.e.c3.ci[2:9, ]
  
# C4 native  
  c4.rich[1:8, 3] <- m.n.c4.sum$estimate[2:9]
  c4.rich[1:8, 4:5] <- m.n.c4.ci[2:9, ]
# C4 exotic  
  c4.rich[9:16, 3] <- m.e.c4.sum$estimate[2:9]
  c4.rich[9:16, 4:5] <- m.e.c4.ci[2:9, ]
 
# data frame   
  c3.rich <- data.frame(c3.rich)
  c4.rich <- data.frame(c4.rich)
  
# Mean and CIs as numbers for plotting  
  c3.rich[,3:5] %<>% lapply(function(x) as.numeric(as.character(x)))
  c4.rich[,3:5] %<>% lapply(function(x) as.numeric(as.character(x)))
  
# save workspace to use for plotting
  save.image("Rarefaction/Rdata/Independent_rarefaction_model_coefficient_values.RData") 
   
 
# ----------------------------------------------------------------------------

  

 