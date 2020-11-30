
# Date created: 3/8/18
# Updated: 19/3 (using new EV data)

# aim ------------------------------------------------------------------------
# Model the total, C3 and C4 richness for 15-rarefied richness at 100 km cell size for native and exotic grasses

# library ------------------------------------------------------------------
  library(dplyr)
  library(broom)
  library(magrittr)
  library(raster)

  rm(list = ls())
  
  setwd("C:/Users/s436862/Dropbox/Rarefaction/Results")
  
# data ----------------------------------------------------------------   
# environmental data
# retained from varaible selection process
# note this is also the order these pop up in the plots, so vary it here if need be
  vs.evs <- c("cell.cat", "prop.cover", "pcoldq", "pwarmq", "amt", "ts", "arid", "pewc", "th", "hii")
# select from wider EV list
  evs <- read.csv("EVs/CSV/100 km EFs scaled.csv", header = T) %>%
            dplyr::select(vs.evs) %>%
            mutate(prop.cover = prop.cover/100) # convert prop cover from 0 - 1 

# native species
  nat <- read.csv("Rarefaction/CSV/Rarefied native richness 10 to 50 records.csv", header = T) %>%
         dplyr::select(total.rare.15, C3.rare.15, C4.rare.15)
   
  colnames(nat) <- c("n.tot", "n.c3", "n.c4")
  head(nat)
 
# exotic species
  exo <- read.csv("Rarefaction/CSV/Rarefied exotic richness 10 to 50 records.csv", header = T) %>%
    dplyr::select(total.rare.15, C3.rare.15, C4.rare.15)
  
  colnames(exo) <- c("n.tot", "n.c3", "n.c4")
  head(exo)  
  
# bind nat spp and climate data    
  nat.ev <- cbind(evs, nat) %>%
    filter(cell.cat == "land") %>%
    dplyr::select(-cell.cat)
  
# bind exo spp and climate data    
  exo.ev <- cbind(evs, exo) %>%
    filter(cell.cat == "land") %>%
    dplyr::select(-cell.cat)

# models --------------------------------------------------------------
# native --------------------------------------------------------------
# total
  m.n.tot <- lm(n.tot ~ pcoldq + pwarmq + amt + ts + arid + pewc + th + hii + prop.cover, data = nat.ev)
  summary(m.n.tot) 
  m.n.tot.sum <- tidy(m.n.tot)
  m.n.tot.ci <- confint(m.n.tot)
# C3  
  m.n.c3 <- lm(n.c3 ~ pcoldq + pwarmq + amt + ts + arid + pewc + th + hii + prop.cover, data = nat.ev)
  summary(m.n.c3) 
  m.n.c3.sum <- tidy(m.n.c3)
  m.n.c3.ci <- confint(m.n.c3)
# C4
  m.n.c4 <- lm(n.c4 ~ pcoldq + pwarmq + amt + ts + arid + pewc + th + hii + prop.cover, data = nat.ev)
  summary(m.n.c4) 
  m.n.c4.sum <- tidy(m.n.c4)
  m.n.c4.ci <- confint(m.n.c4)
   
# exotic ------------------------------------------------------------------------ 
# total
  m.e.tot <- lm(n.tot ~ pcoldq + pwarmq + amt + ts + arid + pewc + th + hii + prop.cover, data = exo.ev)
  summary(m.e.tot) 
  m.e.tot.sum <- tidy(m.e.tot)
  m.e.tot.ci <- confint(m.e.tot)
# C3  
  m.e.c3 <- lm(n.c3 ~ pcoldq + pwarmq + amt + ts + arid + pewc + th + hii + prop.cover, data = exo.ev)
  summary(m.e.c3) 
  m.e.c3.sum <- tidy(m.e.c3)
  m.e.c3.ci <- confint(m.e.c3)
# C4
  m.e.c4 <- lm(n.c4 ~ pcoldq + pwarmq + amt + ts + arid + pewc + th + hii + prop.cover, data = exo.ev)
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

# One for each total, C3 and C4 richness (i.e. native and introduced will be plotted together) 
  total.rich <- spp.ev.mat
  c3.rich <- spp.ev.mat
  c4.rich <- spp.ev.mat
  
 # insert coeffficents & CIs --------------------------------
# 2:9 excludes the intercept and prop.cover terms
# Total native  
  total.rich[1:8, 3] <- m.n.tot.sum$estimate[2:9]
  total.rich[1:8, 4:5] <- m.n.tot.ci[2:9, ]
# Total exotic  
  total.rich[9:16, 3] <- m.e.tot.sum$estimate[2:9]
  total.rich[9:16, 4:5] <- m.e.tot.ci[2:9, ]
  
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
  total.rich <- data.frame(total.rich)
  c3.rich <- data.frame(c3.rich)
  c4.rich <- data.frame(c4.rich)
  
# Mean and CIs as numbers for plotting  
  total.rich[,3:5] %<>% lapply(function(x) as.numeric(as.character(x)))
  c3.rich[,3:5] %<>% lapply(function(x) as.numeric(as.character(x)))
  c4.rich[,3:5] %<>% lapply(function(x) as.numeric(as.character(x)))
  
# save workspace to use for plotting
  save.image("Rarefaction/Rdata/Proportion_rarefaction_model_coefficient_values.RData") 
   
 
# ----------------------------------------------------------------------------

  

 