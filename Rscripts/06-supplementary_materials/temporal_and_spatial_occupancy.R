
# library -----------------------------------------------------
  library(tidyverse)
  library(raster)
  
  rm(list = ls())
  
# data --------------------------------------------------------
# record data
  dat <- readRDS("Data files/ALA/master data/master grass records.rds")  
  
# species richness and predictor varaibles
  spp_pv <- read.csv("Results/csv/spp predictor variables 1104.csv")
  
# raster
  aus <- raster("Data files/Australia/Australia 1136.grd")
  
# data columns: 
# fo = first occurence (grand species mean)
# cells_occ_all = % cells >=1 records
# cells_occ_15 = % cells >=15 records
# hii_cor = correlation with human impacts
  df <- data.frame(spp = names(spp_pv[1:6]),
                         fo = NA,
                         cell_occ_all = NA,
                         cell_occ_15 = NA,
                         hii_cor = NA)
  
# mean first record of occurrence ----------------------------
  pp_fun <- function(p_p, stat_us) {
      x <- dat %>% filter(pp == p_p,
                          status == stat_us) %>% 
                   ungroup() %>%                 # if there are any prev groupings
                   group_by(species, year) %>%   # arrange by species and their year 
                   arrange(.by_group = T) %>%    # earliest species' record first
                   ungroup() %>%
                   distinct(species, .keep_all = T) # takes that first record only
      y <- ceiling(mean(x$year))
    return(y)
  }
  
  total_fun <- function(stat_us) {
    x <- dat %>% filter(status == stat_us) %>% 
      ungroup() %>%                 
      group_by(species, year) %>%   
      arrange(.by_group = T) %>%   
      ungroup() %>%
      distinct(species, .keep_all = T) 
    y <- ceiling(mean(x$year))
    return(y)
    }
 
# native (C3, C4, total)
  df$fo[1] <- pp_fun("C3", "native")
  df$fo[2] <- pp_fun("C4", "native")
  df$fo[3] <- total_fun("native")
# nonnative (C3, C4, total)    
  df$fo[4] <- pp_fun("C3", "nonnative")
  df$fo[5] <- pp_fun("C4", "nonnative")
  df$fo[6] <- total_fun("nonnative")
 
  df  

# proportions of occurrence -------------------------------------  
  prop_occ_all <- function(p_p, stat_us){
  
    group <- dat %>% filter(status == stat_us, pp == p_p)
    xy <- cbind(group$longitude, group$latitude)
    ras <- rasterize(xy, aus, fun = function(x,...) {length(unique(na.omit(x))) })
    ras2 <- getValues(ras)
    ras3 <- length(na.omit(ras2))
    occ <- round(ras3/length(spp_pv), 2)
    return(occ)}
  
  prop_occ_15 <- function(p_p, stat_us){
    
    group <- dat %>% filter(status == stat_us, pp == p_p)
    xy <- cbind(group$longitude, group$latitude)
    ras <- rasterize(xy, aus, fun = function(x,...) {length(unique(na.omit(x))) })
    ras2 <- getValues(ras)
    ras3 <- ras2[ras2 >= 15]
    ras4 <- length(na.omit(ras3))
    occ <- round(ras4/length(spp_pv), 2)
    return(occ)}
  
  
# run: native 
  df$cell_occ_all[1] <- prop_occ_all("C3", "native")
  df$cell_occ_all[2] <- prop_occ_all("C4", "native")
  df$cell_occ_all[3] <- prop_occ_all("C3", "nonnative")
  df$cell_occ_all[4] <- prop_occ_all("C4", "nonnative")
#  nonnative 
  df$cell_occ_15[1] <- prop_occ_15("C3", "native")
  df$cell_occ_15[2] <- prop_occ_15("C4", "native")
  df$cell_occ_15[3] <- prop_occ_15("C3", "nonnative")
  df$cell_occ_15[4] <- prop_occ_15("C4", "nonnative")
  
  df
  
# correlation with human impact
  df$hii_cor[1] <- round(cor(spp_pv$native_C3, spp_pv$hii, use = "complete.obs", method = "spearman"), 2)
  df$hii_cor[2] <- round(cor(spp_pv$native_C4, spp_pv$hii, use = "complete.obs", method = "spearman"), 2)
  df$hii_cor[3] <- round(cor(spp_pv$nonnative_C3, spp_pv$hii, use = "complete.obs", method = "spearman"), 2)
  df$hii_cor[4] <- round(cor(spp_pv$nonnative_C4, spp_pv$hii, use = "complete.obs", method = "spearman"), 2)

  df  

# save 
  write.csv(df, "Results/csv/appendix - temporal and spatial occupation.csv", row.names = F)
  
# -------------------------------------------------------------
  
  
  
  
  
