
# summary statistics of rarefaction outputs


# library -------------------------------------------------------------------------
  library(tidyverse)
  library(raster)
  library(rgdal)
  
  setwd("C:/Users/s436862/Dropbox/Rarefaction/Data files")
  
  rm(list = ls())

# 1. data  -----------------------------------------------------------------------------
# poaceae
  dat <- readRDS("ALA/2019 ALA master data/master grass data.rds") %>%
    dplyr::select(-sub.family)

# australia 
  raster <- raster("Australia/aus 100-km.grd")
  plot(raster)

# 1.1 data filtering --------------------------------------------------------------------
  nat <- dat %>% filter(status == "native")
  exo <- dat %>% filter(status == "exotic")

# number of species
  nat.species <- levels(as.factor(nat$species))
  exo.spp <- unique(exo$species)
  
# species by pathway
  dat.spp <- distinct(dat, species, .keep_all = T)
 
  group_by(dat.spp, status, pp) %>%
    summarise(n = n())
  
# 2. native ------------------------------------------------------------------------------
  xy.n <- cbind(nat$longitude, nat$latitude)

# assign raster cell to each point
  nat$cell <- raster::extract(raster, xy.n)

# total records per cell
  n_tot.n <- rasterize(xy.n, raster, fun = function(x, ...) length(x))
  plot(log10(n_tot.n))

# mask offshore values
  nat.masked <- mask(n_tot.n, raster)
  plot(nat.masked)

# retrieve values
  nat.recs <- sum(getValues(nat.masked), na.rm = T)
  
# 3. exotic ------------------------------------------------------------------------------
  xy.e <- cbind(exo$longitude, exo$latitude)
  
# assign raster cell to each point
  exo$cell <- raster::extract(raster, xy.e)
  
# total records per cell
  n_tot.e <- rasterize(xy.e, raster, fun = function(x, ...) length(x))
  plot(log10(n_tot.e))
  
# mask offshore values
  exo.masked <- mask(n_tot.e, raster)
  plot(exo.masked)
  
# retrieve values
  exo.recs <- sum(getValues(exo.masked), na.rm = T)  
  
# 2. summary statistics on observed rarefaction 10-50 subsamples --------------------------------------------------  
# 2.1 native + exotic correlation matrix on subsamples ----------------------------------------------------------
  rm(list = ls())
  
  nat <- read.csv("Results/Rarefaction/CSV/Rarefied native richness 10 to 50 records.csv", header = T)
  exo <- read.csv("Results/Rarefaction/CSV/Rarefied exotic richness 10 to 50 records.csv", header = T)
  
  poa <- cbind(nat, exo)
  
  cor <- cor(poa, use = "complete.obs")
  write.csv(cor, "Results/Rarefaction/CSV/Rarefied richness 10 to 50 record correlation matrix.csv", row.names = T)
  
# 2.2 native percentage of cells occupied ---------------------------------------------------  
  # rarefied richness data
  nat <- read.csv("Results/Rarefaction/CSV/Rarefied native richness 10 to 50 records.csv", header = T)
  
  # cell categories
  aus.cells <- read.csv("C:/Users/s436862/Dropbox/Rarefaction/Data files/Australia/aus 100-km.csv", header = T)
  land.cells <- filter(aus.cells, cell.cat == "land") # 1133
  
  # land cells
  dat.cells <- cbind(aus.cells, nat)
  dat.cells.f <- filter(dat.cells, cell.cat == "land")
  
  # nas to zero for percentage estimates
  dat.cells.f[is.na(dat.cells.f)] <- 0
  dat.cells.e <- dat.cells.f[,4:ncol(dat.cells.f)]
  
  # the amount of cells occupied for each column
  d <- dat.cells.e
  k <- length(dat.cells.e[,1])
  c3.10 <- sum(d$C3.rare.10 > 0)/k*100
  
  ff <- function(x) (sum(x > 0))/k*100
  
  
  d.p <- apply(d, MARGIN = 2, ff)
  dd.p <- data.frame(as.list(d.p))
  
  write.csv(dd.p, "Results/Rarefaction/CSV/Percent of cells occupied native.csv", row.names = F)
  
# --------------------------------------------------------------------------------------  
# 2.3 exotic percentage of cells occupied ---------------------------------------------------  
# rarefied richness data
  exo <- read.csv("Results/Rarefaction/CSV/Rarefied exotic richness 10 to 50 records.csv", header = T)
  
# cell categories
  aus.cells <- read.csv("C:/Users/s436862/Dropbox/Rarefaction/Data files/Australia/aus 100-km.csv", header = T)
  land.cells <- filter(aus.cells, cell.cat == "land") # 1133
  
# land cells
  dat.cells <- cbind(aus.cells, exo)
  dat.cells.f <- filter(dat.cells, cell.cat == "land")
  
# nas to zero for percentage estimates
  dat.cells.f[is.na(dat.cells.f)] <- 0
  dat.cells.e <- dat.cells.f[,4:ncol(dat.cells.f)]
  
# the amount of cells occupied for each column
  d <- dat.cells.e
  k <- length(dat.cells.e[,1])
  c3.10 <- sum(d$C3.rare.10 > 0)/k*100
  
  ff <- function(x) (sum(x > 0))/k*100
  
  
  d.p <- apply(d, MARGIN = 2, ff)
  dd.p <- data.frame(as.list(d.p))
  
  write.csv(dd.p, "Results/Rarefaction/CSV/Percent of cells occupied exotic.csv", row.names = F)
  
# -------------------------------------------------------------------------------------------  
  
# 3. percentage of cells above a proportion of total, C3 and C4 (at 15-records) ---------------
# e.g. what pencentage of cells have above 90% C4s? 80%? 70%? Etc.
  setwd("C:/Users/s436862/Dropbox/Rarefaction/Results")

# 3.1 data frame -------------------------------------------------------------------------------
# Australia land categories and values
  aus.cells <- read.csv("C:/Users/s436862/Dropbox/Rarefaction/Data files/Australia/aus 100-km.csv", header = T)
  
# rasters as data frames (note richness standardised between 0 and 1)
  n.tot <- getValues(raster("Rarefaction/Rasters/Rarefied rasters/r15.n.tot.grd")/15)
  n.c3 <- getValues(raster("Rarefaction/Rasters/Rarefied rasters/r15.n.c3.grd")/15)
  n.c4 <- getValues(raster("Rarefaction/Rasters/Rarefied rasters/r15.n.c4.grd")/15)
  
  e.tot <- getValues(raster("Rarefaction/Rasters/Rarefied rasters/r15.e.tot.grd")/15)
  e.c3 <- getValues(raster("Rarefaction/Rasters/Rarefied rasters/r15.e.c3.grd")/15)
  e.c4 <- getValues(raster("Rarefaction/Rasters/Rarefied rasters/r15.e.c4.grd")/15)
 
# bind 'em
  prop <- cbind(aus.cells, 
                n.tot, n.c3, n.c4, 
                e.tot, e.c3, e.c4)
  prop.l <- prop %>% filter(., cell.cat == "land")
  prop.s <- prop.l %>% dplyr::select(-cell.cat, -prop.cover, -cell.id)
  
# Na to zeroes
  prop.s[is.na(prop.s)] <- 0
  head(prop.s)
  
# 3.2 proportion thresholds caulcualtions ------------------------------------------------- 
# >: 0.9, 0.8 ... 0.5
# <=: 0.5, 0.4 ... 0.1

# > thresholds ----------------------------------------------------------------------------
  l <- length(prop.s[,1])
# test  
  test90 <- sum(prop.s$n.tot > 0.9, na.rm = T)/l*100
 
# 90
  p90 <- function(x) (sum(x > 0.9, na.rm = T))/l*100
  p90.l <- apply(prop.s, MARGIN = 2, p90)
  p90.df <- data.frame(as.list(p90.l))
  
# 80
  p80 <- function(x) (sum(x > 0.8, na.rm = T))/l*100
  p80.l <- apply(prop.s, MARGIN = 2, p80)
  p80.df <- data.frame(as.list(p80.l))
  
# 70
  p70 <- function(x) (sum(x > 0.7, na.rm = T))/l*100
  p70.l <- apply(prop.s, MARGIN = 2, p70)
  p70.df <- data.frame(as.list(p70.l))
  
# 60
  p60 <- function(x) (sum(x > 0.6, na.rm = T))/l*100
  p60.l <- apply(prop.s, MARGIN = 2, p60)
  p60.df <- data.frame(as.list(p60.l))
  
# 50
  p50 <- function(x) (sum(x > 0.5, na.rm = T))/l*100
  p50.l <- apply(prop.s, MARGIN = 2, p50)
  p50.df <- data.frame(as.list(p50.l))
  
# <= thrsholds -----------------------------------------------  
# test  
  test10 <- sum(prop.s$e.c4 <= 0.1, na.rm = T)/l*100
  
# 10
  p10 <- function(x) (sum(x <= 0.1, na.rm = T))/l*100
  p10.l <- apply(prop.s, MARGIN = 2, p10)
  p10.df <- data.frame(as.list(p10.l))
  
# 20
  p20 <- function(x) (sum(x <= 0.2, na.rm = T))/l*100
  p20.l <- apply(prop.s, MARGIN = 2, p20)
  p20.df <- data.frame(as.list(p20.l))
  
# 30
  p30 <- function(x) (sum(x <= 0.3, na.rm = T))/l*100
  p30.l <- apply(prop.s, MARGIN = 2, p30)
  p30.df <- data.frame(as.list(p30.l))
  
# 40
  p40 <- function(x) (sum(x <= 0.4, na.rm = T))/l*100
  p40.l <- apply(prop.s, MARGIN = 2, p40)
  p40.df <- data.frame(as.list(p40.l))
  
#" 49" (even though its 50 and under, as its the same names as above's 50)
  p49 <- function(x) (sum(x <= 0.5, na.rm = T))/l*100
  p49.l <- apply(prop.s, MARGIN = 2, p49)
  p49.df <- data.frame(as.list(p49.l))
  
# 3.3 merge and save ----------------------------------------------------------- 
  prop.df <- rbind(p90.df, p80.df, p70.df, p60.df, p50.df, 
                   p49.df, p40.df, p30.df, p20.df, p10.df)
  
  row.names(prop.df) <- c(">90", ">80", ">70", ">60", ">50",
                          "<=50", "<=40", "<=30", "<=20", "<=10")
  
  prop.df.t <- t(prop.df)
  
  write.csv(prop.df.t, "Rarefaction/CSV/Percentage of observed occupied cells at proportions thesholds.csv", row.names = T)
  
# --------------------------------------------------------------------------------
  
# 4. correlation matrix of observed and predicted distributions --------------------------  
# combine native and exotic rasters to data frame
# observed raster data
  setwd("C:/Users/s436862/Dropbox/Rarefaction/Results/Rarefaction/Rasters/Rarefied rasters")
  
  current.list <- list.files(pattern = ".grd")
  names <- gsub(pattern = "\\.grd$", "", current.list)
  
  c.stack <- stack(current.list)
  names(c.stack) <- names
  
  list2env(setNames(unstack(c.stack), names(c.stack)), .GlobalEnv)
  
# predicted raster data
  setwd("C:/Users/s436862/Dropbox/Rarefaction/Results/Rarefaction/Rasters/Predicted distributions")
  
  current.list <- list.files(pattern = ".grd")
  names <- gsub(pattern = "\\.grd$", "", current.list)
  
  c.stack <- stack(current.list)
  names(c.stack) <- names
  
  list2env(setNames(unstack(c.stack), names(c.stack)), .GlobalEnv) 
  
# o = observed, p = predicted; n = native, e = exotic; tot, c3 and c4
  o.n.tot <- getValues(r15.n.tot)
  o.n.c3 <- getValues(r15.n.c3)
  o.n.c4 <- getValues(r15.n.c4)
  
  o.e.tot <- getValues(r15.e.tot)
  o.e.c3 <- getValues(r15.e.c3)
  o.e.c4 <- getValues(r15.e.c4)
  
  p.n.tot <- getValues(n.tot.predicted)
  p.n.c3 <- getValues(n.c3.predicted)
  p.n.c4 <- getValues(n.c4.predicted)
  
  p.e.tot <- getValues(e.tot.predicted)
  p.e.c3 <- getValues(e.c3.predicted)
  p.e.c4 <- getValues(e.c4.predicted)
  
  spp.df <- cbind(o.n.tot, o.n.c3, o.n.c4, 
                  o.e.tot, o.e.c3, o.e.c4,
                  p.n.tot, p.n.c3, p.n.c4,
                  p.e.tot, p.e.c3, p.e.c4) 
  
  spp.df <- data.frame(spp.df) 
  
# correlation matrix
  p.o.cor <- cor(spp.df, use = "complete.obs")
  write.csv(p.o.cor, "C:/Users/s436862/Dropbox/Rarefaction/Results/Rarefaction/CSV/Observed-predicted distribution correlation matrix.csv", row.names = T)    
  
  
# --------------------------------------------------------------------------------------    