# Date created: 31/7/18
# Last updated: 12/3/19

# update from v6: rarefying C4 and C3 separetely (rather than relative proportions) using the 'independent' rarefaction function (see Rarefaction v5 for original version of equations)

# library -------------------------------------------------------------------------
library(tidyverse)
library(raster)
library(rgdal)
library(iNEXT)

# setwd("C:/Users/s436862/Dropbox/Rarefaction/Data files")
setwd("C:/Users/s429217/onedrive/data/grasses/kyle grid")

rm(list = ls())

# australia 
raster <- raster("aus 100 km v2.grd")


# poaceae
dat.all <- readRDS("master grass data.rds") 

################################################################################
# function to do rarefaction using both coverage and size rarefaction
# specify minimum number of records, which is used as the size to rarify to in the size rarefaction (default = 15)
# specifiy coverage to rarify to (default = 0.8)

cs <- function(dat, min.rec = 15, coverage = 0.8) {

  # assign each point in the dataframe to raster cell
  xy <- cbind(dat$longitude, dat$latitude)
  dat$cell <- raster::extract(raster, xy)
                                         
  # number of records per cell
  nr <- dat %>%
        group_by(cell) %>%
        summarise(n.rec = n()) %>%
        filter(!is.na(cell))
        
  dat <- full_join(dat, nr)
  
# filter by min.rec and extract number of records of each species in each cell
  cr <- dat %>%
        ungroup() %>%
        filter(n.rec >= min.rec) %>%
        mutate(species = factor(species)) %>%
        group_by(species, cell) %>%
        summarise(n = n()) 
        
# get a list of the cell numbers
  cell.list <- as.numeric(as.character(levels(factor(cr$cell))))
  
# store coverage output
  out_cov <- numeric()
# store size output
  out_size <- numeric()
  
# do the rarefaction cell by cell  
  for(i in 1:length(cell.list)) {
    td <- data.frame(spp = cr$n[cr$cell == cell.list[i]])
    td <- td[!is.na(td$spp), ]

# coverage rarefaction using iNEXT function
    temp1 <- estimateD(td, datatype = "abundance", base = "coverage", level = coverage, conf = NULL)
    out_cov[i] <- temp1[, 4]

# size rarefaction using iNEXT function
    temp2 <- estimateD(td, datatype = "abundance", base = "size", level = min.rec, conf = NULL)
    out_size[i] <- temp2[, 4]
  }
  
# remove the most extreme 1% of values from coverage estimate because there are sometimes outliers
  up <- quantile(out_cov, 0.99)
  out_cov[out_cov > up] <- NA
  
# put the rarefaction estimates into the raster
# need to include the missing cell values as well as the occupied cells
  cell_cov <- rep(NA, length(getValues(raster)))
  cell_size <- rep(NA, length(getValues(raster)))

# add the occupied cells
  cell_cov[cell.list] <- out_cov
  cell_size[cell.list] <- out_size
# generate the raster object for estimated richness  
  rast_cov <- setValues(raster, cell_cov)
  rast_size <- setValues(raster, cell_size)
  
# number of records per cell
  nrec <- rep(NA, length(getValues(raster)))
  nrec[nr$cell] <- nr$n.rec
  
  
# return the values for each cell and the rasters
  return(list(cell_cov, rast_cov, cell_size, rast_size, nrec))
  
}
################################################################################

# filter to group of interest and run function on each
  dat <- dat.all %>%
         ungroup() %>%
         filter(status == "native" & pp == "C4") %>%
         mutate(species = factor(species))
       
  nc4 <- cs(dat)     
       
       
  dat <- dat.all %>%
         ungroup() %>%
         filter(status == "native" & pp == "C3") %>%
         mutate(species = factor(species))
       
  nc3 <- cs(dat)     
     
     
  dat <- dat.all %>%
         ungroup() %>%
         filter(status == "exotic" & pp == "C4") %>%
         mutate(species = factor(species))
       
  ec4 <- cs(dat)     

  dat <- dat.all %>%
         ungroup() %>%
         filter(status == "exotic" & pp == "C3") %>%
         mutate(species = factor(species))
       
  ec3 <- cs(dat)     

  dat <- dat.all %>%
         ungroup() %>%
         filter(status == "native") %>%
         mutate(species = factor(species))
       
  allnat <- cs(dat)     

  dat <- dat.all %>%
         ungroup() %>%
         filter(status == "exotic") %>%
         mutate(species = factor(species))
       
  allexotic <- cs(dat)     


# all native and exotic richness
  par(mfrow= c(2, 2))
  plot(allnat[[2]])
  plot(allexotic[[2]])
    a <- cor.test(allexotic[[1]], allnat[[1]], method = "spearman")
  plot(allexotic[[1]] ~ allnat[[1]], main = round(a$estimate, 2))
  
# plot the coverage results
  par(mfrow = c(3, 3), mar = c(3, 3, 1, 1))
  plot(nc4[[2]])
  plot(nc3[[2]])
    a <- cor.test(nc4[[1]], nc3[[1]], method = "spearman")
  plot(nc4[[1]] ~ nc3[[1]], main = round(a$estimate, 2))
  plot(ec4[[2]])
  plot(ec3[[2]])
    a <- cor.test(ec4[[1]], ec3[[1]], method = "spearman")
  plot(ec4[[1]] ~ ec3[[1]], main = round(a$estimate, 2))
    a <- cor.test(ec4[[1]], nc4[[1]], method = "spearman")
  plot(ec4[[1]] ~ nc4[[1]], main = round(a$estimate, 2))
    abline(0, 1)
    a <- cor.test(ec3[[1]], nc3[[1]], method = "spearman")
  plot(ec3[[1]] ~ nc3[[1]], main = round(a$estimate, 2))
    abline(0, 1)
  



       