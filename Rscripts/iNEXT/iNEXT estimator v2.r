############################################################################
# species richness estimation using iNEXT (coverage) based estimator 
############################################################################
# date created: 3/4/19
# last updated: 1/7/19

# based on Richard's script in dedicated folder, and v1

# library -------------------------------------------------------------------------
  library(tidyverse)
  library(raster)
  library(rgdal)
  library(iNEXT)

  setwd("C:/Users/s436862/Dropbox/Poaceae")

  rm(list = ls())

# data ----------------------------------------------------------------------------
# australia 
  raster <- raster("Data files/Australia/aus 100 km v2.grd")

# poaceae
  dat.all <- readRDS("Data files/ALA/ALA master data/master grass records.rds") 

# iNEXT function ------------------------------------------------------------------
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
          
  # get a list of the occupied cell numbers
    cell.list <- as.numeric(as.character(levels(factor(cr$cell))))
    
  # store coverage output
    out_cov <- numeric()
  # store size output
    out_size <- numeric()
  # check for warning
    out_warn <- numeric()
    
    # do the rarefaction cell by cell  
      for(i in 1:length(cell.list)) {
        td <- data.frame(spp = cr$n[cr$cell == cell.list[i]])
        td <- td[!is.na(td$spp), ]
    
    # coverage rarefaction using iNEXT function
    # check for warning
        out_warn[i] <- 0
        temp1 <- tryCatch(estimateD(td, datatype = "abundance", base = "coverage", level = coverage, conf = NULL), warning = function(w) {out_warn[i] <<- 1})
        
    # if there was a warning run again
        if(out_warn[i] == 1) temp1 <- estimateD(td, datatype = "abundance", base = "coverage", level = coverage, conf = NULL)
        out_cov[i] <- temp1[, 4]
    
    # size rarefaction using iNEXT function
        temp2 <- estimateD(td, datatype = "abundance", base = "size", level = min.rec, conf = NULL)
        out_size[i] <- temp2[, 4]
      }
    
  # put the rarefaction estimates into the raster
  # need to include the missing cell values as well as the occupied cells
    cell_cov <- rep(NA, length(getValues(raster)))
    cell_size <- rep(NA, length(getValues(raster)))
    cell_cov_warn <- rep(NA, length(getValues(raster)))
  
  # add the occupied cells
    cell_cov[cell.list] <- out_cov
    cell_size[cell.list] <- out_size
  
  # coverage estimates with warning cells set to NA
    cell_cov_warn[cell.list] <- out_warn
    out_cov_warn <- ifelse(out_warn == 1, NA, out_cov)
    cell_cov_warn[cell.list] <- out_cov_warn
  
  # generate the raster object for estimated richness  
    rast_cov <- setValues(raster, cell_cov)
    rast_size <- setValues(raster, cell_size)
  
  # coverage raster with warning cells set to NA
    rast_cov_warn <- setValues(raster, cell_cov_warn)
    
  # number of records per cell
    nrec <- rep(NA, length(getValues(raster)))
    nrec[nr$cell] <- nr$n.rec
    
    nrec[nr$cell] <- nr$n.rec
    
  # raw species richness
    spp_per_cell <- as.numeric(factor(dat$species))
    n_spp <- rasterize(xy, raster, field = spp_per_cell, fun = function(x,...) {length(unique(na.omit(x))) })
    m_spp <- mask(n_spp, raster)
    plot(m_spp)
    
  # return the values for each cell and the rasters
    return(list(cell_cov, rast_cov, cell_size, rast_size, nrec, cell_cov_warn, rast_cov_warn, m_spp))
    
}
# -----------------------------------------------------------------------------------

# write data etc. -------------------------------------------------------------------
# notes -----------------------------------------------------------------------------
# we produced several arrays of origin x pathway
# these 8 lists in each as follows: [[1]] df sr calculated by coverage - w warning cells
#                                   [[2]] raster sr calculated by coverage - w warning cells
#                                   [[3]] df sr calculated by size (i.e. 15-rec rarefaction)                   
#                                   [[4]] raster sr calculated by size (i.e. 15-rec rarefaction)    
#                                   [[5]] df number of records per cell
#                                   [[6]] df coverage w warning cells removed
#                                   [[7]] raster coverage w warning cells removed
#                                   [[8]] raw species richness       
# -----------------------------------------------------------------------------------  
# filter to group of interest and run function on each
  dat <- dat.all %>%
         ungroup() %>%
         filter(status == "native" & pp == "C4") %>%
         mutate(species = factor(species))
       
  nc4 <- cs(dat)  
  
  par(mfrow = c(2, 2))

# plot records
  nrec <- setValues(raster, nc4[[5]])
  plot(nrec)
# plot sr with warining cells removed
  plot(nc4[[7]])   
# plot raw sr
  plot(nc4[[8]])
       
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


# -----------------------------------------------------------------------------
# plotting checks -------------------------------------------------------------
# all native and exotic richness
  par(mfrow= c(2, 2))
  plot(allnat[[2]])
  plot(allexotic[[2]])
    a <- cor.test(allexotic[[1]], allnat[[1]], method = "spearman")
  plot(allexotic[[1]] ~ allnat[[1]], main = round(a$estimate, 2))
  
# plot the coverage results w warnings retained
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
  

# estimates with warning cells removed
# all native and exotic richness
  par(mfrow= c(2, 2))
  plot(allnat[[7]])
  plot(allexotic[[7]])
    a <- cor.test(allexotic[[6]], allnat[[6]], method = "spearman")
  plot(allexotic[[6]] ~ allnat[[6]], main = round(a$estimate, 2))
  
# plot the coverage results
  par(mfrow = c(3, 3), mar = c(3, 3, 1, 1))
  plot(nc4[[7]])
  plot(nc3[[7]])
    a <- cor.test(nc4[[6]], nc3[[6]], method = "spearman")
  plot(nc4[[6]] ~ nc3[[6]], main = round(a$estimate, 2))
  plot(ec4[[7]])
  plot(ec3[[7]])
    a <- cor.test(ec4[[6]], ec3[[6]], method = "spearman")
  plot(ec4[[6]] ~ ec3[[6]], main = round(a$estimate, 2))
    a <- cor.test(ec4[[6]], nc4[[6]], method = "spearman")
  plot(ec4[[6]] ~ nc4[[6]], main = round(a$estimate, 2))
    abline(0, 1)
    a <- cor.test(ec3[[6]], nc3[[6]], method = "spearman")
  plot(ec3[[6]] ~ nc3[[6]], main = round(a$estimate, 2))
    abline(0, 1)

# 3. saving ----------------------------------------------------------------------------
# notes -----------------------------------------------------------------------------
# there are lists for each origin x pp
# these 8 lists are as follows: [[1]] df sr calculated by coverage - w warning cells
#                               [[2]] raster rr calculated by coverage - w warning cells
#                               [[3]] df sr calculated by size (i.e. 15-rec rarefaction)                        #                               [[4]] raster sr calculated by size (i.e. 15-rec rarefaction)    
#                               [[5]] df number of records per cell
#                               [[6]] df coverage w warning cells removed
#                               [[7]] raster coverage w warning cells removed
#                               [[8]] raw species richness       
# ------------------------------------------------------------------------------------    

# all the info together 
  save(allnat, allexotic, nc3, nc4, ec3, ec4, file = "Results/iNEXT/Rdata/iNEXT estimator v2.RData")

# rasters ----------------------------------------------------------------------------
  par(mfrow = c(1, 1))
  
# record number [[5]]
  n.tot <- setValues(raster, allnat[[5]])
  plot(n.tot)  
  writeRaster(n.tot, "Results/iNEXT/Rasters/record number/n.tot.grd", overwrite = T)
  n.c3 <- setValues(raster, nc3[[5]])
  plot(n.c3)   
  writeRaster(n.c3, "Results/iNEXT/Rasters/record number/n.c3.grd", overwrite = T)
  n.c4 <- setValues(raster, nc4[[5]])
  plot(n.c4) 
  writeRaster(n.c4, "Results/iNEXT/Rasters/record number/n.c4.grd", overwrite = T)
  
  e.tot <- setValues(raster, allexotic[[5]])
  plot(e.tot)  
  writeRaster(e.tot, "Results/iNEXT/Rasters/record number/e.tot.grd", overwrite = T)
  e.c3 <- setValues(raster, ec3[[5]])
  plot(e.c3)   
  writeRaster(e.c3, "Results/iNEXT/Rasters/record number/e.c3.grd", overwrite = T)
  e.c4 <- setValues(raster, ec4[[5]])
  plot(e.c4) 
  writeRaster(e.c4, "Results/iNEXT/Rasters/record number/e.c4.grd", overwrite = T)
    
# species richness [[8]]
  n.tot <- allnat[[8]]
  plot(n.tot)  
  writeRaster(n.tot, "Results/iNEXT/Rasters/species richness/n.tot.grd", overwrite = T)
  n.c3 <- nc3[[8]]
  plot(n.c3)   
  writeRaster(n.c3, "Results/iNEXT/Rasters/species richness/n.c3.grd", overwrite = T)
  n.c4 <- nc4[[8]]
  plot(n.c4) 
  writeRaster(n.c4, "Results/iNEXT/Rasters/species richness/n.c4.grd", overwrite = T)
  
  e.tot <- allexotic[[8]]
  plot(e.tot)  
  writeRaster(e.tot, "Results/iNEXT/Rasters/species richness/e.tot.grd", overwrite = T)
  e.c3 <- ec3[[8]]
  plot(n.c3)   
  writeRaster(e.c3, "Results/iNEXT/Rasters/species richness/e.c3.grd", overwrite = T)
  e.c4 <- ec4[[8]]
  plot(e.c4) 
  writeRaster(e.c4, "Results/iNEXT/Rasters/species richness/e.c4.grd", overwrite = T)
    
# 15-record rarefaction [[4]]
  n.tot <- allnat[[4]]
  plot(n.tot)  
  writeRaster(n.tot, "Results/iNEXT/Rasters/observed 15 rec/n.tot.grd", overwrite = T)
  n.c3 <- nc3[[4]]
  plot(n.c3)   
  writeRaster(n.c3, "Results/iNEXT/Rasters/observed 15 rec/n.c3.grd", overwrite = T)
  n.c4 <- nc4[[4]]
  plot(n.c4) 
  writeRaster(n.c4, "Results/iNEXT/Rasters/observed 15 rec/n.c4.grd", overwrite = T)
  
  e.tot <- allexotic[[4]]
  plot(e.tot)  
  writeRaster(e.tot, "Results/iNEXT/Rasters/observed 15 rec/e.tot.grd", overwrite = T)
  e.c3 <- ec3[[4]]
  plot(n.c3)   
  writeRaster(e.c3, "Results/iNEXT/Rasters/observed 15 rec/e.c3.grd", overwrite = T)
  e.c4 <- ec4[[4]]
  plot(e.c4) 
  writeRaster(e.c4, "Results/iNEXT/Rasters/observed 15 rec/e.c4.grd", overwrite = T)
  
# 15 record 0.8 coverage warning RETAINED [[2]]
  n.tot <- allnat[[2]]
  plot(n.tot)  
  writeRaster(n.tot, "Results/iNEXT/Rasters/observed 15 rec 0.8 cov warn retained/n.tot.grd", overwrite = T)
  n.c3 <- nc3[[2]]
  plot(n.c3)   
  writeRaster(n.c3, "Results/iNEXT/Rasters/observed 15 rec 0.8 cov warn retained/n.c3.grd", overwrite = T)
  n.c4 <- nc4[[2]]
  plot(n.c4) 
  writeRaster(n.c4, "Results/iNEXT/Rasters/observed 15 rec 0.8 cov warn retained/n.c4.grd", overwrite = T)
  
  e.tot <- allexotic[[2]]
  plot(e.tot)  
  writeRaster(e.tot, "Results/iNEXT/Rasters/observed 15 rec 0.8 cov warn retained/e.tot.grd", overwrite = T)
  e.c3 <- ec3[[2]]
  plot(n.c3)   
  writeRaster(e.c3, "Results/iNEXT/Rasters/observed 15 rec 0.8 cov warn retained/e.c3.grd", overwrite = T)
  e.c4 <- ec4[[2]]
  plot(e.c4) 
  writeRaster(e.c4, "Results/iNEXT/Rasters/observed 15 rec 0.8 cov warn retained/e.c4.grd", overwrite = T)
  
# 15-record 0.8 coverage warning REMOVED [[7]]
  n.tot <- allnat[[7]]
  plot(n.tot)  
  writeRaster(n.tot, "Results/iNEXT/Rasters/observed 15 rec 0.8 cov warn removed/n.tot.grd", overwrite = T)
  n.c3 <- nc3[[7]]
  plot(n.c3)   
  writeRaster(n.c3, "Results/iNEXT/Rasters/observed 15 rec 0.8 cov warn removed/n.c3.grd", overwrite = T)
  n.c4 <- nc4[[7]]
  plot(n.c4) 
  writeRaster(n.c4, "Results/iNEXT/Rasters/observed 15 rec 0.8 cov warn removed/n.c4.grd", overwrite = T)
  
  e.tot <- allexotic[[7]]
  plot(e.tot)  
  writeRaster(e.tot, "Results/iNEXT/Rasters/observed 15 rec 0.8 cov warn removed/e.tot.grd", overwrite = T)
  e.c3 <- ec3[[7]]
  plot(n.c3)   
  writeRaster(e.c3, "Results/iNEXT/Rasters/observed 15 rec 0.8 cov warn removed/e.c3.grd", overwrite = T)
  e.c4 <- ec4[[7]]
  plot(e.c4) 
  writeRaster(e.c4, "Results/iNEXT/Rasters/observed 15 rec 0.8 cov warn removed/e.c4.grd", overwrite = T)

# ---------------------------------------------------------------------------
       