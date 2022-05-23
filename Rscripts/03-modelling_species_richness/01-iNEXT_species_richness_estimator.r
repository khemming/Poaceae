

# library -------------------------------------------------------------------------
  library(raster)
  library(rgdal)
  library(iNEXT)
  library(tidyverse)

  rm(list = ls())

# data ----------------------------------------------------------------------------
# australia 
  raster <- raster("Data files/Australia/Australia 1104.grd")

# poaceae
  dat_all <- readRDS("Data files/ALA/master data/master grass records.rds") 

# iNEXT function ------------------------------------------------------------------
# function to do rarefaction using both coverage and size rarefaction
# specify minimum number of records, which is used as the size to rarify to in the size rarefaction (default = 15)
# specifiy coverage to rarify to (default = 0.8)

inext <- function(dat, min.rec = 15, coverage = 0.8) {

  # assign each point in the dataframe to raster cell
    xy <- cbind(dat$longitude, dat$latitude)
    dat$cell <- raster::extract(raster, xy)
                                         
  # number of records per cell
    nr <- dat %>%
          group_by(cell) %>%
          summarise(n.rec = n()) %>%
          filter(!is.na(cell))
          
    dat2 <- full_join(dat, nr, by = "cell")
  
  # filter by min.rec and extract number of records of each species in each cell
    cr <- dat2 %>%
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
  dat <- dat_all %>%
         ungroup() %>%
         filter(status == "native" & pp == "C4") %>%
         mutate(species = factor(species))
       
  nc4 <- inext(dat)  
  
  par(mfrow = c(2, 2))

# plot records
  nrec <- setValues(raster, nc4[[5]])
  plot(nrec)
# plot sr with warining cells removed
  plot(nc4[[7]])   
# plot raw sr
  plot(nc4[[8]])
       
  dat <- dat_all %>%
         ungroup() %>%
         filter(status == "native" & pp == "C3") %>%
         mutate(species = factor(species))
       
  nc3 <- inext(dat)     
     
     
  dat <- dat_all %>%
         ungroup() %>%
         filter(status == "nonnative" & pp == "C4") %>%
         mutate(species = factor(species))
       
  ec4 <- inext(dat)     

  dat <- dat_all %>%
         ungroup() %>%
         filter(status == "nonnative" & pp == "C3") %>%
         mutate(species = factor(species))
       
  ec3 <- inext(dat)     

  dat <- dat_all %>%
         ungroup() %>%
         filter(status == "native") %>%
         mutate(species = factor(species))
       
  allnat <- inext(dat)     

  dat <- dat_all %>%
         ungroup() %>%
         filter(status == "nonnative") %>%
         mutate(species = factor(species))
       
  allnonnative <- inext(dat)     


# -----------------------------------------------------------------------------
# plotting checks -------------------------------------------------------------
# all native and nonnative richness
  par(mfrow= c(2, 2))
  plot(allnat[[2]])
  plot(allnonnative[[2]])
    a <- cor.test(allnonnative[[1]], allnat[[1]], method = "spearman")
  plot(allnonnative[[1]] ~ allnat[[1]], main = round(a$estimate, 2))
  
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
# all native and nonnative richness
  par(mfrow= c(2, 2))
  plot(allnat[[7]])
  plot(allnonnative[[7]])
    a <- cor.test(allnonnative[[6]], allnat[[6]], method = "spearman")
  plot(allnonnative[[6]] ~ allnat[[6]], main = round(a$estimate, 2))
  
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

# save ----------------------------------------------------------------------------
# notes -----------------------------------------------------------------------------
# there are lists for each origin x pp
# these 8 lists are as follows: [[1]] df sr calculated by coverage - w warning cells
#                               [[2]] raster rr calculated by coverage - w warning cells
#                               [[3]] df sr calculated by size (i.e. 15-rec rarefaction)        #                               [[4]] raster sr calculated by size (i.e. 15-rec rarefaction)    
#                               [[5]] df number of records per cell
#                               [[6]] df coverage w warning cells removed
#                               [[7]] raster coverage w warning cells removed
#                               [[8]] raw species richness       
# ------------------------------------------------------------------------------------    

# all the info together 
  save(allnat, allnonnative, nc3, nc4, ec3, ec4, file = "Data files/rdata/iNEXT estimator metadata.RData")

# rasters ----------------------------------------------------------------------------
  par(mfrow = c(1, 1))
  
# save iNEXT rasters (15-record 0.8 coverage warning REMOVED [[7]])
  plot(allnat[[7]])  
  writeRaster(allnat[[7]], "Results/rasters/iNEXT/native_total.grd", overwrite = T)
  plot(nc3[[7]])
  writeRaster(nc3[[7]], "Results/rasters/iNEXT/native_C3.grd", overwrite = T)
  plot(nc4[[7]]) 
  writeRaster(nc4[[7]], "Results/rasters/iNEXT/native_C4.grd", overwrite = T)
  
  
  plot(allnonnative[[7]])  
  writeRaster(allnonnative[[7]], "Results/rasters/iNEXT/nonnative_total.grd", overwrite = T)
  plot(ec3[[7]])   
  writeRaster(ec3[[7]], "Results/rasters/iNEXT/nonnative_C3.grd", overwrite = T)
  plot(ec4[[7]]) 
  writeRaster(ec4[[7]], "Results/rasters/iNEXT/nonnative_C4.grd", overwrite = T)

# ---------------------------------------------------------------------------
       