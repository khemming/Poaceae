
  library(ggplot2)
  library(ggmap)
  library(tidyr)
  library(raster)
  library(dplyr)
  
# read in elevation data (obtained from getData)
  elev <- getData('alt', country = "AUS")
  plot(elev)
  
  new.extent <- extent(c(110.8, 155.2, -46, -9.1))
  # note: changing ymax to a bigger value does not increase plot extent (?)
  elev <- crop(elev, new.extent)
  plot(elev)
  
  elev <- aggregate(elev, fact = 70)
  elev

########################################
# read in AVH grass record data
  poa <- read.csv("c:\\users\\s429217\\onedrive\\data\\grasses\\avh grass records.csv")
  dim(poa)
  glimpse(poa)
         
  poa.spp <- group_by(poa, species) %>%
             filter(duplicated(species) == F) %>%
             dplyr:::select(order, family, genus, species, year, status)
            
# number of records per species
  poa.nr  <- group_by(poa, species) %>%
             summarise(n_rec = n()) %>%
             arrange(-n_rec)
             

  overlap <- group_by(poa.spp, genus, status) %>%
             summarise(n = n()) %>%
             spread(status, n) %>%
             filter(is.na(native) == F & is.na(introduced) == F)
             
  print.data.frame(overlap) 
  
################################################################
# create a raster on the same scale as the elevation model
  aus2 <- raster(xmn = extent(elev)[1],
                 xmx = extent(elev)[2],
                 ymn = extent(elev)[3],
                 ymx = extent(elev)[4],
                 resolution = res(elev))

# number of records by grid squares
  xy <- cbind(poa$long, poa$lat)
  n_tot <- rasterize(xy, aus2, fun = function(x,...) length(x))
  plot(log10(n_tot))
  
# number of species by grid squares
  spp <- as.numeric(factor(poa$species))
  n_spp <- rasterize(xy, aus2, field = spp, fun = function(x,...) {length(unique(na.omit(x))) })
  plot(log10(n_spp))

# correct species richness using chao1 estimator
# number of singletons
  n_sing <- rasterize(xy, aus2, field = spp, fun = function(x,...) {length(which(table(x)==1)) })

# number of doubletons
  n_doub <- rasterize(xy, aus2, field = spp, fun = function(x,...) {length(which(table(x)==2)) })

# chao1 estimator
  chao <- n_spp + ((n_sing * (n_sing - 1)) / (2 * (n_doub + 1)))
  plot(log10(chao))
  
  plot(as.vector(chao) ~ as.vector(n_spp))
  abline(0, 1)

################################################################################

  poa.nat <- filter(poa, status == "native")

  nat_xy <- cbind(poa.nat$long, poa.nat$lat)
  nat_spp <- as.numeric(factor(poa.nat$species))
  nat_n_spp <- rasterize(nat_xy, aus2, field = nat_spp, fun = function(x,...) {length(unique(na.omit(x))) })
  nat_n_tot <- rasterize(nat_xy, aus2, fun = function(x,...) length(x))
  plot(log10(nat_n_spp))
# number of singletons
  nat_n_sing <- rasterize(nat_xy, aus2, field = nat_spp, fun = function(x,...) {length(which(table(x)==1)) })
# number of doubletons
  nat_n_doub <- rasterize(nat_xy, aus2, field = nat_spp, fun = function(x,...) {length(which(table(x)==2)) })
# chao1 estimator
  nat_chao <- nat_n_spp + ((nat_n_sing * (nat_n_sing - 1)) / (2 * (nat_n_doub + 1)))
  plot(log10(nat_chao))
  
  plot(as.vector(nat_chao) ~ as.vector(nat_n_spp))
  abline(0, 1)

  poa.int <- filter(poa, status == "introduced")

  int_xy <- cbind(poa.int$long, poa.int$lat)
  int_spp <- as.numeric(factor(poa.int$species))
  int_n_spp <- rasterize(int_xy, aus2, field = int_spp, fun = function(x,...) {length(unique(na.omit(x))) })
  plot(log10(int_n_spp))
# number of singletons
  int_n_sing <- rasterize(int_xy, aus2, field = int_spp, fun = function(x,...) {length(which(table(x)==1)) })
# number of doubletons
  int_n_doub <- rasterize(int_xy, aus2, field = int_spp, fun = function(x,...) {length(which(table(x)==2)) })
# chao1 estimator
  int_chao <- int_n_spp + ((int_n_sing * (int_n_sing - 1)) / (2 * (int_n_doub + 1)))
  plot(log10(int_chao))

  plot(as.vector(int_chao) ~ as.vector(int_n_spp))
  abline(0, 1)
  
# compare native and exotics with effort
  n <- as.vector(nat_chao) / as.vector(nat_n_spp)
  e <- as.vector(int_chao) / as.vector(int_n_spp)
  t.test(log(n), log(e))
  

  a <- stack(nat_chao, int_chao)
  layerStats(a, stat = "pearson", na.rm = T)
  
  b <- int_chao / (int_chao + nat_chao)
  plot(b)

# read in Australian BIOCLIM climate data
  clim <- read.table("C:\\Users\\s429217\\OneDrive\\data\\dung beetle\\data\\aus BIOCLIM climate.txt", header=T, sep="\t")

  clim_xy <- cbind(clim$long, clim$lat)
  clim_var <- clim$mean.temp
  out.var <- rasterize(clim_xy, aus2, field = clim_var, fun = function(x,...) {mean(x) })
  plot(out.var)
  
  cor.test(as.vector(b), as.vector(out.var))
  
# rank abundance curves

  par(mfrow = c(2, 1), mar = c(1, 5, 2, 1))
  a <- table(factor(poa.nat$species))
  a <- a[order(-a)]
  plot(log10(a), type = "l", xaxt = "n", ylab = "Log10(number of records)", main = "Native grasses",
       ylim = c(0, 3.5))

  a <- table(factor(poa.int$species))
  a <- a[order(-a)]
  plot(log10(a), type = "l", xaxt = "n", ylab = "Log10(number of records)", main = "Alien grasses",
      ylim = c(0, 3.5))
