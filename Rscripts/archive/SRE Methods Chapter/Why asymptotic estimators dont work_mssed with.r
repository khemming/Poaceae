


  library(raster)
  library(oz)
  library(tidyverse)
  library(ggmap)
  library(tidyr)

  
           
########################################
  rm(list = ls())
  
  setwd("C:/Users/s436862/Dropbox/Climate Matching/1. Data files")
  
# species records    
  poa <- read.csv("AVH/AVH grass records.csv", header = T)
  
# may not be necessary:

# look for potential duplicated herbarium records
# defined as the same species in the same location (lat and long to 2 decimal places) in the same year
#  dup <- paste(poa$species, round(poa$lat, 2), round(poa$long, 2), poa$year)
#  poa.nodup <- poa[duplicated(dup) == FALSE, ]
#  poa.nodup$species <- factor(poa.nodup$species)
#  poa <- poa.nodup
  
################################################################
# raster
  aus <- raster("australia raster/aus.grd")
  aus_100 <- aggregate(aus, fac = 100, fun = mean)
  values(aus_100) <- 1:ncell(aus_100)
  aus <- aus_100

# number of records by grid squares
  xy <- cbind(poa$long, poa$lat)

# assign each point in the dataframe to raster cells
  poa$cell <- raster:::extract(aus, xy)

################################################################################
# select the columns we need
  sp <- poa %>%
        select(species, status, year, cell)
  
# number of records per cell
  n.rec <- table(sp$cell)
  nr <- data.frame(cell = as.numeric(names(n.rec)), n.rec = as.vector(n.rec))
  
# add number of records per cell to the dataframe
  sp <- full_join(sp, nr)
  
# number of records per cell in order
  nr <- nr[order(nr$n.rec), ]
  tail(nr)
  
################################################################################
# function to compute asymptotic estimators for a given cell: chao1, ACE and cumulative number of species over time
# by ordering the records by year and then calculating a new value for the estimator
# each time a new record is added
# we can then see how the estimator changes over time / with the addition of new records
# and whether the values stabilise
# requires as input: x = list of species names, one for each record
#                    y = the year of each record

  # test
  # x <- sp$species
  # y <- sp$year
  chao1 <- function(x, y) {
    ord <- order(y)
    x <- x[ord]
    out <- numeric(length(x))
    acc <- out
    out.ace <- out
    for(i in 1:length(x)) {
      a <- x[1:i]
      b <- table(a)
      obs.rich <- length(b)
      c <- sum(b == 1)   # number of singletons
      d <- sum(b == 2)   # number of doubletons
      out[i] <- obs.rich + (c * (c - 1)) / (2 * (d + 1))  # f1 only chao estimater
      acc[i] <- obs.rich  # species accumulation over time
      
    # ACE estimator
      s.rare <- sum(b <= 10)
      s.abun <- sum(b > 10)
      n.rare <- sum(b[b <= 10])
      c.ace <- 1 - (c / n.rare)
      k.f <- sum(b[b <= 10] * (b[b <= 10] - 1))
      g2.ace <- max(((s.rare * k.f) / (c.ace * n.rare * (n.rare - 1))) - 1, 0)
      out.ace[i] <- s.abun + s.rare/c.ace + (c/c.ace)*g2.ace
    }
    return(list(out, out.ace, acc))
  }
  
  c = 0
  for(i in 1:10) {
    c <- i * (i - 1) * sum(b == i) + c
    }

# Right, so what did we just do ------------------------------
# choose a cell and look at the accumulation of species over time (red line) in that cell
# and how the Chao (black line) and ACE (blue line) estimators change accordingly
# for the asymptotic estimators to work, we expect all three lines to reach a stable asymptote
# after some number of records 
  inp <- poa %>%
         filter(cell == 1347) %>%
         select(species, year)
         
  sp <- as.character(inp$species)
  yr <- inp$year
  
# number of species
  length(table(sp))
# number of records
  length(sp)
  
  out <- chao1(sp, yr)
  
  plot(out[[1]], type = "l")
  lines(out[[2]], col = "blue")
  lines(out[[3]], col = "red")
  
  
 