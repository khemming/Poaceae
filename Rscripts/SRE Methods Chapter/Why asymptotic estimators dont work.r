
  library(raster)
  library(oz)
  library(tidyverse)
  library(ggmap)
  library(tidyr)

           
########################################
# read in AVH grass record data
  poa <- read.csv("c:\\users\\s429217\\onedrive\\data\\grasses\\avh grass records.csv")
  dim(poa)
  glimpse(poa)
  
# may not be necessary:

# look for potential duplicated herbarium records
# defined as the same species in the same location (lat and long to 2 decimal places) in the same year
#  dup <- paste(poa$species, round(poa$lat, 2), round(poa$long, 2), poa$year)
#  poa.nodup <- poa[duplicated(dup) == FALSE, ]
#  poa.nodup$species <- factor(poa.nodup$species)
#  poa <- poa.nodup
  
################################################################
# make a raster
  l1 <- floor(min(poa$long))
  l2 <- ceiling(max(poa$long))
  l3 <- floor(min(poa$lat))
  l4 <- ceiling(max(poa$lat))
  
  nr <- length(seq(l3, l4, by = 1))
  nc <- length(seq(l1, l2, by = 1))
  
  aus2 <- raster(nrows = nr, ncols = nc, 
                 xmn = l1, xmx = l2, 
                 ymn = l3, ymx = l4,
                 vals = 1:(nr*nc))

# number of records by grid squares
  xy <- cbind(poa$long, poa$lat)

# assign each point in the dataframe to raster cells
  poa$cell <- raster:::extract(aus2, xy)

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
      out[i] <- obs.rich + (c * (c - 1)) / (2 * (d + 1))  # chao estimater
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
  

################################################################################

# choose a cell and look at the accumulation of species over time (red line) in that cell
# and how the Chao (black line) and ACE (blue line) estimators change accordingly
# for the asymptotic estimators to work, we expect all three lines to reach a stable asymptote
# after some number of records 
  inp <- poa %>%
         filter(cell == 1499) %>%
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