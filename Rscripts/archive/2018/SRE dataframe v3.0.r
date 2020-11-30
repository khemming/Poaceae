######################### SRE dataframe ##################################
# Date created: 14/3/18
# Last updated: 30/5/18

# Updates from 2.1 ------------------------------------------------------
# adding in exotic species into rarefaction; therefore, will make rare a function you can just do on a spp dataframe. 
# Will check (but not do this) for the extrapolation SREs as well.
# -----------------------------------------------------------------------

# Overall aim: to test differences in native and exotic distributions using different species richness estimators (SREs), including extrapolation and interpolation methods, for use with herbarium data

# This script will collate all of the data into a dataframe of Australian cells 
# though the aim is to have this in a script so that (1) any land area can be entered (2) any species richness estimators (SREs) can be used

# Refer to previous versions of script, and if needed, similar 'dataframe'/'df' scripts in Old Scripts folder

# Next scripts: SRE EF EDA (which will include correlation analysis) & SRE plots (which will include correlation figures)

# Outcomes for this scripot:
# 1. Proportion of cells occupied: geographic bias -- a multiscale dataframe of record extent over australia
# 2 + 3. SRE dataframe for each native and exotic status using each SRE, and a few other indices
# plus 4. a species accumulation curve

  library(raster)
  library(tidyverse)
  library(ggmap)
  library(oz)

  rm(list = ls())
  
  setwd("C:/Users/s436862/Dropbox/Rarefaction/1. Data files")
  
# 1. Proportion of cells occupied: rarefaction EDA ---------------------
# Aim -----------------------------------------------------------------------------
# decide where the appropriate ranges are for cell width & rarefaction cutoff points
# These are opposing forces: we want the highest 'cutoff' point (giving the biggest range of relative species richness between cells), but the lowest % of cells excluded, to have a good coverage of Australia
  
# This script is split into two sections because of the aggregate function: first is the 1 km dataframe, and second the 2-250 km one, with their merger at the end
  
  rm(list = ls())
  
  setwd("C:/Users/s436862/Dropbox/Rarefaction/1. Data files")
  
# 1.1 Record coverage across scales -------------------------------------  
# Note: add in 'mean correlation between cells' record# and HII at each cell width  
# 1.2 1km width dataframe ----------------------------------------------
# Aims: ----------------------------------------------------------------
# (1) compute 1 km cell width   
# (2) compute proportion of cells occupied (>0 records) at each width
# (3) compute proportion of cells occupied at different cell-record cutoffs (>25, >50, >100,>250, >500 records) at each width
# (4) compute the correlation between average record number and HII score for each scale
# Dataframe generation ------------------------------------------------  
# HII raster (for record#-human correlation & to remove sea-related NA values)
  raster <- raster("EFs/EFs cropped/hii")
  
# Poa  
  spp <- read.csv("AVH/AVH grass records.csv", header = T)
  xy <- cbind(spp$long, spp$lat)
  
# number of records per cell (n)
  n <- rasterize(xy, raster, fun = function(x,...) {length(na.omit(x)) })
  n_val <- getValues(n)
  raster_val <- getValues(raster)
  n_rec_na <- data.frame(raster_val, n_val)
  n_rec_b <- n_rec_na[!is.na(raster_val), ] 
  n_rec <- n_rec_b[ ,2] # 9.9M cells. Wow
  
# convert NAs to zeroes
  n_rec[is.na(n_rec)] <- 0 
  sum(is.na(n_rec)) # beautiful
  
# community estimates
  cell_width <- 1
  total_cells <- length(n_rec)
  prop_occ <- sum(n_rec > 0) / total_cells * 100
  twenty_five <- sum(n_rec >= 25) / total_cells * 100
  fifty <- sum(n_rec >= 50) / total_cells * 100
  one_hundred <- sum(n_rec >= 100) / total_cells * 100
  two_fifty <- sum(n_rec >= 250) / total_cells * 100
  five_hundred <- sum(n_rec >= 500) / total_cells * 100 
  
# dataframe (8 cols)
  out_1km <- as.data.frame(cbind(cell_width, mean(n_rec), total_cells, prop_occ, twenty_five, fifty, one_hundred, two_fifty, five_hundred))
  
  colnames(out_1km) <- c("cell_width", "mean_rec", "total_cells", "all_records", "twenty_five", "fifty", "one_hundred", "two_fifty", "five_hundred")   
  # total: 8 + 1
  
# remove everything but output in environment to do next section
  rm(list=setdiff(ls(), "out_1km"))  
  
  
# 1.3 2-500 km width dataframe -----------------------------------------
# Required: -------------------------------------------------------------
# (1) cell widths: 2 km to where it asymptotes (100% occupied) -- 250 km for Poa has 99.51% coverage, using that  
# (2) compute proportion of cells occupied (>0 records) at each width
# (3) compute proportion of cells occupied at different cell-record cutoffs (>25, >50, >100,>250, >500 records) at each width
  rm(list = ls())
# raster template 
  raster <- raster("EFs/EFs cropped/arid.grd")
  
# Poa  
  spp <- read.csv("AVH/AVH grass records.csv", header = T)
  xy <- cbind(spp$long, spp$lat)
  
# test    
#width <- 100
  
# Cell-proportion function -------------------------
  prop <- function(width) 
  {
    
  # aggregate
    raster_agg <- aggregate(raster, fact = width, fun = mean)
    
  # number of records per cell (n)
    n <- rasterize(xy, raster_agg, fun = function(x,...) {length(na.omit(x)) })
    
  # exclude extra-terreestrial (NA) cells from dataframe
    b <- getValues(raster_agg)
    n_val <- getValues(n) 
    
    n_rec_na <- data.frame(b, n_val)
    n_rec_b <- n_rec_na[!is.na(b), ] 
    n_rec <- n_rec_b[ ,2]
    
  # convert NAs to zeroes
    n_rec[is.na(n_rec)] <- 0 
    sum(is.na(n_rec)) # beautiful
    
  # community estimates
    total_cells <- length(n_rec)
    prop_occ <- sum(n_rec > 0) / total_cells * 100
    twenty_five <- sum(n_rec >= 25) / total_cells * 100
    fifty <- sum(n_rec >= 50) / total_cells * 100
    one_hundred <- sum(n_rec >= 100) / total_cells * 100
    two_fifty <- sum(n_rec >= 250) / total_cells * 100
    five_hundred <- sum(n_rec >= 500) / total_cells * 100 
    
  # dataframe (8 cols)
    res <- as.data.frame(cbind(mean(n_rec), total_cells, prop_occ, twenty_five, fifty, one_hundred, two_fifty, five_hundred))                                                                                         
    return(res)                                                                    
    
  }  #finish function
  
# Output ----------------------------
# scale range (2 - 250 km)
  width <- c(2, 3, 4, 6, 8, 12, 16, 22, 32, 50, 75, 100, 125, 175, 250)
  
  out <- matrix(nrow = length(width), ncol = 9) # res cols +1
  
  for(i in 1:length(width)) 
  {
    out[i, 1] <- width[i]
    out[i, 2:9] <- as.numeric(prop(width[i]))
  }
  
  out_multi_km <- out
  
  colnames(out_multi_km) <- c("cell_width", "mean_rec", "total_cells", "all_records", "twenty_five", "fifty", "one_hundred", "two_fifty", "five_hundred")   
  # total: 8 + 1
  
# merge dataframes
  out_prop <- rbind(out_1km, out_multi_km)
  
# save 
  write.csv(out_prop, file = "C:/Users/s436862/Dropbox/Rarefaction/4. Results/Grass groups AVH/Rarefaction/CSV/Multiscale_rare_cell_occupation.csv", row.names = F)

# ---------------------------------------------------------------------    
  
  
# 2. Native & Introduced SRE dataframe & rasters ------------------------------------------
# 2.1 Rarefaction -----------------------------------------------------
# Here, I want to generate my native and exotic rasters (and then data frames) of species richness using the rarefaction method  
  
# required: 
# spp = species record list with lat/long
# raster = land area
# scale = scale by which raster will be converted to (assumed 1-km^2 raster and 10,000-km^2 scale)  
# n_min = rarefied richness value  
  rm(list = ls())
  
# species data  
  spp <- read.csv("AVH/AVH grass records.csv", header = T) %>%
    dplyr::select(species, lat, long, year, status) 
  
# raster with appropriate cell size and each cell has a value (=ID)  
  raster <- raster("Australia/aus_100km.grd")
  
# spp record lat/longs  
  xy <- cbind(spp$long, spp$lat)
  
# assign each point in the dataframe to raster cell
  spp$cell <- raster::extract(raster, xy)
  
# number of records per cell
  n_rec <- table(spp$cell)
  nr <- data.frame(cell = as.numeric(names(n_rec)), n_rec = as.vector(n_rec))
  
# add number of records, per cell, to the dataframe
# select the columns we need
  sp <- spp %>%
    select(species, status, year, cell)
  
# number of records per cell
  n_rec <- table(sp$cell)
  nr <- data.frame(cell = as.numeric(names(n_rec)), 
                   n_rec = as.vector(n_rec))
  
# add number of records per cell to the dataframe
  sp <- full_join(sp, nr)
  
# get a list of the cell numbers (that actually have records in them)
  cell_list <- as.numeric(levels(factor(sp$cell)))
  
# Rarefaction function ------------------------------------------------------
# Note: this is for using both Int and Nat that are both in the same data set; if you want to do it singularly, then refer to SRE df 3.0 script
  
# Calculate rarefaction estimate of number of species (Hurlbert, 1971)
# Requires as input: 
# sp = a vector of all the records with species names & cell_id
# status = a vector of status (native or introduced) for all records in a cell
# n = the number of samples for rarefaction
  rare <- function(sp, status, n) {
    N <- length(sp)            # number of records
    sp_n <- table(sp)          # number of records for each species
    
  # get the status for each species in alphabetical order: 1 = introduced, 0 = native
    a <- unique(cbind(sp, status))
    a <- a[order(a[, 1]), ]
    ne <- ifelse(a[, 2] == "introduced", 1, 0)
    
    out <- numeric(length(sp_n))  # vector to store estimate for each species
    
  # for each species, calculate the expected number of occurrences from n records
    for(i in 1:length(sp_n)) {
      out[i] <- 1 - exp(lchoose((N - sp_n[i]), n) - lchoose(N, n))    
      # use lchoose (i.e. log scale) to avoid problems with big numbers
    }
  # output estimated total richness, and estimated richness of introduced and native species
    return(c(round(sum(out)), round(sum(out[ne == 1])), round(sum(out[ne == 0]))))
  }
  
# Run function ----------------------------------------------------------
# matrix to store output: one row for each cell, 3 columns: 1 = total richness, 2 = introduced richness, 3 = native richness
  out_rare <- matrix(nrow = length(cell_list), ncol = 3)
  
# loop through all the cells, extract data from dataframe sp, and calculate rarefied richness using n_min as the number of samples for rarefaction
# for cells with less than or equal to n_min records, record as NA
  n_min <- 50
  
  for(j in 1:length(cell_list)) {
    cell <- filter(sp, cell == cell_list[j])
    if(cell$n_rec[1] <= n_min) out_rare[j] <- NA else {
      spp <- as.character(cell$species)
      status <- as.character(cell$status)
      out_rare[j, ] <- rare(spp, status, n_min)
    }
  }
  
# put the rarefaction estimates into the raster
# need to include the missing cell values as well as the occupied cells
# first make a matrix with all missing values
  x <- matrix(NA, nrow = length(getValues(raster)), ncol = 3)
# add the occupied cells
  x[cell_list, ] <- out_rare
# generate the raster object for total richness  
  est_rich <- setValues(raster, x[, 1])
  plot(est_rich)
  
# generate the raster object for introduced richness  
  est_rich_int <- setValues(raster, x[, 2])
  plot(est_rich_int)
  
# generate the raster object for native richness  
  est_rich_nat <- setValues(raster, x[, 3])
  plot(est_rich_nat)
  
# ----------------------------------------------------------------------  
# Generate rarefaction data frame:
# cell_id | nat_rare | int_rare
  cell_id <- getValues(raster)
  nat_rare <- getValues(est_rich_nat)
  int_rare <- getValues(est_rich_int)
  
  rare_df <- as.data.frame(cbind(cell_id, nat_rare, int_rare))
  
  write.csv(rare_df, file = "C:/Users/s436862/Dropbox/Rarefaction/4. Results/Rarefaction/CSV/Nat_Int rarefaction.csv")
  
# Keep only this bit (because otherwise a gives me weird SR values)
  rm(list=setdiff(ls(), "rare_df")) 
# ----------------------------------------------------------------------------------     
    
# 2.2 Extrapolation SREs ---------------------------------------------------------------
# Requirements ---------------------------------------------------------
# species
  spp <- read.csv("AVH/AVH grass records.csv", header = T) %>%
    dplyr::filter(status == "native")   %>%
    dplyr::select(species, lat, long) 

# aggregated Aus raster
  raster <- raster("australia raster/aus_100km.grd")
  
# number of records by grid squares
  xy <- cbind(spp$long, spp$lat)

# SRE calculations ---------------------------------------------------     
# total number of records per cell (n)
  n <- rasterize(xy, raster, fun = function(x,...) length(x))
# actual richness (a)
  sps <- as.numeric(factor(spp$species))
  a <- rasterize(xy, raster, field = sps, fun = function(x,...) {length(unique(na.omit(x))) })
# singletons, doubletons 
  f1 <- rasterize(xy, raster, field = sps, fun = function(x,...) {length(which(table(x)==1)) })
  f2 <- rasterize(xy, raster, field = sps, fun = function(x,...) {length(which(table(x)==2)) })
    
# ACE equations 
# rare species(<=10) per cell
  s_rare <- rasterize(xy, raster, field = sps, fun = function(x,...) {length(which(table(x)<11)) })
# abundant species (>10) per cell
  s_abun <- rasterize(xy, raster, field = sps, fun = function(x,...) {length(which(table(x)>10)) })
# number of rare individuals (records) per cell
  n_rare <- rasterize(xy, raster, field = sps, fun = function(x,...) {sum(table(x)[which(table(x)<=10)]) })
# validatation method (see below)* 
  n_comm <- rasterize(xy, raster, field = sps, fun = function(x,...) {sum(table(x)[which(table(x)>10)]) })
    
# kf = k*(k - 1)*fk part of the g2 eqn 
# need something like kf <- sum(n[n <= 10] * (n[n <= 10] - 1)) * sRare 
# let's see if doing this functionally equivalent in the rasterize function works
  kf <- rasterize(xy, raster, field = sps, fun = function(x,...) {sum((table(x)[which(table(x)<=10)])*(table(x)[which(table(x)<=10)] - 1)) })
  kf <- getValues(kf)
  sum(kf, na.rm = T) # seem legit
    
# get values
  s_rare <- getValues(s_rare)
  sum(s_rare, na.rm = T)
    
  s_abun <- getValues(s_abun)
  sum(s_abun, na.rm = T)
    
  n_rare <- getValues(n_rare)
  sum(n_rare, na.rm = T) 
  n_comm <- getValues(n_comm)
  sum(n_comm, na.rm = T)
  
# *n_rare & n_comm should sum to equal n 
  sum(n_comm + n_rare, na.rm = T) 
  # indeed 
    
# retrieve cell values of ther est of the SRE correctives ---------------------------------
# actual record no. + richness
  n <- getValues(n)
  a <- getValues(a)
# SRE correctives
  f1 <- getValues(f1)
  f2 <- getValues(f2)
    
# replace NAs with zeroes for SRE calculations
  raster_df <- getValues(raster)
  sre <- data.frame(raster_df, n, a, f1, f2, s_rare, s_abun, n_rare, kf, rarefied_rich) # note rarefaction is in here
  sre[is.na(sre$raster), 2:10] <- NA
  sre[is.na(sre)] <- 0
  sum(is.na(sre)) # cool
    
# split them back up
  n <- sre$n
  a <- sre$a
  f1 <- sre$f1
  f2 <- sre$f2
  s_rare <- sre$s_rare
  s_abun <- sre$s_abun
  n_rare <- sre$n_rare
  kf <- sre$kf
  rarefied_rich <- sre$rarefied_rich

# SRE calculations -------------------------------------------------------------------------
# For ACE: we have created thus far s_rare, s_abun, n_rare, kf from the rasterization process
# and so here we collect them into the ACE equation
  
# sample coverage    
  c_ace <- 1 - (f1 / n_rare)
    
# coefficient of variation (g2)
  g2a <- max(((s_rare * kf) / (c_ace * n_rare * (n_rare - 1))) - 1, 0, na.rm = T)
    
  g2b <- ((s_rare * kf) / (c_ace * n_rare * (n_rare - 1))) - 1
  g2b <- ifelse(g2b < 0, 0, g2b)
  
# ACE estimator 
  ace <- s_abun + s_rare/c_ace + (f1/c_ace * g2b)
    
# Chao1 
  chao1 <- ifelse(f2 == 0, 
          (a + (((n-1) / n) * (f1 * (f1-1)) / (2 * (f2+1)))),
          (a + (((n-1) / n) * ((f1^2)/(2*f2))))
                  ) # 'if' won't handle two conditional statements 
# Bootstrap
# unitl next time, buddy boy
    
# Jackknife second order 
    jack_second <- a + 2*f1 - f2
   

# 2.3 Save extrapolation and rarefaction as one ------------------------------
# before we can save, we need to separate the ocean NAs from the interior'a missing vlaues (zeroes) which are also currently NAs 
# we can do this with an EF that covers 100% of Aus
  arid <- raster("C:/Users/s436862/Dropbox/Rarefaction/1. Data files/EFs/EFs cropped/arid")
  arid <- aggregate(arid, fac = 100, fun = mean)
  arid <- getValues(arid)
  
# add cell column
  cell <- data.frame(1:2538)
  colnames(cell) <- "cell"  
  
# create sre dataframe
  sre <- cbind(cell, n, a, chao1, ace, jack_second, rarefaction, arid)  

# where arid = NA, we want the sre's to also be NA  
# (conviniently removes ocean and off-shore island cells)  
  sre[is.na(sre$arid), 2:7] <- NA
  
  
# (finally) save
  write.csv(sre, file = "C:/Users/s436862/Dropbox/Rarefaction/4. Results/Grass groups AVH/Rarefaction/CSV/sre.csv", row.names = F)
  

# ----------------------------------------------------------------------------------     
  
# 3. Native and Introduced SRE dataframe & rasters
# 3. Species accumulation curves (SAC) ----------------------------------------------
# produced from "Why assymptotic estimators don't work" and beerbayes No. 3.   
  
# First I wat to make a plot of all the cells' communitites -- SR x record no.
# (then circle 4x comms I want to check out, using PP)
# 4x communitites in a 'z' type shape; basically, two high-records cells, of similar SR; and two low-record cells, of similar SR, as well.  
  
  rm(list = ls())
  
  setwd("C:/Users/s436862/Dropbox/Rarefaction/1. Data files")
  
# species records    
  poa <- read.csv("AVH/AVH grass records.csv", header = T)
# raster  
  aus <- raster("australia raster/aus_100km.grd")

# number of records by grid squares
  xy <- cbind(poa$long, poa$lat)
  
# assign each point in the dataframe to raster cells
  poa$cell <- raster:::extract(aus, xy)
  
# select relevant columns
  sp <- poa %>%
    dplyr::select(species, status, year, cell)
  
# number of records per cell
  n_rec <- table(sp$cell)
  nr1 <- data.frame(cell = as.numeric(names(n_rec)), n_rec = as.vector(n_rec))
  
# add number of records per cell to the dataframe
  sp <- full_join(sp, nr1)
  sp_df <- sp
  
# number of records per cell in order
  nr <- nr1[order(nr1$n_rec), ]
  tail(nr)  
  
# Communities -------------------------------------------------- 
# which cells have high/low richness etc.
  grass_cell_summary <- sp %>% 
    group_by(cell) %>%
    summarise(sr = n_distinct(species), 
              rec_no = n())
  
# eyeball cells of interest
  ggplot(grass_cell_summary, aes(x = rec_no, y = sr)) + 
    geom_point() +
    theme_bw() +
    scale_x_continuous(limits = c(0,4500)) # exclude that massive cell cos it's too big
  #ggsave("C:/Users/s436862/Dropbox/Rarefaction/4. Results/Grass groups AVH/Rarefaction/Graphs/Poa SR x Rec no.jpeg", plot = last_plot(), scale = 1, dpi = 500, device = "jpeg")  
  
# highest community (a)    
  a_f <- filter(grass_cell_summary, between(sr, 280, 310) & between(rec_no, 3000, 5000))
  a_1347 <- a_f[1,]
# higher community (b)    
  b_f <- filter(grass_cell_summary, between(sr, 260, 300) & between(rec_no, 1500, 1900))
  b_690 <- b_f
# lower community (c)    
  c_f <- filter(grass_cell_summary, between(sr, 130, 170) & between(rec_no, 1500, 1750))
  c_1935 <- c_f[2,]
# lowest community (d)    
  d_f <- filter(grass_cell_summary, between(sr, 140, 160) & between(rec_no, 400, 480))
  d_1455 <- d_f[3,]
  
  comms1 <- bind_rows(a_1347, b_690, c_1935, d_1455)
  community <- c("hs", "hr", "lr", "ls")
  comms <- cbind(community, comms1) # cheaaa, buddy
  
  #ggplot(comms, aes(x = rec_no, y = sr))  +
    geom_point() # cool
  
# ---------------------------------------------------------------------------------------- 
# Species accumulatiom curve (SAC): Per record, richness increase ---------------------------------------  
  sac <- function(x, y) {
    ord <- order(y) # turn you into year
    x <- x[ord]
    chao <- numeric(length(x))
    accum <- out
    ace <- out
    jack <- out
    for(i in 1:length(x)) {
      a <- x[1:i] # not i, but cumulative; or something. Idk.
      b <- table(a)
      obs.rich <- length(b)
      f1 <- sum(b == 1)   
      f2 <- sum(b == 2) 
      
    # Chao1
      chao[i] <- ifelse(f2 == 0, 
                        (obs.rich + (f1 * (f1-1)) / (2 * (f2+1))),
                        (obs.rich + ((f1^2)/(2*f2))))
      
      accum[i] <- obs.rich  # species accumulation over time
      
    # Jack second
      jack[i] <- obs.rich + (2 * f1 - f2)
      
    # ACE estimator
      s.rare <- sum(b <= 10)
      s.abun <- sum(b > 10)
      n.rare <- sum(b[b <= 10])
      c.ace <- 1 - (f1 / n.rare)
      k.f <- sum(b[b <= 10] * (b[b <= 10] - 1))
      g2.ace <- max(((s.rare * k.f) / (c.ace * n.rare * (n.rare - 1))) - 1, 0)
      ace[i] <- s.abun + s.rare/c.ace + (f1/c.ace)*g2.ace
      
    }
    return(data.frame(idx = 1:length(out), 
                      chao,
                      ace, 
                      accum,
                      jack))
  }
  
# ---------------------------------------------------------------------------------------- 
# Run it for 4x comms 
# ---------------------------------------------------------------------------------------- 
# HT; cell 1347 ------------------------
  input <- poa %>%
    filter(cell == 1347) %>%
    select(species, year)
  
  sp <- as.character(input$species)
  yr <- input$year
  length(table(sp))
  length(sp)
  
  ht <- sac(sp, yr) # find out what you are, basically
  plot(ht[[5]], type = "l")
  lines(ht[[3]], col = "blue")
  lines(ht[[4]], col = "red") 
  lines(ht[[2]], col = "green")
  
  ht <- gather(ht, est, val, -idx)
  
  ggplot(ht, aes(x = idx, y = val, color = est)) +
    geom_line() +
    theme_bw() +
  labs(title = "Largest record no.",
       x = "Year",
       y = "Species richness",
       fill = "SRE") # not sure about how to change 'est' to SRE
  ggsave("C:/Users/s436862/Dropbox/Rarefaction/4. Results/Grass groups AVH/Rarefaction/Graphs/SAC_HT.jpeg", plot = last_plot(), scale = 1, dpi = 500, device = "jpeg")  
  
# HR; cell 690 ------------------------
  input <- poa %>%
    filter(cell == 690) %>%
    select(species, year)
  
  sp <- as.character(input$species)
  yr <- inp$year
  length(table(sp))
  length(sp)
  
  hr <- sac(sp, yr) # find out what you are, basically
  plot(hr[[5]], type = "l")
  lines(hr[[3]], col = "blue")
  lines(hr[[4]], col = "red") 
  lines(hr[[2]], col = "green") # das ist Jack, ja
  
  hr <- gather(hr, est, val, -idx)
  
  ggplot(hr, aes(x = idx, y = val, color = est)) +
    geom_line() +
    theme_bw() +
    labs(title = "Second largest record no.",
         x = "Year",
         y = "Species richness",
         fill = "SRE") # not sure about how to change 'est' to SRE
  ggsave("C:/Users/s436862/Dropbox/Rarefaction/4. Results/Grass groups AVH/Rarefaction/Graphs/SAC_HR.jpeg", plot = last_plot(), scale = 1, dpi = 500, device = "jpeg")  
  
  
# LR; cell 1935 ------------------------
  input <- poa %>%
    filter(cell == 1935) %>%
    select(species, year)
  
  sp <- as.character(input$species)
  yr <- inp$year
  length(table(sp))
  length(sp)
  
  lr <- sac(sp, yr) # find out what you are, basically
  plot(lr[[5]], type = "l")
  lines(lr[[3]], col = "blue")
  lines(lr[[4]], col = "red") 
  lines(lr[[2]], col = "green") # das ist Jack, ja
  
  lr <- gather(lr, est, val, -idx)
  
  ggplot(lr, aes(x = idx, y = val, color = est)) +
    geom_line() +
    theme_bw() +
    labs(title = "Third largest record no.",
         x = "Year",
         y = "Species richness",
         fill = "SRE") # not sure about how to change 'est' to SRE
  ggsave("C:/Users/s436862/Dropbox/Rarefaction/4. Results/Grass groups AVH/Rarefaction/Graphs/SAC_LR.jpeg", plot = last_plot(), scale = 1, dpi = 500, device = "jpeg")  
# LT; cell 1455 ------------------------
  input <- poa %>%
    filter(cell == 1455) %>%
    select(species, year)
  
  sp <- as.character(input$species)
  yr <- inp$year
  length(table(sp))
  length(sp)
  
  lt <- sac(sp, yr) # find out what you are, basically
  plot(lt[[5]], type = "l")
  lines(lt[[3]], col = "blue")
  lines(lt[[4]], col = "red") 
  lines(lt[[2]], col = "green") # das ist Jack, ja
  
  lt <- gather(lt, est, val, -idx)
  
  ggplot(lt, aes(x = idx, y = val, color = est)) +
    geom_line() +
    theme_bw() +
    labs(title = "Smallest record no.",
         x = "Year",
         y = "Species richness",
         fill = "SRE") # not sure about how to change 'est' to SRE
  ggsave("C:/Users/s436862/Dropbox/Rarefaction/4. Results/Grass groups AVH/Rarefaction/Graphs/SAC_LT.jpeg", plot = last_plot(), scale = 1, dpi = 500, device = "jpeg")    

# Ratio of f1:f2 across Australia raster -----------------------------------------
# let's make two things:
# (1) raster of ratio of f1:f2;
# (2) correlation between ratio & record no.
#     would this work? I am not sure. Maybe not as a raster, but the values, surely  
  
  library(car)
  
  setwd("C:/Users/s436862/Dropbox/Rarefaction/1. Data files")
  
# data  
  poa <- read.csv("AVH/AVH grass records.csv", header = T)
  xy <- cbind(poa$long, poa$lat)
  spp <- as.numeric(factor(poa$species))
  raster <- raster("Australia raster/aus_100km")
  
# total number of records per cell (n)
  n <- rasterize(xy, raster, fun = function(x,...) {length(na.omit(x)) })
# actual richness (a)
  a <- rasterize(xy, raster, field = sps, fun = function(x,...) {length(unique(na.omit(x))) })
  
# single- & double-tons
  f1 <- rasterize(xy, raster, field = sps, fun = function(x,...) {length(which(table(x)==1)) })
  plot(f1)
  f2 <- rasterize(xy, raster, field = sps, fun = function(x,...) {length(which(table(x)==2)) })
  plot(f2)
  
# plot -- overlay, my friend, my enemy.  
  ratio <- overlay(f1, f2, fun = function(x, y) {return (x/y)})
  plot(ratio)
  
  writeRaster(ratio, "C:/Users/s436862/Dropbox/Rarefaction/4. Results/Grass groups AVH/Rarefaction/Raster/F1-F2 ratio", overwrite = T)
  
  arid <- raster("C:/Users/s436862/Dropbox/Rarefaction/1. Data files/EFs/EFs cropped/arid")
  
  a_v <- getValues(a)
  ratio_v <- getValues(ratio)
  a_ra <- cbind(a_v, ratio_v)
  
  a_ratio_cor <- cor(a_ra, method = "pearson", use = "complete.obs")
  cor(ef, method = "pearson", use ="complete.obs") # not sure why you're not playing ball
  
  
  
# 4. Extras ######################################
# Poa SR at 50- and 200-km scales --------------------------------------------
  setwd("C:/Users/s436862/Dropbox/Rarefaction/1. Data files")
  
  poa <- read.csv("AVH/AVH grass records.csv", header = T)
  xy <- cbind(poa$long, poa$lat) 
  spp <- as.numeric(factor(poa$species))
  
  aus <- raster("Australia raster/aus")
  
# 50-km  
  width <- 50 
  aus1 <- aggregate(aus, fac = width, fun = mean)
  poa_50 <- rasterize(xy, aus1, field = sps, fun = function(x,...) {length(unique(na.omit(x))) })
  plot(poa_50)
  writeRaster(poa_50,"C:/Users/s436862/Dropbox/Rarefaction/4. Results/Grass groups AVH/Rarefaction/Raster/Poa_50km", overwrite = T)
  
# 200-km  
  width <- 200 
  aus2 <- aggregate(aus, fac = width, fun = mean)
  poa_200 <- rasterize(xy, aus2, field = sps, fun = function(x,...) {length(unique(na.omit(x))) })
  plot(poa_200)
  writeRaster(poa_200,"C:/Users/s436862/Dropbox/Rarefaction/4. Results/Grass groups AVH/Rarefaction/Raster/Poa_200km", overwrite = T)  
  
  
# ----------------------------------------------------------------------------------------    
  
  
  
  