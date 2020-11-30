
# Hierarchical Bayesian species distribution models: vignette (part 1), pilot analysis (part 2), Richard's model (part 3) & stacking for multiple species (part 4)

  library(raster)
  library(dplyr)
  library(UsingR) # for some good EDA type functions
  library(ggthemes)
  library(car)
  library(ggplot2)
  library(rgdal)
  library(tidyverse)
  library(ggmap)
  library(oz)
  library(hSDM)
  library(UsingR)
  library(ggcorrplot)
  library(corrplot)
  
  rm(list = ls())

  setwd("C:/Users/s436862/Dropbox/Climate matching/1. Data files")


# 1.0 Hierarchical Bayesian species distribution models with the hSDM R package package vignette 

# Chapter 2: Occurence data ------------------------------------------
# 1.1 Data generation -------------------------------------------------------
  data(altitude,package="hSDM") 
  alt.orig <- rasterFromXYZ(altitude) 
  extent(alt.orig) <- c(0,50,0,50) 
  plot(alt.orig) 

# Center and scale altitudinal data 
  alt <- scale(alt.orig,center=TRUE,scale=TRUE) 
  plot(alt)
  
# Target parameters 
  beta.target <- matrix(c(-1,1),ncol=1) 
  
# Matrix of covariates (including the intercept) 
  ncells <- ncell(alt) 
  X <- cbind(rep(1,ncells),values(alt)) 
  
# Probability of presence as a quadratic function of altitude  
  logit.theta <- X %*% 
    beta.target 
  theta <- inv.logit(logit.theta)
  
# Coordinates of raster cells 
  coords <- coordinates(alt) 

# Transform the probability of presence into a raster 
  theta <- rasterFromXYZ(cbind(coords,theta)) 

# Color palette for probability plots 
  colRP <- colorRampPalette(c("white","yellow","orange", "red","brown","black"))
  
# Plot the probability of presence 
  brks <- seq(0,1,length.out=100) 
  arg <- list(at=seq(0,1,length.out=5), 
              labels=c("0","0.25","0.5","0.75","1")) 
  
  nb <- length(brks)-1 
  plot(theta, main="Initial probabilities", 
                            col=colRP(nb), 
                            breaks=brks,
                            axis.args=arg,
                            zlim=c(0,1))

# So far I don't understand anything, but the plot looks really cool
  
# We can assume a number n of sites in the landscape where we have been able to observe or not the presence of the species.
# We can simulate the presence or absence of the species at these n sites given our model  (above plot)
  
# Number of observation sites 
  nsite <- 200 
# Set seed for repeatability 
  seed <- 1234  
  
# Sample the observations in the landscape 
  set.seed(seed) 
  x.coord <- runif(nsite,0,50) 
  set.seed(2*seed) 
  y.coord <- runif(nsite,0,50) 
  
  library(sp) 
  sites.sp <- SpatialPoints(coords=cbind(x.coord,y.coord)) 
  
# Extract altitude data for sites 
  alt.sites <- extract(alt,sites.sp) 
  
# Compute theta for these observations 
  X.sites <- cbind(rep(1,nsite),alt.sites) 
  logit.theta.site <- X.sites %*% 
    beta.target 
  
  theta.site <- inv.logit(logit.theta.site) 
  
# Simulate observations 
  visits <- rep(1,nsite) 
# One visit per site for the moment 
  set.seed(seed) 
  Y <- rbinom(nsite,visits,theta.site) 
# Group explicative and response variables in a data-frame 
  data.obs.df <- data.frame(Y,visits,alt=X.sites[,2]) 
  
# Transform observations in a spatial object 
  data.obs <- SpatialPointsDataFrame(coords=coordinates(sites.sp), data=data.obs.df)
  
# Plot observations 
  plot(alt.orig) 
  points(data.obs[data.obs$Y==1,],pch=16)  
  points(data.obs[data.obs$Y==0,],pch=1)
  # interesting ...
  
# 1.2 Parameter inference using the hSDM.binomial() function ----------------------------------
# The hSDM.binomial() function performs a Binomial logistic regression in a Bayesian framework.
# Before using this function we need to prepare a bit the data for predictions. 
# We want to have predictions on the whole landscape, not only at observation points. 
# To directly obtain these predictions, we can create a data frame including altitudinal data on the whole landscape. 
# This data frame will be used for the suitability.pred argument. 
# The data frame for predictions must include the same column names as those used in the formula for the suitability argument (i.e. "alt" our example).  
  
  data.pred <- data.frame(alt=values(alt))
  
# We can now call the hSDM.binomial() function. 
# Setting parameter save.p to 1, we can save in memory the MCMC values for predictions. 
# These values can be used to compute several statistics for each predictions (mean, median, 95% quantiles). 
  mod.hSDM.binomial <- hSDM.binomial(presences=data.obs$Y, 
                                     trials=data.obs$visits, 
                                     suitability=~alt,
                                     data=data.obs, 
                                     suitability.pred=data.pred, 
                                     burnin=1000, 
                                     mcmc=1000, 
                                     thin=1, 
                                     beta.start=0, 
                                     mubeta=0, 
                                     Vbeta=1.0E6, 
                                     seed=1234, 
                                     verbose=1, 
                                     save.p=1)
 
# 1.3 Analysis of the results -----------------------------------------------------
# The hSDM.binomial() function returns an MCMC (Markov chain Monte Carlo) for each parameter of the model and also for the model deviance. 
# To obtain parameter estimates, MCMC values can be summarized through a call to the summary() function from the 'coda'  package. 
# We can check that the values of the target parameters, ??0 = ???1 and ??1 = 1, are within the 95% confidence interval of the parameter estimates.  
  library(coda)
  coda::summary(mod.hSDM.binomial$mcmc)
  # well they are just lying to us then
  
  summary(mod.hSDM.binomial$mcmc)
  # did some stuff. Idk what
  
# Parameters estimates can be compared to results obtained with the glm() function.
#== glm results for comparison 
  mod.glm <- glm(cbind(Y, visits -Y) ~ alt, family = "binomial", data = data.obs) 
  summary(mod.glm)
  
  plot(mod.hSDM.binomial$mcmc) 
  # "Trace and density estimate for each variable of the MCMC", apparently
  
  
# The hSDM.binomial() function also returns two other objects. 
# The first one, theta.latent, is the predictive posterior mean of the latent variable ?? (the probability of presence) for each observation 
  str(mod.hSDM.binomial$theta.latent)
  summary(mod.hSDM.binomial$theta.latent)
  
  
# The second one, theta.pred is the set of sampled values from the predictive posterior
# (if parameter save.p is set to 1) or the predictive posterior mean (if save.p is set to 0) for each prediction. 
# In our example, save.p is set to 1 and theta.pred is an mcmc object. 
# Values in theta.pred can be used to plot the predicted probability of presence on the whole landscape and the uncertainty associated to predictions (below's output).  
  
# Create a raster for predictions 
  theta.pred.mean <- raster(theta) 
  
# Create rasters for uncertainty 
  theta.pred.2.5 <- theta.pred.97.5 <- raster(theta) 
  
# Attribute predicted values to raster cells 
  theta.pred.mean[] <- apply(mod.hSDM.binomial$theta.pred,2,mean) 
  theta.pred.2.5[] <- apply(mod.hSDM.binomial$theta.pred,2,quantile,0.025) 
  theta.pred.97.5[] <- apply(mod.hSDM.binomial$theta.pred,2,quantile,0.975) 
  
# Plot the predicted probability of presence and uncertainty 
  plot(theta.pred.mean,main="Mean",col=colRP(nb),breaks=brks, axis.args=arg,zlim=c(0,1))
  plot(theta.pred.2.5,main="Quantile 2.5 %",col=colRP(nb),breaks=brks, axis.args=arg,zlim=c(0,1))
  plot(theta.pred.97.5,main="Quantile 97.5 %",col=colRP(nb),breaks=brks, axis.args=arg,zlim=c(0,1))
  # Predicted probability of presence and uncertainty of predictions. 
  # Mean probability of presence (top), predictions at 2.5% quantile and 97.5% quantile can be plotted from the mcmc object plot.p.pred returned by function hSDM.binomial().
  
  
# In our example, we can compare the predictions to the initial probability of presence computed from our model to check that our predictions are correct (below one).

# Comparing predictions to initial values 
  plot(theta[],theta.pred.mean[],cex.lab=1.4,xlim=c(0,1),ylim=c(0,1)) 
  points(theta[],theta.pred.2.5[],cex.lab=1.4,col=grey(0.5)) 
  points(theta[],theta.pred.97.5[],cex.lab=1.4,col=grey(0.5)) 
  abline(a=0,b=1,col="red",lwd=2)
  # Predicted vs. initial probabilities of presence. 
  # Initial probabilities of presence are computed from the Binomial logistic regression model with target parameters
  
# 1.4 Site-occupancy model ----------------------------------------------------------
# 2.2.1 Mathematical formulation 
# Extensive notes in my written book; I 1/3 understand what's going on
  
# 2.2.2 Data generation
# To explore the characteristics of the hSDM.siteocc() function, we can generate a new virtual data-set on the basis of the site-occupancy model described above
  
# We draw at random the number of visits at each site of the previous example 
# Number of visits associated to each observation point 
  set.seed(seed) visits <- rpois(nsite,lambda=3) 
  
# Mean number of visits ~3 # NB: Setting a too low mean number of visits per site (lambda < 3) 
# leads to inaccurate parameter estimates 
  visits[visits==0] <- 1 # Number of visits must be > 0 
  # weeeeeeell with my herbarium data, I have no idea how many visits I do and do not have   
  
# Vector of observation sites 
  sites <- vector()
  for (i in 1:nsite) 
  { 
    sites <- c(sites,rep(i,visits[i]))
  }
  
# The survey conditions for each visit are determined by two explicative variables, w1 and the altitude (variable denoted A). 
# These two variables explain the observability of the species  
# yit ~ Bernoulli(zi??it) & logit(??it) = ??0 +??1w1it +??2Ait <- ??
  
# We fix the intercept and the effects of these two variables: 
# ??0 = ???1, ??1 = 1 and ??2 = ???1
# for determining the detection probability. 
# In our case, the detection probability decreases with altitude (??2 < 0).  
# Explicative variables for observation process 
  nobs <- sum(visits) 
  set.seed(seed) 
  w1 <- rnorm(n=nobs,0,1) 
  W <- cbind(rep(1, nobs), w1, X.sites[sites, 2]) 
  # observation process: error associated with false negs/pos etc.
  
# Target parameters for observation process 
  gamma.target <- matrix(c(-1,1,-1),ncol=1)
  
# Using covariates and parameters for the two processes, we compute the probability that the habitat is suitable (??i) and the species detection probability (??i). 
# We also draw the random variables zi and yi and construct the observation data-set
# Ecological process (suitability) 
  logit.theta.site <- X.sites %*% 
    beta.target 
  theta.site <- inv.logit(logit.theta.site) 
  set.seed(seed) 
  Z <- rbinom(nsite,1,theta.site)
  
# Observation process (detectability) 
  logit.delta.obs <- W %*% 
    gamma.target 
  delta.obs <- inv.logit(logit.delta.obs) 
  set.seed(seed) 
  Y <- rbinom(nobs,1,delta.obs*Z[sites])
  
# Data-sets 
  data.obs <- data.frame(Y, w1,alt = X.sites[sites, 2],site = sites) 
  data.suit <- data.frame(alt=X.sites[,2])
  # cool
  
# 2.2.3 Parameter inference using the hSDM.siteocc() function
# The hSDM.siteocc() function estimates the parameter of a site-occupancy model in a Bayesian framework.
  mod.hSDM.siteocc <- hSDM.siteocc(# Observations 
    presence = data.obs$Y, 
    observability = ~w1+alt, 
    site = data.obs$site, 
    data.observability = data.obs, 
    # Habitat 
    suitability=~alt, 
    data.suitability=data.suit, 
    # Predictions 
    suitability.pred=data.pred,
    # Chains 
    burnin=1000, 
    mcmc=1000, 
    thin=1, 
    # Starting values 
    beta.start=0, gamma.start=0, 
    # Priors 
    mubeta=0, 
    Vbeta=1.0E6, 
    mugamma=0, 
    Vgamma=1.0E6, 
    # Various 
    seed=1234, verbose=1, save.p=1) 
  
# 2.2.4 Analysis of the results
  summary(mod.hSDM.siteocc$mcmc) 
  
  plot(mod.hSDM.siteocc$mcmc)
  
  
  
  
  
  
  
# --------------------------------------------------------------------

# 2. Pilot analysis --------------------------------------------------
# 2.1 Generating dataframe -------------------------------------------
# Required: 
# cell no. | spp rec # | all rec # | a few EFS  
  rm(list = ls())
  
# species
  spp <- read.csv("1. Data files/AVH/AVH grass records.csv", header = T) %>%
    dplyr::filter(status == "introduced") %>%
    dplyr::select(species, lat, long)
  xy <- cbind(spp$long, spp$lat)
  
# Australia
  aus <- raster("1. Data files/Australia raster/aus_100km")
  values(aus) <- 1:ncell(aus)

# total number of records per cell (n)
  n_tot <- rasterize(xy, aus, fun = function(x,...) length(x)) 
  n_tot <- getValues(n_tot)
  
# EFs    
  EFs <- read.csv("C:/Users/s436862/Dropbox/Rarefaction/4. Results/Grass groups AVH/Rarefaction/CSV/EFs_2538.csv") %>%
              dplyr::select(mat, th, ap)
  mat <- EFs$mat
  th <- EFs$th
  ap <- EFs$ap
  
# cell_id  
  cell <- getValues(aus)
  df_1 <- cbind(cell, n_tot, EFs)
  
# add in one species' records
  spp_sum <- dplyr::count(spp, species)

# # filter for species <20 records [get a reference for this]    
#   spp_sum <- filter(spp_sum, n >= 20)
  
# pull out of spp
  austro <- filter(spp, species == "Austrostipa nitida")
  xy_a <- cbind(austro$long, austro$lat)
  
  n_spp <- rasterize(xy_a, aus, field = austro, fun = function(x,...) length(x)) 
  
  n_spp <- getValues(n_spp)
  n_spp1 <- n_spp[,"species"]
  n_spp1 <- data.frame(n_spp1)
  colnames(n_spp1) <- "n_spp"
  sum(n_spp1, na.rm = T)
  
# bind together  
  df2 <- cbind(cell, n_spp, n_tot, mat, ap, th)
  df2 <- data.frame(df2)
  df3 <- select(df2, -lat, -long)
  
# change possible offshore spp values to 0
  df3[is.na(df3$mat), 2:3] <- NA
  
# remove offshore rows 
  df4 <- df3[!is.na(df3$mat), ]
   
# Austro's & total spp NAs to 0
  df4[is.na(df4$species)] <- 0
  df4$n_tot[is.na(df4$n_tot)] <- 0
  sum(is.na(df4)) # 0 - cool
  # Note: I'm not sure how this works, but I will need to do this for all species (if I still want to make that massive DF), so will need to austomate that
  
  colnames(df4) <- c("cell", "n_spp", "n_tot", "temp", "precip", "top.het")
  
# save  
  write.csv(df4, "C:/Users/s436862/Dropbox/Climate Matching/4. Results/CSV/hSDM pilot dataframe.csv", row.names = F)  
  
# 2.2 Generating full-scale dataframe -------------------------------
# This might help:
  int.spp <- group_by(spp, species) %>%
    dplyr:::select(species)
  
# number of records per species
  int.nr  <- group_by(int.spp, species) %>%
    summarise(n_rec = n()) %>%
    arrange(-n_rec)
  
  overlap <- summarise(int.spp, n = n()) %>%
    spread(species, n) 
  
  print.data.frame(overlap) 
# --------------------------------------------------------------------  

    
# 3.0 Richard's fitting hSDM in JAGS script -------------------------
  library(tidyverse)
  library(jagsUI)
  library(ggmap)
  library(raster)
  
  
  setwd("C:/Users/s436862/Dropbox/Climate matching/4. Results")

  
# Data -------------------------------------------------------------    
  dat <- read.csv("CSV/hSDM pilot dataframe.csv")
  glimpse(dat)
  
  dat$pa <- ifelse(dat$n_spp > 0, 1, 0) # presence/absence points

# data for Jags model
  N <- nrow(dat)
  
# EF EDA 
  par(mfrow = c(2, 2))
  plot(temp ~ factor(pa), data = dat)
  plot(precip ~ factor(pa), data = dat)
  plot(top.het ~ factor(pa), data = dat)# all are skewed
  
# use temp and temp^2 to allow for a quadratic relationship
  temp <- scale(dat$temp)
  temp2 <- temp^2
  # interesting ...
  
# same for precip (and log-transform this because it is skewed)
  precip <- scale(log(dat$precip))
  precip2 <- precip^2
  
# log-transfrom heterogeneity because it's skewed
  top.het <- scale(log(dat$top.het + 1))
  
  par(mfrow = c(2, 2))
  plot(temp ~ factor(dat$pa))
  plot(precip ~ factor(dat$pa))
  plot(top.het ~ factor(dat$pa))
  # looking good
  
# JAGS model -----------------------------------------------------------------
  jags.mod <- "model {
  
  for(i in 1:N){
  z[i] ~ dbern(theta[i])
  logit(theta[i]) <- beta[1] + beta[2] * temp[i] + beta[3] * temp2[i] + beta[4] * precip[i] + beta[5] * precip2[i] + beta[6] * top.het[i]
  
  n_spp[i] ~ dbin(z[i] * p, n_tot[i])
  }
  
  p ~ dunif(0, 1)
  for(i in 1:6) {
  beta[i] ~ dnorm(0, 0.0001)
  }
  
  }"  #model finish

  
# Run model ------------------------------------------------
# notes: 
# investigate how jags function works; 
# what are these parameters? Are they hSDM things?
# parameters.to.save - what are you
# inits -- kinda get what they're up to  
  setwd("C:/Users/s436862/Dropbox/Rarefaction/4. Results/Grass groups AVH/Rarefaction/CSV")
  write(jags.mod, "jagsmod.txt")
  
  mod0 <- jags(model.file = "jagsmod.txt",
               data = list(N = N, n_spp = dat$n_spp, n_tot = dat$n_tot,
                           temp = temp, temp2 = temp2, precip = precip, precip2 = precip2, top.het = top.het),
               parameters.to.save = c("p", "beta", "theta"),
               inits = function() list(p = runif(1), z = dat$pa),
               n.chains = 3,
               n.iter = 5000,
               n.burnin = 1000,
               n.thin = 1,
               parallel = T)
  
  mod0
  mod.sum <- mod0$summary
  
# p is detection probability
# theta is estimated probability of presence
  out.theta <- mod.sum[substr(rownames(mod.sum), 1, 5) == "theta", 1]
  hist(out.theta, breaks = seq(0, 1, 0.1))
  abline(h = sum(dat$pa))
  
# make raster with presence absence values
  pa <- rep(NA, 47 * 54)
  pa[dat$cell] <- dat$pa
  
  aus2 <- aus2 <- raster("C:/Users/s436862/Dropbox/Climate Matching/1. Data files/Australia raster/aus_100km")
  aus2 <- setValues(aus2, pa)
  
  par(mfrow = c(1, 1))
  plot(aus2) # so this is actually where it occurs (AVH data)
  
# extract theta values
# match to cell and fill in the missing values
  theta <- rep(NA, 47 * 54)
  theta[dat$cell] <- mod.sum[substr(rownames(mod.sum), 1, 5) == "theta", 1]
  
  est_theta <- setValues(aus2, theta)
  plot(est_theta)
  
# be nice to overlay the observed point locations on top of this
  
# plot discrepancy between observed and predicted
  disc <- setValues(aus2, theta - pa)
  plot(disc)
  hist(disc)  
  
# -------------------------------------------------------------------------
  
# 4.0 Stacking all species -----------------------------------------------
# Aim----------------------------------------------------------------------
# To loop through any species and model it akin to above
# So, want I ultimately want is a DF with every individual species' presence/absence and est_theta. Or rasters. Whatever.
# I probably want this in terms of -- I have no idea what the best way to store this info to extract it later. Maybe separate ones: a collective of all and individual ones somewhere else
  
# I want to sum est_theta and PA. 
  
# Let's just do est_theta first. A sum of it
  
# How? Ok - no data cleaning; that will be a different script
# poa, where NA = sea; 0 = land (or spp_tot = land, or something)
# -> n_tot = (at least) 0
# sp <- filter(species == "")
# ->  n_spp 1, 2, 3, ... [again where NA = sea; 0 = land]
  

# 4.1 Generating data frame with all species' abundance (rec no.) for each cell ---------
# Introdcued species data
  int <- read.csv("AVH/AVH grass records.csv", header = T) %>%
    dplyr::filter(status == "introduced")
  glimpse(int)
  
# Aus @ appropriate scale
  aus <- raster("Australia/aus_100km.grd")
  values(aus) <- 1:ncell(aus)

# Generate cell id with record no.s for the cell    
# Lat/long  
  xy <- cbind(int$long, int$lat)
  
# Cell id (# and xy) for each species per cell 
  int$cell_id <- raster::extract(aus, xy)
  cell_id <- int$cell_id
  
# Cell ID of complete raster (2538) 
  cell_xy_2538 <- rasterToPoints(aus, spatial = F)
  colnames(cell_xy_2538) <- c("x", "y", "cell_id") 
  cell_xy_2538 <- data.frame(cell_xy_2538)
  
# Cell ID of Australia continent (i.e. no offshore island or ocean values; 1142) 
  oz1 <- raster("1. Data files/EFs/EFs cropped/arid")
  oz1 <- aggregate(oz1, fact = 100, fun = mean)
  oz_val <- rasterToPoints(oz1, spatial = F) # gave me my magic 1142 
  oz2 <- data.frame(oz_val)
  colnames(oz2) <- c("x", "y", "val")
  cell_xy_1142 <- data.frame(oz2)
  gh <- right_join(cell_xy_2538, oz2)
  cell_xy_1142 <- gh[,-4] 
  
# Join to introduced species records 
  int_spp <- right_join(int, cell_xy_1142) 
  
# Transpose to wide format -- tyvm, Andrew
  spp_cells1 <- dplyr::select(int_spp, cell_id, species) # took out x and y, btw
  spp_cells2 <- group_by(spp_cells1, cell_id, species) %>%
    summarise(n = n())
  spp_wide <- spread(spp_cells2, key = species, value = n, fill = 0)  
  
# Save 
  write.csv(spp_wide, "C:/Users/s436862/Dropbox/Climate Matching/4. Results/CSV/hSDM INT abundance.csv", row.names = F) 
  
# 4.2 Stacked hSDM -----------------------------------------------------------------
# How would I go about doing this? 
# (1) loop hSDM 'function' for all species 
# (2) individually save outputs (theta, pa and spr_pot) in an 'easily accessable' format to
# (3) stack them somewhere else
  
  
# Let's do this for one species and see where we end up  ----------------------------
# Dataframe: cell no. | spp rec # | all rec # | a few EFS 
# Int spp "abundance" data frame
  int <- read.csv("C:/Users/s436862/Dropbox/Climate Matching/4. Results/CSV/hSDM INT abundance.csv", header = T)

# # Find a species with a good amount of records (Or I could exclude species with, say, <20 records)
#   int_spp <- read.csv("AVH/AVH grass records.csv", header = T) %>%
#     dplyr::filter(status == "introduced") %>%
#     group_by(species) %>%
#     summarise(n_rec = n()) %>%
#     arrange(-n_rec)
 
# Australia
  aus <- raster("1. Data files/Australia raster/aus_100km")
  values(aus) <- 1:ncell(aus)
  
# total number of records per cell (n)
  n_tot <- rasterize(xy, aus, fun = function(x,...) length(x)) 
  n_tot <- getValues(n_tot)
  
# EF (no avh-spp values; 1139 cells)
  ef <- read.csv("C:/Users/s436862/Dropbox/Climate Matching/1. Data files/EFs/EFs aggregated/EF_1139.csv", header = T) %>%
    dplyr::select(cell_id, mat, th, ap)
  
  int_ef_42 <- left_join(int, ef)
  int_ef <- int_ef_42[!is.na(int_ef_42$ap), ]
  
# int_ef now has all the info for the model, but if that's a pain when I run the loop, I'll have a split copy, too
  int_39 <- dplyr::select(int_ef, -ap, -th, -mat)
  
  
# Seems havng two dataframes would be most useful? 
# (1) cell_no | n_tot | EFs 
# (2) spp. "A"-"Z"  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  

  
  
  
  
  n_bro$cell <- raster:::extract(aus, xy_a)
  n_cell_bro <- table(bro)
  
  spp[i] <- filter(int, species == "i")
  xy[i] <- cbind(spp[i]$long, spp[i]$lat)
  
  n_spp[i] <- rasterize(xy[i], aus, field = spp[i], fun = function(x,...) length(x)) 
  n_spp <- getValues(n_spp)
  n_spp1 <- n_spp[,"species"]
  n_spp1 <- data.frame(n_spp1)
  colnames(n_spp1) <- "n_spp"
  sum(n_spp1, na.rm = T)
  
# bind together  
  df2 <- cbind(cell, n_spp, n_tot, mat, ap, th)
  df2 <- data.frame(df2)
  df3 <- select(df2, -lat, -long)
  
# change possible offshore spp values to 0
  df3[is.na(df3$mat), 2:3] <- NA
  
# remove offshore rows 
  df4 <- df3[!is.na(df3$mat), ]
  
# Austro's & total spp NAs to 0
  df4[is.na(df4$species)] <- 0
  df4$n_tot[is.na(df4$n_tot)] <- 0
  sum(is.na(df4)) # 0 - cool
  # Note: I'm not sure how this works, but I will need to do this for all species (if I still want to make that massive DF), so will need to austomate that
  
  colnames(df4) <- c("cell", "n_spp", "n_tot", "temp", "precip", "top.het")
  
  # save  
  write.csv(df4, "C:/Users/s436862/Dropbox/Climate Matching/4. Results/CSV/hSDM pilot dataframe.csv", row.names = F)  
  
# -----------------------------------------------------------------------------  
# JAGS model -----------------------------------------------------------------
  jags.mod <- "model {
  
  for(i in 1:N){
  z[i] ~ dbern(theta[i])
  logit(theta[i]) <- beta[1] + beta[2] * temp[i] + beta[3] * temp2[i] + beta[4] * precip[i] + beta[5] * precip2[i] + beta[6] * top.het[i]
  
  n_spp[i] ~ dbin(z[i] * p, n_tot[i])
  }
  
  p ~ dunif(0, 1)
  for(i in 1:6) {
  beta[i] ~ dnorm(0, 0.0001)
  }
  
  }"  #model finish

  
# Run model ------------------------------------------------
# notes: 
# investigate how jags function works; 
# what are these parameters? Are they hSDM things?
# parameters.to.save - what are you
# inits -- kinda get what they're up to  
  setwd("C:/Users/s436862/Dropbox/Rarefaction/4. Results/Grass groups AVH/Rarefaction/CSV")
  write(jags.mod, "jagsmod.txt")
  
  mod0 <- jags(model.file = "jagsmod.txt",
               data = list(N = N, n_spp = dat$n_spp, n_tot = dat$n_tot,
                           temp = temp, temp2 = temp2, precip = precip, precip2 = precip2, top.het = top.het),
               parameters.to.save = c("p", "beta", "theta"),
               inits = function() list(p = runif(1), z = dat$pa),
               n.chains = 3,
               n.iter = 5000,
               n.burnin = 1000,
               n.thin = 1,
               parallel = T)
  
  mod0
  mod.sum <- mod0$summary
  
  # p is detection probability
  # theta is estimated probability of presence
  out.theta <- mod.sum[substr(rownames(mod.sum), 1, 5) == "theta", 1]
  hist(out.theta, breaks = seq(0, 1, 0.1))
  abline(h = sum(dat$pa))
  
  # make raster with presence absence values
  pa <- rep(NA, 47 * 54)
  pa[dat$cell] <- dat$pa
  
  aus2 <- aus2 <- raster("C:/Users/s436862/Dropbox/Climate Matching/1. Data files/Australia raster/aus_100km")
  aus2 <- setValues(aus2, pa)
  
  par(mfrow = c(1, 1))
  plot(aus2) # so this is actually where it occurs (AVH data)
  
  # extract theta values
  # match to cell and fill in the missing values
  theta <- rep(NA, 47 * 54)
  theta[dat$cell] <- mod.sum[substr(rownames(mod.sum), 1, 5) == "theta", 1]
  
  est_theta <- setValues(aus2, theta)
  plot(est_theta)
  
  # be nice to overlay the observed point locations on top of this
  
  # plot discrepancy between observed and predicted
  disc <- setValues(aus2, theta - pa)
  plot(disc)
  hist(disc)  
  
  
  
  
  
# -----------------------------------------------------------------------------  
  
