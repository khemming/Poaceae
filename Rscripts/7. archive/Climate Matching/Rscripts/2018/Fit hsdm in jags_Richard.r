
  library(tidyverse)
  library(jagsUI)
  library(ggmap)
  library(raster)


  setwd("c:\\users\\s429217\\onedrive\\pgrad\\kyle hemming\\sdm")
  
  dat <- read.csv("hSDM pilot dataframe.csv")
  glimpse(dat)
  
  dat$pa <- ifelse(dat$n_spp > 0, 1, 0)
  
  par(mfrow = c(2, 2))
  plot(temp ~ factor(pa), data = dat)
  plot(precip ~ factor(pa), data = dat)
  plot(top.het ~ factor(pa), data = dat)
  
# data for Jags model
  N <- nrow(dat)
  
# use temp and temp^2 to allow for a quadratic relationship
  temp <- scale(dat$temp)
  temp2 <- temp^2
  
# same for precip (and log-transform this because it is skewed)
  precip <- scale(log(dat$precip))
  precip2 <- precip^2
  
# log-transfrom heterogeneity because it's skewed
  top.het <- scale(log(dat$top.het + 1))
  
  par(mfrow = c(2, 2))
  plot(temp ~ factor(dat$pa))
  plot(precip ~ factor(dat$pa))
  plot(top.het ~ factor(dat$pa))

################################################################################
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


#RUN MODEL------------------------------------------------

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


  aus2 <- raster(nrows = 47, ncols = 54,
                 xmn = 110.8, xmx = 155.8,
                 ymn = -46.26667, ymx = -7.1,
                 vals = pa)
                 
  par(mfrow = c(1, 1))
  plot(aus2)
                 
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
  

