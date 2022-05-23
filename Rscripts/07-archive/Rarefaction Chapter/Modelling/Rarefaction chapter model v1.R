# Date created: 3/8/18
# Updated: 

# Based on Rarefaction-EF script 
# Continuing from Rarefaction chapter data and EF variable selection scripts



# Aim ------------------------------------------------------------------------
# That is about the outcomes of the SRE methods, rarefaction data and envireonmental varaible section scripts: i.e. modelling int- and nat-EF relationships with the reduced set of EFs

# Build models linking rarefied richness of native and introduced grass species (and broken into C3 and C4 distributions for both) with environmental (climate and anhtropogenic) correlates 

# I'll run this generically for nat_tot_30, and then loop it or whatever, to be able to do all of them (3 x 8 x 2)


# Library ------------------------------------------------------------------
  library(RColorBrewer)
  library(raster)
  library(ggmap)
  library(tidyr)
  library(dplyr)
  library(rasterVis)
  library(maptools)
  library(ggthemes)
  library(ggcorrplot)
  library(corrplot)
  library(factoextra)
  library(car)
  library(glmnet)
  library(ncvreg)

  rm(list = ls())
  
  setwd("C:/Users/s436862/Dropbox/Rarefaction/4. Results/Rarefaction")
  
# Data ---------------------------------------------------------------------
# Species  
  nat <- read.csv("CSV/Native multiple cutoff COMPLETE.csv", header = T)
  int <- read.csv("CSV/Introduced multiple cutoff COMPLETE.csv", header = T)

# Note: will run through with native data, and then repeat for introduced.    
  #spp.raw <- nat
  #spp.raw <-int  
  
# Photosynthetic pathway
  #spp.raw <- spp.raw$C3.rare.50
  #spp.raw <- spp.raw$C4.rare.50

# Spp data wrangling ----------------------------------------------------------
# GLMnet function requires on NAs, and we need Australian cells only
# I did run it with them as zeroes, but now we're just gonna cut em out  
# Aus-only cells
  land.cat <- read.csv("C:/Users/s436862/Dropbox/Climate Matching/1. Data files/EFs/4. EFs complete/Terrestrial categories.csv", header = T)
  spp <- cbind(land.cat, spp.raw) %>% 
    filter(cell_category == "terrestrial") %>%
    select(-cell_category, -cell_id)

# ------------------------------------  
# NA = zero --------------------------- 
  #spp[is.na(spp)] <- 0 
  #sum(is.na(spp))      
  
  #spp <- as.matrix(spp)
# ------------------------------------

# Environmental variables ------------------------------------------------
# variable selection efs
  vs.efs <- c("arid",  "mat", "pwarmq", "pcoldq", 
                "th", "pewc",    "hii",     "ps")
  
  efs <- read.csv("C:/Users/s436862/Dropbox/Climate Matching/1. Data files/EFs/4. EFs complete/EF_COMPLETE.csv", header = T)%>%
    filter(cell_category == "terrestrial")    # Aus only cells
   
  
# remove same rows of Efs as spp
  spp.ef <- cbind(efs, spp)
  
# Remove rows with NAs
  spp.ef.na <- spp.ef[complete.cases(spp.ef), ] # note, will remove PEWC row I think, which might be nice
  sum(is.na(spp.ef.na))      
  
# sep spp and efs again
  spp2 <- select(spp.ef.na, spp.raw)
  spp <- as.matrix(spp2)
  
  efs2 <- select(spp.ef.na, vs.efs)
  
# check for NAs in efs  
  table(is.na(efs2) == T)
  
# If yes -----------------------------------------------------------------------------
# There is one NA, specifically, row 933, column 9 (PEWC) 
# Short-term solution: replace with mean
# Long-term solution: remove row (and spp. row too) OR hope PEWC is useless, and therefore we can leave it out and pretend this didn't happen 
# OR ask Richard...?
  
# NA --> mean(pewc)  
  # efs[is.na(efs)] <- mean(efs[,7], na.rm= T)
  # table(is.na(efs) == T) # score
# ------------------------------------------------------------------------------------
  
# standardise 
  ef.s <- data.frame(scale(efs2, center = T, scale = T)) 
  
# Required as matrix (spp can be whatever)
  ef.x <- as.matrix(ef.s) 

# VIF scores -------------------------------------------------------------------------------
# nat.c3.vif
  nat.c3 <- spp
  nat.c3.vif <- vif(lm(nat.c3 ~ ., data = ef.s))
  
# nat.c4.vif
  nat.c4 <- spp
  nat.c4.vif <- vif(lm(nat.c4 ~ ., data = ef.s))
  
# nat.c4.vif
  int.c3 <- spp
  int.c3.vif <- vif(lm(int.c3 ~ ., data = ef.s))
  
# nat.c4.vif
  int.c4 <- spp
  int.c4.vif <- vif(lm(int.c4 ~ ., data = ef.s))
  
#1:  arid     pwetq     pdryq    twarmq    tcoldq    pwarmq        th      pewc    pcoldq       hii 
#   23.090896 20.480740 13.403719  9.568859 15.991352  8.537623  2.121924  2.271777  6.917892  2.471118
  
#2:   
  vif(lm(nat.c3 ~ . , data = ef.s))
# arid     twarmq   tcoldq   pwarmq       th     pewc   pcoldq      hii 
# 9.843505 8.333467 6.180610 5.812766 2.106767 2.250453 6.754862 2.380766 
  
#3:  MAT not temperature extremes 
  vif(lm(nat.c3 ~ . -pwetq -pdryq -twarmq -tcoldq, data = ef.s))
#  arid      pwarmq       th     pewc   pcoldq      hii      mat 
# 7.670676 5.965647 2.080506 2.127582 6.451245 2.247952 4.504069 
# ---------------
  
  
# LASSO model fitting ----------------------------------------------------------------------
# Requires ---------------------------------------------------------------------------------
# x = ef data as matrix
# y = easiest in matrix as well
# Within GLMnet function, specify alpha:
# Alpha = 1 (default), glmnet applied LASSO penalty
# Alpha = 0, glmnet applied Ridge Regression penalty
# Alpha = 0-1, applies weighted score
# We'll use 1 
  
# (2) Family 
# normally distributed X variables = gaussian 

# (3) No NAs -- see above
  
# Model visualisations ------------------------------------------------------------------ 
# Function glmnet() returns models from each lambda parameter that has been applied. 
  my.glmnet.mod.a1 <- glmnet(ef.x, spp, family = "gaussian", alpha = 1)
 
# function print() to view and plot() to visualise  
  print(my.glmnet.mod.a1) # as lambda decreases %Dev (%age explained), also decreases 
  plot(my.glmnet.mod.a1)  # top no.s = no. of predictors are in model
                          # L1 norm = penality for model complexity (I think)
                          # coefficients = larger lambda, more have values different to zero
  

# We can look at the estimated coefficients for a specific model using function coef(), where s is the value of lambda.
# Lambda is the regularization parameter (??) which indicates how strongly we penalize the model complexity. Function glmnet() tries a whole array of lambda values; these can be specified by the user, but glmnet has a clever way to choose them (see the function documentation for details), so here we will stick to their default choice.
# For instance, for lambda = 0.02 we have:
  coef(my.glmnet.mod.a1, s = 0.02)
  
# Model selection ------------------------------------------------------------------
# Now, rather than having to choose a model ourselves, we can use functionality available in the package to guide this selection. 
# For each value of lambda, the dataset is split into training and testing data folds; models are fitted to the training data folds; their prediction ability is assessed using the test folds. With this, function cv.glmnet() generates a picture of how prediction ability changes with the degree of model complexity. We can then compare the prediction ability at different lambdas, and keep the model that is optimal. By default, cv.glmnet() uses deviance as a metric.

# Function cv.glmnet() uses cross-validation to do so (10-fold CV, by default):  
  my.glmnet.mod.a1.cv <- cv.glmnet(ef.x, spp, family = "gaussian", alpha = 1)
  # uses k-fold -- training and test data partitioning  
  
  plot(my.glmnet.mod.a1.cv)

# This is a cross-validation curve (red dotted line), showing the performance metric chosen (here deviance) with respect to lambda, together with error bars indicating +/- one standard deviation. Two selected lambdas are indicated by vertical dotted lines: 
# (1) 'lambda.min', which is the ?? at which deviance is minimized; 
# (2) 'lambda.1se', which is the largest value of ?? (i.e. most regularized model) such that deviance is within 1 standard error from the minimum. 
  
# We can view the selected lambdas like this:
  my.glmnet.mod.a1.cv$lambda.min  
  my.glmnet.mod.a1.cv$lambda.1se                                 
  
  coef(my.glmnet.mod.a1.cv, s = "lambda.min") 
 
#How does it compare with the model that corresponds to 'lambda.1se'?
  coef(my.glmnet.mod.a1.cv, s = "lambda.1se")
  # interesting - they're essentially the same
  
# AUC 
# As mentioned above, by default cv.glmnet() uses deviance as a performance metric, but other metrics can be used too. Check out the function documentation. A performance metric that is commonly used in species distribution modelling is the AUC (area under the ROC curve). This is a metric of discrimination ability (more about this in the model evaluation prac), and you would choose what performance metric to use for model selection, depending on what you wanted out of your model. You may want to try cv.glmnet() with AUC now:
  my.glmnet.mod.a1.cv.AUC <- cv.glmnet(ef.x, spp, family = "gaussian", alpha = 1, type.measure="auc")
  plot(my.glmnet.mod.a1.cv.AUC) # why does the x axis show this as MSE? Shouldn't it be AUC?
  my.glmnet.mod.a1.cv.AUC$lambda.min
  coef(my.glmnet.mod.a1.cv.AUC, s = "lambda.min") # really similar to the other lambda min 
  coef(my.glmnet.mod.a1.cv.AUC, s = "lambda.1se") # really similar to the other lambda 1se
  # ---> try and stick in the ;lambda here which correspnds to the highest R2 value?
  
# test AUC plot axis
 
# Calculate R2 ------------------------------------------------------------------
# first googled answer:  
# 1a) For lambda min
  r2.lmin <- fit$glmnet.fit$dev.ratio[which(fitnet$glmnet.fit$lambda == fitnet$lambda.min)]
# 1b) lambda se
  r2.lse <- fit$glmnet.fit$dev.ratio[which(fitnet$glmnet.fit$lambda == fitnet$lambda.1se)] 
  r2.lmin <- my.glmnet.mod.a1$glmnet.fit$dev.ratio[which(fitnet$glmnet.fit$lambda == fitnet$lambda.min)]

# 2nd
# their data
  cvfit <- cv.glmnet(X, y)
  rsq <- 1-cvfit$cvm/var(y)
  
# my data  
  my.glmnet.mod.a1 <- glmnet(ef.x, spp, family = "gaussian", alpha = 1)
  
  cvfit <- cv.glmnet(ef.x, spp)
  rsq <- 1 - cvfit$cvm/var(spp)
  
  plot(cvfit)
  
  cvfit <- cv.ncvreg(ef.x, spp, penalty="lasso", returnY = T)

  plot(cvfit, type="rsq")
  
# Save --------------------------------------------------------------------------
# Saving simplest model: lambda min using LASSO and %Dev
  
# Nat
#   m1.nat.c3.30 <- coef(my.glmnet.mod.a1.cv, s = "lambda.min") 
#   m2.nat.c4.30 <- coef(my.glmnet.mod.a1.cv, s = "lambda.min") 
# Int  
#   m1.int.c3.30 <- coef(my.glmnet.mod.a1.cv, s = "lambda.min") 
#   m2.int.c4.30 <- coef(my.glmnet.mod.a1.cv, s = "lambda.min") 
  
  nat.c3 <- matrix(m1.nat.c3.30)
  nat.c4 <- matrix(m2.nat.c4.30)
  int.c3 <- matrix(m1.int.c3.30)
  int.c4 <- matrix(m2.int.c4.30)
  
  rare.30.lasso.coefs <- cbind(nat.c3, int.c3, nat.c4, int.c4)
  
  rownames(rare.30.lasso.coefs) <- c("Intercept",   "arid",  "pwetq",  "pdryq", 
                                         "twarmq", "tcoldq", "pwarmq",   "th",   
                                           "pewc",    "hii")
  colnames(rare.30.lasso.coefs) <- c("nat.c3", "int.c3", "nat.c4", "int.c4")
                                      
  write.csv(rare.30.lasso.coefs, file = "CSV/Model LASSO Nat_int C3_4 rare_30 coefficients 6.8.18.csv")                               
# -------------------------------------------------------------------------------
  
  
  
  
  
  
# nat_c3, c4 for 30-rec SAC for mos timport varaible: let's go with pwetq
  nat.c3 <- spp$C3.rare.30
  ef.pwetq <- efs$pwetq
  plot(ef.pwetq, nat.c3)
  
  plot(efs$arid, nat.c3)
# Ok I don't understand the SAC curve Bockserger used    