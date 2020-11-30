
# Date created: 26/6/18
# Last updated: 25/7


# EF variable selection ------------------------------------------------------------------
# Because these outputs are EF-driven, this is going in Climate matching
# (But see Rarefaction METADATA for more info)

# Aim ------------------------------------------------------------------------------------
# The aim of this script is to come up with a set of environmental variables that satisfy the ecological drivers of what we think are driving grass species in Australia.

# There are two parts to this process. This script accounts for the first part.

# Using ecological reasoning, PCA and correlation tests to turn the ~22 EFs to a 'happy' dozen.
# We want to account for the main drivers in temperature, precipitation, seasonality, drought. topographic heterogeneity, etc., as well as human drivers, but reduce the amount of EFs on account of strong correlations between these key indices.

# The second part will be in the Rarefaction-EF modelling script, which will then model the native/introduced x C3/C4 species groups, employing ridge regression/LASSO variable selection methods derived from the PR stats code. That's not included because it also models the species, which I don't want to do here. And we could use this for the climate matching and SRE methods chapters.

# Library ---------------------------------------------------------------------
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
  
  rm(list = ls())
  
  setwd("C:/Users/s436862/Dropbox/Climate Matching")
  
# 1. Data -------------------------------------------------
# Environmental factors (EFs)
  ef <- read.csv("1. Data files/EFs/4. EFs complete/EF_COMPLETE.csv", header = T) %>%
    filter(cell_category == "terrestrial") %>% # Aus-only cells
    select(-glu, -cell_id, -cell_category) # global land use not useful
  
# standardise  
  ef.s <- data.frame(scale(ef, center = T, scale = T)) 

# Use only the variable selected ones 
  ef.red <- select(ef.s, pwetq, pcoldq, ts, arid, mat, th, pewc, hii)
  
  
# correlation matrices  
  cm <- cor(ef.s, method = "pearson", use = "complete.obs")
  cm.vs <- cor(ef.red, method = "pearson", use = "complete.obs")
  
  
# Covaration matrix 1 ------------------------------------------------------ 
  
  
  corrplot(cm, type = "lower", order = "hclust", tl.col = "black", tl.srt = 45)
  
  ggcorrplot(cm, hc.order = TRUE, type = "upper",
             colors = c("blue", "white", "red"),
             lab = TRUE)
  
  corrplot.mixed(cm, method = "square")
  
  cor_pmat(cm)
  
# Correlation matrix 2 ----------------------------------------------------
  col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
  
  jpeg(filename = "C:/Users/s436862/Dropbox/Climate matching/4. Results/Graphs/Correlations/EF correlation matrix_test1.jpeg", width = 15, height = 15, units = 'cm', res = 500)
   
  col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
  
  corrplot(cm, method = "color", col = col(200),
           type = "upper", order = "hclust", number.cex = .7,
           addCoef.col = "black", # Add coefficient of correlation
           tl.col = "black", tl.srt = 45, # Text label color and rotation
           number.digits = 2,
           # Combine with significance
           #p.mat = p.mat, sig.level = 0.01, insig = "blank", 
           # hide correlation coefficient on the principal diagonal
           diag = FALSE)
  
  corrplot(cm, method="color", col=col(200),  
           type="upper", order="hclust", 
           addCoef.col = "black", # Add coefficient of correlation
           addCoefasPercent = T, # Add coefficient of correlation
           tl.col="black", tl.srt=45, #Text label color and rotation
           number.digits = 2,
           diag=FALSE)
   
  
  corrplot.mixed(cm,
                 lower.col = "black", 
                 number.cex = .5, # for correlation numbers
                 upper =  "circle",
                 title = "Environmental variables",
                 cl.ratio = 0.15, # width of legend 
                 tl.cex = 0.5, # variable names
                 cl.cex = 1,   # legend numbers
                 mar=c(0,0,1,0))
  
   dev.off()
# Correlation matrix 3 ---------------------------------------------------
   jpeg(filename = "C:/Users/s436862/Dropbox/Climate matching/4. Results/Graphs/Correlations/All EF correlation matrix.jpeg", width = 20, height = 20, units = 'cm', res = 500)
   
   col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
   corrplot(cm, method = "color", col = col(200),  
            type = "upper", order = "hclust", 
            addCoef.col = "black", # Add coefficient of correlation
            tl.col = "black", tl.srt = 45, #Text label color and rotation
            diag = FALSE, # hide correlation coefficient on the principal diagonal
            title = "Environmental variables",
            number.cex = .5, # for correlation numbers
            cl.ratio = 0.15, # width of legend 
            tl.cex = 0.7, # variable names
            cl.cex = 1,   # legend numbers
            mar=c(0,0,1,0))
   dev.off()
   
   jpeg(filename = "C:/Users/s436862/Dropbox/Climate matching/4. Results/Graphs/Correlations/Variable selection EF correlation matrix.jpeg", width = 15, height = 15, units = 'cm', res = 500)
   
   col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
   corrplot(cm.vs, method = "color", col = col(200),  
            type = "upper", order = "hclust", 
            addCoef.col = "black", # Add coefficient of correlation
            tl.col = "black", tl.srt = 45, #Text label color and rotation
            diag = FALSE, # hide correlation coefficient on the principal diagonal
            title = "Reduced set of environmental variables",
            number.cex = 1, # for correlation numbers
            cl.ratio = 0.2, # width of legend 
            tl.cex = 1.3, # variable names
            cl.cex = 1,   # legend numbers
            mar=c(0,0,1,0))
 
   dev.off()
   
# Save ------------------------------------------------------------------- 
  # write.csv(cm, file = "C:/Users/s436862/Dropbox/Climate matching/4. Results/CSV/EF complete correlation matrix.csv", row.names = T)
  
  # jpeg(filename = "C:/Users/s436862/Dropbox/Climate matching/4. Results/Graphs/Correlations/EF correlation matrix_test1.jpeg", width = 15, height = 15, units = 'cm', res = 500)
  # corplt <- corrplot(cm, type = "lower", order = "hclust", tl.col = "black", tl.srt = 45)
  # dev.off()
  # think corrplot is nicer; check other stuff you can do with it on: https://cran.r-project.org/web/packages/corrplot/vignettes/corrplot-intro.html
  
  

# PCA -------------------------------------------------------------------------------
# Using prcomp() and princomp() functions form base R
# refer to: http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/118-principal-component-analysis-in-r-prcomp-vs-princomp/
  pca_all <- prcomp(na.omit(ef.s), scale = F, center = F)
 
# visualise (% of variance explained by each principle component - each 'dimension')  
  fviz_eig(pca_all) 
  
  summary(pca_all) # componant 1 explains 39% of variance -- each component is a dimension. This doesn't concern me unless I want to name and use these dimensions (e.g. dimension one could be 'hot and warm' climate)
  
# let's see what this does if I choose the first four components
  comp <- data.frame(pca_all$x[,1:4])
  plot(comp, pch=16, col=rgb(0,0,0,0.5)) # think this is how data are distriubted along each dimension in 2-D space.. (Maaaaybe??)
  
# Ok so now we're gonna look at the 4-D stuff in 3-D (?) -- again, dunno what this stuff is
  library(rgl)
  
# Multi 3D plot -- for the lols
  plot3d(comp$PC1, comp$PC2, comp$PC3)
  plot3d(comp$PC1, comp$PC3, comp$PC4) 
  
# graphing each cell 
# (note I scaled and centred before because otherwise it looked super weird)
  fviz_pca_ind(pca_all,
               col.ind = "cos2" # Color by the quality of representation
  )
  
# graphing each EF 
  fviz_pca_var(pca_all,
               col.var = "contrib", # Color by contributions to the PC
               gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
               repel = TRUE # Avoid text overlapping
  )
  
# save that
  jpeg(filename = "C:/Users/s436862/Dropbox/Climate Matching/4. Results/Graphs/EF PCA_all.jpeg", width = 16, height = 12, units = 'cm', res = 500)
  fviz_pca_var(pca_all,
               col.var = "contrib", # Color by contributions to the PC
               gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
               repel = TRUE # Avoid text overlapping
  )
  dev.off()
  
# Assess to the PCA results -----------------------------------------   
# Eigenvalues
  eig.val <- get_eigenvalue(pca_all)
  eig.val
  
# Results for variables
  res.var <- get_pca_var(pca_all)
  res.var$coord    # Coordinates
  res.var$contrib  # Contributions to the PCs
  res.var$cos2     # Quality of representation 
  
  # No idea what any of that does
  
# 2. EF reduction -------------------------------------------------------------------
# There are a lot of EFs, and only so many can get in there and make any difference. SO to do that today, I am going to change a few things up looking at the PCA first of all.
  
# Looking at it, I'vre attempted to reduce EFs who are next to each other, and choose those which seem most relevant ecologically, if it is ambiguous. Let's find out who got through to the next round.
  
# 2.1 Reduction via PCA -------------------------------------------------------------
# reduced EF.s
  ef.s.red <- dplyr::select(ef.s, ap, iso, tcoldm, mat, twarmm, pcoldq, hii, th, pewc, elev, ts)
  
  a1 <- cor(ef.s.red, method = "pearson", use = "complete.obs")
  
  write.csv(a1, file = "4. Results/CSV/Reduced EF correlation matrix.csv")
  
# corrplot method  
  corrplot(a1, type = "lower", order = "hclust", tl.col = "black", tl.srt = 45)
  # there are still heaps of crrelations but that's cool
  
  jpeg(filename = "C:/Users/s436862/Dropbox/Climate matching/4. Results/Graphs/Reduced EF correlation matrix.jpeg", width = 10, height = 12, units = 'cm', res = 500)
  
  corplt <- corrplot(a1, type = "lower", order = "hclust", tl.col = "black", tl.srt = 45)
  
  dev.off()
  
  # think corrplot is nicer; check other stuff you can do with it on: https://cran.r-project.org/web/packages/corrplot/vignettes/corrplot-intro.html
  

# pca-reduction 1: the visual inspection
  pca_red1 <- prcomp(na.omit(ef.s.red), scale = T, center = T)
  
  
# visualise (% of variance explained by each principle component)  
  fviz_eig(pca_red1) # not sure what this tells me
  
# graphing each cell 
# (note I scaled and centred before because otherwise it looked super weird)
  fviz_pca_ind(pca_red1,
               col.ind = "cos2" # Color by the quality of representation
  )
  
# graphing each EF 
  fviz_pca_var(pca_red1,
               col.var = "contrib", # Color by contributions to the PC
               gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
               repel = TRUE # Avoid text overlapping
  )
  
# save that
  jpeg(filename = "C:/Users/s436862/Dropbox/Climate Matching/4. Results/Graphs/Reduced EF PCA.jpeg", width = 16, height = 12, units = 'cm', res = 500)
  fviz_pca_var(pca_red1,
               col.var = "contrib", # Color by contributions to the PC
               gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
               repel = TRUE # Avoid text overlapping
  )
  dev.off()
  
# 2.2 Further reduction via correlations ---------------------------------------
# I have chosen ap, pcoldq, tawrmm, tcoldm, pewc, th, hii
# these represent a range of 'important' temperature and precipitation vairables, which at the same time have the least amount of correlation betwen them
# Refer to notes in Rarefaction document v.5.0.1 for the specifics  



# 3. LASSO and ridge regression variable section  ----------------------------
# Derived from PR Statistics (PR SDM script), Practical 2 (supplmentary) -----
# As in, I am working from these, so go there for more info.
# The aim of this section is to produce models that I can compare with 1. 
# At this stage, I am not sure whether I will use all these methods, just one, or a mix. I think a mix, but I am therefore not sure what the concoction will be.
  
# There will be lots of notes.
  
# 3.1 LASSO ------------------------------------------------------------------
# Data --------------------------------------------------------------------
# Spp.(y) data ------------------------------------------------------------
# Note: using templates (native C3/4 richness) to assess fit with EFs
# Will go through this with total richness first, then figure out how to double it for C3/4 (and then double that again for Introduced spp.)
  
# native rarefied (30 records) richness
    spp.raw <- read.csv("C:/Users/s436862/Dropbox/Rarefaction/4. Results/Rarefaction/CSV/Rarefied_30 Nat_Int Total_C3_4.csv", header = T) %>%
    select(nat_total)
  
  land.cat <- read.csv("1. Data files/EFs/4. EFs aggregated/Terrestrial categories.csv", header = T)
  
  spp.tot <- cbind(land.cat, spp.raw) %>% # matrix for use in glmnet function
    mutate(nat_total = ifelse(is.na(nat_total) == T, 0, nat_total)) %>% # NAs to zeroes
    filter(cell_category == "terrestrial") %>% # Aus cells only
    select(-cell_category, -cell_id)
  
# Environmental (x) data ------------------------------------------------
# EF data
  ef <- read.csv("1. Data files/EFs/4. EFs aggregated/EF_COMPLETE.csv", header = T) %>%
    filter(cell_category == "terrestrial") %>% # Aus only cells
    select(-glu, -cell_id, -cell_category)
  
  table(is.na(ef) == T) # There is one NA
                        # Specifically, row 933, column 9 (PEWC)
# This is problematic: I dunno how to run the model with it in
# Thus:
# Short-term solution: replace with mean
# Long-term solution: remove row (and spp. row too) OR hope PEWC is useless, and therefore we can leave it out and pretend this didn't happen OR ask Richard
  
# NA --> mean(pewc)  
  ef[is.na(ef)] = mean(ef[,9], na.rm= T)
  table(is.na(ef) == T) # score
  
# standardise 
  ef.s <- data.frame(scale(ef, center = T, scale = T)) 
 
# Required as matrix
# My method
  ef.x <- as.matrix(ef.s) 
  
# -------------------------------------------------------------------------------
# PR stats matrix: longer code but can change to not-linear predictors ------------------------------------
  ef.x2 <- model.matrix( ~ hii + arid + rz + sp + st 
                         + elev + evap + pawc + pewc + mat 
                         + mdr + iso + ts + twarmm + tcoldm + tar  
                         + twetq + tdryq + twarmq + tcoldq + ap  
                         + pwetm + pdrym + ps + pwetq + pdryq 
                         + pwarmq + pcoldq + th, ef.s) 
  ef.x2 <- ef.x2[,-1] 
# -------------------------------------------------------------------------------
  
  
# LASSO model fitting ----------------------------------------------------
# LASSO model  
# Requires: 
# (1) alpha
# Alpha = 1 (default), glmnet applied LASSO penalty
# Alpha = 0, glmnet applied Ridge Regression penalty
# Alpha = 0-1, applies both, weighted appropriately
# We'll use 1 for today 
  
# (2) Family 
# = gaussian ? 
  hist(ef.x) # Standardised variables look normally distributed

# (3) No NAs -- see above
  
# Models + visualisation --------------------------------------------------- 
# Function glmnet() returns a sequence of models for the user to choose from, one for each lambda parameter that has been applied. We can use function print() to view all of them: 
  my.glmnet.mod.a1 <- glmnet(ef.x, spp.tot$nat_total, family = "gaussian", alpha = 1)
  
# Note how, as Lambda decreases, more predictors have coefficients that are different to zero  
  print(my.glmnet.mod.a1)
  
# We can also visualize the model coefficients, plotted as a function of the L1 norm (???pi=1|??i|)):  
  plot(my.glmnet.mod.a1)
  
# Each curve correspond to the values of the regression coefficient for a given predictor. The numbers on the axis at the top of the plot indicate how many predictors are in the model for a particular model complexity (i.e. how many regression coefficients are different than zero). Note that L1 norm values are smaller the greater the ?? (more penalty -> fewer/smaller regression coefficients). See how for large ?? (left side of the plot), we start with 0 predictors in the model (intercept-only model). As ?? decreases (move to the right), more and more coefficients take values different to zero.
  
# We can look at the estimated coefficients for a specific model using function coef(), where s is the value of lambda. For instance, for lambda=0.02 we have:
  coef(my.glmnet.mod.a1, s = 0.02)
  
# Model specific selection ---------------------------------------------------
# Now, rather than having to choose a model ourselves, we can use functionality available in the package to guide this selection. Function cv.glmnet() uses cross-validation to do so (10-fold CV, by default):  
  my.glmnet.mod.a1.cv <- cv.glmnet(ef.x, spp.tot$nat_total, family = "gaussian", alpha = 1)
  # uses k-fold -- training and test data partitioning  
  
# Let's plot the output of the function:
  plot(my.glmnet.mod.a1.cv)
  # cool
  
# This is a cross-validation curve (red dotted line), showing the performance metric chosen (here deviance) with respect to lambda, together with error bars indicating +/- one standard deviation. Two selected lambdas are indicated by vertical dotted lines: 
# (1) 'lambda.min', which is the ?? at which deviance is minimized; 
# (2) 'lambda.1se', which is the largest value of ?? (i.e. most regularized model) such that deviance is within 1 standard error from the minimum. 
  

# We can view the selected lambdas like this:
  my.glmnet.mod.a1.cv$lambda.min  
  ## [1] 0.01875986
  my.glmnet.mod.a1.cv$lambda.1se                                 
  ## [1] 0.404169
  
# Q: what does this mean? Can I (should I) select any 'model' from between these two values?
  
# Let's now inspect the model identified at 'lambda.min':
  
  coef(my.glmnet.mod.a1.cv, s = "lambda.min") 
  # includes maybe 2/3 of predictors. Few coefficients basically at 0. Seems good.
  
#How does it compare with the model that corresponds to 'lambda.1se'?
  coef(my.glmnet.mod.a1.cv, s = "lambda.1se")
  # interesting - they're completely different
  # dropped vast majority of predictors
  
# As mentioned above, by default cv.glmnet() uses deviance as a performance metric, but other metrics can be used too. Check out the function documentation. A performance metric that is commonly used in species distribution modelling is the AUC (area under the ROC curve). This is a metric of discrimination ability (more about this in the model evaluation prac), and you would choose what performance metric to use for model selection, depending on what you wanted out of your model. You may want to try cv.glmnet() with AUC now:
  my.glmnet.mod.a1.cv.AUC <- cv.glmnet(ef.x, spp.tot$nat_total, family = "gaussian", alpha = 1, type.measure="auc")
  plot(my.glmnet.mod.a1.cv.AUC)
  my.glmnet.mod.a1.cv.AUC$lambda.min
  coef(my.glmnet.mod.a1.cv.AUC, s = "lambda.min")
  # really similar to the other lambda min :) 
  
# as tot he exact model to choose? Between the lines? one of the lines or the other line? I dunno. Ask Richard tomorrow, or get a fiar idea and discuss tomorrow.   
    
# 3.2 Ridge regression -------------------------------------------------------  
# Data -----------------------------------------------------------------------
# Run the data sections from LASSO, above  

# Model fitting --------------------------------------------------------------
# Let's have a look at ridge regression now. As shown above, in ridge regression the penalty involves the regression coefficients squared. This results in shrinking of coefficients, but usually not all the way to zero. Therefore we say that ridge regression does not produce "sparse" models, as the lasso does. 
  
# To apply ridge regression, we call functions glmnet() or cv.glmnet as above, but setting the 'elasticnet' parameter alpha to zero:
  my.glmnet.mod.a0 <- glmnet(ef.x, spp.tot$nat_total, family = "gaussian", alpha = 0)
  
  my.glmnet.mod.a0.cv <- cv.glmnet(ef.x, spp.tot$nat_total, family = "gaussian", alpha = 0)
  
# Have a look at the plot of regression coefficients, and compare it with the one obtained earlier for the lasso:
  plot(my.glmnet.mod.a0)
  # Pretty similar
  # only difference is mechanics where LASSO removes variables (top line goes from 0 - 29)
  # Ridge keeps at least dummy versions of them
  
# Let's now check what are the optimal values for the regularization parameter ??, and the model coefficients for the model identified as optimal:
    plot(my.glmnet.mod.a0.cv)  
    # pretty
  
  coef(my.glmnet.mod.a0.cv, s = "lambda.min")   
  # most predictors are in, and most have some kind of reasonable coefficient 
  # similar to lasso? I've have to properly check
  
  
    
# 3.3 Spatial predictions with glmnet ----------------------------------------
# Package glmnet includes a predict() function to compute predictions. Look at its documentation (?glmnet::predict.glmnet). As you can see, this function allows predicting given a matrix of new values at which predictions are to be made. To our knowledge, there are currently no functions that allow computing predictions from a glmnet object directly onto a raster. Therefore, here, we will transform our rasters into a matrix, we will use that matrix for prediction, and then we will turn the vector predictions back onto a raster. This works well for our case study, but may be slow when working with bigger rasters (larger extents, and/or finer resolutions).
  
# We first turn our raster stack into a dataframe, leaving aside the NA values:
  nsw.stack.S.df<-as.data.frame(nsw.stack.S, xy = TRUE, na.rm = TRUE)
  nsw.stack.S.df$vegsys <- factor(nsw.stack.S.df$vegsys, levels=1:9)
    
    
    
    
    
    
    
    
# ----------------------------------------------------------------------------  
# 5. Single-variate plots ------------------------------------------
  # Here I think looking at nat vs. Int species richness in a few key areas will be good
  
  # Might help :
  library(ggThemeAssist)
  
  # plot1 <- ggplot(data= spp_ef, aes(x=ap, y=nat_rare)) + 
  #   geom_point(aes(col=spp_df$bamboo, size=dat$prop.aban)) # or whatever your plot is
  # 
  # ggThemeAssistGadget(plot1) # makes an easy-to-use interface and then returns the code that configures your plot how you like it!
  
# Plot function ---------------------------------------------------------
# Requires:
# df: complete df of all the efs and spp
# xcol: spp
# ycol enviro factor
# xlab: ef name
# ylab: spp name
# save: exact save locale
  
# test --------------------------------------------------
  plot_fun(spp_ef, "ap", "nat_rare", "Native richness (rare)", "Annual precip.", "Nat_rare-AP raw.jpeg")
  
  # that didn't work - let' try the other way
  df <- spp_ef
  xcol <- "ap"
  ycol <- "nat_rare"
  xlab <- "Annual precip."
  ylab <- "Native richness (rare)"
  save <- "Nat_rare-AP raw.jpeg"
  
  # Function to plot combinations of columns & rows
  plot_fun <- function(df, xcol, ycol, xlab, ylab, save){
    df <- select(df, x = one_of(xcol), y = one_of(ycol))
    
    n <- ggplot(data = df, aes(x, y)) +
      geom_point(size = 1.5) +
      theme_bw() + 
      labs(x = ylab,
           y = xlab) +
      theme(axis.title = element_text(size = 14)) + 
      geom_smooth(method = "lm", se = FALSE)
    
    #ggThemeAssistGadget(n) 
    
    n + theme(axis.ticks = element_line(size = 0.8), 
              panel.grid.major = element_line(linetype = "blank"), 
              panel.grid.minor = element_line(size = 0.5, 
                                              linetype = "blank"), axis.title = element_text(size = 17), 
              axis.text = element_text(size = 14), 
              panel.background = element_rect(fill = NA))
    
    
    
    # save
    ggsave(save, plot = last_plot(), dpi = 500, device = "jpeg")
    
    return(n)
  } # finish script
  

# 5.1 Native -----------------------------------------------------------
# Requires in this order:
# df: complete df of all the efs and spp
# xcol: spp
# ycol enviro factor
# xlab: ef name
# ylab: spp name
# save: exact save locale 
  
  # Nat-AP raw
  m_nat_ap <- lm(nat_rare ~ ap, data = spp_ef)
  summary(m_nat_ap) 0.07
  
  plot_fun(spp_ef, 
           "ap", 
           "nat_rare", 
           "Native richness (rare)", 
           "Annual precip.",
           "Nat_rare-AP raw.jpeg")
  
  # Nat-GLU raw 
  m_nat_glu <- lm(nat_rare ~ glu, data = spp_ef)
  summary(m_nat_glu)
  
  plot_fun(spp_ef, 
           "glu", 
           "nat_rare", 
           "Native richness (rare)", 
           "Land use", 
           "Nat_rare-GLU raw.jpeg")
  
  # Nat-TS raw 
  m_nat_ts <- lm(nat_rare ~ ts, data = spp_ef)
  summary(m_nat_ts) #0.03
  
  plot_fun(spp_ef, 
           "ts", 
           "nat_rare", 
           "Native richness (rare)", 
           "Temp. seasonality", 
           "Nat_rare-TS raw.jpeg")
  
  # Nat-HII raw 
  m_nat_hii <- lm(nat_rare ~ hii, data = spp_ef)
  summary(m_nat_hii) # 21
  
  plot_fun(spp_ef, 
           "hii", 
           "nat_rare", 
           "Native richness (rare)", 
           "Human influence index", 
           "Nat_rare-HII raw.jpeg")
  
  
# ---------------------------------------------------------------------------  

# 5.2 Introduced -----------------------------------------------------------
# Requires in this order:
# df: complete df of all the efs and spp
# xcol: spp
# ycol enviro factor
# xlab: ef name
# ylab: spp name
# save: exact save locale 

# Int-AP raw 
  m_int_ap <- lm(int_rare ~ ap, data = spp_ef)
  summary(m_int_ap) # 0.03
  
  plot_fun(spp_ef, 
           "ap", 
           "int_rare", 
           "Exotic richness (rare)", 
           "Annual precip.",
           "Int_rare-AP raw.jpeg")

# Int-GLU raw 
  m_int_glu <- lm(int_rare ~ glu, data = spp_ef)
  summary(m_int_glu)
  
  plot_fun(spp_ef, 
           "glu", 
           "int_rare", 
           "Exotic richness (rare)", 
           "Land use",
           "Int_rare-GLU raw.jpeg")

# Int-hii raw
  m_int_hii <- lm(int_rare ~ hii, data = spp_ef)
  summary(m_int_hii)
  
  plot_fun(spp_ef, 
           "hii", 
           "int_rare", 
           "Exotic richness (rare)", 
           "Human influence index",
           "Int_rare-HII raw.jpeg")
  # that's insane

# --------------------------------------------------------------------------- 
