###############################

# Regression diagnostics/exploratory data analysis
# for environmental factors and native and introduced Poa spps
# see metadata for more info


# Sources: A protocol for data exploration to avoid common statistical problems (Zuur et al. 2010)
# & http://www.statmethods.net/stats/rdiagnostics.html



  rm(list = ls())
  
  library(UsingR) # for some good EDA type functions
  library(ggthemes)
  library(car)
  library(ggplot2)
  library(tidyr)
  library(raster)
  library(rgdal)
  library(tidyverse)
 
  setwd("C:/Users/s436862/Dropbox/Climate Matching/1. Data files")

# envirnomental factors + n/i species richness @ 75km^2   
  efp <- read.csv(file = "C:\\Users\\s436862\\Dropbox\\Climate Matching\\4. Results\\EF x Poa\\chao.poa_EF_75km.csv", header = T, sep = ",")
  
# covaration matrix  
  chart.Correlation(efp)
  a <- cor(efp, method = "pearson")
  plot(a)
  
  #write.csv(a, file = "C:/Users/s436862/Dropbox/Climate Matching/4. Results/EFs/EF_pearson.correlations.csv")
  

# GG boxplot method (not quite working)
########################  

  # (need help)
 ggplot(poa_long,aes(y = poa_v, x = value, group = parameter)) +
    geom_point(size = 0.5, na.rm = T) +
    geom_smooth(method = "lm", na.rm = T)+
    facet_wrap(~ parameter, scale = "free" )
  
  ggsave("C:\\Users\\s436862\\Dropbox\\Climate Matching\\4. Results\\11 environmental factors\\Poa_11.ef_scatterplots.jpeg", plot = last_plot(), scale = 4, device = "jpeg")
  
# boxplots  
  ggplot(data = efp, aes(y = arid, x = value)) +
      geom_boxplot()
  
# box and dartplots  

########################  

# Boxplot
########################  
  
  par(mfrow= c(1,2))
  #boxplot(Sparrows$wingcrd,  ylab = "Wing length (mm)")
  #dotchart(Sparrows$wingcrd, xlab = "Wing length (mm)",
  #         ylab = "Order of the data")
  
# Y: nat and int
  boxplot(efp$nat)
  boxplot(log(efp$nat))
  # heaps better (note: -inf produced)
  
  boxplot(efp$int)
  boxplot(log(efp$int)) 
  # the box disappeared, but looks better (also note: -inf produced)

# X: EFs    
  boxplot(efp$arid)
  boxplot(log(ef$arid))
  # looks better
  
  boxplot(efp$evap)
  boxplot(log(efp$evap))
  # does not look better
  
  boxplot(efp$elev)
  boxplot(log(efp$elev))
  # kinda better
  
  boxplot(efp$mat)
  boxplot(log(efp$mat))
  # good, but not better
  
  boxplot(efp$wm)
  boxplot(log(efp$wm))
  # no better
  
  boxplot(efp$cm)
  boxplot(log(efp$cm))
  # no better
  
  boxplot(efp$ap)
  boxplot(log(efp$ap))
  # better
  
  boxplot(efp$ps)
  boxplot(log(efp$ps))
  # bit better
  
  boxplot(efp$wq)
  boxplot(log(efp$wq))
  # better
  
  boxplot(efp$cq)
  boxplot(log(efp$cq))
  # much better
  
  boxplot(efp$hii)
  boxplot(log(efp$hii))
  # bit better
  
  boxplot(efp$rz)
  boxplot(log(efp$rz))
  # Not better
  
  boxplot(efp$sp)
  boxplot(log(efp$sp))
  # little better
  
  boxplot(efp$st)
  boxplot(log(efp$st))
  # kinda better?

  
########################

# Dotchart (Cleveland boxplot)
########################  
# Y: nat and int
  dotchart(efp$nat)
  dotchart(log(efp$nat))
  # looks more random for sure
  
  dotchart(efp$int)
  dotchart(log(efp$int)) 
  # looks more random aussi
  
# X: EFs    
  dotchart(efp$arid)
  dotchart(log(efp$arid))
  # looks better
  
  dotchart(efp$evap)
  dotchart(log(efp$evap))
  # no differnce
  
  dotchart(efp$elev)
  dotchart(log(efp$elev))
  # marginally better
  
  dotchart(efp$mat)
  dotchart(log(efp$mat))
  # no difference
  
  dotchart(efp$wm)
  dotchart(log(efp$wm))
  # no differnce
  
  dotchart(efp$cm)
  dotchart(log(efp$cm))
  # worse
  
  dotchart(efp$ap)
  dotchart(log(efp$ap))
  # better
  
  dotchart(efp$ps)
  dotchart(log(efp$ps))
  # no difference
  
  dotchart(efp$wq)
  dotchart(log(efp$wq))
  # marginally better
  
  dotchart(efp$cq)
  dotchart(log(efp$cq))
  # bit better
  
  dotchart(efp$hii)
  dotchart(log(efp$hii))
  # bit better
  
  dotchart(efp$rz)
  dotchart(log(efp$rz))
  # Not better
  
  dotchart(efp$sp)
  dotchart(log(efp$sp))
  #little better
  
  dotchart(efp$st)
  dotchart(log(efp$st))
  # kinda better
 
########## 4/7: Zero trouble Y
  # frequency plot or corrgram

  # taken care of them, I think :)     
  
######### 5/7: Collinearity
  # VIF & scatterplots, conditional boxplots


# Histograms    
  
  

########################

#  Histograms
########################  
  par(mfrow= c(2,2))
  
  hist(efp$arid)
  hist(log(efp$arid))
  # looks better
  
  hist(efp$evap)
  hist(log(efp$evap))
  # worse
  
  hist(efp$elev)
  hist(log(efp$elev))
  # take your pick
  
  hist(efp$mat)
  hist(log(efp$mat))
  # worse
  
  hist(efp$wm)
  hist(log(efp$wm))
  # worse
  
  hist(efp$cm)
  hist(log(efp$cm))
  # worse
  
  hist(efp$ap)
  hist(log(efp$ap))
  # better
  
  hist(efp$ps)
  hist(log(efp$ps))
  # no difference
  
  hist(efp$wq)
  hist(log(efp$wq))
  # better
  
  hist(efp$cq)
  hist(log(efp$cq))
  # muuuuch better
  
  hist(efp$hii)
  hist(log(efp$hii))
  # no difference
  
  hist(efp$rz)
  hist(log(efp$rz))
  # Not better
  
  hist(efp$sp)
  hist(log(efp$sp))
  # not better
  
  hist(efp$st)
  hist(log(efp$st))
  # not better
  
########################

# Linear models & other tests or normality, homogeneity of variance, collinearity and independence
######################## 

##### Native regression 
  
# all EFs
  m_nat1 <- lm(nat ~ glu + ap + arid + cm + cq + elev + hii + evap + mat + ps + rz + sp + st + wm + wq, data = efp)
  
# reduced
  m_nat2 <- lm(nat ~ glu + ap + elev + hii + evap + rz + cm, data = efp)
  summary(m_nat2)
  
  
### Outliers
# assessing outliers
  outlierTest(m_nat1) # Bonferonni p-value for most extreme obs
  qqPlot(m_nat1, main="QQ Plot") #qq plot for studentized resid 
  leveragePlots(m_nat1) # leverage plots

### Influential Observations
# added variable plots 
  avPlots(m_nat1)
# Cook's D plot
# identify D values > 4/(n-k-1) 
  cutoff <- 4/((nrow(m_nat1)-length(m_nat1$coefficients)-2)) 
  plot(m_nat1, which=4, cook.levels=cutoff)
# Influence Plot 
  influencePlot(m_nat1, main="Influence Plot", sub="Circle size is proportial to Cook's Distance" )
  # think this crashes r
  # yup
  # took out "id.method" -- works fine now :) 
  
### Non-normality 
# Normality of Residuals
# qq plot for studentized resid
  qqPlot(m_nat1, main="QQ Plot")
# distribution of studentized residuals
  sresid <- studres(m_nat1) 
  hist(sresid, freq=FALSE, 
       main="Distribution of Studentized Residuals")
  xm_nat1<-seq(min(sresid),max(sresid),length=40) 
  ym_nat1<-dnorm(xm_nat1) 
  lines(xm_nat1, ym_nat1)
 
### Non-constant Error Variance
# Evaluate homoscedasticity
# non-constant error variance test
  ncvTest(m_nat1)
# plot studentized residuals vs. fitted values 
  spreadLevelPlot(m_nat1)
  
### Multi-collinearity
# evaluate collienarity  
  vif(m_nat1) # variance inflation factor (VIF) --> GVIF = generalVIF?
  sqrt(vif(m_nat1)) > 2 # problem?
  summary(m_nat1)
  # R2 = .41
  
# nat2: removed values of (G)VIFs above ten; arid, cq, evap, mat, ps, sp, st  
  m_nat2 <- lm(nat ~ ap + glu + evap + cm + elev + hii + rz, data = efp)
  summary(m_nat2)  # R2 decreased by 0.01
  # R2 = .38; still good
  vif(m_nat2) # variance inflation factor (VIF)
  sqrt(vif(m_nat2)) > 2 # problem?
  
# nat3: just anthropogenic activities
  m_nat3 <- lm(nat ~ glu + hii, data = efp)
  summary(m_nat3)  # R2 decreased by 0.01
  # R2 = .23; still high!
  vif(m_nat3) # variance inflation factor (VIF)
  sqrt(vif(m_nat3)) > 2 # problem?
  
### Nonlinearity
# component + residual plot 
  crPlots(m_nat1)
# Ceres plots 
  ceresPlots(m_nat1)
  
### Non-independence of Errors
# Test for Autocorrelated Errors
  durbinWatsonTest(m_nat1)
  
#  Additional Diagnostic Help
#  The gvlma( ) function in the gvlma package, performs a global validation of 
#  linear model assumptions as well separate evaluations of skewness, kurtosis, 
#  and heteroscedasticity.
# Global test of model assumptions
  library(gvlma)
  gvmodel <- gvlma(m_nat1) 
  summary(gvmodel)


#### introduced  
# model: introduced (with all terms)
  m_int1 <- lm(int ~ glu + ap + arid + cm + cq + elev + hii + evap + mat + ps + rz + sp + st + wm + wq, data = efp)
  summary(m_int1)
### Outliers
# assessing outliers
  outlierTest(m_int1) # Bonferonni p-value for most extreme obs
  qqPlot(m_int1, main="QQ Plot") #qq plot for studentized resid 
  leveragePlots(m_int1) # leverage plots
  
### Influential Observations
# added variable plots 
  avPlots(m_int1)
# Cook's D plot
# identify D values > 4/(n-k-1) 
  cutoff <- 4/((nrow(m_int1)-length(m_int1$coefficients)-2)) 
  plot(m_int1, which=4, cook.levels=cutoff)
# Influence Plot 
  influencePlot(m_int1, main="Influence Plot", sub="Circle size is proportial to Cook's Distance" )
  # think this crashes r
  # yup
  # took out "id.method" -- works fine now :) 
  
### Non-normality 
# Normality of Residuals
# qq plot for studentized resid
  qqPlot(m_int1, main="QQ Plot")
# distribution of studentized residuals
  sresid <- studres(m_int1) 
  hist(sresid, freq=FALSE, 
       main="Distribution of Studentized Residuals")
  xm_int1<-seq(min(sresid),max(sresid),length=40) 
  ym_int1<-dnorm(xm_int1) 
  lines(xm_int1, ym_int1)
  
### Non-constant Error Variance
# Evaluate homoscedasticity
# non-constant error variance test
  ncvTest(m_int1)
# plot studentized residuals vs. fitted values 
  spreadLevelPlot(m_int1)
  
### Multi-collinearity
# evaluate collienarity  
  vif(m_int1) # variance inflation factor (VIF) --> GVIF = generalVIF?
  sqrt(vif(m_int1)) > 2 # problem?
  summary(m_int1)
  # R2 = .60
  
# int2: removed values of (G)VIFs above ten; arid, cq, evap, mat, ps, sp, st  
  m_int2 <- lm(int ~ ap + glu + evap + cm + elev + hii + rz, data = efp)
  summary(m_int2)  # R2 decreased by 0.01
  # R2 = .59; still good
  vif(m_int2) # variance inflation factor (VIF)
  sqrt(vif(m_int2)) > 2 # problem?
  
# int3: just anthropogenic activities
  m_int3 <- lm(int ~ glu + hii, data = efp)
  summary(m_int3)  # R2 decreased by 0.02
  # R2 = .53; still high!
  vif(m_int3) # variance inflation factor (VIF)
  sqrt(vif(m_int3)) > 2 # problem?
  
### Nonlinearity
# component + residual plot 
  crPlots(m_int1)
# Ceres plots 
  ceresPlots(m_int1)
  
### Non-independence of Errors
# Test for Autocorrelated Errors
  durbinWatsonTest(m_int1)
  
#  Additional Diagnostic Help
#  The gvlma( ) function in the gvlma package, performs a global validation of 
#  linear model assumptions as well separate evaluations of skewness, kurtosis, 
#  and heteroscedasticity.
# Global test of model assumptions
  library(gvlma)
  gvmodel <- gvlma(m_int1) 
  summary(gvmodel)
  
########################

# multivariate plots
#######################
  
  # GGplot likes long data format
  efp_long <- gather(efp, parameter, value, glu:wq, factor_key = T)
  # paramter = EF name
  # value = grid cell value
  # factor_key = treat the new key (parameter) column as a factor 
  # (instead of character vector)

 ###### test 
# reduced covariates  
  m_nat2 <- lm(nat ~ glu + ap + elev + hii + evap + rz + cm, data = efp)
  summary(m_nat2)
  
  m_int2 <- lm(int ~ glu + ap + elev + hii + evap + rz + cm, data = efp)
  summary(m_int2)
  
  efp_m2 <- gather(efp, parameter, value, glu, ap, elev, hii, evap, rz, cm, factor_key = T) %>%
    select(-arid:-wq)
  # somehow, Kyle, that worked
  
  ggplot(efp_m2, aes(y = nat, x = value, group = parameter)) +
    geom_point(size = 0.5, na.rm = T) +
    geom_smooth(method = "lm")+
    facet_wrap(~ parameter, scale = "free" )
  
  # can't plot multivariate stats on one plot, brah. 
  
  
  
  
######
  
# native    
  ggplot(efp_long, aes(y = nat, x = value, group = parameter)) +
    geom_point(size = 0.5, na.rm = T) +
    geom_smooth(method = "lm")+
    facet_wrap(~ parameter, scale = "free" )
  
  ggsave("C:\\Users\\s436862\\Dropbox\\Climate Matching\\4. Results\\11 environmental factors\\Poa_11.ef_scatterplots.jpeg", plot = last_plot(), scale = 4, device = "jpeg")
  # Note: save it with size = 2; 0.5 is better for -> window, though
  
# introduced   
  ggplot(efp_long, aes(y = int, x = value, group = parameter)) +
    geom_point(size = 0.5, na.rm = T) +
    geom_smooth(method = "lm")+
    facet_wrap(~ parameter, scale = "free" )
  
##### plotting subsets of the parameters in long form
  efp_long$parameter <- as.factor(efp_long$parameter)
  efp_long.subset <- filter(efp_long, parameter == "ap" | parameter == "glu" | parameter == "evap" | parameter == "cm" | parameter == "hii"| parameter == "rz") %>%
    droplevels()
  # good for dropping parameters I don't like

# native adjusted EFs    
  ggplot(efp_long.subset,aes(y = nat, x = value, group = parameter)) +
    geom_point() +
    facet_wrap(~ parameter, scale = "free") +
    #geom_errorbar(aes(ymin = lcl.r, ymax = ucl.r), width = 0.04, position = dodge) +
    labs(x = "", y = "") +
    #geom_line(dat = egl.aug.log, aes(y = yy, x = xx), size =2) +
    #geom_line(dat = hol.aug.log, aes(y = yy, x = xx), size = 2) +
    geom_hline(yintercept = 0, lty = 5 ) +
    theme_classic() +
    theme_bw() +
    #choosen colour palette
    theme(plot.title = element_text(size = 12),
          plot.subtitle=element_text(size=20, hjust=0.5, face="italic", color="blue"),
          legend.position = "none",
          legend.background = element_rect(fill="white", size=1, linetype="solid", colour ="black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.title.y = element_text(colour = "black",size =14),
          axis.title.x = element_text(colour = "black",size =18),
          axis.text.x=element_text(size = 12),
          axis.text.y=element_text(colour = "black",size = 12),
          axis.ticks.x = element_line(size = 1),
          axis.ticks.y =element_line(size = 1),
          axis.line.x = element_line(size = 1),
          axis.line.y = element_line(size = 1)) +
    #scale_y_continuous(limits=c(-2.5,2)) +
    #scale_x_continuous(limits=c(0,4)) +
    ggtitle(sub = "", "")
  
  ggsave("C:\\Users\\s436862\\Dropbox\\Climate Matching\\4. Results\\EF x Poa\\Nat_EF_scatterplots.jpeg", plot = last_plot(), scale = 2, device = "jpeg")
  # Note: save it with size = 2; 0.5 is better for -> window, though
  
  
########################  
  
# Single variate regression plots
########################
  
# GGplot likes long data format
  efp_long <- gather(efp, parameter, value, glu:wq, factor_key = T)
  # paramter = EF name
  # value = grid cell value
  # factor_key = treat the new key (parameter) column as a factor 
  # (instead of character vector)

# drop variables    
  efp_long$parameter <- as.factor(efp_long$parameter)
  efp_long.subset <- filter(efp_long, parameter == "ap" | parameter == "glu" | parameter == "evap" | parameter == "cm" | parameter == "hii"| parameter == "rz") %>%
    droplevels()
  
  
# roooight, plottin' time
  ggplot(efp, aes(y = nat, x = ap)) +
    geom_point(size = 1.5, pch = 1, alpha = 1) +
    geom_smooth(method = "lm", color = "black") +
    labs(x = "Annual precipitation",
         y = "Native species richness") +
    theme_tufte() +
    #choosen colour palette
    theme(legend.position = "none",
          plot.title = element_text(size=16, hjust=0, face="italic", color="black"),
          plot.subtitle=element_text(size=16, hjust=0, face="italic", color="black"),
          legend.background = element_rect(fill="white", size=1, linetype="solid", colour ="black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.title.y=element_text(colour = "black",size =14),
          axis.title.x = element_text(colour = "black",size =14),
          axis.text.x=element_text(size = 14),
          axis.text.y=element_text(colour = "black",size = 14),
          axis.ticks.x = element_line(size = 1),
          axis.ticks.y =element_line(size = 1),
          axis.line.x = element_line(size = 1),
          axis.line.y = element_line(size = 1)) +
    annotate("text", x = 7.8, y = 4.4, label = "italic(R) ^ 2 == 0.22",
             parse = TRUE, col = "blue") +
    #scale_y_continuous(limits=c(-2.5,2)) + 
    #scale_x_continuous(limits=c(0,4)) +
    ggtitle("Log annual precipitation ~ log native richness")
  
  ggsave("C:/Users/s436862/Dropbox/Climate Matching/4. Results/Poa_x_AP/log.native_log.AP.jpeg", plot = last_plot(), scale = 1, dpi = 200, device = "jpeg")
  
  
  
  
  
  
  
  ######################