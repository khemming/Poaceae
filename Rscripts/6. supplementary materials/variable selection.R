#####################################################################
# environmental variable selection for linear regression
#####################################################################
# date created: 17/5
# last updated:

# aim ------------------------------------------------------------------------
# dwindle the 28 variables down to 8 or so
# using correlation criteria
# and VIF scores we'll assess

# library ---------------------------------------------------------------------
  library(tidyverse)
  library(RColorBrewer)
  library(purrr)
  library(ggthemes)
  library(corrplot)
  library(factoextra)

  setwd("C:/Users/s436862/Dropbox/Poaceae/Results")
  
# data -----------------------------------------------------------------------
  dat <- read.csv("EVs/CSV/EVs scaled.csv", header = T) %>%
    filter(cell.category.pewc == "land") %>%
    dplyr::select(-cell.id, -cell.category.pewc, -cell.category.complete, -prop.cover)
  
  dat1 <- dplyr::select(dat, -clay)
  dat2 <- dplyr::select(dat, pcoldq, pwarmq, amt, ts, arid, pewc, th, hii, pewc)
  dat3 <- dplyr::select(dat, pcoldq, pwarmq, amt, ts, arid, pewc, th, hii, clay)
  
  
# 1. Pearson's correlation -------------------------------------------------------
  pcor <- round(cor(dat1, method = "pearson", use = "complete.obs"), 2)
  write.csv(pcor, "EVs/CSV/100 km correlation matrix.csv")  
  
# reduced EVs on account fo variable selection 
  pcor.vs <- round(cor(dat2, method = "pearson", use = "complete.obs"), 2)
  write.csv(pcor.vs, "EVs/CSV/100 km VS correlation matrix.csv")  
  
# correlation matrix #1: all EFs 
  col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
  
  jpeg("EVs/Graphs/EDA/Correlation matrix.jpeg", width = 15, height = 15, units = "cm", res = 300)
  corrplot(pcor, method = "color", col = col(200),
           type = "upper", order = "hclust", number.cex = .4,
           addCoef.col = "black", # Add coefficient of correlation
           tl.col = "black", tl.srt = 90) # Text label color and rotation
  dev.off()           
  
# correlation matrix #2: reduced set (via EDA) 
  jpeg("EVs/Graphs/EDA/VS correlation matrix.jpeg", width = 15, height = 15, units = "cm", res = 300)
  corrplot(pcor.vs, method = "color", col = col(200),
           type = "upper", order = "hclust", number.cex = .7,
           addCoef.col = "black", # Add coefficient of correlation
           tl.col = "black", tl.srt = 90) # Text label color and rotation
  dev.off()           
  
# 2. PCA ------------------------------------------------------------------------------
# complete EV data set
  res.pca <- prcomp(dat, scale = F)
  
  jpeg("EVs/Graphs/EDA/PCA.jpeg", width = 15, height = 15, units = "cm", res = 500)
  fviz_pca_var(res.pca,
               col.var = "contrib", # Color by contributions to the PC
               gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
               repel = TRUE   # Avoid text overlapping
  )
  dev.off() 
  
# complete EV data set
  res.pca2 <- prcomp(dat2, scale = F)
  
  jpeg("EVs/Graphs/EDA/PCA VS.jpeg", width = 15, height = 15, units = "cm", res = 500)
  fviz_pca_var(res.pca2,
               col.var = "contrib", # Color by contributions to the PC
               gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
               repel = TRUE   # Avoid text overlapping
  )
  
  dev.off() 
  
  
# 3. human influence model -----------------------------------------------------------
# aim --------------------------------------------------------------------------------
# curious as to how well environmental factors explain the distribution of human influence in Australia
# potentially rationale (extra stuff) as to why exotic species may instead be associated with them
  
# data ----------------------------------------------------------------------------
  dat <- read.csv("Results/EVs/CSV/100 km EFs scaled.csv", header = T) %>%
    filter(cell.category.v2 == "land") %>%
    dplyr::select(-cell.id, -cell.category.v1, -cell.category.v2)
  
# doing a step-wise regression (for now)
# step 1: everything
  m1 <- lm(hii ~ amt + ap + arid + elev + iso + mdr + pawc + pcoldq + 
             pdrym + pet + pewc + ps + pwarmq + pwetm + pwetq +
             rz + sp + st + tar + tcoldm + tcoldq + tdryq + th +
             ts + twarmm + twarmq + twetq + prop.cover, data = dat)
  summary(m1)  
  
# step 2: take out the quarter-month pairs
  m2 <- lm(hii ~ amt + ap + arid + elev + iso + mdr + pawc + pcoldq + 
             pet + pewc + ps + pwarmq + pwetq +
             rz + sp + st + tar + tcoldq + tdryq + th +
             ts + twarmq + twetq + prop.cover, data = dat)
  summary(m2)  
  
  
# step 3: take out the non-signifant terms
  m3 <- lm(hii ~ amt + ap + arid + elev + iso + 
             pcoldq + pet + pewc + ps + pwetq +
             rz + tcoldq + tdryq + th + ts + 
             twarmq + twetq + prop.cover, data = dat)
  summary(m3)  
  
# intersting, this says that the greatest factors explaining human influence are:
# (+) tcoldq, (-) atm, (+) ts, (-) twarmq, and ~ (+) ap and pwetq
# interesting!
  
# step 4: the variables I used in the model
  m4 <- lm(hii ~ ts + amt + pcoldq + arid + th + pewc + prop.cover, data = dat)
  summary(m4)  
  
