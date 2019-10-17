# Date created: 21/3/18
# Updated: 

# Aims: 
# (1) correlations between species richness estimators (SREs; including rarefaction) and sample-size (record no.) FOR POA

# (2) Correlations between EFs and the like

# (3) multivariate SRE-EF models for Native and Introduced grass species

library(dplyr)
library(car)

rm(list = ls())

setwd("C:/Users/s436862/Dropbox/Climate Matching/4. Results/grass groups AVH/Rarefaction")

################ Rarefaction EDA & 'simple' multivariate analysis ##########################
# Data based on: -----------------------------------------------------------------
# Rarefaction df + rasters; and methods templated by 'Single-scale SR-EF models' & 'Single-scale SR-EF Model-plot PILOT'
# the first aim of this section is to assess correlations between rarefaction and extrapolation estimators (SREs) with sample size for Poa, Nat and Int

# --------------------------- POA ---------------------------------------------
# For POA (so Nat + Int), rarefaction was calculated using both, so this seems most appropriate
# Poa ----------------------------------------------------------
# get rarefaction & extrapolation sres, and EFs together  
  extra <- read.csv("CSV/Poa_SRE.csv")
  rare <- read.csv("CSV/rarefaction_Poa_Int_Nat_multi_cutoffs.csv")
  ef <- read.csv("CSV/EFs.csv") 
# bind, rename  
  sre_ef_na <- data.frame(cbind(extra$n, extra$a, extra$chao1, extra$ichao1, extra$jack_second, extra$lanum, rare$poa_50, ef))
  colnames(sre_ef_na) <- c("tot_rec", "baseline", "chao1", "ichao", "jack_2nd", "lanum", "rarefied", "glu", "hii", "ap", "arid","cm", "cq", "elev", "evap", "mat", "ps", "rz", "sp", "st", "wm", "wq", "pawc", "pewc", "th")
  
# remove NA rows  
  arid <- sre_ef_na$arid
  sre_ef <- sre_ef_na[!is.na(arid), ] 
# save  
  write.csv(sre_ef, file = "CSV/Poa_SRE_EF.csv", row.names = F)

# Native-EF df ----------------------------------------------------------------
  extra <- read.csv("CSV/Nat_SRE.csv")
  rare <- read.csv("CSV/rarefaction_Poa_Int_Nat_multi_cutoffs.csv")
  ef <- read.csv("CSV/EFs.csv") 
# bind, rename  
  sre_ef_na <- data.frame(cbind(extra$n, extra$a, extra$chao1, extra$ichao1, extra$jack_second, extra$lanum, rare$nat_50, ef))
  colnames(sre_ef_na) <- c("tot_rec", "baseline", "chao1", "ichao", "jack_2nd", "lanum", "rarefied", "glu", "hii", "ap", "arid","cm", "cq", "elev", "evap", "mat", "ps", "rz", "sp", "st", "wm", "wq", "pawc", "pewc", "th")
  
# remove NA rows  
  arid <- sre_ef_na$arid
  sre_ef <- sre_ef_na[!is.na(arid), ] 
# save  
  write.csv(sre_ef, file = "CSV/Nat_SRE_EF.csv", row.names = F)
  
# Int-EF df ----------------------------------------------------------------
  extra <- read.csv("CSV/Int_SRE.csv")
  rare <- read.csv("CSV/rarefaction_Poa_Int_Nat_multi_cutoffs.csv")
  ef <- read.csv("CSV/EFs.csv") 

# bind, rename  
  sre_ef_na <- data.frame(cbind(extra$n, extra$a, extra$chao1, extra$ichao1, extra$jack_second, extra$lanum, rare$int_50, ef))
  colnames(sre_ef_na) <- c("tot_rec", "baseline", "chao1", "ichao", "jack_2nd", "lanum", "rarefied", "glu", "hii", "ap", "arid","cm", "cq", "elev", "evap", "mat", "ps", "rz", "sp", "st", "wm", "wq", "pawc", "pewc", "th")
  
# remove NA rows  
  arid <- sre_ef_na$arid
  sre_ef <- sre_ef_na[!is.na(arid), ] 

# save  
  write.csv(sre_ef, file = "CSV/Int_SRE_EF.csv", row.names = F)

# EF-correlation matrices ------------------------------------------------------
  ef <- select(sre_ef, ap, arid, cm, cq, elev, evap, hii, mat, ps, rz, sp, st, wm, wq, th, pawc, pewc)
  ef_cor <- cor(ef, method = "pearson", use ="complete.obs")
  # note: removed glu as categorical variable
  # tb and th give exactly the same answers (shouldn't really be surprising)
  write.csv(ef_cor, file = "CSV/EF_correlations.csv")
 
# How Poa correlates wih EFs   
  sre <- read.csv("CSV/Poa_SRE.csv")
  rare <- read.csv("CSV/rarefaction_Poa_Int_Nat_multi_cutoffs.csv")
  sre_r_na <- data.frame(cbind(sre$n, sre$a, sre$chao1, sre$ichao1, sre$jack_second, sre$lanum, rare$poa_50))
  colnames(sre_r_na) <- c("tot_rec", "baseline", "chao1", "ichao", "jack_2nd", "lanum", "rarefied")
# remove NA rows  
  chao1 <- sre_r_na$chao1
  sre_r <- sre_r_na[!is.na(chao1), ] 
# SRE Pearson Correlation Matrix 
  sre_r_cor <- cor(sre_r, method = "pearson", use ="complete.obs") # Lanum, you're crazy
  write.csv(sre_r_cor, file = "CSV/SRE total records correlation.csv")
  
# -----------------------------------------------------------------------------
  
##################### Rarefaction & SRE-EF multivariate analysis ########################
# then, to assess how introduced and native spcies richness patterns are explained via the EFs using different SREs

  rm(list = ls())
  setwd("C:/Users/s436862/Dropbox/Climate Matching/4. Results/grass groups AVH/Rarefaction")
  sre_ef <- read.csv("CSV/Poa_SRE_EF.csv")

# SRE-EF models--------------------------------------------------------------
# with EFs progressively reduced via correlations, VIF scores, significance
# -------------------------- Poa --------------------------------------------  
# Rarefied ------------------------------------------------------------------  
  r1 <- lm(rarefied ~ glu + ap + arid + cm + cq + elev + evap + hii + mat + ps + rz + sp + st + wm + wq + th + pawc + pewc, data = sre_ef) 
  summary(r1)
  
# reduced via correlations (c) 
# Removing : AP, CQ, EVAP, MAT, PS, WM, PEWC, ST  
  r1c <- lm(rarefied ~ glu + arid + cm + elev + hii + rz + sp + wq + th + pawc, data = sre_ef) 
  summary(r1c)
  
# VIF scores (v)  
  r1cv <- vif(lm(rarefied ~ glu + arid + cm + elev + hii + rz + sp + wq + th + pawc, data = sre_ef))
  r1cv # all good

# removing those which aren't significant (s) 
# Aridity, SP*, PAWC*, TH* 
# * = marginally significant. But removing them, R2 didn't decrease all that much
# note these were iterative steps  
  r1cvs <- lm(rarefied ~ cm + elev + rz + wq, data = sre_ef) 
  summary(r1cvs)
  
# Baseline ------------------------------------------------------------------
  b1 <- lm(baseline ~ glu + ap + arid + cm + cq + elev + evap + hii + mat + ps + rz + sp + st + wm + wq + th + pawc + pewc, data = sre_ef) 
  summary(b1)    
  
# reduced via correlations (c) 
  b1c <- lm(baseline ~ glu + arid + cm + elev + evap + hii + mat + rz + sp + wq + th + pawc, data = sre_ef) 
  summary(b1c)

# VIF scores (v1 & v2)  
  b1cv1 <- vif(lm(baseline ~ glu + arid + cm + elev + evap + hii + mat + rz + sp + wq + th + pawc, data = sre_ef))
  b1cv1 # ARID, MAT, CM, EVAP
 
  b1cv2 <- vif(lm(baseline ~ glu + cm + elev + hii + rz + sp + wq + th + pawc, data = sre_ef))
  b1cv2 # all good
  
# removing those which aren't significant (s) 
  b1cv2s1 <- lm(baseline ~ glu + cm + elev + hii + rz + sp + wq + th + pawc, data = sre_ef)
  summary(b1cv2s1)
  # removing ELEV, SP, PAWC 
  b1cv2s2 <- lm(baseline ~ glu + cm + hii + rz + wq + th, data = sre_ef) 
  summary(b1cv2s2)
  
# Chao1  --------------------------------------------------------------------
  c1 <- lm(chao1 ~ glu + ap + arid + cm + cq + elev + evap + hii + mat + ps + rz + sp + st + wm + wq + th + pawc + pewc, data = sre_ef) 
  summary(c1)    
  
  # reduced via correlations (c) 
  c1c <- lm(chao1 ~ glu + arid + cm + elev + evap + hii + mat + rz + sp + wq + th + pawc, data = sre_ef) 
  summary(c1c)
  
  # VIF scores (v1 & v2)  
  c1cv1 <- vif(lm(chao1 ~ glu + arid + cm + elev + evap + hii + mat + rz + sp + wq + th + pawc, data = sre_ef))
  c1cv1 # ARID, MAT, CM, EVAP
  
  c1cv2 <- vif(lm(chao1 ~ glu + cm + elev + hii + rz + sp + wq + th + pawc, data = sre_ef))
  c1cv2 # all good
  
# removing those which aren't significant (s) 
  c1cv2s1 <- lm(chao1 ~ glu + cm + elev + hii + rz + sp + wq + th + pawc, data = sre_ef)
  summary(c1cv2s1)
  # removing ELEV, SP, PAWC 
  c1cv2s2 <- lm(chao1 ~ glu + cm + hii + rz + wq + th, data = sre_ef) 
  summary(c1cv2s2)
  
# iChao1 --------------------------------------------------------------------
  ic1 <- lm(ichao ~ glu + ap + arid + cm + cq + elev + evap + hii + mat + ps + rz + sp + st + wm + wq + th + pawc + pewc, data = sre_ef) 
  summary(ic1)    
  
# reduced via correlations (c) 
  ic1c <- lm(ichao ~ glu + arid + cm + elev + evap + hii + mat + rz + sp + wq + th + pawc, data = sre_ef) 
  summary(ic1c)
  
# VIF scores (v1 & v2)  
  ic1cv1 <- vif(lm(ichao ~ glu + arid + cm + elev + evap + hii + mat + rz + sp + wq + th + pawc, data = sre_ef))
  ic1cv1 # ARID -- just; MAT, CM, EVAP
  
  ic1cv2 <- vif(lm(chao1 ~ glu + arid + cm + elev + hii + rz + sp + wq + th + pawc, data = sre_ef))
  ic1cv2 # kept arid -- all is good
  
# removing those which aren't significant (s) 
  ic1cv2s1 <- lm(ichao ~ glu + cm + arid + elev + hii + rz + sp + wq + th + pawc, data = sre_ef)
  summary(ic1cv2s1)
  # removing ELEV, SP, PAWC (& arid because it explains less than 1%) 
  ic1cv2s2 <- lm(ichao ~ glu + cm + hii + rz + wq + th, data = sre_ef) 
  summary(ic1cv2s2)
  
# Jackknife second order ----------------------------------------------------  
  j1 <- lm(jack_2nd ~ glu + ap + arid + cm + cq + elev + evap + hii + mat + ps + rz + sp + st + wm + wq + th + pawc + pewc, data = sre_ef) 
  summary(j1)    
  
# reduced via correlations (c) 
  j1c <- lm(jack_2nd ~ glu + arid + cm + elev + evap + hii + mat + rz + sp + wq + th + pawc, data = sre_ef) 
  summary(j1c)
  
# VIF scores (v1 & v2)  
  j1cv1 <- vif(lm(jack_2nd ~ glu + arid + cm + elev + evap + hii + mat + rz + sp + wq + th + pawc, data = sre_ef))
  j1cv1 # ARID -- just (going to keep); MAT, CM, EVAP
  
  j1cv2 <- vif(lm(jack_2nd ~ glu + cm + elev + hii + rz + sp + wq + th + pawc, data = sre_ef))
  j1cv2 # all good
  
# removing those which aren't significant (s) 
  j1cv2s1 <- lm(jack_2nd ~ glu + cm + elev + hii + rz + sp + wq + th + pawc, data = sre_ef)
  summary(j1cv2s1)
  # removing ELEV, SP, PAWC -- and arid because of low R2 contribution
  j1cv2s2 <- lm(jack_2nd ~ glu + cm + hii + rz + wq + th, data = sre_ef) 
  summary(j1cv2s2)  
# Lanum ---------------------------------------------------------------------  
  l1 <- lm(lanum ~ glu + ap + arid + cm + cq + elev + evap + hii + mat + ps + rz + sp + st + wm + wq + th + pawc + pewc, data = sre_ef) 
  summary(l1)    
  # r2 = 0.0009
  # not going to bother going through the motions

  
# ---------------------------------------------------------------------------
# -------------------------- Native -----------------------------------------  
  rm(list = ls())
  setwd("C:/Users/s436862/Dropbox/Climate Matching/4. Results/grass groups AVH/Rarefaction")
  sre_ef <- read.csv("CSV/Nat_SRE_EF.csv")
  
# Rarefied ------------------------------------------------------------------  
  r1 <- lm(rarefied ~ glu + ap + arid + cm + cq + elev + evap + hii + mat + ps + rz + sp + st + wm + wq + th + pawc + pewc, data = sre_ef) 
  summary(r1)
  
# reduced via correlations (c)
# Removing : AP, CQ, EVAP, MAT, PS, WM, PEWC, ST  
  r1c <- lm(rarefied ~ glu + arid + cm + elev + hii + rz + sp + wq + th + pawc, data = sre_ef) 
  summary(r1c)
  
# VIF scores (v)  
  r1cv <- vif(lm(rarefied ~ glu + arid + cm + elev + hii + rz + sp + wq + th + pawc, data = sre_ef))
  r1cv # all good
  
# removing those which aren't significant (s) 
# CM, SP, TH
  r1cvs <- lm(rarefied ~ glu + arid + elev + hii + rz + wq + pawc, data = sre_ef) 
  summary(r1cvs)
  
# Only E EFs (e)
  r1cvse <- lm(rarefied ~ arid + elev + rz + wq + pawc, data = sre_ef) 
  summary(r1cvse)
  
# Only A EFs (a)  
  r1cvsa <- lm(rarefied ~ glu + hii, data = sre_ef) 
  summary(r1cvsa)
  
# Baseline ------------------------------------------------------------------
  b1 <- lm(baseline ~ glu + ap + arid + cm + cq + elev + evap + hii + mat + ps + rz + sp + st + wm + wq + th + pawc + pewc, data = sre_ef) 
  summary(b1)    
  
# reduced via correlations (c) 
  b1c <- lm(baseline ~ glu + arid + cm + elev + evap + hii + mat + rz + sp + wq + th + pawc, data = sre_ef) 
  summary(b1c)
  
# VIF scores (v1 & v2)  
  b1cv1 <- vif(lm(baseline ~ glu + arid + cm + elev + evap + hii + mat + rz + sp + wq + th + pawc, data = sre_ef))
  b1cv1 # ARID*, MAT, CM, EVAP
  # * = just out of accepted VIF score; removing b/c it doesn't add to R2 much
  
  b1cv2 <- vif(lm(baseline ~ glu + cm + elev + hii + rz + sp + wq + th + pawc, data = sre_ef))
  b1cv2 # all good
  
# removing those which aren't significant (s) 
  b1cv2s1 <- lm(baseline ~ glu + cm + elev + hii + rz + sp + wq + th + pawc, data = sre_ef)
  summary(b1cv2s1)
  # removing ELEV, SP, PAWC 
  b1cv2s2 <- lm(baseline ~ glu + cm + hii + rz + wq + th, data = sre_ef) 
  summary(b1cv2s2)
  
# Only E EFs (e)
  b1cv2s2e <- lm(baseline ~ cm + rz + wq + th, data = sre_ef) 
  summary(b1cv2s2e)
  
# Only A EFs (a)  
  b1cv2s2a <- lm(baseline ~ glu + hii, data = sre_ef) 
  summary(b1cv2s2a)
# Chao1  --------------------------------------------------------------------
  c1 <- lm(chao1 ~ glu + ap + arid + cm + cq + elev + evap + hii + mat + ps + rz + sp + st + wm + wq + th + pawc + pewc, data = sre_ef) 
  summary(c1)    
  
# reduced via correlations (c) 
  c1c <- lm(chao1 ~ glu + arid + cm + elev + evap + hii + mat + rz + sp + wq + th + pawc, data = sre_ef) 
  summary(c1c)
  
# VIF scores (v1 & v2)  
  c1cv1 <- vif(lm(chao1 ~ glu + arid + cm + elev + evap + hii + mat + rz + sp + wq + th + pawc, data = sre_ef))
  c1cv1 # ARID*, CM, EVAP, MAT
  # * just enough but removing anyway
  
  c1cv2 <- vif(lm(chao1 ~ glu + cm + elev + hii + rz + sp + wq + th + pawc, data = sre_ef))
  c1cv2 # all good
  
# removing those which aren't significant (s) 
  c1cv2s1 <- lm(chao1 ~ glu + cm + elev + hii + rz + sp + wq + th + pawc, data = sre_ef)
  summary(c1cv2s1)
  # removing ELEV, SP, PAWC 
  c1cv2s2 <- lm(chao1 ~ glu + cm + hii + rz + wq + th, data = sre_ef) 
  summary(c1cv2s2)
 
# Only E EFs (e)
  c1cv2s2e <- lm(chao1 ~ cm + rz + wq + th, data = sre_ef) 
  summary(c1cv2s2e)
  
# Only A EFs (a)  
  c1cv2s2a <- lm(chao1 ~ glu + hii, data = sre_ef) 
  summary(c1cv2s2a)
  
# iChao1 --------------------------------------------------------------------
  ic1 <- lm(ichao ~ glu + ap + arid + cm + cq + elev + evap + hii + mat + ps + rz + sp + st + wm + wq + th + pawc + pewc, data = sre_ef) 
  summary(ic1)    
  
# reduced via correlations (c) 
  ic1c <- lm(ichao ~ glu + arid + cm + elev + evap + hii + mat + rz + sp + wq + th + pawc, data = sre_ef) 
  summary(ic1c)
  
# VIF scores (v1 & v2)  
  ic1cv1 <- vif(lm(ichao ~ glu + arid + cm + elev + evap + hii + mat + rz + sp + wq + th + pawc, data = sre_ef))
  ic1cv1 # ARID -- just; MAT, CM, EVAP
  
  ic1cv2 <- vif(lm(chao1 ~ glu + arid + cm + elev + hii + rz + sp + wq + th + pawc, data = sre_ef))
  ic1cv2 # kept arid -- all is good
  
# removing those which aren't significant (s) 
  ic1cv2s1 <- lm(ichao ~ glu + cm + arid + elev + hii + rz + sp + wq + th + pawc, data = sre_ef)
  summary(ic1cv2s1)
  # removing ELEV, SP, PAWC (& arid because it explains less than 1%) 
  ic1cv2s2 <- lm(ichao ~ glu + cm + hii + rz + wq + th, data = sre_ef) 
  summary(ic1cv2s2)
  
# Jackknife second order ----------------------------------------------------  
  j1 <- lm(jack_2nd ~ glu + ap + arid + cm + cq + elev + evap + hii + mat + ps + rz + sp + st + wm + wq + th + pawc + pewc, data = sre_ef) 
  summary(j1)    
  
# reduced via correlations (c) 
  j1c <- lm(jack_2nd ~ glu + arid + cm + elev + evap + hii + mat + rz + sp + wq + th + pawc, data = sre_ef) 
  summary(j1c)
  
# VIF scores (v1 & v2)  
  j1cv1 <- vif(lm(jack_2nd ~ glu + arid + cm + elev + evap + hii + mat + rz + sp + wq + th + pawc, data = sre_ef))
  j1cv1 # ARID -- just (going to keep); MAT, CM, EVAP
  
  j1cv2 <- vif(lm(jack_2nd ~ glu + cm + arid + elev + hii + rz + sp + wq + th + pawc, data = sre_ef))
  j1cv2 # all good
  
# removing those which aren't significant (s) 
  j1cv2s1 <- lm(jack_2nd ~ glu + cm + elev + arid + hii + rz + sp + wq + th + pawc, data = sre_ef)
  summary(j1cv2s1)
  # removing ELEV, SP, PAWC -- and arid because of low R2 contribution
  j1cv2s2 <- lm(jack_2nd ~ glu + cm + hii + rz + wq + th, data = sre_ef) 
  summary(j1cv2s2)  
# Lanum ---------------------------------------------------------------------  
  l1 <- lm(lanum ~ glu + ap + arid + cm + cq + elev + evap + hii + mat + ps + rz + sp + st + wm + wq + th + pawc + pewc, data = sre_ef) 
  summary(l1)    
  # r2 = negative. Wow. 
  
# ---------------------------------------------------------------------------

# -------------------------- Introduced -----------------------------------------  
  rm(list = ls())
  setwd("C:/Users/s436862/Dropbox/Climate Matching/4. Results/grass groups AVH/Rarefaction")
  sre_ef <- read.csv("CSV/Int_SRE_EF.csv")
  
# Rarefied ------------------------------------------------------------------  
  r1 <- lm(rarefied ~ glu + ap + arid + cm + cq + elev + evap + hii + mat + ps + rz + sp + st + wm + wq + th + pawc + pewc, data = sre_ef) 
  summary(r1)
  
# reduced via correlations (c)
# Removing : AP, CQ, EVAP, MAT, PS, WM, PEWC, ST  
  r1c <- lm(rarefied ~ glu + arid + cm + elev + hii + rz + sp + wq + th + pawc, data = sre_ef) 
  summary(r1c)
  
# VIF scores (v)  
  r1cv <- vif(lm(rarefied ~ glu + arid + cm + elev + hii + rz + sp + wq + th + pawc, data = sre_ef))
  r1cv # all good
  
# removing those which aren't significant (s), iteratively
  # CM, SP, TH, RZ, ELEV, PAWC
  r1cvs <- lm(rarefied ~ glu + hii + wq, data = sre_ef) 
  summary(r1cvs)
  
# Only E EFs (e)
  r1cvse <- lm(rarefied ~ wq + th, data = sre_ef) 
  summary(r1cvse)
  
# Only A EFs (a)  
  r1cvsa <- lm(rarefied ~ glu + hii, data = sre_ef) 
  summary(r1cvsa) 
  
# Baseline ------------------------------------------------------------------
  b1 <- lm(baseline ~ glu + ap + arid + cm + cq + elev + evap + hii + mat + ps + rz + sp + st + wm + wq + th + pawc + pewc, data = sre_ef) 
  summary(b1)    
  
# reduced via correlations (c) 
  b1c <- lm(baseline ~ glu + arid + cm + elev + evap + hii + mat + rz + sp + wq + th + pawc, data = sre_ef) 
  summary(b1c)
  
# VIF scores (v1 & v2)  
  b1cv1 <- vif(lm(baseline ~ glu + arid + cm + elev + evap + hii + mat + rz + sp + wq + th + pawc, data = sre_ef))
  b1cv1 # ARID*, MAT, CM, EVAP
  # * = just out of accepted VIF score; removing b/c it doesn't add to R2 much
  
  b1cv2 <- vif(lm(baseline ~ glu + cm + elev + hii + rz + sp + wq + th + pawc, data = sre_ef))
  b1cv2 # all good
  
# removing those which aren't significant (s) 
  b1cv2s1 <- lm(baseline ~ glu + cm + elev + hii + rz + sp + wq + th + pawc, data = sre_ef)
  summary(b1cv2s1)
  # removing ELEV, SP, PAWC 
  b1cv2s2 <- lm(baseline ~ glu + cm + hii + rz + wq + th, data = sre_ef) 
  summary(b1cv2s2)
# ones which don't contribute to a high r2
  b1cv2s3 <- lm(baseline ~ glu + hii + th, data = sre_ef) 
  summary(b1cv2s3)
  
# Only E EFs (e)
  b1cvs3e <- lm(baseline ~ wq + th, data = sre_ef) 
  summary(b1cvs3e)
  
# Only A EFs (a)  
  b1cvs3a <- lm(baseline ~ glu + hii, data = sre_ef) 
  summary(b1cvs3a)
  
# Chao1  --------------------------------------------------------------------
  c1 <- lm(chao1 ~ glu + ap + arid + cm + cq + elev + evap + hii + mat + ps + rz + sp + st + wm + wq + th + pawc + pewc, data = sre_ef) 
  summary(c1)    
  
# reduced via correlations (c) 
  c1c <- lm(chao1 ~ glu + arid + cm + elev + evap + hii + mat + rz + sp + wq + th + pawc, data = sre_ef) 
  summary(c1c)
  
# VIF scores (v1 & v2)  
  c1cv1 <- vif(lm(chao1 ~ glu + arid + cm + elev + evap + hii + mat + rz + sp + wq + th + pawc, data = sre_ef))
  c1cv1 # ARID*, CM, EVAP, MAT
  # * just enough but removing anyway
  c1cv2 <- vif(lm(chao1 ~ glu + cm + elev + hii + rz + sp + wq + th + pawc, data = sre_ef))
  c1cv2 # all good
  
# removing those which aren't significant (s) 
  c1cv2s1 <- lm(chao1 ~ glu + cm + elev + hii + rz + sp + wq + th + pawc, data = sre_ef)
  summary(c1cv2s1)
  # removing ELEV, SP, PAWC + WQ
  c1cv2s2 <- lm(chao1 ~ glu + cm + hii + rz +th, data = sre_ef) 
  summary(c1cv2s2)
  
  # tried removing some which did not look to contribute to R2
  # interatively removed CM & RZ
  c1cv2s3 <- lm(chao1 ~ glu + hii + th, data = sre_ef) 
  summary(c1cv2s3)
  
# Only E EFs (e)
  c1cvs3e <- lm(chao1 ~ wq + th, data = sre_ef) 
  summary(c1cvs3e)
  
# Only A EFs (a)  
  c1cvs3a <- lm(chao1 ~ glu + hii, data = sre_ef) 
  summary(c1cvs3a)
  
  
# iChao1 --------------------------------------------------------------------
  ic1 <- lm(ichao ~ glu + ap + arid + cm + cq + elev + evap + hii + mat + ps + rz + sp + st + wm + wq + th + pawc + pewc, data = sre_ef) 
  summary(ic1)    
  
# reduced via correlations (c) 
  ic1c <- lm(ichao ~ glu + arid + cm + elev + evap + hii + mat + rz + sp + wq + th + pawc, data = sre_ef) 
  summary(ic1c)
  
# VIF scores (v1 & v2)  
  ic1cv1 <- vif(lm(ichao ~ glu + arid + cm + elev + evap + hii + mat + rz + sp + wq + th + pawc, data = sre_ef))
  ic1cv1 # ARID -- just; MAT, CM, EVAP
  
  ic1cv2 <- vif(lm(chao1 ~ glu + arid + cm + elev + hii + rz + sp + wq + th + pawc, data = sre_ef))
  ic1cv2 # kept arid -- all is good
  
  # removing those which aren't significant (s) 
  ic1cv2s1 <- lm(ichao ~ glu + cm + arid + elev + hii + rz + sp + wq + th + pawc, data = sre_ef)
  summary(ic1cv2s1)
  # removing ARID, SP, WQ, PAWC & ELEV (DN contrib. to R2)
  ic1cv2s2 <- lm(ichao ~ glu + hii + cm + th, data = sre_ef) 
  summary(ic1cv2s2)
  
# Jackknife second order ----------------------------------------------------  
  j1 <- lm(jack_2nd ~ glu + ap + arid + cm + cq + elev + evap + hii + mat + ps + rz + sp + st + wm + wq + th + pawc + pewc, data = sre_ef) 
  summary(j1)    
  
# reduced via correlations (c) 
  j1c <- lm(jack_2nd ~ glu + arid + cm + elev + evap + hii + mat + rz + sp + wq + th + pawc, data = sre_ef) 
  summary(j1c)
  
# VIF scores (v1 & v2)  
  j1cv1 <- vif(lm(jack_2nd ~ glu + arid + cm + elev + evap + hii + mat + rz + sp + wq + th + pawc, data = sre_ef))
  j1cv1 # ARID -- just (going to keep); MAT, CM, EVAP
  
  j1cv2 <- vif(lm(jack_2nd ~ glu + cm + arid + elev + hii + rz + sp + wq + th + pawc, data = sre_ef))
  j1cv2 # all good
  
# removing those which aren't significant (s) 
  j1cv2s1 <- lm(jack_2nd ~ glu + elev + arid + hii + rz + sp + wq + th + pawc, data = sre_ef)
  summary(j1cv2s1)
  # removing ARID, ELEV, SP, WQ, PAWC
  j1cv2s2 <- lm(jack_2nd ~ glu + hii + th, data = sre_ef)   
  summary(j1cv2s2)  
# Lanum ---------------------------------------------------------------------  
  l1 <- lm(lanum ~ glu + ap + arid + cm + cq + elev + evap + hii + mat + ps + rz + sp + st + wm + wq + th + pawc + pewc, data = sre_ef) 
  summary(l1)    
  # r2 = negative. Wow. 
  
# ---------------------------------------------------------------------------






