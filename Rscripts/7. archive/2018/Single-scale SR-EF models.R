
# Models and their various clothes -------

# Built on: 'Single-scale SR-EF DF.R'

# ------------------ Native Model ----------------------
nat_100 <- read.csv("C:/Users/s436862/Dropbox/Climate Matching/4. Results/Grass groups AVH/100 km scale/Nat SR-EFs DF.csv")
  
# EF Pearson Correlation Matrix ------------

nat_100_cor <- select(nat_100, ap, arid, cm, cq, elev, evap, hii, mat, ps, rz, sp, st, wm, wq, th, pawc, pewc)
# note: removed glu as categorical variables seem not to be welcome here
# tb and th give exactly the same answers (shouldn't really be surprising)
nat_100_cor <- cor(nat_100_cor, method = "pearson", use ="complete.obs")

# csv
write.csv(nat_100_cor, file = "C:/Users/s436862/Dropbox/Climate Matching/4. Results/Grass groups AVH/100 km scale/Nat EF correlation matrix.csv")
# note: don't do rownames = F for this cos we want them in this instance :)

# -----------------------------------------

# Reducing no. of EFs --------------------------------------------  
# everythang    
nat_1_na_a <- lm(chao1 ~ glu + ap + arid + cm + cq + elev + evap + hii + mat + ps + rz + sp + st + wm + wq + th + pawc + pewc, data = nat_100) 
# Ant says GLM is a better one for flexible
summary(avh_1_na_a)
# 100 km = 0.50 -- better than 27!


# avh_1_na_b (reduced efs on account of correlations)
# taking out ARID, MAT, WM, and SP + ST  
avh_1_na_b <- lm(chao1 ~ glu + arid + ap + cm + cq + elev + evap + hii + ps + rz + wq + th + pawc, data = nat_100) 
summary(avh_1_na_b)
# @ 100 = 0.49 -- wth 

# avh_1_na_c (reduced efs on account of significance)
# (note these are different between 100 and 50 km scales)
# common ones being taken out: CM, MAT, EVAP, PS
avh_1_na_c <- lm(chao1 ~ glu + arid + ap + cm + hii + wq + th + pawc, data = nat_100)
summary(avh_1_na_c)
# 0.43

# reducing stuff via VIF calculations  (just AP)   
avh_1_na_c <- vif(lm(chao1 ~ glu + arid + cm + hii + wq + th + pawc, data = nat_100))
avh_1_na_c
# If VIF up to 3 is bad, ok, bu twith all these, non eare above 3.4, and the model explains a lot mroe with them in it
avh_1_na_d <- lm(chao1 ~ glu + arid + cm + hii + wq + th + pawc, data = nat_100)
summary(avh_1_na_d)
# much worse without ap and 

# just LU and just HII
  nat_100_lu <- lm(chao1 ~ glu + hii, data = nat_100)
  summary(nat_100_lu)
  # 0.17 -- allg
  
  nat_100_hii <- lm(chao1 ~ arid, data = nat_100)
  summary(nat_100_hii)
  # 0.05, cool still the same




# Reduce community numbers (low SR cells) -------------------------
# using ichao1 SR  

# no zeroes  
avh_ef_0 <- filter(avh_ef, n != 0)
avh_1_b_0 <- lm(ichao1 ~ glu + ap + cq + elev + hii + rz + wq, data = avh_ef_0) 
summary(avh_1_b_0)
# 0.566

# no v.low (<10)
avh_ef_vl <- filter(avh_ef, ichao1>= 10)
avh_1_b_vl <- lm(ichao1 ~ glu + ap + cq + elev + hii + rz + wq, data = avh_ef_vl) 
summary(avh_1_b_vl)
# 0.563

# no low (<50)
avh_ef_l <- filter(avh_ef, ichao1>= 50)
avh_1_b_l <- lm(ichao1 ~ glu + ap + cq + elev + hii + rz + wq, data = avh_ef_l) 
summary(avh_1_b_l)
# 0.557

# no med (<100)  
avh_ef_m <- filter(avh_ef, ichao1>= 100)
avh_1_b_m <- lm(ichao1 ~ glu + ap + cq + elev + hii + rz + wq, data = avh_ef_m) 
summary(avh_1_b_m)
# 0.542

# cool   

# -----------------------------------------

# Introduced Model [update via Native model] -------------
# data ------------------------------
int_100 <- read.csv("C:/Users/s436862/Dropbox/Climate Matching/4. Results/Grass groups AVH/100 km scale/Int SR-EFs DF.csv")


# Reducing no. of EFs --------------------------------------------  
# everythang    
avh_1_na_a <- lm(chao1 ~ glu + ap + arid + cm + cq + elev + evap + hii + mat + ps + rz + sp + st + wm + wq + th + pawc + pewc, data = int_100) 
# Ant says GLM is a better one for flexible
summary(avh_1_na_a)
# 100 km = 0.68 


# avh_1_na_b (reduced efs on account of correlations)
# taking out ARID, MAT, WM, and SP + ST  
avh_1_na_b <- lm(chao1 ~ glu + ap + cm + cq + elev + evap + hii + ps + rz + wq + th + pawc, data = int_100) 
summary(avh_1_na_b)
# @ 100 = 0.49 -- wth 

# avh_1_na_c (reduced efs on account of significance)
# (note these are different between 100 and 50 km scales)
# common ones being taken out: CM, MAT, EVAP, PS
avh_1_na_c <- lm(chao1 ~ glu + arid + hii + ps + th, data = int_100)
summary(avh_1_na_c)
# 0.43

# reducing stuff via VIF calculations    
avh_1_na_c <- vif(lm(chao1 ~ glu + arid + hii + ps + th, data = int_100))
avh_1_na_c
# If VIF up to 3 is bad, ok, bu twith all these, non eare above 3.4, and the model explains a lot mroe with them in it
avh_1_na_d <- lm(ichao1 ~ glu + arid + hii + ps + th, data = int_100)
summary(avh_1_na_d)

# just land use and just hii
avh_1_na_lu <- lm(chao1 ~ glu + hii, data = int_100)
summary(avh_1_na_lu)
# = .44, interesting

avh_1_na_hii <- lm(chao1 ~ glu, data = int_100)
summary(avh_1_na_hii)
# = .49, interesting
# seems HII and LU are correlated!  

# therefore, model with cells of <200 records  
avh_1_na_d <- lm(ichao1 ~ glu + cq + elev + hii + ps, data = avh_ef)
summary(avh_1_na_d)
# 0.42, pretty good, gonna keep it

# without <200
avh_ef_1_no_200 <- filter(avh_ef, n> 200)
avh_1_b_m <- lm(ichao1 ~ glu + ap + cq + elev + hii + ps + wq, data = avh_ef_1_no_200) 
summary(avh_1_b_m)
# 100 = 0.542
# 50 = 0.37 <- this must be because we are throwing aaway many more cells at this scale
# Reduce community numbers (low SR cells) -------------------------
# using ichao1 SR  

# no zeroes  
avh_ef_0 <- filter(avh_ef, n != 0)
avh_1_b_0 <- lm(ichao1 ~ glu + ap + cq + elev + hii + rz + wq, data = avh_ef_0) 
summary(avh_1_b_0)
# 0.566

# no v.low (<10)
avh_ef_vl <- filter(avh_ef, ichao1>= 10)
avh_1_b_vl <- lm(ichao1 ~ glu + ap + cq + elev + hii + rz + wq, data = avh_ef_vl) 
summary(avh_1_b_vl)
# 0.563

# no low (<50)
avh_ef_l <- filter(avh_ef, ichao1>= 50)
avh_1_b_l <- lm(ichao1 ~ glu + ap + cq + elev + hii + rz + wq, data = avh_ef_l) 
summary(avh_1_b_l)
# 0.557

# no med (<100)  
avh_ef_m <- filter(avh_ef, ichao1>= 100)
avh_1_b_m <- lm(ichao1 ~ glu + ap + cq + elev + hii + rz + wq, data = avh_ef_m) 
summary(avh_1_b_m)
# 0.542

# cool   

# -----------------------------------------


# 200 km -------------------------   
# Generating SRE-EF dataframe --------------------------------------------------------

rm(list = ls())

setwd("C:/Users/s436862/Dropbox/Climate Matching/1. Data files/")

# load dataframe
avh_200km <- read.csv("C:/Users/s436862/Dropbox/Climate Matching/4. Results/AVH/200 km scale/200km SREs AVH.csv")

# EFs    
arid <- raster("EFs/EFs cropped/arid.grd") 
ap <- raster("EFs/EFs cropped/ap.grd") 
cm <- raster("EFs/EFs cropped/cm.grd")
cq <- raster("EFs/EFs cropped/cq.grd")
rz <- raster("EFs/EFs cropped/rz.grd")
sp <- raster("EFs/EFs cropped/sp.grd")
st <- raster("EFs/EFs cropped/st.grd")
elev <- raster("EFs/EFs cropped/elev.grd")
evap <- raster("EFs/EFs cropped/evap.grd")
hii <- raster("EFs/EFs cropped/hii.grd")
mat <- raster("EFs/EFs cropped/mat.grd")
ps <- raster("EFs/EFs cropped/ps.grd")
wm <- raster("EFs/EFs cropped/wm.grd")
wq <- raster("EFs/EFs cropped/wq.grd")
glu <- raster("Efs/EFs cropped/glu.grd")

ef.stack <- stack(ap, arid, cm, cq, elev,
                  evap, hii, mat, ps, 
                  rz, sp, st, wm, wq)
names(ef.stack) <- c("ap", "arid", "cm", "cq", "elev", "evap", "hii", "mat", "ps", "rz", "sp", "st", "wm", "wq")
names(glu) <- ("glu")  

# aggregate (note: land use is categorical, requires modal agg. function)  
ef.ag <- aggregate(ef.stack, fact = 200, fun = mean)
glu.ag <- aggregate(glu, fact = 200, fun = modal)
# should HII be calculated with a median score? That's what Haque et al. 2017 did ...   Ask Richard

# combine   
ef <- stack(glu.ag, ef.ag)

# get values + make dataframe
ef_v_na <- as.data.frame(getValues(ef))
# remove NAs (same method as for AVH)  
arid <- getValues(ef$arid)
ef_v <- ef_v_na[!is.na(arid), ] 

# Now both ef_v & AVH dfs same length, we merge
avh_ef <- data.frame(avh_200km, ef_v)

# make glu discrete factor
avh_ef$glu <- factor(avh_ef$glu, levels = c(1, 2, 3, 5, 6, 7), labels = c("agriculture", "forest", "grassland", "urban", "arid", "water")) 

# Note: have kept in zero values for AVH; i.e. where n = 0

# save 
write.csv(avh_ef, file = "C:/Users/s436862/Dropbox/Climate Matching/4. Results/AVH/200 km scale/200km SREs_EFs AVH.csv", row.names = F)


# Modelling it ---------------------------------------------
avh_ef <- read.csv("C:/Users/s436862/Dropbox/Climate Matching/4. Results/AVH/200 km scale/200km SREs_EFs AVH.csv")

# Model nomenclature: 
# avh; 
# 1, 5, 2 for scale; 
# a, b, c for EFs (a = all, b = redcued, c = s/t else);
# na, vl, l, m for community status;  
# e.g. avh_1_a_vl


##### EF correlation matrix 
# df with only the efs
efs <- select(avh_ef, ap, arid, cm, cq, elev, evap, hii, mat, ps, rz, sp, st, wm, wq)
# note: removed glu as categorical variables seem not to be welcome here; ask Richard how do
ef_cor_mat <- cor(efs, method = "pearson")
# csv
write.csv(ef_cor_mat, file = "C:/Users/s436862/Dropbox/Climate Matching/4. Results/AVH/200 km scale/200km EF correlation matrix AVH.csv")
# note: don't do rownames = F for this cos we want them in this instance :)
# note: HII didn't compute; not sure of the cause

# Reducing no. of EFs --------------------------------------------  
# everythang    
avh_2_na_a <- lm(ichao1 ~ glu + ap + arid + cm + cq + elev + evap + hii + mat + ps + rz + sp + st + wm + wq, data = avh_ef) 
# Ant says GLM is a better one for flexible
summary(avh_2_na_a)
# 100 km = 0.61
# here, 50 km, = 0.51

# avh_2_na_b (reduced efs on account of correlations)
# taking out ARID, MAT, WM, and SP + ST  
avh_2_na_b <- lm(ichao1 ~ glu + ap + cm + cq + elev + evap + hii + ps + rz + wq, data = avh_ef) 
summary(avh_2_na_b)
# @ 100 = 0.57
# here 0.49

# avh_2_na_b (reduced efs on account of significance)
# (note these are different between 100 and 50 km scales)
# common ones being taken out: CM, EVAP; but PS is nearing signif and RZ is not 
avh_2_na_c <- lm(ichao1 ~ glu + ap + cq + elev + hii + ps + wq, data = avh_ef)
summary(avh_2_na_c)
# 0.566 for 100; cool
# 0.48 here
# note: AP still not signif but idc <- almost is in 50!

# reducing stuff via VIF calculations    
avh_2_na_c <- vif(lm(ichao1 ~ glu + ap + cq + elev + hii + ps + wq, data = avh_ef))
avh_2_na_c
# therefore, removing ap and wq (same for 50)
avh_2_na_d <- lm(ichao1 ~ glu + cq + elev + hii + ps, data = avh_ef)
summary(avh_2_na_d)
# 0.42, pretty good, gonna keep it

# therefore, model with cells of <200 records  
avh_2_na_d <- lm(ichao1 ~ glu + cq + elev + hii + ps, data = avh_ef)
summary(avh_2_na_d)
# 0.42, pretty good, gonna keep it

# without <200
avh_ef_2_no_200 <- filter(avh_ef, n> 200)
avh_2_b_m <- lm(ichao1 ~ glu + ap + cq + elev + hii + ps + wq, data = avh_ef_2_no_200) 
summary(avh_2_b_m)


# 100 = 0.542
# 50 = 0.37 <- this must be because we are throwing aaway many more cells at this scale
# Reduce community numbers (low SR cells) -------------------------
# using ichao1 SR  

# no zeroes  
avh_ef_0 <- filter(avh_ef, n != 0)
avh_2_b_0 <- lm(ichao1 ~  glu + ap + cm + elev + hii + ps + wq, data = avh_ef_0) 
summary(avh_2_b_0)
# 100 = 0.566
# 200 = 0.58

# no v.low (<10)
avh_ef_vl <- filter(avh_ef, ichao1>= 10)
avh_2_b_vl <- lm(ichao1 ~  glu + ap + cm + elev + hii + ps + wq, data = avh_ef_vl) 
summary(avh_2_b_vl)
# 100 = 0.563
# 200 = 0.58

# no low (<50)
avh_ef_l <- filter(avh_ef, ichao1>= 50)
avh_2_b_l <- lm(ichao1 ~  glu + ap + cm + elev + hii + ps + wq, data = avh_ef_l) 
summary(avh_2_b_l)
# 100 = 0.557
# 200 = 0.579

# no med (<100)  
avh_ef_m <- filter(avh_ef, ichao1>= 100)
avh_2_b_m <- lm(ichao1 ~  glu + ap + cm + elev + hii + ps + wq, data = avh_ef_m) 
summary(avh_2_b_m)
# 100 = 0.542
# 200 = 0.59

# cool   

# -----------------------------------------


# poa x AP --------------------------------------


library(ggthemes)

#plot developed 
ggplot(dat2, aes(y = native.species, x = precipitation)) +
  geom_point(size = 1.5, pch = 1, alpha = 1) +
  geom_smooth(method = "lm", color = "black") +
  labs(x = "Precipitation (log)",
       y = "Native species richness (log)") +
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


ggplot(nat_df, aes(y = log(nat_v), x = log(precip_v))) +
  geom_point() +
  abline()
# R2 = 0.17 

n_sing <- rasterize(xy, precip_ag, field = spp, fun = function(x,...) {length(which(table(x)==1)) })

# number of doubletons ("                         " two occurrences in the sample)
n_doub <- rasterize(xy, precip_ag, field = spp, fun = function(x,...) {length(which(table(x)==2)) })

# chao1 estimator
chao <- poa_r + ((n_sing * (n_sing - 1)) / (2 * (n_doub + 1)))
plot(chao)
plot(log10(chao))

# introduced
int <- filter(poa, status == "introduced")
i.xy <- cbind(int$long, int$lat)
i.spp <- as.numeric(factor(int$species))

int_r <- rasterize(i.xy, precip_ag, field = n.spp, fun = function(x,...) {length(unique(na.omit(x)))})

# extract values 
int_v <- getValues(int_r)
precip_v <- getValues(precip_ag)
int_df <- data.frame(int_v, precip_v)

# plotting
m2 <- lm(log(int_v) ~ log(precip_v))
abline(m2)
summary(m2)
# R2 = 0.17 

m3 <- glm(int_v ~ int_v)
plot(m3)
summary(m3)

#test1
dat10 <-  filter(int_df, !precip_v == "NA" ) %>%
  filter(!int_v == "NA" )
m2 <- lm(log(dat10$int_v) ~ log(dat10$precip_v))
summary(m2)

m2 <- lm(log(dat10$int_v) ~ log(dat10$precip_v))
summary(m2)

plot(log(dat10$int_v) ~ log(dat10$precip_v))
abline(m2, col = "red", lwd = 5)


#test2
#anthony
head(dat10)
glimpse(dat10)
dat20 <- mutate(dat10, precipitation = log(as.numeric(precip_v)),
                introduced.species = log(as.numeric(int_v))) %>%
  filter(precip_v > 1 & int_v > 1)

m3 <- lm(dat20$introduced.species ~ dat20$precipitation)
summary(m3)

# dat3 <- select(dat2, precipitation, introduced.species) %>%
#   gather(va)

library(ggthemes)

#plot developed 
ggplot(dat20, aes(y = introduced.species, x = precipitation)) +
  geom_point(size = 1.5, pch = 1, alpha = 1) +
  geom_smooth(method = "lm", color = "black") +
  labs(x = "Precipitation (log)",
       y = "Introduced species richness (log))") +
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
  annotate("text", x = 7.8, y = 4.4, label = "italic(R) ^ 2 == 0.13",
           parse = TRUE, col = "blue") +
  #scale_y_continuous(limits=c(-2.5,2)) + 
  #scale_x_continuous(limits=c(0,4)) +
  ggtitle("Log annual precipitation ~ log introduced richness")

ggsave("C:/Users/s436862/Dropbox/Climate Matching/4. Results/Poa_x_AP/log.introduced_log.AP.jpeg", plot = last_plot(), scale = 1, dpi = 200, device = "jpeg")

ggplot(int_df, aes(y = log(int_v), x = log(precip_v))) +
  geom_point() +
  abline()

f <- lm(log(int_v) ~ log(precip_v))
summary(f)
plot(f)

# R2 = 0.17 

# save
write.csv(poa_x_AP, file = "C:\\Users\\s436862\\Dropbox\\Climate Matching\\4. Results\\Poa_x_AP\\poa_x_AP.csv")

pdf(file = "C:\\Users\\s436862\\Dropbox\\Climate Matching\\4. Results\\Chao corrected Poa\\poa_x_AP.pdf", width = 7, heigh = 5)
plot(log(poa_v) ~ log(precip_v))
abline(0, 1)
dev.off()

