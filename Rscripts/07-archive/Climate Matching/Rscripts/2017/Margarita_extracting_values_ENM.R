library(dismo)
library(raster)
library(maxlike)
library(foreach)
library(doParallel)
library(dismo)
library(raster)
library(sp)
library(sm)
library(rgl)
library(rpanel)
library(tcltk)
library(rJava)
library(PresenceAbsence)
library(zoo)


library(dismo)
library(maptools)

###first steps done in ARCGIS
#mask raster and same extent#

#load data
setwd("C:/Margarita/ENM/duplicates_2")


setwd("C:/Margarita/ENM")
#mu = read.csv("C:/Margarita/ENM/exp_mult.csv", header=T)


#rasters
r1 <- raster("prec_10m.asc")
plot(r1)

r2 = raster("temp_10m_.asc")
#makes mask
rs = stack(r1, r2)

plot(rs)

#load data and subsets
acrob <- read.table("C:/Margarita/ENM/duplicates_2/aepy_fur.csv",  header=TRUE,  sep=',')

head(acrob)
acrob2 <- acrob[,-1]



####
####
###code working for background points per spp

presvals1 <- extract(rs, acrob2)
XY_presvals1 <- data.frame(cbind(acrob, presvals1))
set.seed(1963)
backgr <- randomPoints(rs, 500)
absvals <- extract(rs, backgr)
XY_backgr <- data.frame(cbind(backgr, absvals))
pb1 <- c(rep(1, nrow(presvals1)), rep(0, nrow(absvals)))

pre_bck <- data.frame(cbind(acrob$species, pb1, rbind(presvals1, absvals)))
pb4 <- dplyr::bind_rows(XY_presvals1, XY_backgr)
str(pb4)
pb4  [1,1] <- NA
pb4[is.na(pb4$species)] <- "Acrobates_pygmaeus"

levels(pb4$species)

##to double check there are no NA values left from extracting on raster stack)
complete.cases(pb4)
pb5 <- pb4[complete.cases(pb4), ]
str(pb5)

###to bind everything
pb5 <- data.frame (cbind(pb4, pre_bck$pb1))

plot(pb5$x, pb5$y)
#to write file###


write.csv(pb5, file="C:/Margarita/ENM/background_files/ac_pyg_bck.csv", row.names = FALSE)


