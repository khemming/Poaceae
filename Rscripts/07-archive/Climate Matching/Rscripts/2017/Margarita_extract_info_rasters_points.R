library(dismo)
library(raster)
library(foreach)
library(doParallel)
library(dismo)
library(raster)
library(sp)
library(sm)
library(rgl)
library(rpanel)
library(tcltk)
library(PresenceAbsence)
library(zoo)
library(maptools)

#load data
setwd("C:/Margarita/ENM")

#rasters
prec = raster("prec_10m.asc")
plot(prec)

temp = raster("temp_10m_.asc")
#makes mask
rs = stack(prec, temp)

plot(rs)

#load data and subsets
acrob <- read.table("C:/Margarita/ENM/duplicates_2/aepy_fur.csv",  header=TRUE,  sep=',')
head(acrob)
acrob2 <- acrob[,-1]

#extract values of grids
presvals1 <- extract(rs, acrob2)
XY_presvals1 <- data.frame(cbind(acrob, presvals1))
#set.seed(1963)
data <- dplyr::bind_rows(XY_presvals1)
str(data)
write.csv(data, file="C:/Margarita/ENM/background_files/ac_pyg_bckkyle_.csv", row.names = FALSE)

##this is to extract background points
#backgr <- randomPoints(rs, 500)
#absvals <- extract(rs, backgr)
#XY_backgr <- data.frame(cbind(backgr, absvals))
#pb1 <- c(rep(1, nrow(presvals1)), rep(0, nrow(absvals)))

