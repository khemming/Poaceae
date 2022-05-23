# 1Chao1 & the SpadeR package

# Data type; I think we have 1, which is:
# Type (1) abundance data (datatype = "abundance"): Input data consist of species (in rows) by community (in columns) matrix. 
# The entries of each row are the observed abundances of a species in N communities.

# therefore cell# would be columns (= community)
# unique Species would make up each row
# and their abundance (frequency of records) in each grid would be noted along the grid cells (=communities); can't have NAs, gotta be 0's

# Can I do this? Hmm.

# this will only be required when I know what grid cell size is appropriate. 

# rasters: what kind of information is contained within thee?
# can i get the lat/longs of the 4 pts which define each of the grid cells?


# Data wrangling: turning AVh data into species x community ---------------
# rows = unique species; columns community (i.e. grid cells)
# abundance data contained within (i.e. record #)

rm(list = ls())

library(SpadeR)
library(rasterVis)
library(ggmap)
library(tidyr)
library(rgdal)
library(maptools)
library(raster)
library(dismo)

setwd("C:/Users/s436862/Dropbox/Climate Matching/1. Data files")

# SpadeR examples
  abu <- ChaoSpeciesData$Abu
  sum(abu[,1])

ChaoSpecies(abu, datatype = "abundance", k = 10, conf = 0.95)




# raster template
b <- raster("EFs/EFs cropped/arid.grd")
raster <- aggregate(b, fact = 100, fun = mean)

# Poa
poa <- read.csv("AVH/AVH grass records.csv", header = T) %>%
  dplyr::select(species, lat, long)

# what I'd normally calculate
p <- poa
xy <- cbind(p$long, p$lat)
spp <- as.numeric(factor(p$species))


### not test  
# number of records (n)
n <- rasterize(xy, raster, fun = function(x,...) {length(na.omit(x)) })
# actual richness (a)
a <- rasterize(xy, raster, field = spp, fun = function(x,...) {length(unique(na.omit(x))) }) 
# single- and double-tons
sing <- rasterize(xy, raster, field = spp, fun = function(x,...) {length(which(table(x)==1)) })
doub <- rasterize(xy, raster, field = spp, fun = function(x,...) {length(which(table(x)==2)) })


### test 1
# add in species names
xys <- cbind(p$long, p$lat, spp)


# record number + species
n <- rasterize(xy, raster, field = spp, fun = function(x,...) c(length(na.omit(x)), count(spp)) )

# test 2
r <- raster(ncol=36, nrow=18, crs='+proj=utm +zone=14 +datum=WGS84')
r[] <- 1:ncell(r)
rxy <- cbind(x=-50, y=seq(-80, 80, by=20))

# extract
e <- raster::extract(r, rxy, buffer=10)
ee <- t(data.frame(e))
rownames(ee) <- NULL

data.frame(xy, ee)

# test 3
n[is.na(n)] <- 0
y[] <- yFromCell(n, col=1:ncol(n))
ph <- xyFromCell(n, ncell(n))

r <- raster(system.file("external/test.grd", package="raster"))
#make copies which will hold cell identifiers, x & y values
ry <- rx <- rc <- r

#define raster of cell numbers for the non-NA cells
rc[] <- seq(1:length(r))
# for some reason, this is not working:  rc <- mask[rc,r]
# but this works fine:
rc[is.na(r)] <-NA

#assign coordinate values to corresponding rasters
ry[] <- yFromCell(rc, getValues(rc))
rx[] <- xFromCell(rc, getValues(rc))

# stack em up and view
s <- stack(r,rc,rx,ry)
names(s) <- c("values", "cell #", "x-coord", "y-coord")
plot(s)


# test 4
library(dismo)
library(rgdal)
library(raster)

#C:\Users\s436862\Dropbox\Stats\Example data\germany-places-shape
germany.mrc <- dismo::gmap("Australia")
germany.places <- readOGR(dsn = "C:/Users/s436862/Dropbox/Stats/Example data/germany-places-shape/places.shp", layer = "places")
projection(germany.places) <- CRS("+proj=lonlat +ellps=WGS84")
# Reproject to RasterLayer's CRS
germany.places.mrc <- spTransform(germany.places, CRS(projection(germany.mrc)))
# getting 10 random places in Germany
set.seed(35)
germany.places.mrc.sample <- germany.places.mrc[sample(nrow(germany.places.mrc), 10), ]
# getting data from those 10
data <- data.frame(coordinates(germany.places.mrc.sample),
                   germany.places.mrc.sample$name, 
                   raster::extract(germany.mrc, germany.places.mrc.sample))
names(data) <- c("x", "y", "name", "value")


australia.mrc <- dismo::gmap("Australia")
n <-data.frame(n)

# got spdf
n1 <- SpatialPointsDataFrame(xy, poa)
projection(n1) <- CRS("+proj=lonlat +ellps=WGS84")
# let's make a raster w cells, and try and add it to it as another variable
b <- raster("EFs/EFs cropped/arid.grd")
raster <- aggregate(b, fact = 100, fun = mean)
# w <- as.numeric(getValues(raster))
# spdf <- SpatialPointsDataFrame(raster, w)

# What would be cool if I could tack on corresponding grid #, next to lat/long

# e <- raster::extract(n, n1, method = "simple")
# projection(e) <- CRS("+proj=lonlat +ellps=WGS84")
# e1 <- SpatialPointsDataFrame(e, xy)
# f <- aggregate(n1, by = g1, FUN = sum)
# 
# g <- matrix(nrow = length(p$species), ncol = 1)
# g[1:length(g),] <- 100
# g1 <- list(g)

# basically where I have gotten to
n1

n2 <- rasterize(n1, raster, field = n1$species, fun = function(x,...) length(x))
# Getting centraol point xy of aggregated cells 
h <- xyFromCell(raster, 1:ncell(raster))

i <- raster::extract(n2, h, method = "simple", buffer = 100000)



