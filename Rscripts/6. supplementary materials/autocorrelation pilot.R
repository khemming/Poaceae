####################################################
# spatial autocorrelation
####################################################

# library ------------------------------------------
  library(tidyverse)
  library(spdep)

  rm(list = ls())

# data ---------------------------------------------
  load("Data files/rdata/model_data.Rdata")
  aus <- raster("Data files/Australia/Australia 100 km.grd")
  aus_df <- read.csv("Data files/Australia/Australia 100 km pewc.csv")
  head(aus_df)

# correct number of cells ----------------------------
# scope: to get the right number (and position) of cells which are included in the model
  plot(aus)
  cell_list <- rasterToPoints(aus)
  cell_list <- data.frame(cell_list)
  names(cell_list) <- c("x", "y", "cell_id")
  aus_xy <- left_join(aus_df, cell_list, by = "cell_id")
  head(aus_xy)
  
  aus_xy$n_c3 <- ifelse(!is.na(getValues(native_C3)), "present", "absent")
  aus_xy$n_c4 <- ifelse(!is.na(getValues(native_C4)), "present", "absent")
  aus_xy$nn_c3 <- ifelse(!is.na(getValues(nonnative_C3)), "present", "absent")
  aus_xy$nn_c4 <- ifelse(!is.na(getValues(nonnative_C3)), "present", "absent")
  
# native C3 test -------------------------------
# subset for cell coordinates of only native c3
  nat_c3_xy <- aus_xy %>% filter(n_c3 == "present") %>%
          dplyr::select(x, y)
  
# create spatially weighted neighbour values for each cell
  nb_listw <- nb2listw(dnearneigh(as.matrix(nat_c3_xy), 0, 120, longlat = T), zero.policy = TRUE, style = "W")
  print.listw(nb_listw, zero.policy = T)
# run test
  moran_nc3 <- lm.morantest(m_nc3, nb_listw, zero.policy = T, spChk = F, resfun = residuals, naSubset = T)  
  
# THEY'RE THE SAME LENGTH  
  length(nb_listw$neighbours)
  length(residuals(m_nc3))
  
  coords <- coordinates(aus)
  plot <- graph2nb(nb_listw, nat_c3_xy)
  
# --------------------------------------------  
  data(meuse.grid)
  head(meuse.grid)
  coordinates(meuse.grid) <- c("x", "y")
  head(meuse.grid)
  gridded(meuse.grid) <- TRUE
  head(meuse.grid)
  dst <- max(slot(slot(meuse.grid, "grid"), "cellsize"))
  head(dst)
  mg_nb <- dnearneigh(coordinates(meuse.grid), 0, dst)
  mg_nb
  
  

#coordinates(nat_c3_xy) <- c("x", "y")
  head(nat_c3_xy)
#gridded(nat_c3_xy) <- TRUE
  head(nat_c3_xy)
  dst <- max(slot(slot(nat_c3_xy, "grid"), "cellsize"))
  head(dst)
  nc3_nb <- dnearneigh(as.matrix(nat_c3_xy), 0, 100, longlat = TRUE)
  summary(nc3_nb)
  nbw_c <- nb2listw(nc3_nb, zero.policy = TRUE)
  print(nbw_c, zero.policy = TRUE)
  
  moran_nc3 <- lm.morantest(m_nc3, nbw_c, zero.policy = T, resfun = residuals)  
 
  
  m_nc3$model
  coord <- rasterToPoints(aus)[,1:2]
  head(coord)
  cell_dists <- as.matrix(dist(cbind(coord[,1], coord[,2])))
  cell_dists_inv <- 1/cell_dists  
  diag(cell_dists_inv) <- 0  
  cell_dists_inv[1:5,1:5]
  
  nbw_list <- nb2listw(knn2nb(knearneigh(coord, k = 6)), style = "W")
  
  
  
  moran_nc3 <- lm.morantest(m_nc3, nbw_list, zero.policy = T, resfun = residuals)  
  
  
# gls ----------------------------------------------------  
  library(MuMIn)
  library(ape)
  library(nlme)  
  library(tidyverse)
  
  rm(list = ls())
  
# data ---------------------------------------------
  load("Data files/rdata/model_data.Rdata")
  aus <- raster("Data files/Australia/Australia 100 km.grd")
  aus_df <- read.csv("Data files/Australia/Australia 100 km pewc.csv")
  head(aus_df)
  xy <- aus_xy %>% filter(n_c3 == "present") %>%
    dplyr::select(x, y)
  
# load data
  bird.diversity <- read.table("C:/Users/s436862/Downloads/bird.diversity.txt", head=T)
  str(bird.diversity) # each data point has an associated x and y
  model <- gls( log(Bird_diversity + 1) ~ log(Tree_diversity + 1) , data = bird.diversity )
  plot( log(Bird_diversity + 1) ~ log(Tree_diversity + 1) , data = bird.diversity )
  abline(model)

# semivariogram
  semivario <- Variogram(model, form = ~Lon_x + Lat_y, resType = "normalized")
  plot(semivario, smooth = TRUE)
  
# trying Moran's I
  geo <- cbind(bird.diversity$Lon_x, bird.diversity$Lat_y)
  head(geo)
  samples.dist <- as.matrix(dist(geo))
  head(samples.dist)
  samples.dist.inv <- 1/samples.dist
  diag(samples.dist.inv) <- 0
  Moran.I(log(bird.diversity$Bird_diversity+1) , samples.dist.inv ,alternative="greater")
  
# given there is positive spatial a/c we ...
  
# remove spatial a/c effects
  model <- gls( log(Bird_diversity + 1) ~ log(Tree_diversity + 1) , data = bird.diversity )
  linear.autocor <- gls( log(Bird_diversity + 1) ~ log(Tree_diversity + 1) , correlation = corLin(form = ~Lon_x + Lat_y, nugget=T), data = bird.diversity )
  AIC(linear.autocor)
  gaussian.autocor <- gls( log(Bird_diversity + 1) ~ log(Tree_diversity + 1) , correlation = corGaus(form = ~Lon_x + Lat_y, nugget=T), data = bird.diversity )
  AIC(gaussian.autocor) # lower
  
# check out our plot using Tukey-Anscombe Plot (Residual vs. Fitted)
  plot(fitted(gaussian.autocor), residuals(gaussian.autocor))
  abline(h=0,lty=3)
# check out semivarigrame to see if we removed the variance of spatial a/c
  semivario <- Variogram(gaussian.autocor, form = ~Lon_x + Lat_y, resType = "normalized")
  plot(semivario, smooth = TRUE)

# use my data -----------------------------------------------------------------
  rm(list = ls())
  load("Data files/rdata/model_data.Rdata")
  aus <- raster("Data files/Australia/Australia 100 km.grd")
  aus_df <- read.csv("Data files/Australia/Australia 100 km pewc.csv")
  head(aus_df)
  xy <- aus_xy %>% filter(n_c3 == "present") %>%
    dplyr::select(x, y)
  
# load data
   model <- 
  plot( log(Bird_diversity + 1) ~ log(Tree_diversity + 1) , data = bird.diversity )
  abline(model)
  
# semivariogram
  semivario <- Variogram(model, form = ~Lon_x + Lat_y, resType = "normalized")
  plot(semivario, smooth = TRUE)
  
# trying Moran's I
  geo <- cbind(bird.diversity$Lon_x, bird.diversity$Lat_y)
  samples.dist <- as.matrix(dist(geo))
  samples.dist.inv <- 1/samples.dist
  diag(samples.dist.inv) <- 0
  Moran.I(log(bird.diversity$Bird_diversity+1) , samples.dist.inv ,alternative="greater")
  
# given there is positive spatial a/c we ...
  
# remove spatial a/c effects
  model <- gls( log(Bird_diversity + 1) ~ log(Tree_diversity + 1) , data = bird.diversity )
  linear.autocor <- gls( log(Bird_diversity + 1) ~ log(Tree_diversity + 1) , correlation = corLin(form = ~Lon_x + Lat_y, nugget=T), data = bird.diversity )
  AIC(linear.autocor)
  gaussian.autocor <- gls( log(Bird_diversity + 1) ~ log(Tree_diversity + 1) , correlation = corGaus(form = ~Lon_x + Lat_y, nugget=T), data = bird.diversity )
  AIC(gaussian.autocor) # lower
  
# check out our plot using Tukey-Anscombe Plot (Residual vs. Fitted)
  plot(fitted(gaussian.autocor), residuals(gaussian.autocor))
  abline(h=0,lty=3)
# check out semivarigrame to see if we removed the variance of spatial a/c
  semivario <- Variogram(gaussian.autocor, form = ~Lon_x + Lat_y, resType = "normalized")
  plot(semivario, smooth = TRUE)