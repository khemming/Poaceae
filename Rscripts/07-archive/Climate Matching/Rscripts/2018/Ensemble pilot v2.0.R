# Date created: 8/4/18
# Last modified: 8/5/18 (I have no recollection of writing this btw)

# Ensemble Pilot (#2 atm)
# using my data

library(biomod2)
library(dplyr)
library(tidyr)

# Formatting the data --------------------------------------------------------
# they have it in incidence format, for each cell of Australia, 
# the steps for this then are:
# (1) get cell identifier (# and/or xy) for each record
# (2) exclude cell id's that fall outside of continental Aus (1142 cells is magic number)
# (3) get presence/absence for each species per cell
# (4) transpose to 'incidence' format
  
  rm(list = ls())

  setwd("C:/Users/s436862/Dropbox/Climate Matching/1. Data files")

# Species
  int <- read.csv("AVH/AVH grass records.csv", header = T) %>%
    dplyr::filter(status == "introduced") %>%
    select(species, lat, long)
  xy <- cbind(int$long, int$lat)
  
# Aus
  aus <- raster("Australia raster/aus.grd")
  aus <- aggregate(aus, fac = 100, fun = mean)
  values(aus) <- 1:ncell(aus)

# Cell id (# and xy) for each species per cell 
  int$cell_id <- raster::extract(aus, xy)
  cell_id <- int$cell_id
# each cells' 'central' xy 
  cell_xy <- rasterToPoints(aus, spatial = F)
  colnames(cell_xy) <- c("x", "y", "cell_id") 

# each cell_id is not in the ocean or an offshore island 
  oz1 <- raster("EFs/EFs cropped/arid")
  oz1 <- aggregate(oz1, fact = 100, fun = mean)
  oz_val <- rasterToPoints(oz1, spatial = F) # gave me my magic 1142 :) 
  oz2 <- data.frame(oz_val)
  colnames(oz2) <- c("x", "y", "val")
  cell_xy <- data.frame(cell_xy)
# use the x or y col to join to cell_xy, and remove all rows which don't have a 'val' attached to them
  gh <- right_join(cell_xy, oz2)
  cell_xy <- gh 
  
# join to introduced species records 
  int_spp <- right_join(int, cell_xy) 
  
# transpose it to a wide format
  spp_cells <- select(int_spp, cell_id, x, y, species)

# need only 1 record per species per cell (presence/absence)
  spp_cells <- distinct(spp_cells) 
  spp_wide <- spread(spp_cells, key = species, value = cell_id, fill = 0)

# change values to 1 (from cell_id #)   
  spp_wide1 <- ifelse(spp_wide[ , 3:length(spp_wide)] >0, 1, 0) 

# put x and y back in there
  x <- spp_wide$x
  y <- spp_wide$y
  spp_wide1 <- data.frame(spp_wide1)
  spp_wide2 <- cbind(x, y, spp_wide1) 

# save ------------------------------------------------------
  write.csv(spp_wide2, file = "C:/Users/s436862/Dropbox/Climate Matching/4. Results/CSV/Int incidence Aus.csv", row.names = F)
 
  
# Species of interest ------------------------------------------------------------  
# Time to choose a species 
# Not sure/bothered how to order them highest to lowest incidence, but I really should
# Instead I have somehow got this Avena character 
  
# For my model to run I need (1) species data, (2) EF data and (3) xy data  
    rm(list = ls())
  
  spp <- read.csv("C:/Users/s436862/Dropbox/Climate Matching/4. Results/CSV/Int incidence Aus.csv", header = T)
  
  sum(spp[, "Avena.fatua"]) # it's in 129 cells
  myRespName <- 'Avena.fatua'

# data (column)
  myResp <- as.numeric(spp[,myRespName])

# XY
  xy <- spp[,c("x", "y")]
  
# EF layers 
  myExpl <- stack("C:/Users/s436862/Dropbox/Climate Matching/4. Results/Rasters/Aus EF stack")
 
# Build model ------------------------------------------------------------------
  myBiomodData <- BIOMOD_FormatingData(resp.var = myResp,
                                       expl.var = myExpl,
                                       resp.xy = xy,
                                       resp.name = myRespName)
# Need 'absence' data
# Don't know how or when this gets resolved
# Not sure about NAs (two grey points on the plot)
# Or test/training data
  
# Check whether the data are correctly formatted by printing and plotting
  myBiomodData
  plot(myBiomodData)
  # beeeeeeeeeeeeeeeautiful
  # there shouldn't be those two undefined there
  # deal with later
  
  
# Modelling ------------------------------------------------------------------
# Defining Models Options using default options.
  myBiomodOption <- BIOMOD_ModelingOptions()

# computing the models
# Notes from the pdf: "We are now ready for running the set of models on our species. As we do not have evaluation data, we will make 3-fold cross-validation (number controlled by "NbRunEval" argument) of our models by randomly splitting our data set into 2 subsets using "DataSplit"
# get all that?

myBiomodModelOut <- BIOMOD_Modeling(
  myBiomodData,
  models = c('SRE','CTA','RF','MARS','FDA'), # what's SRE, eh?
  models.options = myBiomodOption,
  NbRunEval=3,
  DataSplit=80,                              # 80% of data used? (20% training data?)
  Prevalence=0.5,
  VarImport=3,
  models.eval.meth = c('TSS','ROC'),
  SaveObj = TRUE,
  rescal.all.models = TRUE,
  do.full.models = FALSE,
  modeling.id = paste(myRespName,"FirstModeling",sep=""))
  # what's this 'did not converge' message?

# model summary
  myBiomodModelOut 

# models evaluations
  myBiomodModelEval <- get_evaluations(myBiomodModelOut)
# print the dimnames of this object
  dimnames(myBiomodModelEval)
  myBiomodModelEval <- get_evaluations(myBiomodModelOut)

# printing some score of the random forest model 
  myBiomodModelEval["TSS", "Testing.data","RF",,] # not bad?

# let's print the ROC scores of all selected models (Ok?)
  myBiomodModelEval["ROC","Testing.data",,,]  # that's fun

# assess the relative importance of the explanatory models
  get_variables_importance(myBiomodModelOut) # very cool
  # Note: "Relative importance of variable returned are raw data. It may be useful to normalise them to make them comparable one to another"
  
  
# Ensemble modeling  -----------------------------------------------------
# "Here comes one of the most interesting features of biomod2. BIOMOD_EnsembleModeling combines individual models to build some kind of meta-model. In the following example, we decide to exclude all models having a TSS score lower than 0.7."
# Since most of mine didn't reach that threshold, I shall choose 0.6  

myBiomodEM <- BIOMOD_EnsembleModeling(
  modeling.output = myBiomodModelOut,
  chosen.models = 'all',
  em.by='all',
  eval.metric = c('TSS'),
  eval.metric.quality.threshold = c(0.6),
  prob.mean = T,
  prob.cv = T,
  prob.ci = T,
  prob.ci.alpha = 0.05,
  prob.median = T,
  committee.averaging = T,
  prob.mean.weight = T,
  prob.mean.weight.decay = 'proportional' )

# Apparently we can "... easily access to the data and outputs of BIOMOD_Modeling using some specifc functions to  make your life easier"
  myBiomodEM # ok..

# evaulatiuon scores
  get_evaluations(myBiomodEM) # still have no idea what these outputs mean

# Projection ----------------------------------------------------
# Once the models are calibrated and evaluated, we might want to project the potential distribution of the species over space and time 
# This is made using BIOMOD_Projection   
# "All projections are stored directly on your hard drive" <-- concerned

# projection over the globe under current conditions
# this makes a file set where the wd is set. 
# So good idea to set wd somewhere appropriate before doing this bit  
  myBiomodProj <- BIOMOD_Projection(
    modeling.output = myBiomodModelOut,
    new.env = myExpl,
    proj.name = 'current',
    selected.models = 'all',
    binary.meth = 'TSS',
    compress = 'xz',
    clamping.mask = F,
    output.format = '.grd') 

# summary of crated oject
  myBiomodProj # ?

# files created (on hardrive)
  list.files("Avena.fatua/proj_current/")
# = some projections and rasters
# I grabbed one; let's see what it does
  j <- raster("Avena.fatua/proj_current/proj_current_Avena.fatua_TSSbin.grd")
  plot(j) # cool

# make some plots sub-selected by str.grep argument (?)
  plot(myBiomodProj, str.grep = 'MARS') 
  # super cool

# if you want to make custom plots, you can also get the projected map
  myCurrentProj <- get_predictions(myBiomodProj)  
  
# load environmental variables (... for the future ...) 
# Note this won't work because my original EFs won't match their future ones  
  myExplFuture = stack( system.file( "external/bioclim/future/bio3.grd",
                                     package="biomod2"),
                        system.file( "external/bioclim/future/bio4.grd",
                                     package="biomod2"),
                        system.file( "external/bioclim/future/bio7.grd",
                                     package="biomod2"),
                        system.file( "external/bioclim/future/bio11.grd",
                                     package="biomod2"),
                        system.file( "external/bioclim/future/bio12.grd",
                                     package="biomod2"))
  
  myBiomodProjFuture <- BIOMOD_Projection(
    modeling.output = myBiomodModelOut,
    new.env = myExplFuture,
    proj.name = 'future',
    selected.models = 'all',
    binary.meth = 'TSS',
    compress = 'xz',
    clamping.mask = T,
    output.format = '.grd')  

# (again) make some plots, sub-selected by str.grep argument
  plot(myBiomodProjFuture, str.grep = 'MARS') 

# The last step of this vignette is to make Ensemble Forcasting, that means to project the meta-models you have created with BIOMOD_EnsembleModeling. BIOMOD_EnsembleForecasting required the output of BIOMOD_EnsembleModeling and BIOMOD_Projection. It will combine the projections made according to models ensemble rules defined at the ensemble modelling step  
  myBiomodEF <- BIOMOD_EnsembleForecasting(
    EM.output = myBiomodEM,
    projection.output = myBiomodProj)
  
  myBiomodEF
  
# reduce layer names for plotting convegences
  plot(myBiomodEF) # cool

# Conclusion ---------------------------------------------------------
# This vignette describes how to build and test a range of models within biomod2 but also how to build ensemble projections under current and future conditions. With few modifications, you should be able to apply the default functions onto your own dataset
# and that I did :) 








