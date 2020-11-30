# Date created: 10/4/18
# Last updated:



# Ensemble modelling pilot/experimental use

# Basically, I am just going to play around with biomod2, to get a feel for what I will do with it
# Wish me luck

# What I will do is, after going through the tutorial once with their data, I will have a go with my data, and overwrite their stuff (if you want the OG materials, refer to the original how-to)


############## Example of how to use Biomod2 (Simple_species_modelling) #################
# working through: "An example of species distribution modeling with biomod2"

# This vignette illustrates how to build, evaluate and project a single species distribution model using biomod2 package. The three main modeling steps, described bellow, are the following :
# 1. formatting the data
# 2. computing the models
# 3. making the projections
# The example is deliberately simple (few technicals explanations) to make sure it is easy to transpose to your own data relatively simply.
# Here we are going to modeled the current and future (2050) distribution of Gulo Gulo.


  library(biomod2)

  setwd("C:/Users/s436862/Dropbox/Climate Matching/4. Results/Ensemble/Pilot")

# Formatting the data ------------------------------------------------------------------
# lspecies data
  spp1 <- read.csv(system.file("external/species/mammals_table.csv",
                                    package="biomod2"))
  head(spp1)
  # incidence (presence/absence) data of each species across Xs (different communities?)
  
# species of interest ----------------------------------------------------------------
  myRespName1 <- 'GuloGulo'
# data (column) 
  myResp1 <- as.numeric(spp1[,myRespName1])
# XY
  xy1 <- spp1[,c("X_WGS84", "Y_WGS84")]

# EF layers
# Note: I have these for Aus, but I will just see what doing one does for this
  myExpl1 <- raster(system.file( "external/bioclim/current/bio3.grd", package="biomod2"))
  myExpl1
  plot(myExpl1)
  j <- getValues(myExpl1)
  sum(is.na(j)) # so lots of NAs (not zeroes)
  
 myExpl1 = stack( system.file( "external/bioclim/current/bio3.grd",
                               package="biomod2"),
                  system.file( "external/bioclim/current/bio4.grd",
                               package="biomod2"),
                  system.file( "external/bioclim/current/bio7.grd",
                               package="biomod2"),
                  system.file( "external/bioclim/current/bio11.grd",
                               package="biomod2"),
                  system.file( "external/bioclim/current/bio12.grd",
                               package="biomod2"))

 myBiomodData1 <- BIOMOD_FormatingData(resp.var = myResp1,
                                      expl.var = myExpl1,
                                      resp.xy = xy1,
                                      resp.name = myRespName1)
 # Need absence data -- so I need to generate these; some help file helps, i think
 
# "At this point, check whether the data are correctly formatted by printing and plotting the created object." (?)
  myBiomodData1
  # cool
  plot(myBiomodData1)
  # very cool
 
 
# Modelling ------------------------------------------------------------------
# Defining Models Options using default options.
  myBiomodOption <- BIOMOD_ModelingOptions()
  # scary big; leaving for now 
  # ah, this produces the set of instructions for parameterisation (etc. I assume) that we use later
  # Cool
 
# computing the models
  myBiomodModelOut <- BIOMOD_Modeling(
    myBiomodData,
    models = c('SRE','CTA','RF','MARS','FDA'),
    models.options = myBiomodOption,
    NbRunEval=3,
    DataSplit=80,
    Prevalence=0.5,
    VarImport=3,
    models.eval.meth = c('TSS','ROC'),
    SaveObj = TRUE,
    rescal.all.models = TRUE,
    do.full.models = FALSE,
    modeling.id = paste(myRespName,"FirstModeling",sep=""))
  
  myBiomodModelOut # ok?
  
#  models evaluations
  myBiomodModelEval <- get_evaluations(myBiomodModelOut)
  
# print the dimnames of this object
  dimnames(myBiomodModelEval)
  
# printing some score of the random forest model 
  myBiomodModelEval["TSS", "Testing.data","RF",,]
  
# let's print the ROC scores of all selected models (Ok?)
  myBiomodModelEval["ROC","Testing.data",,,]  
 # that's fun
  
# assess teh relative importance of the explanatory models
  get_variables_importance(myBiomodModelOut)
  
# Ensemble modeling  -----------------------------------------------------
# "Here comes one of the most interesting features of biomod2. BIOMOD_EnsembleModeling combines individual models to build some kind of meta-model. In the following example, we decide to exclude all models having a TSS score lower than 0.7."
  
  myBiomodEM <- BIOMOD_EnsembleModeling(
    modeling.output = myBiomodModelOut,
    chosen.models = 'all',
    em.by='all',
    eval.metric = c('TSS'),
    eval.metric.quality.threshold = c(0.7),
    prob.mean = T,
    prob.cv = T,
    prob.ci = T,
    prob.ci.alpha = 0.05,
    prob.median = T,
    committee.averaging = T,
    prob.mean.weight = T,
    prob.mean.weight.decay = 'proportional' )
  
# Apparently we can "... easily access to the data and outputs of BIOMOD_Modeling using some specifc functions to   make your life easier"
  myBiomodEM # ok..
  
# evaulatiuon scores
  get_evaluations(myBiomodEM)

  
# Projection ----------------------------------------------------
#  Once the models are calibrated and evaluated, we might want to project the potential distribution of   the species over space and time. 
#  This is made using BIOMOD_Projection   
# "All projections are stored directly on your hard drive" -- concerned
  
# projection over the globe under current conditions
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
  list.files("GuloGulo/proj_current/")
  # = some projections and rasters
# I grabbed one; let's see what it does
  j <- raster("GuloGulo/proj_current/proj_current_GuloGulo_TSSbin.grd")
  plot(j)
  # cool
  
# make some plots sub-selected by str.grep argument (?)
  plot(myBiomodProj, str.grep = 'MARS') 
  
# if you want to make custom plots, you can also get the projected map
  myCurrentProj <- get_predictions(myBiomodProj)  
  
# load environmental variables (for the future.) (Ooooooooooooh...)
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
  # so this 'GulaGula' likes Siberia, huh?
  # and are these are perhaps different to the original ones? Depending on future climate
  
# The last step of this vignette is to make Ensemble Forcasting, that means to project the meta-
# models you have created with BIOMOD_EnsembleModeling. BIOMOD_EnsembleForecasting required
# the output of BIOMOD_EnsembleModeling and BIOMOD_Projection. It will combine the projections
# made according to models ensemble rules defined at the ensemble modelling step.  
  myBiomodEF <- BIOMOD_EnsembleForecasting(
    EM.output = myBiomodEM,
    projection.output = myBiomodProj)
  
  myBiomodEF
# reduce layer names for plotting convegences
  plot(myBiomodEF)
 
# Conclusion ---------------------------------------------------------
# This vignette describes how to build and test a range of models within biomod2 but also how to build ensemble projections under current and future conditions. With few modications, you should be able to apply the default functions onto your own dataset.  
  
  
  
  