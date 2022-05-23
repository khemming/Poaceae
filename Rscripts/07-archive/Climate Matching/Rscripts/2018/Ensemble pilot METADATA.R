Date created: 10/4/18
Last updated:



Ensemble modelling pilot/experimental use METADATA

############## Example of how to use Biomod2 (in Stats/R how-tos) ################## 
working through: "An example of species distribution modeling with biomod2"

Ok, so so far the tutorial is going well
(Got some data which looks like incidence, rather than abundance)
And some EFs, stolen from WorldClim

There needs to be some pseudo-absences generated, which will be an intersting procedure
As yet, I cannot see them having made a vignette on how to do it, howver there is a helpfile online which looks like it'd give me some direction: https://rdrr.io/rforge/BIOMOD/man/pseudo.abs.html
(it's simply the helpfile from Biomod2, if you type in BIOMOD_FormatingData) Cool.

Furthermore:
"If your environmental data are in matrix (or data.frame) format, you have to give a species as vector having a length that match with the number of rows of your environmental dataset. That implies to add NA's in all points where you do not have information on species presence-absence."
-- Got it! But Zeroes need to be NAs. Not too hard to do (if they aren't already).

Before I use my species-EF data(frame), I shall crack on with their tutorial

# Ensemble modelling -----------------------------------------
NOTE 7:
You can controle the way formal models are combined with em.by argument. The vignette "Ensem-
bleModelingAssembly" illustrate the offered possibilities
Find this (I think) at: https://rpubs.com/dgeorges/38564
