#   R code for:
#   A protocol for data exploration to avoid common statistical problems
#   Methods in Ecology and Evolution
#
#   Alain F. Zuur (1,2), Elena N. Ieno (1,2), Chris S. Elphick (3)
#
#   1 Highland Statistics Ltd., 6 Laverock Road, Newburgh, AB41 6FN, UK
#   2 University of Aberdeen, Oceanlab, Main Street, Newburgh, AB41 6AA, UK
#   3 University of Connecticut, Department of Ecology and Evolutionary Biology and Center for Conservation Biology, 75 N. Eagleville Road, U-43, Storrs, CT 06269-3043, USA
#
#
#   This file was produced by:
#   Alain Zuur (highstat@highstat.com)
#   www.highstat.com
#
#   A detailed explanation of the R code used in the paper can be found in:
#   A Beginner's Guide to R (2009).
#   Zuur, AF, Ieno, EN, Meesters, EHWG. Springer
#   http://www.springer.com/statistics/computational/book/978-0-387-93836-3
#
#
######################################################################
#    DISCLAIMER
#    This program is free software; you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation; either version 2 of the License, or
#    (at your option) any later version.
#
#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.
#
######################################################################
#
#    The R code below was tested on R version 2.9.1
#    October 2009



setwd("C:/Users/s436862/Dropbox/Sources/Supplementary Figues/Zuur et al. 2009_Appendix_S1")


#Figure 2
sparrows <- read.table(file = "SparrowsElphick.txt", header = TRUE)
str(sparrows)


par(mfrow= c (1,2), mar = c(5,4,2,1))
boxplot(Sparrows$wingcrd,  ylab = "Wing length (mm)")
dotchart(Sparrows$wingcrd, xlab = "Wing length (mm)",
         ylab = "Order of the data")



#Figure 3
library(lattice)
Z <- cbind(Sparrows$wingcrd, Sparrows$tarsus,  Sparrows$head,
           Sparrows$culmen,  Sparrows$nalospi, Sparrows$wt)

colnames(Z) <- c("wing length", "tarsus length", "head length",
                 "culmen length", "nalospi to bill tip", "weight")

dotplot(as.matrix(Z), groups = FALSE,
        strip = strip.custom(bg = 'white',
        par.strip.text = list(cex = 0.8)),
        scales = list(x = list(relation = "free"),
                      y = list(relation = "free"),
                      draw = FALSE),
        col = 1, cex  = 0.5, pch = 16,
        xlab = "Value of the variable",
        ylab = "Order of the data from text file")
#
###############################################################
#Figure 4
#Godwit intake rates

Godwits <- read.table(file="Godwits.txt", header=TRUE)

#Data info:
#Sex
#sex 1=female		0 should go out
#sex 2=male

#Age
#1= adult
#2= juvenile
#0=UNKNOWN

#Location
#locationa=0
#locationb=1

#Period
#period=0 ;southern summr
#period=1; prepare for migration
#period=2; sourthern winter


library(lattice)

Godwits$fSEX <- factor(Godwits$SEX, levels = c(0, 1, 2),
                       labels = c("Not", "Female", "Male"))
Godwits$fPERIOD <- factor(Godwits$PERIOD, levels = c(0, 1, 2),
                          labels = c("Summer", "Pre-migration", "Winter"))

bwplot(mgconsumed ~ fPERIOD | fSEX, data = Godwits,
   strip = strip.custom(bg = 'white'),   subset = SEX!=0,
   cex = .5, layout = c(2, 1),
   xlab = "Migration period", ylab = "Intake rate",
   par.settings = list(
      box.rectangle = list(col = 1),
      box.umbrella  = list(col = 1),
      plot.symbol   = list(cex = .5, col = 1)),
       scales = list(x = list(relation = "same"),
                     y = list(relation = "same")))


##############################################################

#Figure 5
Sparrows <- read.table(file="SparrowsElphick.txt", header=TRUE)

Sparrows$fMonth<-factor(Sparrows$Month,
                        levels = c(5, 6, 7, 8, 9, 10),
                        labels = c("May", "June", "July", "August",
                                   "Sept.", "Oct."))


Sparrows$I1 <- Sparrows$fMonth =="June" |
               Sparrows$fMonth =="July" |
               Sparrows$fMonth =="August"


hist(Sparrows$wt[Sparrows$I1],
     xlab = "Weight (g)", breaks = 30,
     main = "", ylab = "Frequency")



library(lattice)
histogram( ~ wt | fMonth, type = "count",
    xlab = "Weight (g)",
    ylab = "Frequency",
    nint=30,layout=c(1,3),
    strip.left = strip.custom(bg = 'white'),
    strip = F,
    col.line = "black", col = "white",
    scales = list(x = list(relation = "same"),
                  y = list(relation = "same"),
                  draw = TRUE),
    subset = fMonth =="June" | fMonth == "July" |fMonth == "August",
    data = Sparrows)
    
###############################################################

#Figure 6: Not an R graph


###############################################################
#Figure 7:

RiceField <- read.table(file="ElphickBirdData.txt", header = TRUE)
par(mar = c(4, 4, 3, 2))
plot(table(round(RiceField$AREA * RiceField$AQBIRDS)),
    type = "h",
    xlim = c(0, 100),
    xlab = "Observed values", ylab = "Frequency")


###############################################################
#Figure 8
RiceField <- read.table(file="ElphickBirdData.txt", header = TRUE)

#These are all the species
AllS <- c(
"TUSW",     "GWFG",     "WHGO",     "CAGO",     "MALL",
"GADW",     "GWTE",     "CITE",     "UNTE",     "AMWI",     "NOPI",
"NOSH",     "RIDU",     "CANV",     "BUFF",     "WODU",     "RUDU",
"EUWI",     "UNDU",     "PBGB",     "SORA",     "COOT",     "COMO",
"AMBI",     "BCNH",     "GBHE",     "SNEG",     "GREG",     "WFIB",
"SACR",     "AMAV",     "BNST",     "BBPL",     "KILL",     "LBCU",
"GRYE",     "LEYE",     "LBDO",     "SNIP",     "DUNL",     "WESA",
"LESA",     "PEEP",     "RUFF",     "UNSH",     "RBGU",     "HEGU",
"CAGU",     "GUSP")

#Determine species richness
Richness <- colSums(RiceField[,AllS] > 0, na.rm = TRUE)

#Remove all covariates
Birds  <- RiceField[,AllS]

#To reduce the of variables in the figure, we only used the
#20 species that occured at more than 40 sites.
#As a result, N = 20. Else it becomes a mess.
Birds2 <- Birds[, Richness > 40]
N <- ncol(Birds2)


AllNames <- names(Birds2)
A <- matrix(nrow = N, ncol = N)

for (i in 1:N){
  for (j in 1:N){
    A[i,j] <- sum(RiceField[,AllS[i]]==0  & RiceField[,AllS[j]]==0, na.rm=TRUE)
    }}


A1 <- A/2035
print(A1, digits = 2)
rownames(A1) <- AllNames
colnames(A1) <- AllNames


library(lattice)

panel.corrgram.2 <- function(x, y, z, subscripts, at = pretty(z), scale = 0.8, ...)
{
    require("grid", quietly = TRUE)
    x <- as.numeric(x)[subscripts]
    y <- as.numeric(y)[subscripts]
    z <- as.numeric(z)[subscripts]
    zcol <- level.colors(z, at = at, ...)
    for (i in seq(along = z))
    {
        lims <- range(0, z[i])
        tval <- 2 * base::pi *
            seq(from = lims[1], to = lims[2], by = 0.01)
        grid.polygon(x = x[i] + .5 * scale * c(0, sin(tval)),
                     y = y[i] + .5 * scale * c(0, cos(tval)),
                     default.units = "native",
                     gp = gpar(fill = zcol[i]))
        grid.circle(x = x[i], y = y[i], r = .5 * scale,
                    default.units = "native")
    }
}




levelplot(A1,xlab=NULL,ylab=NULL,
    at=do.breaks(c(0.5,1.01),101),
    panel=panel.corrgram.2,
    scales=list(x=list(rot=90)),
    colorkey=list(space="top"),
    col.regions=colorRampPalette(c("red","white","blue")))


#Grey colours
levelplot(A1,xlab=NULL,ylab=NULL,
    at=do.breaks(c(0.5,1.01),101),
    panel=panel.corrgram.2,
    scales=list(x=list(rot=90)),
    colorkey=list(space="top"),
    col.regions=colorRampPalette(c(grey(0.8),grey(0.5),grey(0.2))))



####################################################################
#Figure 9 & Table 1
Sparrows2 <- read.table(file = "VegSamplesV1.txt", header = TRUE)
#Different Sparrow object

names(Sparrows2)
# [1] "Year"                "Site"                "UniversalPlotName"
# [4] "Banded"              "PtCountsum"          "Avgmaxht"
# [7] "Avgdens"             "ht.thatch"           "S.patens"
#[10] "Distichlis"          "S.alternifloraShort" "S.alternifloraTall"
#[13] "Juncus"              "Bare"                "Other"
#[16] "Phragmites"          "Shrub"               "Tallsedge"
#[19] "Water"

#Load our own library files
source("HighstatLib.R")

#Seclect covraites
Z<-Sparrows2[,c("Avgmaxht", "Avgdens", "ht.thatch",
                "S.patens", "Distichlis", "S.alternifloraShort",
                "S.alternifloraTall", "Juncus", "Bare", "Other",
                "Phragmites", "Shrub", "Tallsedge", "Water")]
               
corvif(Z)      #Part of Table 1

#Run linear regression
M1<-lm(Banded~Avgmaxht + Avgdens + ht.thatch + S.patens +
              Distichlis + S.alternifloraShort + S.alternifloraTall +
              Juncus + Bare + Other + Phragmites + Shrub + Tallsedge +
               Water, data = Sparrows2)
summary(M1)    #Part of Table 1


#Chop out covariates
Z<-Sparrows2[,c("ht.thatch", "S.patens", "Distichlis",
                "S.alternifloraShort", "Juncus", "Bare", "Other",
                "Phragmites", "Shrub", "Tallsedge", "Water")]
corvif(Z)      #Part of Table 1


#Linear regression on subset
M2 <- lm(Banded ~ ht.thatch + S.patens +
                  Distichlis + S.alternifloraShort +
                  Juncus + Bare + Other + Phragmites + Shrub + Tallsedge +
                  Water, data = Sparrows2)
summary(M2)    #Part of Table 1


M2 <- lm(Banded ~ Juncus + Shrub, data = Sparrows2)
drop1(M2, test = "F")
coeff(M1)
step(M2)

M3 <- lm(Banded ~ Juncus+Shrub, data = Sparrows2)
summary(M3)    #Part of Table 1


#Figure 9
Z <- as.vector(as.matrix(Sparrows2[, c("Avgmaxht", "Avgdens",
              "ht.thatch", "S.patens", "Distichlis",
              "S.alternifloraShort", "S.alternifloraTall", "Juncus",
              "Bare", "Other", "Phragmites", "Shrub", "Tallsedge", "Water")]))


#Setup the data in vector format for the xyplot
Y10 <- rep(Sparrows2$Banded, 14)

MyNames <- names(Sparrows2[,c("Avgmaxht", "Avgdens", "ht.thatch",
                "S.patens", "Distichlis", "S.alternifloraShort",
                "S.alternifloraTall", "Juncus", "Bare", "Other",
                "Phragmites", "Shrub", "Tallsedge", "Water")])

ID10 <- rep(MyNames, each = length(Sparrows2$Banded))
library(lattice)


ID11 <- factor(ID10, labels = c("% Juncus gerardii",
               "% Shrub", "Height of thatch", "% Spartina patens",
               "% Distichlis", "% Bare ground", "% Other vegetation",
               "% Phragmites australis", "% Tall sedge", "% Water",
               "% Spartina alterniflora (short)",
               "% Spartina alterniflora (tall)",
               "Maximum vegetation height",
               "Vegetation stem density"),
               levels = c("Juncus", "Shrub", "Avgmaxht", "S.patens",
                          "Distichlis", "Bare", "Other", "Phragmites",
                          "Tallsedge", "Water", "S.alternifloraShort",
                          "S.alternifloraTall", "ht.thatch", "Avgdens"))


xyplot(Y10 ~ Z | ID11, col = 1,
  strip = function(bg='white',...) strip.default(bg='white',...),
  scales = list(alternating = T,
                x = list(relation = "free"),
                y = list(relation = "same")),
  xlab = "Covariates",
  par.strip.text = list(cex = 0.8),
  ylab = "Banded",
  panel=function(x, y, subscripts,...){
    panel.grid(h =- 1, v = 2)
    panel.points(x, y, col = 1, pch = 16)
    if(ID10[subscripts][1] != "Tallsedge") {panel.loess(x,y,col=1,lwd=2)}
    })


##################################################################
#Figure 10

Sparrows <- read.table(file = "SparrowsElphick.txt", header = TRUE)
source(file = "HighstatLib.R")
MyNames <- c("wing chord", "tarsus length", "head length",
             "culmen length", "nalospi to bill tip", "weightt")
pairs(Sparrows[,c(1, 3, 4, 5, 6, 7)],
      lower.panel = panel.cor,
      cex.labels=1.3,
      labels=MyNames)


###################################################################
#Figure 11
Sparrows <- read.table(file = "SparrowsElphick.txt", header = TRUE)

#Take the data from species 1, Sex = 0 and Wing length >= 65
I1 <- Sparrows$SpeciesCode == 1 & Sparrows$Sex != "0" & Sparrows$wingcrd < 65
Wing1<- Sparrows$wingcrd[I1]
Wei1 <- Sparrows$wt[I1]
Mon1 <- factor(Sparrows$Month[I1])
Sex1<- factor(Sparrows$Sex[I1])


#Define Month and Sex as categorical variables
fMonth1 <- factor(Mon1,levels=c(5,6,7,8,9),
                labels=c("May","Jun","Jul","Aug","Sep"))
fSex1   <- factor(Sex1, levels=c(4,5),labels=c("Male","Female"))

M1 <- lm(Wei1 ~ Wing1*fMonth1*fSex1)
summary(M1)
anova(M1)

#Make the coplot
coplot(Wei1 ~ Wing1 | fMonth1 * fSex1, ylab = "Weight (g)",
       xlab = "Wing length (mm)",
       panel = function(x, y, ...) {
         tmp <- lm(y ~ x, na.action = na.omit)
         abline(tmp)
         points(x, y) })



##################################################################
#Figure 12
Waders <- read.table(file = "wader.txt", header = TRUE)

#Define the time axis
Time <- seq(1,25)

par(mfrow = c(2, 2), mar = c(5, 4, 3, 2))
plot(Time, Waders$C.fuscicolis, type = "l", xlab = "Time (2 weeks)",
     ylab = "C. fuscicollis abundance")
acf(Waders$C.fuscicolis, main = "C. fuscicollis ACF")

plot(Time, Waders$L.dominicanus, type = "l", xlab = "Time (2 weeks)",
     ylab = "L. dominicanus abundance")
acf(Waders$L.dominicanus, main = "L. dominicanus ACF")

#################################################################
























 
 



