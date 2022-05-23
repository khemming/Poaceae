
  setwd("c:\\data\\australia\\australian virtual herbarium\\raw data")
  file.list <- dir("c:\\data\\australia\\australian virtual herbarium\\raw data")
  
  read.raw <- function(x) {
    a <- read.csv(x)
    a <- a[, c("Scientific.Name", "Matched.Scientific.Name", "locality", "Order...matched", "Family...matched", "Genus...matched", "Species...matched",
               "Latitude...processed", "Longitude...processed", "IBRA.Region...parsed", "State...parsed", "Year...parsed", "Taxon.identification.issue",
               "inferred.Duplicate.Record", "Name.not.in.national.checklists")]
    return(a)
  }
  
  avh.dat <- read.raw(file.list[1])
  for(i in 2:length(file.list)) {
    b <- read.raw(file.list[i])
    avh.dat <- rbind(avh.dat, b)
  }
  
  avh.dat <- avh.dat[order(avh.dat$Matched.Scientific.Name), ]
  dim(avh.dat)
  
  avh.dat$locality <- tolower(avh.dat$locality)
  table(grepl("cultivate", avh.dat$locality))
  table(grepl("garden", avh.dat$locality))

# remove records with mention of cultivate or garden in the locality
  avh.dat <- subset(avh.dat, !(grepl("cultivate", locality)))
  avh.dat <- subset(avh.dat, !(grepl("garden", locality)))
  dim(avh.dat)
  
# remove locality
  avh.dat <- avh.dat[, c("Scientific.Name", "Matched.Scientific.Name", "Order...matched", "Family...matched", "Genus...matched", "Species...matched",
                         "Latitude...processed", "Longitude...processed", "IBRA.Region...parsed", "State...parsed", "Year...parsed", "Taxon.identification.issue",
                         "inferred.Duplicate.Record", "Name.not.in.national.checklists")]
  
  write.table(avh.dat, "c:\\data\\australia\\australian virtual herbarium\\AVH data extracted.dat", row.names=F, sep="\t")
  

  
  
  