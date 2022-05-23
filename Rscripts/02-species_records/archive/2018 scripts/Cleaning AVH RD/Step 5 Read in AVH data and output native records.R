
  library(dplyr)

# read in AVH data
  avh <- read.table("c:\\users\\s429217\\onedrive\\data\\avh\\AVH data extracted.dat", sep="\t", header=T, strip.white = T)
  dim(avh)
  
# exclude lats > 0
  avh <- avh[avh$Latitude...processed < 0, ]
  
# exclude Macquarie Island & Lord Howe Island
  avh <- avh[avh$Latitude...processed > -50 & avh$Longitude...processed < 155, ]

  table(avh$Taxon.identification.issue, avh$Name.not.in.national.checklists)
  
# delete rows with Species...matched data missing
  avh <- avh[!(avh$Species...matched == ""), ]
   
# list of species
  spp.list <- levels(avh$Species...matched)
  length(spp.list)
  
# read in APC names
  apc <- read.csv("c:\\users\\s429217\\onedrive\\data\\avh\\APC names native naturalised.csv", strip.white = T)
  apc.nat <- select(apc, name, naturalised)
  length(apc$naturalised[apc$naturalised == 0])
  
##########################################################################################
# extract names from avh and apc and compare
  avh.names <- data.frame(sci.name = as.character(levels(avh$Species...matched)), avh = 1)  
  apc.names <- data.frame(sci.name = as.character(levels(apc.nat$name)), apc = 1, native = table(apc.nat$name, apc.nat$naturalised)[, 1])
  
  allnames <- merge(avh.names, apc.names, by = "sci.name", all = T)
  allnames <- mutate(allnames, avh = ifelse(is.na(avh) == T, 0, 1),
                               apc = ifelse(is.na(apc) == T, 0, 1),
                               tot = avh + apc)
  allnames$sci.name <- as.character(allnames$sci.name)                             
  allnames <- arrange(allnames, sci.name)                 

  head(allnames)
  table(allnames$tot)

# problem with " x " in names
  allnames[grepl(" x ", allnames$sci.name), ]
  
#########################################################################################
# remove " x " from avh names
  avh.names$sci.name <- gsub(" x ", " ", avh.names$sci.name)
  
  allnames <- merge(avh.names, apc.names, by = "sci.name", all = T)
  allnames <- mutate(allnames, avh = ifelse(is.na(avh) == T, 0, 1),
                               apc = ifelse(is.na(apc) == T, 0, 1),
                               tot = avh + apc)
  allnames$sci.name <- as.character(allnames$sci.name)                             
  allnames <- arrange(allnames, sci.name)                 

  head(allnames)
  table(allnames$tot)

# check problem with " x " in names is resolved
  allnames[grepl("Salix", allnames$sci.name), ]
  allnames[grepl(" x ", allnames$sci.name), ]

# list of native species names that are in avh data
  nat_names <- allnames$sci.name[allnames$avh == 1 & (is.na(allnames$native) == F & allnames$native == 1)]

# fix up avh names with " x "
  avh$Species...matched <- gsub(" x ", " ", avh$Species...matched)
  avh$native <- ifelse(avh$Species...matched %in% nat_names, 1, 0)
  
  avh.nat <- filter(avh, native == 1)
  dim(avh.nat)
  table(avh.nat$Taxon.identification.issue, avh.nat$Name.not.in.national.checklists)
  
# output all avh naturalised records
  write.csv(avh.nat, "c:\\users\\s429217\\onedrive\\data\\avh\\AVH native records.csv")
  
  


  