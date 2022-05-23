
  library(dplyr)

# function to trim leading and trailing white space
  trim <- function( x ) {
    gsub("(^[[:space:]]+|[[:space:]]+$)", "", x)
  }  
    
  apc <- read.csv("c:\\data\\australian virtual herbarium\\nslsimplename-2015-06-25-2021.csv")
  glimpse(apc)
  
# include only the APC names
  dat <- filter(apc, rank == "Species" & classifications %in% c("[APC, APNI]", "[APNI, APC]"))
  
# seperate each distribution entry as a matrix
  a <- dat$apc_distribution
  b <- strsplit(as.character(a), split = ",")
  n_col <- unlist(lapply(b, function(x) length(x)))
  
  c <- matrix("", nrow = nrow(dat), ncol = max(n_col))
  for(i in 1:length(a)) if(n_col[i] > 0) c[i, 1:n_col[i]] <- b[[i]]

# trim leading and trailing white space from the distribution entries
  c <- apply(c, 2, function(x) trim(x))
  table(c)
  head(c)

# read in distribution fields (created using "Create distribution fields for APC data.R") 
  distr <- read.csv("c:\\users\\s429217\\onedrive\\data\\avh\\distribution fields updated.csv")
  distr <- mutate(distr, native = ifelse(is.na(native) == T, 0, 1),
                         naturalised = ifelse(is.na(naturalised) == T, 0, 1))
                         
  names(distr) <- c("field", "native", "naturalised", "freq")                       
                         
# identify species classified as native and naturalised (exotic)
# and native species that have naturalised in Australia
  exo <- matrix(0, nrow = nrow(c), ncol = ncol(c))
  nat <- matrix(0, nrow = nrow(c), ncol = ncol(c))

# create vectors to speed things up
  field <- distr$field
  naturalised <- distr$naturalised
  native <- distr$native
  
# identiy whether each species is listed as native or naturalised from the fields
  exo <- rowSums(apply(c, c(1, 2), function(x) naturalised[which(field == x)]))
  nat <- rowSums(apply(c, c(1, 2), function(x) native[which(field == x)]))

# now combine these to determine exclusively naturalised species (i.e. no native entry)
  exc_nat <- ifelse(nat == 0 & exo > 0, 1, 0)
  
# number of naturalised species
  sum(exc_nat)

# native species that have naturalised elsewhere in Australia
  native_nat <- ifelse(nat == 1 & exo > 0, 1, 0)
  sum(native_nat)

# list each species as native / naturalised
  dat.final <- mutate(dat, naturalised = exc_nat, native_naturalised = native_nat) %>%
               dplyr:::select(name, naturalised, native_naturalised, apc_name, apc_familia, familia, author, authority, genus, species) %>%
               arrange(familia, name)
               
# output
  write.csv(dat.final, "c:\\users\\s429217\\onedrive\\data\\avh\\APC names native naturalised.csv")
               
