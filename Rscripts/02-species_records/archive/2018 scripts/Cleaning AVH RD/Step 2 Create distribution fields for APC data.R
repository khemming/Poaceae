
  library(dplyr)

# function to trim leading and trailing white space
  trim <- function( x ) {
    gsub("(^[[:space:]]+|[[:space:]]+$)", "", x)
  }  
    
  apc <- read.csv("c:\\data\\australian virtual herbarium\\nslsimplename-2015-06-25-2021.csv")
  glimpse(apc)
  
# include only the APC names
  dat <- filter(apc, rank == "Species" & classifications %in% c("[APC, APNI]", "[APNI, APC]"))
  
  a <- dat$apc_distribution
  b <- strsplit(as.character(a), split = ",")
  n_col <- unlist(lapply(b, function(x) length(x)))
  
  c <- matrix("", nrow = nrow(dat), ncol = max(n_col))
  for(i in 1:length(a)) if(n_col[i] > 0) c[i, 1:n_col[i]] <- b[[i]]

# trim leading and trailing white space
  c <- apply(c, 2, function(x) trim(x))
  table(c)
  
  write.csv(table(c), "AVH/Supplied data/distribution fields out.csv")
  
