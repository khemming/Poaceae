
# date created: 14/4/18
# last modified: 21/6/18

# C3/C4: attaching to AVH data 

# The aim of this script is to create a dataframe that attaches the photosynthetic pathway (pp) to each species

# Library and raw data
  library(ggplot2)
  library(ggmap)
  library(tidyr)
  library(raster)
  library(rgdal)
  library(maptools)
  library(dplyr)
  library(purrr)
  
  setwd("C:/Users/s436862/Dropbox/Climate Matching/Data files/")
  
  rm(list = ls())
  
# AVH Poa data
  poa <- read.csv("AVH/AVH grass records.csv", header = T)
  
# C3/C4 photosynthentic pathway (pp)
  pp <- read.csv("Osborne C3-C4/c3-c4.csv", header = T)
  
# Making the species-photosynthetic pathway data frame -------------------------------
  table(pp$pp)
# From this dataset there are genera who are:
# C3 (391 spp)
# C4 (306)
# C3 & C4 (9)
# unknown (2)
  
# How many species total did we get from whom?   ------------------------------------------  
# How much did Osborne contribute? How much did teh wider literature give us?
 
# Total species: 1382 (= 1047 + 335) 
  
# Osborne contribution:  
  j <- filter(poa_pp, pp == "C3" | pp == "C4")
  length(unique(j$species))
  # 1038
 
# Plus: 1 + 63 (Pannicum) + another genus (29) = 93
# 1038 + 93 = 1131
# 1131  / 1382 * 100 = 81.83792 % from Osborne

# From further afield:  
# Couldn't find records for 15 species (Mixed: 2Nat, 1Int; Unknown: 4nat/8int) = 1382 - 1041 -326 =  15

# Therfore sourced not from Osborne =   
# 1382 - 15 - 1131 = 236
# 236/ 1382 * 100 = 17.0767
 
 
 
# --------------------------------------------------------------------------------- 
 
# Species belonging to the same genera in Osborne's data set are all set to the same pp (as per Osborne's methods and advice in their paper). 
# So any genus that's not explictly C3 or 4 I am going to label 'NA', as will stuff that's missing from Osborne's data, that I can't locate on the web, manually
# these will not included in subsequent modelling exericses, but will be kept in the dataframe
  
# merge poa and pp 
  poa_pp <- left_join(poa, pp, by = "genus")
  
  table(poa_pp$genus, poa_pp$pp)
 
# identify species/genera who are not C3 or C4 
  no_pp_gen <- filter(poa_pp, is.na(pp) | pp == "C3 & C4",
                  !duplicated(genus)) 
  
  no_pp_spp <- filter(poa_pp, is.na(pp) | pp == "C3 & C4",
                  !duplicated(species)) 
  
# Cleaning data of mixed and NA pp ----------------------------------------
# for the sake of brevity
  p <- no_pp_spp
  p$pp <- as.numeric(p$pp)
  
# Check record number of these species/genera   
  list(unique(p$genus))
  list(unique(p$species))
# Genera changed ----------------------------------------------------------  
# 29 genera, denoted by [#]
# Steps taken to figure out genus/species photosynthetic pathway:
# (1) Checking Osborne 2014 as to if there are species-level (rather than genus) changes within the paper for a given genus
# There are, so I am manually adding these in. Note within methods I did this.
# (2) Looking for web sources for extra details
# (3) Labels: mixed, C3, C4, and unknown; note 'mixed' can be interms of genus mix, with not enough work done to distinguish between species, or within a species which does not confrom to one or the other pathway

  
# [1] Alloteropsis
# Two species: cimicina, semilata
# Cimicina is C4 (Osborne, 2014)
# semialata has both C3 and C4 subspecies, no subsp. info therefore mixed https://en.wikipedia.org/wiki/Alloteropsis_semialata)
  p[1, 18] <- "C4"
  p[2, 18] <- "mixed"
      
# [2] Amelichloa
# Two species: brachychaeta, caudata
  p[3:4, 18] <- "unknown" # DNF pp, closest was: https://onlinelibrary.wiley.com/doi/full/10.1111/j.1096-0031.2010.00310.x
  
# [3] "Amphibromus"    
  p[5:14, 18] <- "C3" # C3: Land of Sweeping Plains: Managing and Restoring the Native Grasslands has this info in it
                   # https://www.castlemaineflora.org.au/pic/a/amphi/amner.htm
  
# [4] "Aristida"    
  p[15:73, 18] <- "C3" # C3, only C4 is A. longifolia (don't have); https://www.ncbi.nlm.nih.gov/pubmed/21628285
  
# [5] "Australopyrum"
  p[74:76, 18] <- "C3" # All species C3: https://web.archive.org/web/20071120045514/http://delta-intkey.com/grass/www/australo.htm
  
# [6] "Austrostipa" 
  p[77:140, 18] <- "C3" # C3: Osborne calling the genus 'Stipa' instead
  
# [7] "Avellinia"       # Unknown; closest I could get to an answer: https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5672130/
  p[141, 18] <- "unknown"    
  
# [8] "Deyeuxia"
  p[142:174, 18] <- "C3"  # C3: https://www.environment.gov.au/system/files/resources/2edcda80-d9b7-49d4-9e97-36236b91e9f9/files/mvg19-nvis-tussock-grasslands.pdf
  
# [9] "Elytrigia"  
  p[175:176, 18] <- "C3"    # Repens = C3: https://castlemaineflora.org.au/pic/e/elytr/elrep.html & all = C3 (mentions its C3 but doesn't prove it) https://www.tandfonline.com/doi/pdf/10.1080/00288233.1996.9513212
  
# [10] "Eragrostis"
  p[177:256,18] <- "C4"    # Walteri is C3 (which we don't have records for), rest are C4 (Osborne 2014)
  
# [11] "Hookerochloa"  
  p[257:258, 18] <- "unknown"  # Unknown (only two species; natives)
  
# [12] "Jarava"    
  p[259, 18] <- "C3" # C3: https://www.academia.edu/11816490/Evidence_of_shift_in_C4_species_range_in_central_Argentina_during_the_late_Holocene
  
# [13] "Lachnagrostis" 
  p[260:279, 18] <- "C3" # C3: https://nativegrassresourcesgroup.files.wordpress.com/2015/01/understandingc3c4.pdf & https://www.rbg.vic.gov.au/documents/Muelleria_Vol_37_-_pp65-74_Brown_-_New_Guinea.pdf 
  
# [14] "Lophopyrum"  
  p[280, 18] <- "C3" # Synonym: Thinopyrum [same spp]; C3 https://www.tandfonline.com/doi/pdf/10.1080/00288233.1996.9513222
  
# [15] "Megathyrsus"  # C4 sub-type: https://www.ncbi.nlm.nih.gov/pubmed/24642845
  p[281, 18] <- "C4"
  
# [16] "Microlaena" 
  p[282:284, 18] <- "C3" # C3: https://nativegrassresourcesgroup.files.wordpress.com/2015/01/understandingc3c4.pdf & https://www.dpi.nsw.gov.au/agriculture/pastures-and-rangelands/species-varieties/pf/factsheets/weeping-grass-or-microlaena
  
# [17] "Molineriella" # Few synonyms: http://powo.science.kew.org/taxon/urn:lsid:ipni.org:names:408804-1 ; couldn't get pp
  p[284, 18] <- "unknown"
  
# [18] "Moorochloa"   # C4: https://journals.plos.org/plosone/article/file?type=supplementary&id=info:doi/10.1371/journal.pone.0151075.s002
  p[285, 18] <- "C4"
  
# [19] "Neurachne"   # A mixed genus: 
  p[286, 18] <- "C3" # alopecuroidea (C3) http://www.plantphysiol.org/content/plantphysiol/early/2017/02/02/pp.16.01893.full.pdf
  p[287, 18] <- "C3" # annularis (C3) https://florabase.dpaw.wa.gov.au/science/nuytsia/522.pdf
  p[288, 18] <- "C3" # lanigera (C3) https://florabase.dpaw.wa.gov.au/science/nuytsia/522.pdf
  p[289, 18] <- "mixed" # minor (mixed) http://www.plantphysiol.org/content/plantphysiol/early/2017/02/02/pp.16.01893.full.pdf
  p[290, 18] <- "C4" # munroi (C4) http://www.plantphysiol.org/content/plantphysiol/early/2017/02/02/pp.16.01893.full.pdf
  p[291, 18] <- "C3" # queenslandica C3 https://www.researchgate.net/publication/248898485_Distribution_and_cytology_of_Australian_Neurachne_and_its_allies_Poaceae_a_group_containing_C_3_C_4_and_C_3_-C_4_Intermediate_species
  p[292, 18] <- "C3" # tenuifolia C3 https://www.researchgate.net/publication/248898485_Distribution_and_cytology_of_Australian_Neurachne_and_its_allies_Poaceae_a_group_containing_C_3_C_4_and_C_3_-C_4_Intermediate_species
  
# [20] "Panicum" (3_ species); note: all notes from Supplementary materials 4. from Osborne unless otherwise specified
  p[293, 18] <- "C4"    # antidotale - C4
  p[294, 18] <- "C3"    # bisulcatum - C3
  p[295, 18] <- "C4"    # bombycinum - c4
  p[296, 18] <- "C4"    # buncei - c4
  p[297, 18] <- "C4"    # capillare - C4
  p[298, 18] <- "C4"    # chillagoanum - C4
  p[299, 18] <- "C4"    # coloratum - c4
  p[300, 18] <- "C4"    # decompositum - C4
  
  p[301, 18] <- "C4"    # effusum - c4
  p[302, 18] <- "C4"    # gilvum - c4
  p[303, 18] <- "unknown"   # hillmanii - no info (with aus distribution looks likely to be a C3)
  p[304, 18] <- "C3"    # incomtum - C3
  p[305, 18] <- "C3"    # lachnophyllum - C3
  p[306, 18] <- "C4"    # laevinode - C4
  p[307, 18] <- "C4"    # larcomianum - C4
  p[308, 18] <- "C4"    # latzii - C4
  
  p[309, 18] <- "C4"    # luzonense - c4
  p[310, 18] <- "C4"    # miliaceum - C4
  p[311, 18] <- "C4"    # mindanaense - C4
  p[312, 18] <- "C4"    # mitchellii - C4
  p[313, 18] <- "C4"    # novemnerve - C4
  p[314, 18] <- "C4"    # obseptum - C4
  p[315, 18] <- "C4"    # paludosum - C4
  p[316, 18] <- "C3"    # pygmaeum - c3
  
  p[317, 18] <- "C4"    # queenslandicum - C4
  p[318, 18] <- "C4"    # racemosum - C4
  p[319, 18] <- "C4"    # repens -  c4
  p[320, 18] <- "C4"    # schinzii - C4
  p[321, 18] <- "C4"    # seminudum - c4
  p[322, 18] <- "C4"    # simile - C4: http://www.hornsby.nsw.gov.au/__data/assets/pdf_file/0008/106919/Grasses-workshop-booklet-2013-July.pdf
  p[323, 18] <- "C4"    # trachyrhachis
  p[324, 18] <- "C3"    # trichoides
  
  
# [21] "Pseudopogonatherum" 
  p[325, 18] <- "C4"    # C4: https://books.google.com.au/books?id=bBspDwAAQBAJ&pg=PA139&lpg=PA139&dq=Pseudopogonatherum+contortum+%22C4%22&source=bl&ots=lZbQjSj6qH&sig=fYMm_WnB2ryrxpFTDBjzVJYe01c&hl=en&sa=X&ved=2ahUKEwj5s8TWkPvdAhUQF4gKHcdqBJUQ6AEwAHoECAMQAQ#v=onepage&q=Pseudopogonatherum%20contortum%20%22C4%22&f=false
  p[326, 18] <- "C4"    # C4 annual irritans: https://openi.nlm.nih.gov/detailedresult.php?img=PMC4523779_fpls-06-00560-g001&req=4

# [22] "Saxipoa"        
  p[327, 18] <- "unknown"    # Recent genus; it's extremely likely to be C3, but can't find source
  
# [23] "Steinchisma", two species
  p[328, 18] <- "mixed"    # Mixed: https://www.jstor.org/stable/10.1086/378657?seq=1#metadata_info_tab_contents
  p[329, 18] <- "C3"    # C3: (same paper as above) https://www.ncbi.nlm.nih.gov/pubmed/27073202

# [24] "Sylvipoa"          
  p[330, 18] <- "unknown"    # One species, origin paper: no info on phptosyntheitc pathway, however https://www.researchgate.net/publication/248900734_Saxipoa_and_Sylvipoa_-_two_new_genera_and_a_new_classification_for_Australian_Poa_Poaceae_Poinae
  
# [25] "Tetrarrhena"     
  p[331:336, 18] <- "C3"  # C3: https://nativegrassresourcesgroup.files.wordpress.com/2015/01/understandingc3c4.pdf
  
# [26] "Thellungia"        
  p[337, 18] <- "C4"    # C4: https://pdfs.semanticscholar.org/015b/4dfaa494f183e3201210d838223cacda617a.pdf
  
# [27] "Thinopyrum"       
  p[338:340, 18] <- "unknown"    # uuuuuunknown
    
# "Walwhalleya"     
  p[341:343, 18] <- "C3"  # <- that's a made up word. Seriously; C3
  
# "Zuloagaea"  
  p[344, 18] <- "C4"  # https://www.semanticscholar.org/paper/Photosynthetic-responses-of-a-C3-and-three-C4-of-to-Alfonso-Br%C3%BCggemann/6d2cd129d20204876caf92435c683992b0b826d4
  
# Merging corrected species into poa_pp -----------------------------------------  
# insert corrected-pp derived from above
# make a note of 'C3 & C4' class -- I'll leave it in, and subset out in the Rarefaction script
  cor <- select(p, species, pp)
  
# Insert corrected genera into poa_pp
  pp_poa_cor <- left_join(poa_pp, cor, by = "species")
  pp_poa_cor$pp.x <- as.character(pp_poa_cor$pp.x)
  pp_poa_cor$pp.y <- as.character(pp_poa_cor$pp.y)
  
  pp_poa_cor$pp <- ifelse(is.na(pp_poa_cor$pp.y) == FALSE, pp_poa_cor$pp.y, pp_poa_cor$pp.x)
  
  table(pp_poa_cor$species, pp_poa_cor$pp)
  
# --------------------------------------------------------------------------  
# 'C3 & C4' into NAs (Not run here: 20/6/18) --------------------------------------------------------
#   pp_poa_cor$pp <- ifelse(pp_poa_cor$pp.z == "C3 & C4", NA, pp_poa_cor$pp.z)
#   table(pp_poa_cor$genus, pp_poa_cor$pp)
# -------------------------------------------------------------------------- 
# clean out the pp's  
  pp_poa_cor <- select(pp_poa_cor, -pp.x, -pp.y)
  
# C3-C4 splits: assessing the data frame ---------------------------------------------
# genera  
  head(table(pp_poa_cor$genus, pp_poa_cor$pp))
  
# species
  head(table(pp_poa_cor$species, pp_poa_cor$pp))
  
# How many species are there of each?
  poa_c3c4 <- distinct(pp_poa_cor, species, .keep_all = TRUE) # why there is a dot there, IDK
  table(poa_c3c4$status, poa_c3c4$pp, exclude = NULL)
 
# Save ------------------------------------------------------------------
  write.csv(pp_poa_cor, file = "Osborne C3-C4/AVH grass pp.csv", row.names = F)
  
  no_pp <- read.csv("Osborne C3-C4/c3-c4 corrected sources.csv", header = T)
  
 
# save this badboy
  write.csv(poa_pp, file = "C:/Users/s436862/Dropbox/Climate Matching/Results/C3-C4/Poa spps C3-C4.csv", row.names = F)
  
  
  
  
  
  
  
  
  
# I want unique spp. C3/C4 and status
# So number of native spp. C3/C4 and same with int. Hmm.
  c34 <- read.csv("C:/Users/s436862/Dropbox/Climate Matching/4. Results/C3-C4/Poa spps C3-C4.csv", header = T) %>%
    group_by(species, pp) %>%
    filter(duplicated(species) == F) %>%
    dplyr:::select(species, status, pp)
  
  c34_nat <- filter(c34, status == "native")
  c34_int <- filter(c34, status == "introduced")

# sum number of c3 and c4 for each
  sum(c34_nat == "C3", na.rm = T)
  sum(c34_nat == "C4", na.rm = T)
  
  sum(c34_int == "C3", na.rm = T)
  sum(c34_int == "C4", na.rm = T) 
  
# load C3-C4 and descriminate between nat and int
  xy <- cbind(avh$long, avh$lat)
  spp <- as.numeric(factor(avh$species))
    
# plot C3/C4 ratio acorss australia
  scale <- 100
  
  b <- raster("EFs/EFs cropped/arid.grd")
  aus <- raster("Australia raster/aus.r.grd")
  oz <- borders("world", region = "Australia")
  
  
  
# aggregate original raster
  raster <- aggregate(b, fact = scale, fun = mean)
  
# actual richness (a)
  gh <- rasterize(xy, raster, field = spp, fun = function(x,...) {length(unique(na.omit(x))) })

 
  
# -------------------------------------------------
  
  
  
  
  
  
  
  
  
  
  