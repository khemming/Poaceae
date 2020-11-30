########################

# see metadata for notes

#### species richness (poa from AVH) and mean annual precipitation (AP; bio_12 from worldclim) 
# plotted in grid cells of Australian dimensions
  rm(list = ls())
  par(mfrow = c(1,1))
  
  library(ggplot2)
  library(ggmap)
  library(tidyr)
  library(raster)
  library(dplyr)
  library(rgdal)
  library(maptools)
  
  setwd("C:\\Users\\s436862\\Dropbox\\Climate Matching\\1. Data files")
  
########## Load data
# Oz (for plotting outline)  
  data(wrld_simpl)
  oz <- subset(wrld_simpl, NAME=="Australia")
  projection(oz) <- "+proj=utm +zone=48 +datum=WGS84"
 
# Australia (for raster dimensions etc.)
  aus <- raster("Australia raster/aus.r.grd")

# mean annual precipiation (bio_12) with Australian coverage
  precip <- raster("Worldclim/precip4/bio_12_Aus.asc")
  #plot(precip) # al dente
  #plot(oz, add = T)

# manageable grid sizes  
  precip_ag <- aggregate(precip, fact = 100, fun = mean)
  plot(precip_ag)
  
# Poa species data 
  poa <- read.table("AVH from Richard\\AVH grass records.txt", header = T, sep = "\t")
  poa <- group_by(poa, species) %>%
      select(status, species, lat, long)


###########
# Native Species richness 
  
# native
  nat <- filter(poa, status == "native")
  n.xy <- cbind(nat$long, nat$lat)
  n.spp <- as.numeric(factor(nat$species))
  
  nat_r <- rasterize(n.xy, precip_ag, field = n.spp, fun = function(x,...) {length(unique(na.omit(x)))})
  
# extract values 
  nat_v <- getValues(nat_r)
  precip_v <- getValues(precip_ag)
  nat_df <- data.frame(nat_v, precip_v)
  r
  
# plotting
  m1 <- lm(log(nat_v) ~ log(precip_v))
  summary(m1)
  
  #test1
 dat1 <-  filter(nat_df, !precip_v == "NA" ) %>%
   filter(!nat_v == "NA" )
 m2 <- lm(log(dat1$nat_v) ~ log(dat1$precip_v))
 summary(m2)
 
  m2 <- lm(log(dat1$nat_v) ~ log(dat1$precip_v))
summary(m2)

plot(log(dat1$nat_v) ~ log(dat1$precip_v))
abline(m2, col = "red", lwd = 5)


 #test2
#anthony
 head(dat1)
 glimpse(dat1)
 dat2 <- mutate(dat1, precipitation = log(as.numeric(precip_v)),
                native.species = log(as.numeric(nat_v))) %>%
   filter(precip_v > 1 & nat_v > 1)
 
 m3 <- lm(dat2$native.species ~ dat2$precipitation)
 summary(m3)
 
 # dat3 <- select(dat2, precipitation, native.species) %>%
 #   gather(va)
 
 library(ggthemes)
 
#plot developed 
 ggplot(dat2, aes(y = native.species, x = precipitation)) +
   geom_point(size = 1.5, pch = 1, alpha = 1) +
   geom_smooth(method = "lm", color = "black") +
   labs(x = "Precipitation (log)",
        y = "Native species richness (log)") +
   theme_tufte() +
   #choosen colour palette
   theme(legend.position = "none",
         plot.title = element_text(size=16, hjust=0, face="italic", color="black"),
         plot.subtitle=element_text(size=16, hjust=0, face="italic", color="black"),
         legend.background = element_rect(fill="white", size=1, linetype="solid", colour ="black"),
         panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
         axis.title.y=element_text(colour = "black",size =14),
         axis.title.x = element_text(colour = "black",size =14),
         axis.text.x=element_text(size = 14),
         axis.text.y=element_text(colour = "black",size = 14),
         axis.ticks.x = element_line(size = 1),
         axis.ticks.y =element_line(size = 1),
         axis.line.x = element_line(size = 1),
         axis.line.y = element_line(size = 1)) +
   annotate("text", x = 7.8, y = 4.4, label = "italic(R) ^ 2 == 0.22",
            parse = TRUE, col = "blue") +
   #scale_y_continuous(limits=c(-2.5,2)) + 
   #scale_x_continuous(limits=c(0,4)) +
   ggtitle("Log annual precipitation ~ log native richness")
 
 ggsave("C:/Users/s436862/Dropbox/Climate Matching/4. Results/Poa_x_AP/log.native_log.AP.jpeg", plot = last_plot(), scale = 1, dpi = 200, device = "jpeg")
 

  ggplot(nat_df, aes(y = log(nat_v), x = log(precip_v))) +
    geom_point() +
    abline()
      # R2 = 0.17 
  
  n_sing <- rasterize(xy, precip_ag, field = spp, fun = function(x,...) {length(which(table(x)==1)) })
  
  # number of doubletons ("                         " two occurrences in the sample)
  n_doub <- rasterize(xy, precip_ag, field = spp, fun = function(x,...) {length(which(table(x)==2)) })
  
  # chao1 estimator
  chao <- poa_r + ((n_sing * (n_sing - 1)) / (2 * (n_doub + 1)))
  plot(chao)
  plot(log10(chao))
  
##################
# introduced
  int <- filter(poa, status == "introduced")
  i.xy <- cbind(int$long, int$lat)
  i.spp <- as.numeric(factor(int$species))
  
  int_r <- rasterize(i.xy, precip_ag, field = n.spp, fun = function(x,...) {length(unique(na.omit(x)))})
  
# extract values 
  int_v <- getValues(int_r)
  precip_v <- getValues(precip_ag)
  int_df <- data.frame(int_v, precip_v)
  
# plotting
  m2 <- lm(log(int_v) ~ log(precip_v))
  abline(m2)
  summary(m2)
  # R2 = 0.17 
  
  m3 <- glm(int_v ~ int_v)
  plot(m3)
  summary(m3)
  
#test1
  dat10 <-  filter(int_df, !precip_v == "NA" ) %>%
    filter(!int_v == "NA" )
  m2 <- lm(log(dat10$int_v) ~ log(dat10$precip_v))
  summary(m2)
  
  m2 <- lm(log(dat10$int_v) ~ log(dat10$precip_v))
  summary(m2)
  
  plot(log(dat10$int_v) ~ log(dat10$precip_v))
  abline(m2, col = "red", lwd = 5)
  
  
  #test2
  #anthony
  head(dat10)
  glimpse(dat10)
  dat20 <- mutate(dat10, precipitation = log(as.numeric(precip_v)),
                 introduced.species = log(as.numeric(int_v))) %>%
    filter(precip_v > 1 & int_v > 1)
  
  m3 <- lm(dat20$introduced.species ~ dat20$precipitation)
  summary(m3)
  
  # dat3 <- select(dat2, precipitation, introduced.species) %>%
  #   gather(va)
  
  library(ggthemes)
  
  #plot developed 
  ggplot(dat20, aes(y = introduced.species, x = precipitation)) +
    geom_point(size = 1.5, pch = 1, alpha = 1) +
    geom_smooth(method = "lm", color = "black") +
    labs(x = "Precipitation (log)",
         y = "Introduced species richness (log))") +
    theme_tufte() +
    #choosen colour palette
    theme(legend.position = "none",
          plot.title = element_text(size=16, hjust=0, face="italic", color="black"),
          plot.subtitle=element_text(size=16, hjust=0, face="italic", color="black"),
          legend.background = element_rect(fill="white", size=1, linetype="solid", colour ="black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.title.y=element_text(colour = "black",size =14),
          axis.title.x = element_text(colour = "black",size =14),
          axis.text.x=element_text(size = 14),
          axis.text.y=element_text(colour = "black",size = 14),
          axis.ticks.x = element_line(size = 1),
          axis.ticks.y =element_line(size = 1),
          axis.line.x = element_line(size = 1),
          axis.line.y = element_line(size = 1)) +
    annotate("text", x = 7.8, y = 4.4, label = "italic(R) ^ 2 == 0.13",
             parse = TRUE, col = "blue") +
    #scale_y_continuous(limits=c(-2.5,2)) + 
    #scale_x_continuous(limits=c(0,4)) +
    ggtitle("Log annual precipitation ~ log introduced richness")
  
  ggsave("C:/Users/s436862/Dropbox/Climate Matching/4. Results/Poa_x_AP/log.introduced_log.AP.jpeg", plot = last_plot(), scale = 1, dpi = 200, device = "jpeg")
  
  ggplot(int_df, aes(y = log(int_v), x = log(precip_v))) +
    geom_point() +
    abline()
  
  f <- lm(log(int_v) ~ log(precip_v))
  summary(f)
  plot(f)
  
  # R2 = 0.17 
  
# save
  write.csv(poa_x_AP, file = "C:\\Users\\s436862\\Dropbox\\Climate Matching\\4. Results\\Poa_x_AP\\poa_x_AP.csv")
    
  pdf(file = "C:\\Users\\s436862\\Dropbox\\Climate Matching\\4. Results\\Chao corrected Poa\\poa_x_AP.pdf", width = 7, heigh = 5)
  plot(log(poa_v) ~ log(precip_v))
  abline(0, 1)
  dev.off()
 

#############  

# correct species richness using chao1 estimator

# number of singletons (number of species with only a single occurrence in the sample)
  n_sing <- rasterize(xy, precip_ag, field = spp, fun = function(x,...) {length(which(table(x)==1)) })

# number of doubletons ("                         " two occurrences in the sample)
  n_doub <- rasterize(xy, precip_ag, field = spp, fun = function(x,...) {length(which(table(x)==2)) })

# chao1 estimator
  chao <- poa_r + ((n_sing * (n_sing - 1)) / (2 * (n_doub + 1)))
  plot(chao)
  plot(log10(chao))

  plot(as.vector(chao) ~ as.vector(poa_r))
  abline(0, 1)
  # nooooo idea what this output means

# save   
  pdf(file = "C:\\Users\\s436862\\Dropbox\\Climate Matching\\4. Results\\Chao corrected Poa\\chao.corrected_all.poa.pdf", width = 7, heigh = 5)
  plot(as.vector(chao) ~ as.vector(poa_r))
  abline(0, 1)
  dev.off()
  

  
    
###############################    
# Native species
  
  poa_nat <- filter(poa, status == "native")
  # dplyr function: alters the df so columns are relative to status 
  # (not filtering out all columns bar status)
  nat_xy <- cbind(poa_nat$long, poa_nat$lat)
  nat_spp <- as.numeric(factor(poa_nat$species))
  # again, species richness
  nat_n_spp <- rasterize(nat_xy, precip_ag, field = nat_spp, fun = function(x,...) {length(unique(na.omit(x))) })
  nat_n_tot <- rasterize(nat_xy, precip_ag, fun = function(x,...) length(x))
  plot(log10(nat_n_spp))

# number of singletons
  nat_n_sing <- rasterize(nat_xy, precip_ag, field = nat_spp, fun = function(x,...) {table(na.omit(x))[1] })
# number of doubletons
  nat_n_doub <- rasterize(nat_xy, precip_ag, field = nat_spp, fun = function(x,...) {table(na.omit(x))[2] })

# chao1 estimator
  nat_chao <- nat_n_spp + ((nat_n_sing * (nat_n_sing - 1)) / (2 * (nat_n_doub + 1)))
  plot(nat_chao)
  plot(log10(nat_chao))

  plot(as.vector(nat_chao) ~ as.vector(nat_n_spp))
  abline(0, 1)
  # that looks nice (still not sure what it means)
# save   
  pdf(file = "C:\\Users\\s436862\\Dropbox\\Climate Matching\\4. Results\\Chao corrected Poa\\chao.corrected_nat.poa.pdf", width = 7, heigh = 5)
  plot(as.vector(nat_chao) ~ as.vector(nat_n_spp))
  abline(0, 1)
  dev.off()
  
############################  

# Introduced species  
  poa_int <- filter(poa, status == "introduced")
  
  int_xy <- cbind(poa_int$long, poa_int$lat)
  int_spp <- as.numeric(factor(poa_int$species))
  int_n_spp <- rasterize(int_xy, precip_ag, field = int_spp, fun = function(x,...) {length(unique(na.omit(x))) })
  plot(log10(int_n_spp))
# number of singletons
  int_n_sing <- rasterize(int_xy, precip_ag, field = int_spp, fun = function(x,...) {table(na.omit(x))[1] })
# number of doubletons
  int_n_doub <- rasterize(int_xy, precip_ag, field = int_spp, fun = function(x,...) {table(na.omit(x))[2] })
# chao1 estimator
  int_chao <- int_n_spp + ((int_n_sing * (int_n_sing - 1)) / (2 * (int_n_doub + 1)))
  plot(log10(int_chao))
  
  plot(as.vector(int_chao) ~ as.vector(int_n_spp))
  abline(0, 1)
  # Again, very neat - but different to native one
  # perhaps this is why the graph with all species looks a little wonky?

# save   
  pdf(file = "C:\\Users\\s436862\\Dropbox\\Climate Matching\\4. Results\\Poa_x_AP\\chao.corrected_int.poa.pdf", width = 7, heigh = 5)
  plot(as.vector(int_chao) ~ as.vector(int_n_spp))
  abline(0, 1)
  dev.off()
  
#######################  plotting native & introduced species richness w GG  
  # native and introduced speciess corrected richness from above
  nat_chao
  int_chao
  
# distributions?  
  hist(nat_chao)
  # totally not normally distributed -- is that an issue for what I am up atm?
  # Going to assume that's a 'no' for the mo'
  hist(log(nat_chao))
  # looks much nicer -- oh well
  hist(int_chao)
  # even worse...
  hist(log(int_chao))
  # so much nicer, too

# GG  
  ggplot(data = rich, aes(x = grid.lon, y = grid.lat, fill = richness)) +
    oz + 
    geom_tile() +
    #tile = square (not point) 
    facet_grid(~ status)
  
  
  
  
  
#################  
# compare native and introduced with effort
  n <- as.vector(nat_chao) / as.vector(nat_n_spp)
  i <- as.vector(int_chao) / as.vector(int_n_spp)
  t.test(log(n), log(i))
  # output
  # Welch Two Sample t-test
  
  # data:  log(n) and log(i)
  # t = -5.6502, df = 552.13, p-value = 2.569e-08
  # alternative hypothesis: true difference in means is not equal to 0
  #  95 percent confidence interval:
  #  -0.11730824 -0.05678557
  # sample estimates:
  #   mean of x  mean of y 
  # 0.03950939 0.12655630
   
    # so the groups are not equally distributed (just!) 
    # as proportions of logged, chao-corrected, total species richness  
  
  plot(nat_chao, int_chao)
  
  nat <- getValues(nat_chao)
  int <- getValues(int_chao)
  
  m1 <- lm(nat ~ int)
  abline(m1)
  
  m2 <- glm(nat ~ int)
  summary(m2)
  
  
  
  a <- stack(nat_chao, int_chao)
  layerStats(a, stat = "pearson", na.rm = T)
  # output
  # pearson correlation coefficient`
  #             layer.1   layer.2
  # layer.1  1.0000000 0.3748786
  # layer.2  0.3748786 1.0000000
  # $mean
  # layer.1  layer.2 
  # 75.72688 30.31555 
    # what does this mean
  b <- int_chao / (int_chao + nat_chao)
  plot(b)
  # some kind of proportions plot of % of introduced species? Yeah... 
  # Seems to be a few in the anthropogenic areas!
  # and South. A lot South
  
  
############################  

# rank abundance curves
  par(mfrow = c(2, 1), mar = c(1, 5, 2, 1))
  a <- table(factor(poa_nat$species))
  a <- a[order(-a)]
  plot(log10(a), type = "l", xaxt = "n", ylab = "Log10(number of records)", main = "Native grasses",
       ylim = c(0, 3.5))
  
  a <- table(factor(poa_int$species))
  a <- a[order(-a)]
  plot(log10(a), type = "l", xaxt = "n", ylab = "Log10(number of records)", main = "Introduced grasses",
       ylim = c(0, 3.5))
  # again, don't know what this means, but relative to each other, looks pretty similar?
  
# save   
  pdf(file = "C:\\Users\\s436862\\Dropbox\\Climate Matching\\4. Results\\Poa_x_AP\\rank_abundance_curve_n_x_i.pdf", width = 7, heigh = 5)
  plot(log10(a), type = "l", xaxt = "n", ylab = "Log10(number of records)", main = "Native grasses",
       ylim = c(0, 3.5))
  plot(log10(a), type = "l", xaxt = "n", ylab = "Log10(number of records)", main = "Native grasses",
       ylim = c(0, 3.5))
  dev.off()
  
  par(mfrow = c(1,1))
  
  
############

# comparing genera for introduced and native overlaps   
# filter
  poa_spp <- group_by(poa, species) %>%
    filter(duplicated(species) == F) %>%
    dplyr:::select(order, family, genus, species, year, status)
  
# number of records per species
  poa.nr  <- group_by(poa, species) %>%
    summarise(n_rec = n()) %>%
    arrange(-n_rec)
  
  overlap <- group_by(poa_spp, genus, status) %>%
    summarise(n = n()) %>%
    spread(status, n) %>%
    filter(is.na(native) == F & is.na(introduced) == F)
  
  print.data.frame(overlap) 
  
#############

# for odd values: 'if' and 'else' functions
  
# me, my_class and last_5 are preloaded
# Embedded control structure: fix the error
  if (mean(my_class) < 80) {
    if (mean(my_class) > me) {
      print("average year, but still smarter than me")
    } else {
      print("average year, but I'm not that bad")
    }}
  } else {
    if (mean(my_class) > me) {
      print("smart year, even smarter than me")
    } else {
      print("smart year, but I am smarter")
    }
  }
##########

# For getting rid of NA values:
  
#  Hi Brian, 

# To get cell numbers for raster cell values greater than zero, what you need is: 
  
  cellnb.na <- Which(ras > 0, cells = TRUE) 
  
#   Take alook at: 
#    > ?Which 