### GGplot codes
library(ggthemes)

# Notes from Andrew about automating some of this:
# One can call up themes like functions, if I save a theme that I like (let's call it 'Kyle_is_awesome_theme)
# and I simply put it in a function, and load the file with that function in it (with the libraries needed etc.)
# and it will do it all for me! Then, if I want to change the plot -- can update source file, and have it changed for all of the plots that use it :)

# NOTE: to use rasters with GGplot, load rasterVis package :) :) 

# Richness plots from rasters
################# 
q <- gplot(log(int_chao)) + 
  theme_classic()+
  geom_raster(aes(fill = value)) +
  scale_fill_gradient(low = 'white',
                      high = 'red',
                      na.value="white") +
  coord_equal()

p <- q + guides(fill=guide_legend(title="Species richness")) +
  xlab("Long") + 
  ylab("Lat") +
  oz + 
  xlim(110, 160) +
  ylim(-45, -10)

print(p)

# save
ggsave("C:/Users/s436862/Dropbox/Climate Matching/4. Results/Poa/introduced.richness_log.choa.corrected.jpeg", plot = last_plot(), scale = 1, dpi = 200, device = "jpeg")


#### together
s <- stack(log(int_chao), log(nat_chao))
names(s) <- c("Introduced","Native")


q <- gplot(s) + 
  theme_classic()+
  geom_raster(aes(fill = value)) +
  facet_wrap(~ variable) +
  scale_fill_gradient(low = 'white',
                      high = 'red',
                      na.value="white") +
  coord_equal()

p <- q + guides(fill=guide_legend(title="Species richness")) +
  xlab("Lat") + 
  ylab("Long") +
  oz + 
  xlim(110, 160) +
  ylim(-45, -10)

print(p)

# save
ggsave("C:/Users/s436862/Dropbox/Climate Matching/4. Results/Poa/poa.richness_log.chao.corrected.jpeg", plot = last_plot(), scale = 1, dpi = 200, device = "jpeg")


###### Species records plots
################################   Anotehr method of plotting:

q <- gplot(s) + 
  theme_bw()+
  geom_raster(aes(fill = value)) +
  facet_wrap(~ variable) +
  theme(aspect.ratio = 1)

print(q)
# To do:
# (1) remove grey bg
# (2) x y -> call lat long 
# (3) legend (value), put at top w explanation: "chao corrected log transformed spp rich"
# (4) plot ratio     

ggsave("C:\\Users\\s436862\\Dropbox\\Climate Matching\\4. Results\\Chao corrected Poa\\corrected_spp_rich.jpeg", plot = last_plot(), scale = 1, device = "jpeg")
#################

# long-wide transformation & plota
################
##### GGplot with Anthony  
# 15/8: So now I would like to plot poa aganst 11 EF as scatterplots

# log all data
log.poa_ef <- log(poa_ef)

# GGplot likes long data format
poa_long <- gather(log.poa_ef, parameter, value, bio_1:pet_he_yr, factor_key = T)
# paramter = EF name
# vlaue = grid cell value
# factor.key = treat the new key (parameter) column as a factor 
# (instead of character vector)

ggplot(poa_long,aes(y = poa_v, x = value, group = parameter)) +
  geom_point(size = 0.5, na.rm = T) +
  geom_smooth(method = "lm", na.rm = T)+
  facet_wrap(~ parameter, scale = "free" )

ggsave("C:\\Users\\s436862\\Dropbox\\Climate Matching\\4. Results\\11 environmental factors\\Poa_11.ef_scatterplots.jpeg", plot = last_plot(), scale = 4, device = "jpeg")
# Note: save it with size = 2; 0.5 is better for -> window, though

##### plotting subsets of the parameters in long form

poa_long$parameter <- as.factor(poa_long$parameter)
poa_long.subset <- filter(poa_long, parameter == "bio_1" | parameter == "bio_12") %>%
  droplevels()


ggplot(poa_long.subset,aes(y = poa_v, x = value, group = parameter)) +
  geom_point() +
  facet_wrap(~ parameter, scale = "free") +
  #geom_errorbar(aes(ymin = lcl.r, ymax = ucl.r), width = 0.04, position = dodge) +
  labs(x = "", y = "") +
  #geom_line(dat = egl.aug.log, aes(y = yy, x = xx), size =2) +
  #geom_line(dat = hol.aug.log, aes(y = yy, x = xx), size = 2) +
  geom_hline(yintercept = 0, lty = 5 ) +
  theme_classic() +
  theme_bw() +
  #choosen colour palette
  theme(plot.title = element_text(size = 20),
        plot.subtitle=element_text(size=16, hjust=0.5, face="italic", color="blue"),
        legend.position = "none",
        legend.background = element_rect(fill="white", size=1, linetype="solid", colour ="black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y=element_text(colour = "black",size =12),
        axis.title.x = element_text(colour = "black",size =14),
        axis.text.x=element_text(size = 14),
        axis.text.y=element_text(colour = "black",size = 14),
        axis.ticks.x = element_line(size = 1),
        axis.ticks.y =element_line(size = 1),
        axis.line.x = element_line(size = 1),
        axis.line.y = element_line(size = 1)) +
  #scale_y_continuous(limits=c(-2.5,2)) +
  #scale_x_continuous(limits=c(0,4)) +
  ggtitle(sub = "", "")


##########
##### GGplot with Anthony  
# 15/8: So now I would like to plot poa aganst 11 EF as scatterplots

# log all data
log.poa_ef <- log(poa_ef)

# GGplot likes long data format
poa_long <- gather(log.poa_ef, parameter, value, bio_1:pet_he_yr, factor_key = T)
# paramter = EF name
# vlaue = grid cell value
# factor.key = treat the new key (parameter) column as a factor 
# (instead of character vector)

ggplot(poa_long,aes(y = poa_v, x = value, group = parameter)) +
  geom_point(size = 0.5, na.rm = T) +
  geom_smooth(method = "lm", na.rm = T)+
  facet_wrap(~ parameter, scale = "free" )

ggsave("C:\\Users\\s436862\\Dropbox\\Climate Matching\\4. Results\\11 environmental factors\\Poa_11.ef_scatterplots.jpeg", plot = last_plot(), scale = 4, device = "jpeg")
# Note: save it with size = 2; 0.5 is better for -> window, though

##### plotting subsets of the parameters in long form

poa_long$parameter <- as.factor(poa_long$parameter)
poa_long.subset <- filter(poa_long, parameter == "bio_1" | parameter == "bio_12") %>%
  droplevels()


ggplot(poa_long.subset,aes(y = poa_v, x = value, group = parameter)) +
  geom_point() +
  facet_wrap(~ parameter, scale = "free") +
  #geom_errorbar(aes(ymin = lcl.r, ymax = ucl.r), width = 0.04, position = dodge) +
  labs(x = "", y = "") +
  #geom_line(dat = egl.aug.log, aes(y = yy, x = xx), size =2) +
  #geom_line(dat = hol.aug.log, aes(y = yy, x = xx), size = 2) +
  geom_hline(yintercept = 0, lty = 5 ) +
  theme_classic() +
  theme_bw() +
  #choosen colour palette
  theme(plot.title = element_text(size = 20),
        plot.subtitle=element_text(size=16, hjust=0.5, face="italic", color="blue"),
        legend.position = "none",
        legend.background = element_rect(fill="white", size=1, linetype="solid", colour ="black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y=element_text(colour = "black",size =12),
        axis.title.x = element_text(colour = "black",size =14),
        axis.text.x=element_text(size = 14),
        axis.text.y=element_text(colour = "black",size = 14),
        axis.ticks.x = element_line(size = 1),
        axis.ticks.y =element_line(size = 1),
        axis.line.x = element_line(size = 1),
        axis.line.y = element_line(size = 1)) +
  #scale_y_continuous(limits=c(-2.5,2)) +
  #scale_x_continuous(limits=c(0,4)) +
  ggtitle(sub = "", "")

####### plotting 1 EF versus poa

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



# linear scatter plot 6/10/2017 -------------------------------------------

# used for Chao estimators for grid cell vs. chao metric analysis, in my case

ggplot(data = out, aes(x = cell_width, y = corrected_mean)) +
  geom_line(size = 1, alpha = 0.8) +
  geom_point(size = 3) +
  # geom_hline(yintercept = 0.1, lty = 2, alpha = 0.3) +
  # geom_hline(yintercept = 0.05, alpha = 0.3) +
  # geom_hline(yintercept = 0.01, lty = 2, alpha = 0.3) +
  theme(axis.line = element_line(colour = 'black', size = 0.65),
        axis.ticks = element_line(colour = "black", size = 0.65),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.key = element_blank(),
        strip.background = element_rect(fill = "white", colour = "black", size = 1),
        panel.background = element_rect(fill = "white", colour = NA),
        axis.text = element_text(size = rel(0.8), colour = "black")) +
  # scale_y_continuous(breaks = c(0.01,0.05,0.10,0.25,0.5,0.75,1), limits = c(0,200)) +
  # scale_x_continuous(breaks = c(3.31,3.4,3.6,3.8,3.9,4.0,4.1,4.2), limits = c(3.31,4.25),expand = c(0,0)) +
  xlab("cell width (km)") +
  ylab("corrected mean SR")
