# Bernd's regression lesson - W3

# Section 8: multivariate data

weight <- c(150, 135, 210, 140)
height <- c(65, 61, 70, 65)
gender <- c("F", "F", "M", "F")

study <- data.frame(weight, height, gender)
study

study2 = data.frame(w = weight, h = height, g = gender)

row.names(study) <- c("Mary", "Alice", "Bob", "Judy")

data() # interesting

PlantGrowth # interesting

weight.ctrl <- PlantGrowth$weight[PlantGrowth$group == "ctrl"]
weight.ctrl # not sure what's going on here

unstack(PlantGrowth) # super cool data wrangling thing
# reverse is stack

# reading in some gecko stuff
  setwd("C:/Users/s436862/Dropbox/Stats/BioStats101")

  geckos <- read.csv("geckos_anova.csv")
  
  boxplot(weight ~ group, data = PlantGrowth)
  
# Linear regression: section 9
# 5 steps: 
# #1  scatter plot
# #2 add a null modell (no slope)
# #3 calculate the correlation coefficient  
# #4 do regression and check summary output
# #5 check assumptions
  
  geckonumber <- 1:6
  bodysize <- c(34, 108, 64, 88, 99, 51)
  tail <- c(5, 17, 11, 8, 14, 5)
  geckos <- data.frame(geckonumber = geckonumber, bodysize = bodysize, tail = tail)
  
  geckos
  summary(geckos)
  
  x <- geckos$bodysize
  y <- geckos$tail
  
  #1
  plot(x, y, ylim = c(0, 18), pch = 16, col = "red")
  
  #2
  abline(h = mean(geckos$tail), col = "red", lty = 2) # if our line = this one, we expect no relationship
  
  
  for (i in 1:6) lines(c(bodysize[i], bodysize[i]),
                       c(tail[i], 10), col = "red", lwd = 3) # super cool
  
  # actual regression line
    abline(lm(tail ~ bodysize), col = "blue", lwd = 2)
  # and residuals
    fitted <- predict(lm(tail ~ bodysize))
    for (i in 1:6) lines(c(bodysize[i], bodysize[i]) +
                           0.2, c(tail[i], fitted[i]), col = "blue",
                         lwd = 2) # uber super cool
  
  
  #3 correlation coefficient
    cor(bodysize, tail)
  #  [1] 0.9670111 # wow
    
  #4a do regression: print coefficients
    m1 <- lm(tail ~ bodysize, data = geckos)
    summary(m1)  
  
  #4b do anove
    anova(m1)
    # they're different
  
  #5 check asummptions (should do this first) 
    hist(resid(m1))
    par(mfrow = c(2,2))
    plot(m1)    
    
    
    
    
    
    
    
    
  
  