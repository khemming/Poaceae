# Date created: 17/4/18
# Last updated:

# Lasso pilot/exploration ----------------------------------------------------------
# based on lasso.pdf (in Stats/Mulivariate)

library(glmnet)

# Cars data
  data(mtcars)
  mtcars
  
# Pick 3 variables; 0 = y; 1 + 2 = x
# (IDk why they decided to start at 0)  
  v0 <- "mpg"
  v1 <- "disp"
  v2 <- "hp"
  
# correlations among these variables
  round(cor(mtcars[, c(v0, v1, v2)]), 2) # cool
  
# the units of measurement for the different indepedent variables are quite different
# therefore we shall standardise them - and we will do the independent variable too for consistency
  standardize <- function(x) {(x-mean(x))/sd(x)}
  z0 <- standardize(mtcars[, v0])
  z1 <- standardize(mtcars[, v1])
  z2 <- standardize(mtcars[, v2])
  lstsq <- lm(z0~z1+z2-1)
  lstsq_beta <- coef(lstsq)
  
# The lm function uses the traditional linear regression approach, sometimes called least squares regression, because it minimizes (least) the sum of squared deviations (squares) of the residuals
# To understand this better, here is the residual sum of squared deviations (rss) for a range of possible regression coefficients
  n_lstsq <- 41
  s <- seq(-1, 1, length=n_lstsq)
  rss_lstsq <- matrix(NA, nrow=n_lstsq, ncol=n_lstsq)
  for (i in 1:n_lstsq) {
    for (j in 1:n_lstsq) {
      rss_lstsq[i, j] <- sum((z0-s[i]*z1-s[j]*z2)^2)
    }
  }
  persp(s, s, rss_lstsq, xlab="beta1", ylab="beta2", zlab="rss_lstsq")

# Here is the contour of the 3D plot
  draw_axes <- function() {
    k2 <- seq(-1, 1, length=5)
    par(mar=c(4.6, 4.6, 0.6, 0.6), xaxs="i", yaxs="i")
    plot(1.02*range(s), 1.02*range(s), type="n", xlab="beta1", ylab="beta2", axes=FALSE
    )
    axis(side=1, pos=0, col="gray", at=k2, labels=rep(" ", length(k2)))
    axis(side=2, pos=0, col="gray", at=k2, labels=rep(" ", length(k2)))
    text(k2[-3], -0.05, k2[-3], cex=0.5, col="black")
    text(-0.05, k2[-3], k2[-3], cex=0.5, col="black")
  }
  k1 <- c(1, 1.1, 1.2, 1.5, 2, 2.5, 3:9)
  k1 <- c(0.1*k1, k1, 10*k1, 100*k1, 1000*k1)
  draw_axes()
  contour(s, s, matrix(rss_lstsq,nrow=n_lstsq), levels=k1, add=TRUE, col="black")
  text(lstsq_beta[1], lstsq_beta[2], "X", cex=0.5)
  # which kind makes sense, looking top down
  # cool
  
  k1 <- k1[k1>min(rss_lstsq)]
  
# The level curves (the values where the thre dimension surface is constant) are elliptical
# which reflects the correlation in and that is induced by the correlation between z1 and z2
# The small "X" represents the minimum value, or the least squares solution 
# It corresponds to height of 7.81 units  
  
# Now suppose that you were willing to sacrifice a bit on the residual sum of squares. 
# You'd be willing to settle for a value of B1 and B2 that produced a residual sum of squares of 8 instead of 7.8. 
# In exchange, you'd get asolution that was a bit closer to zero. 
# What would that value be? 
# Any value on the ellipse labelled 8 would be equally desirable from the least squares perspective. 
# But the point on the ellipse closest to (0, 0) has the most simplicity.  
  find_closest <- function(x, target) {
    d <- abs(x-target)
    return(which(d==min(d))[1])
  }
  draw_circle <- function(r) {
    radians <- seq(0, 2*pi, length=100)
    lines(r*sin(radians), r*cos(radians))
  }
  ridge <- glmnet(cbind(z1, z2), z0, alpha=0, intercept=FALSE, nlambda=1000)
  m_ridge <- dim(ridge$beta)[2]
  rss_ridge <- rep(NA,m_ridge)
  for (i in 1:m_ridge) {
    rss_ridge[i] <- sum((z0 - ridge$beta[1, i]*z1 -ridge$beta[2, i]*z2)^2)
  }
  r1 <- find_closest(rss_ridge, k1[1])
  draw_axes()
  contour(s, s, matrix(rss_lstsq, nrow=n_lstsq), levels=k1, add=TRUE, col="gray")
  contour(s, s, matrix(rss_lstsq, nrow=n_lstsq), levels=k1[1], add=TRUE, col="black")
  draw_circle(sqrt(ridge$beta[1, r1]^2+ridge$beta[2, r1]^2))
  text(lstsq_beta[1], lstsq_beta[2], "X", cex=0.5)
  arrows(lstsq_beta[1], lstsq_beta[2], ridge$beta[1, r1], ridge$beta[2, r1], len=0.05)
  
#  For ridge regression, find the circle which just barely touches the ellipse corresponding to a level surface of 8.
#  These values are B1 = -0.52 and B2 = -0.32.
  
# Now what do I mean by "simplicity"? (Indeed.) 
# In this case, I mean less of a tendency to produce extreme predictions.
# Regression coefficients that are flatter, that is, closer to zero, have less of a tendency to produce extreme predictions
# Extreme predictions are sometimes okay, but they are often a symptom of overfitting  
  
# Ridge regression -------------------------------------------------  
# I think
  
# Now ridge regression offers you a multitude of choices, depending on the trade-offs you are willing to make between efficiency (small rss) and simplicity (regression coefficients close to zero). 
# If you wanted a bit more simplicity and could suffer a bit more on the residual sums of squares end of things, you could find the point on the level surface 9 or 10.
  r2 <- find_closest(rss_ridge, k1[2])
  r3 <- find_closest(rss_ridge, k1[3])
  draw_axes()
  contour(s, s, matrix(rss_lstsq,nrow=n_lstsq), levels=k1, add=TRUE, col="gray")
  contour(s, s, matrix(rss_lstsq,nrow=n_lstsq), levels=k1[2], add=TRUE, col="black")
  draw_circle(sqrt(ridge$beta[1, r2]^2+ridge$beta[2, r2]^2))
  text(lstsq_beta[1], lstsq_beta[2], "X", cex=0.5)
  arrows(lstsq_beta[1], lstsq_beta[2], ridge$beta[1, r1], ridge$beta[2, r1], len=0.05)
  arrows(ridge$beta[1, r1], ridge$beta[2, r1], ridge$beta[1, r2], ridge$beta[2, r2], len=0.05) 
  
  
# not sure what this one is about  
  draw_axes()
  contour(s, s, matrix(rss_lstsq,nrow=n_lstsq), levels=k1, add=TRUE, col="gray")
  contour(s, s, matrix(rss_lstsq,nrow=n_lstsq), levels=k1[3], add=TRUE, col="black")
  draw_circle(sqrt(ridge$beta[1, r3]^2+ridge$beta[2, r3]^2))
  text(lstsq_beta[1], lstsq_beta[2], "X", cex=0.5)
  arrows(lstsq_beta[1], lstsq_beta[2], ridge$beta[1, r1], ridge$beta[2, r1], len=0.05)
  arrows(ridge$beta[1, r1], ridge$beta[2, r1], ridge$beta[1, r2], ridge$beta[2, r2], len=0.05)
  arrows(ridge$beta[1, r2], ridge$beta[2, r2], ridge$beta[1, r3], ridge$beta[2, r3], len=0.05) 
  
   