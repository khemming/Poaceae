###################


## Bagging AKA 'bootstrapping' from the internet

### 30/5

set.seed(10)  
# what does '10' specify?
y<-c(1:1000)  
x1<-c(1:1000)*runif(1000,min=0,max=2)  
x2<-c(1:1000)*runif(1000,min=0,max=2)  
x3<-c(1:1000)*runif(1000,min=0,max=2)
?set.seed
# still don't know what the 10 is, 
?runif
# or what the stuff does (referred to as 'random errors added') - it does just gives random deviates from the distribution 
# allg

# As you can see, y is a sequence of the values from 1 to 1000. x1, x2, and x3 are permutations of y, 
# but with random errors added. runif generates a specified number of random numbers from 0 to 1, 
# unless a min and max are specified, in which case the numbers fall between those values. 
# Each of the x sequences will roughly approximate y, but with random errors thrown in. 
# The set.seed function is simply to ensure that the subsequent random number generation proceeds in a 
# predictable fashion, so that your results match mine.

# Next, fitting a linear model to the variables results in an R squared of .7042:
  
lm_fit<-lm(y~x1+x2+x3)  
summary(lm_fit) 
# not a bad R2 value to be honest ...

# Now we will see how well the x values predict y. 
# First, we designate a random sample of y to be our "test" set. 
# The rest will be the training set.

set.seed(10)  
all_data<-data.frame(y,x1,x2,x3)  
positions <- sample(nrow(all_data),size=floor((nrow(all_data)/4)*3))  
training<- all_data[positions,]  
testing<- all_data[-positions,]

# The above code places all of our variables into a data frame, 
# then randomly selects 3/4 of the data to be the training set, and places the rest into the testing set.
# Cool - using 3/4 of the datat to see how well it predicts the other 1/4 :) 

# We are now able to generate predictions for the testing set by creating a linear model on the training set 
# and applying it to the testing set. We are also able to calculate the prediction error by subtracting 
# the actual values from the predicted values (the error calculation here is root mean squared error):

lm_fit<-lm(y~x1+x2+x3,data=training)  
predictions<-predict(lm_fit,newdata=testing)  
error<-sqrt((sum((testing$y-predictions)^2))/nrow(testing))  

# The calculated error should be 161.15.

# The next step is to run a function that implements bagging. |
# In order to do this, I will be using the foreach package. 
# Although I will not use it in parallel mode, this code is designed for parallel execution, and 
# I highly recommend reading my post about how to do it if you do not know how.
# ('my post' is just talking about looping stuff)

library(foreach)  
length_divisor<-4  
iterations<-1000  
predictions<-foreach(m=1:iterations,.combine=cbind) %do% {  
  training_positions <- sample(nrow(training), size=floor((nrow(training)/length_divisor)))  
  train_pos<-1:nrow(training) %in% training_positions  
  lm_fit<-lm(y~x1+x2+x3,data=training[train_pos,])  
  predict(lm_fit,newdata=testing)  
  }  
predictions<-rowMeans(predictions)  
error<-sqrt((sum((testing$y-predictions)^2))/nrow(testing)) 
# Symbols. Symbols everywhere


# The above code randomly samples 1/4 of the training set in each iteration, and 
# generates predictions for the testing set based the sample. 
# It will execute the number of time specified by iterations. When iterations was set to 10, 
# I received an error value of 161.10. At 300 iterations, error went to 161.12, at 500 iterations, 
# error went to 161.19, at 1000 iterations, error went to 161.13, and at 5000 iterations, error went to 161.07. 
# Eventually, bagging will converge, and more iterations will not help any further. 
# However, the potential for improvement in results exists. 
# You should be extremely cautious and assess the stability of the results before deploying this approach, 
# however, as too few iterations or too large a length divisor can cause extremely unstable results. 
# This example is trivial, but this can lead to better results in a more "real-world" application.

# Finally, we can place this code into a function to wrap it up nicely:
  
  bagging<-function(training,testing,length_divisor=4,iterations=1000)  
  {  
    predictions<-foreach(m=1:iterations,.combine=cbind) %do% {  
      training_positions <- sample(nrow(training), size=floor((nrow(training)/length_divisor)))  
      train_pos<-1:nrow(training) %in% training_positions  
      lm_fit<-lm(y~x1+x2+x3,data=training[train_pos,])  
      predict(lm_fit,newdata=testing)  
    }  
    rowMeans(predictions)  
  }  

  # As you can see, bagging can be a useful tool when used correctly. 
  # Although this is a trivial example, you can replace the data and even replace the simple linear model 
  # with more powerful models to make this function more useful.








