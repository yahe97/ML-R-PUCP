#-------------------------------------------------------------------
# Tutorial 1: Linear Models
#-------------------------------------------------------------------

#-------------------------------------------------------------------
# Gradient Descent
#-------------------------------------------------------------------


#install.packages("ISLR")


#Calculating the coefficients of a linear regression model

library(ISLR)

#Info about Hitters data: https://rdrr.io/cran/ISLR/man/Hitters.html

x = model.matrix( Salary ~. , Hitters)[ , -1]

y = na.omit(Hitters$Salary)

x <- scale(x)
y <- scale(y)


# Y = beta * X
# Y_hat = beta * X + error


#1) R package
model <- lm(y ~ x-1)

model$coefficients


#2) Manual solution

#Residual sum squared

# RSS = (Y - X %*% beta)' (Y - X %*% beta)
# RSS = t(Y) %*% Y - 2 * t(Y) %*% X %*% beta + t(beta) %*% t(X) %*% X %*% beta
# dRSS/dbeta = - 2 * t(X) %*% Y + 2 * t(X) %*% X %*% beta
# set dRSS/dbeta = 0

c( solve( t(x) %*% x ) %*% t(x) %*% y )



# 3) Gradient Descent

#RSS

# RSS = (Y - X %*% beta)' (Y - X %*% beta)
# RSS = t(Y) %*% Y - 2 * t(Y) %*% X %*% beta + t(beta) %*% t(X) %*% X %*% beta
# dRSS/dbeta = - 2 * t(X) %*% Y + 2 * t(X) %*% X %*% beta

# Gradient = -2 * t(x) %*% (Y - X %*% beta)

# w(t+1) = w(t) - alpha * gradient

w0 <- runif( 19 )

# w1 <- w0 - alpha * ( -2 * t(x) %*% (Y - X %*%  w0) )

# alpha = 0.0005

w1 = w0 + 0.0005 * 2 * t(x) %*% (y - x %*% w0)

while(sum(w1 == w0) != 19){
  
  w0 <- w1
  w1 <- w0 + 0.0005 * 2 * t(x) %*% (y - x %*% w0) 
}

c(w1)


#-------------------------------------------------------------------
# Data generation
# Checking Training Error vs Test Error
#-------------------------------------------------------------------

# SIMULATIONS (training error vs test error)
rm( list = ls() )

# Set seed for random samples
set.seed(1)

# Generating 201 observations for X
X <- seq(from = 0, to = 20, by = 0.1)


# Data Generation Model DGM
dgm <- 500 + 20*X - 90*sin(X) 
plot( X, dgm )


# Generating y 
# Add some noise to data generation model
y = dgm + rnorm(length(X), mean = 10, sd = 100) 
plot( X, y )

# y and X in a data frame
data = data.frame( dgm, y , X )

# Training sample
# Split data in training and testing
# 80% of data
tot_obs = nrow( data )
idx = sample( tot_obs , size = 0.8 * tot_obs  )
training_sample = data[ idx , ]
testing_sample = data[ -idx , ]

#-------------------------------------------------------------------


# See some information
str(data)



#--------------------------
# Plotting training and DGM
#--------------------------

# Plot observed data
plot( X, y, 
      col = 'deepskyblue4', 
      xlab = 'X label', 
      main = 'Datos & DGM' )

# Plot Data Generation Model
lines(X, dgm, 
      col='firebrick1', 
      lwd=4) #DGM in red



#--------------------------
# Estimating with 3 models
#--------------------------

# We want to estimate a model which has the lowest possible error
# 1) Linear model

model1 <- lm(y ~ X, data = training_sample )


# 2) Polynomial model 

# y = x + x^2 + x^3 + x^4 + ..... + x^18
# Use I() isolate term to add additional variables
lm(y ~ X + I(X^2), data = training_sample )

# ^ is a special character in regression 
# It adds interactions between co-variables
X2 <- X ** 2
data2 = data.frame( y , X, X2 )
lm(y ~ X + (X)^2, data = data2 )

# polynomial regression
model2 <- lm(y ~ X + I(X^2) + I(X^3) + I(X^4) + 
               I(X^5) + I(X^6) + I(X^7) + I(X^8) +
               I(X^9) + I(X^10) + I(X^11) + I(X^12),
             data = training_sample )

# 3) Nonparametric Model (Spline)

model3 <- with( training_sample, 
                smooth.spline( training_sample$X, 
                               training_sample$y, 
                               all.knots = TRUE, 
                               df = 200 ) ) 

#--------------------------
# Root Mean Square Error (RMSE)
#--------------------------

rmse = function( data, 
                 actual_col = 'y', 
                 predicted_col = 'predict' ) {
  
  sqrt( mean( ( data[ , actual_col] - data[ , predicted_col ] ) ^ 2 ) )
}

# Getting predictions
predicted1<- fitted(model1)
predicted2<- fitted(model2)
predicted3<- fitted(model3)

# Add predicted values to data frame
training_sample$linear_model_predict <-  predicted1
training_sample$polynomial_predict <-  predicted2
training_sample$spline_predict <-  predicted3



#RMSE 
rmse_linear <- rmse( data = training_sample, actual_col = 'y', 
                     predicted_col = 'linear_model_predict' )
rmse_poly <- rmse( data = training_sample, actual_col = 'y', 
                   predicted_col = 'polynomial_predict' )
rmse_spline <- rmse( data = training_sample, actual_col = 'y', 
                     predicted_col = 'spline_predict' )

# Data Frame of results
rmse_labels <- c( "Linear Model", "Polynomial Model", "Spline df = 100")
rmse_values <- c( rmse_linear, rmse_poly, rmse_spline )
results <- data.frame( rmse_labels, rmse_values )
colnames( results ) <- c( "Models" , "RMSE")
results


# Models      RMSE
# 1     Linear Model 118.41108
# 2 Polynomial Model  85.42259
# 3  Spline df = 120  47.26163


# Plot: Linear model
library( ggplot2 )

ggplot( training_sample, aes( x = X, y = y ) ) + 
  geom_point( col = 'deepskyblue4' ) +
  geom_line( aes( y = linear_model_predict ), size = 1, col = 'blue') + 
  geom_line( aes( y = dgm ), size = 1, col = 'green')


# Plot: Polynomial model 

ggplot( training_sample, aes( x = X, y = y ) ) + 
  geom_point( col = 'deepskyblue4' ) +
  geom_line( aes( y = polynomial_predict ), size = 1, col = 'blue') + 
  geom_line( aes( y = dgm ), size = 1, col = 'green')


# Plot: Spline
ggplot( training_sample, aes( x = X, y = y ) ) + 
  geom_point( col = 'deepskyblue4' ) +
  geom_line( aes( y = spline_predict ), size = 1, col = 'blue') + 
  geom_line( aes( y = dgm ), size = 1, col = 'green')


#--------------------------
# Test Error
#--------------------------

#Now, with testing data set

# Get predictions in testing data
predicted1<- predict( model1, newdata = testing_sample )
predicted2<- predict( model2, newdata = testing_sample )
predicted3<- predict( model3, testing_sample$X )$y

# Add predicted values to dataframe
testing_sample$linear_model_predict <-  predicted1
testing_sample$polynomial_predict <-  predicted2
testing_sample$spline_predict <-  predicted3


#RMSE
rmse_linear <- rmse( data = testing_sample, 
                     actual_col = 'y', 
                     predicted_col = 'linear_model_predict' )
rmse_poly <- rmse( data = testing_sample, actual_col = 'y', 
                   predicted_col = 'polynomial_predict' )
rmse_spline <- rmse( data = testing_sample, actual_col = 'y', 
                     predicted_col = 'spline_predict' )


# Data Frame of results
rmse_labels_test <- c( "Linear Model", "Polynomial Model", "Spline df = 120")
rmse_values_test <- c( rmse_linear, rmse_poly, rmse_spline )
results <- data.frame( rmse_labels_test, rmse_values_test )
colnames( results ) <- c( "Models" , "RMSE")
results

#--------------------------
# FINAL OUTPUTS:
#--------------------------

# Testing Data
# Models     RMSE
# 1     Linear Model 124.4648
# 2 Polynomial Model 107.2970
# 3  Spline df = 120 122.0814

# Training Data
# Models      RMSE
# 1     Linear Model 118.41108
# 2 Polynomial Model  85.42259
# 3  Spline df = 100  47.26163

####### CROSS-VALIDATION AND BOOSTRAP


#####################
### Cross-Validation
####################

rm(list=ls())
# install.packages( "ISLR" )
library(ISLR)

# We will try to predict mpg (miles per gallon)


#-------------------------------
# 1. The validation Set Approach
#-------------------------------

set.seed(1)

str(Auto)

dim(Auto)

# Dividing the sample
# Take 196 numbers from 1 to 392
train = sample( x = 392, size = 196 )  #without replacement 


# Running regression with training data
# Train model with the whole data
lm.fit = lm( mpg ~ horsepower, data=Auto , subset=train )
lm.fit$coefficients
summary( lm.fit )

attach(Auto) # Call columns by names


# MSE (mean squared error) 
# Testing data
mean( ( mpg - predict( lm.fit, Auto) )[-train]^2 ) 


# Analogous but with polynomial of degree 2 (more complexity)

# mpg = BETA1*horsepower + BETA*horsepower^2

lm.fit2=lm( mpg ~ poly(horsepower,2), data=Auto , subset=train )

mean( ( mpg - predict(lm.fit2,Auto) )[-train]^2 )


# Analogous but with polynomial of degree 3
lm.fit3=lm( mpg ~ poly(horsepower,3), data=Auto ,subset=train ) 

mean( ( mpg-predict(lm.fit3,Auto) )[-train]^2 )



# A different random selection
set.seed(2)

train = sample(392,196)

lm.fit=lm(mpg~horsepower,subset=train)
mean((mpg-predict(lm.fit,Auto))[-train]^2)

lm.fit2=lm(mpg~poly(horsepower,2),data=Auto,subset=train)
mean((mpg-predict(lm.fit2,Auto))[-train]^2)

lm.fit3=lm(mpg~poly(horsepower,3),data=Auto,subset=train)
mean((mpg-predict(lm.fit3,Auto))[-train]^2)


## PLot 5.2 B
store_mse_matrix <- matrix(NA, 10, 11)

# Loop
for ( i1 in c(1:10)){
  set.seed( i1 )
  train <-  sample(392,196)
  mse_vec <-  rep(0,10)
  
  for ( i2 in c(1:10)){
    lm.fit2 <-  lm( mpg~poly( horsepower, i2 ),data=Auto,subset=train)
    mse_vec[ i2 ] <- mean((mpg-predict(lm.fit2,Auto))[-train]^2)
  }
  store_mse_matrix[ , i1 ] <- mse_vec
}
store_mse_matrix[, 11 ] <- c(1:10)
results <- data.frame( store_mse_matrix )

# Plot
ggplot( results, aes( x = X11, y = X1 ) ) + 
  geom_line( col = 'deepskyblue4' ) +
  geom_line( aes( y = X2 ), size = 1, col = 'blue') + 
  geom_line( aes( y = X3 ), size = 1, col = 'green') + 
  geom_line( aes( y = X4 ), size = 1, col = 'black') + 
  geom_line( aes( y = X5 ), size = 1, col = 'pink') + 
  geom_line( aes( y = X6 ), size = 1, col = 'yellow') + 
  geom_line( aes( y = X7 ), size = 1, col = 'gray') + 
  geom_line( aes( y = X8 ), size = 1, col = 'orange') + 
  geom_line( aes( y = X9 ), size = 1, col = 'red') + 
  geom_line( aes( y = X10 ), size = 1, col = 'magenta') +
  labs( x = "Degree of Polynomial", y = 'Mean Squared Error' ) +
  theme_classic()




#-------------------------------
# 2. Leave-One-Out Cross-Validation
#    (LOOCV)
#-------------------------------

# LOOCV involves one fold per observation

#There is nothing random here 

#lGLM models allow us to build a linear relationship between the response and predictors, 
#even though their underlying relationship is not linear

glm.fit = glm( mpg ~ horsepower, data=Auto ) 
coef(glm.fit)

lm.fit=lm( mpg ~ horsepower, data=Auto )
coef(lm.fit)


library(boot)

glm.fit = glm( mpg ~ horsepower , data=Auto )

#It gives us the LOOCV prediction error by default 
cv.err = cv.glm( Auto , glm.fit )

#Delta delivers cross validation error 
#delta[1] for LOOCV and delta[2] 
cv.err$delta

cv.err$delta[1]




# mpg = BETA1*horsepower 

# mpg = BETA1*horsepower + BETA*horsepower^2

# mpg = BETA1*horsepower + BETA*horsepower^2 + BETA*horsepower^3

# mpg = BETA1*horsepower + BETA*horsepower^2 + BETA*horsepower^3 + BETA*horsepower^4

# mpg = BETA1*horsepower + BETA*horsepower^2 + BETA*horsepower^3 + BETA*horsepower^4 + BETA*horsepower^5
# Vector of zeros
# Store MSE results 
cv.error = rep(0,5)
for (i in 1:5){
  
  glm.fit = glm( mpg ~ poly(horsepower,i), data=Auto )
  cv.error[i] = cv.glm( Auto , glm.fit )$delta[1]
  
}
cv.error

# Form a little data frame for plotting
cv.df <- data.frame(degree = 1:5,
                    cv.error = cv.error)

qplot(data = cv.df, x = degree, y = cv.error, geom = "line",
      ylab = "LOOCV error estimate") + geom_point()



#-------------------------------
# 3. K-Fold Cross-Validation
#-------------------------------


# Set seed

set.seed(17)
cv.error.10 = rep(0,10)

for (i in 1:10){
  glm.fit = glm( mpg ~ poly(horsepower,i), data=Auto )
  cv.error.10[i] = cv.glm( Auto , glm.fit , K=10 )$delta[2] # Use K to generate k-folds
  # About Delta = The second component is the adjusted cross-validation estimate.
  # The adjustment is designed to compensate for the bias introduced by not using leave-one-out cross-validation.
} 

cv.error.10

# Form a little data frame for plotting
cv.df <- data.frame(degree = 1:10,
                    cv.error = cv.error.10)

qplot(data = cv.df, x = degree, y = cv.error, geom = "line",
      ylab = "10-fold CV error estimate") + geom_point()



#####################
### The Bootstrap
####################

# Function to get bootstrap sample
alpha.fn <- function(data,index){
  X=data$X[index]
  Y=data$Y[index]
  return((var(Y)-cov(X,Y))/(var(X)+var(Y)-2*cov(X,Y)))
}

alpha.fn(Portfolio,1:100)

set.seed(1)

alpha.fn(Portfolio,sample(100,100,replace=T))

boot(Portfolio,alpha.fn,R=1000)


# Estimating the Accuracy of a Linear Regression Model

boot.fn=function(data,index)
  return(coef(lm(mpg~horsepower,data=data,subset=index)))

boot.fn(Auto,1:392)

set.seed(1)

boot.fn(Auto,sample(392,392,replace=T))
boot.fn(Auto,sample(392,392,replace=T))
boot(Auto,boot.fn,1000)
summary(lm(mpg~horsepower,data=Auto))$coef

boot.fn=function(data,index)
  coefficients(lm(mpg~horsepower+I(horsepower^2),data=data,subset=index))

set.seed(1)

boot(Auto,boot.fn,1000)
summary(lm(mpg~horsepower+I(horsepower^2),data=Auto))$coef