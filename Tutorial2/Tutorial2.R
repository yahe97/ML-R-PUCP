#----------------------------------------------------------------------------
# The Bootstrap
#----------------------------------------------------------------------------

library(ISLR)

# Function to get bootstrap sample
# Var(αX + (1 − α)Y ) we choos alpha to reduce risk
# Full Sample
alpha.fn <- function(data,index){
  X=data$X[index]
  Y=data$Y[index]
  return( ( var( Y )-cov( X,Y ) )/( var( X )+var( Y )-2*cov( X, Y ) ) )
}
alpha.fn(Portfolio,1:100)

# Full sample
library(boot)
set.seed(1)
alpha.fn( Portfolio,sample( 100, 100, replace = T ) )
res <- boot(Portfolio,alpha.fn,R=1000)
coef_1 <- summary(lm( res$t ~ 1))$coef[1]

plot(density(res$t), 
     col = "lightblue", lwd = 2, 
     xlab = "Values", 
     ylab = "Density", 
     main = "Density plot of Risk")
abline(v = coef_1, col = "red", lwd = 2)


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


####### RIDGE AND LASSO

# clean environment
rm(list=ls()) 

# call ISLR library
library(ISLR)  

# See Hitters data
View(Hitters)

# See name of columns of data
names(Hitters) 

# Get dimension of data
dim(Hitters)


# 1 Step : Drop NA Values
# Shows the number of observations with missing values /glmnet does not accept missing values
sum( is.na(Hitters$Salary) )

# deleting missing obs
Hitters = na.omit(Hitters)
sum( is.na(Hitters$Salary) )

# Get dimension of data
# 322  20
dim(Hitters)

# Get the number os NA
sum( is.na(Hitters) )

A = model.matrix( Salary~., Hitters )
# Get Covariables
x = model.matrix( Salary~., Hitters )[ , -1 ] #all independent vars
# See dimension of X matrix
dim( x )
# We have 19 variables
# Get the outcome of variable Y
y = Hitters$Salary  #outcome 



#-------------------------------
# Ridge
#-------------------------------

library(glmnet)

## Define the set of lambda values
# The highest values of lambda are at the beginning
# seq start, end number of observations
grid = 10^seq( 10 ,-2 ,length=100 )
grid

# y = BETA0 +  BETA1*X + BETA2*X2 + .... +BETA19*X19 + lambda 

#glmnet by default standardizes all vars 
ridge.mod = glmnet( x, y, alpha = 0, lambda = grid )  #alpha 0 = ridge 
ridge.mod

dim( coef(ridge.mod) )  #100 lambda's values, for each lambda there are 20 estimated coefficients (19 vars + intercept)
coef(ridge.mod)



## What happens to the coefficients when lambda changes? 
# If lambda is high, the coefficients will tend to be zero  
ridge.mod$lambda[ 1 ]
coef(ridge.mod)[, 1 ]

ridge.mod$lambda[ 100 ]
coef(ridge.mod)[,100]


# Sum of coefficients
sum( coef(ridge.mod)[-1, 20 ]^2 )
sum(coef(ridge.mod)[-1, 100 ]^2) 
sum(coef(ridge.mod)[-1,1]^2)


# We can obtain the ridge regression coefficients for a new value of lambda, say 50
predict( ridge.mod, s = 100 , type="coefficients" )[1:20,]



## Now we divide the sample into two groups

# Training and Test Set (or validation)
set.seed(1)
train = sample( 1:nrow(x) , nrow(x)/2) #half of the obs
test = (-train)   
y.test = y[test] 


## We implement the ridge regression over all possible values of lambda, 
# but using training sample
ridge.mod = glmnet( x[train,] ,y[train], alpha=0 , 
                    lambda=grid , thresh = 1e-12 )


## Predicting with the trained model on the test set for lambda=4
ridge.pred = predict( ridge.mod , s=4 , newx=x[test,])


#Test MSE
mean((ridge.pred - y.test)^2)


#Test MSE
ridge.pred=predict(ridge.mod, s=1e10,newx=x[test,]) #with high lambda
mean((ridge.pred-y.test)^2)


ridge.pred=predict(ridge.mod,s=0,newx=x[test,],exact=T,x=x[train,],y=y[train]) #with lambda=0
mean((ridge.pred-y.test)^2)



## WITH WHICH lambda IS OBTAINED THE LOWEST MSE (MEAN SQUARED ERROR)?

# Cross validation

dim(x[train,])
length(y[train])
set.seed(1)


## Carrying out the ridge regression for different values of lambda
cv.out = cv.glmnet( x[train,] , y[train] , alpha=0 )
plot(cv.out)
bestlam = cv.out$lambda.min
bestlam  #the best  lambda -> min MSE


## We predict the ridge value with the best lambda (in the test set)
ridge.pred = predict( ridge.mod , s = bestlam , newx=x[test,] ) 

mean((ridge.pred-y.test)^2)


## Finally, the model is reestimated using all the observations in the sample and the value of lambda selected
out = glmnet( x , y , alpha=0 )

predict(out , type="coefficients", s = bestlam )[1:20,]



#-------------------------------
# The Lasso
#-------------------------------

# Train the model with train data
lasso.mod = glmnet( x[train,] , y[train] , alpha=1 , lambda=grid )

lasso.mod$lambda[ 20 ]
coef(lasso.mod)[, 20 ]

lasso.mod$lambda[ 100 ]
coef(lasso.mod)[, 100 ]

## Predicting with the trained model on the test set for lambda=4
lasso.pred = predict( lasso.mod , s=4 , newx=x[test,])
#Test MSE
mean((lasso.pred - y.test)^2)


#Test MSE
lasso.pred=predict(lasso.mod, s=1e10,newx=x[test,]) #with high lambda
mean((lasso.pred-y.test)^2)


# Set seed
set.seed(1)

# Estimate the model using Cross Validation
cv.out = cv.glmnet( x[train,] , y[train] ,alpha=1 )

# Plot 
plot(cv.out)

# Get best lambda
bestlam = cv.out$lambda.min

# Get prediction using the best lambda and test data
lasso.pred=predict( lasso.mod , s=bestlam , newx=x[test,]) 

# Get MSE
mean((lasso.pred-y.test)^2)

# Train the data using different lambdas values
out=glmnet( x , y , alpha=1, lambda=grid ) 

# Get coefficients 
lasso.coef = predict( out , type="coefficients" , s=bestlam )[1:20,]
lasso.coef

# Get coefficients  != 0 
lasso.coef[lasso.coef==0]


## Comparing outcomes

set.seed(123)

# Estimae Beta
n    <- 1000
p   <-  100
X   <- matrix(rnorm(n*p,0,1),n,p)
beta <- rnorm(p,0,1)
Y    <- X%*%beta+rnorm(n,0,0.5)

beta1 <- solve( t(X) %*% X + 5*diag(p) , t(X)%*%Y )

# Get Beta from glmnet
beta2 <- glmnet(X,Y, alpha=0, lambda=10, 
                intercept=FALSE, standardize=FALSE, 
                family="gaussian")$beta@x

# See diference
beta1-beta2


## Get right measures
# the penalty used in GLMNET needs to be scaled by a factor of y/N
# n observations
n    <- 1000
# p predictors
p   <-  100
#Get X matrix
X   <- matrix(rnorm(n*p,0,1),n,p)

# generate beta
beta <- rnorm(p,0,1)

# Get Y
Y    <- X%*%beta+rnorm(n,0,0.5)

# Get Std of Y
sd_y <- sqrt(var(Y)*(n-1)/n)[1,1]

# Estimate Beta
beta1 <- solve(t(X)%*%X+10*diag(p),t(X)%*%(Y))[,1]

# Use glment
fit_glmnet <- glmnet(X,Y, alpha=0, 
                     standardize = F, 
                     intercept = FALSE, thresh = 1e-20)

# Get second beta
beta2 <- as.vector(coef(fit_glmnet, s = sd_y*10/n, 
                        exact = TRUE,  x = X, y = Y, ))[-1]

# See results
cbind(beta1, beta2)


### With intercept
# Get mean of columns
mean_x <- colMeans(X)
# STD
sd_x <- sqrt(apply(X,2,var)*(n-1)/n)

# Scaled
X_scaled <- matrix(NA, nrow = n, ncol = p)
for(i in 1:p){
  X_scaled[,i] <- (X[,i] - mean_x[i])/sd_x[i] 
}
X_scaled_ones <- cbind(rep(1,n), X_scaled)

# 
beta3 <- solve(t(X_scaled_ones)%*%X_scaled_ones+1000*diag(x = c(0, rep(1,p))),t(X_scaled_ones)%*%(Y))[,1]
beta3 <- c(beta3[1] - crossprod(mean_x,beta3[-1]/sd_x), beta3[-1]/sd_x)

fit_glmnet2 <- glmnet(X,Y, alpha=0, thresh = 1e-20)
beta4 <- as.vector(coef(fit_glmnet2, s = sd_y*1000/n, exact = TRUE, x = X, y = Y))

cbind(beta3[1:10], beta4[1:10])



### No intercept
set.seed(123)

n <- 1000
p <-  100
X <- matrix(rnorm(n*p,0,1),n,p)
beta <- rnorm(p,0,1)
Y <- X%*%beta+rnorm(n,0,0.5)

sd_y <- sqrt(var(Y)*(n-1)/n)[1,1]

mean_x <- colMeans(X)
sd_x <- sqrt(apply(X,2,var)*(n-1)/n)

X_scaled <- matrix(NA, nrow = n, ncol = p)
for(i in 1:p){
  X_scaled[,i] <- (X[,i] - mean_x[i])/sd_x[i] 
}

beta1 <- solve(t(X_scaled)%*%X_scaled+10*diag(p),t(X_scaled)%*%(Y))[,1]

fit_glmnet <- glmnet(X_scaled,Y, alpha=0, standardize = F, intercept = 
                       FALSE, thresh = 1e-20)

beta2 <- as.vector(coef(fit_glmnet, s = sd_y*10/n, exact = TRUE,  x = X, y = Y))[-1]
cbind(beta1[1:10], beta2[1:10])
