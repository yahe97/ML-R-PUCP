## Pregunta 4 

# Conjunto de datos
library(ISLR)
View(College)
dim(College)
# Numero de valores nulo = 0 
sum( is.na(College) )
# Dividiendo entre variables dependientes e independientes
x = model.matrix( Apps~., College )[ , -1 ]
y = College$Apps 
names(College)

# a. División del conjunto de datos 
set.seed(1)
train = sample( 1:nrow(x) , nrow(x)*0.7)  
training_sample = College[ train , ]
testing_sample = College[ -train , ]
test = (-train)   
y.test = y[test] 
y.test

# b. Ajuste del modelo lineal OLS y MSE de prueba

model1 <- lm(Apps~Private+Accept+Enroll+Top10perc+Top25perc+
               F.Undergrad+P.Undergrad+Outstate+Room.Board+
               Books+Personal+PhD+Terminal+S.F.Ratio+
               perc.alumni+Expend+Grad.Rate,training_sample  )
predicted1<- fitted(model1)
training_sample$linear_model_predict <-  predicted1
mse = function( data, 
                 actual_col = 'y', 
                 predicted_col = 'predict' ) {
  
   mean( ( data[ , actual_col] - data[ , predicted_col ] ) ^ 2 )
}

predicted2<- predict( model1, newdata = testing_sample)
testing_sample$linear_model_predict <-  predicted2


MSE_linear <- mse( data = testing_sample, 
                     actual_col = 'Apps', 
                     predicted_col = 'linear_model_predict' )

# c. Modelo Ridge, lambda elegido de cross validation

## Ridge
library(glmnet)
dim(x[train,])
length(y[train])
grid = 10^seq( 10 ,-2 ,length=100 )
ridge.mod = glmnet( x[train,] ,y[train], alpha=0 , 
                    lambda=grid , thresh = 1e-12 )

cv.out = cv.glmnet( x[train,] , y[train] , alpha=0 )
bestlam = cv.out$lambda.min
bestlam
ridge.pred = predict( ridge.mod , s = bestlam , newx=x[test,] ) 
out = glmnet( x , y , alpha=0 )
predict(out , type="coefficients", s = bestlam )
MSE_ridge = mean((ridge.pred-y.test)^2)

# d. Modelo Lasso, lambda elegido de cross validation
lasso.mod = glmnet( x[train,] , y[train] , 
                    alpha=1 , lambda=grid )
cv.out = cv.glmnet( x[train,] , y[train] ,alpha=1 )
bestlam = cv.out$lambda.min
lasso.pred=predict( lasso.mod , s=bestlam , newx=x[test,]) 
MSE_lasso =mean((lasso.pred-y.test)^2)

# Predicciones de coeficientes
predict(out , type="coefficients", s = bestlam )
# e. Resumen de resultados

# Data Frame of results
mse_labels_test <- c( "Linear Model", "Ridge Model", "Lasso Model")
mse_values_test <- c( MSE_linear, MSE_ridge, MSE_lasso )
results <- data.frame( mse_labels_test, mse_values_test )
colnames( results ) <- c( "Models" , "MSE")
results
##    Models     MSE
#1 Linear Model 1261630
#2  Ridge Model 1120589
#3  Lasso Model 1254379
## Pregunta 5

library(ISLR2)
View(Boston)
# Dividiendo entre variables dependientes e independientes
x = model.matrix( crim~., Boston )[ , -1 ]
y = Boston$crim 
names(Boston)
grid = 10^seq( 10 ,-2 ,length=100 )
train = sample( 1:nrow(x) , nrow(x)*0.8)  
training_sample = Boston[ train , ]
testing_sample = Boston[ -train , ]
test = (-train)   
y.test = y[test] 
y.test
#OLs 
model1 <- lm(crim~zn+indus+chas+nox+rm+
               age+dis+rad+tax+ptratio+
               lstat+medv,training_sample  )
predicted1<- fitted(model1)
summary(model1)
training_sample$linear_model_predict <-  predicted1
mse = function( data, 
                actual_col = 'y', 
                predicted_col = 'predict' ) {
  
  mean( ( data[ , actual_col] - data[ , predicted_col ] ) ^ 2 )
}

predicted2<- predict( model1, newdata = testing_sample)
testing_sample$linear_model_predict <-  predicted2

MSE_linear <- mse( data = testing_sample, 
                   actual_col = 'crim', 
                   predicted_col = 'linear_model_predict' )
# OLS 2
# Usando las variables más significativas

model2 <- lm(crim~+zn+dis+rad+
               medv,training_sample )
predicted2<- fitted(model2)
summary(model2)
training_sample$linear_model_predict2 <-  predicted2
mse = function( data, 
                actual_col = 'y', 
                predicted_col = 'predict' ) {
  
  mean( ( data[ , actual_col] - data[ , predicted_col ] ) ^ 2 )
}

predicted3<- predict( model2, newdata = testing_sample)
testing_sample$linear_model_predict2 <-  predicted3

MSE_linear_2 <- mse( data = testing_sample, 
                   actual_col = 'crim', 
                   predicted_col = 'linear_model_predict2' )

#ridge 
ridge.mod = glmnet( x[train,] ,y[train], alpha=0 , 
                    lambda=grid , thresh = 1e-12 )

cv.out = cv.glmnet( x[train,] , y[train] , alpha=0 )
bestlam = cv.out$lambda.min
bestlam
ridge.pred = predict( ridge.mod , s = bestlam , newx=x[test,] ) 
out = glmnet( x , y , alpha=0 )
predict(out , type="coefficients", s = bestlam )
MSE_ridge = mean((ridge.pred-y.test)^2)

#ridge 2
ridge.mod = glmnet( x[train,c(1,7,8,12)] ,y[train], alpha=0 , 
                    lambda=grid , thresh = 1e-12 )

cv.out = cv.glmnet( x[train,c(1,7,8,12)] , y[train] , alpha=0 )
bestlam = cv.out$lambda.min
bestlam
ridge.pred = predict( ridge.mod , s = bestlam , newx=x[test,c(1,7,8,12)] ) 
out = glmnet( x , y , alpha=0 )
predict(out , type="coefficients", s = bestlam )
MSE_ridge2 = mean((ridge.pred-y.test)^2)


#lasso 
lasso.mod = glmnet( x[train,] , y[train] , 
                    alpha=1 , lambda=grid )
cv.out = cv.glmnet( x[train,] , y[train] ,alpha=1 )
bestlam = cv.out$lambda.min
lasso.pred=predict( lasso.mod , s=bestlam , newx=x[test,]) 
MSE_lasso =mean((lasso.pred-y.test)^2)
lasso.coef = predict( out , type="coefficients" , s=bestlam )
lasso.coef


# Data Frame of results
mse_labels_test <- c( "Linear Model","Second linear Model", "Ridge Model", "Lasso Model","Ridge Model 2 ")
mse_values_test <- c( MSE_linear,MSE_linear_2, MSE_ridge, MSE_lasso ,MSE_ridge2  )
results <- data.frame( mse_labels_test, mse_values_test )
colnames( results ) <- c( "Models" , "MSE")
results

##        Models      MSE
#1        Linear Model 12.04218
#2 Second linear Model 11.19673
#3         Ridge Model 11.04327
#4         Lasso Model 11.66977
#5      Ridge Model 2  10.39595

## Comentario : 
# Se ha evaluado 4 modelos en los cuales, se observa que el mejor modelo usando
# validación cruzada es ridge model ya que me minimiza el error. Se usaron 
# las variables con mayor significancia del segundo modelo lineal para ridge,
# que mejoró la minimización del error.

