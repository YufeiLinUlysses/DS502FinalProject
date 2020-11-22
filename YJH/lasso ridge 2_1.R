library(caret)
library(glmnet)
library(dplyr)
library(ggplot2)
library(corrplot)
library(knitr)
library(skimr)
library(kableExtra)
library(purrr)
library(pander)
library(rmarkdown)

set.seed(1)
getwd()
setwd("C://Users//86084/Desktop/wpi/2020 fall/DS502/final project")
traindata = read.csv('new dataset/train_p2.csv')
testdata = read.csv('new dataset/test_p2.csv')




#### RIDGE REGRESSION
## Ridge regression is very similar to linear regression, ridge regression is also try
## to minimize the RSS, but it add a penalty term, try to prevent cause overfitting 
## when add more predictors

### Prepare Model

### bootstrap the training data
set.seed(2)
bs = sample(dim(traindata)[1],dim(traindata)[1],replace = T)
train = traindata[bs,]
test = testdata
train$SalePrice = log(train$SalePrice)
test$SalePrice = log(test$SalePrice)

X_train = model.matrix(SalePrice~.,data = train)[,-1]
X_test = model.matrix(SalePrice~.,test)[,-1]
y_train = train$SalePrice
y_test = test$SalePrice


### set the initial lambda 
set.seed(1234)
grid=10^seq(10,-2, length =100)
Ridge.Alpha=0
Ridge.Fit = glmnet(X_train, y_train, alpha=Ridge.Alpha, lambda=grid)
Ridge.Fitcv = cv.glmnet(X_train, y_train, alpha = Ridge.Alpha,nfolds = 10,type.measure = 'deviance')

### choose the optimal lambda
best.lambda1 = Ridge.Fitcv$lambda.min
best.lambda1
## with crossvaldiation, we get the optimal lambda is equal to 0.0341
plot(Ridge.Fitcv)

##### Check Accuarcy
Ridge.Pred <- predict(Ridge.Fit, s= best.lambda1, newx = X_test)
Ridge.Pred = exp(Ridge.Pred)
accuracy = mean(abs(exp(y_test) - (Ridge.Pred))/exp(y_test) <=0.05)
accuracy
##### Error Matrix
# let look at the MSE get from Ridge
sqrt(mean((Ridge.Pred -exp(y_test))^2))
data.frame(R2 = R2(Ridge.Pred,y_test),RSME = RSME(Ridge.Pred,y_test),MAE = MAE(Ridge.Pred,y_test))


#### LASSO REGRESSION 
## Compare to ridge regression, lasso and ridge regression both try to minimize 
## the RSS and have a penalty term to prevent overfitting. But Lasso regression 
## has a advantage that lasso will make some predictors' coefficient to be 0, which
## could use for model selection.

##### Prepare Model


### set initial alpha and lambda 
Lasso.Alpha=1

set.seed(123)
Lasso.Fit = glmnet(X_train, y_train, alpha=Lasso.Alpha, lambda=grid)
Lasso.Fitcv = cv.glmnet(X_train, y_train, alpha = Lasso.Alpha,nfolds = 10,type.measure = 'deviance')


### choose the optimal lambda
best.lambda = Lasso.Fitcv$lambda.min
best.lambda
## with crossvaldiation, we get the optimal lambda is equal to 0.0008
plot(Lasso.Fitcv)

##### Check The Accuarcy
Lasso.Pred =  predict(Lasso.Fit, s= best.lambda, newx = X_test)
Lasso.Pred = exp(Lasso.Pred)
accuary1 = mean(abs(exp(y_test) - Lasso.Pred)/exp(y_test) <=0.05);accuary1



### the coefficient choose from Lasso
##  show the predictors lasso choosed.
lasso.coef = predict(Lasso.Fit, s = best.lambda, type="coefficients")[1:53,]
sort(lasso.coef[lasso.coef!=0],decreasing = TRUE)

##### Error Matrix
# let look at the MSE get from Lasso
sqrt(mean((Lasso.Pred -exp(y_test))^2))


##### Cross Validation
data.frame(R2 = R2(Lasso.Pred,y_test),RSME = RSME(Lasso.Pred,y_test),MAE = MAE(Lasso.Pred,y_test))


