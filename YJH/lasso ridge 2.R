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


# remove the irrational data point
ggplot(HousePricing,aes(x=GrLivArea,y=SalePrice))+geom_point()
HousePricing = HousePricing[HousePricing$GrLivArea<4500,]

# find which variables contain NA
NAcol <- which(colSums(is.na(HousePricing)) > 0)
sort(colSums(sapply(HousePricing[NAcol], is.na)), decreasing = TRUE)

# remove NA 
HousePricing$PoolQC[is.na(HousePricing$PoolQC)] = 'None'
quailty = c('None'=0,'Fa' = 1,'TA' = 2,'Gd' = 3,'Ex' = 4)
HousePricing$PoolQC<-recode(HousePricing$PoolQC,'None'=0,'Fa' = 1,'TA' = 2,'Gd' = 3,'Ex' = 4)

HousePricing$MiscFeature[is.na(HousePricing$MiscFeature)] = 'None'
HousePricing$MiscFeature = as.factor(HousePricing$MiscFeature)

HousePricing$Alley[is.na(HousePricing$Alley)] = 'None'
HousePricing$Alley=recode(HousePricing$Alley,'None' = 0,'Pave' = 1,'Grvl' = 2)

HousePricing$Fence[is.na(HousePricing$Fence)] = 'None'
HousePricing$Fence=recode(HousePricing$Fence,'None' = 0,'MnWw' = 1,'GdWo' = 2,'MnPrv' = 3,'GdPrv' = 4)

HousePricing$FireplaceQu[is.na(HousePricing$FireplaceQu)] = 'None'
HousePricing$FireplaceQu=recode(HousePricing$FireplaceQu,'None' = 0,'Po' = 1,'Fa' = 2,'TA' = 3,'Gd' = 4,'Ex' = 5)


HousePricing$LotFrontage[is.na(HousePricing$LotFrontage)]
for (i in 1:nrow(HousePricing)){
  if (is.na(HousePricing$LotFrontage[i])){
    HousePricing$LotFrontage[i] = as.integer(median(HousePricing$LotFrontage[HousePricing$Neighborhood==HousePricing$Neighborhood[i]],na.rm = TRUE))
  }
}


HousePricing$GarageType[is.na(HousePricing$GarageType)] = 'None'
HousePricing$GarageYrBlt[is.na(HousePricing$GarageYrBlt)] <- HousePricing$YearBuilt[is.na(HousePricing$GarageYrBlt)]
HousePricing$GarageFinish[is.na(HousePricing$GarageFinish)] = 'None'
HousePricing$GarageQual[is.na(HousePricing$GarageQual)] = 'None'
HousePricing$GarageCond[is.na(HousePricing$GarageCond)] = 'None'

length(which(is.na(HousePricing$BsmtQual) & is.na(HousePricing$BsmtCond) & is.na(HousePricing$BsmtExposure) & is.na(HousePricing$BsmtFinType1) & is.na(HousePricing$BsmtFinType2)))
HousePricing[!is.na(HousePricing$BsmtFinType1) & (is.na(HousePricing$BsmtCond)|is.na(HousePricing$BsmtQual)|is.na(HousePricing$BsmtExposure)|is.na(HousePricing$BsmtFinType2)), c('BsmtQual', 'BsmtCond', 'BsmtExposure', 'BsmtFinType1', 'BsmtFinType2')]

HousePricing$BsmtFinType2[333] = names(sort(table(HousePricing$BsmtFinType2),decreasing = TRUE))[1]
HousePricing$BsmtExposure[949] = names(sort(table(HousePricing$BsmtExposure),decreasing = TRUE))[1]

HousePricing$BsmtQual[is.na(HousePricing$BsmtQual)] = 'None'
HousePricing$BsmtQual=recode(HousePricing$BsmtQual,'None' = 0,'Po' = 1,'Fa' = 2,'TA' = 3,'Gd' = 4,'Ex' = 5)

HousePricing$BsmtCond[is.na(HousePricing$BsmtCond)] = 'None'
HousePricing$BsmtCond=recode(HousePricing$BsmtCond,'None' = 0,'Po' = 1,'Fa' = 2,'TA' = 3,'Gd' = 4,'Ex' = 5)

HousePricing$BsmtExposure[is.na(HousePricing$BsmtExposure)]='None'
HousePricing$BsmtExposure=recode(HousePricing$BsmtExposure,'None' = 0,'No' = 1,'Mn' = 2,'Av' = 3,'Gd' = 4)

HousePricing$BsmtFinType1[is.na(HousePricing$BsmtFinType1)] = 'None'
HousePricing$BsmtFinType1=recode(HousePricing$BsmtFinType1,'None' = 0,'Unf' = 1,'LwQ' = 2,'Rec' = 3,'BLQ' = 4,'ALQ' = 5,'GLQ' = 6)

HousePricing$BsmtFinType2[is.na(HousePricing$BsmtFinType2)] = 'None'
HousePricing$BsmtFinType2=recode(HousePricing$BsmtFinType2,'None' = 0,'Unf' = 1,'LwQ' = 2,'Rec' = 3,'BLQ' = 4,'ALQ' = 5,'GLQ' = 6)


HousePricing$MasVnrType[is.na(HousePricing$MasVnrType)] = 'None'
median(HousePricing$SalePrice[HousePricing$MasVnrType=='BrkCmn'])
median(HousePricing$SalePrice[HousePricing$MasVnrType=='BrkFace'])
median(HousePricing$SalePrice[HousePricing$MasVnrType=='None'])
median(HousePricing$SalePrice[HousePricing$MasVnrType=='Stone'])
HousePricing$MasVnrType=recode(HousePricing$MasVnrType,'BrkCmn' = 0,'None' = 0,'BrkFace' = 1,'Stone' = 2)

HousePricing$MasVnrArea[(is.na(HousePricing$MasVnrArea))] = 0
HousePricing$Electrical[is.na(HousePricing$Electrical)] <- names(sort(-table(HousePricing$Electrical)))[1]
HousePricing$Electrical=as.factor(HousePricing$Electrical)

HousePricing$MSZoning = as.factor(HousePricing$MSZoning)
HousePricing$Street=recode(HousePricing$Street,'Pave' = 0,'Grvl' = 1)
HousePricing$LotShape=recode(HousePricing$LotShape,'IR3' = 0,'IR2' = 1,'IR1' = 2,'Reg' =2)
HousePricing$Utilities=recode(HousePricing$Utilities,'ELO' = 0,'NoSeWa' = 1,'NoSewr' = 2,'AllPub' =2)
HousePricing$LotConfig = as.factor(HousePricing$LotConfig)
HousePricing$Condition1 = as.factor(HousePricing$Condition1)
HousePricing$Condition2 = as.factor(HousePricing$Condition2)
HousePricing$LandContour = as.factor(HousePricing$LandContour)
HousePricing$RoofStyle = as.factor(HousePricing$RoofStyle)
HousePricing$LandSlope=recode(HousePricing$LandSlope,'Sev' = 0,'Mod' = 1,'Gtl' = 2)
HousePricing$BldgType = as.factor(HousePricing$BldgType)
HousePricing$HouseStyle=as.factor(HousePricing$HouseStyle)
HousePricing$RoofMatl=as.factor(HousePricing$RoofMatl)
HousePricing$Exterior1st=as.factor(HousePricing$Exterior1st)
HousePricing$Exterior2nd=as.factor(HousePricing$Exterior2nd)
HousePricing$ExterQual=recode(HousePricing$ExterQual,'Po' = 0,'Fa' = 1,'TA' = 2,'Gd' = 3,'Ex' = 4)
HousePricing$ExterCond=recode(HousePricing$ExterCond,'Po' = 0,'Fa' = 1,'TA' = 2,'Gd' = 3,'Ex' = 4)
HousePricing$Foundation = as.factor(HousePricing$Foundation)
HousePricing$Heating = as.factor(HousePricing$Heating)
HousePricing$HeatingQC=recode(HousePricing$HeatingQC,'Po' = 0,'Fa' = 1,'TA' = 2,'Gd' = 3,'Ex' = 4)
HousePricing$CentralAir=recode(HousePricing$CentralAir,'N' = 0,'Y' = 1)
HousePricing$KitchenQual=recode(HousePricing$KitchenQua,'Po' = 0,'Fa' = 1,'TA' = 2,'Gd' = 3,'Ex' = 4)
HousePricing$Functional=recode(HousePricing$Functional,'Sal' = 0,'Sev' = 1,'Maj2' = 2,'Maj1' = 3,'Mod' = 4,'Min2' = 5,'Min1' = 6,'Typ' = 7)
HousePricing$GarageType=as.factor(HousePricing$GarageType)

HousePricing$GarageFinish=recode(HousePricing$GarageFinish,'None' = 0,'Unf' = 1,'RFn' = 2,'Fin' = 3)
HousePricing$GarageCond=recode(HousePricing$GarageCond,'None' = 0,'Po' = 1,'Fa' = 2,'TA' = 3,'Gd' = 4,'Ex' = 5)
HousePricing$PavedDrive=recode(HousePricing$PavedDrive,'N' = 0,'P' = 1,'Y' = 2)
HousePricing$SaleType = as.factor(HousePricing$SaleType)
HousePricing$SaleCondition = as.factor(HousePricing$SaleCondition)
HousePricing$MoSold = as.factor(HousePricing$MoSold)
HousePricing$MSSubClass = as.factor(HousePricing$MSSubClass)

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
Ridge.Alpha=0
Ridge.Fit = glmnet(X_train, y_train, alpha=Ridge.Alpha, lambda=grid)
Ridge.Fitcv = cv.glmnet(X_train, y_train, alpha = Ridge.Alpha,nfolds = 10,type.measure = 'deviance')

### choose the optimal lambda
best.lambda1 = Ridge.Fitcv$lambda.min
plot(Ridge.Fitcv)

##### Check Accuarcy
Ridge.Pred <- predict(Ridge.Fit, s= best.lambda1, newx = X_test)
Ridge.Pred = exp(Ridge.Pred)
accuary = mean(abs(exp(y_test) - (Ridge.Pred))/exp(y_test) <=0.05);accuary

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
grid=10^seq(10,-2, length =100)
set.seed(123)
Lasso.Fit = glmnet(X_train, y_train, alpha=Lasso.Alpha, lambda=grid)
Lasso.Fitcv = cv.glmnet(X_train, y_train, alpha = Lasso.Alpha,nfolds = 10,type.measure = 'deviance')


### choose the optimal lambda
best.lambda = Lasso.Fitcv$lambda.min
best.lambda
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


