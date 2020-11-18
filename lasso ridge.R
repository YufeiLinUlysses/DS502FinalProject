library(caret)
library(glmnet)
library(dplyr)
library(ggplot2)
library(corrplot)
library(knitr)
library(skimr)
library(kableExtra)
library(purrr)
library(rmarkdown)
getwd()
setwd("C://Users//86084/Desktop/wpi/2020 fall/DS502/final project")
# lasso and ridge
data = read.csv('demo/train_p.csv')
data
set.seed(1)
bs = sample(dim(data)[1],dim(data)[1],replace = T)
bsdata = data[bs,]

set.seed(2)
trainsize = sample(dim(bsdata)[1],0.7*dim(bsdata)[1])
traindata = bsdata[trainsize,]
testdata = bsdata[-trainsize,]
traindata


#lasso 
Lasso.Alpha=1
Lasso.Lambda = 10^(seq(3,-1,length=100))
grid=10^seq(10,-2, length =100)
set.seed(111)
XTrain = model.matrix(SalePrice~.,data = traindata)[,-1]
XTest = model.matrix(SalePrice~.,testdata)[,-1]
YTrain = traindata$SalePrice
YTest = testdata$SalePrice

Lasso.Fit = glmnet(XTrain, YTrain, alpha=Lasso.Alpha, lambda=Lasso.Lambda)
Lasso.Fitcv = cv.glmnet(XTrain, YTrain, alpha = Lasso.Alpha, lambda = Lasso.Lambda)
set.seed(2222)
best.lambda = Lasso.Fitcv$lambda.min
best.lambda
plot(Lasso.Fitcv)
Lasso.Pred <- predict(Lasso.Fit, s= best.lambda, newx = XTest)
sqrt(mean((Lasso.Pred -YTest)^2))
accuary1 = mean(abs(YTest - Lasso.Pred)/YTest <=0.05);accuary1

lasso.coef = predict(Lasso.Fit, s = best.lambda, type="coefficients")
sum(lasso.coef!=0)

x1 = sample(length(YTest),length(YTest))
x1 = sort(x1)
dd = data.frame(x1,Lasso.Pred,YTest)
ggplot()+geom_line(data = dd,aes(x = x1,y = YTest,colour = "ytest"),size=1)+
  geom_line(data = dd,aes(x = x1,y = Lasso.Pred,colour = "prediction"),size=1)

# ridge 
set.seed(1234)
Ridge.Alpha=0
Ridge.Lambda = 10^(seq(4,-1,length=100))
Ridge.Fit = glmnet(XTrain, YTrain, alpha=Ridge.Alpha, lambda=Ridge.Lambda)
Ridge.Fitcv = cv.glmnet(XTrain, YTrain, alpha = Ridge.Alpha, lambda = Ridge.Lambda)
best.lambda1 = Ridge.Fitcv$lambda.min
best.lambda1
Ridge.Pred <- predict(Ridge.Fit, s= best.lambda1, newx = XTest)
sqrt(mean((Ridge.Pred -YTest)^2))
accuary = mean(abs(YTest - Ridge.Pred)/YTest <=0.05);accuary1
predict(Ridge.Fit, s = best.lambda1, type="coefficients")
x2 = sample(length(YTest),length(YTest))
x2 = sort(x2)
dd1 = data.frame(x2,Ridge.Pred,YTest)
ggplot()+geom_line(data = dd1,aes(x = x2,y = YTest,colour = "YTest"),size=1)+
  geom_line(data = dd1,aes(x = x2,y = Ridge.Pred,colour = "prediction"),size=1)

