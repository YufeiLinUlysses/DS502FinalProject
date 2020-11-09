install.packages('caret', dependencies = c('Depends', 'Suggests'))
install.packages('caret')
library(caret)
library(glmnet)
library(dplyr)
library(ggplot2)
library(corrplot)
train = read.csv('dataset/train.csv')
test = read.csv('dataset/test.csv')
dim(train)
dim(test)

# get rid of ids
train = train[,-1]
train
hist(x = train$SalePrice)
summary(train$SalePrice)
train
ggplot(train,aes(x=GrLivArea,y=SalePrice))+geom_point()
train = train[train$GrLivArea<4500,]
# find which variables contain NA
NAcol <- which(colSums(is.na(train)) > 0)
sort(colSums(sapply(train[NAcol], is.na)), decreasing = TRUE)

#PoolQC
# replace NA with No
train$PoolQC[is.na(train$PoolQC)] = 'No'
# label encoding 
quailty = c('No'=0,'Fa' = 1,'TA' = 2,'Gd' = 3,'Ex' = 4)
train$PoolQC<-recode(train$PoolQC,'No'=0,'Fa' = 1,'TA' = 2,'Gd' = 3,'Ex' = 4)
ggplot(train,aes(x=PoolQC,y=SalePrice))+geom_point()
table(train$PoolQC)

# PoolArea
train[train$PoolArea>0 & train$PoolQC==0, c('PoolArea', 'PoolQC', 'OverallQual')]
table(train$MiscFeature)
train$MiscFeature[is.na(train$MiscFeature)] = 'None'
train$MiscFeature = as.factor(train$MiscFeature)
ggplot(train,aes(x=MiscFeature,y=SalePrice))+geom_point()

table(train$Alley)
train$Alley[is.na(train$Alley)] = 'None'
train$Alley=recode(train$Alley,'None' = 0,'Pave' = 1,'Grvl' = 2)
ggplot(train,aes(x=Alley,y=SalePrice))+geom_point()


table(train$Fence)
train$Fence[is.na(train$Fence)] = 'No'
train$Fence=recode(train$Fence,'No' = 0,'MnWw' = 1,'GdWo' = 2,'MnPrv' = 3,'GdPrv' = 4)
ggplot(train,aes(x=Fence,y=SalePrice))+geom_point()

table(train$FireplaceQu)
train$FireplaceQu[is.na(train$FireplaceQu)] = 'No'
train$FireplaceQu=recode(train$FireplaceQu,'No' = 0,'Po' = 1,'Fa' = 2,'TA' = 3,'Gd' = 4,'Ex' = 5)
ggplot(train,aes(x=FireplaceQu,y=SalePrice))+geom_point()

# LotFrontage
table(train$LotFrontage)
train$LotFrontage[is.na(train$LotFrontage)]


# replace the NA with median of its neighborhood
for (i in 1:nrow(train)){
  if (is.na(train$LotFrontage[i])){
    train$LotFrontage[i] = as.integer(median(train$LotFrontage[train$Neighborhood==train$Neighborhood[i]],na.rm = TRUE))
  }
}
train$LotFrontage[is.na(train$LotFrontage)]
train$LotFrontage=as.factor(train$LotFrontage)

# Garage
table(train$GarageYrBlt)
train$GarageYrBlt[is.na(train$GarageYrBlt)]
ggplot(train,aes(x=GarageYrBlt,y=SalePrice))+geom_point()
length(which(is.na(train$GarageType)))
train$GarageType[is.na(train$GarageType)] = 'No'
train$GarageYrBlt[is.na(train$GarageYrBlt)] <- train$YearBuilt[is.na(train$GarageYrBlt)]
train$GarageFinish[is.na(train$GarageFinish)] = 'No'
train$GarageQual[is.na(train$GarageQual)] = 'No'
train$GarageCond[is.na(train$GarageCond)] = 'No'


length(which(is.na(train$BsmtQual) & is.na(train$BsmtCond) & is.na(train$BsmtExposure) & is.na(train$BsmtFinType1) & is.na(train$BsmtFinType2)))
train[!is.na(train$BsmtFinType1) & (is.na(train$BsmtCond)|is.na(train$BsmtQual)|is.na(train$BsmtExposure)|is.na(train$BsmtFinType2)), c('BsmtQual', 'BsmtCond', 'BsmtExposure', 'BsmtFinType1', 'BsmtFinType2')]

train$BsmtFinType2[333] = names(sort(table(train$BsmtFinType2),decreasing = TRUE))[1]
train$BsmtExposure[949] = names(sort(table(train$BsmtExposure),decreasing = TRUE))[1]

table(train$BsmtQual)
train$BsmtQual[is.na(train$BsmtQual)] = 'No'
train$BsmtQual=recode(train$BsmtQual,'No' = 0,'Po' = 1,'Fa' = 2,'TA' = 3,'Gd' = 4,'Ex' = 5)

table(train$BsmtCond)
train$BsmtCond[is.na(train$BsmtCond)] = 'No'
train$BsmtCond=recode(train$BsmtCond,'No' = 0,'Po' = 1,'Fa' = 2,'TA' = 3,'Gd' = 4,'Ex' = 5)

train$BsmtExposure[is.na(train$BsmtExposure)]='None'
train$BsmtExposure=recode(train$BsmtExposure,'None' = 0,'No' = 1,'Mn' = 2,'Av' = 3,'Gd' = 4)

train$BsmtFinType1[is.na(train$BsmtFinType1)] = 'None'
train$BsmtFinType1=recode(train$BsmtFinType1,'None' = 0,'Unf' = 1,'LwQ' = 2,'Rec' = 3,'BLQ' = 4,'ALQ' = 5,'GLQ' = 6)

train$BsmtFinType2[is.na(train$BsmtFinType2)] = 'None'
train$BsmtFinType2=recode(train$BsmtFinType2,'None' = 0,'Unf' = 1,'LwQ' = 2,'Rec' = 3,'BLQ' = 4,'ALQ' = 5,'GLQ' = 6)

table(train$MasVnrType)
train$MasVnrType[is.na(train$MasVnrType)] = 'None'
median(train$SalePrice[train$MasVnrType=='BrkCmn'])
median(train$SalePrice[train$MasVnrType=='BrkFace'])
median(train$SalePrice[train$MasVnrType=='None'])
median(train$SalePrice[train$MasVnrType=='Stone'])
train$MasVnrType=recode(train$MasVnrType,'BrkCmn' = 0,'None' = 0,'BrkFace' = 1,'Stone' = 2)

table(train$MasVnrArea)

train$MasVnrArea[(is.na(train$MasVnrArea))] = 0

table(train$Electrical)
train$Electrical[is.na(train$Electrical)] <- names(sort(-table(train$Electrical)))[1]
train$Electrical=as.factor(train$Electrical)

train
table(train$MSZoning)
train$MSZoning = as.factor(train$MSZoning)

table(train$Street)
train$Street=recode(train$Street,'Pave' = 0,'Grvl' = 1)

table(train$LotShape)
train$LotShape=recode(train$LotShape,'IR3' = 0,'IR2' = 1,'IR1' = 2,'Reg' =2)

table(train$Utilities)
train$Utilities=recode(train$Utilities,'ELO' = 0,'NoSeWa' = 1,'NoSewr' = 2,'AllPub' =2)

table(train$LotConfig)
train$LotConfig = as.factor(train$LotConfig)

table(train$Condition1)
train$Condition1 = as.factor(train$Condition1)

table(train$Condition2)
train$Condition2 = as.factor(train$Condition2)

train$LandContour = as.factor(train$LandContour)

table(train$RoofStyle)
train$RoofStyle = as.factor(train$RoofStyle)

table(train$LandSlope)
train$LandSlope=recode(train$LandSlope,'Sev' = 0,'Mod' = 1,'Gtl' = 2)

table(train$BldgType)
train$BldgType = as.factor(train$BldgType)

table(train$HouseStyle)
train$HouseStyle=as.factor(train$HouseStyle)

table(train$RoofMatl)
train$RoofMatl=as.factor(train$RoofMatl)

table(train$Exterior1st)
train$Exterior1st=as.factor(train$Exterior1st)


table(train$Exterior2nd)
train$Exterior2nd=as.factor(train$Exterior2nd)

table(train$ExterQual)
train$ExterQual=recode(train$ExterQual,'Po' = 0,'Fa' = 1,'TA' = 2,'Gd' = 3,'Ex' = 4)

table(train$ExterCond)
train$ExterCond=recode(train$ExterCond,'Po' = 0,'Fa' = 1,'TA' = 2,'Gd' = 3,'Ex' = 4)

table(train$Foundation)
train$Foundation = as.factor(train$Foundation)

table(train$Heating)
train$Heating = as.factor(train$Heating)

table(train$HeatingQC)
train$HeatingQC=recode(train$HeatingQC,'Po' = 0,'Fa' = 1,'TA' = 2,'Gd' = 3,'Ex' = 4)

train
table(train$CentralAir)
train$CentralAir=recode(train$CentralAir,'N' = 0,'Y' = 1)

table(train$KitchenQual)
train$KitchenQual=recode(train$KitchenQua,'Po' = 0,'Fa' = 1,'TA' = 2,'Gd' = 3,'Ex' = 4)

table(train$Functional)
train$Functional=recode(train$Functional,'Sal' = 0,'Sev' = 1,'Maj2' = 2,'Maj1' = 3,'Mod' = 4,'Min2' = 5,'Min1' = 6,'Typ' = 7)

table(train$GarageType)
train$GarageType=as.factor(train$GarageType)

table(train$GarageFinish)
train$GarageFinish=recode(train$GarageFinish,'No' = 0,'Unf' = 1,'RFn' = 2,'Fin' = 3)

table(train$GarageQual)
train$GarageQual=recode(train$GarageQual,'No' = 0,'Po' = 1,'Fa' = 2,'TA' = 3,'Gd' = 4,'Ex' = 5)
train

table(train$GarageCond)
train$GarageCond=recode(train$GarageCond,'No' = 0,'Po' = 1,'Fa' = 2,'TA' = 3,'Gd' = 4,'Ex' = 5)

table(train$PavedDrive)
train$PavedDrive=recode(train$PavedDrive,'N' = 0,'P' = 1,'Y' = 2)

table(train$SaleType)
train$SaleType = as.factor(train$SaleType)

table(train$SaleCondition)
train$SaleCondition = as.factor(train$SaleCondition)

train$MoSold = as.factor(train$MoSold)

train$MSSubClass = as.factor(train$MSSubClass)

numericVars = which(sapply(train, is.numeric))
numericVars
factorVars = which(sapply(train, is.factor))
factorVars
all_numVar = train[,numericVars];all_numVar
cor_numVar =(cor(all_numVar))
cor_sorted = as.matrix(sort(cor_numVar[,"SalePrice"],decreasing = TRUE))
cor_sorted
CorHigh <- names(which(apply(cor_sorted, 1, function(x) abs(x)>0.5)))
cor_numVar <- cor_numVar[CorHigh, CorHigh]
corrplot.mixed(cor_numVar, tl.col="black", tl.pos = "lt", tl.cex = 0.7,cl.cex = .7, number.cex=.7)
numeric = select_if(train,is.numeric)
snumeric = scale(numeric,center = T,scale = T)
factor = select_if(train,is.factor)
factor
# one hot
factor = model.matrix(~.-1,factor) %>% as.data.frame()
factor
newdata = cbind(snumeric,factor)

set.seed(1)
# train is in safe box
sample = sample(dim(newdata)[1],dim(newdata)[1],replace = T)

traindata=newdata[sample,];traindata
trainsize = sample(dim(traindata)[1],0.8*dim(traindata)[1])
train1 = traindata[trainsize,]
test1 = traindata[-trainsize,]

#LASSO
LassoAlpha=1
LassoLambda = 10^(seq(3,-1,length=100))
set.seed(12)
XTrain = model.matrix(SalePrice~.,data = train1)[,-1]
XTest = model.matrix(SalePrice~.,test1)[,-1]
YTrain = train1$SalePrice
YTest = test1$SalePrice
set.seed(123)
LassoFit = glmnet(XTrain, YTrain, alpha=LassoAlpha, lambda=LassoLambda)
LassoFitcv = cv.glmnet(XTrain, YTrain, alpha = LassoAlpha, lambda = LassoLambda)
bestlambda = LassoFitcv$lambda.min;bestlambda
plot(LassoFitcv)
LassoPred <- predict(LassoFit, s= bestlambda, newx = XTest)
sqrt(mean((LassoPred -YTest)^2))
predict(LassoFit, s = bestlambda, type="coefficients")
