library(tidyverse)
library(caret)
library(mice) # missing value imputation
library(ggplot2)
library(ggthemes)
library(caTools) #for train and test split
library(glue)
library(glmnet)
library(MLmetrics)
library(data.table)
library(randomForest)
library(tictoc) # for timing executions 
library(corrplot)

set.seed(123)
HousePricing = read.csv('dataset//train.csv')

############################# feature engineering ##############################


HousePricing$Id = NULL
ggplot(HousePricing,aes(x=GrLivArea,y=SalePrice))+geom_point()
HousePricing = HousePricing[HousePricing$GrLivArea<4500,]

cbind(colnames(HousePricing), class(colnames(HousePricing)))
# find the columns contains NA and the number of NA values in each columns
NAcol <- which(colSums(is.na(HousePricing)) > 0)
sort(colSums(sapply(HousePricing[NAcol], is.na)), decreasing = TRUE)

# LotFrontage
# compute the median of neighbor, na.rm means compute medians without NA
neighbor_Median  = HousePricing %>%
  select(LotFrontage, Neighborhood) %>%
  group_by(Neighborhood) %>%
  summarise(LotFrontage = median(LotFrontage, na.rm = T))

# replace the LotFrontage NA with its neighbor's Lotfrontage's median.
for (i in 1:nrow(HousePricing))
{
  if(is.na(HousePricing$LotFrontage[i])){
    temp = HousePricing$Neighborhood[i]
    HousePricing$LotFrontage[i] = neighbor_Median$LotFrontage[neighbor_Median$Neighborhood == temp]
  }
}

# Alley, NA means no alley.
HousePricing$Alley[is.na(HousePricing$Alley)] = 'None'
HousePricing$Alley = as.factor(HousePricing$Alley)

HousePricing$Utilities = NULL
table(HousePricing$PoolQC)
HousePricing$PoolQC[is.na(HousePricing$PoolQC)] = "None"
HousePricing$PoolQC=recode(HousePricing$PoolQC,'None' = 0,'Fa' = 1,'TA' = 2,'Gd' = 3,'Ex' = 4)
# Fence
HousePricing$Fence[is.na(HousePricing$Fence)] = "None"
HousePricing$Fence = as.factor(HousePricing$Fence)
HousePricing$MiscFeature[is.na(HousePricing$MiscFeature)] = "None"
HousePricing$MiscFeature = as.factor(HousePricing$MiscFeature)

# garage
HousePricing$GarageYrBlt[is.na(HousePricing$GarageYrBlt)] <- HousePricing$YearBuilt[is.na(HousePricing$GarageYrBlt)]
HousePricing$GarageType[is.na(HousePricing$GarageType)] = "None"
HousePricing$GarageType = as.factor(HousePricing$GarageType)
HousePricing$GarageFinish[is.na(HousePricing$GarageFinish)] = "None"
HousePricing$GarageFinish=recode(HousePricing$GarageFinish,'None' = 0,'Unf' = 1,'RFn' = 2,'Fin' = 3)
HousePricing$GarageQual[is.na(HousePricing$GarageQual)] = "None"
HousePricing$GarageQual=recode(HousePricing$GarageQual,'None' = 0,'Po' = 1,'Fa' = 2,'TA' = 3,'Gd' = 4,'Ex' = 5)
HousePricing$GarageCond[is.na(HousePricing$GarageCond)] = "None"
HousePricing$GarageCond=recode(HousePricing$GarageCond,'None' = 0,'Po' = 1,'Fa' = 2,'TA' = 3,'Gd' = 4,'Ex' = 5)
HousePricing$FireplaceQu[is.na(HousePricing$FireplaceQu)] = "None"
HousePricing$FireplaceQu=recode(HousePricing$FireplaceQu,'None' = 0,'Po' = 1,'Fa' = 2,'TA' = 3,'Gd' = 4,'Ex' = 5)

#electric
HousePricing$Electrical[is.na(HousePricing$Electrical)] = "SBrkr"
HousePricing$Electrical = as.factor(HousePricing$Electrical)

# basement 
length(which(is.na(HousePricing$BsmtQual) & is.na(HousePricing$BsmtCond) & is.na(HousePricing$BsmtExposure) & is.na(HousePricing$BsmtFinType1) & is.na(HousePricing$BsmtFinType2)))
HousePricing[!is.na(HousePricing$BsmtCond) & (is.na(HousePricing$BsmtFinType1)|is.na(HousePricing$BsmtQual)|is.na(HousePricing$BsmtExposure)|is.na(HousePricing$BsmtFinType2)), c('BsmtQual', 'BsmtCond', 'BsmtExposure', 'BsmtFinType1', 'BsmtFinType2')]
HousePricing$BsmtFinType2[333] = names(sort(table(HousePricing$BsmtFinType2),decreasing = TRUE))[1]
HousePricing$BsmtExposure[949] = names(sort(table(HousePricing$BsmtExposure),decreasing = TRUE))[1]
HousePricing$BsmtExposure[is.na(HousePricing$BsmtExposure)] = 'None'
HousePricing$BsmtExposure=recode(HousePricing$BsmtExposure,'None' = 0,'No' = 1,'Mn' = 2,'Av' = 3,'Gd' = 4)
HousePricing$BsmtQual[is.na(HousePricing$BsmtQual)] = 'None'
HousePricing$BsmtQual=recode(HousePricing$BsmtQual,'None' = 0,'Po' = 1,'Fa' = 2,'TA' = 3,'Gd' = 4,'Ex' = 5)
HousePricing$BsmtCond[is.na(HousePricing$BsmtCond)] = 'None'
HousePricing$BsmtCond=recode(HousePricing$BsmtCond,'None' = 0,'Po' = 1,'Fa' = 2,'TA' = 3,'Gd' = 4,'Ex' = 5)
HousePricing$BsmtFinType1[is.na(HousePricing$BsmtFinType1)] = 'None'
HousePricing$BsmtFinType1=recode(HousePricing$BsmtFinType1,'None' = 0,'Unf' = 1,'LwQ' = 2,'Rec' = 3,'BLQ' = 4,'ALQ' = 5, 'GLQ' = 6)
HousePricing$BsmtFinType2[is.na(HousePricing$BsmtFinType2)] = 'None'
HousePricing$BsmtFinType2=recode(HousePricing$BsmtFinType2,'None' = 0,'Unf' = 1,'LwQ' = 2,'Rec' = 3,'BLQ' = 4,'ALQ' = 5, 'GLQ' = 6)

# Mas
HousePricing$MasVnrType[is.na(HousePricing$MasVnrType)] = 'None'
HousePricing$MasVnrType = as.factor(HousePricing$MasVnrType)
HousePricing$MasVnrArea[(is.na(HousePricing$MasVnrArea))] = 0

HousePricing$MSZoning = as.factor(HousePricing$MSZoning)
HousePricing$Street = as.factor(HousePricing$Street)
HousePricing$LotShape=recode(HousePricing$LotShape,'IR3' = 0,'IR2' = 1,'IR1' = 2,'Reg' =2)
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
HousePricing$PavedDrive=recode(HousePricing$PavedDrive,'N' = 0,'P' = 1,'Y' = 2)
HousePricing$Heating = as.factor(HousePricing$Heating)
HousePricing$HeatingQC=recode(HousePricing$HeatingQC,'Po' = 0,'Fa' = 1,'TA' = 2,'Gd' = 3,'Ex' = 4)
HousePricing$CentralAir=recode(HousePricing$CentralAir,'N' = 0,'Y' = 1)


HousePricing$KitchenQual=recode(HousePricing$KitchenQua,'Po' = 0,'Fa' = 1,'TA' = 2,'Gd' = 3,'Ex' = 4)
HousePricing$Functional=recode(HousePricing$Functional,'Sal' = 0,'Sev' = 1,'Maj2' = 2,'Maj1' = 3,'Mod' = 4,'Min2' = 5,'Min1' = 6,'Typ' = 7)
HousePricing$Neighborhood = as.factor(HousePricing$Neighborhood)
HousePricing$SaleType = as.factor(HousePricing$SaleType)
HousePricing$SaleCondition = as.factor(HousePricing$SaleCondition)
HousePricing$MoSold = NULL
HousePricing$MSSubClass = as.factor(HousePricing$MSSubClass)


#################### correlation between the numerical variables###############
which(sapply(HousePricing, is.numeric))
numericVars = which(sapply(HousePricing, is.numeric));numericVars
factorVars = which(sapply(HousePricing, is.factor))
all_numVar = HousePricing[,numericVars];all_numVar
cor_numVar =(cor(all_numVar))
cor_sorted = as.matrix(sort(cor_numVar[,"SalePrice"],decreasing = TRUE))
cor_sorted
CorHigh <- names(which(apply(cor_sorted, 1, function(x) abs(x)>0.5)))
cor_numVar <- cor_numVar[CorHigh, CorHigh]
corrplot.mixed(cor_numVar, tl.col="black", tl.pos = "lt", tl.cex = 0.7,cl.cex = .7, number.cex=.7)

########################### further feature engineer ###################
# whether remod 
HousePricing$Remod = ifelse(HousePricing$YearBuilt == HousePricing$YearRemodAdd,0,1)
# the age of house 
HousePricing$age = as.numeric(HousePricing$YrSold) - HousePricing$YearRemodAdd 
# whether is new
HousePricing$isnew = ifelse(HousePricing$YrSold == HousePricing$YearBuilt,1,0)
# total area.
HousePricing$Totalsf = HousePricing$GrLivArea+HousePricing$TotalBsmtSF
# sum up the bathroom.
HousePricing$TotBathrooms <- HousePricing$FullBath + (HousePricing$HalfBath*0.5) + HousePricing$BsmtFullBath + (HousePricing$BsmtHalfBath*0.5)
numericVars = which(sapply(HousePricing, is.numeric))
all_numVar = HousePricing[,numericVars];all_numVar


SalePrice = HousePricing[,"SalePrice"]
all_numVar = subset(all_numVar, select = -c(GrLivArea,BsmtHalfBath,BsmtFullBath,HalfBath,FullBath,SalePrice))
factor =select_if(HousePricing,is.factor)

########################## Normalizing the numerical data #########################
snumeric = scale(all_numVar,center = T,scale = T)

############ one-hot encoding and combine with scaled numerical data #######################################
factor = model.matrix(~.-1,factor) %>% as.data.frame()
newdata = cbind(snumeric,factor)
dim(newdata)
newdata$SalePrice = log(SalePrice)

#############################train test split ###########################
set.seed(1234)
sample1 = sample(dim(HousePricing)[1],dim(HousePricing)[1],replace = T)
HousePricing1 = HousePricing[sample1,]
sample = sample(dim(HousePricing)[1],0.7*dim(HousePricing)[1])
training_data = HousePricing1[sample,]
testing_data = HousePricing1[-sample,]


###########################bootstraping training data#####################
train_data = newdata %>% head(nrow(training_data)) %>% as.data.frame()
test_data = newdata %>% tail(nrow(testing_data)) %>% as.data.frame()
dim(train_data)
dim(test_data)

set.seed(2)
split = sample.split(train_data$SalePrice, SplitRatio = 0.8)
train_set = subset(train_data, split == T)
dim(train_set)
validation_set = subset(train_data, split == F)
dim(validation_set)

X_train = model.matrix(SalePrice~.,data = train_set)[,-1]
X_test = model.matrix(SalePrice~.,validation_set)[,-1]
y_train = train_set$SalePrice
y_test = validation_set$SalePrice

################################## Ridge regression #############################

grid=10^seq(10,-2, length =100)
Ridge.Alpha=0
Ridge.Fit = glmnet(X_train, y_train, alpha=Ridge.Alpha, lambda=grid)
##### use crossva lidation to choose optimal lambda 
Ridge.Fitcv = cv.glmnet(X_train, y_train, alpha = Ridge.Alpha,nfolds = 10,type.measure = 'deviance')
best.lambda1 = Ridge.Fitcv$lambda.min
best.lambda1
## with crossvaldiation, we get the optimal lambda is equal to 0.0341
plot(Ridge.Fitcv)
Ridge.Pred <- predict(Ridge.Fit, s= best.lambda1, newx = X_test)
Ridge.Pred = exp(Ridge.Pred)

#############  accuarcy
accuary = mean(abs(exp(y_test) - (Ridge.Pred))/exp(y_test) <=0.05)
accuary

############
sqrt(mean((Ridge.Pred -exp(y_test))^2))


############################## Lasso Regression ###############################
Lasso.Alpha=1
grid=10^seq(10,-2, length =100)
set.seed(123)
Lasso.Fit = glmnet(X_train, y_train, alpha=Lasso.Alpha, lambda=grid)
Lasso.Fitcv = cv.glmnet(X_train, y_train, alpha = Lasso.Alpha,nfolds = 10,type.measure = 'deviance')
best.lambda = Lasso.Fitcv$lambda.min
best.lambda
plot(Lasso.Fitcv)

######## the coefficient choosed by lasso
lasso.coef = predict(Lasso.Fit, s = best.lambda, type="coefficients")
length(lasso.coef)
lasso.coef = predict(Lasso.Fit, s = best.lambda, type="coefficients")[1:217,]
sort(lasso.coef[lasso.coef!=0],decreasing = TRUE)

Lasso.Pred =  predict(Lasso.Fit, s= best.lambda, newx = X_test)
Lasso.Pred = exp(Lasso.Pred)

######## accuaracy
accuary1 = mean(abs(exp(y_test) - Lasso.Pred)/exp(y_test) <=0.05)
accuary1

########
sqrt(mean((Lasso.Pred -exp(y_test))^2))

