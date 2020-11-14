############################################ Libraries ################################
install.packages("tidyverse")
install.packages("caret")
install.packages("mice") # missing value imputation
install.packages("ggplot2")
install.packages("ggthemes")
install.packages("caTools") #for train and test split
install.packages("glue")
install.packages("glmnet")
install.packages("MLmetrics")
install.packages("data.table")
install.packages("tictoc") # for timing executions 
install.packages("corrplot") # for correlation 
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
library(tictoc) # for timing executions 
library(corrplot) # for correlation 

###################################### Data Loading ###########################################
tic("CSV Reading time using read.csv ")
setwd("~/DS502FinalProject/SourceData")
training_data = read.csv("train.csv", stringsAsFactors = FALSE)
testing_data = read.csv("test.csv", stringsAsFactors = FALSE)
toc()

##tic("CSV Reading time using fread ")
##training_data = fread("../input/train.csv", stringsAsFactors = FALSE)
##testing_data = fread("../input/test.csv", stringsAsFactors = FALSE)
##toc()
# fread is significantly faster than read.csv for large volume of data.

glue("Dimension of Training dataset : {dim(training_data)}")
glue("Dimension of Testing dataset : {dim(testing_data)}")

glue("Columns in training dataset and their correspomnding data types : ")
cbind(colnames(training_data), class(colnames(training_data)))

glue("Columns in testing dataset and their correspomnding data types : ")
cbind(colnames(testing_data), class(colnames(testing_data)))

########################################### Combine the dataset ##############################

combined_data = bind_rows(training_data, testing_data)

colnames(combined_data)
glue("Dimension of Combined Dataset : {dim(combined_data)}")

glimpse(combined_data)
# check number of NAs in each column:
cbind(colSums(is.na(combined_data)))

# drop ID column from the dataset 
combined_data$Id = NULL
str(combined_data)
cbind(colSums(is.na(combined_data)))


# percentage of missing Values in dataset ####
cbind(colSums(is.na(combined_data)),colSums(is.na(combined_data))/(nrow(combined_data)) * 100)


# fixing Missing Values :
# MSZoning
# 4 missing values 
unique(combined_data$MSZoning)
table(combined_data$MSZoning)
# Most of the values are populated with RL
combined_data$MSZoning[is.na(combined_data$MSZoning)] = "RL"

# LotFrontage : 486 missing values
glue("Mean of LotFrontage : {mean(combined_data$LotFrontage, na.rm = T)} and Median of LotFrontage : {median(combined_data$LotFrontage, na.rm = T)}")

neighbor_Median  = combined_data %>%
  select(LotFrontage, Neighborhood) %>%
  group_by(Neighborhood) %>%
  summarise(LotFrontage = median(LotFrontage, na.rm = T))

# alternative : 
# neighbor_Median = aggregate(LotFrontage ~ Neighborhood, data = combined_data, median)
for (i in 1:nrow(combined_data))
{
  if(is.na(combined_data$LotFrontage[i])){
    temp = combined_data$Neighborhood[i]
    combined_data$LotFrontage[i] = neighbor_Median$LotFrontage[neighbor_Median$Neighborhood == temp]
  }
  
}

# Alley has 93.21% missing data
# and as per data description, NA =  no alley access
# Create a third alley type but before that convert it to character
combined_data$Alley = as.character(combined_data$Alley)
combined_data$Alley[is.na(combined_data$Alley)] = "None"
combined_data$Alley = as.factor(combined_data$Alley)

# utilites :  type of utilities available 
combined_data$Utilities = as.factor(combined_data$Utilities)
unique(combined_data$Utilities)
table(combined_data$Utilities)

# One NA  and All other has AllPubs 
combined_data$Utilities[is.na(combined_data$Utilities)] = "AllPub"
# But single value: no vaiation 
#  Discard it 
combined_data$Utilities = NULL

unique(combined_data$Exterior1st)
table(combined_data$Exterior1st)

combined_data$SalePrice[is.na(combined_data$Exterior1st)]

which.max(table(combined_data$Exterior1st))

# its in testing data
combined_data$Exterior1st[is.na(combined_data$Exterior1st)] = "VinylSd"
unique(combined_data$Exterior2nd)
table(combined_data$Exterior2nd)
which.max(table(combined_data$Exterior2nd))

combined_data$Exterior2nd[is.na(combined_data$Exterior2nd)] = "VinylSd"

# MasVbeType
unique(combined_data$MasVnrType)
table(combined_data$MasVnrType)
combined_data$MasVnrType[is.na(combined_data$MasVnrType)] = "None"
# Mass Veneer Area : 23 NAs
unique(combined_data$MasVnrArea)
combined_data$MasVnrArea[is.na(combined_data$MasVnrArea)] = median(combined_data$MasVnrArea, na.rm = T)

# Basement Details ####
# Basement Criteria : None for NA
unique(combined_data$BsmtQual)
combined_data$BsmtQual = as.character(combined_data$BsmtQual)
combined_data$BsmtQual[is.na(combined_data$BsmtQual)] = "None"

unique(combined_data$BsmtCond)
combined_data$BsmtCond = as.character(combined_data$BsmtCond)
combined_data$BsmtCond[is.na(combined_data$BsmtCond)] = "None"

combined_data$BsmtExposure = as.character(combined_data$BsmtExposure)
combined_data$BsmtExposure[is.na(combined_data$BsmtExposure)] = "None"
combined_data$BsmtFinType1 = as.character(combined_data$BsmtFinType1)
combined_data$BsmtFinType1[is.na(combined_data$BsmtFinType1)] = "None"
combined_data$BsmtFinType2 = as.character(combined_data$BsmtFinType2)
combined_data$BsmtFinType2[is.na(combined_data$BsmtFinType2)] = "None"


combined_data$BsmtFinSF1[is.na(combined_data$BsmtFinSF1)] = median(combined_data$BsmtFinSF1, na.rm = T)

cbind(table(combined_data$BsmtFinSF2)) 
# most 0s
combined_data$BsmtFinSF2[is.na(combined_data$BsmtFinSF2)] = 0

unique(combined_data$BsmtUnfSF)
combined_data$BsmtUnfSF[is.na(combined_data$BsmtUnfSF)] = median(combined_data$BsmtUnfSF, na.rm = T)

unique(combined_data$TotalBsmtSF)
combined_data$TotalBsmtSF[is.na(combined_data$TotalBsmtSF)] = median(combined_data$TotalBsmtSF, na.rm = T)

unique(combined_data$BsmtFullBath)
table(combined_data$BsmtFullBath)
combined_data$BsmtFullBath[is.na(combined_data$BsmtFullBath)] = 0

unique(combined_data$BsmtHalfBath)
table(combined_data$BsmtHalfBath)
combined_data$BsmtHalfBath[is.na(combined_data$BsmtHalfBath)] = 0

# Kitchen ####
unique(combined_data$KitchenQual)
table(combined_data$KitchenQual)
combined_data$KitchenQual[is.na(combined_data$KitchenQual)] = "TA"

## Electrical ####
unique(combined_data$Electrical)
table(combined_data$Electrical)
combined_data$Electrical[is.na(combined_data$Electrical)] = "SBrkr"

# functional
unique(combined_data$Functional)
table(combined_data$Functional)
combined_data$Functional[is.na(combined_data$Functional)] = "Typ"

unique(combined_data$FireplaceQu)
table(combined_data$FireplaceQu)
combined_data$FireplaceQu[is.na(combined_data$FireplaceQu)] = "None"

# Garage ####
unique(combined_data$GarageType)
table(combined_data$GarageType)
combined_data$GarageType[is.na(combined_data$GarageType)] = "None"

combined_data$GarageYrBlt[is.na(combined_data$GarageYrBlt)] = 0

table(combined_data$GarageFinish)
combined_data$GarageFinish[is.na(combined_data$GarageFinish)] = "None"

combined_data$GarageArea[is.na(combined_data$GarageArea)] = 0 
combined_data$GarageCars[is.na(combined_data$GarageCars)] = 0
combined_data$GarageQual[is.na(combined_data$GarageQual)] = "None"
combined_data$GarageCond[is.na(combined_data$GarageCond)] = "None"

# Misclenneous #####
combined_data$PoolQC[is.na(combined_data$PoolQC)] = "None"
combined_data$Fence[is.na(combined_data$Fence)] = "None"
combined_data$MiscFeature[is.na(combined_data$MiscFeature)] = "None"
combined_data$MiscVal


# sales type ####
unique(combined_data$SaleType)
table(combined_data$SaleType)
combined_data$SaleType[is.na(combined_data$SaleType)] = 'WD'


# Changing characters to Factors variables
# apply(combined_data,1, function(x){ if(class(x) == "character"){x = as.factor(x)}})
combined_data = as.data.frame(unclass(combined_data))

# some numeric data to factor
combined_data$MSSubClass = as.factor(combined_data$MSSubClass)
ggplot(combined_data, aes(x = MSSubClass, y = SalePrice, fill = MSSubClass)) + 
  geom_boxplot(color = "black") +
  labs(x = "MSSubClass", y = "Sale Price", title = "Sale Price Distibution on Sub Class")


combined_data$MoSold = as.factor(combined_data$MoSold)
ggplot(combined_data, aes(x = MoSold, y = SalePrice, fill = MoSold)) + 
  geom_boxplot(color = "black") +
  labs(x = "Month Sold", y = "Sale Price", title = "Sale Price Distibution on Month Sold")

combined_data$YrSold = as.factor(combined_data$YrSold)
ggplot(combined_data, aes(x = YrSold, y = SalePrice, fill = YrSold)) + 
  geom_boxplot(color = "black") +
  labs(x = "Year Sold", y = "Sale Price", title = "Sale Price Distibution on Year Sold")

combined_data$OverallCond = as.factor(combined_data$OverallCond)
ggplot(combined_data, aes(x = OverallCond, y = SalePrice, fill = OverallCond)) + 
  geom_boxplot(color = "black") +
  labs(x = "Overall Condition", y = "Sale Price", title = "Sale Price Distibution on Overall Condition")

combined_data$OverallQual = as.factor(combined_data$OverallQual)
ggplot(combined_data, aes(x = OverallQual, y = SalePrice, fill = OverallQual)) + 
  geom_boxplot(color = "black") +
  labs(x = "Overall Quality", y = "Sale Price", title = "Sale Price Distibution on Overall Quality")


# Feature Engineering ####
# check for normality of numeric features
cbind(colnames(combined_data), class(colnames(combined_data)))

# Variation in Sales price
ggplot(combined_data, aes(x = SalePrice, y = SalePrice)) + 
  geom_boxplot(fill = "orange", color = "steelblue") +
  labs(title = "Box Plot of  sale price")

# Density Ditribution
ggplot(combined_data, aes(x = SalePrice)) + 
  geom_density(fill = "cyan", alpha = 0.8)+
  labs(title = "Density Distribution of Sales Price", x =  "Sales Price")
# +ve skewness
qqnorm(combined_data$SalePrice, col = "steelblue")
qqline(combined_data$SalePrice, col = "red", lwd =2)

# log tranformation
combined_data$SalePrice = log(combined_data$SalePrice)
ggplot(combined_data, aes(x = SalePrice)) + 
  geom_density(fill = "cyan", alpha = 0.8)+
  labs(title = "Density Distribution of Log of  Sales Price", x =  "Sales Price")
# 
qqnorm(combined_data$SalePrice, col = "orange")
qqline(combined_data$SalePrice, col = "steelblue", lwd =2)

#Outliers
ggplot(combined_data, aes(x = SalePrice, y = SalePrice)) + 
  geom_boxplot(fill = "orange", color = "steelblue") +
  labs(title = "Box Plot of log of sale price")

glue("Mean of Sales Price : {mean(combined_data$SalePrice, na.rm = T)} and Median of Sales Price : {median(combined_data$SalePrice, na.rm = T)}.")
#glue("Median of Sales Price : {median(combined_data$SalePrice, na.rm = T)}.")

################################## Dummy variables ######################
dummy = dummyVars(formula = "~.", data = combined_data)

combined_data_1hot = predict(dummy, newdata = combined_data)
dim(combined_data_1hot)

####################### Separate Training_data and Testing_data back #############################

train_data = combined_data_1hot %>% head(nrow(training_data)) %>% as.data.frame()
test_data = combined_data_1hot %>% tail(nrow(testing_data)) %>% as.data.frame()
# remove SalePrice Column from test_data
test_data$SalePrice = NULL 
test_data = as.matrix(test_data)

################### Base Linear Regression Model Using lm() function ##############################

lm_model = lm(formula = SalePrice ~.,data = train_data)

summary(lm_model)
# Multiple R-squared: 0.9475, 
# Adjusted R-squared: 0.9345  
# F-statistic:  73.09  on 289 and 1170 DF, 
# p-value: < 2.2e-16

lm_pred = predict(lm_model, newdata = as.data.frame(test_data))
lm_pred  = exp(lm_pred)
result_lm_model = data.frame(Id = testing_data$Id, SalePrice = lm_pred)
write.csv(result_lm_model, "sub_lm_model.csv", row.names = F)

