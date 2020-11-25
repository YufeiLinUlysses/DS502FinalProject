library('ggplot2')
library('ggthemes') 
library('scales')
library('dplyr') 
library('mice')
library('randomForest') 
library('data.table')
library('gridExtra')
library('corrplot') 
library('GGally')
library('e1071')


HousePricing = read.csv("./data/train.csv")
head(HousePricing)
dim(HousePricing)

sum(sapply(HousePricing[,1:81],typeof) == "character")
sum(sapply(HousePricing[,1:81],typeof) == "integer")

summary(train[,sapply(HousePricing[,1:81],typeof) == "integer"])

# The number of columns and rows
paste("Original training data set has",dim(HousePricing)[1], "rows and", dim(HousePricing)[2], "columns")

# The percentage of data missing in train
paste("The percentage of data missing in the original training data set is ", round(sum(is.na(HousePricing)) / (nrow(HousePricing) *ncol(HousePricing)),4)*100,"%",sep = "")

# The number of duplicated rows
paste("The number of duplicated rows are", nrow(HousePricing) - nrow(unique(HousePricing)))

# data visualization
names(HousePricing)
cat.var = names(HousePricing)[which(sapply(HousePricing, is.character))]
num.var = names(HousePricing)[which(sapply(HousePricing, is.numeric))]
train.num = HousePricing[num.var]
train.cat = HousePricing[cat.var]

## Bar plot/Density plot function

## Bar plot function

plotHist <- function(data_in, i) 
{
  data <- data.frame(x=data_in[[i]])
  p <- ggplot(data=data, aes(x=factor(x))) + stat_count() + xlab(colnames(data_in)[i]) + theme_light() + 
    theme(axis.text.x = element_text(angle = 90, hjust =1))
  return (p)
}

## Density plot function

plotDen <- function(data_in, i){
  data <- data.frame(x=data_in[[i]], SalePrice = data_in$SalePrice)
  p <- ggplot(data= data) + geom_line(aes(x = x), stat = 'density', size = 1,alpha = 1.0) +
    xlab(paste0((colnames(data_in)[i]), '\n', 'Skewness: ',round(skewness(data_in[[i]], na.rm = TRUE), 2))) + theme_light() 
  return(p)
  
}

## Function to call both Bar plot and Density plot function

doPlots <- function(data_in, fun, ii, ncol=3) 
{
  pp <- list()
  for (i in ii) {
    p <- fun(data_in=data_in, i=i)
    pp <- c(pp, list(p))
  }
  do.call("grid.arrange", c(pp, ncol=ncol))
}

# barplots for categorical features
png("./plot_cat.png")
doPlots(train.cat, fun = plotHist, ii = 1:4, ncol = 2)
dev.off()

png("./plot_cat2.png")
doPlots(train.cat, fun = plotHist, ii = 5:8, ncol = 2)
dev.off()

png("./plot_cat3.png")
doPlots(train.cat, fun = plotHist, ii = 9:12, ncol = 2)
dev.off()

png("./plot_cat4.png")
doPlots(train.cat, fun = plotHist, ii = 13:18, ncol = 2)
dev.off()

png("./plot_cat5.png")
doPlots(train.cat, fun = plotHist, ii = 19:22, ncol = 2)
dev.off()


# boxplots

ggplot(HousePricing, aes(x = Neighborhood, y = SalePrice)) +
  geom_boxplot() +
  geom_hline(aes(yintercept=80), 
             colour='red', linetype='dashed', lwd=2) +
  scale_y_continuous(labels=dollar_format()) +
  theme_few()

# conclusion: boxplot between the neighboorhoods and sale price shows that BrookSide and 
# South & West of Iowa State University have cheap houses. 
# While Northridge and Northridge Heights are rich neighborhoods with several outliers in terms of price.

# density plots for numeric variables
png("./plot_num.png")
doPlots(train.num, fun = plotDen, ii = 2:6, ncol = 2)
dev.off()

png("./plot_num2.png")
doPlots(train.num, fun = plotDen, ii = 7:12, ncol = 2)
dev.off()

png("./plot_num3.png")
doPlots(train.num, fun = plotDen, ii = 13:17, ncol = 2)
dev.off()

# Conclusion: Density plots of the features indicates that the features are skewed. 
# The denisty plot for YearBuilt shows that the data set contains a mix of new and old houses. 
# It shows a downturn in the number of houses in recent years, possibily due to the housing crisis.

png("./plot_num4.png")
doPlots(train.num, fun = plotHist, ii = 18:23, ncol = 2)
dev.off()

# Conclusion: The histograms below show that majority of the houses have 2 full baths, 0 half baths, 
# and have an average of 3 bedrooms.

# Explore correlation

correlation = cor(na.omit(train.num[,-1]))
row_indic = apply(correlation, 1, function(x) sum(x > 0.3 | x < -0.3) > 1)
correlation = correlation[row_indic,row_indic]
png("./corr.png")
corrplot(correlation,method = "shade")
dev.off()


# target varaible vs. predictors

summary(HousePricing$SalePrice)
quantile(HousePricing$SalePrice)
png("./salesPHist.png")
hist(HousePricing$SalePrice,col="blue",breaks = 25,main = "Distribution of SalePrice", xlab = "Sale Price")
dev.off()

# Conclustion: It deviates from normal distribution and it is right skewed

# Plotting 'GrLivArea' too see if there are any outliers
png("./scatterPlot.png")
qplot(GrLivArea, SalePrice, data= HousePricing,col=GrLivArea>4000,xlab = "GrLivArea", ylab="Sale Price",main = "Living Area vs. Sale Price")
dev.off()

summary(HousePricing$GrLivArea)
png("./GrLiveAreaHist.png")
hist(HousePricing$GrLivArea,breaks = 20,xlab="Living area",col = "dark red",main = "Frequency of Living area square feet")
dev.off()
