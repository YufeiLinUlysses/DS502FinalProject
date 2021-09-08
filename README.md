# DS502FinalProject
## Introduction
Traditional house pricing prediction is by comparing the cost and sale price of that specific house. By doing this, it lacks a proper process and an accepted standard. Therefore, being able to predict the price of a house by applying a machine learning model will help improve the efficiency of the real estate market and tends to be an important skill for both sellers and consumers. For the seller, they could make better sales and consumers could have better understanding when they try to make a purchase. Therefore, in this project, we are planning to make predictions of house price based on the 79 different predictors provided by Kaggle dataset (“Advanced Regression Techniques for House Prices,” n.d.) to determine values of residential homes in Ames, Iowa. We are going to build several models based on this data set entirely in R and compare the results to find the model that performs the best in predicting sale price of houses.
## Approaches
### Model Selection
For our project, we are going to use regression analysis as our major method. Our goal is to predict the price of a certain house based on the location, type, space and other variables that can be considered in the real estate market that might affect the price of the house. There are multiple different methods to solve prediction problems. However, the reason why we chose regression as our method is because it is widely used in prediction and forecasting topics and it performs well with numeric data. Therefore, price prediction, as a numeric variable, would perform as a good fit with regression analysis.

In terms of models, we are building 5 linear regression models.(**Linear Regression Model**, **PCR**, **Ridge Regression**, **Lasso Regression** and **Random Forest**). We are going to compare each of these models’ performance and select the one that performs the best to be our best-performance model. After doing that, we will apply the model on our test data set to see its final performance.
### Y-value Transformation
We have noticed our target label SalePrice has a typical right-skewed distribution, which is against an assumption of linear regression – residuals should follow normal distribution with zero mean and equal variance (homoscedasticity) (James 2013). Since skewness is a serious issue and may be the reason for bad performance of a model, sometimes we can apply some transformations to reduce or remove Skewness. In general, square root transformation and log transformation. Log transformation is one of the most popular one and it does transform our data back to normal distribution. However, square root transformation did not do a good at transforming our data. Therefore we decided to use a new approach, which is using fourth root transformation in order to reduce the skewness. After applying log transformation and fourth-root transformation, the skewness of our data has become 0.07 and 0.49 repectively, which both fall into the category of being symmetric.
### Hierachical Cross Validation
We performed hierarchical cross validation on every model we build. Due to lacking data, we split 15% of our data as our testing data and this data is put in a vault. Then we took out 30% of the rest of the data as our testing data to test the performance of each model. Each model is built on a bootstrap sample of the remaining 70% of data with cross validation. After getting the result from the 30%-percent testing data set, we will choose the one that performs the best and apply that method on 15%-percent data set to see its actual performance.
### Error Metrics
We chose R square and RMSE as our error metrics to evaluate the performance of each model. Also, we created our own accuracy metric. In real world, customers tend to be satisfied when the house price is greater than its actual value since they can still bargain with the sellers; however they will be less satisfied when the predicted price is lower than the actual price. Therefore, we set our threshold to be between lower than -0.05% and greater than 20%, which means if the predicted price is within this range, we consider the predicted value as accurate.
### Additonal Data
In general, all columns provided are about the information and the facts of the house itself. However, considering other factors that might affect house prices (e.g. crime rate, neighborhood information), we added new columns from other data sets to add into this data set. We ended up adding the crime index, medium income, percentage of white collar and percentage of residents with college or higher degrees for each neighborhood.

## Models
- **Please find code in [.Rmd](https://github.com/YufeiLinUlysses/DS502FinalProject/blob/main/Final_try.Rmd) format file written in R for more details.**
- **For the entire report, feel free to navigate to [this page](https://github.com/YufeiLinUlysses/DS502FinalProject/blob/main/Final_Report.pdf) to view the final report.**

## Evaluation of different models
We then need to check accuracy, as assumed before, we would look at whether the difference between predicted SalePrice and true SalePrice is within the range we define as accurate prediction. And compare the three reuslts computed by different type of SalePrice. The following is the result. We will select the model with least R2 as a version of best model.

### R squared Error Metric
<img width="864" alt="r1" src="https://user-images.githubusercontent.com/45746834/132552755-799ced86-81c0-4afa-87fe-b1b7f2d823bb.png">

### RMSE Error Metric
<img width="854" alt="r2" src="https://user-images.githubusercontent.com/45746834/132552774-ab1a8fd5-5a31-4424-93a7-11917f5b0c16.png">

### Accuray Error Metric
<img width="851" alt="r3" src="https://user-images.githubusercontent.com/45746834/132552786-dfa9a5e4-94d6-469d-9861-2902c162e226.png">

## Conclusion
From our observation in the error metric tables, we realize that RMSE works best in describing which ones of our models works the best in predicting the y-values. It is because it helps to penalize larger errors, while R2 is not very decisive, and it could imply overfitting. Accuracy is an indicator that we created just to have an additional insight of how well the models perform. Thus, we take a closer look at RMSE for each model:
1. Lasso works the best for Original y-value
2. Ridge works the best for both Log and Fourth Root transformed y-value
We then, could say that Ridge is the one we would like to use for our final validation and testing of prediction of House Price at North Ames, Iowa. From the above three error metric table, we could see that accuracy for each model has actually improved a little for each converted y-value. However, it is worth notice that most of the models are having trouble improving their R-squared or RMSE with a processed y-value, therefore showing a risk of overfitting original data with a converted y-value.
At the end, we pulled our test data out from the vault and performed Ridge Regression over fourth-root transformation, the final result is 0.93 of R-square, 20449.24 of RMSE and approximately 60% of accuracy.
<img width="354" alt="r4" src="https://user-images.githubusercontent.com/45746834/132552800-787e81c8-851e-4a30-9e7b-34c982dffbd5.png">

## Contributers
Yufei Lin | [Github](https://github.com/YufeiLinUlysses) | ylin9@wpi.edu

Shijing Yang | [Github](https://github.com/irisyeung0725) | syang6@wpi.edu

Jingfeng Xia | [Github](https://github.com/Jeffxia601) | jxia@wpi.edu

Jinhong Yu | [Github](https://github.com/860844175) | jyu7@wpi.edu

Yanze Wang
