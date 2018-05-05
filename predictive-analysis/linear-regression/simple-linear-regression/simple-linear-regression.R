# Simple Linear Regression modeling 
# Input Dataset : tvmarketing.csv
# Objective : To get an idea of machine learning models. we will build a simple linear regression model in R.
# Name : Swami Prem Pranav Kayashyap (APFE1786831)
# ------------------------------------------------------
# Date : 21st Apr, 2018

library(ggplot2)
#Import advertising data from tvmarketing.csv 
advertising <- read.csv(file.choose(),stringsAsFactors = F)

str(advertising)

# Scatter Plot (TV vs Sales)
ggplot(advertising, aes(TV, Sales)) +
  geom_point(aes(colour = "red" )) +
  geom_smooth(method=lm) +
  scale_x_continuous(name = "Marketing budget(TV)") + 
  scale_y_continuous(name = "Sales")

#The set.seed( ) command is used to reproduce the same results each time while sampling 70%
#data for the training dataset. This is done so that you get the same training data as your sample
#each time you execute the R code
set.seed(100)  

# Use sample function for getting the random number represents 70% of indices. 
training_indexes = sample(1:nrow(advertising), 0.7*nrow(advertising))

#Create training data objects and test data objects
training_data <- advertising[training_indexes,]
test_data <- advertising[-training_indexes,]

#Create a simple linear regression model
SLR_model<-lm(Sales~TV, data = training_data)

## Check the summary of model. 
summary(SLR_model)

#Predict the sales price of test data using this SLR_model and add this result with test_data
# to evaluate our model's efficiency
test_data$predicted_sales <- predict(SLR_model, test_data)

#One fact is that , R_Square value of the model is equivalent to square of correlation between
#actual output and predicted output from model. Just verify.
R <- cor(test_data$Sales,test_data$predicted_sales)
R_squared <- R^2

#Lets compare the actual Sales and predicted sales
compare_result <- subset(test_data, select = c(Sales,predicted_sales))
View(compare_result)
