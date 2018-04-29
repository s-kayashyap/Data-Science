# multiple Linear Regression modeling 
# Input Dataset : Housing.csv
# Name : Swami Prem Pranav Kayashyap (APFE1786831)
# ------------------------------------------------------
# Date : 21st Apr, 2018

#Import required library
library(ggplot2)

#Import Housing data from Housing.csv 
housing_data <- read.csv(file.choose(),stringsAsFactors = T)

#Check the structure of housing data 
str(housing_data)

# Few Important variables are
# price -> sale price of a house in Rs.
# area -> total size of a property in square feet
# bedrooms -> the number of bedrooms
# bathrooms ->  number of bathrooms etc

#For categorical variable, we create dummy variable for making it meaningful. Depends on the number
#of factor available for that variable (say, n) we have to create n-1 different dummy variable.
#Here I will Create dummy variable for mainroad, guestroom, basement, hotwaterheating,
#airconditioning, prefarea and furnishingstatus

#Create dummy variable

# One simple way to convert mainroad variable to numeric is to replace the levels-Yes's and Nos's
# with 1 and 0. Now store the numeric values in the same variable instead of assigning it to dummy.
summary(housing_data$mainroad)
levels(housing_data$mainroad)<-c(1,0)
housing_data$mainroad <- as.numeric(levels(housing_data$mainroad))[housing_data$mainroad]

#Similarly for other variable, dummy variable is:
levels(housing_data$guestroom)<-c(1,0)
housing_data$guestroom <- as.numeric(levels(housing_data$guestroom))[housing_data$guestroom]

levels(housing_data$basement)<-c(1,0)
housing_data$basement <- as.numeric(levels(housing_data$basement))[housing_data$basement]

levels(housing_data$hotwaterheating)<-c(1,0)
housing_data$hotwaterheating <- as.numeric(levels(housing_data$hotwaterheating))[housing_data$hotwaterheating]

levels(housing_data$airconditioning)<-c(1,0)
housing_data$airconditioning <- as.numeric(levels(housing_data$airconditioning))[housing_data$airconditioning]

levels(housing_data$prefarea)<-c(1,0)
housing_data$prefarea <- as.numeric(levels(housing_data$prefarea))[housing_data$prefarea]

# Now we come across variables having more than 2 levels. "model.matrix" function is very usefull here. 
summary(housing_data$furnishingstatus)

#Converting "furnishingstatus" into dummies . 
dummy <- data.frame(model.matrix(~furnishingstatus, data = housing_data))
housing_data <- cbind(subset(housing_data, select = -c(furnishingstatus)), dummy[,-1])

#Create new metric (eg:area per bedroom, bathrooms per bedroom  )

#metric - area per bedroom
housing_data$area_per_bedroom <- housing_data$area/housing_data$bedrooms

# metric - bathrooms per bedroom
housing_data$bb_ratio <- housing_data$bathrooms/housing_data$bedrooms

# Scatter Plot (Price vs area)
ggplot(housing_data, aes(area, price)) +
  geom_point(aes(colour = "red" )) +
  geom_smooth(method=lm) +
  scale_x_continuous(name = "Area") + 
  scale_y_continuous(name = "Prices")

#Similar to this you can make veriety of scatter plot to get some idea about those variable.

# Scatter Plot (Price vs bedrooms)
ggplot(housing_data, aes(bedrooms, price)) +
  geom_point(aes(colour = "red" )) +
  geom_smooth(method=lm) +
  scale_x_continuous(name = "Bedrooms") + 
  scale_y_continuous(name = "Prices")

#Training and test data building starts and model building also starts

#The set.seed( ) command is used to reproduce the same results each time while sampling 70%
#data for the training dataset. This is done so that you get the same training data as your sample
#each time you execute the R code
set.seed(100)  

# Use sample function for getting the random number represents 70% of indices. 
training_indexes = sample(1:nrow(housing_data), 0.7*nrow(housing_data))

#Create training data objects and test data objects
training_data <- housing_data[training_indexes,]
test_data <- housing_data[-training_indexes,]

#Create a multi linear regression model
MLR_model <- lm(formula = price ~ area + bedrooms + bathrooms + stories + mainroad + guestroom + 
                  basement + hotwaterheating + airconditioning + parking + prefarea + 
                  furnishingstatusunfurnished + furnishingstatussemi.furnished +
                  area_per_bedroom + bb_ratio, data = training_data)
summary(MLR_model)


#Now its time to eliminate few indepencdent variable which is insignificant. For that we need to
#check how these independent variable is correlated to each other or. 
corr_matrix <- cor(housing_data)

#Check what is the Variance Inflation factor (which means the level of dependancy of that variable
#with other independent variable or it is Multicollinear).It is also a result of regression among other independent variable.
#For calculation of VIF of every independent variable, you need to use vif function from package "car"
library("car")

#If VIF for the independent variable is less than 2. It means that this variable is having high level
#of independence. Lets calculate the (VIF.
vif(MLR_model)
summary(MLR_model) #Multiple R-squared:  0.6704,	Adjusted R-squared:  0.6568

# Now As we can see bbratio variable is having High VIF (20.8666) and it is insignificant(p = 0.92518) 
# which is p > 0.05. Make a new model without bbratio variable
MLR_model_2 <- lm(formula = price ~ area + bedrooms + bathrooms + stories + mainroad + guestroom + 
                basement + hotwaterheating + airconditioning + parking + prefarea + 
                furnishingstatusunfurnished + furnishingstatussemi.furnished + area_per_bedroom, data = training_data)

vif(MLR_model_2)
summary(MLR_model_2) #Multiple R-squared:  0.6704,	Adjusted R-squared:  0.6578

#Remove area_per_bedroom VLF = 15.108451 and p = 0.359754 and make new model without area_per_bedroom
MLR_model_3 <- lm(formula = price ~ area + bedrooms + bathrooms + stories + mainroad + guestroom + 
                    basement + hotwaterheating + airconditioning + parking + prefarea + 
                    furnishingstatusunfurnished + furnishingstatussemi.furnished, data = training_data)

vif(MLR_model_3)
summary(MLR_model_3) #Multiple R-squared:  0.6696,	Adjusted R-squared:  0.6579

#Now all the variable have VIF < 2 i.e thus Multicollinearity is not a problem anymore.

#Remove furnishingstatussemi.furnished based on insignificance, as p = 0.757004 and make new model.
MLR_model_3 <- lm(formula = price ~ area + bedrooms + bathrooms + stories + mainroad + guestroom + 
                    basement + hotwaterheating + airconditioning + parking + prefarea + 
                    furnishingstatusunfurnished, data = training_data)

vif(MLR_model_3)
summary(MLR_model_3) #Multiple R-squared:  0.6695,	Adjusted R-squared:  0.6588

#Remove guestroom based on insignificance, as p = 0.166532 and make new model.
MLR_model_5 <- lm(formula = price ~ area + bathrooms + stories + mainroad + 
                    basement + hotwaterheating + airconditioning + parking + prefarea + 
                    furnishingstatusunfurnished, data = training_data)

vif(MLR_model_5)
summary(MLR_model_5) #Multiple R-squared:  0.6675,	Adjusted R-squared:  0.6586 

#Remove guestroom based on insignificance, as p = 0.166532 and make new model.
MLR_model_5 <- lm(formula = price ~ area + bathrooms + stories + mainroad + 
                    basement + hotwaterheating + airconditioning + parking + prefarea + 
                    furnishingstatusunfurnished, data = training_data)

vif(MLR_model_5)
summary(MLR_model_5) #Multiple R-squared:  0.6675,	Adjusted R-squared:  0.6586 

#Remove hotwaterheating based on insignificance, as p = 0.005556 and make new model.
MLR_model_6 <- lm(formula = price ~ area + bathrooms + stories + mainroad + 
                    basement + airconditioning + parking + prefarea + 
                    furnishingstatusunfurnished, data = training_data)

vif(MLR_model_6)
summary(MLR_model_6) #Multiple R-squared:  0.6606,	Adjusted R-squared:  0.6523 

#Remove mainroad based on insignificance, as p = 0.002699 and make new model.
MLR_model_7 <- lm(formula = price ~ area + bathrooms + stories + 
                    basement + airconditioning + parking + prefarea + 
                    furnishingstatusunfurnished, data = training_data)

vif(MLR_model_7)
summary(MLR_model_7) #Multiple R-squared:  0.6522,	Adjusted R-squared:  0.6447 

#Remove basement based on insignificance, as p = 0.00146 and make new model.
MLR_model_final <- lm(formula = price ~ area + bathrooms + stories + airconditioning + parking + prefarea + 
                  furnishingstatusunfurnished, data = training_data)

vif(MLR_model_final)
summary(MLR_model_final) #Multiple R-squared:  0.6426,	Adjusted R-squared:  0.6359 

#Predict the house price by giving test data into MLR_model_final and add this result with test_data
# to evaluate our model's efficiency
test_data$predicted_price <- predict(MLR_model_final, test_data[,-1])

#One fact is that , R_Square value of the model is equivalent to square of correlation between
#actual output and predicted output from model. Just verify.
R <- cor(test_data$price,test_data$predicted_price)
R_squared <- R^2 # R_squared = 0.6714
