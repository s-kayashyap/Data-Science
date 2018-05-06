# Linear Regression Model for car pricing 
# Input Dataset : CarPrice_Assignment.csv
# Scope : To create a model which describe the relation between the price of cars
#         with various parameters. It will be used by the management to understand how
#         exactly the prices vary with the independent parameter.
# USe  :  This model will be used by Automobile consulting company executive. They can
#         accordingly manipulate the design of the cars, the business strategy etc. to
#         meet certain price levels. Further, the model will be a good way for the management
#         to understand the pricing dynamics of a new market. 
# Name : Swami Prem Pranav Kayashyap (APFE1786831)
# ------------------------------------------------------
# Date : 29th Apr, 2018

#Import library
library(tidyr) # To use "separate"
library(ggplot2) # To Use "ggplot"
library(car) #To use vif
library(MASS) # To use stepAIC

#Import CarPrice data from CarPrice_Assignment.csv 
car_price_data <- read.csv(file.choose(),stringsAsFactors = F)

#Data Cleaning and Preparation started..

# Extract the car's company name from CarName  column
car_price_data <-  separate(car_price_data, CarName, into = c("car_company"), sep = "[-]|[ ]", remove = "T", extra = "drop")

#Removing columns which is not required for this analysis
car_price_data <- subset(car_price_data, select = -c(car_ID))

#convering all the non numeric columns to upper case 
car_price_data <- data.frame(lapply(car_price_data, function(v) {
  if (is.character(v)) return(toupper(v))
  else return(v)
}))

#Check the structure of car data 
str(car_price_data)

#Create Dummy variable

#For categorical variable, we create dummy variable for making it meaningful. Depends on the number
#of factor available for that variable (say, n) we have to create n-1 different dummy variable.

#Dummy variable for car_company
dummy <- data.frame(model.matrix(~car_company, data = car_price_data))
car_price_data <- cbind(subset(car_price_data, select = -c(car_company)), dummy[,-1])

#Dummy variable for fueltype
levels(car_price_data$fueltype)<-c(1,0)
car_price_data$fueltype <- as.numeric(levels(car_price_data$fueltype))[car_price_data$fueltype]

#Dummy variable for aspiration
levels(car_price_data$aspiration)<-c(1,0)
car_price_data$aspiration <- as.numeric(levels(car_price_data$aspiration))[car_price_data$aspiration]

#Dummy variable for doornumber
levels(car_price_data$doornumber)<-c(1,0)
car_price_data$doornumber <- as.numeric(levels(car_price_data$doornumber))[car_price_data$doornumber]

#Dummy variable for carbody
dummy <- data.frame(model.matrix(~carbody, data = car_price_data))
car_price_data <- cbind(subset(car_price_data, select = -c(carbody)), dummy[,-1])

#Dummy variable for drivewheel
dummy <- data.frame(model.matrix(~drivewheel, data = car_price_data))
car_price_data <- cbind(subset(car_price_data, select = -c(drivewheel)), dummy[,-1])

#Dummy variable for enginelocation
levels(car_price_data$enginelocation)<-c(1,0)
car_price_data$enginelocation <- as.numeric(levels(car_price_data$enginelocation))[car_price_data$enginelocation]

#Dummy variable for enginetype
dummy <- data.frame(model.matrix(~enginetype, data = car_price_data))
car_price_data <- cbind(subset(car_price_data, select = -c(enginetype)), dummy[,-1])

#Dummy variable for cylindernumber
dummy <- data.frame(model.matrix(~cylindernumber, data = car_price_data))
car_price_data <- cbind(subset(car_price_data, select = -c(cylindernumber)), dummy[,-1])

#Dummy variable for fuelsystem
dummy <- data.frame(model.matrix(~fuelsystem, data = car_price_data))
car_price_data <- cbind(subset(car_price_data, select = -c(fuelsystem)), dummy[,-1])

#Remove temporary dummy data 
rm(dummy)

#Create new metric (eg:avg_mpg  )

#metric - avg_mpg
car_price_data$avg_mpg <- (car_price_data$citympg + car_price_data$highwaympg)/2

#Removing columns having only value as 0 
car_price_data <- car_price_data[,colSums(car_price_data == 0) < nrow(car_price_data)]

#Removing columns having only value as 1 
car_price_data <- car_price_data[,colSums(car_price_data == 1) < nrow(car_price_data)]

#Now check the structure
str(car_price_data)

#We can analyse few independent variable using scatter plot

# Scatter Plot (symboling vs price)
ggplot(car_price_data, aes(symboling, price)) +
  geom_point(aes(colour = "blue" )) +
  geom_smooth(method=lm) +
  scale_x_continuous(name = "Symboling") + 
  scale_y_continuous(name = "Price")

# Scatter Plot (wheelbase vs price)
ggplot(car_price_data, aes(wheelbase, price)) +
  geom_point(aes(colour = "blue" )) +
  geom_smooth(method=lm) +
  scale_x_continuous(name = "wheelbase") + 
  scale_y_continuous(name = "Price")

#Training data and test data build started..

#The set.seed( ) command is used to reproduce the same results each time while sampling 70%
#data for the training dataset. This is done so that you get the same training data as your sample
#each time you execute the R code
set.seed(100)  

# Use sample function for getting the random number represents 70% of indices. 
training_indexes = sample(1:nrow(car_price_data), 0.7*nrow(car_price_data))

#Create training data objects and test data objects
training_data <- car_price_data[training_indexes,]
test_data <- car_price_data[-training_indexes,]

#Model building started...

#Create a multi linear regression model
LR_model_1 <- lm(formula = price ~., data = training_data)

#vif(LR_model_1)
summary(LR_model_1) #Multiple R-squared:  0.9806,	Adjusted R-squared:  0.9651

#Many iterations can be skipped through the stepAIC command to remove multi collinear
#and insignificant variables. The last call that stepAIC makes, contains only the variables
#it considers to be important in the model. 
step <- stepAIC(LR_model_1, direction="both")
step

# Now store the last model equation of stepACI method into an object called LR_model_2
# You can notice that stepAIC removed many variables. 
LR_model_2 <- lm(formula = price ~ symboling + aspiration + enginelocation + 
     carlength + carwidth + carheight + curbweight + enginesize + 
     boreratio + stroke + horsepower + peakrpm + car_companyAUDI + 
     car_companyBMW + car_companyBUICK + car_companyCHEVROLET + 
     car_companyDODGE + car_companyHONDA + car_companyMERCURY + 
     car_companyMITSUBISHI + car_companyPEUGEOT + car_companyPLYMOUTH + 
     car_companyPORSCHE + car_companySAAB + car_companyTOYOTA + 
     car_companyVW + carbodyHARDTOP + carbodyHATCHBACK + carbodySEDAN + 
     carbodyWAGON + enginetypeL + enginetypeOHCV + enginetypeROTOR + 
     cylindernumberFIVE + cylindernumberFOUR + cylindernumberTWELVE + 
     fuelsystemMFI + fuelsystemMPFI + fuelsystemSPDI, data = training_data)

# Now check for multicollinearity. If the VIF is above 2 you would remove the variables if
# they are statistically insignificant
#vif(LR_model_2)
summary(LR_model_2)

# Now As we can see horsepower variable is having High VIF (31.287031) and it is insignificant(p = 0.075193) 
# which is p > 0.05. Make a new model without horsepower variable
LR_model_3 <- lm(formula = price ~ symboling + aspiration + enginelocation + 
                   carlength + carwidth + carheight + curbweight + enginesize + 
                   boreratio + stroke + peakrpm + car_companyAUDI + 
                   car_companyBMW + car_companyBUICK + car_companyCHEVROLET + 
                   car_companyDODGE + car_companyHONDA + car_companyMERCURY + 
                   car_companyMITSUBISHI + car_companyPEUGEOT + car_companyPLYMOUTH + 
                   car_companyPORSCHE + car_companySAAB + car_companyTOYOTA + 
                   car_companyVW + carbodyHARDTOP + carbodyHATCHBACK + carbodySEDAN + 
                   carbodyWAGON + enginetypeL + enginetypeOHCV + enginetypeROTOR + 
                   cylindernumberFIVE + cylindernumberFOUR + cylindernumberTWELVE + 
                   fuelsystemMFI + fuelsystemMPFI + fuelsystemSPDI, data = training_data)

# Now check for multicollinearity. If the VIF is above 2 you would remove the variables if
# they are statistically insignificant
#vif(LR_model_3)
summary(LR_model_3)

# Now As we can see carbodySEDAN variable is having High VIF (18.305489) and it is insignificant(p = 0.029329) 
# Make a new model without carbodySEDAN variable
LR_model_4 <- lm(formula = price ~ symboling + aspiration + enginelocation + 
                   carlength + carwidth + carheight + curbweight + enginesize + 
                   boreratio + stroke + peakrpm + car_companyAUDI + 
                   car_companyBMW + car_companyBUICK + car_companyCHEVROLET + 
                   car_companyDODGE + car_companyHONDA + car_companyMERCURY + 
                   car_companyMITSUBISHI + car_companyPEUGEOT + car_companyPLYMOUTH + 
                   car_companyPORSCHE + car_companySAAB + car_companyTOYOTA + 
                   car_companyVW + carbodyHARDTOP + carbodyHATCHBACK + 
                   carbodyWAGON + enginetypeL + enginetypeOHCV + enginetypeROTOR + 
                   cylindernumberFIVE + cylindernumberFOUR + cylindernumberTWELVE + 
                   fuelsystemMFI + fuelsystemMPFI + fuelsystemSPDI, data = training_data)

# Now check for multicollinearity. If the VIF is above 2 you would remove the variables if
# they are statistically insignificant
#vif(LR_model_4)
summary(LR_model_4)

    
# Now As we can see cylindernumberFIVE variable is having High VIF (7.140547) and it is insignificant(p = 0.180490) 
# Make a new model without cylindernumberFIVE variable
LR_model_5 <- lm(formula = price ~ symboling + aspiration + enginelocation + 
                   carlength + carwidth + carheight + curbweight + enginesize + 
                   boreratio + stroke + peakrpm + car_companyAUDI + 
                   car_companyBMW + car_companyBUICK + car_companyCHEVROLET + 
                   car_companyDODGE + car_companyHONDA + car_companyMERCURY + 
                   car_companyMITSUBISHI + car_companyPEUGEOT + car_companyPLYMOUTH + 
                   car_companyPORSCHE + car_companySAAB + car_companyTOYOTA + 
                   car_companyVW + carbodyHARDTOP + carbodyHATCHBACK + 
                   carbodyWAGON + enginetypeL + enginetypeOHCV + enginetypeROTOR + 
                   cylindernumberFOUR + cylindernumberTWELVE + 
                   fuelsystemMFI + fuelsystemMPFI + fuelsystemSPDI, data = training_data)

# Now check for multicollinearity. If the VIF is above 2 you would remove the variables if
# they are statistically insignificant
#vif(LR_model_5)
summary(LR_model_5)

# Now As we can see carheight variable is having High VIF (4.256512) and it is insignificant(p = 0.034161) 
# Make a new model without cylindernumberFIVE variable
LR_model_6 <- lm(formula = price ~ symboling + aspiration + enginelocation + 
                   carlength + carwidth + curbweight + enginesize + 
                   boreratio + stroke + peakrpm + car_companyAUDI + 
                   car_companyBMW + car_companyBUICK + car_companyCHEVROLET + 
                   car_companyDODGE + car_companyHONDA + car_companyMERCURY + 
                   car_companyMITSUBISHI + car_companyPEUGEOT + car_companyPLYMOUTH + 
                   car_companyPORSCHE + car_companySAAB + car_companyTOYOTA + 
                   car_companyVW + carbodyHARDTOP + carbodyHATCHBACK + 
                   carbodyWAGON + enginetypeL + enginetypeOHCV + enginetypeROTOR + 
                   cylindernumberFOUR + cylindernumberTWELVE + 
                   fuelsystemMFI + fuelsystemMPFI + fuelsystemSPDI, data = training_data)

# Now check for multicollinearity. If the VIF is above 2 you would remove the variables if
# they are statistically insignificant
#vif(LR_model_6)
summary(LR_model_6)

# Now As we can see fuelsystemMPFI variable is having High VIF (3.284775) and it is insignificant(p = 0.144624) 
# Make a new model without fuelsystemMPFI variable
LR_model_7 <- lm(formula = price ~ symboling + aspiration + enginelocation + 
                   carlength + carwidth + curbweight + enginesize + 
                   boreratio + stroke + peakrpm + car_companyAUDI + 
                   car_companyBMW + car_companyBUICK + car_companyCHEVROLET + 
                   car_companyDODGE + car_companyHONDA + car_companyMERCURY + 
                   car_companyMITSUBISHI + car_companyPEUGEOT + car_companyPLYMOUTH + 
                   car_companyPORSCHE + car_companySAAB + car_companyTOYOTA + 
                   car_companyVW + carbodyHARDTOP + carbodyHATCHBACK + 
                   carbodyWAGON + enginetypeL + enginetypeOHCV + enginetypeROTOR + 
                   cylindernumberFOUR + cylindernumberTWELVE + 
                   fuelsystemMFI + fuelsystemSPDI, data = training_data)

# Now check for multicollinearity. If the VIF is above 2 you would remove the variables if
# they are statistically insignificant
#vif(LR_model_7)
summary(LR_model_7)

# Now As we can see car_companyCHEVROLET variable is having High VIF (2.156182) and it is insignificant(p = 0.303243) 
# Make a new model without car_companyCHEVROLET variable
LR_model_8 <- lm(formula = price ~ symboling + aspiration + enginelocation + 
                   carlength + carwidth + curbweight + enginesize + 
                   boreratio + stroke + peakrpm + car_companyAUDI + 
                   car_companyBMW + car_companyBUICK + 
                   car_companyDODGE + car_companyHONDA + car_companyMERCURY + 
                   car_companyMITSUBISHI + car_companyPEUGEOT + car_companyPLYMOUTH + 
                   car_companyPORSCHE + car_companySAAB + car_companyTOYOTA + 
                   car_companyVW + carbodyHARDTOP + carbodyHATCHBACK + 
                   carbodyWAGON + enginetypeL + enginetypeOHCV + enginetypeROTOR + 
                   cylindernumberFOUR + cylindernumberTWELVE + 
                   fuelsystemMFI + fuelsystemSPDI, data = training_data)

# Now check for multicollinearity. If the VIF is above 2 you would remove the variables if
# they are statistically insignificant
#vif(LR_model_8)
summary(LR_model_8)

# Now As we can see symboling variable is having High VIF (2.657015) and it is insignificant(p = 0.091788) 
# Make a new model without symboling variable
LR_model_9 <- lm(formula = price ~ aspiration + enginelocation + 
                   carlength + carwidth + curbweight + enginesize + 
                   boreratio + stroke + peakrpm + car_companyAUDI + 
                   car_companyBMW + car_companyBUICK + 
                   car_companyDODGE + car_companyHONDA + car_companyMERCURY + 
                   car_companyMITSUBISHI + car_companyPEUGEOT + car_companyPLYMOUTH + 
                   car_companyPORSCHE + car_companySAAB + car_companyTOYOTA + 
                   car_companyVW + carbodyHARDTOP + carbodyHATCHBACK + 
                   carbodyWAGON + enginetypeL + enginetypeOHCV + enginetypeROTOR + 
                   cylindernumberFOUR + cylindernumberTWELVE + 
                   fuelsystemMFI + fuelsystemSPDI, data = training_data)

# Now check for multicollinearity. If the VIF is above 2 you would remove the variables if
# they are statistically insignificant
#vif(LR_model_9)
summary(LR_model_9)

# Now As we can see carbodyHATCHBACK variable is having High VIF (2.324258) and it is insignificant(p = 0.022134) 
# Make a new model without carbodyHATCHBACK variable
LR_model_10 <- lm(formula = price ~ aspiration + enginelocation + 
                   carlength + carwidth + curbweight + enginesize + 
                   boreratio + stroke + peakrpm + car_companyAUDI + 
                   car_companyBMW + car_companyBUICK + 
                   car_companyDODGE + car_companyHONDA + car_companyMERCURY + 
                   car_companyMITSUBISHI + car_companyPEUGEOT + car_companyPLYMOUTH + 
                   car_companyPORSCHE + car_companySAAB + car_companyTOYOTA + 
                   car_companyVW + carbodyHARDTOP + 
                   carbodyWAGON + enginetypeL + enginetypeOHCV + enginetypeROTOR + 
                   cylindernumberFOUR + cylindernumberTWELVE + 
                   fuelsystemMFI + fuelsystemSPDI, data = training_data)

# Now check for multicollinearity. If the VIF is above 2 you would remove the variables if
# they are statistically insignificant
#vif(LR_model_10)
summary(LR_model_10)

# Now As we can see car_companyPORSCHE variable is having High VIF (2.146181) and it is insignificant(p = 0.017200) 
# Make a new model without car_companyPORSCHE variable
LR_model_11 <- lm(formula = price ~ aspiration + enginelocation + 
                    carlength + carwidth + curbweight + enginesize + 
                    boreratio + stroke + peakrpm + car_companyAUDI + 
                    car_companyBMW + car_companyBUICK + 
                    car_companyDODGE + car_companyHONDA + car_companyMERCURY + 
                    car_companyMITSUBISHI + car_companyPEUGEOT + car_companyPLYMOUTH + 
                    car_companySAAB + car_companyTOYOTA + 
                    car_companyVW + carbodyHARDTOP + 
                    carbodyWAGON + enginetypeL + enginetypeOHCV + enginetypeROTOR + 
                    cylindernumberFOUR + cylindernumberTWELVE + 
                    fuelsystemMFI + fuelsystemSPDI, data = training_data)

# Now check for multicollinearity. If the VIF is above 2 you would remove the variables if
# they are statistically insignificant
#vif(LR_model_11)
summary(LR_model_11)

# Now As we can see fuelsystemSPDI variable is having High VIF (2.193557) and it is insignificant(p = 0.017179) 
# Make a new model without fuelsystemSPDI variable
LR_model_12 <- lm(formula = price ~ aspiration + enginelocation + 
                    carlength + carwidth + curbweight + enginesize + 
                    boreratio + stroke + peakrpm + car_companyAUDI + 
                    car_companyBMW + car_companyBUICK + 
                    car_companyDODGE + car_companyHONDA + car_companyMERCURY + 
                    car_companyMITSUBISHI + car_companyPEUGEOT + car_companyPLYMOUTH + 
                    car_companySAAB + car_companyTOYOTA + 
                    car_companyVW + carbodyHARDTOP + 
                    carbodyWAGON + enginetypeL + enginetypeOHCV + enginetypeROTOR + 
                    cylindernumberFOUR + cylindernumberTWELVE + 
                    fuelsystemMFI, data = training_data)

# Now check for multicollinearity. If the VIF is above 2 you would remove the variables if
# they are statistically insignificant
#vif(LR_model_12)
summary(LR_model_12)

# Now As we can see aspiration variable is having High VIF (2.153925) and it is insignificant(p = 0.001295) 
# Make a new model without aspiration variable
LR_model_13 <- lm(formula = price ~ enginelocation + 
                    carlength + carwidth + curbweight + enginesize + 
                    boreratio + stroke + peakrpm + car_companyAUDI + 
                    car_companyBMW + car_companyBUICK + 
                    car_companyDODGE + car_companyHONDA + car_companyMERCURY + 
                    car_companyMITSUBISHI + car_companyPEUGEOT + car_companyPLYMOUTH + 
                    car_companySAAB + car_companyTOYOTA + 
                    car_companyVW + carbodyHARDTOP + 
                    carbodyWAGON + enginetypeL + enginetypeOHCV + enginetypeROTOR + 
                    cylindernumberFOUR + cylindernumberTWELVE + 
                    fuelsystemMFI, data = training_data)

# Now check for multicollinearity. If the VIF is above 2 you would remove the variables if
# they are statistically insignificant
#vif(LR_model_13)
summary(LR_model_13)

#Model LR_model_13 looks stable considering both the paramenter multicollinearity and level of significance.
#Now we can reduce few more variable to make our model more simple, if it is having very high vif or highly
#insignificant.

# Now As we can see car_companyVW variable is insignificant(p = 0.934537) 
# Make a new model without car_companyVW variable
LR_model_14 <- lm(formula = price ~ enginelocation + 
                    carlength + carwidth + curbweight + enginesize + 
                    boreratio + stroke + peakrpm + car_companyAUDI + 
                    car_companyBMW + car_companyBUICK + 
                    car_companyDODGE + car_companyHONDA + car_companyMERCURY + 
                    car_companyMITSUBISHI + car_companyPEUGEOT + car_companyPLYMOUTH + 
                    car_companySAAB + car_companyTOYOTA + 
                    carbodyHARDTOP + 
                    carbodyWAGON + enginetypeL + enginetypeOHCV + enginetypeROTOR + 
                    cylindernumberFOUR + cylindernumberTWELVE + 
                    fuelsystemMFI, data = training_data)

# Now check for multicollinearity. If the VIF is above 2 you would remove the variables if
# they are statistically insignificant
#vif(LR_model_14)
summary(LR_model_14)

# Now As we can see  carbodyHARDTOP variable is insignificant(p = 0.83973) 
# Make a new model without carbodyHARDTOP variable
LR_model_15 <- lm(formula = price ~ enginelocation + 
                    carlength + carwidth + curbweight + enginesize + 
                    boreratio + stroke + peakrpm + car_companyAUDI + 
                    car_companyBMW + car_companyBUICK + 
                    car_companyDODGE + car_companyHONDA + car_companyMERCURY + 
                    car_companyMITSUBISHI + car_companyPEUGEOT + car_companyPLYMOUTH + 
                    car_companySAAB + car_companyTOYOTA + 
                    carbodyWAGON + enginetypeL + enginetypeOHCV + enginetypeROTOR + 
                    cylindernumberFOUR + cylindernumberTWELVE + 
                    fuelsystemMFI, data = training_data)

# Now check for multicollinearity. If the VIF is above 2 you would remove the variables if
# they are statistically insignificant
#vif(LR_model_15)
summary(LR_model_15)

# Now As we can see  car_companyMERCURY variable is insignificant(p = 0.487582) 
# Make a new model without car_companyMERCURY variable
LR_model_16 <- lm(formula = price ~ enginelocation + 
                    carlength + carwidth + curbweight + enginesize + 
                    boreratio + stroke + peakrpm + car_companyAUDI + 
                    car_companyBMW + car_companyBUICK + 
                    car_companyDODGE + car_companyHONDA + 
                    car_companyMITSUBISHI + car_companyPEUGEOT + car_companyPLYMOUTH + 
                    car_companySAAB + car_companyTOYOTA + 
                    carbodyWAGON + enginetypeL + enginetypeOHCV + enginetypeROTOR + 
                    cylindernumberFOUR + cylindernumberTWELVE + 
                    fuelsystemMFI, data = training_data)

# Now check for multicollinearity. If the VIF is above 2 you would remove the variables if
# they are statistically insignificant
#vif(LR_model_16)
summary(LR_model_16)

# Now As we can see  fuelsystemMFI variable is insignificant(p = 0.358498) 
# Make a new model without fuelsystemMFI variable
LR_model_17 <- lm(formula = price ~ enginelocation + 
                    carlength + carwidth + curbweight + enginesize + 
                    boreratio + stroke + peakrpm + car_companyAUDI + 
                    car_companyBMW + car_companyBUICK + 
                    car_companyDODGE + car_companyHONDA + 
                    car_companyMITSUBISHI + car_companyPEUGEOT + car_companyPLYMOUTH + 
                    car_companySAAB + car_companyTOYOTA + 
                    carbodyWAGON + enginetypeL + enginetypeOHCV + enginetypeROTOR + 
                    cylindernumberFOUR + cylindernumberTWELVE, data = training_data)

# Now check for multicollinearity. If the VIF is above 2 you would remove the variables if
# they are statistically insignificant
#vif(LR_model_17)
summary(LR_model_17)

# Now As we can see  carbodyWAGON variable is insignificant(p = 0.29694) 
# Make a new model without carbodyWAGON variable
LR_model_18 <- lm(formula = price ~ enginelocation + 
                    carlength + carwidth + curbweight + enginesize + 
                    boreratio + stroke + peakrpm + car_companyAUDI + 
                    car_companyBMW + car_companyBUICK + 
                    car_companyDODGE + car_companyHONDA + 
                    car_companyMITSUBISHI + car_companyPEUGEOT + car_companyPLYMOUTH + 
                    car_companySAAB + car_companyTOYOTA + 
                    enginetypeL + enginetypeOHCV + enginetypeROTOR + 
                    cylindernumberFOUR + cylindernumberTWELVE, data = training_data)

# Now check for multicollinearity. If the VIF is above 2 you would remove the variables if
# they are statistically insignificant
#vif(LR_model_18)
summary(LR_model_18)

# Now As we can see  car_companySAAB variable is insignificant(p = 0.017003) 
# Make a new model without car_companySAAB variable
LR_model_19 <- lm(formula = price ~ enginelocation + 
                    carlength + carwidth + curbweight + enginesize + 
                    boreratio + stroke + peakrpm + car_companyAUDI + 
                    car_companyBMW + car_companyBUICK + 
                    car_companyDODGE + car_companyHONDA + 
                    car_companyMITSUBISHI + car_companyPEUGEOT + car_companyPLYMOUTH + 
                    car_companyTOYOTA + 
                    enginetypeL + enginetypeOHCV + enginetypeROTOR + 
                    cylindernumberFOUR + cylindernumberTWELVE, data = training_data)

# Now check for multicollinearity. If the VIF is above 2 you would remove the variables if
# they are statistically insignificant
#vif(LR_model_19)
summary(LR_model_19)

# Now As we can see  car_companyDODGE variable is insignificant(p = 0.016158) 
# Make a new model without car_companyDODGE variable
LR_model_20 <- lm(formula = price ~ enginelocation + 
                    carlength + carwidth + curbweight + enginesize + 
                    boreratio + stroke + peakrpm + car_companyAUDI + 
                    car_companyBMW + car_companyBUICK + 
                    car_companyHONDA + 
                    car_companyMITSUBISHI + car_companyPEUGEOT + car_companyPLYMOUTH + 
                    car_companyTOYOTA + 
                    enginetypeL + enginetypeOHCV + enginetypeROTOR + 
                    cylindernumberFOUR + cylindernumberTWELVE, data = training_data)

# Now check for multicollinearity. If the VIF is above 2 you would remove the variables if
# they are statistically insignificant
#vif(LR_model_20)
summary(LR_model_20)

# Now As we can see  car_companyAUDI variable is insignificant(p = 0.016692) 
# Make a new model without car_companyAUDI variable
LR_model_21 <- lm(formula = price ~ enginelocation + 
                    carlength + carwidth + curbweight + enginesize + 
                    boreratio + stroke + peakrpm + 
                    car_companyBMW + car_companyBUICK + 
                    car_companyHONDA + 
                    car_companyMITSUBISHI + car_companyPEUGEOT + car_companyPLYMOUTH + 
                    car_companyTOYOTA + 
                    enginetypeL + enginetypeOHCV + enginetypeROTOR + 
                    cylindernumberFOUR + cylindernumberTWELVE, data = training_data)

# Now check for multicollinearity. If the VIF is above 2 you would remove the variables if
# they are statistically insignificant
#vif(LR_model_21)
summary(LR_model_21)

# Now As we can see  car_companyTOYOTA variable is insignificant(p = 0.017312) 
# Make a new model without car_companyTOYOTA variable
LR_model_22 <- lm(formula = price ~ enginelocation + 
                    carlength + carwidth + curbweight + enginesize + 
                    boreratio + stroke + peakrpm + 
                    car_companyBMW + car_companyBUICK + 
                    car_companyHONDA + 
                    car_companyMITSUBISHI + car_companyPEUGEOT + car_companyPLYMOUTH +
                    enginetypeL + enginetypeOHCV + enginetypeROTOR + 
                    cylindernumberFOUR + cylindernumberTWELVE, data = training_data)

# Now check for multicollinearity. If the VIF is above 2 you would remove the variables if
# they are statistically insignificant
#vif(LR_model_22)
summary(LR_model_22)

# Now As we can see  car_companyHONDA variable is insignificant(p = 0.042617) 
# Make a new model without car_companyHONDA variable
LR_model_23 <- lm(formula = price ~ enginelocation + 
                    carlength + carwidth + curbweight + enginesize + 
                    boreratio + stroke + peakrpm + 
                    car_companyBMW + car_companyBUICK +
                    car_companyMITSUBISHI + car_companyPEUGEOT +
                    enginetypeL + enginetypeOHCV + enginetypeROTOR + 
                    cylindernumberFOUR + cylindernumberTWELVE, data = training_data)

# Now check for multicollinearity. If the VIF is above 2 you would remove the variables if
# they are statistically insignificant
#vif(LR_model_23)
summary(LR_model_23)

# Now As we can see  car_companyMITSUBISHI variable is insignificant(p = 0.012281) 
# Make a new model without car_companyMITSUBISHI variable
LR_model_24 <- lm(formula = price ~ enginelocation + 
                    carlength + carwidth + curbweight + enginesize + 
                    boreratio + stroke + peakrpm + 
                    car_companyBMW + car_companyBUICK + car_companyPEUGEOT +
                    enginetypeL + enginetypeOHCV + enginetypeROTOR + 
                    cylindernumberFOUR + cylindernumberTWELVE, data = training_data)

# Now check for multicollinearity. If the VIF is above 2 you would remove the variables if
# they are statistically insignificant
#vif(LR_model_24)
summary(LR_model_24)

# Now As we can see  carlength variable is insignificant(p = 0.027613) 
# Make a new model without carlength variable
LR_model_final <- lm(formula = price ~ enginelocation + 
                    carwidth + curbweight + enginesize + 
                    boreratio + stroke + peakrpm + 
                    car_companyBMW + car_companyBUICK + car_companyPEUGEOT +
                    enginetypeL + enginetypeOHCV + enginetypeROTOR + 
                    cylindernumberFOUR + cylindernumberTWELVE, data = training_data)

# Now check for multicollinearity. If the VIF is above 2 you would remove the variables if
# they are statistically insignificant
#vif(LR_model_final)
summary(LR_model_final)

#Predict the car price by giving test_data as input into LR_model_final to evaluate our model's efficiency.
test_data$predicted_price <- predict(LR_model_final, subset(test_data, select = -c(price)))

#R_Square value using test_data into the model LR_model_final is equals to the square of correlation between
#actual price and predicted price.
R <- cor(test_data$price,test_data$predicted_price)
R_square <- R^2

#Lets compare the actual price and predicted price
compare_result <- subset(test_data, select = c(price,predicted_price))
View(compare_result)
