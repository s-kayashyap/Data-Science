# CredX - Credit Risk Analytics
# Input Dataset : Demographic data.csv
#                 Credit Bureau data.csv
#
# Scope : To build a predictive model which will use the past data of credit card applicants and help CredX
#         identify the right customers to minimise the credit loss.
#
# USe  :  This model can be used to predict the right credit card customers and mitigate the credit risk.
#
# Name :  Swami Prem Pranav Kayashyap (APFE1786831)
# ------------------------------------------------------
# Date : 16th DEC, 2018

# Following steps has been followed to do this analysis
# Step 1. Build a basic model on Demographic data (Balanaced and Unbalanced training dataset).
# Step 2. Data cleaning and preparation of Merged dataset (Demographic and Credit Bureau Data)
# Step 3. EDA on Merged dataset (Demographic and Credit Bureau Data)
# Step 4. Build a logistic regression model on Unbalanced training set and Evaluate the Model
# Step 5. Build a logistic regression model on Balanced training set and Evaluate the Model
# Step 6. Build a logistic regression model on Unbalanced and woe imputed training set and Evaluate the Model
# Step 7. Build a logistic regression model on Balanced and woe imputed training set and Evaluate the Model
# Step 8. Create Application Score card

#Install required package
#install.packages("Hmisc") # To imputes missing value
#install.packages("ggplot2") # For plotting in EDA
#install.packages(reshape2)  # Melt data
#install.packages(corrplot)  # For Heatmap
#install.packages("information") #Create infotable
#install.packages("woe")
#install.packages("dummies") # To use model.matrix to create dummy variable
#install.packages(DMwR)      # To Use SMOTE function
#install.packages(car)       # To use vif
#install.packages(MASS)      # To use stepAIC
#install.packages(caret)     # To draw confusion matix
#install.packages(ROCR)      # To use prediction in KS- statistics
#install.packages(randomForest) # Implement Random Forest

#Load required package
library(Hmisc)     # To imputes missing value
library(ggplot2)   # To use ggplot
library(reshape2)  # Melt data
library(corrplot)  # For Heatmap
library(woe)
library(dummies)  # To use model.matrix to create dummy variable
library(Information) # To use create_infotables
library(DMwR)      # To Use SMOTE function
library(car)       # To use vif
library(MASS)      # To use stepAIC
library(caret)     # To draw confusion matix
library(ROCR)      # To use prediction in KS- statistics
library(randomForest) # Implement Random Forest

###########################################################################
## Steps 1. Data cleaning and preparation                  ################
###########################################################################

#Loading Demographic Data set
Demographic_Data <- read.csv("Demographic data.csv", stringsAsFactors = F)

# Checking structure of Demographic dataset 
str(Demographic_Data)

# Summary of Demographic dataset
summary(Demographic_Data)

#Loading Credit Bureau Data set
Credit_Bureau_Data <- read.csv("Credit Bureau data.csv",stringsAsFactors = F)

# Checking structure of Credit Bureau dataset 
str(Credit_Bureau_Data)

# Summary of dataset
summary(Credit_Bureau_Data)

###Merging the datasets (Demographic_Data and Credit_Bureau_Data)
Credit_Application_Data <- merge(Demographic_Data, Credit_Bureau_Data, by = "Application.ID")

# Checking structure of Credit Application dataset 
str(Credit_Application_Data)

# Summary of Credit Application dataset
summary(Credit_Application_Data)

# First we will be building model on demographic data and we will check the performance and then
# we will build model on Merge dataset (Demographic data and Bureau data)  

################################################################################
######          Operation on Demographic dataset               #################
################################################################################

#Checking Unique Application ID in Demographic_Data
length(Demographic_Data$Application.ID) #71295
length(unique(Demographic_Data$Application.ID)) #71292
sum(duplicated(Demographic_Data$Application.ID))

#Find the rows containing the duplicate application id
Demographic_Data[duplicated(Demographic_Data$Application.ID),]

#Creating a dataframe enlisting the duplicate application ids.
Application_Data_duplicate <- Demographic_Data[ which( Demographic_Data$Application.ID == '765011468' | Demographic_Data$Application.ID == '653287861' | Demographic_Data$Application.ID == '671989187') , ]
Application_Data_duplicate

#Removing the extra rows with duplicate application id (If duplicate applications have performance tag = 0 and 1
#We are removing performance tag = 0 and kepping performance tag = 1 i.e Application which is defaulted)
Demographic_Data <- Demographic_Data[-c(5244,24387,48603), ]

#Changing the column name for Demographic_Data columns
colnames(Demographic_Data)[colnames(Demographic_Data) == "Marital.Status..at.the.time.of.application."] <- "Marital_Status"
colnames(Demographic_Data)[colnames(Demographic_Data) == "No.of.dependents"] <- "No_of_dependents"
colnames(Demographic_Data)[colnames(Demographic_Data) == "Type.of.residence"] <- "Type_of_Residence"
colnames(Demographic_Data)[colnames(Demographic_Data) == "No.of.months.in.current.company"] <- "No_of_months_in_current_company"
colnames(Demographic_Data)[colnames(Demographic_Data) == "No.of.months.in.current.residence"] <- "No_of_months_in_current_residence"

# Checking structure of Demographic_Data
str(Demographic_Data)

#NA value analysis in Demographic_Data
sum(is.na(Demographic_Data))    # 1428 NA's 
colSums(is.na(Demographic_Data))

sum(is.na(Demographic_Data$No_of_dependents))  # 3 NA's 
sum(is.na(Demographic_Data$Performance.Tag)) # 1425 NA's 

#Treating NA values in Credit Application dataset.Replacing all the missing value with mean/median whichever is better.
Demographic_Data$No_of_dependents <- impute(Demographic_Data$No_of_dependents, median)

#Standardising numerical data
Demographic_Data$No_of_dependents <- as.numeric(Demographic_Data$No_of_dependents)

#Check if any column is left with NA values.
colSums(is.na(Demographic_Data))

# Checking structure of Credit Application dataset 
str(Demographic_Data)

#Checking for blank space
sapply(Demographic_Data, function(x) length(which(x == ""))) #No blank spaces found

#Missing values in Gender is replaced with "O" which means others (There are limited no of missing value)
Demographic_Data$Gender <- ifelse(Demographic_Data$Gender =="","O",Demographic_Data$Gender)
#Missing value in marital status is replaced with "Others"
Demographic_Data$Marital_Status <- ifelse(Demographic_Data$Marital_Status =="","Others",Demographic_Data$Marital_Status)
#Missing Values in Education variable is replaced with "Others"
Demographic_Data$Education <- ifelse(Demographic_Data$Education == "","Others",Demographic_Data$Education)
#Missing Values in Profession variable is replaced with "Others"
Demographic_Data$Profession <- ifelse(Demographic_Data$Profession == "","Others",Demographic_Data$Profession)
#Missing Values in Type of Residence variable is replaced with "Others"
Demographic_Data$Type_of_Residence <- ifelse(Demographic_Data$Type_of_Residence == "","Others",Demographic_Data$Type_of_Residence)

# Checking structure of Credit Application dataset 
str(Demographic_Data)

#converting all the non numeric columns to upper case 
Demographic_Data <- data.frame(lapply(Demographic_Data, function(v) {
  if (is.character(v)) return(toupper(v))
  else return(v)
}))

str(Demographic_Data)
summary(Demographic_Data)

#Replacing erroneous values in age column with the mean
Demographic_Data$Age <- ifelse(Demographic_Data$Age < 18, round(mean(Demographic_Data$Age,na.rm=T),0), Demographic_Data$Age)
#Negative income values is replaced with mean value of all data
Demographic_Data$Income <- ifelse(Demographic_Data$Income < 0, round(mean(Demographic_Data$Income,na.rm=T),0), Demographic_Data$Income)

#Dummy variable creation of all the categorical variable

#Dummy variable for Gender
dummy <- data.frame(model.matrix(~Gender, data = Demographic_Data))
Demographic_Data <- cbind(subset(Demographic_Data, select = -c(Gender)), dummy[,-1])

#Dummy variable for Marital_Status
dummy <- data.frame(model.matrix(~Marital_Status, data = Demographic_Data))
Demographic_Data <- cbind(subset(Demographic_Data, select = -c(Marital_Status)), dummy[,-1])

#Dummy variable for No_of_dependents
Demographic_Data$No_of_dependents <- factor(Demographic_Data$No_of_dependents)
dummy <- data.frame(model.matrix(~No_of_dependents, data = Demographic_Data))
Demographic_Data <- cbind(subset(Demographic_Data, select = -c(No_of_dependents)), dummy[,-1])

#Dummy variable for Education
dummy <- data.frame(model.matrix(~Education, data = Demographic_Data))
Demographic_Data <- cbind(subset(Demographic_Data, select = -c(Education)), dummy[,-1])

#Dummy variable for Profession
dummy <- data.frame(model.matrix(~Profession, data = Demographic_Data))
Demographic_Data <- cbind(subset(Demographic_Data, select = -c(Profession)), dummy[,-1])

#Dummy variable for Type_of_Residence
dummy <- data.frame(model.matrix(~Type_of_Residence, data = Demographic_Data))
Demographic_Data <- cbind(subset(Demographic_Data, select = -c(Type_of_Residence)), dummy[,-1])

#Scaling all the numeric predictors
Demographic_Data$Age <- scale(Demographic_Data$Age)
Demographic_Data$Income <- scale(Demographic_Data$Income)
Demographic_Data$No_of_months_in_current_residence <- scale(Demographic_Data$No_of_months_in_current_residence)
Demographic_Data$No_of_months_in_current_company <- scale(Demographic_Data$No_of_months_in_current_company)

# Checking structure of Credit Application dataset 
str(Demographic_Data)

#Extracting Rejected candidates (Having NULL value for performance tag) from Credit Application dataframe
Rejected_Application_Data <- Demographic_Data[is.na(Demographic_Data$Performance.Tag),]

#Application dataframe which doesn't have empty value for the Performance tag (Accepted Application)
Accepted_Application_Data <- subset(Demographic_Data, Demographic_Data$Performance.Tag != "")

length(unique(Accepted_Application_Data$Application.ID)) #69867 rows
summary(Accepted_Application_Data)

#Calculating Response rate
table(Accepted_Application_Data$Performance.Tag)
response <- 2948/(2948+66919)
response  #4.219%

#####       Correlation Matrix         #####

#Removing Application.ID as it is not usefull for correlation matrix
Corr_Mat_Data <- subset(Accepted_Application_Data, select = -c(Application.ID))

#Creating a data frame of all the numerical variables
Corr_Mat_Data <- Corr_Mat_Data[sapply(Corr_Mat_Data,is.numeric)]

#Creating correlation matrix
corr_matrix <- round(cor(Corr_Mat_Data),2)

#Creating lower triangle correlation matrix
get_lower_tri_corr_matrix <-function(corr_matrix){
  corr_matrix[upper.tri(corr_matrix)] <- NA
  return(corr_matrix)
}

#Creating upper triangle correlation matrix
get_upper_tri_corr_matrix <- function(corr_matrix){
  corr_matrix[lower.tri(corr_matrix)]<- NA
  return(corr_matrix)
}

# Use correlation between variables as distance
reorder_corr_matrix <- function(corr_matrix){
  dd <- as.dist((1 - corr_matrix)/2)
  hc <- hclust(dd)
  corr_matrix <- corr_matrix[hc$order, hc$order]
}

# Reorder the correlation matrix
corr_matrix <- reorder_corr_matrix(corr_matrix)

#Upper triangle correlation matrix
upper_tri_corr_matrix <- get_upper_tri_corr_matrix(corr_matrix)

# Melt the correlation matrix
melted_corr_matrix <- melt(upper_tri_corr_matrix, na.rm = TRUE)

# Create a ggheatmap
ggheatmap <- ggplot(melted_corr_matrix, aes(Var2, Var1, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "green", mid = "white", midpoint = 0, limit = c(-1,1), space = "Lab", name="Leading Credit Application Parameters Correlation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 12, hjust = 1)) +
  coord_fixed()

ggheatmap + geom_text(aes(Var2, Var1, label = value), color = "black", size = 2) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.justification = c(1, 0),
    legend.position = c(0.6, 0.7),
    legend.direction = "horizontal") +
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1, title.position = "top", title.hjust = 0.5))

#Flipping the value of performance tag so that it can used to create IV of variables
Accepted_Application_Data$Performance.Tag <- ifelse(Accepted_Application_Data$Performance.Tag == 0,1,0)

# WOE Calculation for Accepted Credit Application data
str(Accepted_Application_Data)

#Calulating IV values for the Credit Application data
IV <- create_infotables(data=Accepted_Application_Data, y="Performance.Tag", bins=10, parallel=TRUE)
IV_Value = data.frame(IV$Summary)

#Based on the Information Value Analysis, Most Significant Predictors are as follows :

#No_of_months_in_current_residence	0.0791
#Income	                            0.0426
#No_of_months_in_current_company	  0.0216
#Age	                              0.0034
#ProfessionSE	                      0.0022
#No_of_dependents2	                0.0019

#The set.seed( ) command is used to reproduce the same results each time while sampling 70%
#data for the training dataset. This is done so that you get the same training data as your sample
#each time you execute the R code
set.seed(100)  

# Use sample function for getting the random number represents 70% of indices. 
training_indexes = sample(1:nrow(Accepted_Application_Data), 0.7*nrow(Accepted_Application_Data))

#Create training data objects and test data objects
training_data <- Accepted_Application_Data[training_indexes,]
test_data <- Accepted_Application_Data[-training_indexes,]

#Check whether performance tag is in balanced proportion or not.
as.data.frame(table(training_data$Performance.Tag))

training_data$Performance.Tag <- as.factor(training_data$Performance.Tag)
## Smote : Synthetic Minority Oversampling Technique To Handle Class Imbalancy In Binary Classification
#Balancing the unbalanced train dataset
training_data_balanced <- SMOTE(Performance.Tag ~., training_data, perc.over = 100, perc.under=200)

as.data.frame(table(training_data_balanced$Performance.Tag))

#Model building started...

#Create a multi logistic regression model
LR_model_1 <- glm(formula = Performance.Tag ~., data = training_data, family = "binomial")

#Listing VIF value in decreasing order
sort((vif(LR_model_1)), decreasing = TRUE)
summary(LR_model_1)

#Many iterations can be skipped through the stepAIC command to remove multi collinear
#and insignificant variables. The last call that stepAIC makes, contains only the variables
#it considers to be important in the model. 
LR_model_2 <- stepAIC(LR_model_1, direction="both")

#Listing VIF value in decreasing order
sort((vif(LR_model_2)), decreasing = TRUE)
summary(LR_model_2)

#Removing No_of_dependents4 VIF = 1.049672 p value is 0.07885
LR_model_3 <- glm(formula = Performance.Tag ~ Income +
                    No_of_months_in_current_residence + 
                    No_of_months_in_current_company +
                    No_of_dependents2 +
                    ProfessionSE, data = training_data, family = "binomial")

#Listing VIF value in decreasing order
sort((vif(LR_model_3)), decreasing = TRUE)
summary(LR_model_3)

#Removing No_of_dependents2 VIF = 1.000023 and p value is 0.01228 * 
LR_model_final <- glm(formula = Performance.Tag ~ Income +
                        No_of_months_in_current_residence + 
                        No_of_months_in_current_company +
                        ProfessionSE, data = training_data, family = "binomial")

#Listing VIF value in decreasing order
sort((vif(LR_model_final)), decreasing = TRUE)
summary(LR_model_final)

#Model Evaluation started...

#Predict the performance by giving test_data as input into LR_model_final.It can be used to evaluate our model's efficiency.
prediction_data <- predict(LR_model_final,type = "response",subset(test_data, select = -c(Performance.Tag)))

#Actual Performance in test data
actual_performance <- factor(ifelse(test_data$Performance.Tag == 1, "Yes","No"))

#To find the optimal probalility cutoff value where our model is predicting efficiently.
check_model_performance <- function(probability_cutoff) 
{
  predicted_performance_temp <- factor(ifelse(prediction_data >= probability_cutoff, "Yes", "No"))
  confusion_matrix_temp    <- confusionMatrix(predicted_performance_temp,actual_performance, positive = "Yes")
  model_accuracy    <- confusion_matrix_temp$overall[1]
  model_sensitivity <- confusion_matrix_temp$byClass[1]
  model_specificity <- confusion_matrix_temp$byClass[2]
  model_evaluation_vector <- t(as.matrix(c(model_accuracy, model_sensitivity, model_specificity)))
  return(model_evaluation_vector)
}

# Creating probability cutoff values starting from 0.01 upto 1.0. Total 90 sample.
prob_cuttoff_sample   <- seq(0.01, 1, by = .01)

#Initiallizing a model_evaluation_matrix of size 90 X 3
model_evaluation_matrix <- matrix(0,100,3)

#filling in the matrix with values from confusion matrix with each probability
for(i in 1:100) {
  model_evaluation_matrix[i,] <- check_model_performance(prob_cuttoff_sample[i])
} 

optimal_prob_cutoff <- prob_cuttoff_sample[which.min(abs(model_evaluation_matrix[,2] - model_evaluation_matrix[,3]))]

#using probability cut-off as optimal_prob_cutoff
optimal_predicted_performance <- factor(ifelse(prediction_data >= optimal_prob_cutoff, "Yes", "No"))

## checking statistics with confusion matrix
confusion_matrix <- table(optimal_predicted_performance, actual_performance)
confusionMatrix(confusion_matrix, positive = "Yes")

#Model Performance Accuracy : 0.4902, Sensitivity : 0.48513,   Specificity : 0.60219

#Evaluation using KS statistic
optimal_predicted_performance <- ifelse(optimal_predicted_performance == "Yes", 1, 0)
actual_performance <- ifelse(actual_performance == "Yes", 1, 0)

optimal_prediction_test<- prediction(optimal_predicted_performance, actual_performance)
performance_measures_test<- performance(optimal_prediction_test, "tpr", "fpr")

ks_table_test <- attr(performance_measures_test, "y.values")[[1]] - (attr(performance_measures_test, "x.values")[[1]])
max(ks_table_test)

#Evaluation using AUC (Area under the curve)
auc <- performance(optimal_prediction_test, measure = "auc")
auc <- auc@y.values[[1]]
print(auc)

#Model Evaluation ends

#Model building started on balanced dataset...

#Create a multi logistic regression model
LR_model_balanced_1 <- glm(formula = Performance.Tag ~., data = training_data_balanced, family = "binomial")

#Many iterations can be skipped through the stepAIC command to remove multi collinear
#and insignificant variables. The last call that stepAIC makes, contains only the variables
#it considers to be important in the model. 
LR_model_balanced_2 <- stepAIC(LR_model_balanced_1, direction="both")

#Listing VIF value in decreasing order
sort((vif(LR_model_balanced_2)), decreasing = TRUE)
summary(LR_model_balanced_2)

#Removing GenderM
LR_model_balanced_3 <- glm(formula = Performance.Tag ~ Income +
                             No_of_months_in_current_residence + 
                             No_of_months_in_current_company +
                             Marital_StatusSINGLE + 
                             No_of_dependents2 +
                             No_of_dependents4 +
                             EducationMASTERS + 
                             EducationOTHERS +
                             EducationPHD +
                             EducationPROFESSIONAL + 
                             ProfessionSAL +
                             ProfessionSE, data = training_data_balanced, family = "binomial")

#Listing VIF value in decreasing order
sort((vif(LR_model_balanced_3)), decreasing = TRUE)
summary(LR_model_balanced_3)

#Removing No_of_months_in_current_residence
LR_model_balanced_4 <- glm(formula = Performance.Tag ~Income +
                             No_of_months_in_current_company +
                             Marital_StatusSINGLE + 
                             No_of_dependents2 +
                             No_of_dependents4 +
                             EducationMASTERS + 
                             EducationOTHERS +
                             EducationPHD +
                             EducationPROFESSIONAL + 
                             ProfessionSAL +
                             ProfessionSE , data = training_data_balanced, family = "binomial")

#Listing VIF value in decreasing order
sort((vif(LR_model_balanced_4)), decreasing = TRUE)
summary(LR_model_balanced_4)

#Removing Marital_StatusSINGLE, EducationOTHERS and ProfessionSAL 
LR_model_balanced_final <- glm(formula = Performance.Tag ~ Income +
                                 No_of_months_in_current_company +
                                 No_of_dependents2 +
                                 No_of_dependents4 +
                                 EducationMASTERS +
                                 EducationPHD +
                                 EducationPROFESSIONAL +
                                 ProfessionSE , data = training_data_balanced, family = "binomial")

#Listing VIF value in decreasing order
sort((vif(LR_model_balanced_final)), decreasing = TRUE)
summary(LR_model_balanced_final)

#Model Evaluation started...

#Predict the performance by giving test_data as input into LR_model_final.It can be used to evaluate our model's efficiency.
prediction_data <- predict(LR_model_balanced_final,type = "response",subset(test_data, select = -c(Performance.Tag)))

#Actual Performance
actual_performance <- factor(ifelse(test_data$Performance.Tag == 1, "Yes","No"))

# Creating probability cutoff values starting from 0.01 upto 1.0. Total 100 sample.
prob_cuttoff_sample   <- seq(0.01, 1, by = .01)

#Initiallizing a model_evaluation_matrix of size 90 X 3
model_evaluation_matrix <- matrix(0,100,3)

#filling in the matrix with values from confusion matrix with each probability
for(i in 1:100) {
  model_evaluation_matrix[i,] <- check_model_performance(prob_cuttoff_sample[i])
} 

optimal_prob_cutoff <- prob_cuttoff_sample[which.min(abs(model_evaluation_matrix[,2] - model_evaluation_matrix[,3]))]

#using probability cut-off as optimal_prob_cutoff
optimal_predicted_performance <- factor(ifelse(prediction_data >= optimal_prob_cutoff, "Yes", "No"))

## checking statistics with confusion matrix
confusion_matrix <- table(optimal_predicted_performance, actual_performance)
confusionMatrix(confusion_matrix, positive = "Yes")

#Model Performance Accuracy : 0.5205, Sensitivity : 0.53115,   Specificity :0.52005

#Evaluation using KS statistic
optimal_predicted_performance <- ifelse(optimal_predicted_performance == "Yes", 1, 0)
actual_performance <- ifelse(actual_performance == "Yes", 1, 0)

optimal_prediction_test<- prediction(optimal_predicted_performance, actual_performance)
performance_measures_test<- performance(optimal_prediction_test, "tpr", "fpr")

ks_table_test <- attr(performance_measures_test, "y.values")[[1]] - (attr(performance_measures_test, "x.values")[[1]])
max(ks_table_test)

#Evaluation using AUC (Area under the curve)
auc <- performance(optimal_prediction_test, measure = "auc")
auc <- auc@y.values[[1]]
print(auc)

################################################################################
######          MERGE DATASET (Credit Application data)        #################
################################################################################

# Now We will work on Merge dataset (Demographic and Credit Beauro Data). We will call all dataset
# as Credit Application dataset.

#Checking Unique Application ID in Credit Application datasets
length(Credit_Application_Data$Application.ID) #71301
length(unique(Credit_Application_Data$Application.ID)) #71292
sum(duplicated(Credit_Application_Data$Application.ID))

#Find the rows containing the duplicate application id
Credit_Application_Data[duplicated(Credit_Application_Data$Application.ID),]

#Creating a dataframe enlisting the duplicate application ids.
Application_Data_duplicate <- Credit_Application_Data[ which( Credit_Application_Data$Application.ID == '765011468' | Credit_Application_Data$Application.ID == '653287861' | Credit_Application_Data$Application.ID == '671989187') , ]
Application_Data_duplicate

#Removing the extra rows with duplicate application id (If duplicate applications have performance tag = 0 and 1
#We are removing performance tag = 0 and kepping performance tag = 1 i.e Application which is defaulted)
Credit_Application_Data <- Credit_Application_Data[-c(46613,46614,46615,47931,47932,47933,54595,54596,54597), ]

#Changing the column name for Credit Application dataset columns
colnames(Credit_Application_Data)[colnames(Credit_Application_Data) == "Marital.Status..at.the.time.of.application."] <- "Marital_Status"
colnames(Credit_Application_Data)[colnames(Credit_Application_Data) == "No.of.dependents"] <- "No_of_dependents"
colnames(Credit_Application_Data)[colnames(Credit_Application_Data) == "Type.of.residence"] <- "Type_of_Residence"
colnames(Credit_Application_Data)[colnames(Credit_Application_Data) == "No.of.months.in.current.company"] <- "No_of_months_in_current_company"
colnames(Credit_Application_Data)[colnames(Credit_Application_Data) == "No.of.months.in.current.residence"] <- "No_of_months_in_current_residence"
colnames(Credit_Application_Data)[colnames(Credit_Application_Data) == "No.of.times.90.DPD.or.worse.in.last.6.months"] <- "No_of_times_90_DPD_or_worse_in_last_6_months"
colnames(Credit_Application_Data)[colnames(Credit_Application_Data) == "No.of.times.60.DPD.or.worse.in.last.6.months"] <- "No_of_times_60_DPD_or_worse_in_last_6_months"
colnames(Credit_Application_Data)[colnames(Credit_Application_Data) == "No.of.times.30.DPD.or.worse.in.last.6.months"] <- "No_of_times_30_DPD_or_worse_in_last_6_months"
colnames(Credit_Application_Data)[colnames(Credit_Application_Data) == "No.of.times.90.DPD.or.worse.in.last.12.months"] <- "No_of_times_90_DPD_or_worse_in_last_12_months"
colnames(Credit_Application_Data)[colnames(Credit_Application_Data) == "No.of.times.60.DPD.or.worse.in.last.12.months"] <- "No_of_times_60_DPD_or_worse_in_last_12_months"
colnames(Credit_Application_Data)[colnames(Credit_Application_Data) == "No.of.times.30.DPD.or.worse.in.last.12.months"] <- "No_of_times_30_DPD_or_worse_in_last_12_months"
colnames(Credit_Application_Data)[colnames(Credit_Application_Data) == "Avgas.CC.Utilization.in.last.12.months"] <- "Avgas_CC_Utilization_in_last_12_months"
colnames(Credit_Application_Data)[colnames(Credit_Application_Data) == "No.of.trades.opened.in.last.6.months"] <- "No_of_trades_opened_in_last_6_months"
colnames(Credit_Application_Data)[colnames(Credit_Application_Data) == "No.of.trades.opened.in.last.12.months"] <- "No_of_trades_opened_in_last_12_months"
colnames(Credit_Application_Data)[colnames(Credit_Application_Data) == "No.of.PL.trades.opened.in.last.6.months"] <- "No_of_PL_trades_opened_in_last_6_months"
colnames(Credit_Application_Data)[colnames(Credit_Application_Data) == "No.of.PL.trades.opened.in.last.12.months"] <- "No_of_PL_trades_opened_in_last_12_months"
colnames(Credit_Application_Data)[colnames(Credit_Application_Data) == "No.of.Inquiries.in.last.6.months..excluding.home...auto.loans."] <- "No_of_Inquiries_in_last_6_months_excluding_home_auto_loans"
colnames(Credit_Application_Data)[colnames(Credit_Application_Data) == "No.of.Inquiries.in.last.12.months..excluding.home...auto.loans."] <- "No_of_Inquiries_in_last_12_months_excluding_home_auto_loans"
colnames(Credit_Application_Data)[colnames(Credit_Application_Data) == "Presence.of.open.home.loan"] <- "Presence_of_open_home_loan"
colnames(Credit_Application_Data)[colnames(Credit_Application_Data) == "Outstanding.Balance"] <- "Outstanding_Balance"
colnames(Credit_Application_Data)[colnames(Credit_Application_Data) == "Total.No.of.Trades"] <- "Total_No_of_Trades"
colnames(Credit_Application_Data)[colnames(Credit_Application_Data) == "Presence.of.open.auto.loan"] <- "Presence_of_open_auto_loan"
colnames(Credit_Application_Data)[colnames(Credit_Application_Data) == "Performance.Tag.y"] <- "Performance.Tag"

#Removing columns which is not required for this analysis such as Performance.Tag.x, as this variable is common for both the dataset so we need any one.
Credit_Application_Data <- subset(Credit_Application_Data, select = -c(Performance.Tag.x))

# Checking structure of Credit Application dataset 
str(Credit_Application_Data)

#NA value analysis in Credit Application dataset
sum(is.na(Credit_Application_Data))    # 3031 NA's 
colSums(is.na(Credit_Application_Data))

sum(is.na(Credit_Application_Data$No_of_dependents))  # 3 NA's 
sum(is.na(Credit_Application_Data$Avgas_CC_Utilization_in_last_12_months)) # 1058 NA's 
sum(is.na(Credit_Application_Data$No_of_trades_opened_in_last_6_months)) # 1 NA's 
sum(is.na(Credit_Application_Data$Presence_of_open_home_loan)) # 272 NA's 
sum(is.na(Credit_Application_Data$Outstanding_Balance)) # 272 NA's 
sum(is.na(Credit_Application_Data$Performance.Tag)) # 1425 NA's 

#Treating NA values in Credit Application dataset.Replacing all the missing value with mean/median whichever is better.
Credit_Application_Data$No_of_dependents <- impute(Credit_Application_Data$No_of_dependents, median)
Credit_Application_Data$Avgas_CC_Utilization_in_last_12_months <- impute(Credit_Application_Data$Avgas_CC_Utilization_in_last_12_months, median)
Credit_Application_Data$No_of_trades_opened_in_last_6_months <- impute(Credit_Application_Data$No_of_trades_opened_in_last_6_months, median)
Credit_Application_Data$Presence_of_open_home_loan <- impute(Credit_Application_Data$Presence_of_open_home_loan, median)
Credit_Application_Data$Outstanding_Balance <- impute(Credit_Application_Data$Outstanding_Balance, mean)

#Standardising numerical data
Credit_Application_Data$No_of_dependents <- as.numeric(Credit_Application_Data$No_of_dependents)
Credit_Application_Data$Avgas_CC_Utilization_in_last_12_months <- as.numeric(Credit_Application_Data$Avgas_CC_Utilization_in_last_12_months)
Credit_Application_Data$No_of_trades_opened_in_last_6_months <- as.numeric(Credit_Application_Data$No_of_trades_opened_in_last_6_months)
Credit_Application_Data$Presence_of_open_home_loan <- as.numeric(Credit_Application_Data$Presence_of_open_home_loan)
Credit_Application_Data$Outstanding_Balance <- as.numeric(Credit_Application_Data$Outstanding_Balance)

#Check if any column is left with NA values.
colSums(is.na(Credit_Application_Data))

# Checking structure of Credit Application dataset 
str(Credit_Application_Data)

#Checking for blank space
sapply(Credit_Application_Data, function(x) length(which(x == ""))) #No blank spaces found

#Missing values in Gender is replaced with "O" which means others (There are limited no of missing value)
Credit_Application_Data$Gender <- ifelse(Credit_Application_Data$Gender =="","O",Credit_Application_Data$Gender)
#Missing value in marital status is replaced with "Others"
Credit_Application_Data$Marital_Status <- ifelse(Credit_Application_Data$Marital_Status =="","Others",Credit_Application_Data$Marital_Status)
#Missing Values in Education variable is replaced with "Others"
Credit_Application_Data$Education <- ifelse(Credit_Application_Data$Education == "","Others",Credit_Application_Data$Education)
#Missing Values in Profession variable is replaced with "Others"
Credit_Application_Data$Profession <- ifelse(Credit_Application_Data$Profession == "","Others",Credit_Application_Data$Profession)
#Missing Values in Type of Residence variable is replaced with "Others"
Credit_Application_Data$Type_of_Residence <- ifelse(Credit_Application_Data$Type_of_Residence == "","Others",Credit_Application_Data$Type_of_Residence)

# Checking structure of Credit Application dataset 
str(Credit_Application_Data)

#converting all the non numeric columns to upper case 
Credit_Application_Data <- data.frame(lapply(Credit_Application_Data, function(v) {
  if (is.character(v)) return(toupper(v))
  else return(v)
}))

str(Credit_Application_Data)
summary(Credit_Application_Data)

#Replacing erroneous values in age column with the mean
Credit_Application_Data$Age <- ifelse(Credit_Application_Data$Age < 18, round(mean(Credit_Application_Data$Age,na.rm=T),0), Credit_Application_Data$Age)
#Negative income values is replaced with mean value of all data
Credit_Application_Data$Income <- ifelse(Credit_Application_Data$Income < 0, round(mean(Credit_Application_Data$Income,na.rm=T),0), Credit_Application_Data$Income)

#Extracting Rejected candidates (Having NULL value for performance tag) from Credit Application dataframe
Rejected_Application_Data <- Credit_Application_Data[is.na(Credit_Application_Data$Performance.Tag),]

#Application dataframe which doesn't have empty value for the Performance tag (Accepted Application)
Accepted_Application_Data <- subset(Credit_Application_Data, Credit_Application_Data$Performance.Tag != "")

length(unique(Accepted_Application_Data$Application.ID)) #69867 rows
summary(Accepted_Application_Data)

#Calculating Response rate
table(Accepted_Application_Data$Performance.Tag)
response <- 2948/(2948+66919)
response  #4.219%


# Replace the Data Frame content with Weight Of Evidence values from Information package
#
# This is an function name 'DF.Replace.WOE', which will extract the values of WOE from Information
# list which can be generated using information package and replace the original values with the
# WOE values.
# @param X is a data sets which is used to create the Information table using information package.
# @param y is the Information table which is created using information package.
# @param Dependent is the dependent binary variable which is used to classify good and bad.
# @return It will return the data frame with WOE values
# @export

DF.Replace.WOE <-
  function(X,
           y,
           Dependent = NULL) {
    z <- 0
    cz <- 0
    D <- X[, Dependent]
    x <- X[,-which(names(X) == Dependent)]
    cn <- names(x)
    if (class(y) == "Information") {
      for (i in 1:ncol(x)) {
        if (class(x[, i]) == "factor") {
          for (j in 1:length(y[[1]][i][[1]][1][[1]])) {
            x[, i] <- as.character(x[, i])
            if (is.na(y[[1]][i][[1]][1][[1]][j])) {
              x[which(is.na(x[, i])), paste(colnames(x)[i], "WOE", sep = ":")] <-
                y[[1]][i][[1]][4][[1]][which(is.na(y[[1]][i][[1]][1][[1]]))]
            }
            else {
              x[which(x[, i] == y[[1]][i][[1]][1][[1]][j]), paste(colnames(x)[i], "WOE", sep = ":")] <-
                y[[1]][i][[1]][4][[1]][j]
            }
          }
        }
        else {
          for (j in 1:length(y[[1]][i][[1]][1][[1]])) {
            cz <-
              as.vector(strsplit(gsub(
                "[]]", "", gsub("[[]", "", y[[1]][i][[1]][1][[1]])
              ), ","))
            if (y[[1]][i][[1]][1][[1]][j] == "NA") {
              x[which(is.na(x[, i])), paste(colnames(x)[i], "WOE", sep = ":")] <-
                y[[1]][i][[1]][4][[1]][which(y[[1]][i][[1]][1][[1]][j] == "NA")]
            }
            else {
              x[which(x[, i] >= as.double(cz[[j]][1]) &
                        x[, i] <= as.double(cz[[j]][2])), paste(colnames(x)[i], "WOE", sep = ":")] <-
                y[[1]][i][[1]][4][[1]][j]
            }
          }
        }
      }
    }
    z <- cbind(x, D)
    colnames(z)[which(names(z) == "D")] <- Dependent
    z <- z[, -which(names(x) == cn)]
    return(z)
  }

#Calulating IV values for the Accepted Credit Application data
IV <- create_infotables(data=Accepted_Application_Data, y="Performance.Tag", bins=10, parallel=TRUE)
IV_Value = data.frame(IV$Summary)

#Now we will create a seperate data sets of WOE values, we will use DF.Replace.WOE function to impute the woe values.
Accepted_Application_Data_woe <- DF.Replace.WOE(Accepted_Application_Data,IV,"Performance.Tag")
Rejected_Application_Data_woe <- DF.Replace.WOE(Rejected_Application_Data,IV,"Performance.Tag")

# Exploratory Data Analysis of Credit Application dataset

#Deriving Insights of Age.

#Checking outliers for Age
quantile(Accepted_Application_Data$Age,seq(0,1,0.01))
boxplot(Accepted_Application_Data$Age)

#Creating age bins
Accepted_Application_Data$BinningAge <- as.factor(cut(Accepted_Application_Data$Age, breaks = c(18, 25, 35, 45, 55, 65)))
agg_age <- merge(aggregate(Performance.Tag ~ BinningAge, Accepted_Application_Data, mean),aggregate(Performance.Tag ~ BinningAge,
                                                                                               Accepted_Application_Data, sum),by = "BinningAge") 
# Adding No.of_prospect
count <- data.frame(table(Accepted_Application_Data$BinningAge))
count <- count[,2]
agg_age <- cbind(agg_age,count)

# changing column name of each variables in Agg_Age dataframe
colnames(agg_age) <- c("Age", "Default_Rate", "No_Of_Default","No_Of_Applicants")

# Round Off the values
agg_age$Default_Rate <- format(round(agg_age$Default_Rate, 4))

# Let's see the default rate of each age bucket in the plot
ggplot(agg_age, aes(Age, No_Of_Applicants,label = Default_Rate)) + 
  geom_bar(stat = 'identity',color="black",fill="lightgreen") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
  geom_text(size = 3, vjust = -0.5) + xlab("Age Bins") +
  ylab("Number of customer in the age bin")

# Removing BinningAge column
Accepted_Application_Data$BinningAge <- NULL

#Deriving Insights of Income

##Checking outliers for Income
quantile(Accepted_Application_Data$Income,seq(0,1,0.01))
boxplot(Accepted_Application_Data$Income)
summary(Accepted_Application_Data$Income)

#Plotting income to derive insights from customer Income
ggplot(Accepted_Application_Data,aes(x=Income)) +geom_bar(color="black",fill="lightgreen")

#Creating Income Bins
Accepted_Application_Data$BinningIncome <- as.factor(cut(Accepted_Application_Data$Income, breaks = c(0, 10, 20, 30, 40, 50, 60),include.lowest = TRUE))

agg_income <- merge(aggregate(Performance.Tag ~ BinningIncome, Accepted_Application_Data, mean),aggregate(Performance.Tag ~ BinningIncome,
                                                                                                     Accepted_Application_Data, sum),by = "BinningIncome") 
# Adding No.of_prospect
count <- data.frame(table(Accepted_Application_Data$BinningIncome))
count <- count[,2]
agg_income <- cbind(agg_income,count)

# changing column name of each variables in agg_income dataframe
colnames(agg_income) <- c("Income", "Default_Rate", "No_Of_Default","No_Of_Applicants")

# Round Off the values
agg_income$Default_Rate <- format(round(agg_income$Default_Rate, 4))

# Let's see the default rate of each age bucket in the plot
ggplot(agg_income, aes(Income, No_Of_Applicants,label = Default_Rate)) + 
  geom_bar(stat = 'identity',color="black",fill="lightgreen") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
  geom_text(size = 3, vjust = -0.5) + xlab("Income Bins") +
  ylab("Number of customer in the Income bin")

# Removing BinningIncome column
Accepted_Application_Data$BinningIncome <- NULL

# Deriving Insights of No of Months in Current Residence

##Checking outliers for No of Months in Current Residence
quantile(Accepted_Application_Data$No_of_months_in_current_residence,seq(0,1,0.01))
boxplot(Accepted_Application_Data$No_of_months_in_current_residence)
summary(Accepted_Application_Data$No_of_months_in_current_residence)

#Plotting to derive insights from No.of.months.in.current.residence Variable
ggplot(Accepted_Application_Data,aes(x=No_of_months_in_current_residence)) +geom_bar(color="black",fill="lightgreen")

#Creating Bins for No.of.months.in.current.residence Variable
Accepted_Application_Data$BinningCurentResidence <- as.factor(cut(Accepted_Application_Data$No_of_months_in_current_residence, breaks = c(0, 20, 40, 60, 80, 100, 130)))
agg_current_residence <- merge(aggregate(Performance.Tag ~ BinningCurentResidence, Accepted_Application_Data, mean),aggregate(Performance.Tag ~ BinningCurentResidence,
                                                                                                                        Accepted_Application_Data, sum),by = "BinningCurentResidence") 
# Adding No.of_prospect
count <- data.frame(table(Accepted_Application_Data$BinningCurentResidence))
count <- count[,2]
agg_current_residence <- cbind(agg_current_residence,count)

# changing column name of each variables in agg_current_residence dataframe
colnames(agg_current_residence) <- c("Current_Residence", "Default_Rate", "No_Of_Default","No_Of_Applicants")

# Round Off the values
agg_current_residence$Default_Rate <- format(round(agg_current_residence$Default_Rate, 4))

# Let's see the default rate of each age bucket in the plot
ggplot(agg_current_residence, aes(Current_Residence, No_Of_Applicants,label = Default_Rate)) + 
  geom_bar(stat = 'identity',color="black",fill="lightgreen") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
  geom_text(size = 3, vjust = -0.5) + xlab("Current_Residence Bins") +
  ylab("Number of customer in the Current_Residence bin")

#Deriving Insights for No of Months in Current Company

##Checking outliers for No of Months in Current Company
quantile(Accepted_Application_Data$No_of_months_in_current_company,seq(0,1,0.01))
boxplot(Accepted_Application_Data$No_of_months_in_current_company)
summary(Accepted_Application_Data$No_of_months_in_current_company)

# Capping the Upper values of Current Months in company with 75
Accepted_Application_Data[(which(Accepted_Application_Data$No_of_months_in_current_company > 75)),]$No_of_months_in_current_company <- 75

#Plotting to derive insights from No.of.months.in.current.company Variable
ggplot(Accepted_Application_Data,aes(x=No_of_months_in_current_company)) +geom_bar(color="black",fill="lightgreen")

#Creating Bins for No.of.months.in.current.company Variable
Accepted_Application_Data$BinningCurrentCompany <- as.factor(cut(Accepted_Application_Data$No_of_months_in_current_company, breaks = c(0, 15, 30, 45, 60, 75)))
agg_CurrentCompany <- merge(aggregate(Performance.Tag ~ BinningCurrentCompany, Accepted_Application_Data, mean),aggregate(Performance.Tag ~ BinningCurrentCompany,
                                                                                                                     Accepted_Application_Data, sum),by = "BinningCurrentCompany") 
# Adding No.of_prospect
count <- data.frame(table(Accepted_Application_Data$BinningCurrentCompany))
count <- count[,2]
agg_CurrentCompany <- cbind(agg_CurrentCompany,count)

# changing column name of each variables in agg_CurrentCompany dataframe
colnames(agg_CurrentCompany) <- c("Current_Company", "Default_Rate", "No_Of_Default","No_Of_Applicants")

# Round Off the values
agg_CurrentCompany$Default_Rate <- format(round(agg_CurrentCompany$Default_Rate, 4))

# Let's see the default rate of each age bucket in the plot
ggplot(agg_CurrentCompany, aes(Current_Company, No_Of_Applicants,label = Default_Rate)) + 
  geom_bar(stat = 'identity',color="black",fill="lightgreen") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
  geom_text(size = 3, vjust = -0.5) + xlab("Current_Company Bins") +
  ylab("Number of customer in the Current_Company bin")

Accepted_Application_Data$BinningCurrentCompany <- NULL

#Deriving Insights of Gender Variable

Accepted_Application_Data$Gender <- as.factor(Accepted_Application_Data$Gender)

#Plotting to derive insights from gender Variable
ggplot(Accepted_Application_Data, aes(x=Gender)) + geom_bar(color="black", fill="lightgreen")

agg_Gender <- merge(aggregate(Performance.Tag ~ Gender, Accepted_Application_Data, mean),aggregate(Performance.Tag ~ Gender,
                                                                                                       Accepted_Application_Data, sum),by = "Gender") 
# Adding No.of_prospect
count <- data.frame(table(Accepted_Application_Data$Gender))
count <- count[,2]
agg_Gender <- cbind(agg_Gender,count)

# changing column name of each variables in agg_CurrentCompany dataframe
colnames(agg_Gender) <- c("Gender", "Default_Rate", "No_Of_Default","No_Of_Applicants")

# Round Off the values
agg_Gender$Default_Rate <- format(round(agg_Gender$Default_Rate, 4))

# Let's see the default rate of each age bucket in the plot
ggplot(agg_Gender, aes(Gender, No_Of_Applicants,label = Default_Rate)) + 
  geom_bar(stat = 'identity',color="black",fill="lightgreen") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
  geom_text(size = 3, vjust = -0.5) + xlab("Gender") +
  ylab("Number of customer in the Gender")

#Deriving Insights of Marital Status

Accepted_Application_Data$Marital_Status <- as.factor(Accepted_Application_Data$Marital_Status)

#Plotting to derive insights from Marital status Variable
ggplot(Accepted_Application_Data,aes(x=Marital_Status)) +geom_bar(color="black",fill="lightgreen")

agg_Marital_Status <- merge(aggregate(Performance.Tag ~ Marital_Status, Accepted_Application_Data, mean),aggregate(Performance.Tag ~ Marital_Status,
                                                                                                                       Accepted_Application_Data, sum),by = "Marital_Status") 
# Adding No.of_prospect
count <- data.frame(table(Accepted_Application_Data$Marital_Status))
count <- count[,2]
agg_Marital_Status <- cbind(agg_Marital_Status,count)

# changing column name of each variables in agg_Marital_Status dataframe
colnames(agg_Marital_Status) <- c("Marital_Status", "Default_Rate", "No_Of_Default","No_Of_Applicants")

# Round Off the values
agg_Marital_Status$Default_Rate <- format(round(agg_Marital_Status$Default_Rate, 4))

# Let's see the default rate of each Marital_Status in the plot
ggplot(agg_Marital_Status, aes(Marital_Status, No_Of_Applicants,label = Default_Rate)) + 
  geom_bar(stat = 'identity',color="black",fill="lightgreen") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
  geom_text(size = 3, vjust = -0.5) + xlab("Marital_Status") +
  ylab("Number of customer in the Marital_Status")

#Deriving Insights of No_of_dependents

#Converting into factor Variable
Accepted_Application_Data$No_of_dependents  <- as.factor(Accepted_Application_Data$No_of_dependents)

#Plotting to derive insights from No_of_dependents
ggplot(Accepted_Application_Data,aes(x=No_of_dependents)) +geom_bar(color="black",fill="lightgreen")

agg_No_of_dependents <- merge(aggregate(Performance.Tag ~ No_of_dependents, Accepted_Application_Data, mean),aggregate(Performance.Tag ~ No_of_dependents,
                                                                                                                                Accepted_Application_Data, sum),by = "No_of_dependents") 
# Adding No.of_prospect
count <- data.frame(table(Accepted_Application_Data$No_of_dependents))
count <- count[,2]
agg_No_of_dependents <- cbind(agg_No_of_dependents,count)

# changing column name of each variables in agg_No_of_dependents dataframe
colnames(agg_No_of_dependents) <- c("No_of_dependents", "Default_Rate", "No_Of_Default","No_Of_Applicants")

# Round Off the values
agg_No_of_dependents$Default_Rate <- format(round(agg_No_of_dependents$Default_Rate, 4))

# Let's see the default rate of each age bucket in the plot
ggplot(agg_No_of_dependents, aes(No_of_dependents, No_Of_Applicants,label = Default_Rate)) + 
  geom_bar(stat = 'identity',color="black",fill="lightgreen") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
  geom_text(size = 3, vjust = -0.5) + xlab("No_of_dependents") +
  ylab("Number of customer in No_of_dependents")

#Deriving Insights of Education

#Converting into factor Variable
Accepted_Application_Data$Education  <- as.factor(Accepted_Application_Data$Education)

#Plotting to derive insights from Education Variable
ggplot(Accepted_Application_Data,aes(x=Education)) +geom_bar(color="black",fill="lightgreen")

agg_Education <- merge(aggregate(Performance.Tag ~ Education, Accepted_Application_Data, mean),aggregate(Performance.Tag ~ Education,
                                                                                                                  Accepted_Application_Data, sum),by = "Education")
# Adding No.of_prospect
count <- data.frame(table(Accepted_Application_Data$Education))
count <- count[,2]
agg_Education <- cbind(agg_Education,count)

# changing column name of each variables in agg_Education dataframe
colnames(agg_Education) <- c("Education", "Default_Rate", "No_Of_Default","No_Of_Applicants")

# Round Off the values
agg_Education$Default_Rate <- format(round(agg_Education$Default_Rate, 4))

# Let's see the default rate of each age bucket in the plot
ggplot(agg_Education, aes(Education, No_Of_Applicants,label = Default_Rate)) + 
  geom_bar(stat = 'identity',color="black",fill="lightgreen") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
  geom_text(size = 3, vjust = -0.5) + xlab("Education") +
  ylab("Number of customer in Education")

#Deriving Insights of Profession

#Converting into factor Variable
Accepted_Application_Data$Profession  <- as.factor(Accepted_Application_Data$Profession)

#Plotting to derive insights from Profession Variable
ggplot(Accepted_Application_Data,aes(x=Profession)) +geom_bar(color="black",fill="lightgreen")

agg_Profession <- merge(aggregate(Performance.Tag ~ Profession, Accepted_Application_Data, mean),aggregate(Performance.Tag ~ Profession,
                                                                                                               Accepted_Application_Data, sum),by = "Profession") 
# Adding No.of_prospect
count <- data.frame(table(Accepted_Application_Data$Profession))
count <- count[,2]
agg_Profession <- cbind(agg_Profession,count)

# changing column name of each variables in agg_Profession dataframe
colnames(agg_Profession) <- c("Profession", "Default_Rate", "No_Of_Default","No_Of_Applicants")

# Round Off the values
agg_Profession$Default_Rate <- format(round(agg_Profession$Default_Rate, 4))

# Let's see the default rate of each Profession bucket in the plot
ggplot(agg_Profession, aes(Profession, No_Of_Applicants,label = Default_Rate)) + 
  geom_bar(stat = 'identity',color="black",fill="lightgreen") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
  geom_text(size = 3, vjust = -0.5) + xlab("Profession") +
  ylab("Number of customer in Profession")

#Deriving Insights of Type of Residence

#Converting into factor Variable
Accepted_Application_Data$Type_of_Residence  <- as.factor(Accepted_Application_Data$Type_of_Residence)

#Plotting to derive insights from Residence Variable
ggplot(Accepted_Application_Data,aes(x=Type_of_Residence)) +geom_bar(color="black",fill="lightgreen")

agg_Type_of_Residence <- merge(aggregate(Performance.Tag ~ Type_of_Residence, Accepted_Application_Data, mean),aggregate(Performance.Tag ~ Type_of_Residence,
                                                                                                                                  Accepted_Application_Data, sum),by = "Type_of_Residence") 
# Adding No.of_prospect
count <- data.frame(table(Accepted_Application_Data$Type_of_Residence))
count <- count[,2]
agg_Type_of_Residence <- cbind(agg_Type_of_Residence,count)

# changing column name of each variables in agg_Type_of_Residence dataframe
colnames(agg_Type_of_Residence) <- c("Type_of_Residence", "Default_Rate", "No_Of_Default","No_Of_Applicants")

# Round Off the values
agg_Type_of_Residence$Default_Rate <- format(round(agg_Type_of_Residence$Default_Rate, 4))

# Let's see the default rate of each Type_of_Residence bucket in the plot
ggplot(agg_Type_of_Residence, aes(Type_of_Residence, No_Of_Applicants,label = Default_Rate)) + 
  geom_bar(stat = 'identity',color="black",fill="lightgreen") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
  geom_text(size = 3, vjust = -0.5) + xlab("Type_of_Residence") +
  ylab("Number of customer in Type_of_Residence")

#Deriving Insights of Type of No_of_times_90_DPD_or_worse_in_last_6_months

#Converting into factor Variable
Accepted_Application_Data$No_of_times_90_DPD_or_worse_in_last_6_months  <- as.factor(Accepted_Application_Data$No_of_times_90_DPD_or_worse_in_last_6_months)

#Plotting to derive insights from No_of_times_90_DPD_or_worse_in_last_6_months Variable
ggplot(Accepted_Application_Data,aes(x=No_of_times_90_DPD_or_worse_in_last_6_months)) +geom_bar(color="black",fill="lightgreen")

agg_90DPD_last_6Month <- merge(aggregate(Performance.Tag ~ No_of_times_90_DPD_or_worse_in_last_6_months, Accepted_Application_Data, mean),aggregate(Performance.Tag ~ No_of_times_90_DPD_or_worse_in_last_6_months,
                                                                                                                                                    Accepted_Application_Data, sum),by = "No_of_times_90_DPD_or_worse_in_last_6_months") 
# Adding No.of_prospect
count <- data.frame(table(Accepted_Application_Data$No_of_times_90_DPD_or_worse_in_last_6_months))
count <- count[,2]
agg_90DPD_last_6Month <- cbind(agg_90DPD_last_6Month,count)

# changing column name of each variables in agg_No_of_times_90_DPD_or_worse_in_last_6_months dataframe
colnames(agg_90DPD_last_6Month) <- c("No_of_times_90_DPD_or_worse_in_last_6_months", "Default_Rate", "No_Of_Default","No_Of_Applicants")

# Round Off the values
agg_90DPD_last_6Month$Default_Rate <- format(round(agg_90DPD_last_6Month$Default_Rate, 4))

# Let's see the default rate of each age bucket in the plot
ggplot(agg_90DPD_last_6Month, aes(No_of_times_90_DPD_or_worse_in_last_6_months, No_Of_Applicants,label = Default_Rate)) + 
  geom_bar(stat = 'identity',color="black",fill="lightgreen") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
  geom_text(size = 3, vjust = -0.5) + xlab("No_of_times_90_DPD_or_worse_in_last_6_months") +
  ylab("Number of customer in 90DPD_last_6Month")

#Deriving Insights of Type of No_of_times_60_DPD_or_worse_in_last_6_months

#Converting into factor Variable
Accepted_Application_Data$No_of_times_60_DPD_or_worse_in_last_6_months  <- as.factor(Accepted_Application_Data$No_of_times_60_DPD_or_worse_in_last_6_months)

#Plotting to derive insights from No_of_times_60_DPD_or_worse_in_last_6_months Variable
ggplot(Accepted_Application_Data,aes(x=No_of_times_60_DPD_or_worse_in_last_6_months)) +geom_bar(color="black",fill="lightgreen")

agg_60DPD_last_6Month <- merge(aggregate(Performance.Tag ~ No_of_times_60_DPD_or_worse_in_last_6_months, Accepted_Application_Data, mean),aggregate(Performance.Tag ~ No_of_times_60_DPD_or_worse_in_last_6_months,
                                                                                                                                                    Accepted_Application_Data, sum),by = "No_of_times_60_DPD_or_worse_in_last_6_months") 
# Adding No.of_prospect
count <- data.frame(table(Accepted_Application_Data$No_of_times_60_DPD_or_worse_in_last_6_months))
count <- count[,2]
agg_60DPD_last_6Month <- cbind(agg_60DPD_last_6Month,count)

# changing column name of each variables in agg_60DPD_last_6Month dataframe
colnames(agg_60DPD_last_6Month) <- c("No_of_times_60_DPD_or_worse_in_last_6_months", "Default_Rate", "No_Of_Default","No_Of_Applicants")

# Round Off the values
agg_60DPD_last_6Month$Default_Rate <- format(round(agg_60DPD_last_6Month$Default_Rate, 4))

# Let's see the default rate of each age bucket in the plot
ggplot(agg_60DPD_last_6Month, aes(No_of_times_60_DPD_or_worse_in_last_6_months, No_Of_Applicants,label = Default_Rate)) + 
  geom_bar(stat = 'identity',color="black",fill="lightgreen") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
  geom_text(size = 3, vjust = -0.5) + xlab("No_of_times_60_DPD_or_worse_in_last_6_months") +
  ylab("Number of customer in 60DPD_last_6Month")

#Deriving Insights of Type of No_of_times_30_DPD_or_worse_in_last_6_months

#Converting into factor Variable
Accepted_Application_Data$No_of_times_30_DPD_or_worse_in_last_6_months  <- as.factor(Accepted_Application_Data$No_of_times_30_DPD_or_worse_in_last_6_months)

#Plotting to derive insights from No_of_times_30_DPD_or_worse_in_last_6_months Variable
ggplot(Accepted_Application_Data,aes(x=No_of_times_30_DPD_or_worse_in_last_6_months)) +geom_bar(color="black",fill="lightgreen")

agg_30DPD_last_6Month <- merge(aggregate(Performance.Tag ~ No_of_times_30_DPD_or_worse_in_last_6_months, Accepted_Application_Data, mean),aggregate(Performance.Tag ~ No_of_times_30_DPD_or_worse_in_last_6_months,
                                                                                                                                                    Accepted_Application_Data, sum),by = "No_of_times_30_DPD_or_worse_in_last_6_months") 
# Adding No.of_prospect
count <- data.frame(table(Accepted_Application_Data$No_of_times_30_DPD_or_worse_in_last_6_months))
count <- count[,2]
agg_30DPD_last_6Month <- cbind(agg_30DPD_last_6Month,count)

# changing column name of each variables in agg_30DPD_last_6Month dataframe
colnames(agg_30DPD_last_6Month) <- c("No_of_times_30_DPD_or_worse_in_last_6_months", "Default_Rate", "No_Of_Default","No_Of_Applicants")

# Round Off the values
agg_30DPD_last_6Month$Default_Rate <- format(round(agg_30DPD_last_6Month$Default_Rate, 4))

# Let's see the default rate of each age bucket in the plot
ggplot(agg_30DPD_last_6Month, aes(No_of_times_30_DPD_or_worse_in_last_6_months, No_Of_Applicants,label = Default_Rate)) + 
  geom_bar(stat = 'identity',color="black",fill="lightgreen") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
  geom_text(size = 3, vjust = -0.5) + xlab("No_of_times_30_DPD_or_worse_in_last_6_months") +
  ylab("Number of customer in 30DPD_last_6Month")

#Deriving Insights of Type of No_of_times_90_DPD_or_worse_in_last_12_months

#Converting into factor Variable
Accepted_Application_Data$No_of_times_90_DPD_or_worse_in_last_12_months  <- as.factor(Accepted_Application_Data$No_of_times_90_DPD_or_worse_in_last_12_months)

#Plotting to derive insights from No_of_times_90_DPD_or_worse_in_last_12_months Variable
ggplot(Accepted_Application_Data,aes(x=No_of_times_90_DPD_or_worse_in_last_12_months)) +geom_bar(color="black",fill="lightgreen")

agg_90DPD_last_12Month <- merge(aggregate(Performance.Tag ~ No_of_times_90_DPD_or_worse_in_last_12_months, Accepted_Application_Data, mean),aggregate(Performance.Tag ~ No_of_times_90_DPD_or_worse_in_last_12_months,
                                                                                                                                                    Accepted_Application_Data, sum),by = "No_of_times_90_DPD_or_worse_in_last_12_months") 
# Adding No.of_prospect
count <- data.frame(table(Accepted_Application_Data$No_of_times_90_DPD_or_worse_in_last_12_months))
count <- count[,2]
agg_90DPD_last_12Month <- cbind(agg_90DPD_last_12Month,count)

# changing column name of each variables in agg_90DPD_last_12Month dataframe
colnames(agg_90DPD_last_12Month) <- c("No_of_times_90_DPD_or_worse_in_last_12_months", "Default_Rate", "No_Of_Default","No_Of_Applicants")

# Round Off the values
agg_90DPD_last_12Month$Default_Rate <- format(round(agg_90DPD_last_12Month$Default_Rate, 4))

# Let's see the default rate of each age bucket in the plot
ggplot(agg_90DPD_last_12Month, aes(No_of_times_90_DPD_or_worse_in_last_12_months, No_Of_Applicants,label = Default_Rate)) + 
  geom_bar(stat = 'identity',color="black",fill="lightgreen") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
  geom_text(size = 3, vjust = -0.5) + xlab("No_of_times_90_DPD_or_worse_in_last_12_months") +
  ylab("Number of customer in 90DPD_last_12Month")

#Deriving Insights of Type of No_of_times_60_DPD_or_worse_in_last_12_months

#Converting into factor Variable
Accepted_Application_Data$No_of_times_60_DPD_or_worse_in_last_12_months  <- as.factor(Accepted_Application_Data$No_of_times_60_DPD_or_worse_in_last_12_months)

#Plotting to derive insights from No_of_times_60_DPD_or_worse_in_last_12_months Variable
ggplot(Accepted_Application_Data,aes(x=No_of_times_60_DPD_or_worse_in_last_12_months)) +geom_bar(color="black",fill="lightgreen")

agg_60DPD_last_12Month <- merge(aggregate(Performance.Tag ~ No_of_times_60_DPD_or_worse_in_last_12_months, Accepted_Application_Data, mean),aggregate(Performance.Tag ~ No_of_times_60_DPD_or_worse_in_last_12_months,
                                                                                                                                                    Accepted_Application_Data, sum),by = "No_of_times_60_DPD_or_worse_in_last_12_months") 
# Adding No.of_prospect
count <- data.frame(table(Accepted_Application_Data$No_of_times_60_DPD_or_worse_in_last_12_months))
count <- count[,2]
agg_60DPD_last_12Month <- cbind(agg_60DPD_last_12Month,count)

# changing column name of each variables in agg_60DPD_last_12Month dataframe
colnames(agg_60DPD_last_12Month) <- c("No_of_times_60_DPD_or_worse_in_last_12_months", "Default_Rate", "No_Of_Default","No_Of_Applicants")

# Round Off the values
agg_60DPD_last_12Month$Default_Rate <- format(round(agg_60DPD_last_12Month$Default_Rate, 4))

# Let's see the default rate of each age bucket in the plot
ggplot(agg_60DPD_last_12Month, aes(No_of_times_60_DPD_or_worse_in_last_12_months, No_Of_Applicants,label = Default_Rate)) + 
  geom_bar(stat = 'identity',color="black",fill="lightgreen") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
  geom_text(size = 3, vjust = -0.5) + xlab("No_of_times_60_DPD_or_worse_in_last_12_months") +
  ylab("Number of customer in 60DPD_last_12Month")

#Deriving Insights of Type of No_of_times_30_DPD_or_worse_in_last_12_months

#Converting into factor Variable
Accepted_Application_Data$No_of_times_30_DPD_or_worse_in_last_12_months  <- as.factor(Accepted_Application_Data$No_of_times_30_DPD_or_worse_in_last_12_months)

#Plotting to derive insights from No_of_times_30_DPD_or_worse_in_last_12_months Variable
ggplot(Accepted_Application_Data,aes(x=No_of_times_30_DPD_or_worse_in_last_12_months)) +geom_bar(color="black",fill="lightgreen")

agg_30DPD_last_12Month <- merge(aggregate(Performance.Tag ~ No_of_times_30_DPD_or_worse_in_last_12_months, Accepted_Application_Data, mean),aggregate(Performance.Tag ~ No_of_times_30_DPD_or_worse_in_last_12_months,
                                                                                                                                                    Accepted_Application_Data, sum),by = "No_of_times_30_DPD_or_worse_in_last_12_months") 
# Adding No.of_prospect
count <- data.frame(table(Accepted_Application_Data$No_of_times_30_DPD_or_worse_in_last_12_months))
count <- count[,2]
agg_30DPD_last_12Month <- cbind(agg_30DPD_last_12Month,count)

# changing column name of each variables in agg_30DPD_last_12Month dataframe
colnames(agg_30DPD_last_12Month) <- c("No_of_times_30_DPD_or_worse_in_last_12_months", "Default_Rate", "No_Of_Default","No_Of_Applicants")

# Round Off the values
agg_30DPD_last_12Month$Default_Rate <- format(round(agg_30DPD_last_12Month$Default_Rate, 4))

# Let's see the default rate of each age bucket in the plot
ggplot(agg_30DPD_last_12Month, aes(No_of_times_30_DPD_or_worse_in_last_12_months, No_Of_Applicants,label = Default_Rate)) + 
  geom_bar(stat = 'identity',color="black",fill="lightgreen") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
  geom_text(size = 3, vjust = -0.5) + xlab("No_of_times_30_DPD_or_worse_in_last_12_months") +
  ylab("Number of customer in 30DPD_last_12Month")

# Deriving insights for aVG cREDIT cARD Utilization in the last 12 months

# Checking NA values in Avgas_CC_Utilization_in_last_12_months

#Binning of values for Modified CC ultilization
Accepted_Application_Data$Binned_CC_Utilization <- as.factor(cut(Accepted_Application_Data$Avgas_CC_Utilization_in_last_12_months, breaks = c(0,10, 20, 30, 40, 50, 60,70,80,90,100,110,120),include.lowest = TRUE))

#Plotting to derive insights from Avgas_CC_Utilization_in_last_12_months
ggplot(Accepted_Application_Data,aes(x=Avgas_CC_Utilization_in_last_12_months)) +geom_bar(color="black",fill="lightgreen")

agg_CC_utilization <- merge(aggregate(Performance.Tag ~ Binned_CC_Utilization, Accepted_Application_Data, mean),aggregate(Performance.Tag ~ Binned_CC_Utilization,
                                                                                                                          Accepted_Application_Data, sum),by = "Binned_CC_Utilization")
# Adding No.of_prospect
count <- data.frame(table(Accepted_Application_Data$Binned_CC_Utilization))
count <- count[,2]
agg_CC_utilization <- cbind(agg_CC_utilization,count)

# changing column name of each variables in Binned_CC_Utilization dataframe
colnames(agg_CC_utilization) <- c("Binned_CC_Utilization", "Default_Rate", "No_Of_Default","No_Of_Applicants")

# Round Off the values
agg_CC_utilization$Default_Rate <- format(round(agg_CC_utilization$Default_Rate, 4))

# Let's see the default rate of each Avgas_CC_Utilization_in_last_12_months bucket in the plot
ggplot(agg_CC_utilization, aes(Binned_CC_Utilization, No_Of_Applicants,label = Default_Rate)) + 
  geom_bar(stat = 'identity',color="black",fill="lightgreen") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
  geom_text(size = 3, vjust = -0.5) + xlab("Binned_CC_Utilization") +
  ylab("Number of customer in Binned_CC_Utilization")

#Removing Binned_CC_Utilization
Accepted_Application_Data$Binned_CC_Utilization <- NULL

#####                    multivariate analysis                  #####

#agg_CC_utilization, No_of_trades_opened_in_last_6_months and Performance.Tag
ggplot(Accepted_Application_Data,aes(x = Avgas_CC_Utilization_in_last_12_months, y = No_of_trades_opened_in_last_6_months, col = Performance.Tag)) +
  geom_point(alpha=0.5, position = "jitter")

#####       Correlation Matrix         #####

#Removing Application.ID as it is not usefull for correlation matrix
Corr_Mat_Data <- subset(Accepted_Application_Data, select = -c(Application.ID))

#Creating a data frame of all the numerical variables
Corr_Mat_Data <- Corr_Mat_Data[sapply(Corr_Mat_Data,is.numeric)]

#Creating correlation matrix
corr_matrix <- round(cor(Corr_Mat_Data),2)

#Creating lower triangle correlation matrix
get_lower_tri_corr_matrix <-function(corr_matrix){
  corr_matrix[upper.tri(corr_matrix)] <- NA
  return(corr_matrix)
}

#Creating upper triangle correlation matrix
get_upper_tri_corr_matrix <- function(corr_matrix){
  corr_matrix[lower.tri(corr_matrix)]<- NA
  return(corr_matrix)
}

# Use correlation between variables as distance
reorder_corr_matrix <- function(corr_matrix){
  dd <- as.dist((1 - corr_matrix)/2)
  hc <- hclust(dd)
  corr_matrix <- corr_matrix[hc$order, hc$order]
}

# Reorder the correlation matrix
corr_matrix <- reorder_corr_matrix(corr_matrix)

#Upper triangle correlation matrix
upper_tri_corr_matrix <- get_upper_tri_corr_matrix(corr_matrix)

# Melt the correlation matrix
melted_corr_matrix <- melt(upper_tri_corr_matrix, na.rm = TRUE)

# Create a ggheatmap
ggheatmap <- ggplot(melted_corr_matrix, aes(Var2, Var1, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "green", mid = "white", midpoint = 0, limit = c(-1,1), space = "Lab", name="Leading Credit Application Parameters Correlation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 12, hjust = 1)) +
  coord_fixed()

ggheatmap + geom_text(aes(Var2, Var1, label = value), color = "black", size = 2) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.justification = c(1, 0),
    legend.position = c(0.6, 0.7),
    legend.direction = "horizontal") +
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1, title.position = "top", title.hjust = 0.5))

# Checking structure of Credit Application dataset 
str(Credit_Application_Data)
summary(Credit_Application_Data)

#Dummy variable creation of all the categorical variable

#Dummy variable for Gender
dummy <- data.frame(model.matrix(~Gender, data = Credit_Application_Data))
Credit_Application_Data <- cbind(subset(Credit_Application_Data, select = -c(Gender)), dummy[,-1])

#Dummy variable for Marital_Status
dummy <- data.frame(model.matrix(~Marital_Status, data = Credit_Application_Data))
Credit_Application_Data <- cbind(subset(Credit_Application_Data, select = -c(Marital_Status)), dummy[,-1])

#Dummy variable for No_of_dependents
Credit_Application_Data$No_of_dependents <- factor(Credit_Application_Data$No_of_dependents)
dummy <- data.frame(model.matrix(~No_of_dependents, data = Credit_Application_Data))
Credit_Application_Data <- cbind(subset(Credit_Application_Data, select = -c(No_of_dependents)), dummy[,-1])

#Dummy variable for Education
dummy <- data.frame(model.matrix(~Education, data = Credit_Application_Data))
Credit_Application_Data <- cbind(subset(Credit_Application_Data, select = -c(Education)), dummy[,-1])

#Dummy variable for Profession
dummy <- data.frame(model.matrix(~Profession, data = Credit_Application_Data))
Credit_Application_Data <- cbind(subset(Credit_Application_Data, select = -c(Profession)), dummy[,-1])

#Dummy variable for Type_of_Residence
dummy <- data.frame(model.matrix(~Type_of_Residence, data = Credit_Application_Data))
Credit_Application_Data <- cbind(subset(Credit_Application_Data, select = -c(Type_of_Residence)), dummy[,-1])

#Dummy variable for No_of_times_90_DPD_or_worse_in_last_6_months
Credit_Application_Data$No_of_times_90_DPD_or_worse_in_last_6_months <- factor(Credit_Application_Data$No_of_times_90_DPD_or_worse_in_last_6_months)
dummy <- data.frame(model.matrix(~No_of_times_90_DPD_or_worse_in_last_6_months, data = Credit_Application_Data))
Credit_Application_Data <- cbind(subset(Credit_Application_Data, select = -c(No_of_times_90_DPD_or_worse_in_last_6_months)), dummy[,-1])

#Dummy variable for No_of_times_60_DPD_or_worse_in_last_6_months
Credit_Application_Data$No_of_times_60_DPD_or_worse_in_last_6_months <- factor(Credit_Application_Data$No_of_times_60_DPD_or_worse_in_last_6_months)
dummy <- data.frame(model.matrix(~No_of_times_60_DPD_or_worse_in_last_6_months, data = Credit_Application_Data))
Credit_Application_Data <- cbind(subset(Credit_Application_Data, select = -c(No_of_times_60_DPD_or_worse_in_last_6_months)), dummy[,-1])

#Dummy variable for No_of_times_30_DPD_or_worse_in_last_6_months
Credit_Application_Data$No_of_times_30_DPD_or_worse_in_last_6_months <- factor(Credit_Application_Data$No_of_times_30_DPD_or_worse_in_last_6_months)
dummy <- data.frame(model.matrix(~No_of_times_30_DPD_or_worse_in_last_6_months, data = Credit_Application_Data))
Credit_Application_Data <- cbind(subset(Credit_Application_Data, select = -c(No_of_times_30_DPD_or_worse_in_last_6_months)), dummy[,-1])

#Dummy variable for No_of_times_90_DPD_or_worse_in_last_12_months
Credit_Application_Data$No_of_times_90_DPD_or_worse_in_last_12_months <- factor(Credit_Application_Data$No_of_times_90_DPD_or_worse_in_last_12_months)
dummy <- data.frame(model.matrix(~No_of_times_90_DPD_or_worse_in_last_12_months, data = Credit_Application_Data))
Credit_Application_Data <- cbind(subset(Credit_Application_Data, select = -c(No_of_times_90_DPD_or_worse_in_last_12_months)), dummy[,-1])

#Dummy variable for No_of_times_60_DPD_or_worse_in_last_12_months
Credit_Application_Data$No_of_times_60_DPD_or_worse_in_last_12_months <- factor(Credit_Application_Data$No_of_times_60_DPD_or_worse_in_last_12_months)
dummy <- data.frame(model.matrix(~No_of_times_60_DPD_or_worse_in_last_12_months, data = Credit_Application_Data))
Credit_Application_Data <- cbind(subset(Credit_Application_Data, select = -c(No_of_times_60_DPD_or_worse_in_last_12_months)), dummy[,-1])

#Dummy variable for No_of_times_30_DPD_or_worse_in_last_12_months
Credit_Application_Data$No_of_times_30_DPD_or_worse_in_last_12_months <- factor(Credit_Application_Data$No_of_times_30_DPD_or_worse_in_last_12_months)
dummy <- data.frame(model.matrix(~No_of_times_30_DPD_or_worse_in_last_12_months, data = Credit_Application_Data))
Credit_Application_Data <- cbind(subset(Credit_Application_Data, select = -c(No_of_times_30_DPD_or_worse_in_last_12_months)), dummy[,-1])

# Checking structure of Credit Application dataset 
str(Credit_Application_Data)

#Scaling all the numeric predictors
Credit_Application_Data$Age <- scale(Credit_Application_Data$Age)
Credit_Application_Data$Income <- scale(Credit_Application_Data$Income)
Credit_Application_Data$No_of_months_in_current_residence <- scale(Credit_Application_Data$No_of_months_in_current_residence)
Credit_Application_Data$No_of_months_in_current_company <- scale(Credit_Application_Data$No_of_months_in_current_company)
Credit_Application_Data$Avgas_CC_Utilization_in_last_12_months <- scale(Credit_Application_Data$Avgas_CC_Utilization_in_last_12_months)
Credit_Application_Data$No_of_trades_opened_in_last_6_months <- scale(Credit_Application_Data$No_of_trades_opened_in_last_6_months)
Credit_Application_Data$No_of_trades_opened_in_last_12_months <- scale(Credit_Application_Data$No_of_trades_opened_in_last_12_months)
Credit_Application_Data$No_of_PL_trades_opened_in_last_6_months <- scale(Credit_Application_Data$No_of_PL_trades_opened_in_last_6_months)
Credit_Application_Data$No_of_PL_trades_opened_in_last_12_months <- scale(Credit_Application_Data$No_of_PL_trades_opened_in_last_12_months)
Credit_Application_Data$No_of_Inquiries_in_last_6_months_excluding_home_auto_loans <- scale(Credit_Application_Data$No_of_Inquiries_in_last_6_months_excluding_home_auto_loans)
Credit_Application_Data$No_of_Inquiries_in_last_12_months_excluding_home_auto_loans <- scale(Credit_Application_Data$No_of_Inquiries_in_last_12_months_excluding_home_auto_loans)
Credit_Application_Data$Outstanding_Balance <- scale(Credit_Application_Data$Outstanding_Balance)
Credit_Application_Data$Total_No_of_Trades <- scale(Credit_Application_Data$Total_No_of_Trades)

#Extracting Rejected candidates (Having NULL value for performance tag) from Credit Application dataframe
Rejected_Application_Data <- Credit_Application_Data[is.na(Credit_Application_Data$Performance.Tag),]

#Application dataframe which doesn't have empty value for the Performance tag (Accepted Application)
Accepted_Application_Data <- subset(Credit_Application_Data, Credit_Application_Data$Performance.Tag != "")

#Flipping the value of performance tag so that it can used to create IV of variables
Accepted_Application_Data$Performance.Tag <- ifelse(Accepted_Application_Data$Performance.Tag == 0,1,0)

# WOE Calculation for Accepted Credit Application data
str(Accepted_Application_Data)

#Calulating IV values for the Credit Application data
IV <- create_infotables(data=Accepted_Application_Data, y="Performance.Tag", bins=10, parallel=TRUE)
IV_Value = data.frame(IV$Summary)

#Top 10 Predictors are as follows :

#Avgas_CC_Utilization_in_last_12_months	                      0.3070
#No_of_trades_opened_in_last_12_months	                      0.2980
#No_of_PL_trades_opened_in_last_12_months	                    0.2961
#No_of_Inquiries_in_last_12_months_excluding_home_auto_loans	0.2955
#Outstanding_Balance	                                        0.2432
#Total_No_of_Trades	                                          0.2366
#No_of_PL_trades_opened_in_last_6_months	                    0.2198
#No_of_Inquiries_in_last_6_months_excluding_home_auto_loans	  0.2053
#No_of_trades_opened_in_last_6_months	                        0.1861
#No_of_times_90_DPD_or_worse_in_last_6_months1	              0.1149

plot_infotables(IV,"Avgas_CC_Utilization_in_last_12_months")
plot_infotables(IV,"No_of_trades_opened_in_last_12_months")
plot_infotables(IV,"No_of_PL_trades_opened_in_last_12_months")
plot_infotables(IV,"No_of_Inquiries_in_last_12_months_excluding_home_auto_loans")
plot_infotables(IV,"Outstanding_Balance")
plot_infotables(IV,"Total_No_of_Trades")
plot_infotables(IV,"No_of_PL_trades_opened_in_last_6_months")
plot_infotables(IV,"No_of_Inquiries_in_last_6_months_excluding_home_auto_loans")

str(Accepted_Application_Data)

#The set.seed( ) command is used to reproduce the same results each time while sampling 70%
#data for the training dataset. This is done so that you get the same training data as your sample
#each time you execute the R code
set.seed(100)  

# Use sample function for getting the random number represents 70% of indices. 
training_indexes = sample(1:nrow(Accepted_Application_Data), 0.7*nrow(Accepted_Application_Data))

#Create training data objects and test data objects
training_data <- Accepted_Application_Data[training_indexes,]
test_data <- Accepted_Application_Data[-training_indexes,]

#Check whether performance tag is in balanced proportion or not.
as.data.frame(table(training_data$Performance.Tag))

## Smote : Synthetic Minority Oversampling Technique To Handle Class Imbalancy In Binary Classification
#Balancing the unbalanced train dataset
training_data$Performance.Tag <- as.factor(training_data$Performance.Tag)
training_data_balanced <- SMOTE(Performance.Tag ~., training_data, perc.over = 100, perc.under=200)

as.data.frame(table(training_data_balanced$Performance.Tag))

#Model building started...

#Create a multi logistic regression model
LR_model_1 <- glm(formula = Performance.Tag ~ Avgas_CC_Utilization_in_last_12_months +
                  No_of_trades_opened_in_last_12_months + No_of_PL_trades_opened_in_last_12_months +
                  No_of_Inquiries_in_last_12_months_excluding_home_auto_loans + Outstanding_Balance +
                  Total_No_of_Trades + No_of_PL_trades_opened_in_last_6_months +
                  No_of_Inquiries_in_last_6_months_excluding_home_auto_loans + No_of_trades_opened_in_last_6_months +
                  No_of_times_90_DPD_or_worse_in_last_6_months1, data = training_data, family = "binomial")

#Listing VIF value in decreasing order
sort((vif(LR_model_1)), decreasing = TRUE)
summary(LR_model_1)

#Removing No_of_trades_opened_in_last_12_months VIF = 37.87 p value is 0.5456
LR_model_2 <- glm(formula = Performance.Tag ~ Avgas_CC_Utilization_in_last_12_months +
                    No_of_PL_trades_opened_in_last_12_months +
                    No_of_Inquiries_in_last_12_months_excluding_home_auto_loans + Outstanding_Balance +
                    Total_No_of_Trades + No_of_PL_trades_opened_in_last_6_months +
                    No_of_Inquiries_in_last_6_months_excluding_home_auto_loans + No_of_trades_opened_in_last_6_months +
                    No_of_times_90_DPD_or_worse_in_last_6_months1, data = training_data, family = "binomial")

#Listing VIF value in decreasing order
sort((vif(LR_model_2)), decreasing = TRUE)
summary(LR_model_2)

#Removing No_of_trades_opened_in_last_6_months VIF = 7.564531 and p value is 0.7701
LR_model_3 <- glm(formula = Performance.Tag ~ Avgas_CC_Utilization_in_last_12_months +
                    No_of_PL_trades_opened_in_last_12_months +
                    No_of_Inquiries_in_last_12_months_excluding_home_auto_loans + Outstanding_Balance +
                    Total_No_of_Trades + No_of_PL_trades_opened_in_last_6_months +
                    No_of_Inquiries_in_last_6_months_excluding_home_auto_loans +
                    No_of_times_90_DPD_or_worse_in_last_6_months1, data = training_data, family = "binomial")

#Listing VIF value in decreasing order
sort((vif(LR_model_3)), decreasing = TRUE)
summary(LR_model_3)

#Removing No_of_PL_trades_opened_in_last_6_months VIF = 4.524756 and p value is 0.0603
LR_model_4 <- glm(formula = Performance.Tag ~ Avgas_CC_Utilization_in_last_12_months +
                    No_of_PL_trades_opened_in_last_12_months +
                    No_of_Inquiries_in_last_12_months_excluding_home_auto_loans + Outstanding_Balance +
                    Total_No_of_Trades +
                    No_of_Inquiries_in_last_6_months_excluding_home_auto_loans +
                    No_of_times_90_DPD_or_worse_in_last_6_months1, data = training_data, family = "binomial")

#Listing VIF value in decreasing order
sort((vif(LR_model_4)), decreasing = TRUE)
summary(LR_model_4)

#Removing No_of_Inquiries_in_last_6_months_excluding_home_auto_loans VIF = 4.934881 and p value is 0.0180
LR_model_5 <- glm(formula = Performance.Tag ~ Avgas_CC_Utilization_in_last_12_months +
                    No_of_PL_trades_opened_in_last_12_months +
                    No_of_Inquiries_in_last_12_months_excluding_home_auto_loans + Outstanding_Balance +
                    Total_No_of_Trades +
                    No_of_times_90_DPD_or_worse_in_last_6_months1, data = training_data, family = "binomial")

#Listing VIF value in decreasing order
sort((vif(LR_model_5)), decreasing = TRUE)
summary(LR_model_5)

#Removing Outstanding_Balance VIF = 1.086181 and p value is 0.0258
LR_model_final <- glm(formula = Performance.Tag ~ Avgas_CC_Utilization_in_last_12_months +
                    No_of_PL_trades_opened_in_last_12_months +
                    No_of_Inquiries_in_last_12_months_excluding_home_auto_loans +
                    Total_No_of_Trades +
                    No_of_times_90_DPD_or_worse_in_last_6_months1, data = training_data, family = "binomial")

#Listing VIF value in decreasing order
sort((vif(LR_model_final)), decreasing = TRUE)
summary(LR_model_final)

#Model Evaluation started...

#Predict the performance by giving test_data as input into LR_model_final.It can be used to evaluate our model's efficiency.
prediction_data <- predict(LR_model_final,type = "response",subset(test_data, select = -c(Performance.Tag)))

#using probability cut-off as 90%
predicted_performance <- factor(ifelse(prediction_data >= 0.90, "Yes", "No"))
actual_performance <- factor(ifelse(test_data$Performance.Tag == 1, "Yes","No"))

## checking statistics with confusion matrix
confusion_matrix <- table(predicted_performance,actual_performance)
confusionMatrix(confusion_matrix, positive = "Yes")

#Model Performance Accuracy : 0.9437 Sensitivity : 0.98308  Specificity : 0.02665 

#To find the optimal probalility cutoff value where our model is predicting efficiently.
check_model_performance <- function(probability_cutoff) 
{
  predicted_performance_temp <- factor(ifelse(prediction_data >= probability_cutoff, "Yes", "No"))
  confusion_matrix_temp    <- confusionMatrix(predicted_performance_temp,actual_performance, positive = "Yes")
  model_accuracy    <- confusion_matrix_temp$overall[1]
  model_sensitivity <- confusion_matrix_temp$byClass[1]
  model_specificity <- confusion_matrix_temp$byClass[2]
  model_evaluation_vector <- t(as.matrix(c(model_accuracy, model_sensitivity, model_specificity)))
  return(model_evaluation_vector)
}

# Creating probability cutoff values starting from 0.01 upto 1.0. Total 90 sample.
prob_cuttoff_sample   <- seq(0.01, 1, by = .01)

#Initiallizing a model_evaluation_matrix of size 90 X 3
model_evaluation_matrix <- matrix(0,100,3)

#filling in the matrix with values from confusion matrix with each probability
for(i in 1:100) {
  model_evaluation_matrix[i,] <- check_model_performance(prob_cuttoff_sample[i])
} 

optimal_prob_cutoff <- prob_cuttoff_sample[which.min(abs(model_evaluation_matrix[,2] - model_evaluation_matrix[,3]))]

#using probability cut-off as optimal_prob_cutoff
optimal_predicted_performance <- factor(ifelse(prediction_data >= optimal_prob_cutoff, "Yes", "No"))

## checking statistics with confusion matrix
confusion_matrix <- table(optimal_predicted_performance, actual_performance)
confusionMatrix(confusion_matrix, positive = "Yes")

#Model Performance Accuracy : 0.6504, Sensitivity : 0.65400,   Specificity : 0.56547

#Evaluation using KS statistic
optimal_predicted_performance <- ifelse(optimal_predicted_performance == "Yes", 1, 0)
actual_performance <- ifelse(actual_performance == "Yes", 1, 0)

optimal_prediction_test<- prediction(optimal_predicted_performance, actual_performance)
performance_measures_test<- performance(optimal_prediction_test, "tpr", "fpr")

ks_table_test <- attr(performance_measures_test, "y.values")[[1]] - (attr(performance_measures_test, "x.values")[[1]])
max(ks_table_test)

#Evaluation using AUC (Area under the curve)
auc <- performance(optimal_prediction_test, measure = "auc")
auc <- auc@y.values[[1]]
print(auc)

#Model Evaluation ends

#Model building started on balanced dataset...

#Create a multi logistic regression model
LR_model_balanced_1 <- glm(formula = Performance.Tag ~ Avgas_CC_Utilization_in_last_12_months +
                    No_of_trades_opened_in_last_12_months +
                    No_of_PL_trades_opened_in_last_12_months +
                    No_of_Inquiries_in_last_12_months_excluding_home_auto_loans +
                    Outstanding_Balance +
                    Total_No_of_Trades +
                    No_of_PL_trades_opened_in_last_6_months +
                    No_of_Inquiries_in_last_6_months_excluding_home_auto_loans +
                    No_of_trades_opened_in_last_6_months +
                    No_of_times_90_DPD_or_worse_in_last_6_months1, data = training_data_balanced, family = "binomial")

#Listing VIF value in decreasing order
sort((vif(LR_model_balanced_1)), decreasing = TRUE)
summary(LR_model_balanced_1)

#Removing No_of_PL_trades_opened_in_last_12_months VIF = 11.717101 and p value is 0.657763
LR_model_balanced_2 <- glm(formula = Performance.Tag ~ Avgas_CC_Utilization_in_last_12_months +
                    No_of_trades_opened_in_last_12_months +
                    No_of_Inquiries_in_last_12_months_excluding_home_auto_loans +
                    Outstanding_Balance +
                    Total_No_of_Trades +
                    No_of_PL_trades_opened_in_last_6_months +
                    No_of_Inquiries_in_last_6_months_excluding_home_auto_loans +
                    No_of_trades_opened_in_last_6_months +
                    No_of_times_90_DPD_or_worse_in_last_6_months1, data = training_data_balanced, family = "binomial")

#Listing VIF value in decreasing order
sort((vif(LR_model_balanced_2)), decreasing = TRUE)
summary(LR_model_balanced_2)

#Removing No_of_PL_trades_opened_in_last_6_months VIF = 5.987088 and p value is 0.6562
LR_model_balanced_3 <- glm(formula = Performance.Tag ~ Avgas_CC_Utilization_in_last_12_months +
                    No_of_trades_opened_in_last_12_months +
                    No_of_Inquiries_in_last_12_months_excluding_home_auto_loans +
                    Outstanding_Balance +
                    Total_No_of_Trades +
                    No_of_Inquiries_in_last_6_months_excluding_home_auto_loans +
                    No_of_trades_opened_in_last_6_months +
                    No_of_times_90_DPD_or_worse_in_last_6_months1, data = training_data_balanced, family = "binomial")

#Listing VIF value in decreasing order
sort((vif(LR_model_balanced_3)), decreasing = TRUE)
summary(LR_model_balanced_3)

#Removing No_of_Inquiries_in_last_6_months_excluding_home_auto_loans VIF = 5.006356 and p value is 0.0139
LR_model_balanced_final <- glm(formula = Performance.Tag ~ Avgas_CC_Utilization_in_last_12_months +
                    No_of_trades_opened_in_last_12_months +
                    No_of_Inquiries_in_last_12_months_excluding_home_auto_loans +
                    Outstanding_Balance +
                    Total_No_of_Trades +
                    No_of_trades_opened_in_last_6_months +
                    No_of_times_90_DPD_or_worse_in_last_6_months1, data = training_data_balanced, family = "binomial")

#Listing VIF value in decreasing order
sort((vif(LR_model_balanced_final)), decreasing = TRUE)
summary(LR_model_balanced_final)

#Model Evaluation started...

#Predict the performance by giving test_data as input into LR_model_final.It can be used to evaluate our model's efficiency.
prediction_data <- predict(LR_model_balanced_final,type = "response",subset(test_data, select = -c(Performance.Tag)))

#Actual Performance
actual_performance <- factor(ifelse(test_data$Performance.Tag == 1, "Yes","No"))

# Creating probability cutoff values starting from 0.01 upto 1.0. Total 100 sample.
prob_cuttoff_sample   <- seq(0.01, 1, by = .01)

#Initiallizing a model_evaluation_matrix of size 90 X 3
model_evaluation_matrix <- matrix(0,100,3)

#filling in the matrix with values from confusion matrix with each probability
for(i in 1:100) {
  model_evaluation_matrix[i,] <- check_model_performance(prob_cuttoff_sample[i])
} 

optimal_prob_cutoff <- prob_cuttoff_sample[which.min(abs(model_evaluation_matrix[,2] - model_evaluation_matrix[,3]))]

#using probability cut-off as optimal_prob_cutoff
optimal_predicted_performance <- factor(ifelse(prediction_data >= optimal_prob_cutoff, "Yes", "No"))

## checking statistics with confusion matrix
confusion_matrix <- table(optimal_predicted_performance, actual_performance)
confusionMatrix(confusion_matrix, positive = "Yes")

#Model Performance Accuracy : 0.6184, Sensitivity : 0.61847,   Specificity : 0.61645

#Evaluation using KS statistic
optimal_predicted_performance <- ifelse(optimal_predicted_performance == "Yes", 1, 0)
actual_performance <- ifelse(actual_performance == "Yes", 1, 0)

optimal_prediction_test<- prediction(optimal_predicted_performance, actual_performance)
performance_measures_test<- performance(optimal_prediction_test, "tpr", "fpr")

ks_table_test <- attr(performance_measures_test, "y.values")[[1]] - (attr(performance_measures_test, "x.values")[[1]])
max(ks_table_test)

#Evaluation using AUC (Area under the curve)
auc <- performance(optimal_prediction_test, measure = "auc")
auc <- auc@y.values[[1]]
print(auc)

# Creating model on accepted_application_data_woe (with balanced training dataset and with original dataset)

# Based on woe value analysis Top 12 most significant variables are as follows:
#Avgas_CC_Utilization_in_last_12_months	                     0.3070
#No_of_trades_opened_in_last_12_months	                     0.2980
#No_of_PL_trades_opened_in_last_12_months	                   0.2961
#No_of_Inquiries_in_last_12_months_excluding_home_auto_loans 0.2955
#Outstanding_Balance	                                       0.2432
#No_of_times_30_DPD_or_worse_in_last_6_months	               0.2418
#Total_No_of_Trades	                                         0.2366
#No_of_PL_trades_opened_in_last_6_months	                   0.2198
#No_of_times_90_DPD_or_worse_in_last_12_months	             0.2143
#No_of_times_60_DPD_or_worse_in_last_6_months	               0.2062
#No_of_Inquiries_in_last_6_months_excluding_home_auto_loans	 0.2053
#No_of_times_30_DPD_or_worse_in_last_12_months	             0.1984
#No_of_trades_opened_in_last_6_months	                       0.1861
#No_of_times_60_DPD_or_worse_in_last_12_months	             0.1859
#No_of_times_90_DPD_or_worse_in_last_6_months	               0.1605

#The set.seed( ) command is used to reproduce the same results each time while sampling 70%
#data for the training dataset. This is done so that you get the same training data as your sample
#each time you execute the R code
set.seed(100)  

# Use sample function for getting the random number represents 70% of indices. 
training_indexes_woe = sample(1:nrow(Accepted_Application_Data_woe), 0.7*nrow(Accepted_Application_Data_woe))

#Create training data objects and test data objects
training_data_woe <- Accepted_Application_Data_woe[training_indexes_woe,]
test_data_woe <- Accepted_Application_Data_woe[-training_indexes_woe,]

#Check whether performance tag is in balanced proportion or not.
as.data.frame(table(training_data_woe$Performance.Tag))

training_data_woe$Performance.Tag <- as.factor(training_data_woe$Performance.Tag)
## Smote : Synthetic Minority Oversampling Technique To Handle Class Imbalancy In Binary Classification
#Balancing the unbalanced train dataset
training_data_woe_balanced <- SMOTE(Performance.Tag ~., training_data_woe, perc.over = 100, perc.under=200)

as.data.frame(table(training_data_woe_balanced$Performance.Tag))

#Model building started...

#Create a multi logistic regression model
LR_model_woe_1 <- glm(formula = Performance.Tag ~ `Avgas_CC_Utilization_in_last_12_months:WOE` +
                  `No_of_trades_opened_in_last_12_months:WOE` +
                  `No_of_PL_trades_opened_in_last_12_months:WOE` +
                  `No_of_Inquiries_in_last_12_months_excluding_home_auto_loans:WOE` +
                  `Outstanding_Balance:WOE` +
                  `No_of_times_30_DPD_or_worse_in_last_6_months:WOE` +
                  `Total_No_of_Trades:WOE` +
                  `No_of_PL_trades_opened_in_last_6_months:WOE` +
                  `No_of_times_90_DPD_or_worse_in_last_12_months:WOE` +
                  `No_of_times_60_DPD_or_worse_in_last_6_months:WOE` +
                  `No_of_Inquiries_in_last_6_months_excluding_home_auto_loans:WOE` +
                  `No_of_times_30_DPD_or_worse_in_last_12_months:WOE` +
                  `No_of_trades_opened_in_last_6_months:WOE` +
                  `No_of_times_60_DPD_or_worse_in_last_12_months:WOE` +
                  `No_of_times_90_DPD_or_worse_in_last_6_months:WOE`, data = training_data_woe, family = "binomial")

#Listing VIF value in decreasing order
sort((vif(LR_model_woe_1)), decreasing = TRUE)
summary(LR_model_woe_1)

#Removing `No_of_times_30_DPD_or_worse_in_last_6_months:WOE` VIF = 12.370365 and p value is 0.0216
#Removing `No_of_times_60_DPD_or_worse_in_last_6_months:WOE` VIF = 11.031343 and p value is 0.4358
#Removing `No_of_trades_opened_in_last_12_months:WOE` VIF =  8.092416 and p value is 0.0410 *
#Removing `No_of_PL_trades_opened_in_last_12_months:WOE` VIF = 7.869881 and p value is 0.2703 

LR_model_woe_2 <- glm(formula = Performance.Tag ~`Avgas_CC_Utilization_in_last_12_months:WOE` +
                        `No_of_Inquiries_in_last_12_months_excluding_home_auto_loans:WOE` +
                        `Outstanding_Balance:WOE` +
                        `Total_No_of_Trades:WOE` +
                        `No_of_PL_trades_opened_in_last_6_months:WOE` +
                        `No_of_times_90_DPD_or_worse_in_last_12_months:WOE` +
                        `No_of_Inquiries_in_last_6_months_excluding_home_auto_loans:WOE` +
                        `No_of_times_30_DPD_or_worse_in_last_12_months:WOE` +
                        `No_of_trades_opened_in_last_6_months:WOE` +
                        `No_of_times_60_DPD_or_worse_in_last_12_months:WOE` +
                        `No_of_times_90_DPD_or_worse_in_last_6_months:WOE`, data = training_data_woe, family = "binomial")

#Listing VIF value in decreasing order
sort((vif(LR_model_woe_2)), decreasing = TRUE)
summary(LR_model_woe_2)

#Removing `No_of_trades_opened_in_last_6_months:WOE` VIF = 3.359461 and p value is 0.70219
#Removing `No_of_Inquiries_in_last_6_months_excluding_home_auto_loans:WOE` VIF = 3.359461 and p value is 0.67125
#Removing `No_of_times_90_DPD_or_worse_in_last_12_months:WOE` 
#Removing `No_of_times_30_DPD_or_worse_in_last_12_months:WOE` 
#Removing `No_of_times_60_DPD_or_worse_in_last_12_months:WOE` 
LR_model_woe_3 <- glm(formula = Performance.Tag ~`Avgas_CC_Utilization_in_last_12_months:WOE` +
                        `No_of_Inquiries_in_last_12_months_excluding_home_auto_loans:WOE` +
                        `Outstanding_Balance:WOE` +
                        `Total_No_of_Trades:WOE` +
                        `No_of_PL_trades_opened_in_last_6_months:WOE` +
                        `No_of_times_90_DPD_or_worse_in_last_6_months:WOE`, data = training_data_woe, family = "binomial")

#Listing VIF value in decreasing order
sort((vif(LR_model_woe_3)), decreasing = TRUE)
summary(LR_model_woe_3)

#Removing `Total_No_of_Trades:WOE`
LR_model_woe_4 <- glm(formula = Performance.Tag ~`Avgas_CC_Utilization_in_last_12_months:WOE` +
                        `No_of_Inquiries_in_last_12_months_excluding_home_auto_loans:WOE` +
                        `Outstanding_Balance:WOE` +
                        `No_of_PL_trades_opened_in_last_6_months:WOE` +
                        `No_of_times_90_DPD_or_worse_in_last_6_months:WOE`, data = training_data_woe, family = "binomial")

#Listing VIF value in decreasing order
sort((vif(LR_model_woe_4)), decreasing = TRUE)
summary(LR_model_woe_4)

#Removing `No_of_PL_trades_opened_in_last_6_months:WOE`
LR_model_woe_final <- glm(formula = Performance.Tag ~`Avgas_CC_Utilization_in_last_12_months:WOE` +
                        `No_of_Inquiries_in_last_12_months_excluding_home_auto_loans:WOE` +
                        `Outstanding_Balance:WOE` +
                        `No_of_times_90_DPD_or_worse_in_last_6_months:WOE`, data = training_data_woe, family = "binomial")

#Listing VIF value in decreasing order
sort((vif(LR_model_woe_final)), decreasing = TRUE)
summary(LR_model_woe_final)

#Model Evaluation started...

#Predict the performance by giving test_data_woe as input into LR_model_woe_final. It can be used to evaluate our model's efficiency.
prediction_data <- predict(LR_model_woe_final,type = "response",subset(test_data_woe, select = -c(Performance.Tag)))

#Actual Performance
actual_performance <- factor(ifelse(test_data_woe$Performance.Tag == 1, "Yes","No"))

# Creating probability cutoff values starting from 0.01 upto 1.0. Total 100 sample.
prob_cuttoff_sample   <- seq(0.01, 1, by = .01)

#Initiallizing a model_evaluation_matrix of size 90 X 3
model_evaluation_matrix <- matrix(0,100,3)

#filling in the matrix with values from confusion matrix with each probability
for(i in 1:100) {
  model_evaluation_matrix[i,] <- check_model_performance(prob_cuttoff_sample[i])
} 

optimal_prob_cutoff <- prob_cuttoff_sample[which.min(abs(model_evaluation_matrix[,2] - model_evaluation_matrix[,3]))]

#using probability cut-off as optimal_prob_cutoff
optimal_predicted_performance <- factor(ifelse(prediction_data >= optimal_prob_cutoff, "Yes", "No"))

## checking statistics with confusion matrix
confusion_matrix <- table(optimal_predicted_performance, actual_performance)
confusionMatrix(confusion_matrix, positive = "Yes")

#Model Performance Accuracy : 0.6071, Sensitivity : 0.67555,   Specificity : 0.60414

#Evaluation using KS statistic
optimal_predicted_performance <- ifelse(optimal_predicted_performance == "Yes", 1, 0)
actual_performance <- ifelse(actual_performance == "Yes", 1, 0)

optimal_prediction_test<- prediction(optimal_predicted_performance, actual_performance)
performance_measures_test<- performance(optimal_prediction_test, "tpr", "fpr")

ks_table_test <- attr(performance_measures_test, "y.values")[[1]] - (attr(performance_measures_test, "x.values")[[1]])
max(ks_table_test)

#Evaluation using AUC (Area under the curve)
auc <- performance(optimal_prediction_test, measure = "auc")
auc <- auc@y.values[[1]]
print(auc)

#Model building started...

#Create a multi logistic regression model
LR_model_woe_balanced_1 <- glm(formula = Performance.Tag ~ `Avgas_CC_Utilization_in_last_12_months:WOE` +
                        `No_of_trades_opened_in_last_12_months:WOE` +
                        `No_of_PL_trades_opened_in_last_12_months:WOE` +
                        `No_of_Inquiries_in_last_12_months_excluding_home_auto_loans:WOE` +
                        `Outstanding_Balance:WOE` +
                        `No_of_times_30_DPD_or_worse_in_last_6_months:WOE` +
                        `Total_No_of_Trades:WOE` +
                        `No_of_PL_trades_opened_in_last_6_months:WOE` +
                        `No_of_times_90_DPD_or_worse_in_last_12_months:WOE` +
                        `No_of_times_60_DPD_or_worse_in_last_6_months:WOE` +
                        `No_of_Inquiries_in_last_6_months_excluding_home_auto_loans:WOE` +
                        `No_of_times_30_DPD_or_worse_in_last_12_months:WOE` +
                        `No_of_trades_opened_in_last_6_months:WOE` +
                        `No_of_times_60_DPD_or_worse_in_last_12_months:WOE` +
                        `No_of_times_90_DPD_or_worse_in_last_6_months:WOE`, data = training_data_woe_balanced , family = "binomial")

#Listing VIF value in decreasing order
sort((vif(LR_model_woe_balanced_1)), decreasing = TRUE)
summary(LR_model_woe_balanced_1)

#remove No_of_times_30_DPD_or_worse_in_last_6_months:WOE - 16.347
#remove No_of_times_60_DPD_or_worse_in_last_6_months:WOE - 13.892
#remove No_of_PL_trades_opened_in_last_12_months:WOE - 9.513
#remove No_of_trades_opened_in_last_12_months:WOE - 9.306

LR_model_woe_balanced_2 <- glm(formula = Performance.Tag ~ `Avgas_CC_Utilization_in_last_12_months:WOE` +
                        `No_of_Inquiries_in_last_12_months_excluding_home_auto_loans:WOE` +
                        `Outstanding_Balance:WOE` +
                        `Total_No_of_Trades:WOE` +
                        `No_of_PL_trades_opened_in_last_6_months:WOE` +
                        `No_of_times_90_DPD_or_worse_in_last_12_months:WOE` +
                        `No_of_Inquiries_in_last_6_months_excluding_home_auto_loans:WOE` +
                        `No_of_times_30_DPD_or_worse_in_last_12_months:WOE` +
                        `No_of_trades_opened_in_last_6_months:WOE` +
                        `No_of_times_60_DPD_or_worse_in_last_12_months:WOE` +
                        `No_of_times_90_DPD_or_worse_in_last_6_months:WOE`, data = training_data_woe_balanced , family = "binomial")


#Listing VIF value in decreasing order
sort((vif(LR_model_woe_balanced_2)), decreasing = TRUE)
summary(LR_model_woe_balanced_2)

#remove `No_of_times_60_DPD_or_worse_in_last_12_months:WOE` - 5.866641 
#remove `No_of_times_90_DPD_or_worse_in_last_12_months:WOE` - 5.170369 
#remove `No_of_times_30_DPD_or_worse_in_last_12_months:WOE` - 5.104228 

LR_model_woe_balanced_3 <- glm(formula = Performance.Tag ~ `Avgas_CC_Utilization_in_last_12_months:WOE` +
                                 `No_of_Inquiries_in_last_12_months_excluding_home_auto_loans:WOE` +
                                 `Outstanding_Balance:WOE` +
                                 `Total_No_of_Trades:WOE` +
                                 `No_of_PL_trades_opened_in_last_6_months:WOE` +
                                 `No_of_Inquiries_in_last_6_months_excluding_home_auto_loans:WOE` +
                                 `No_of_trades_opened_in_last_6_months:WOE` +
                                 `No_of_times_90_DPD_or_worse_in_last_6_months:WOE`, data = training_data_woe_balanced , family = "binomial")


#Listing VIF value in decreasing order
sort((vif(LR_model_woe_balanced_3)), decreasing = TRUE)
summary(LR_model_woe_balanced_3)

#remove `No_of_PL_trades_opened_in_last_6_months:WOE` - 4.028237
#remove `No_of_trades_opened_in_last_6_months:WOE`  - 3.935991
#remove `Total_No_of_Trades:WOE` - 3.975069

LR_model_woe_balanced_4 <- glm(formula = Performance.Tag ~ `Avgas_CC_Utilization_in_last_12_months:WOE` +
                                 `No_of_Inquiries_in_last_12_months_excluding_home_auto_loans:WOE` +
                                 `Outstanding_Balance:WOE` +
                                 `No_of_Inquiries_in_last_6_months_excluding_home_auto_loans:WOE` +
                                 `No_of_times_90_DPD_or_worse_in_last_6_months:WOE`, data = training_data_woe_balanced , family = "binomial")


#Listing VIF value in decreasing order
sort((vif(LR_model_woe_balanced_4)), decreasing = TRUE)
summary(LR_model_woe_balanced_4)

#remove `No_of_Inquiries_in_last_12_months_excluding_home_auto_loans:WOE` - 3.959058 
LR_model_woe_balanced_final <- glm(formula = Performance.Tag ~ `Avgas_CC_Utilization_in_last_12_months:WOE` +
                                 `Outstanding_Balance:WOE` +
                                 `No_of_Inquiries_in_last_6_months_excluding_home_auto_loans:WOE` +
                                 `No_of_times_90_DPD_or_worse_in_last_6_months:WOE`, data = training_data_woe_balanced , family = "binomial")


#Listing VIF value in decreasing order
sort((vif(LR_model_woe_balanced_final)), decreasing = TRUE)
summary(LR_model_woe_balanced_final)

#Model Evaluation started...

#Predict the performance by giving test_data_woe as input into LR_model_woe_final. It can be used to evaluate our model's efficiency.
prediction_data <- predict(LR_model_woe_balanced_final,type = "response",subset(test_data_woe, select = -c(Performance.Tag)))

#Actual Performance
actual_performance <- factor(ifelse(test_data_woe$Performance.Tag == 1, "Yes","No"))

# Creating probability cutoff values starting from 0.01 upto 1.0. Total 100 sample.
prob_cuttoff_sample   <- seq(0.01, 1, by = .01)

#Initiallizing a model_evaluation_matrix of size 90 X 3
model_evaluation_matrix <- matrix(0,100,3)

#filling in the matrix with values from confusion matrix with each probability
for(i in 1:100) {
  model_evaluation_matrix[i,] <- check_model_performance(prob_cuttoff_sample[i])
} 

optimal_prob_cutoff <- prob_cuttoff_sample[which.min(abs(model_evaluation_matrix[,2] - model_evaluation_matrix[,3]))]

#using probability cut-off as optimal_prob_cutoff
optimal_predicted_performance <- factor(ifelse(prediction_data >= optimal_prob_cutoff, "Yes", "No"))

## checking statistics with confusion matrix
confusion_matrix <- table(optimal_predicted_performance, actual_performance)
confusionMatrix(confusion_matrix, positive = "Yes")

#Model Performance Accuracy : 0.6311, Sensitivity : 0.63499,   Specificity : 0.63091

#Evaluation using KS statistic
optimal_predicted_performance <- ifelse(optimal_predicted_performance == "Yes", 1, 0)
actual_performance <- ifelse(actual_performance == "Yes", 1, 0)

optimal_prediction_test<- prediction(optimal_predicted_performance, actual_performance)
performance_measures_test<- performance(optimal_prediction_test, "tpr", "fpr")

ks_table_test <- attr(performance_measures_test, "y.values")[[1]] - (attr(performance_measures_test, "x.values")[[1]])
max(ks_table_test)

#Evaluation using AUC (Area under the curve)
auc <- performance(optimal_prediction_test, measure = "auc")
auc <- auc@y.values[[1]]
print(auc)

# Based on Model Evaluation of various model we consider LR_model_woe_balanced_final as our final mode which is having
# below Model Performance
# Accuracy    : 0.6311
# Sensitivity : 0.63499
# Specificity : 0.63091
# KS Score    : 0.2659
# AUC         : 0.6329

#Creating Appplication Scorecard

# Create the Application Scorecard 
#
# This is an function name `Application.Scorecard`, The scorecard is being developed using 
# specified odds at a score and specified "points to double the odds" (pdo), the factor and
# offset can easily be calculated by using the following simultaneous equations:
#Score = Offset + Factor * ln (odds) ---------------------> 1
#Score + pdo = Offset + Factor * ln (2 * odds) -----------> 2
#
#Solving the above equations for pdo i.e ( eqn 2 - eqn 1 ), we get
# pdo = Factor * ln (2), therefore Factor = pdo / ln (2);
# Offset = Score - {Factor * ln (Odds)}
#
# For example, if a scorecard were being scaled where the user wanted odds of 10:1 at 400 points
# and wanted the odds to double every 20 points (i.e., pdo = 20), the factor and offset would be:
#
#Factor = 20 / ln (2) = 28.8539 
#Offset = 400 - {28.8539 ln (10)} = 333.56
#
#And each score corresponding to each set of odds (or each record) can be calculated as:
#Score = 333.56 + 28.8539 ln (odds)
Application.Scorecard <-
  function(test_data) {
    
    STD_ODDS <- 10
    STD_PDO <- 20
    STD_SCORE <- 400
    Factor <- STD_PDO/ log(2)
    STD_OFFSET <- STD_SCORE - Factor * log(STD_ODDS)

    # Predict the performance by giving test_data as input into LR_model_woe_balanced_final.
    # It can be used to evaluate our model's efficiency.
    prediction_data <- predict(LR_model_woe_balanced_final,type = "response",subset(test_data, select = -c(Performance.Tag)))
    
    #Optimal Predicted performance
    optimal_predicted_performance <- ifelse(optimal_predicted_performance == 1, "Yes","No")
    
    #Actual Performance
    actual_performance <- factor(ifelse(test_data$Performance.Tag == 1, "Yes","No"))
    
    odds <- prediction_data/(1-prediction_data)
    score <- STD_OFFSET + Factor * log(odds)
    #Application Scorecard
    Application_Score_Card <- data.frame(test_data$Application.ID, test_data_woe$Performance.Tag,prediction_data,odds,score,optimal_predicted_performance,actual_performance)
    return(Application_Score_Card)
  }
asc_test_data <- Application.Scorecard(test_data_woe)

###################             End of the Analysis               #############################
