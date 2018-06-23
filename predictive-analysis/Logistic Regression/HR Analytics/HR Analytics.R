# HR Analytics Case Study
#
# Scope                 : To build a logistic regression model for getting probability of attrition. This
#                         HR Analytics case study is being carried for the company who wants to act upon 
#                         the cause of attrition in the company.
#
# Uses                  : This model will be used by the management to understand what changes they
#                         should make to their workplace, in order to get most of their employees to stay.
# Input datasets ->
#
# 1. general_data.csv   : It Provides demographic informations of employee such as Age, working experience,
#                         educational background, department working, monthly income, salary hike etc.
# 2. employee_survey.csv: Rating by employee regarding job statisfacton, enviroment statisfaction etc
# 3. manager_survey.csv : Rating by manager regarding performance of the employee, job involvement etc
# 4. in_time.csv        : In time of each employee for whole year 2015.
# 5. out_time.csv       : Out time of each employee for whole year 2015.
#
# Name : Swami Prem Pranav Kayashyap (APFE1786831)
#        
# ------------------------------------------------------
# Date : 27th May, 2018

#Import library
library(Hmisc)     # To imputes missing value
library(dplyr)     # To use gather and convert wide format to long format
library(tidyr)     # To use gather and convert wide format to long format
library(stringr)   # String manupulation
library(compare)   # To compare 2 dataframe having same data
library(ggplot2)   # For plotting
library(cowplot)   # For plot_grid
library(car)       # To use vif
library(MASS)      # To use stepAIC
library(caret)     # To draw confusion matix
library(ROCR)      # To use prediction in KS- statistics
#Reading from input datasets
general_data     <- read.csv("general_data.csv", stringsAsFactors = F)
employee_survey  <- read.csv("employee_survey_data.csv", stringsAsFactors = F)
manager_survey   <- read.csv("manager_survey_data.csv", stringsAsFactors = F)
in_time          <- read.csv("in_time.csv", stringsAsFactors = F)
out_time         <- read.csv("out_time.csv", stringsAsFactors = F)

#Adding the missing column heading EmployeeID for in_time and out_time datasets.
colnames(in_time) [1]  <- "EmployeeID"
colnames(out_time) [1] <- "EmployeeID"

#Checking for duplicates of key field EmployeeID. If sum return 0 that means no duplicates. 
sum(duplicated(general_data$EmployeeID))          # 0 duplicates
sum(duplicated(employee_survey$EmployeeID))       # 0 duplicates
sum(duplicated(manager_survey$EmployeeID))        # 0 duplicates
sum(duplicated(in_time$EmployeeID))               # 0 duplicates
sum(duplicated(out_time$EmployeeID))              # 0 duplicates

#checking for NA's in all the datasets
sum(is.na(general_data))    # 28 NA's 
sum(is.na(employee_survey)) # 83 NA's
sum(is.na(manager_survey))  # 0 NA's
sum(is.na(in_time))         # 109080 NA's
sum(is.na(out_time))        # 109080 NA's

#NA value analysis in general_data dataset
colSums(is.na(general_data))
sum(is.na(general_data$NumCompaniesWorked)) #19 NA's
sum(is.na(general_data$TotalWorkingYears))  #9 NA's

#If total_working_years and years_at_company is same. it means, employee has worked in 1 company. 
general_data$diff_wrking_yrs <- general_data$TotalWorkingYears - general_data$YearsAtCompany
general_data$NumCompaniesWorked [(which(is.na(general_data$NumCompaniesWorked)))] <- ifelse(general_data$diff_wrking_yrs [(which(is.na(general_data$NumCompaniesWorked)))] == 0, 1, NA)
general_data <- subset(general_data, select = -c(diff_wrking_yrs))

#Removing employee having NA value in any column
general_data <- general_data[rowSums(is.na(general_data)) == 0,]

#Checking if any other NA values 
colSums(is.na(general_data))


#NA value analysis in employee_survey dataset 
colSums(is.na(employee_survey))
sum(is.na(employee_survey$EnvironmentSatisfaction)) #25 NA's
sum(is.na(employee_survey$JobSatisfaction))         #20 NA's
sum(is.na(employee_survey$WorkLifeBalance))         #38 NA's

#Replacing all the missing value with median of the data set for EnvironmentSatisfaction, JobSatisfaction
#and WorkLifeBalance. Avoiding mean value because of decimal. 
employee_survey$EnvironmentSatisfaction <- impute(employee_survey$EnvironmentSatisfaction, median)
employee_survey$JobSatisfaction         <- impute(employee_survey$JobSatisfaction, median)
employee_survey$WorkLifeBalance         <- impute(employee_survey$WorkLifeBalance, median)

#Checking if any other NA values 
colSums(is.na(employee_survey))

#NA value analysis in in_time dataset

#converting the dataset from wide format to long format. We need to have a key value pair. Here key is the
#date and the value is check-in time.
in_time        <-  gather(in_time, in_day, in_day_time, X2015.01.01 :X2015.12.31)
in_time$in_day <-  str_replace(in_time$in_day, "X", "")

colSums(is.na(in_time))

#converting the dataset from wide format to long format. We need to have a key value pair. Here key is the
#date and the value is check-out time.
out_time <-  gather(out_time, out_day, out_day_time, X2015.01.01 :X2015.12.31)
out_time$out_day <- str_replace(out_time$out_day, "X", "")

colSums(is.na(out_time))

#Removing rows having NA value in the dataset
in_time       <- in_time[ (!is.na(in_time$in_day_time)), ]
out_time      <- out_time[ (!is.na(out_time$out_day_time)), ]

#Checking if any other NA values 
colSums(is.na(in_time))
colSums(is.na(out_time))

#compare in_time and out_time dataset by EmployeeID. To check whether we can combine these two dataset.
compare(in_time$EmployeeID, out_time$EmployeeID, allowAll=TRUE)

#Combining in_time and out_time dataset
in_out_time   <- cbind(in_time, out_time)
in_out_time   <- subset(in_out_time, select = c(EmployeeID,in_day,in_day_time,out_day_time))

# convert to datetime
in_out_time$in_day       <- as.POSIXlt(in_out_time$in_day, format = "%Y.%m.%d")
in_out_time$in_day_time  <- as.POSIXlt(in_out_time$in_day_time, format = "%Y-%m-%d %H:%M:%S")
in_out_time$out_day_time <- as.POSIXlt(in_out_time$out_day_time, format = "%Y-%m-%d %H:%M:%S")

#Calculating working hours which is the difference between in and out time at particular date
in_out_time$working_hour <-  difftime(in_out_time$out_day_time, in_out_time$in_day_time, units = c("hours"))

#Calculating average working hours
emp_avg_working_hour <- aggregate(in_out_time$working_hour, by = list(in_out_time$EmployeeID), mean)

#Renaming the columns emp_avg_working_hour
colnames(emp_avg_working_hour) <- c("EmployeeID", "avg_working_hrs")

#Rounding the average working hours upto 2 decimal place
emp_avg_working_hour$avg_working_hrs <- round(emp_avg_working_hour$avg_working_hrs, 2)

#merging all the data into one dataset input_data
input_data <- merge(general_data , emp_avg_working_hour, by = "EmployeeID")
input_data <- merge(input_data, employee_survey, by = "EmployeeID")
input_data <- merge(input_data, manager_survey, by = "EmployeeID")

#Standardising numerical data
input_data$avg_working_hrs         <- as.numeric(input_data$avg_working_hrs)
input_data$EnvironmentSatisfaction <- as.numeric(input_data$EnvironmentSatisfaction)
input_data$JobSatisfaction         <- as.numeric(input_data$JobSatisfaction)
input_data$WorkLifeBalance         <- as.numeric(input_data$WorkLifeBalance)

#Checking for blank space
sapply(input_data, function(x) length(which(x == ""))) #No blank spaces found

#####                                 Data cleaning ends                                    #####

#####                                 Exploratory data analysis started                     #####

#univariate and Byvariate analysis on categorical variables

#Univariate analysis based on Attrition 
ggplot(input_data, aes(input_data$Attrition , fill = Attrition)) +
  geom_bar(stat= "count") +
  geom_text(stat = "count", aes(label= scales::percent((..count..)/sum(..count..))), vjust=-0.2, position = position_dodge(0.2)) +
  xlab("Attrition") +
  ylab("Total Employee") +
  ggtitle(" No of Employee Vs Attrition")

#Byvariate analysis of Attrition with other categorical attributes such as BusinessTravel,Department,
#EducationField, Gender, JobLevel and MaritalStatus.

#Attrition and BusinessTravel
ggplot(input_data, aes(input_data$BusinessTravel , fill = Attrition)) +
  geom_bar(stat= "count") +
  geom_text(stat = "count", aes(label= scales::percent((..count..)/sum(..count..))), vjust=-0.2, position = position_dodge(0.2)) +
  xlab("BusinessTravel") +
  ylab("Total Employee") +
  ggtitle("BusinessTravel based Attrition")

#Attrition and Department
ggplot(input_data, aes(input_data$Department , fill = Attrition)) +
  geom_bar(stat= "count") +
  geom_text(stat = "count", aes(label= scales::percent((..count..)/sum(..count..))), vjust=-0.2, position = position_dodge(0.2)) +
  xlab("Department") +
  ylab("Total Employee") +
  ggtitle("Department based Attrition")

#Attrition and EducationField
ggplot(input_data, aes(input_data$EducationField , fill = Attrition)) +
  geom_bar(stat= "count") +
  geom_text(stat = "count", aes(label= scales::percent((..count..)/sum(..count..))), vjust=-0.2, position = position_dodge(0.2)) +
  xlab("EducationField") +
  ylab("Total Employee") +
  ggtitle("EducationField  based Attrition")

#Attrition and Gender
ggplot(input_data, aes(input_data$Gender , fill = Attrition)) +
  geom_bar(stat= "count") +
  geom_text(stat = "count", aes(label= scales::percent((..count..)/sum(..count..))), vjust=-0.2, position = position_dodge(0.2)) +
  xlab("Gender") +
  ylab("Total Employee") +
  ggtitle("Gender based Attrition")

#Attrition and JobLevel
ggplot(input_data, aes(input_data$JobLevel , fill = Attrition)) +
  geom_bar(stat= "count") +
  geom_text(stat = "count", aes(label= scales::percent((..count..)/sum(..count..))), vjust=-0.2, position = position_dodge(0.2)) +
  xlab("JobLevel") +
  ylab("Total Employee") +
  ggtitle("JobLevel based Attrition")

#Attrition and MaritalStatus
ggplot(input_data, aes(input_data$MaritalStatus , fill = Attrition)) +
  geom_bar(stat= "count") +
  geom_text(stat = "count", aes(label= scales::percent((..count..)/sum(..count..))), vjust=-0.2, position = position_dodge(0.2)) +
  xlab("MaritalStatus") +
  ylab("Total Employee") +
  ggtitle("MaritalStatus based Attrition")

#Byvariate analysis of Attrition with other categorical attributes such as StockOptionLevel, EnvironmentSatisfaction,
#JobSatisfaction, WorkLifeBalance, JobInvolvement and PerformanceRating.

#Attrition and StockOptionLevel
ggplot(input_data, aes(input_data$StockOptionLevel , fill = Attrition)) +
  geom_bar(stat= "count") +
  geom_text(stat = "count", aes(label= scales::percent((..count..)/sum(..count..))), vjust=-0.2, position = position_dodge(0.2)) +
  xlab("StockOptionLevel") +
  ylab("Total Employee") +
  ggtitle("StockOptionLevel based Attrition")

#Attrition and EnvironmentSatisfaction
ggplot(input_data, aes(input_data$EnvironmentSatisfaction , fill = Attrition)) +
  geom_bar(stat= "count") +
  geom_text(stat = "count", aes(label= scales::percent((..count..)/sum(..count..))), vjust=-0.2, position = position_dodge(0.2)) +
  xlab("EnvironmentSatisfaction") +
  ylab("Total Employee") +
  ggtitle("EnvironmentSatisfaction based Attrition")

#Attrition and JobSatisfaction
ggplot(input_data, aes(input_data$EnvironmentSatisfaction , fill = Attrition)) +
  geom_bar(stat= "count") +
  geom_text(stat = "count", aes(label= scales::percent((..count..)/sum(..count..))), vjust=-0.2, position = position_dodge(0.2)) +
  xlab("JobSatisfaction") +
  ylab("Total Employee") +
  ggtitle("JobSatisfaction based Attrition")

#Attrition and WorkLifeBalance
ggplot(input_data, aes(input_data$WorkLifeBalance , fill = Attrition)) +
  geom_bar(stat= "count") +
  geom_text(stat = "count", aes(label= scales::percent((..count..)/sum(..count..))), vjust=-0.2, position = position_dodge(0.2)) +
  xlab("WorkLifeBalance") +
  ylab("Total Employee") +
  ggtitle("WorkLifeBalance based Attrition")

#Attrition and JobInvolvement
ggplot(input_data, aes(input_data$JobInvolvement , fill = Attrition)) +
  geom_bar(stat= "count") +
  geom_text(stat = "count", aes(label= scales::percent((..count..)/sum(..count..))), vjust=-0.2, position = position_dodge(0.2)) +
  xlab("JobInvolvement") +
  ylab("Total Employee") +
  ggtitle("JobInvolvement based Attrition")

#Attrition and PerformanceRating
ggplot(input_data, aes(factor(input_data$PerformanceRating) , fill = Attrition)) +
  geom_bar(stat= "count") +
  geom_text(stat = "count", aes(label= scales::percent((..count..)/sum(..count..))), vjust=-0.2, position = position_dodge(0.2)) +
  xlab("PerformanceRating") +
  ylab("Total Employee") +
  ggtitle("PerformanceRating based Attrition")

# Univariate analysis using Boxplots for numeric variables such as PercentSalaryHike,
# YearsWithCurrManager, DistanceFromHome, YearsAtCompany, YearsSinceLastPromotion and MonthlyIncome.
ggplot(input_data, aes(x="", y=PercentSalaryHike)) +
  geom_boxplot() +
  labs(title = "PercentSalaryHike", x = "", y = "PercentSalaryHike")

ggplot(input_data, aes(x="", y=YearsWithCurrManager)) +
  geom_boxplot() +
  labs(title = "YearsWithCurrManager", x = "", y = "YearsWithCurrManager")

ggplot(input_data, aes(x="", y=DistanceFromHome)) +
  geom_boxplot() +
  labs(title = "DistanceFromHome", x = "", y = "DistanceFromHome")

ggplot(input_data, aes(x="", y=YearsAtCompany)) +
  geom_boxplot() +
  labs(title = "YearsAtCompany", x = "", y = "YearsAtCompany")

ggplot(input_data, aes(x="", y=YearsSinceLastPromotion)) +
  geom_boxplot() +
  labs(title = "YearsSinceLastPromotion", x = "", y = "YearsSinceLastPromotion")

ggplot(input_data, aes(x="", y=MonthlyIncome)) +
  geom_boxplot() +
  labs(title = "MonthlyIncome", x = "", y = "MonthlyIncome")

#Conclusion: No significant outliers in the above numeric variables 

#Attrition and extended working hours
input_data$extended_working_hrs <- ifelse(input_data$avg_working_hrs > 8,"Yes","No")
ggplot(input_data, aes(input_data$extended_working_hrs , fill = Attrition)) +
  geom_bar(stat= "count") +
  geom_text(stat = "count", aes(label= scales::percent((..count..)/sum(..count..))), vjust=-0.2, position = position_dodge(0.2)) +
  xlab("extended_working_hrs") +
  ylab("Total Employee") +
  ggtitle("extended_working_hrs based Attrition")
#Conclusion: Employee working more then 8 hour have more attrition percent.

#Removing few columns which is not requied for this analysis such as EmployeeID, Age, EmployeeCount,
#StandardHours and Over18
input_data <- subset(input_data, select = -c(EmployeeID, Age, EmployeeCount,StandardHours, Over18))

#convering all the non numeric columns to upper case 
input_data <- data.frame(lapply(input_data, function(v) {
  if (is.character(v)) return(toupper(v))
  else return(v)
}))

#Dummy variable for Attrition
levels(input_data$Attrition)<-c(1,0)
input_data$Attrition <- as.numeric(levels(input_data$Attrition))[input_data$Attrition]

#Dummy variable for Gender
levels(input_data$Gender)<-c(1,0)
input_data$Gender <- as.numeric(levels(input_data$Gender))[input_data$Gender]

#Dummy variable for extended_working_hrs
levels(input_data$extended_working_hrs)<-c(1,0)
input_data$extended_working_hrs <- as.numeric(levels(input_data$extended_working_hrs))[input_data$extended_working_hrs]

#Dummy variable for BusinessTravel
dummy <- data.frame(model.matrix(~BusinessTravel, data = input_data))
input_data <- cbind(subset(input_data, select = -c(BusinessTravel)), dummy[,-1])

#Dummy variable for Department
dummy <- data.frame(model.matrix(~Department, data = input_data))
input_data <- cbind(subset(input_data, select = -c(Department)), dummy[,-1])

#Dummy variable for EducationField
dummy <- data.frame(model.matrix(~EducationField, data = input_data))
input_data <- cbind(subset(input_data, select = -c(EducationField)), dummy[,-1])

#Dummy variable for JobRole
dummy <- data.frame(model.matrix(~JobRole, data = input_data))
input_data <- cbind(subset(input_data, select = -c(JobRole)), dummy[,-1])

#Dummy variable for MaritalStatus
dummy <- data.frame(model.matrix(~MaritalStatus, data = input_data))
input_data <- cbind(subset(input_data, select = -c(MaritalStatus)), dummy[,-1])

#Dummy variable for Education
input_data$Education <- factor(input_data$Education)
levels(input_data$Education)<-c("Below College","College","Bachelor","Master","Doctor")
dummy <- data.frame(model.matrix(~Education, data = input_data))
input_data <- cbind(subset(input_data, select = -c(Education)), dummy[,-1])

#Dummy variable for EnvironmentSatisfaction
input_data$EnvironmentSatisfaction <- factor(input_data$EnvironmentSatisfaction)
levels(input_data$EnvironmentSatisfaction)<-c("Low","Medium","High","Very High")
dummy <- data.frame(model.matrix(~EnvironmentSatisfaction, data = input_data))
input_data <- cbind(subset(input_data, select = -c(EnvironmentSatisfaction)), dummy[,-1])

#Dummy variable for JobSatisfaction
input_data$JobSatisfaction <- factor(input_data$JobSatisfaction)
levels(input_data$JobSatisfaction)<- c("Low","Medium","High","Very High")
dummy <- data.frame(model.matrix(~JobSatisfaction, data = input_data))
input_data <- cbind(subset(input_data, select = -c(JobSatisfaction)), dummy[,-1])

#Dummy variable for WorkLifeBalance
input_data$WorkLifeBalance <- factor(input_data$WorkLifeBalance)
levels(input_data$WorkLifeBalance)<- c("Bad","Good","Better","Best")
dummy <- data.frame(model.matrix(~WorkLifeBalance, data = input_data))
input_data <- cbind(subset(input_data, select = -c(WorkLifeBalance)), dummy[,-1])

#Dummy variable for JobInvolvement
input_data$JobInvolvement <- factor(input_data$JobInvolvement)
levels(input_data$JobInvolvement)<- c("Low","Medium","High","Very High")
dummy <- data.frame(model.matrix(~JobInvolvement, data = input_data))
input_data <- cbind(subset(input_data, select = -c(JobInvolvement)), dummy[,-1])

#Dummy variable for PerformanceRating
input_data$PerformanceRating <- factor(input_data$PerformanceRating)
levels(input_data$PerformanceRating)<- c("Excellent","Outstanding")
dummy <- data.frame(model.matrix(~PerformanceRating, data = input_data))
input_data <- cbind(subset(input_data, select = -c(PerformanceRating)),subset(dummy, select = c(PerformanceRatingOutstanding)))

#Scaling all the numeric predictors
input_data$DistanceFromHome <- scale(input_data$DistanceFromHome)
input_data$NumCompaniesWorked <- scale(input_data$NumCompaniesWorked)
input_data$PercentSalaryHike <- scale(input_data$PercentSalaryHike)
input_data$StockOptionLevel <- scale(input_data$StockOptionLevel)
input_data$TotalWorkingYears <- scale(input_data$TotalWorkingYears)
input_data$TrainingTimesLastYear <- scale(input_data$TrainingTimesLastYear)
input_data$YearsAtCompany <- scale(input_data$YearsAtCompany)
input_data$YearsSinceLastPromotion <- scale(input_data$YearsSinceLastPromotion)
input_data$YearsWithCurrManager <- scale(input_data$YearsWithCurrManager)
input_data$avg_working_hrs <- scale(input_data$avg_working_hrs)

#The set.seed( ) command is used to reproduce the same results each time while sampling 70%
#data for the training dataset. This is done so that you get the same training data as your sample
#each time you execute the R code
set.seed(100)  

# Use sample function for getting the random number represents 70% of indices. 
training_indexes = sample(1:nrow(input_data), 0.7*nrow(input_data))

#Create training data objects and test data objects
training_data <- input_data[training_indexes,]
test_data <- input_data[-training_indexes,]

#Model building started...

#Create a multi logistic regression model
LR_model_1 <- glm(formula = Attrition ~., data = training_data, family = "binomial")
summary(LR_model_1)

#Many iterations can be skipped through the stepAIC command to remove multi collinear
#and insignificant variables. The last call that stepAIC makes, contains only the variables
#it considers to be important in the model. 
LR_model_2 <- stepAIC(LR_model_1, direction="both")

#Listing VIF value in decreasing order
sort((vif(LR_model_2)), decreasing = TRUE)
summary(LR_model_2)

#Remove EducationFieldLIFE.SCIENCES 
LR_model_3 <- glm(formula = Attrition ~ DistanceFromHome + MonthlyIncome + 
                    NumCompaniesWorked + PercentSalaryHike + TotalWorkingYears + 
                    TrainingTimesLastYear + YearsAtCompany + YearsSinceLastPromotion + 
                    YearsWithCurrManager + extended_working_hrs + BusinessTravelTRAVEL_FREQUENTLY + 
                    BusinessTravelTRAVEL_RARELY + 
                    EducationFieldMARKETING + EducationFieldMEDICAL + EducationFieldOTHER + 
                    EducationFieldTECHNICAL.DEGREE + JobRoleMANUFACTURING.DIRECTOR + 
                    JobRoleRESEARCH.DIRECTOR + JobRoleRESEARCH.SCIENTIST + JobRoleSALES.EXECUTIVE + 
                    MaritalStatusSINGLE + EnvironmentSatisfactionMedium + EnvironmentSatisfactionHigh + 
                    EnvironmentSatisfactionVery.High + JobSatisfactionMedium + 
                    JobSatisfactionHigh + JobSatisfactionVery.High + WorkLifeBalanceGood + 
                    WorkLifeBalanceBetter + WorkLifeBalanceBest + JobInvolvementHigh, 
                    family = "binomial", data = training_data)

#Listing VIF value in decreasing order
sort((vif(LR_model_3)), decreasing = TRUE)
summary(LR_model_3)


#Remove YearsAtCompany 
LR_model_4 <- glm(formula = Attrition ~ DistanceFromHome + MonthlyIncome + 
                    NumCompaniesWorked + PercentSalaryHike + TotalWorkingYears + 
                    TrainingTimesLastYear + YearsSinceLastPromotion + 
                    YearsWithCurrManager + extended_working_hrs + BusinessTravelTRAVEL_FREQUENTLY + 
                    BusinessTravelTRAVEL_RARELY + 
                    EducationFieldMARKETING + EducationFieldMEDICAL + EducationFieldOTHER + 
                    EducationFieldTECHNICAL.DEGREE + JobRoleMANUFACTURING.DIRECTOR + 
                    JobRoleRESEARCH.DIRECTOR + JobRoleRESEARCH.SCIENTIST + JobRoleSALES.EXECUTIVE + 
                    MaritalStatusSINGLE + EnvironmentSatisfactionMedium + EnvironmentSatisfactionHigh + 
                    EnvironmentSatisfactionVery.High + JobSatisfactionMedium + 
                    JobSatisfactionHigh + JobSatisfactionVery.High + WorkLifeBalanceGood + 
                    WorkLifeBalanceBetter + WorkLifeBalanceBest + JobInvolvementHigh, 
                  family = "binomial", data = training_data)

#Listing VIF value in decreasing order
sort((vif(LR_model_4)), decreasing = TRUE)
summary(LR_model_4)

#Remove BusinessTravelTRAVEL_RARELY 
LR_model_5 <- glm(formula = Attrition ~ DistanceFromHome + MonthlyIncome + 
                    NumCompaniesWorked + PercentSalaryHike + TotalWorkingYears + 
                    TrainingTimesLastYear + YearsSinceLastPromotion + 
                    YearsWithCurrManager + extended_working_hrs + BusinessTravelTRAVEL_FREQUENTLY +
                    EducationFieldMARKETING + EducationFieldMEDICAL + EducationFieldOTHER + 
                    EducationFieldTECHNICAL.DEGREE + JobRoleMANUFACTURING.DIRECTOR + 
                    JobRoleRESEARCH.DIRECTOR + JobRoleRESEARCH.SCIENTIST + JobRoleSALES.EXECUTIVE + 
                    MaritalStatusSINGLE + EnvironmentSatisfactionMedium + EnvironmentSatisfactionHigh + 
                    EnvironmentSatisfactionVery.High + JobSatisfactionMedium + 
                    JobSatisfactionHigh + JobSatisfactionVery.High + WorkLifeBalanceGood + 
                    WorkLifeBalanceBetter + WorkLifeBalanceBest + JobInvolvementHigh, 
                  family = "binomial", data = training_data)

#Listing VIF value in decreasing order
sort((vif(LR_model_5)), decreasing = TRUE)
summary(LR_model_5)

#Remove WorkLifeBalanceGood 
LR_model_6 <- glm(formula = Attrition ~ DistanceFromHome + MonthlyIncome + 
                    NumCompaniesWorked + PercentSalaryHike + TotalWorkingYears + 
                    TrainingTimesLastYear + YearsSinceLastPromotion + 
                    YearsWithCurrManager + extended_working_hrs + 
                    BusinessTravelTRAVEL_RARELY + 
                    EducationFieldMARKETING + EducationFieldMEDICAL + EducationFieldOTHER + 
                    EducationFieldTECHNICAL.DEGREE + JobRoleMANUFACTURING.DIRECTOR + 
                    JobRoleRESEARCH.DIRECTOR + JobRoleRESEARCH.SCIENTIST + JobRoleSALES.EXECUTIVE + 
                    MaritalStatusSINGLE + EnvironmentSatisfactionMedium + EnvironmentSatisfactionHigh + 
                    EnvironmentSatisfactionVery.High + JobSatisfactionMedium + 
                    JobSatisfactionHigh + JobSatisfactionVery.High + 
                    WorkLifeBalanceBetter + WorkLifeBalanceBest + JobInvolvementHigh, 
                  family = "binomial", data = training_data)

#Listing VIF value in decreasing order
sort((vif(LR_model_6)), decreasing = TRUE)
summary(LR_model_6)

#Now All the variable have VIF less then 2. So remove insignificant variable, whose p-value is high.

#Remove DistanceFromHome 
LR_model_7 <- glm(formula = Attrition ~ MonthlyIncome + 
                    NumCompaniesWorked + PercentSalaryHike + TotalWorkingYears + 
                    TrainingTimesLastYear + YearsSinceLastPromotion + 
                    YearsWithCurrManager + extended_working_hrs + 
                    BusinessTravelTRAVEL_RARELY + 
                    EducationFieldMARKETING + EducationFieldMEDICAL + EducationFieldOTHER + 
                    EducationFieldTECHNICAL.DEGREE + JobRoleMANUFACTURING.DIRECTOR + 
                    JobRoleRESEARCH.DIRECTOR + JobRoleRESEARCH.SCIENTIST + JobRoleSALES.EXECUTIVE + 
                    MaritalStatusSINGLE + EnvironmentSatisfactionMedium + EnvironmentSatisfactionHigh + 
                    EnvironmentSatisfactionVery.High + JobSatisfactionMedium + 
                    JobSatisfactionHigh + JobSatisfactionVery.High + 
                    WorkLifeBalanceBetter + WorkLifeBalanceBest + JobInvolvementHigh, 
                  family = "binomial", data = training_data)


summary(LR_model_7)

#Remove EducationFieldMEDICAL 
LR_model_8 <- glm(formula = Attrition ~ MonthlyIncome + 
                    NumCompaniesWorked + PercentSalaryHike + TotalWorkingYears + 
                    TrainingTimesLastYear + YearsSinceLastPromotion + 
                    YearsWithCurrManager + extended_working_hrs + 
                    BusinessTravelTRAVEL_RARELY + 
                    EducationFieldMARKETING + EducationFieldOTHER + 
                    EducationFieldTECHNICAL.DEGREE + JobRoleMANUFACTURING.DIRECTOR + 
                    JobRoleRESEARCH.DIRECTOR + JobRoleRESEARCH.SCIENTIST + JobRoleSALES.EXECUTIVE + 
                    MaritalStatusSINGLE + EnvironmentSatisfactionMedium + EnvironmentSatisfactionHigh + 
                    EnvironmentSatisfactionVery.High + JobSatisfactionMedium + 
                    JobSatisfactionHigh + JobSatisfactionVery.High + 
                    WorkLifeBalanceBetter + WorkLifeBalanceBest + JobInvolvementHigh, 
                  family = "binomial", data = training_data)


summary(LR_model_8)

#Remove WorkLifeBalanceBest 
LR_model_9 <- glm(formula = Attrition ~ MonthlyIncome + 
                    NumCompaniesWorked + PercentSalaryHike + TotalWorkingYears + 
                    TrainingTimesLastYear + YearsSinceLastPromotion + 
                    YearsWithCurrManager + extended_working_hrs + 
                    BusinessTravelTRAVEL_RARELY + 
                    EducationFieldMARKETING + EducationFieldOTHER + 
                    EducationFieldTECHNICAL.DEGREE + JobRoleMANUFACTURING.DIRECTOR + 
                    JobRoleRESEARCH.DIRECTOR + JobRoleRESEARCH.SCIENTIST + JobRoleSALES.EXECUTIVE + 
                    MaritalStatusSINGLE + EnvironmentSatisfactionMedium + EnvironmentSatisfactionHigh + 
                    EnvironmentSatisfactionVery.High + JobSatisfactionMedium + 
                    JobSatisfactionHigh + JobSatisfactionVery.High + 
                    WorkLifeBalanceBetter + JobInvolvementHigh, 
                  family = "binomial", data = training_data)


summary(LR_model_9)


#Remove EducationFieldMARKETING 
LR_model_10 <- glm(formula = Attrition ~ MonthlyIncome + 
                    NumCompaniesWorked + PercentSalaryHike + TotalWorkingYears + 
                    TrainingTimesLastYear + YearsSinceLastPromotion + 
                    YearsWithCurrManager + extended_working_hrs + 
                    BusinessTravelTRAVEL_RARELY + 
                    EducationFieldOTHER + 
                    EducationFieldTECHNICAL.DEGREE + JobRoleMANUFACTURING.DIRECTOR + 
                    JobRoleRESEARCH.DIRECTOR + JobRoleRESEARCH.SCIENTIST + JobRoleSALES.EXECUTIVE + 
                    MaritalStatusSINGLE + EnvironmentSatisfactionMedium + EnvironmentSatisfactionHigh + 
                    EnvironmentSatisfactionVery.High + JobSatisfactionMedium + 
                    JobSatisfactionHigh + JobSatisfactionVery.High + 
                    WorkLifeBalanceBetter + JobInvolvementHigh, 
                  family = "binomial", data = training_data)


summary(LR_model_10)

#Remove JobRoleRESEARCH.SCIENTIST 
LR_model_11 <- glm(formula = Attrition ~ MonthlyIncome + 
                     NumCompaniesWorked + PercentSalaryHike + TotalWorkingYears + 
                     TrainingTimesLastYear + YearsSinceLastPromotion + 
                     YearsWithCurrManager + extended_working_hrs + 
                     BusinessTravelTRAVEL_RARELY + 
                     EducationFieldOTHER + 
                     EducationFieldTECHNICAL.DEGREE + JobRoleMANUFACTURING.DIRECTOR + 
                     JobRoleRESEARCH.DIRECTOR + JobRoleSALES.EXECUTIVE + 
                     MaritalStatusSINGLE + EnvironmentSatisfactionMedium + EnvironmentSatisfactionHigh + 
                     EnvironmentSatisfactionVery.High + JobSatisfactionMedium + 
                     JobSatisfactionHigh + JobSatisfactionVery.High + 
                     WorkLifeBalanceBetter + JobInvolvementHigh, 
                   family = "binomial", data = training_data)


summary(LR_model_11)

#Remove JobRoleSALES.EXECUTIVE 
LR_model_12 <- glm(formula = Attrition ~ MonthlyIncome + 
                     NumCompaniesWorked + PercentSalaryHike + TotalWorkingYears + 
                     TrainingTimesLastYear + YearsSinceLastPromotion + 
                     YearsWithCurrManager + extended_working_hrs + 
                     BusinessTravelTRAVEL_RARELY + 
                     EducationFieldOTHER + 
                     EducationFieldTECHNICAL.DEGREE + JobRoleMANUFACTURING.DIRECTOR + 
                     JobRoleRESEARCH.DIRECTOR + 
                     MaritalStatusSINGLE + EnvironmentSatisfactionMedium + EnvironmentSatisfactionHigh + 
                     EnvironmentSatisfactionVery.High + JobSatisfactionMedium + 
                     JobSatisfactionHigh + JobSatisfactionVery.High + 
                     WorkLifeBalanceBetter + JobInvolvementHigh, 
                   family = "binomial", data = training_data)


summary(LR_model_12)

#Remove JobInvolvementHigh 
LR_model_13 <- glm(formula = Attrition ~ MonthlyIncome + 
                     NumCompaniesWorked + PercentSalaryHike + TotalWorkingYears + 
                     TrainingTimesLastYear + YearsSinceLastPromotion + 
                     YearsWithCurrManager + extended_working_hrs + 
                     BusinessTravelTRAVEL_RARELY + 
                     EducationFieldOTHER + 
                     EducationFieldTECHNICAL.DEGREE + JobRoleMANUFACTURING.DIRECTOR + 
                     JobRoleRESEARCH.DIRECTOR + 
                     MaritalStatusSINGLE + EnvironmentSatisfactionMedium + EnvironmentSatisfactionHigh + 
                     EnvironmentSatisfactionVery.High + JobSatisfactionMedium + 
                     JobSatisfactionHigh + JobSatisfactionVery.High + 
                     WorkLifeBalanceBetter, 
                   family = "binomial", data = training_data)


summary(LR_model_13)

#Remove PercentSalaryHike 
LR_model_14 <- glm(formula = Attrition ~ MonthlyIncome + 
                     NumCompaniesWorked + TotalWorkingYears + 
                     TrainingTimesLastYear + YearsSinceLastPromotion + 
                     YearsWithCurrManager + extended_working_hrs + 
                     BusinessTravelTRAVEL_RARELY + 
                     EducationFieldOTHER + 
                     EducationFieldTECHNICAL.DEGREE + JobRoleMANUFACTURING.DIRECTOR + 
                     JobRoleRESEARCH.DIRECTOR + 
                     MaritalStatusSINGLE + EnvironmentSatisfactionMedium + EnvironmentSatisfactionHigh + 
                     EnvironmentSatisfactionVery.High + JobSatisfactionMedium + 
                     JobSatisfactionHigh + JobSatisfactionVery.High + 
                     WorkLifeBalanceBetter, 
                   family = "binomial", data = training_data)


summary(LR_model_14)

#Remove MonthlyIncome 
LR_model_15 <- glm(formula = Attrition ~ NumCompaniesWorked + TotalWorkingYears + 
                     TrainingTimesLastYear + YearsSinceLastPromotion + 
                     YearsWithCurrManager + extended_working_hrs + 
                     BusinessTravelTRAVEL_RARELY + 
                     EducationFieldOTHER + 
                     EducationFieldTECHNICAL.DEGREE + JobRoleMANUFACTURING.DIRECTOR + 
                     JobRoleRESEARCH.DIRECTOR + 
                     MaritalStatusSINGLE + EnvironmentSatisfactionMedium + EnvironmentSatisfactionHigh + 
                     EnvironmentSatisfactionVery.High + JobSatisfactionMedium + 
                     JobSatisfactionHigh + JobSatisfactionVery.High + 
                     WorkLifeBalanceBetter, 
                   family = "binomial", data = training_data)


summary(LR_model_15)

#Remove JobRoleRESEARCH.DIRECTOR 
LR_model_16 <- glm(formula = Attrition ~ NumCompaniesWorked + TotalWorkingYears + 
                     TrainingTimesLastYear + YearsSinceLastPromotion + 
                     YearsWithCurrManager + extended_working_hrs + 
                     BusinessTravelTRAVEL_RARELY + 
                     EducationFieldOTHER + 
                     EducationFieldTECHNICAL.DEGREE + JobRoleMANUFACTURING.DIRECTOR + 
                     MaritalStatusSINGLE + EnvironmentSatisfactionMedium + EnvironmentSatisfactionHigh + 
                     EnvironmentSatisfactionVery.High + JobSatisfactionMedium + 
                     JobSatisfactionHigh + JobSatisfactionVery.High + 
                     WorkLifeBalanceBetter, 
                   family = "binomial", data = training_data)

summary(LR_model_16)

#Remove EducationFieldTECHNICAL.DEGREE 
LR_model_17 <- glm(formula = Attrition ~ NumCompaniesWorked + TotalWorkingYears + 
                     TrainingTimesLastYear + YearsSinceLastPromotion + 
                     YearsWithCurrManager + extended_working_hrs + 
                     BusinessTravelTRAVEL_RARELY + 
                     EducationFieldOTHER + JobRoleMANUFACTURING.DIRECTOR + 
                     MaritalStatusSINGLE + EnvironmentSatisfactionMedium + EnvironmentSatisfactionHigh + 
                     EnvironmentSatisfactionVery.High + JobSatisfactionMedium + 
                     JobSatisfactionHigh + JobSatisfactionVery.High + 
                     WorkLifeBalanceBetter, 
                   family = "binomial", data = training_data)

summary(LR_model_17)

#Remove EducationFieldOTHER 
LR_model_18 <- glm(formula = Attrition ~ NumCompaniesWorked + TotalWorkingYears + 
                     TrainingTimesLastYear + YearsSinceLastPromotion + 
                     YearsWithCurrManager + extended_working_hrs + 
                     BusinessTravelTRAVEL_RARELY + 
                     JobRoleMANUFACTURING.DIRECTOR + 
                     MaritalStatusSINGLE + EnvironmentSatisfactionMedium + EnvironmentSatisfactionHigh + 
                     EnvironmentSatisfactionVery.High + JobSatisfactionMedium + 
                     JobSatisfactionHigh + JobSatisfactionVery.High + 
                     WorkLifeBalanceBetter, 
                   family = "binomial", data = training_data)

summary(LR_model_18)

#Remove BusinessTravelTRAVEL_RARELY 
LR_model_19 <- glm(formula = Attrition ~ NumCompaniesWorked + TotalWorkingYears + 
                     TrainingTimesLastYear + YearsSinceLastPromotion + 
                     YearsWithCurrManager + extended_working_hrs +
                     JobRoleMANUFACTURING.DIRECTOR + 
                     MaritalStatusSINGLE + EnvironmentSatisfactionMedium + EnvironmentSatisfactionHigh + 
                     EnvironmentSatisfactionVery.High + JobSatisfactionMedium + 
                     JobSatisfactionHigh + JobSatisfactionVery.High + 
                     WorkLifeBalanceBetter, 
                   family = "binomial", data = training_data)

summary(LR_model_19)

#Remove JobSatisfactionMedium 
LR_model_20 <- glm(formula = Attrition ~ NumCompaniesWorked + TotalWorkingYears + 
                     TrainingTimesLastYear + YearsSinceLastPromotion + 
                     YearsWithCurrManager + extended_working_hrs +
                     JobRoleMANUFACTURING.DIRECTOR + 
                     MaritalStatusSINGLE + EnvironmentSatisfactionMedium + EnvironmentSatisfactionHigh + 
                     EnvironmentSatisfactionVery.High + 
                     JobSatisfactionHigh + JobSatisfactionVery.High + 
                     WorkLifeBalanceBetter, 
                   family = "binomial", data = training_data)

summary(LR_model_20)

#Remove JobSatisfactionHigh 
LR_model_21 <- glm(formula = Attrition ~ NumCompaniesWorked + TotalWorkingYears + 
                     TrainingTimesLastYear + YearsSinceLastPromotion + 
                     YearsWithCurrManager + extended_working_hrs +
                     JobRoleMANUFACTURING.DIRECTOR + 
                     MaritalStatusSINGLE + EnvironmentSatisfactionMedium + EnvironmentSatisfactionHigh + 
                     EnvironmentSatisfactionVery.High + 
                     JobSatisfactionVery.High + 
                     WorkLifeBalanceBetter, 
                   family = "binomial", data = training_data)

summary(LR_model_21)

#Remove TrainingTimesLastYear 
LR_model_22 <- glm(formula = Attrition ~ NumCompaniesWorked + TotalWorkingYears + 
                     YearsSinceLastPromotion + 
                     YearsWithCurrManager + extended_working_hrs +
                     JobRoleMANUFACTURING.DIRECTOR + 
                     MaritalStatusSINGLE + EnvironmentSatisfactionMedium + EnvironmentSatisfactionHigh + 
                     EnvironmentSatisfactionVery.High + 
                     JobSatisfactionVery.High + 
                     WorkLifeBalanceBetter, 
                   family = "binomial", data = training_data)

summary(LR_model_22)

#Remove JobRoleMANUFACTURING.DIRECTOR 
LR_model_final <- glm(formula = Attrition ~ NumCompaniesWorked + TotalWorkingYears + 
                     YearsSinceLastPromotion + YearsWithCurrManager + extended_working_hrs +
                     MaritalStatusSINGLE + EnvironmentSatisfactionMedium + EnvironmentSatisfactionHigh + 
                     EnvironmentSatisfactionVery.High + JobSatisfactionVery.High + WorkLifeBalanceBetter, 
                   family = "binomial", data = training_data)

summary(LR_model_final)

#Model Evaluation started...

#Predict the Attrition by giving test_data as input into LR_model_final.It can be used to evaluate our model's efficiency.
prediction_data <- predict(LR_model_final,type = "response",subset(test_data, select = -c(Attrition)))

#using probability cut-off as 50%
predicted_attrition <- factor(ifelse(prediction_data >= 0.50, "Yes", "No"))
actual_attrition <- factor(ifelse(test_data$Attrition == 1, "Yes","No"))

## checking statistics with confusion matrix
confusion_matrix <- table(predicted_attrition,actual_attrition)
confusionMatrix(confusion_matrix, positive = "Yes")

#To find the optimal probalility cutoff value where our model is predicting efficiently.
check_model_performance <- function(probability_cutoff) 
{
  predicted_attrition_temp <- factor(ifelse(prediction_data >= probability_cutoff, "Yes", "No"))
  confusion_matrix_temp    <- confusionMatrix(predicted_attrition_temp,actual_attrition, positive = "Yes")
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
for(i in 1:100)
{
  model_evaluation_matrix[i,] <- check_model_performance(prob_cuttoff_sample[i])
} 

optimal_prob_cutoff <- prob_cuttoff_sample[which.min(abs(model_evaluation_matrix[,2] - model_evaluation_matrix[,3]))]

#using probability cut-off as optimal_prob_cutoff
optimal_predicted_attrition <- factor(ifelse(prediction_data >= optimal_prob_cutoff, "Yes", "No"))

## checking statistics with confusion matrix
confusion_matrix <- table(optimal_predicted_attrition, actual_attrition)
confusionMatrix(confusion_matrix, positive = "Yes")

#Evaluation using KS statistic
optimal_predicted_attrition <- ifelse(optimal_predicted_attrition == "Yes", 1, 0)
actual_attrition <- ifelse(actual_attrition == "Yes", 1, 0)

optimal_prediction_test<- prediction(optimal_predicted_attrition, actual_attrition)
performance_measures_test<- performance(optimal_prediction_test, "tpr", "fpr")

ks_table_test <- attr(performance_measures_test, "y.values")[[1]] - (attr(performance_measures_test, "x.values")[[1]])
max(ks_table_test)

#Model Evaluation ends
