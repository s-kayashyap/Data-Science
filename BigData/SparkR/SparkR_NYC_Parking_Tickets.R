# New York City Parking Ticket Case Study
#
# Scope                 : To understand the parking violation dataset. We will compare metrics/phenomenon related to 
#                         Parking Tickets over three different years-2015, 2016 and 2017.
#
# Uses                  : New York City is a thriving metropolis. Just like most other metros that size, one of the
#                         biggest problems its citizens face, is parking. The classic combination of a huge number of
#                         cars, and a cramped geography is the exact recipe that leads to a huge number of parking tickets.
#
# Input datasets        : This dataset is open source and available at Kaggle.com for download using the following links.
# https://www.kaggle.com/new-york-city/nyc-parking-tickets/data#Parking_Violations_Issued_-_Fiscal_Year_2015.csv
# https://www.kaggle.com/new-york-city/nyc-parking-tickets/data#Parking_Violations_Issued_-_Fiscal_Year_2016.csv
# https://www.kaggle.com/new-york-city/nyc-parking-tickets/data#Parking_Violations_Issued_-_Fiscal_Year_2017.csv
# 
# Developer : Swami Prem Pranav Kayashyap (APFE1786831)
#        
# ------------------------------------------------------
# Date : 09th Sep, 2018

# Methodology: We will conduct the analysis in 3 stages.
#
# Stage-1: Data Quality Verification and Cleaning.
# Stage-2: Overview and Examining the dataset.
# Stage 3: Aggregation tasks - comparing metrics and insights across the years
#*******************************************************

# Pre-Requisites:
# We will utilize Hadoop platform and HDFS for storing and analysis of the datasets
# Following steps need to be performed before running this code:
# 1. Create the directory /user/skashyap733_gmail/mydata/nyc_parking to keep this three datasets. 
# 2. Copy the datasets to above directory. We have copied dataset from /common_folder/nyc_parking to
#    /user/skashyap733_gmail/mydata/nyc_parking using web console by using below command.
#    hadoop fs -cp /common_folder/nyc_parking/* /user/skashyap733_gmail/mydata/nyc_parking/*
# 3. Launch RStudio (Spark should be installed) and execute the code.
#*******************************************************

# Load the necessary R packages
spark_path <- '/usr/local/spark'
if (nchar(Sys.getenv("SPARK_HOME")) < 1) {
  Sys.setenv(SPARK_HOME = spark_path)
}
library(SparkR, lib.loc = c(file.path(Sys.getenv("SPARK_HOME"), "R", "lib")))  # load SparkR to utilize spark environment
library(dplyr)
library(stringr)
library(ggplot2)

# Initialise a spark session.
sparkR.session(master = "yarn-clients", sparkConfig = list(spark.driver.memory = "1g"))

# Import the dataset files into dataframes in R environment
nyc_parking_tkts_2015 <- read.df(path="hdfs:///common_folder/nyc_parking/Parking_Violations_Issued_-_Fiscal_Year_2015.csv", source= "csv", inferSchema= "true", header= "true")
nyc_parking_tkts_2016 <- read.df(path="hdfs:///common_folder/nyc_parking/Parking_Violations_Issued_-_Fiscal_Year_2016.csv", source= "csv", inferSchema= "true", header= "true")
nyc_parking_tkts_2017 <- read.df(path="hdfs:///common_folder/nyc_parking/Parking_Violations_Issued_-_Fiscal_Year_2017.csv", source= "csv", inferSchema= "true", header= "true")

#
# Stage-1: Data Quality Verification and Cleaning.
#

# Cleaning Column names of the dataframes nyc_parking_tkts_2015
# we will remove white spaces between consecutive words and seperate it with "_". and we will remove special characters "?".
colnames(nyc_parking_tkts_2015)<- str_trim(colnames(nyc_parking_tkts_2015), side= "both")
colnames(nyc_parking_tkts_2015)<- str_replace_all(colnames(nyc_parking_tkts_2015), pattern=" ", replacement = "_")
colnames(nyc_parking_tkts_2015)<- str_replace_all(colnames(nyc_parking_tkts_2015), pattern="\\?", replacement = "")
colnames(nyc_parking_tkts_2015)

# Cleaning Column names of the dataframes nyc_parking_tkts_2016
# we will remove white spaces between consecutive words and seperate it with "_". and we will remove special characters "?".
colnames(nyc_parking_tkts_2016)<- str_trim(colnames(nyc_parking_tkts_2016), side= "both")
colnames(nyc_parking_tkts_2016)<- str_replace_all(colnames(nyc_parking_tkts_2016), pattern=" ", replacement = "_")
colnames(nyc_parking_tkts_2016)<- str_replace_all(colnames(nyc_parking_tkts_2016), pattern="\\?", replacement = "")
colnames(nyc_parking_tkts_2016)

# Cleaning Column names of the dataframes nyc_parking_tkts_2017
# we will remove white spaces between consecutive words and seperate it with "_". and we will remove special characters "?".
colnames(nyc_parking_tkts_2017)<- str_trim(colnames(nyc_parking_tkts_2017), side= "both")
colnames(nyc_parking_tkts_2017)<- str_replace_all(colnames(nyc_parking_tkts_2017), pattern=" ", replacement = "_")
colnames(nyc_parking_tkts_2017)<- str_replace_all(colnames(nyc_parking_tkts_2017), pattern="\\?", replacement = "")
colnames(nyc_parking_tkts_2017)

# Understanding the dimensions and structure of the dataframe nyc_parking_tkts_2015
printSchema(nyc_parking_tkts_2015)
dim(nyc_parking_tkts_2015)
# Rows: 11,809,233 | Columns: 51

# Understanding the dimensions and structure of the dataframe nyc_parking_tkts_2016
printSchema(nyc_parking_tkts_2016)
dim(nyc_parking_tkts_2016)
# Rows: 10,626,899 | Columns: 51

# Understanding the dimensions and structure of the dataframe nyc_parking_tkts_2017
printSchema(nyc_parking_tkts_2017)
dim(nyc_parking_tkts_2017)
# Rows: 10,803,028 | Columns: 43

# From dimensions and structure analysis it is clear that the dataframes have varying number of columns. As the columns "Latitude",
# "Longitude", "Community_Board", "Community_Council", "Census_Tract", "BIN", "BBL" and "NTA" are logged for the fiscal year 2015,
# 2016 but not for 2017. So, We will be removing these columns during the detailed analysis.

# Detailed Data Quality Verification of nyc_parking_tkts_2015

# Removing duplicate rows in the dataset [Two or More rows having the same Summons_Number are duplicate]
nyc_parking_tkts_2015<- dropDuplicates(nyc_parking_tkts_2015, "Summons_Number")
dim(nyc_parking_tkts_2015)
#  Rows: 10,951,256 | Columns: 51

# Since ticket issue date is an critical parameter. Let us check if there are any missing values in it.
createOrReplaceTempView(nyc_parking_tkts_2015, "nyc_parking_tkts_2015_view")
count_null_issue_date_2015 <- SparkR::sql("SELECT SUM(CASE WHEN issue_date IS NULL THEN 1
                                          ELSE 0
                                          END) null_issue_date,
                                          COUNT(*) num_of_rows
                                          FROM nyc_parking_tkts_2015_view")
head(count_null_issue_date_2015)
#There are no records with missing Issue Date or Null Issue Date

# Converting the date paramters[Issue_Date, Vehicle_Expiration_Date and Date_First_Observed] to a suitable format for analysis.
nyc_parking_tkts_2015$Issue_Date <- SparkR::to_date(nyc_parking_tkts_2015$Issue_Date, 'MM/dd/yyyy')
nyc_parking_tkts_2015$Vehicle_Expiration_Date <- SparkR::to_date(nyc_parking_tkts_2015$Vehicle_Expiration_Date, 'yyyyMMdd')
nyc_parking_tkts_2015$Date_First_Observed <- SparkR::to_date(nyc_parking_tkts_2015$Date_First_Observed, 'yyyyMMdd')

# Let's Understand the Range of ticket Issue Dates Available in the Dataset nyc_parking_tkts_2015
createOrReplaceTempView(nyc_parking_tkts_2015, "nyc_parking_tkts_2015_view")
range_issue_date_2015 <- SparkR::sql("SELECT min(issue_date) as min_issue_date_2015, max(issue_date) as max_issue_date_2015
                                     FROM nyc_parking_tkts_2015_view")
head(range_issue_date_2015)
# Min_IssueDate_2015 : 1985-07-16
# Max_IssueDate_2015 : 2015-06-30
# The Issue Tickets range between 16th July 1985 to 30th June 2015. Clearly this is Nonconforming.

# We Will create Additional Columns in the Dataset that Correspond to the Year and Month of Ticket Issue
nyc_parking_tkts_2015$Issue_Year <- year(nyc_parking_tkts_2015$Issue_Date)
nyc_parking_tkts_2015$Issue_Month <- month(nyc_parking_tkts_2015$Issue_Date)

#Now let's observe the Distribution of Issue Date.
createOrReplaceTempView(nyc_parking_tkts_2015, "nyc_parking_tkts_2015_view")
count_per_issue_year_2015 <- SparkR::sql("SELECT Issue_Year, Issue_Month, count(*) as Num_of_Records
                                         FROM nyc_parking_tkts_2015_view
                                         GROUP BY Issue_Year,Issue_Month
                                         ORDER BY Issue_Year desc, Issue_Month desc")
head(count_per_issue_year_2015, nrow(count_per_issue_year_2015))

# Subsetting the DataFrame according to the Fiscal Year (July,2014 to June,2015).[Read Assumptions For Justification]
nyc_parking_tkts_2015 <- nyc_parking_tkts_2015[nyc_parking_tkts_2015$Issue_Date >= "2014-07-01" & nyc_parking_tkts_2015$Issue_Date <= "2015-06-30"]
nrow(nyc_parking_tkts_2015)
# 10,598,035 records

# The columns "Latitude", "Longitude", "Community_Board", "Community_Council", "Census_Tract", "BIN", "BBL" and "NTA" are present
# for the fiscal year 2015 and 2016 but not for 2017. We will Check the number of null values in these columns and remove if necessary. 
createOrReplaceTempView(nyc_parking_tkts_2015, "nyc_parking_tkts_2015_view")
null_count_column_2015 <- SparkR::sql("SELECT COUNT(*) Num_of_Rows,
                                      SUM(CASE WHEN No_Standing_or_Stopping_Violation IS NULL THEN 1 ELSE 0
                                      END) null_stand_stop_violation,
                                      SUM(CASE WHEN Hydrant_Violation IS NULL THEN 1 ELSE 0
                                      END) null_hydrant_violation,
                                      SUM(CASE WHEN Double_Parking_Violation IS NULL THEN 1 ELSE 0
                                      END) null_double_parking_violation,
                                      SUM(CASE WHEN Latitude IS NULL THEN 1 ELSE 0
                                      END) Nulls_Latitude,
                                      SUM(CASE WHEN Longitude IS NULL THEN 1 ELSE 0
                                      END) null_longitude,
                                      SUM(CASE WHEN Community_Board IS NULL THEN 1 ELSE 0
                                      END) Null_community_board,
                                      SUM(CASE WHEN Community_Council IS NULL THEN 1 ELSE 0
                                      END) nulls_community_council,
                                      SUM(CASE WHEN Census_Tract IS NULL THEN 1 ELSE 0
                                      END) nulls_census_tract,
                                      SUM(CASE WHEN BIN IS NULL THEN 1 ELSE 0
                                      END) nulls_bin,
                                      SUM(CASE WHEN BBL IS NULL THEN 1 ELSE 0
                                      END) null_bbl,
                                      SUM(CASE WHEN NTA IS NULL THEN 1 ELSE 0
                                      END) null_nta     
                                      FROM nyc_parking_tkts_2015_view")
head(null_count_column_2015)
# These columns have only null values therefore they should be dropped to standardize the dataset between the years. Dropping these columns.
nyc_parking_tkts_2015<- drop(nyc_parking_tkts_2015, c("No_Standing_or_Stopping_Violation",
                                                      "Hydrant_Violation",
                                                      "Double_Parking_Violation",
                                                      "Latitude",
                                                      "Longitude",
                                                      "Community_Board",
                                                      "Community_Council",
                                                      "Census_Tract",
                                                      "BIN",
                                                      "BBL",
                                                      "NTA") )
colnames(nyc_parking_tkts_2015)

# Correcting the format for the Violation_Time, Time_First_Observed, From_Hours_In_Effect and To_Hours_In_Effect. We can observe that
# the string format of these time attributes include only a partial component of AM/PM. Therefore we will append M at the end of each
# time attribute before converting it into a timestamp.
nyc_parking_tkts_2015$Concat_M <- "M"
nyc_parking_tkts_2015$Violation_Time <- concat(nyc_parking_tkts_2015$Violation_Time,  nyc_parking_tkts_2015$Concat_M)
nyc_parking_tkts_2015$Time_First_Observed <- concat(nyc_parking_tkts_2015$Time_First_Observed, nyc_parking_tkts_2015$Concat_M)
nyc_parking_tkts_2015$From_Hours_In_Effect <- concat(nyc_parking_tkts_2015$From_Hours_In_Effect, nyc_parking_tkts_2015$Concat_M)
nyc_parking_tkts_2015$To_Hours_In_Effect <- concat(nyc_parking_tkts_2015$To_Hours_In_Effect, nyc_parking_tkts_2015$Concat_M)
nyc_parking_tkts_2015<- drop(nyc_parking_tkts_2015, c("Concat_M"))

# Extracting Violation Hour, Violation Minute and 12 hour clock format.
nyc_parking_tkts_2015$Violation_Hour <- substr(nyc_parking_tkts_2015$Violation_Time, 1, 2)
nyc_parking_tkts_2015$Violation_Minute <- substr(nyc_parking_tkts_2015$Violation_Time, 3, 4)
nyc_parking_tkts_2015$Violation_Clock <- substr(nyc_parking_tkts_2015$Violation_Time, 5, 6)

# We've observed that there are few records that have both 00xxAM as well as 12xxAM. Therefore we will replace all 00xxAM to 12xxAM
nyc_parking_tkts_2015$Violation_Hour <- regexp_replace(x = nyc_parking_tkts_2015$Violation_Hour,pattern = "\\00",replacement = "12")

# Concatenating the components into a standardized Violation Time.
nyc_parking_tkts_2015$Violation_Time <- concat(nyc_parking_tkts_2015$Violation_Hour, nyc_parking_tkts_2015$Violation_Minute, nyc_parking_tkts_2015$Violation_Clock)


#Converting Violation_Time, Time_First_Observed, From_Hours_In_Effect, To_Hours_In_Effect into a TimeStamp
nyc_parking_tkts_2015$Violation_Time<-to_timestamp(x = nyc_parking_tkts_2015$Violation_Time, format = "hhmma")
nyc_parking_tkts_2015$Time_First_Observed<- to_timestamp(x= nyc_parking_tkts_2015$Time_First_Observed, format = "hhmma")
nyc_parking_tkts_2015$From_Hours_In_Effect<- to_timestamp(x= nyc_parking_tkts_2015$From_Hours_In_Effect, format = "hhmma")
nyc_parking_tkts_2015$To_Hours_In_Effect<- to_timestamp(x= nyc_parking_tkts_2015$To_Hours_In_Effect, format = "hhmma")

# The dimensions of Formatted and Cleaned Dataset that will be used for Analysis:
printSchema(nyc_parking_tkts_2015)
head(nyc_parking_tkts_2015)
dim(nyc_parking_tkts_2015)
#Rows: 10,598,035 | Columns: 45

# Detailed Data Quality Verification of nyc_parking_tkts_2016

# Removing duplicate rows in the dataset [Two or More rows having the same Summons_Number are duplicate]
nyc_parking_tkts_2016<- dropDuplicates(nyc_parking_tkts_2016, "Summons_Number")
dim(nyc_parking_tkts_2016)
#  Rows: 10,626,899 | Columns: 51

# Since ticket issue date is an critical parameter. Let us check if there are any missing values in it.
createOrReplaceTempView(nyc_parking_tkts_2016, "nyc_parking_tkts_2016_view")
count_null_issue_date_2016 <- SparkR::sql("SELECT SUM(CASE WHEN issue_date IS NULL THEN 1
                                          ELSE 0
                                          END) null_issue_date,
                                          COUNT(*) num_of_rows
                                          FROM nyc_parking_tkts_2016_view")
head(count_null_issue_date_2016)
#There are no records with missing Issue Date or Null Issue Date

# Converting the date paramters[Issue_Date, Vehicle_Expiration_Date and Date_First_Observed] to a suitable format for analysis.
nyc_parking_tkts_2016$Issue_Date <- SparkR::to_date(nyc_parking_tkts_2016$Issue_Date, 'MM/dd/yyyy')
nyc_parking_tkts_2016$Vehicle_Expiration_Date <- SparkR::to_date(cast(nyc_parking_tkts_2016$Vehicle_Expiration_Date,"string"), 'yyyyMMdd')
nyc_parking_tkts_2016$Date_First_Observed <- SparkR::to_date(cast(nyc_parking_tkts_2016$Date_First_Observed,"string"), 'yyyyMMdd')

# Let's Understand the Range of ticket Issue Dates Available in the Dataset nyc_parking_tkts_2016
createOrReplaceTempView(nyc_parking_tkts_2016, "nyc_parking_tkts_2016_view")
range_issue_date_2016 <- SparkR::sql("SELECT min(issue_date) as min_issue_date_2016, max(issue_date) as max_issue_date_2016
                                     FROM nyc_parking_tkts_2016_view")
head(range_issue_date_2016)
# Min_IssueDate_2016 : 1970-04-13
# Max_IssueDate_2016 : 2069-10-02
# The Issue Tickets range between 13th Apr 1970 to 02nd Oct 2069. Clearly this is Nonconforming.

# We Will create Additional Columns in the Dataset that Correspond to the Year and Month of Ticket Issue
nyc_parking_tkts_2016$Issue_Year <- year(nyc_parking_tkts_2016$Issue_Date)
nyc_parking_tkts_2016$Issue_Month <- month(nyc_parking_tkts_2016$Issue_Date)

#Now let's observe the Distribution of Issue Date.
createOrReplaceTempView(nyc_parking_tkts_2016, "nyc_parking_tkts_2016_view")
count_per_issue_year_2016 <- SparkR::sql("SELECT Issue_Year, Issue_Month, count(*) as Num_of_Records
                                         FROM nyc_parking_tkts_2016_view
                                         GROUP BY Issue_Year,Issue_Month
                                         ORDER BY Issue_Year desc, Issue_Month desc")
count_per_issue_year_2016
head(count_per_issue_year_2016, nrow(count_per_issue_year_2016))

# Subsetting the DataFrame according to the Fiscal Year (July,2015 to June,2016).[Read Assumptions For Justification]
nyc_parking_tkts_2016 <- nyc_parking_tkts_2016[nyc_parking_tkts_2016$Issue_Date >= "2015-07-01" & nyc_parking_tkts_2016$Issue_Date <= "2016-06-30"]
nrow(nyc_parking_tkts_2016)
# 10,396,894 records

# The columns "Latitude", "Longitude", "Community_Board", "Community_Council", "Census_Tract", "BIN", "BBL" and "NTA" are present
# for the fiscal year 2015 and 2016 but not for 2017. We will Check the number of null values in these columns and remove if necessary. 
createOrReplaceTempView(nyc_parking_tkts_2016, "nyc_parking_tkts_2016_view")
null_count_column_2016 <- SparkR::sql("SELECT COUNT(*) Num_of_Rows,
                                      SUM(CASE WHEN No_Standing_or_Stopping_Violation IS NULL THEN 1 ELSE 0
                                      END) null_stand_stop_violation,
                                      SUM(CASE WHEN Hydrant_Violation IS NULL THEN 1 ELSE 0
                                      END) null_hydrant_violation,
                                      SUM(CASE WHEN Double_Parking_Violation IS NULL THEN 1 ELSE 0
                                      END) null_double_parking_violation,
                                      SUM(CASE WHEN Latitude IS NULL THEN 1 ELSE 0
                                      END) Nulls_Latitude,
                                      SUM(CASE WHEN Longitude IS NULL THEN 1 ELSE 0
                                      END) null_longitude,
                                      SUM(CASE WHEN Community_Board IS NULL THEN 1 ELSE 0
                                      END) Null_community_board,
                                      SUM(CASE WHEN Community_Council IS NULL THEN 1 ELSE 0
                                      END) nulls_community_council,
                                      SUM(CASE WHEN Census_Tract IS NULL THEN 1 ELSE 0
                                      END) nulls_census_tract,
                                      SUM(CASE WHEN BIN IS NULL THEN 1 ELSE 0
                                      END) nulls_bin,
                                      SUM(CASE WHEN BBL IS NULL THEN 1 ELSE 0
                                      END) null_bbl,
                                      SUM(CASE WHEN NTA IS NULL THEN 1 ELSE 0
                                      END) null_nta     
                                      FROM nyc_parking_tkts_2016_view")
head(null_count_column_2016)
# These columns have only null values therefore they should be dropped to standardize the dataset between the years. Dropping these columns.
nyc_parking_tkts_2016<- drop(nyc_parking_tkts_2016, c("No_Standing_or_Stopping_Violation",
                                                      "Hydrant_Violation",
                                                      "Double_Parking_Violation",
                                                      "Latitude",
                                                      "Longitude",
                                                      "Community_Board",
                                                      "Community_Council",
                                                      "Census_Tract",
                                                      "BIN",
                                                      "BBL",
                                                      "NTA") )
colnames(nyc_parking_tkts_2016)

# Correcting the format for the Violation_Time, Time_First_Observed, From_Hours_In_Effect and To_Hours_In_Effect. We can observe that
# the string format of these time attributes include only a partial component of AM/PM. Therefore we will append M at the end of each
# time attribute before converting it into a timestamp.
nyc_parking_tkts_2016$Concat_M <- "M"
nyc_parking_tkts_2016$Violation_Time <- concat(nyc_parking_tkts_2016$Violation_Time,  nyc_parking_tkts_2016$Concat_M)
nyc_parking_tkts_2016$Time_First_Observed <- concat(nyc_parking_tkts_2016$Time_First_Observed, nyc_parking_tkts_2016$Concat_M)
nyc_parking_tkts_2016$From_Hours_In_Effect <- concat(nyc_parking_tkts_2016$From_Hours_In_Effect, nyc_parking_tkts_2016$Concat_M)
nyc_parking_tkts_2016$To_Hours_In_Effect <- concat(nyc_parking_tkts_2016$To_Hours_In_Effect, nyc_parking_tkts_2016$Concat_M)
nyc_parking_tkts_2016<- drop(nyc_parking_tkts_2016, c("Concat_M"))

# Extracting Violation Hour, Violation Minute and 12 hour clock format.
nyc_parking_tkts_2016$Violation_Hour <- substr(nyc_parking_tkts_2016$Violation_Time, 1, 2)
nyc_parking_tkts_2016$Violation_Minute <- substr(nyc_parking_tkts_2016$Violation_Time, 3, 4)
nyc_parking_tkts_2016$Violation_Clock <- substr(nyc_parking_tkts_2016$Violation_Time, 5, 6)

# We've observed that there are few records that have both 00xxAM as well as 12xxAM. Therefore we will replace all 00xxAM to 12xxAM
nyc_parking_tkts_2016$Violation_Hour <- regexp_replace(x = nyc_parking_tkts_2016$Violation_Hour,pattern = "\\00",replacement = "12")

# Concatenating the components into a standardized Violation Time.
nyc_parking_tkts_2016$Violation_Time <- concat(nyc_parking_tkts_2016$Violation_Hour, nyc_parking_tkts_2016$Violation_Minute, nyc_parking_tkts_2016$Violation_Clock)


# Converting Violation_Time, Time_First_Observed, From_Hours_In_Effect, To_Hours_In_Effect into a TimeStamp
nyc_parking_tkts_2016$Violation_Time<-to_timestamp(x = nyc_parking_tkts_2016$Violation_Time, format = "hhmma")
nyc_parking_tkts_2016$Time_First_Observed<- to_timestamp(x= nyc_parking_tkts_2016$Time_First_Observed, format = "hhmma")
nyc_parking_tkts_2016$From_Hours_In_Effect<- to_timestamp(x= nyc_parking_tkts_2016$From_Hours_In_Effect, format = "hhmma")
nyc_parking_tkts_2016$To_Hours_In_Effect<- to_timestamp(x= nyc_parking_tkts_2016$To_Hours_In_Effect, format = "hhmma")

# The dimensions of Formatted and Cleaned Dataset that will be used for Analysis:
printSchema(nyc_parking_tkts_2016)
head(nyc_parking_tkts_2016)
dim(nyc_parking_tkts_2016)
#Rows: 10,396,894 | Columns: 45

# Detailed Data Quality Verification of nyc_parking_tkts_2017

# Removing duplicate rows in the dataset [Two or More rows having the same Summons_Number are duplicate]
nyc_parking_tkts_2017<- dropDuplicates(nyc_parking_tkts_2017, "Summons_Number")
dim(nyc_parking_tkts_2017)
#  Rows: 10,803,028 | Columns: 43

# Since ticket issue date is an critical parameter. Let us check if there are any missing values in it.
createOrReplaceTempView(nyc_parking_tkts_2017, "nyc_parking_tkts_2017_view")
count_null_issue_date_2017 <- SparkR::sql("SELECT SUM(CASE WHEN issue_date IS NULL THEN 1
                                          ELSE 0
                                          END) null_issue_date,
                                          COUNT(*) num_of_rows
                                          FROM nyc_parking_tkts_2017_view")
head(count_null_issue_date_2017)
#There are no records with missing Issue Date or Null Issue Date

# Converting the date paramters[Issue_Date, Vehicle_Expiration_Date and Date_First_Observed] to a suitable format for analysis.
nyc_parking_tkts_2017$Issue_Date <- SparkR::to_date(nyc_parking_tkts_2017$Issue_Date, 'MM/dd/yyyy')
nyc_parking_tkts_2017$Vehicle_Expiration_Date <- SparkR::to_date(cast(nyc_parking_tkts_2017$Vehicle_Expiration_Date,"string"), 'yyyyMMdd')
nyc_parking_tkts_2017$Date_First_Observed <- SparkR::to_date(cast(nyc_parking_tkts_2017$Date_First_Observed,"string"), 'yyyyMMdd')

# Let's Understand the Range of ticket Issue Dates Available in the Dataset nyc_parking_tkts_2017
createOrReplaceTempView(nyc_parking_tkts_2017, "nyc_parking_tkts_2017_view")
range_issue_date_2017 <- SparkR::sql("SELECT min(issue_date) as min_issue_date_2017, max(issue_date) as max_issue_date_2017
                                     FROM nyc_parking_tkts_2017_view")
head(range_issue_date_2017)
# Min_IssueDate_2017 : 1972-03-30
# Max_IssueDate_2017 : 2069-11-19
# The Issue Tickets range between 30th March 1972 to 19th Nov 2069. Clearly this is Nonconforming.

# We Will create Additional Columns in the Dataset that Correspond to the Year and Month of Ticket Issue
nyc_parking_tkts_2017$Issue_Year <- year(nyc_parking_tkts_2017$Issue_Date)
nyc_parking_tkts_2017$Issue_Month <- month(nyc_parking_tkts_2017$Issue_Date)

#Now let's observe the Distribution of Issue Date.
createOrReplaceTempView(nyc_parking_tkts_2017, "nyc_parking_tkts_2017_view")
count_per_issue_year_2017 <- SparkR::sql("SELECT Issue_Year, Issue_Month, count(*) as Num_of_Records
                                         FROM nyc_parking_tkts_2017_view
                                         GROUP BY Issue_Year,Issue_Month
                                         ORDER BY Issue_Year desc, Issue_Month desc")
head(count_per_issue_year_2017, nrow(count_per_issue_year_2017))

# Subsetting the DataFrame according to the Fiscal Year (July,2016 to June,2017).[Read Assumptions For Justification]
nyc_parking_tkts_2017 <- nyc_parking_tkts_2017[nyc_parking_tkts_2017$Issue_Date >= "2016-07-01" & nyc_parking_tkts_2017$Issue_Date <= "2017-06-30"]
nrow(nyc_parking_tkts_2017)
# 10,539,563 records

# We will Check the number of null values in columns No_Standing_or_Stopping_Violation,Hydrant_Violation and Double_Parking_Violation  and remove if necessary. 
createOrReplaceTempView(nyc_parking_tkts_2017, "nyc_parking_tkts_2017_view")
null_count_column_2017 <- SparkR::sql("SELECT COUNT(*) num_of_rows,
                                      SUM(CASE WHEN No_Standing_or_Stopping_Violation IS NULL THEN 1 ELSE 0
                                      END) null_stand_stop_violation,
                                      SUM(CASE WHEN Hydrant_Violation IS NULL THEN 1 ELSE 0
                                      END) null_hydrant_violation,
                                      SUM(CASE WHEN Double_Parking_Violation IS NULL THEN 1 ELSE 0
                                      END) null_double_parking_violation
                                      FROM nyc_parking_tkts_2017_view")
head(null_count_column_2017)
# These columns have only null values. therefore dropping these columns.
nyc_parking_tkts_2017<- drop(nyc_parking_tkts_2017, c("No_Standing_or_Stopping_Violation",
                                                      "Hydrant_Violation",
                                                      "Double_Parking_Violation") )
colnames(nyc_parking_tkts_2017)

# Correcting the format for the Violation_Time, Time_First_Observed, From_Hours_In_Effect and To_Hours_In_Effect. We can observe that
# the string format of these time attributes include only a partial component of AM/PM. Therefore we will append M at the end of each
# time attribute before converting it into a timestamp.
nyc_parking_tkts_2017$Concat_M <- "M"
nyc_parking_tkts_2017$Violation_Time <- concat(nyc_parking_tkts_2017$Violation_Time,  nyc_parking_tkts_2017$Concat_M)
nyc_parking_tkts_2017$Time_First_Observed <- concat(nyc_parking_tkts_2017$Time_First_Observed, nyc_parking_tkts_2017$Concat_M)
nyc_parking_tkts_2017$From_Hours_In_Effect <- concat(nyc_parking_tkts_2017$From_Hours_In_Effect, nyc_parking_tkts_2017$Concat_M)
nyc_parking_tkts_2017$To_Hours_In_Effect <- concat(nyc_parking_tkts_2017$To_Hours_In_Effect, nyc_parking_tkts_2017$Concat_M)
nyc_parking_tkts_2017<- drop(nyc_parking_tkts_2017, c("Concat_M"))

# Extracting Violation Hour, Violation Minute and 12 hour clock format.
nyc_parking_tkts_2017$Violation_Hour <- substr(nyc_parking_tkts_2017$Violation_Time, 1, 2)
nyc_parking_tkts_2017$Violation_Minute <- substr(nyc_parking_tkts_2017$Violation_Time, 3, 4)
nyc_parking_tkts_2017$Violation_Clock <- substr(nyc_parking_tkts_2017$Violation_Time, 5, 6)

# We've observed that there are few records that have both 00xxAM as well as 12xxAM. Therefore we will replace all 00xxAM to 12xxAM
nyc_parking_tkts_2017$Violation_Hour <- regexp_replace(x = nyc_parking_tkts_2017$Violation_Hour,pattern = "\\00",replacement = "12")

# Concatenating the components into a standardized Violation Time.
nyc_parking_tkts_2017$Violation_Time <- concat(nyc_parking_tkts_2017$Violation_Hour, nyc_parking_tkts_2017$Violation_Minute, nyc_parking_tkts_2017$Violation_Clock)

# Converting Violation_Time, Time_First_Observed, From_Hours_In_Effect, To_Hours_In_Effect into a TimeStamp
nyc_parking_tkts_2017$Violation_Time<-to_timestamp(x = nyc_parking_tkts_2017$Violation_Time, format = "hhmma")
nyc_parking_tkts_2017$Time_First_Observed<- to_timestamp(x= nyc_parking_tkts_2017$Time_First_Observed, format = "hhmma")
nyc_parking_tkts_2017$From_Hours_In_Effect<- to_timestamp(x= nyc_parking_tkts_2017$From_Hours_In_Effect, format = "hhmma")
nyc_parking_tkts_2017$To_Hours_In_Effect<- to_timestamp(x= nyc_parking_tkts_2017$To_Hours_In_Effect, format = "hhmma")

# The dimensions of Formatted and Cleaned Dataset that will be used for Analysis:
printSchema(nyc_parking_tkts_2017)
head(nyc_parking_tkts_2017)
dim(nyc_parking_tkts_2017)
# Rows: 10,539,563 | Columns: 45

createOrReplaceTempView(nyc_parking_tkts_2015, "nyc_parking_tkts_2015_view")
createOrReplaceTempView(nyc_parking_tkts_2016, "nyc_parking_tkts_2016_view")
createOrReplaceTempView(nyc_parking_tkts_2017, "nyc_parking_tkts_2017_view")

#
# Stage-2: Overview and Examining the dataset.
#

#
# Q1. Find total number of tickets for each year.
#
years <- c("2015", "2016", "2017")
no_of_tkts <- c(nrow(nyc_parking_tkts_2015), nrow(nyc_parking_tkts_2016), nrow(nyc_parking_tkts_2017))
tkts_vs_years <- data.frame(no_of_tkts, years)
ggplot(tkts_vs_years, aes(x = years , y = no_of_tkts)) +
  geom_col() +
  xlab("Years") +
  ylab("Number of Tickets") + 
  ggtitle("Years vs. Number of Tickets") +
  geom_text(aes(label = no_of_tkts), vjust=-0.1)

#
# Q2. Find out how many unique states the cars which got parking tickets came from.
#

# Unique registration states from where car ticket issued for 2015
unique_state_2015 <- SparkR::sql("SELECT DISTINCT(Registration_State) FROM nyc_parking_tkts_2015_view")
# Unique registration states from where car ticket issued for 2016
unique_state_2016 <- SparkR::sql("SELECT DISTINCT(Registration_State) FROM nyc_parking_tkts_2016_view")
# Unique registration states from where car ticket issued for 2017
unique_state_2017 <- SparkR::sql("SELECT DISTINCT(Registration_State) FROM nyc_parking_tkts_2017_view")

years <- c("2015", "2016", "2017")
state_count <- c(nrow(unique_state_2015), nrow(unique_state_2016), nrow(unique_state_2017))
state_count_vs_years <- data.frame(years, state_count)
ggplot(state_count_vs_years, aes(x = years , y = state_count)) +
  geom_col() +
  xlab("Years") +
  ylab("Number of Unique State") + 
  ggtitle("Years vs. Number of Unique State") +
  geom_text(aes(label = state_count), vjust=-0.1)

#
# Q3. Some parking tickets don't have addresses on them, which is cause for concern. Find out how many such tickets there are.
#

# No of missing address records for year 2015
missing_address_2015 <- SparkR::sql("SELECT SUM(CASE WHEN House_Number IS NULL or Street_Name IS NULL THEN 1 ELSE 0 
                                    END) as missing_address_count 
                                    FROM nyc_parking_tkts_2015_view")
# No of missing address records for year 2016
missing_address_2016 <- SparkR::sql("SELECT SUM(CASE WHEN House_Number IS NULL or Street_Name IS NULL THEN 1 ELSE 0 
                                    END) as missing_address_count
                                    FROM nyc_parking_tkts_2016_view")
# No of missing address records for year 2015
missing_address_2017 <- SparkR::sql("SELECT SUM(CASE WHEN House_Number IS NULL or Street_Name IS NULL THEN 1 ELSE 0 
                                    END) as missing_address_count 
                                    FROM nyc_parking_tkts_2017_view")
head(missing_address_2015)
years <- c("2015", "2016", "2017")
missing_address_count <- c(head(missing_address_2015), head(missing_address_2015), head(missing_address_2015))
missing_address_count_vs_years <- data.frame(years, missing_address_count)
ggplot(missing_address_count_vs_years, aes(x = years , y = missing_address_count)) +
  geom_col() +
  xlab("Years") +
  ylab("Number of Missing Address") + 
  ggtitle("Years vs. Number of Missing Address") +
  geom_text(aes(label = missing_address_count), vjust=-0.1)


#
# Stage 3: Aggregation tasks - comparing metrics and insights across the years
#

#
# Q1. How often does each violation code occur? (frequency of violation codes - find the top 5) 
#

# Top 5 violation code for 2015
violation_code_2015 <- SparkR::sql("SELECT Violation_Code, count(*) as no_of_tickets
                                   FROM nyc_parking_tkts_2015_view 
                                   GROUP BY Violation_Code
                                   ORDER BY no_of_tickets DESC")
top5_violation_code_2015 <- data.frame(head(violation_code_2015,5))
ggplot(top5_violation_code_2015, aes(x = as.factor(Violation_Code) , y = no_of_tickets)) +
  geom_col() +
  xlab("Violation_Code") +
  ylab("Number of Tickets") + 
  ggtitle("Violation Code vs. Number of Tickets in the year 2015") +
  geom_text(aes(label = no_of_tickets), vjust=-0.1)

# Top 5 violation code for 2016
violation_code_2016 <- SparkR::sql("SELECT Violation_Code, count(*) as no_of_tickets
                                   FROM nyc_parking_tkts_2016_view 
                                   GROUP BY Violation_Code
                                   ORDER BY no_of_tickets DESC")
top5_violation_code_2016 <- data.frame(head(violation_code_2016,5))
ggplot(top5_violation_code_2016, aes(x = as.factor(Violation_Code) , y = no_of_tickets)) +
  geom_col() +
  xlab("Violation_Code") +
  ylab("Number of Tickets") + 
  ggtitle("Violation Code vs. Number of Tickets in the year 2016") +
  geom_text(aes(label = no_of_tickets), vjust=-0.1)

# Top 5 violation code for 2017
violation_code_2017 <- SparkR::sql("SELECT Violation_Code, count(*) as no_of_tickets
                                   FROM nyc_parking_tkts_2017_view 
                                   GROUP BY Violation_Code
                                   ORDER BY no_of_tickets DESC")
top5_violation_code_2017 <- data.frame(head(violation_code_2017,5))
ggplot(top5_violation_code_2017, aes(x = as.factor(Violation_Code) , y = no_of_tickets)) +
  geom_col() +
  xlab("Violation_Code") +
  ylab("Number of Tickets") + 
  ggtitle("Violation Code vs. Number of Tickets in the year 2017") +
  geom_text(aes(label = no_of_tickets), vjust=-0.1)

#
# Q2. How often does each vehicle body type get a parking ticket? How about the vehicle make? (find the top 5 for both)
#

# Top 5 Vehicle Body Type for 2015
vehicle_body_type_2015 <- SparkR::sql("SELECT Vehicle_Body_Type, count(*)as no_of_tickets
                                      FROM nyc_parking_tkts_2015_view 
                                      GROUP BY Vehicle_Body_Type
                                      ORDER BY no_of_tickets DESC")
top5_vehicle_body_type_2015<- data.frame(head(vehicle_body_type_2015,5))
ggplot(top5_vehicle_body_type_2015, aes(x = as.factor(Vehicle_Body_Type) , y = no_of_tickets)) +
  geom_col() +
  xlab("Vehicle Body Type") +
  ylab("Number of Tickets") + 
  ggtitle("Vehicle Body Type vs. Number of Tickets in the year 2015") +
  geom_text(aes(label = no_of_tickets), vjust=-0.1)

# Top 5 Vehicle Body Type for 2016
vehicle_body_type_2016 <- SparkR::sql("SELECT Vehicle_Body_Type, count(*)as no_of_tickets
                                      FROM nyc_parking_tkts_2016_view 
                                      GROUP BY Vehicle_Body_Type
                                      ORDER BY no_of_tickets DESC")
top5_vehicle_body_type_2016<- data.frame(head(vehicle_body_type_2016,5))
ggplot(top5_vehicle_body_type_2016, aes(x = as.factor(Vehicle_Body_Type) , y = no_of_tickets)) +
  geom_col() +
  xlab("Vehicle Body Type") +
  ylab("Number of Tickets") + 
  ggtitle("Vehicle Body Type vs. Number of Tickets in the year 2016") +
  geom_text(aes(label = no_of_tickets), vjust=-0.1)

# Top 5 Vehicle Body Type for 2017
vehicle_body_type_2017 <- SparkR::sql("SELECT Vehicle_Body_Type, count(*)as no_of_tickets
                                      FROM nyc_parking_tkts_2017_view 
                                      GROUP BY Vehicle_Body_Type
                                      ORDER BY no_of_tickets DESC")
top5_vehicle_body_type_2017<- data.frame(head(vehicle_body_type_2017,5))
ggplot(top5_vehicle_body_type_2017, aes(x = as.factor(Vehicle_Body_Type) , y = no_of_tickets)) +
  geom_col() +
  xlab("Vehicle Body Type") +
  ylab("Number of Tickets") + 
  ggtitle("Vehicle Body Type vs. Number of Tickets in the year 2017") +
  geom_text(aes(label = no_of_tickets), vjust=-0.1)

# Top 5 Vehicle Make for 2015
vehicle_make_2015 <- SparkR::sql("SELECT Vehicle_Make, count(*)as no_of_tickets
                                 FROM nyc_parking_tkts_2015_view 
                                 GROUP BY Vehicle_Make
                                 ORDER BY no_of_tickets DESC")
top5_vehicle_make_2015<- data.frame(head(vehicle_make_2015,5))
ggplot(top5_vehicle_make_2015, aes(x = as.factor(Vehicle_Make) , y = no_of_tickets)) +
  geom_col() +
  xlab("Vehicle Make") +
  ylab("Number of Tickets") + 
  ggtitle("Vehicle Make vs. Number of Tickets in the year 2015") +
  geom_text(aes(label = no_of_tickets), vjust=-0.1)

# Top 5 Vehicle Make for 2016
vehicle_make_2016 <- SparkR::sql("SELECT Vehicle_Make, count(*)as no_of_tickets
                                 FROM nyc_parking_tkts_2016_view 
                                 GROUP BY Vehicle_Make
                                 ORDER BY no_of_tickets DESC")
top5_vehicle_make_2016<- data.frame(head(vehicle_make_2016,5))
ggplot(top5_vehicle_make_2016, aes(x = as.factor(Vehicle_Make) , y = no_of_tickets)) +
  geom_col() +
  xlab("Vehicle Make") +
  ylab("Number of Tickets") + 
  ggtitle("Vehicle Make vs. Number of Tickets in the year 2016") +
  geom_text(aes(label = no_of_tickets), vjust=-0.1)

# Top 5 Vehicle Make for 2017
vehicle_make_2017 <- SparkR::sql("SELECT Vehicle_Make, count(*)as no_of_tickets
                                 FROM nyc_parking_tkts_2017_view 
                                 GROUP BY Vehicle_Make
                                 ORDER BY no_of_tickets DESC")
top5_vehicle_make_2017<- data.frame(head(vehicle_make_2017,5))
ggplot(top5_vehicle_make_2017, aes(x = as.factor(Vehicle_Make) , y = no_of_tickets)) +
  geom_col() +
  xlab("Vehicle Make") +
  ylab("Number of Tickets") + 
  ggtitle("Vehicle Make vs. Number of Tickets in the year 2017") +
  geom_text(aes(label = no_of_tickets), vjust=-0.1)

#
# Q3. A precinct is a police station that has a certain zone of the city under its command. Find the (5 highest) frequencies of:
#

#
# Q3.1. Violating Precincts (this is the precinct of the zone where the violation occurred). Using this, can you make any insights for parking violations in any specific areas of the city?  
#

# Top 5 Violation Precinct for 2015
violation_precinct_2015 <- SparkR::sql("SELECT Violation_Precinct, count(*)as no_of_tickets
                                       FROM nyc_parking_tkts_2015_view 
                                       GROUP BY Violation_Precinct
                                       ORDER BY no_of_tickets DESC")
top5_violation_precinct_2015<- data.frame(head(violation_precinct_2015,5))
ggplot(top5_violation_precinct_2015, aes(x = as.factor(Violation_Precinct) , y = no_of_tickets)) +
  geom_col() +
  xlab("Violation Precinct") +
  ylab("Number of Tickets") + 
  ggtitle("Violation Precinct vs. Number of Tickets in the year 2015") +
  geom_text(aes(label = no_of_tickets), vjust=-0.1)

# Top 5 Violation Precinct for 2016
violation_precinct_2016 <- SparkR::sql("SELECT Violation_Precinct, count(*)as no_of_tickets
                                       FROM nyc_parking_tkts_2016_view 
                                       GROUP BY Violation_Precinct
                                       ORDER BY no_of_tickets DESC")
top5_violation_precinct_2016<- data.frame(head(violation_precinct_2016,5))
ggplot(top5_violation_precinct_2016, aes(x = as.factor(Violation_Precinct) , y = no_of_tickets)) +
  geom_col() +
  xlab("Violation Precinct") +
  ylab("Number of Tickets") + 
  ggtitle("Violation Precinct vs. Number of Tickets in the year 2016") +
  geom_text(aes(label = no_of_tickets), vjust=-0.1)

# Top 5 Violation Precinct for 2017
violation_precinct_2017 <- SparkR::sql("SELECT Violation_Precinct, count(*)as no_of_tickets
                                       FROM nyc_parking_tkts_2017_view 
                                       GROUP BY Violation_Precinct
                                       ORDER BY no_of_tickets DESC")
top5_violation_precinct_2017<- data.frame(head(violation_precinct_2017,5))
ggplot(top5_violation_precinct_2017, aes(x = as.factor(Violation_Precinct) , y = no_of_tickets)) +
  geom_col() +
  xlab("Violation Precinct") +
  ylab("Number of Tickets") + 
  ggtitle("Violation Precinct vs. Number of Tickets in the year 2017") +
  geom_text(aes(label = no_of_tickets), vjust=-0.1)

#
#Q3.2. Issuing Precincts (this is the precinct that issued the ticket)
#

# Top 5 Issuer Precinct for 2015
issuer_precinct_2015 <- SparkR::sql("SELECT Issuer_Precinct, count(*)as no_of_tickets
                                    FROM nyc_parking_tkts_2015_view 
                                    GROUP BY Issuer_Precinct
                                    ORDER BY no_of_tickets DESC")
top5_issuer_precinct_2015<- data.frame(head(issuer_precinct_2015,5))
ggplot(top5_issuer_precinct_2015, aes(x = as.factor(Issuer_Precinct) , y = no_of_tickets)) +
  geom_col() +
  xlab("Issuer Precinct") +
  ylab("Number of Tickets") + 
  ggtitle("Issuer Precinct vs. Number of Tickets in the year 2015") +
  geom_text(aes(label = no_of_tickets), vjust=-0.1)

# Top 5 Issuer Precinct for 2016
issuer_precinct_2016 <- SparkR::sql("SELECT Issuer_Precinct, count(*)as no_of_tickets
                                    FROM nyc_parking_tkts_2016_view 
                                    GROUP BY Issuer_Precinct
                                    ORDER BY no_of_tickets DESC")
top5_issuer_precinct_2016<- data.frame(head(issuer_precinct_2016,5))
ggplot(top5_issuer_precinct_2016, aes(x = as.factor(Issuer_Precinct) , y = no_of_tickets)) +
  geom_col() +
  xlab("Issuer Precinct") +
  ylab("Number of Tickets") + 
  ggtitle("Issuer Precinct vs. Number of Tickets in the year 2016") +
  geom_text(aes(label = no_of_tickets), vjust=-0.1)

# Top 5 Issuer Precinct for 2017
issuer_precinct_2017 <- SparkR::sql("SELECT Issuer_Precinct, count(*)as no_of_tickets
                                    FROM nyc_parking_tkts_2017_view 
                                    GROUP BY Issuer_Precinct
                                    ORDER BY no_of_tickets DESC")
top5_issuer_precinct_2017<- data.frame(head(issuer_precinct_2017,5))
ggplot(top5_issuer_precinct_2017, aes(x = as.factor(Issuer_Precinct) , y = no_of_tickets)) +
  geom_col() +
  xlab("Issuer Precinct") +
  ylab("Number of Tickets") + 
  ggtitle("Issuer Precinct vs. Number of Tickets in the year 2017") +
  geom_text(aes(label = no_of_tickets), vjust=-0.1)

# Comparison for Issuer Precinct between year 2015, 2016 and 2017
joined_issuer_precinct <- rbind(top5_issuer_precinct_2015, top5_issuer_precinct_2016, top5_issuer_precinct_2017)
joined_issuer_precinct$years <- c("2015","2015","2015","2015", "2015","2016","2016","2016","2016", "2016","2017","2017","2017","2017", "2017")
ggplot(joined_issuer_precinct, aes(x = as.factor(Issuer_Precinct) , y = no_of_tickets)) +
  geom_col() +
  facet_grid(~years) +
  xlab("Issuer Precinct") +
  ylab("Number of Tickets") + 
  ggtitle("Issuer Precinct vs. Number of Tickets Over the years") +
  geom_text(aes(label = no_of_tickets), vjust=-0.1)

#
# Q4. Find the violation code frequency across 3 precincts which have issued the most number of tickets - do
#     these precinct zones have an exceptionally high frequency of certain violation codes? Are these codes 
#     common across precincts?
#

#Violation Code distribution in 3 precincts (0, 19 and 18) which have issued the most number of tickets in year 2015

#Violation Code Distribution in Issuer Precinct 0
violation_code_0_2015<- SparkR::sql("SELECT Violation_Code,count(*)as no_of_tickets, Issuer_Precinct
                                    FROM nyc_parking_tkts_2015_view 
                                    WHERE Issuer_Precinct = 0
                                    GROUP BY Violation_Code, Issuer_Precinct
                                    ORDER BY no_of_tickets DESC")
top5_violation_code_0_2015<- data.frame(head(violation_code_0_2015, 5))

#Violation Code Distribution in Issuer Precinct 19
violation_code_19_2015<- SparkR::sql("SELECT Violation_Code,count(*)as no_of_tickets, Issuer_Precinct
                                     FROM nyc_parking_tkts_2015_view 
                                     WHERE Issuer_Precinct = 19
                                     GROUP BY Violation_Code, Issuer_Precinct
                                     ORDER BY no_of_tickets DESC")
top5_violation_code_19_2015<- data.frame(head(violation_code_19_2015, 5))

#Violation Code Distribution in Issuer Precinct 18
violation_code_18_2015<- SparkR::sql("SELECT Violation_Code,count(*)as no_of_tickets, Issuer_Precinct
                                     FROM nyc_parking_tkts_2015_view 
                                     WHERE Issuer_Precinct = 18
                                     GROUP BY Violation_Code, Issuer_Precinct
                                     ORDER BY no_of_tickets DESC")
top5_violation_code_18_2015<- data.frame(head(violation_code_18_2015, 5))

#Violation Code Distribution in other Issuer Precinct
violation_code_other_2015<- SparkR::sql("SELECT Violation_Code,count(*)as no_of_tickets, Issuer_Precinct
                                        FROM nyc_parking_tkts_2015_view 
                                        WHERE Issuer_Precinct NOT IN (0,19,18)
                                        GROUP BY Violation_Code, Issuer_Precinct
                                        ORDER BY no_of_tickets DESC")
top5_violation_code_other_2015<- data.frame(head(violation_code_other_2015, 5))
top5_violation_code_other_2015$Issuer_Precinct<- c("Other","Other","Other","Other","Other")
joined_violation_code_2015 <- rbind(top5_violation_code_0_2015, top5_violation_code_19_2015, top5_violation_code_18_2015, top5_violation_code_other_2015)
ggplot(joined_violation_code_2015, aes(x = as.factor(Violation_Code) , y = no_of_tickets)) +
  geom_col() +
  facet_grid(~Issuer_Precinct) +
  xlab("Violation Code") +
  ylab("Number of Tickets") + 
  ggtitle("Violation Code distribution vs. Top Issuer Precinct in 2015") +
  geom_text(aes(label = no_of_tickets), vjust=-0.1)

#Violation Code distribution in 3 precincts (0, 19 and 18) which have issued the most number of tickets in year 2016

#Violation Code Distribution in Issuer Precinct 0
violation_code_0_2016<- SparkR::sql("SELECT Violation_Code,count(*)as no_of_tickets, Issuer_Precinct
                                    FROM nyc_parking_tkts_2016_view 
                                    WHERE Issuer_Precinct = 0
                                    GROUP BY Violation_Code, Issuer_Precinct
                                    ORDER BY no_of_tickets DESC")
top5_violation_code_0_2016<- data.frame(head(violation_code_0_2016, 5))

#Violation Code Distribution in Issuer Precinct 19
violation_code_19_2016<- SparkR::sql("SELECT Violation_Code,count(*)as no_of_tickets, Issuer_Precinct
                                     FROM nyc_parking_tkts_2016_view 
                                     WHERE Issuer_Precinct = 19
                                     GROUP BY Violation_Code, Issuer_Precinct
                                     ORDER BY no_of_tickets DESC")
top5_violation_code_19_2016<- data.frame(head(violation_code_19_2016, 5))

#Violation Code Distribution in Issuer Precinct 18
violation_code_18_2016<- SparkR::sql("SELECT Violation_Code,count(*)as no_of_tickets, Issuer_Precinct
                                     FROM nyc_parking_tkts_2016_view 
                                     WHERE Issuer_Precinct = 18
                                     GROUP BY Violation_Code, Issuer_Precinct
                                     ORDER BY no_of_tickets DESC")
top5_violation_code_18_2016<- data.frame(head(violation_code_18_2016, 5))

#Violation Code Distribution in other Issuer Precinct
violation_code_other_2016<- SparkR::sql("SELECT Violation_Code,count(*)as no_of_tickets, Issuer_Precinct
                                        FROM nyc_parking_tkts_2016_view 
                                        WHERE Issuer_Precinct NOT IN (0,19,18)
                                        GROUP BY Violation_Code, Issuer_Precinct
                                        ORDER BY no_of_tickets DESC")
top5_violation_code_other_2016<- data.frame(head(violation_code_other_2016, 5))
top5_violation_code_other_2016$Issuer_Precinct<- c("Other","Other","Other","Other","Other")
joined_violation_code_2016 <- rbind(top5_violation_code_0_2016, top5_violation_code_19_2016, top5_violation_code_18_2016, top5_violation_code_other_2016)
ggplot(joined_violation_code_2016, aes(x = as.factor(Violation_Code) , y = no_of_tickets)) +
  geom_col() +
  facet_grid(~Issuer_Precinct) +
  xlab("Violation Code") +
  ylab("Number of Tickets") + 
  ggtitle("Violation Code distribution vs. Top Issuer Precinct in 2016") +
  geom_text(aes(label = no_of_tickets), vjust=-0.1)

#Violation Code distribution in 3 precincts (0, 19 and 14) which have issued the most number of tickets in year 2017

#Violation Code Distribution in Issuer Precinct 0
violation_code_0_2017<- SparkR::sql("SELECT Violation_Code,count(*)as no_of_tickets, Issuer_Precinct
                                    FROM nyc_parking_tkts_2017_view 
                                    WHERE Issuer_Precinct = 0
                                    GROUP BY Violation_Code, Issuer_Precinct
                                    ORDER BY no_of_tickets DESC")
top5_violation_code_0_2017<- data.frame(head(violation_code_0_2017, 5))

#Violation Code Distribution in Issuer Precinct 19
violation_code_19_2017<- SparkR::sql("SELECT Violation_Code,count(*)as no_of_tickets, Issuer_Precinct
                                     FROM nyc_parking_tkts_2017_view 
                                     WHERE Issuer_Precinct = 19
                                     GROUP BY Violation_Code, Issuer_Precinct
                                     ORDER BY no_of_tickets DESC")
top5_violation_code_19_2017<- data.frame(head(violation_code_19_2017, 5))

#Violation Code Distribution in Issuer Precinct 18
violation_code_18_2017<- SparkR::sql("SELECT Violation_Code,count(*)as no_of_tickets, Issuer_Precinct
                                     FROM nyc_parking_tkts_2017_view 
                                     WHERE Issuer_Precinct = 18
                                     GROUP BY Violation_Code, Issuer_Precinct
                                     ORDER BY no_of_tickets DESC")
top5_violation_code_18_2017<- data.frame(head(violation_code_18_2017, 5))

#Violation Code Distribution in other Issuer Precinct
violation_code_other_2017<- SparkR::sql("SELECT Violation_Code,count(*)as no_of_tickets, Issuer_Precinct
                                        FROM nyc_parking_tkts_2017_view 
                                        WHERE Issuer_Precinct NOT IN (0,19,18)
                                        GROUP BY Violation_Code, Issuer_Precinct
                                        ORDER BY no_of_tickets DESC")
top5_violation_code_other_2017<- data.frame(head(violation_code_other_2017, 5))
top5_violation_code_other_2017$Issuer_Precinct<- c("Other","Other","Other","Other","Other")
joined_violation_code_2017 <- rbind(top5_violation_code_0_2017, top5_violation_code_19_2017, top5_violation_code_18_2017, top5_violation_code_other_2017)
ggplot(joined_violation_code_2017, aes(x = as.factor(Violation_Code) , y = no_of_tickets)) +
  geom_col() +
  facet_grid(~Issuer_Precinct) +
  xlab("Violation Code") +
  ylab("Number of Tickets") + 
  ggtitle("Violation Code distribution vs. Top Issuer Precinct in 2017") +
  geom_text(aes(label = no_of_tickets), vjust=-0.1)

#
# Q5. You'd want to find out the properties of parking violations across different times of the day:
#     The Violation Time field is specified in a strange format. Find a way to make this into a time 
#     attribute that you can use to divide into groups.
#
#    Find a way to deal with missing values, if any.
#
#    Divide 24 hours into 6 equal discrete bins of time. The intervals you choose are at your discretion.
#
#

#
# Ans5.
#
# We have corrected the format of violation time during data cleaning phase and converted into timestamp[Refer cleaning steps above]

#Analysing and Cleaning missing values for 2015.
missing_violation_times_2015<- SparkR::sql("SELECT count(*)as total_count, 
                                           SUM(CASE WHEN Violation_Time is NULL THEN 1 ELSE 0 END) as missing_violation_time,
                                           100*SUM(CASE WHEN Violation_Time IS NULL THEN 1 ELSE 0 END)/count(*) as percent_missing_violation_time
                                           from nyc_parking_tkts_2015_view")
head(missing_violation_times_2015)
#2015 dataset 0.5812% records with Missing Violation Time is Negligable and will therefore be removed before analysis.

nyc_parking_tkts_2015_cleaned<- subset(nyc_parking_tkts_2015, isNotNull(nyc_parking_tkts_2015$Violation_Time))
nyc_parking_tkts_2015_cleaned$Violation_Hour <- hour(cast(nyc_parking_tkts_2015_cleaned$Violation_Time,dataType = "string"))
createOrReplaceTempView(nyc_parking_tkts_2015_cleaned, "nyc_parking_tkts_2015_cleaned_view")

#Divide 24 hours into 6 equal discrete bins of time. The intervals you choose are at your discretion. 
violation_hour_bin_2015 <- SparkR::sql("SELECT Violation_Hour,
                                       Violation_Code,
                                       CASE WHEN Violation_Hour BETWEEN 0 AND 3 THEN '0_3'
                                       WHEN Violation_Hour BETWEEN 4 AND 7 THEN '4_7'
                                       WHEN Violation_Hour BETWEEN 8 AND 11 THEN '8_11'
                                       WHEN Violation_Hour BETWEEN 12 AND 15 THEN '12_15' 
                                       WHEN Violation_Hour BETWEEN 16 AND 19 THEN '16_19' 
                                       WHEN Violation_Hour BETWEEN 20 AND 23 THEN '20_23' 
                                       END AS Violation_Hour_Bin
                                       FROM nyc_parking_tkts_2015_cleaned_view")

createOrReplaceTempView(violation_hour_bin_2015, "violation_hour_bin_2015_view")

hour_bin_tkts_2015 <- SparkR::sql("SELECT Violation_Hour_Bin, Violation_Code,num_of_tkts
                                  FROM (SELECT Violation_Hour_Bin, Violation_Code,num_of_tkts, dense_rank() over (partition by Violation_Hour_Bin order by num_of_tkts desc) Rnk
                                  FROM (SELECT Violation_Hour_Bin,Violation_Code, count(*)as num_of_tkts
                                  FROM violation_hour_bin_2015_view
                                  GROUP BY Violation_Hour_Bin,Violation_Code))
                                  WHERE Rnk <= 3")
hour_bin_tkts_2015_df <- data.frame(head(hour_bin_tkts_2015, nrow(hour_bin_tkts_2015)))
ggplot(hour_bin_tkts_2015_df, aes(x = as.factor(Violation_Code) , y = num_of_tkts)) +
  geom_col() +
  facet_grid(~Violation_Hour_Bin) +
  xlab("Violation Code") +
  ylab("Number of Tickets") + 
  ggtitle("Violation Code distribution vs. Violation_Hour_Bin") +
  geom_text(aes(label = num_of_tkts), vjust=-0.1)

#Analysing and Cleaning missing values for 2016.
missing_violation_times_2016<- SparkR::sql("SELECT count(*)as total_count, 
                                           SUM(CASE WHEN Violation_Time is NULL THEN 1 ELSE 0 END) as missing_violation_time,
                                           100*SUM(CASE WHEN Violation_Time IS NULL THEN 1 ELSE 0 END)/count(*) as percent_missing_violation_time
                                           from nyc_parking_tkts_2016_view")
head(missing_violation_times_2016)
#2016 dataset 0.6114%% records with Missing Violation Time is Negligable and will therefore be removed before analysis.

nyc_parking_tkts_2016_cleaned<- subset(nyc_parking_tkts_2016, isNotNull(nyc_parking_tkts_2016$Violation_Time))
nyc_parking_tkts_2016_cleaned$Violation_Hour <- hour(cast(nyc_parking_tkts_2016_cleaned$Violation_Time,dataType = "string"))
createOrReplaceTempView(nyc_parking_tkts_2016_cleaned, "nyc_parking_tkts_2016_cleaned_view")

#Divide 24 hours into 6 equal discrete bins of time. The intervals you choose are at your discretion. 
violation_hour_bin_2016 <- SparkR::sql("SELECT Violation_Hour,
                                       Violation_Code,
                                       CASE WHEN Violation_Hour BETWEEN 0 AND 3 THEN '0_3'
                                       WHEN Violation_Hour BETWEEN 4 AND 7 THEN '4_7'
                                       WHEN Violation_Hour BETWEEN 8 AND 11 THEN '8_11'
                                       WHEN Violation_Hour BETWEEN 12 AND 15 THEN '12_15' 
                                       WHEN Violation_Hour BETWEEN 16 AND 19 THEN '16_19' 
                                       WHEN Violation_Hour BETWEEN 20 AND 23 THEN '20_23' 
                                       END AS Violation_Hour_Bin
                                       FROM nyc_parking_tkts_2016_cleaned_view")

createOrReplaceTempView(violation_hour_bin_2016, "violation_hour_bin_2016_view")

hour_bin_tkts_2016 <- SparkR::sql("SELECT Violation_Hour_Bin, Violation_Code,num_of_tkts
                                  FROM (SELECT Violation_Hour_Bin, Violation_Code,num_of_tkts, dense_rank() over (partition by Violation_Hour_Bin order by num_of_tkts desc) Rnk
                                  FROM (SELECT Violation_Hour_Bin,Violation_Code, count(*)as num_of_tkts
                                  FROM violation_hour_bin_2016_view
                                  GROUP BY Violation_Hour_Bin,Violation_Code))
                                  WHERE Rnk <= 3")
hour_bin_tkts_2016_df <- data.frame(head(hour_bin_tkts_2016, nrow(hour_bin_tkts_2016)))
ggplot(hour_bin_tkts_2016_df, aes(x = as.factor(Violation_Code) , y = num_of_tkts)) +
  geom_col() +
  facet_grid(~Violation_Hour_Bin) +
  xlab("Violation Code") +
  ylab("Number of Tickets") + 
  ggtitle("Violation Code distribution vs. Violation_Hour_Bin") +
  geom_text(aes(label = num_of_tkts), vjust=-0.1)

#Analysing and Cleaning missing values for 2017.
missing_violation_times_2017<- SparkR::sql("SELECT count(*)as total_count, 
                                           SUM(CASE WHEN Violation_Time is NULL THEN 1 ELSE 0 END) as missing_violation_time,
                                           100*SUM(CASE WHEN Violation_Time IS NULL THEN 1 ELSE 0 END)/count(*) as percent_missing_violation_time
                                           from nyc_parking_tkts_2017_view")
head(missing_violation_times_2017)
#2017 dataset 0.6114% records with Missing Violation Time is Negligable and will therefore be removed before analysis.

nyc_parking_tkts_2017_cleaned<- subset(nyc_parking_tkts_2017, isNotNull(nyc_parking_tkts_2017$Violation_Time))
nyc_parking_tkts_2017_cleaned$Violation_Hour <- hour(cast(nyc_parking_tkts_2017_cleaned$Violation_Time,dataType = "string"))
createOrReplaceTempView(nyc_parking_tkts_2017_cleaned, "nyc_parking_tkts_2017_cleaned_view")

#Divide 24 hours into 6 equal discrete bins of time. The intervals you choose are at your discretion. 
violation_hour_bin_2017 <- SparkR::sql("SELECT Violation_Hour,
                                       Violation_Code,
                                       CASE WHEN Violation_Hour BETWEEN 0 AND 3 THEN '0_3'
                                       WHEN Violation_Hour BETWEEN 4 AND 7 THEN '4_7'
                                       WHEN Violation_Hour BETWEEN 8 AND 11 THEN '8_11'
                                       WHEN Violation_Hour BETWEEN 12 AND 15 THEN '12_15' 
                                       WHEN Violation_Hour BETWEEN 16 AND 19 THEN '16_19' 
                                       WHEN Violation_Hour BETWEEN 20 AND 23 THEN '20_23' 
                                       END AS Violation_Hour_Bin
                                       FROM nyc_parking_tkts_2017_cleaned_view")

createOrReplaceTempView(violation_hour_bin_2017, "violation_hour_bin_2017_view")

hour_bin_tkts_2017 <- SparkR::sql("SELECT Violation_Hour_Bin, Violation_Code,num_of_tkts
                                  FROM (SELECT Violation_Hour_Bin, Violation_Code,num_of_tkts, dense_rank() over (partition by Violation_Hour_Bin order by num_of_tkts desc) Rnk
                                  FROM (SELECT Violation_Hour_Bin,Violation_Code, count(*)as num_of_tkts
                                  FROM violation_hour_bin_2017_view
                                  GROUP BY Violation_Hour_Bin,Violation_Code))
                                  WHERE Rnk <= 3")
hour_bin_tkts_2017_df <- data.frame(head(hour_bin_tkts_2017, nrow(hour_bin_tkts_2017)))
ggplot(hour_bin_tkts_2017_df, aes(x = as.factor(Violation_Code) , y = num_of_tkts)) +
  geom_col() +
  facet_grid(~Violation_Hour_Bin) +
  xlab("Violation Code") +
  ylab("Number of Tickets") + 
  ggtitle("Violation Code distribution vs. Violation_Hour_Bin") +
  geom_text(aes(label = num_of_tkts), vjust=-0.1)

#
# Q5.b Now, try another direction. For the 3 most commonly occurring violation codes, find the most common times of day (in terms of the bins from the previous part)
#

# Top-3 Violation Code vs. Violation Time Bin Analysis for 2015
top3_violation_code_2015 <- SparkR::sql("SELECT Violation_Code,count(*) no_of_tickets
                                        FROM nyc_parking_tkts_2015_cleaned_view
                                        GROUP BY Violation_Code
                                        ORDER BY no_of_tickets desc")

head(top3_violation_code_2015,3)
#Top-3 Violation Code for 2015 are 21, 38 and 14

common_hour_bin_2015 <- SparkR::sql("SELECT Violation_Code, Violation_Hour_Bin, count(*) no_of_tickets
                                    FROM violation_hour_bin_2015_view
                                    WHERE violation_code IN (21,38,14)
                                    GROUP BY Violation_Code, Violation_Hour_Bin
                                    ORDER BY Violation_Code, Violation_Hour_Bin, no_of_tickets desc")	

common_hour_bin_2015_df <- data.frame(head(common_hour_bin_2015, nrow(common_hour_bin_2015)))
ggplot(common_hour_bin_2015_df, aes(x = as.factor(Violation_Hour_Bin) , y = no_of_tickets)) +
  geom_col() +
  facet_grid(~Violation_Code) +
  xlab("Violation_Hour_Bin") +
  ylab("Number of Tickets") + 
  ggtitle("Violation_Hour_Bin distribution vs. Violation Code ") +
  geom_text(aes(label = no_of_tickets), vjust=-0.1)

# Top-3 Violation Code vs. Violation Time Bin Analysis for 2016
top3_violation_code_2016 <- SparkR::sql("SELECT Violation_Code,count(*) no_of_tickets
                                        FROM nyc_parking_tkts_2016_cleaned_view
                                        GROUP BY Violation_Code
                                        ORDER BY no_of_tickets desc")

head(top3_violation_code_2016,3)
#Top-3 Violation Code for 2016 are 21, 36 and 38

common_hour_bin_2016 <- SparkR::sql("SELECT Violation_Code, Violation_Hour_Bin, count(*) no_of_tickets
                                    FROM violation_hour_bin_2016_view
                                    WHERE violation_code IN (21,38,14)
                                    GROUP BY Violation_Code, Violation_Hour_Bin
                                    ORDER BY Violation_Code, Violation_Hour_Bin, no_of_tickets desc")	

common_hour_bin_2016_df <- data.frame(head(common_hour_bin_2016, nrow(common_hour_bin_2016)))
ggplot(common_hour_bin_2016_df, aes(x = as.factor(Violation_Hour_Bin) , y = no_of_tickets)) +
  geom_col() +
  facet_grid(~Violation_Code) +
  xlab("Violation_Hour_Bin") +
  ylab("Number of Tickets") + 
  ggtitle("Violation_Hour_Bin distribution vs. Violation Code for 2016 ") +
  geom_text(aes(label = no_of_tickets), vjust=-0.1)

# Top-3 Violation Code vs. Violation Time Bin Analysis for 2017
top3_violation_code_2017 <- SparkR::sql("SELECT Violation_Code,count(*) no_of_tickets
                                        FROM nyc_parking_tkts_2017_cleaned_view
                                        GROUP BY Violation_Code
                                        ORDER BY no_of_tickets desc")

head(top3_violation_code_2017,3)
#Top-3 Violation Code for 2017 are 21, 36 and 38

common_hour_bin_2017 <- SparkR::sql("SELECT Violation_Code, Violation_Hour_Bin, count(*) no_of_tickets
                                    FROM violation_hour_bin_2017_view
                                    WHERE violation_code IN (21,38,14)
                                    GROUP BY Violation_Code, Violation_Hour_Bin
                                    ORDER BY Violation_Code, Violation_Hour_Bin, no_of_tickets desc")	

common_hour_bin_2017_df <- data.frame(head(common_hour_bin_2017, nrow(common_hour_bin_2017)))
ggplot(common_hour_bin_2017_df, aes(x = as.factor(Violation_Hour_Bin) , y = no_of_tickets)) +
  geom_col() +
  facet_grid(~Violation_Code) +
  xlab("Violation_Hour_Bin") +
  ylab("Number of Tickets") + 
  ggtitle("Violation_Hour_Bin distribution vs. Violation Code ") +
  geom_text(aes(label = no_of_tickets), vjust=-0.1)

#
# Q6. Let's try and find some seasonality in this data
#
#     First, divide the year into some number of seasons, and find frequencies of tickets for each season.
#
#     Then, find the 3 most common violations for each of these season
#

# Dividing 2015 dataset into 4 seasons {12,1,2} is Winter season then (3,5) as Spring, (6,8) as Summer and (9,12) as Fall
season_2015 <- SparkR::sql("SELECT Summons_Number, Violation_Code,
                           CASE WHEN Issue_Month IN (1,2,12) THEN 'winter'
                           WHEN Issue_Month BETWEEN 3 AND 5 THEN 'spring'
                           WHEN Issue_Month BETWEEN 6 AND 8 THEN 'summer'
                           WHEN Issue_Month BETWEEN 9 AND 12 THEN 'fall' 
                           END AS season
                           FROM nyc_parking_tkts_2015_view")
createOrReplaceTempView(season_2015, "season_2015_view")

season_vs_no_of_tkts_2015<- SparkR::sql("SELECT season, Count(*)as no_of_tkts
                                        FROM season_2015_view
                                        GROUP BY season
                                        ORDER BY no_of_tkts desc")

season_vs_no_of_tkts_2015_df<- data.frame(head(season_vs_no_of_tkts_2015))
season_vs_no_of_tkts_2015_df$years<- c(2015,2015,2015,2015)

# Dividing 2016 dataset into 4 seasons {12,1,2} is Winter season then (3,5) as Spring, (6,8) as Summer and (9,12) as Fall
season_2016 <- SparkR::sql("SELECT Summons_Number, Violation_Code,
                           CASE WHEN Issue_Month IN (1,2,12) THEN 'winter'
                           WHEN Issue_Month BETWEEN 3 AND 5 THEN 'spring'
                           WHEN Issue_Month BETWEEN 6 AND 8 THEN 'summer'
                           WHEN Issue_Month BETWEEN 9 AND 12 THEN 'fall' 
                           END AS season
                           FROM nyc_parking_tkts_2016_view")
createOrReplaceTempView(season_2016, "season_2016_view")

season_vs_no_of_tkts_2016<- SparkR::sql("SELECT season, Count(*)as no_of_tkts
                                        FROM season_2016_view
                                        GROUP BY season
                                        ORDER BY no_of_tkts desc")

season_vs_no_of_tkts_2016_df<- data.frame(head(season_vs_no_of_tkts_2016))
season_vs_no_of_tkts_2016_df$years<- c(2016,2016,2016,2016)

# Dividing 2017 dataset into 4 seasons {12,1,2} is Winter season then (3,5) as Spring, (6,8) as Summer and (9,12) as Fall
season_2017 <- SparkR::sql("SELECT Summons_Number, Violation_Code,
                           CASE WHEN Issue_Month IN (1,2,12) THEN 'winter'
                           WHEN Issue_Month BETWEEN 3 AND 5 THEN 'spring'
                           WHEN Issue_Month BETWEEN 6 AND 8 THEN 'summer'
                           WHEN Issue_Month BETWEEN 9 AND 12 THEN 'fall' 
                           END AS season
                           FROM nyc_parking_tkts_2017_view")
createOrReplaceTempView(season_2017, "season_2017_view")

season_vs_no_of_tkts_2017<- SparkR::sql("SELECT season, Count(*)as no_of_tkts
                                        FROM season_2017_view
                                        GROUP BY season
                                        ORDER BY no_of_tkts desc")

season_vs_no_of_tkts_2017_df<- data.frame(head(season_vs_no_of_tkts_2017))
season_vs_no_of_tkts_2017_df$years<- c(2017,2017,2017,2017)

# Comparison of Season vs. Frequency of Tickets over the Year 2015, 2016, 2017
joined_season_vs_no_of_tkts<- rbind(season_vs_no_of_tkts_2015_df, season_vs_no_of_tkts_2016_df, season_vs_no_of_tkts_2017_df)
ggplot(joined_season_vs_no_of_tkts, aes(x = as.factor(season) , y = no_of_tkts)) +
  geom_col() +
  facet_grid(~years) +
  xlab("Seasons") +
  ylab("Number of Tickets") + 
  ggtitle("Comparison of Seasons vs. Frequency of Tickets ") +
  geom_text(aes(label = no_of_tkts), vjust=-0.1)

#
# Q6.b Then, find the 3 most common violations for each of these season
#

# Violation Code Distribution vs season analysis for year 2015
violation_code_vs_season_2015 <- SparkR::sql("SELECT season, Violation_Code, no_of_tkts
                                             FROM (SELECT dense_rank() over (partition by season order by no_of_tkts desc) rank,
                                             season, Violation_Code, no_of_tkts
                                             FROM (SELECT season,Violation_Code, Count(*) no_of_tkts
                                             FROM season_2015_view
                                             GROUP BY season, Violation_Code))
                                             WHERE rank <= 3
                                             ORDER BY season, no_of_tkts desc")

violation_code_vs_season_2015_df <-  data.frame(head(violation_code_vs_season_2015, nrow(violation_code_vs_season_2015)))
ggplot(violation_code_vs_season_2015_df, aes(x = as.factor(Violation_Code) , y = no_of_tkts)) +
  geom_col() +
  facet_grid(~season) +
  xlab("Violation Code") +
  ylab("Number of Tickets") + 
  ggtitle("Comparison of Seasons vs. Frequency of Violation Codes for 2015 ") +
  geom_text(aes(label = no_of_tkts), vjust=-0.1)

# Violation Code Distribution vs season analysis for year 2016
violation_code_vs_season_2016 <- SparkR::sql("SELECT season, Violation_Code, no_of_tkts
                                             FROM (SELECT dense_rank() over (partition by season order by no_of_tkts desc) rank,
                                             season, Violation_Code, no_of_tkts
                                             FROM (SELECT season,Violation_Code, Count(*) no_of_tkts
                                             FROM season_2016_view
                                             GROUP BY season, Violation_Code))
                                             WHERE rank <= 3
                                             ORDER BY season, no_of_tkts desc")

violation_code_vs_season_2016_df <-  data.frame(head(violation_code_vs_season_2016, nrow(violation_code_vs_season_2016)))
ggplot(violation_code_vs_season_2016_df, aes(x = as.factor(Violation_Code) , y = no_of_tkts)) +
  geom_col() +
  facet_grid(~season) +
  xlab("Violation Code") +
  ylab("Number of Tickets") + 
  ggtitle("Comparison of Seasons vs. Frequency of Violation Codes for 2016 ") +
  geom_text(aes(label = no_of_tkts), vjust=-0.1)

# Violation Code Distribution vs season analysis for year 2017
violation_code_vs_season_2017 <- SparkR::sql("SELECT season, Violation_Code, no_of_tkts
                                             FROM (SELECT dense_rank() over (partition by season order by no_of_tkts desc) rank,
                                             season, Violation_Code, no_of_tkts
                                             FROM (SELECT season,Violation_Code, Count(*) no_of_tkts
                                             FROM season_2017_view
                                             GROUP BY season, Violation_Code))
                                             WHERE rank <= 3
                                             ORDER BY season, no_of_tkts desc")

violation_code_vs_season_2017_df <-  data.frame(head(violation_code_vs_season_2017, nrow(violation_code_vs_season_2017)))
ggplot(violation_code_vs_season_2017_df, aes(x = as.factor(Violation_Code) , y = no_of_tkts)) +
  geom_col() +
  facet_grid(~season) +
  xlab("Violation Code") +
  ylab("Number of Tickets") + 
  ggtitle("Comparison of Seasons vs. Frequency of Violation Codes for 2017 ") +
  geom_text(aes(label = no_of_tkts), vjust=-0.1)

#
# Q7. The fines collected from all the parking violation constitute a revenue source for the NYC police 
#     department. Let's take an example of estimating that for the 3 most commonly occurring codes.
#
# a.  Find total occurrences of the 3 most common violation codes
#
# b. Then, search the internet for NYC parking violation code fines. You will find a website (on the nyc.gov URL)
#    that lists these fines. They're divided into two categories, one for the highest-density locations of the
#    city, the other for the rest of the city. For simplicity, take an average of the two.
#
# c. Using this information, find the total amount collected for all of the fines. State the code which has the highest total collection.
#
# d. What can you intuitively infer from these findings?
#
#Finding the total fine amount for top 3 violation code in the year 2015
violation_code_freq_2015<- SparkR::sql("SELECT Violation_Code, count(*) as no_of_tkts
                                       FROM nyc_parking_tkts_2015_view 
                                       GROUP BY Violation_Code
                                       ORDER BY no_of_tkts desc")
head(violation_code_freq_2015,3)
# code -----------> Avg fine
# 21   -----------> $55
# 38   -----------> $50
# 14   -----------> $115

top3_violation_code_2015<- data.frame(head(violation_code_freq_2015,3))
top3_violation_code_2015$year <- c(2015,2015,2015)
top3_violation_code_2015$avg_fine<- c(55,50,115)
top3_violation_code_2015$total_amount<- top3_violation_code_2015$no_of_tkts * top3_violation_code_2015$avg_fine
top3_violation_code_2015

#Finding the total fine amount for top 3 violation code in the year 2016
violation_code_freq_2016<- SparkR::sql("SELECT Violation_Code, count(*) as no_of_tkts
                                       FROM nyc_parking_tkts_2016_view 
                                       GROUP BY Violation_Code
                                       ORDER BY no_of_tkts desc")
head(violation_code_freq_2016,3)
# code -----------> Avg fine
# 21   -----------> $55
# 36   -----------> $50
# 38   -----------> $50

top3_violation_code_2016<- data.frame(head(violation_code_freq_2016,3))
top3_violation_code_2016
top3_violation_code_2016$year <- c(2016,2016,2016)
top3_violation_code_2016$avg_fine<- c(55,50,50)
top3_violation_code_2016$total_amount<- top3_violation_code_2016$no_of_tkts * top3_violation_code_2016$avg_fine
top3_violation_code_2016

#Finding the total fine amount for top 3 violation code in the year 2017
violation_code_freq_2017<- SparkR::sql("SELECT Violation_Code, count(*) as no_of_tkts
                                       FROM nyc_parking_tkts_2017_view 
                                       GROUP BY Violation_Code
                                       ORDER BY no_of_tkts desc")
head(violation_code_freq_2017,3)
# code -----------> Avg fine
# 21   -----------> $55
# 36   -----------> $50
# 38   -----------> $50

top3_violation_code_2017<- data.frame(head(violation_code_freq_2017,3))
top3_violation_code_2017$year <- c(2017,2017,2017)
top3_violation_code_2017$avg_fine<- c(55,50,50)
top3_violation_code_2017$total_amount<- top3_violation_code_2017$no_of_tkts * top3_violation_code_2017$avg_fine
top3_violation_code_2017

top3_violation_code<- rbind(top3_violation_code_2015, top3_violation_code_2016, top3_violation_code_2017)
ggplot(top3_violation_code, aes(x = as.factor(Violation_Code) , y = no_of_tkts)) +
  geom_col() +
  facet_grid(~year) +
  xlab("Violation Code") +
  ylab("Number of Tickets") + 
  ggtitle("Top 3 violation code vs. number of tickets over the years ") +
  geom_text(aes(label = no_of_tkts), vjust=-0.1)

##########################     END OF ANALYSIS  #######################
