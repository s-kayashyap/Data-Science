# Spark Funds Investment Analysis 
# Input Dataset : companies.txt, rounds2.csv, eng_countries.csv, mapping.csv
# Output Dataset : Investments_Analyis_Result.xlsx
# Objective : The objective is to identify the best sectors, countries, and a suitable investment type for making investments.
#             The overall strategy is to invest where others are investing, implying that the best sectors and countries are
#             the ones where most investments are happening.
# Name : Swami Prem Pranav Kayashyap (APFE1786831)
# ------------------------------------------------------
# Date : 4th Feb, 2018

#Checkpoint 1: Data Cleaning 1

#Load the companies ("companies.txt") and rounds ("rounds2.csv") data (provided on the previous page) into two data frames 
#and name them companies and rounds2 respectively.
companies <- read.delim(file.choose(),header = TRUE, sep = "\t", stringsAsFactors = F)
rounds2 <- read.csv(file.choose(), stringsAsFactors = F)

library(dplyr)

# Convert company's permalink data and Investment's company_permalink to same case so that we can
# merge both the data frame. 
companies$permalink <- sapply(companies$permalink, tolower)
rounds2$company_permalink <- sapply(rounds2$company_permalink, tolower)

# How many unique companies are present in rounds2?
distinct_investment_company <- distinct(rounds2, company_permalink)
#investment_companies_count <- summarise(rounds2, n_distinct(rounds2$company_permalink))

# How many unique companies are present in companies?
distinct_company <- distinct(companies, permalink)
#companies_count <- summarise(companies, n_distinct(companies$permalink))

# Merge the two data frames so that all variables (columns) in the companies frame are added to the
# rounds2 data frame. Name the merged frame master_frame. How many observations are present in 
# master_frame?
master_frame <- merge(x = rounds2, y = companies, by.x = "company_permalink", by.y = "permalink")

# Checkpoint 2: Funding Type Analysis

# Calculate the average investment amount for each of the four funding types (venture, angel, seed,
# and private equity )
funding_avg_investment <- summarise(group_by(filter(master_frame, grepl("venture|angel|seed|private_equity", funding_round_type)), funding_round_type), mean(raised_amount_usd, na.rm = T))

# Checkpoint 3: Country Analysis

# For the chosen investment type, make a data frame named top9 with the top nine countries. 
# based on the total investment amount each country has received. country_wis_investment
top9 <- head(arrange(summarise(group_by(filter(master_frame, grepl("venture", funding_round_type),country_code != ""), country_code), investment = sum(raised_amount_usd, na.rm = T)), desc(investment)),9)

# import file "eng_countries.csv" for Analysing the Top 3 English-Speaking Countries
countries_language <- read.csv(file.choose())
top9_with_language <- merge(top9, countries_language, by = "country_code")
top9_with_language <- arrange(top9_with_language, desc(investment) )

# Checkpoint 4: Sector Analysis 1

library(stringr)

# Extract the primary sector of each category list from the category_list column
master_frame <-  separate(master_frame, category_list, into = c("primary_sector"), sep = "[|]", remove = "F")

# Checkpoint 5: Sector Analysis 2

# import mapping data from "mapping.csv".
#mapping each primary sector to one of the eight main sectors
sector_mapping <- read.csv(file.choose())

#gather the data together. We need to have a key value pair. The key is the main_sector and the 
#value is my_value
sector_mapping <- gather(sector_mapping, main_sector, my_value, 2:10)
# Removing the rows having value of my_value as 0 
sector_mapping <- sector_mapping[!(sector_mapping$my_value == 0),]
# Removing the my_value column information from dataframe.
sector_mapping <- sector_mapping[,-3]
# Code for a merged data frame with each primary sector mapped to its main sector (the primary sector should be present
# in a separate column).
master_frame <- merge(x = master_frame,  y = sector_mapping, by.x = 'primary_sector', by.y = 'category_list')

library(sqldf)
# Create three separate data frames D1, D2 and D3 for each of the three countries containing the
# observations of funding type FT falling within the 5-15 million USD range.
D1 <- sqldf("select *, count(company_permalink) as num_of_investment, sum(raised_amount_usd) as invested_amount from master_frame where country_code = 'USA' group by main_sector")
D2 <- sqldf("select *, count(company_permalink) as num_of_investment, sum(raised_amount_usd) as invested_amount from master_frame where country_code = 'GBR' group by main_sector")
D3 <- sqldf("select *, count(company_permalink) as num_of_investment, sum(raised_amount_usd) as invested_amount from master_frame where country_code = 'IND' group by main_sector")


# Analysis for D1

#Total number of investments (count)
D1_total_no_of_investment <- sqldf("select sum(num_of_investment) from D1")

#Total amount of investment (USD)
D1_total_investment_amount <- sqldf("select sum(invested_amount) from D1")

# Top Sector name (no. of investment-wise)
D1_top_sector_name <- sqldf("select main_sector from D1 order by num_of_investment desc limit 0,1")

# Second Sector name (no. of investment-wise)
D1_second_sector_name <- sqldf("select main_sector from D1 order by num_of_investment desc limit 1,1")

# Third Sector name (no. of investment-wise)
D1_third_sector_name <- sqldf("select main_sector from D1 order by num_of_investment desc limit 2,1")

# Number of investments in top sector (3)
D1_no_of_investment_top_sector <- sqldf("select num_of_investment from D1 order by num_of_investment desc limit 0,1")

# Number of investments in second sector (4)
D1_no_of_investment_second_sector <- sqldf("select num_of_investment from D1 order by num_of_investment desc limit 1,1")

# Number of investments in third sector (5)
D1_no_of_investment_third_sector <- sqldf("select num_of_investment from D1 order by num_of_investment desc limit 2,1")

# For point 3 (top sector count-wise), which company received the highest investment?
D1_company_highest_investment <- sqldf("select name from master_frame where main_sector = 'Others' group by company_permalink order by sum(raised_amount_usd) desc limit 0,1")

# For point 4 (second best sector count-wise), which company received the highest investment?
D1_company_second_highest_investment <- sqldf("select name from master_frame where main_sector = 'Cleantech...Semiconductors' group by company_permalink order by sum(raised_amount_usd) desc limit 0,1")

# Analysis for D2

#Total number of investments (count)
D2_total_no_of_investment <- sqldf("select sum(num_of_investment) from D2")

#Total amount of investment (USD)
D2_total_investment_amount <- sqldf("select sum(invested_amount) from D2")

# Top Sector name (no. of investment-wise)
D2_top_sector_name <- sqldf("select main_sector from D2 order by num_of_investment desc limit 0,1")

# Second Sector name (no. of investment-wise)
D2_second_sector_name <- sqldf("select main_sector from D2 order by num_of_investment desc limit 1,1")

# Third Sector name (no. of investment-wise)
D2_third_sector_name <- sqldf("select main_sector from D2 order by num_of_investment desc limit 2,1")

# Number of investments in top sector (3)
D2_no_of_investment_top_sector <- sqldf("select num_of_investment from D2 order by num_of_investment desc limit 0,1")

# Number of investments in second sector (4)
D2_no_of_investment_second_sector <- sqldf("select num_of_investment from D2 order by num_of_investment desc limit 1,1")

# Number of investments in third sector (5)
D2_no_of_investment_third_sector <- sqldf("select num_of_investment from D2 order by num_of_investment desc limit 2,1")

# For point 3 (top sector count-wise), which company received the highest investment?
D2_company_highest_investment <- sqldf("select name from master_frame where main_sector = 'Others' group by company_permalink order by sum(raised_amount_usd) desc limit 0,1")

# For point 4 (second best sector count-wise), which company received the highest investment?
D2_company_second_highest_investment <- sqldf("select name from master_frame where main_sector = 'Social..Finance..Analytics..Advertising' group by company_permalink order by sum(raised_amount_usd) desc limit 0,1")

# Analysis for D3

#Total number of investments (count)
D3_total_no_of_investment <- sqldf("select sum(num_of_investment) from D3")

#Total amount of investment (USD)
D3_total_investment_amount <- sqldf("select sum(invested_amount) from D3")

# Top Sector name (no. of investment-wise)
D3_top_sector_name <- sqldf("select main_sector from D3 order by num_of_investment desc limit 0,1")

# Second Sector name (no. of investment-wise)
D3_second_sector_name <- sqldf("select main_sector from D3 order by num_of_investment desc limit 1,1")

# Third Sector name (no. of investment-wise)
D3_third_sector_name <- sqldf("select main_sector from D3 order by num_of_investment desc limit 2,1")

# Number of investments in top sector (3)
D3_no_of_investment_top_sector <- sqldf("select num_of_investment from D3 order by num_of_investment desc limit 0,1")

# Number of investments in second sector (4)
D3_no_of_investment_second_sector <- sqldf("select num_of_investment from D3 order by num_of_investment desc limit 1,1")

# Number of investments in third sector (5)
D3_no_of_investment_third_sector <- sqldf("select num_of_investment from D3 order by num_of_investment desc limit 2,1")

# For point 3 (top sector count-wise), which company received the highest investment?
D3_company_highest_investment <- sqldf("select name from master_frame where main_sector = 'Others' group by company_permalink order by sum(raised_amount_usd) desc limit 0,1")

# For point 4 (second best sector count-wise), which company received the highest investment?
D3_company_second_highest_investment <- sqldf("select name from master_frame where main_sector = 'News..Search.and.Messaging' group by company_permalink order by sum(raised_amount_usd) desc limit 0,1")

