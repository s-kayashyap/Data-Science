# Uber Supply-Demand Gap 
# Input Dataset : Uber Request Data.csv
# Output Dataset: Uber Data cleaned.csv
# Objective : The aim of this analysis is to identify the root cause of the problem (i.e. cancellation
#             and non-availability of cabs) and recommend ways to improve the situation. As a result
#             of this analysis, we should be able to present to the client the root cause(s) and
#             possible hypotheses of the problem(s) and recommend ways to improve them.  
# Name : Swami Prem Pranav Kayashyap (APFE1786831)
# ------------------------------------------------------
# Date : 11th Mar, 2018

#Import Uber data from "Uber Request Data.csv" file
uber_data <- read.csv(file.choose(),stringsAsFactors = FALSE)

#Converting all pickup points to lower case
uber_data$Pickup.point <- sapply(uber_data$Pickup.point, tolower)

#Converting all status to lower case
uber_data$Status <- sapply(uber_data$Status, tolower)

#list of format to idetify heterogeneous datetime data in our dataframe   
date_format_list <- c( "%d/%m/%Y %H:%M", "%d-%m-%Y %H:%M:%S")

#Cleaning Request time data and creating Request hours (derived data) column
i <- 1
for (mydate in uber_data$Request.timestamp) {
  for (date_format in date_format_list) {
    cab_request_timestamp <- as.POSIXct(mydate, format = date_format)

    if(!is.na(cab_request_timestamp)){
      cab_request_time <- format(cab_request_timestamp,"%d-%m-%Y %H:%M:%S")
      uber_data$RequestTime[i] <- cab_request_time
      
      cab_request_hour <- format(cab_request_timestamp,"%H")
      uber_data$RequestHours[i] <- cab_request_hour
      break;
    }
  }
  i <- i+1
}

#Cleaning Drop time data and creating drop hours (derived data) column
i <- 1
for (mydate in uber_data$Drop.timestamp) {
  for (date_format in date_format_list) {
    cab_drop_timestamp <- as.POSIXct(mydate, format = date_format)
    
    if(!is.na(cab_drop_timestamp)){
      cab_drop_time <- format(cab_drop_timestamp,"%d-%m-%Y %H:%M:%S")
      uber_data$DropTime[i] <- cab_drop_time
      
      cab_drop_hour <- format(cab_drop_timestamp,"%H")
      uber_data$DropHour[i] <- cab_drop_hour
      break;
    }
  }
  i <- i+1
}


#Removing unnecessary column data
uber_data$Request.timestamp <- NULL
uber_data$Drop.timestamp <- NULL

#Saving Cleaned Uber Data to do analysis on tableau. 
write.csv(uber_data,"Uber Data cleaned.csv")

#load the required packages 
require(dplyr)
require(ggplot2)
#problem 1. Large number of cab requests from city to airport during the Morning (5AM to 9AM) Time slot.
#           Large number of cab requests from airport to city during the Evening (5PM to 9PM) Time slot.

# Plot the number of cabs requestes from various pickup points
cab_request_vs_hour <- ggplot(uber_data,aes(x=factor(RequestHours),fill=factor(Pickup.point)))

plot1 <- cab_request_vs_hour + geom_bar(stat='count', position = "dodge") +
  ggtitle("Uber Cabs requests from various pickup point") +
  labs(x="Time in Hours", y="Number of Cabs Requests") +
  labs(fill="Pickup Point")

#view the plot
plot1


#problem 2. Large number of cab requests got cancelled during the Morning (5AM to 9AM) Time slot

#Subsetted dataframe should only consist of cancelled request
cancelled_cab_request <- subset(uber_data, uber_data$Status == "cancelled")

cancelled_cab_request_vs_hour <- ggplot(cancelled_cab_request,aes(x=factor(RequestHours)))
plot2 <- cancelled_cab_request_vs_hour + geom_bar(stat="count", position = "dodge") +
  ggtitle("Cancelled Trip during various time slots") +
  labs(x="Time in Hours", y="Number of Cabs Requests")

#view the plot
plot2

#problem 3. Large number of cab requests when no cars available during the late Evening (5PM to 9PM) Time slot.

#Subsetted dataframe should only consist of No cars available request 
nocars_cab_request <- subset(uber_data, uber_data$Status == "no cars available")

nocars_cab_request_vs_hour <- ggplot(nocars_cab_request,aes(x=factor(RequestHours)))
plot3 <- nocars_cab_request_vs_hour + geom_bar(stat="count", position = "dodge") +
  ggtitle("No cars available during various time slots") +
  labs(x="Time in Hours", y="Number of Cabs Requests")

#view the plot
plot3

#Analysis of gap between supply and demand

# Total drivers available to serve. 
total_cab_driver <- summarise(uber_data, n_distinct(Driver.id))
total_cab_driver <- total_cab_driver - 1 #Removing NA value count

#total cab requests
total_requests <- count(uber_data)

#total request from city
total_request_city <- length(which(uber_data$Pickup.point == "city"))

#total request from airport
total_request_airport <- length(which(uber_data$Pickup.point == "airport"))

#total requests from the city
total_completed <- length(which(uber_data$Status == "trip completed"))

#total Completed requests
total_completed <- length(which(uber_data$Status == "trip completed"))

#Total utilization
utilization <- (total_completed/total_cab_driver*100)

#Analysis of gap between supply and demand in early morning (5AM to 9AM) timeslot
early_morning_requests <- subset(uber_data,uber_data$RequestHours == "05"| uber_data$RequestHours == "06" | uber_data$RequestHours == "07" | uber_data$RequestHours == "08" | uber_data$RequestHours == "09")

# No of cancelled cab requests for early morning timeslot
em_total_cancelled <- length(which(early_morning_requests$Status == "cancelled"))

# No of cancelled cab requests from airport for early morning timeslot
em_airport_cancelled <- length(which(early_morning_requests$Pickup.point == "airport" & early_morning_requests$Status == "cancelled"))

# No of cab requests from city with no cars available for early morning timeslot
em_city_cancelled <- length(which(early_morning_requests$Pickup.point == "city" & early_morning_requests$Status == "cancelled"))

# Percentage of cancelled requests from airport to total no cars available during morning rush
em_percent_airport_cancelled <- (em_airport_cancelled/em_total_cancelled*100)

# Percentage of cancelled requests from city to total cancelled requests during morning rush
em_percent_city_cancelled <- (em_city_cancelled/em_total_cancelled*100)

#Analysis of gap between supply and demand in late evening (5PM to 9PM) timeslot
late_evening_requests <- subset(uber_data,uber_data$RequestHours == "17"| uber_data$RequestHours == "18" | uber_data$RequestHours == "19" | uber_data$RequestHours == "20" | uber_data$RequestHours == "21")

# No of cab requests with no cars available for late evening timeslot
le_total_nocar_available <- length(which(late_evening_requests$Status == "no cars available"))

# No of cab requests from airport with no cars available for late evening timeslot
le_airport_nocar_available <- length(which(late_evening_requests$Pickup.point == "airport" & late_evening_requests$Status == "no cars available"))

# No of cab requests from city with no cars available for late evening timeslot
le_city_nocar_available <- length(which(late_evening_requests$Pickup.point == "city" & late_evening_requests$Status == "no cars available"))

# Percentage of no cars available status from airport to total no cars available during evening rush
le_percent_airport_nocar <- (le_airport_nocar_available/le_total_nocar_available*100)

# Percentage of no_cars_available status from city to total no_cars_available during evening rush
le_percent_city_nocar <- (le_city_nocar_available/le_total_nocar_available*100)

