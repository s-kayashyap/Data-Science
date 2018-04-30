# R Exercise with various libraries 
# Input Dataset : loan_cleaned.csv
# Name : Swami Prem Pranav Kayashyap (APFE1786831)
# ------------------------------------------------------
# Date : 7th Jan, 2018

#this is how comment can be written
a <- 2 #assign 2 into variable a
b <- 3
c <- a + b
C <- b - a #R is case sensitive

#vector
?c
num_vector <- c(12,13,14,15,16,27,23,25,32,36,321)
num_vector
num_vector[1]

str_vector_1 <- c("swami", "prem", "pranav", "kayashyap")
str_vector_1
str_vector_2 <- c('swami', 'prem', 'pranav', 'kayashyap')
str_vector_2

class(num_vector)
class(str_vector_2)

#Factor
?factor
my_vector_factor <- c(1,2,4,3,5,6,3,2,4,5,3,2,3,4,5,6,5,4,3,6,4,5,3,2,1,2,3,4,5,6) 
my_factor <- factor(my_vector_factor)
my_factor
my_factor[1]
levels(my_factor)

#Matrics
?matrix
my_matrix <- matrix(1:12, byrow = FALSE, nrow = 3)
my_matrix <- matrix(1:12, byrow = TRUE, nrow = 3)
my_matrix
my_matrix[3,2]

#data frame
?data.frame
?rbind
?cbind
col1 <- c("cr11","cr12","cr13","cr14","cr15")
col2 <- c("cr21","cr22","cr23","cr24","cr25")
col3 <- c("cr31","cr32","cr33","cr34","cr35")

my_data_frame1 <- data.frame(col1,col2,col3)
my_data_frame1

col4 <- c("cr41","cr42","cr43","cr44","cr45")
col5 <- c("cr51","cr52","cr53","cr54","cr55")

my_data_frame2 <- data.frame(col4, col5)
my_data_frame2

col1 <- c("cr16","cr17","cr18","cr19")
col2 <- c("cr26","cr27","cr28","cr29")
col3 <- c("cr36","cr37","cr38","cr39")
col4 <- c("cr46","cr47","cr48","cr49")
col5 <- c("cr56","cr57","cr58","cr59")

my_data_frame3 <- data.frame(col1, col2, col3, col4, col5)
my_data_frame3

my_data_frame4 <- cbind(my_data_frame1, my_data_frame2)
my_data_frame4

my_data_frame4 <- rbind(my_data_frame4, my_data_frame3)
my_data_frame4

my_data_frame4$col2[3]
my_data_frame4[3,2]

#list
my_list <- list(num_vector, str_vector_1, my_factor, my_matrix, my_data_frame4)
my_list
my_list[[1]][1]

#Conditional statement
if(my_list[[1]][1] == 12) {
  print("if condition is true")
}else {
  print("if condition is false")
}

if(my_list[[1]][1] == 100) {
  print("if condition is true")
}else {
  print("if condition is false")
}

#Looping statement
for(index in my_list[[1]]) {
  print(index)
}

i <- 1
while (i < 6) {
  print(i)
  i <- i+1  
}

#Function
my_num_vector <- my_list[[1]]
my_num_vector <- c(NA,my_num_vector,NA)
my_num_vector

#Built-in function 
mean(my_num_vector,na.rm = TRUE)

sd(my_num_vector,na.rm = TRUE)

is.na(my_num_vector)
which(!is.na(my_num_vector))
sum(is.na(my_num_vector))
my_num_vector[which.max(my_num_vector)]
max(my_num_vector, na.rm = TRUE)

#User defined function
find_minimum_number <- function(my_num_vector_argument) {
  min_num <- min(my_num_vector_argument, na.rm = TRUE)
    return(min_num)
}

min_number <- find_minimum_number(my_num_vector)

#apply Family

#apply() // apply(matrix or 2D array, apply on rows or on cols, applying function )
apply(my_data_frame4,c(1,2),toupper)
my_matrix
increase_by_two <- function(input){
  return(input+2)
}
apply(my_matrix, 1,sum)
apply(my_matrix, 2,sd)
apply(my_matrix, c(1,2),increase_by_two)

#Import cleaned loan data from loan_cleaned.csv to just understand about few R functionality.
#This is how we import data from csv file into R data frame.
loan <- read.csv(file.choose(),stringsAsFactors = F, na.strings = c("NA", "N/A", "", " "))

#lapply() applicable for list only lapply(my_list,"[",rows_number,cols_number)
?lapply
my_list
lapply(my_list,"[",1)
lapply(my_list, toupper)
loan <- loan[1:10,1:3]
loan$id[1] <- "prem";
lapply(loan, function(v) {
  if(is.character(v)) {
    return(toupper(v))
  }else {
    return(v)
  }
})

loan$id[1] <- 1200;
str(loan)
loan$id <- as.integer(as.factor(loan$id))

my_list_output <- lapply(loan,sum)
simple_var <- sapply(loan, sum)
simple_var
my_list_output
loan <- loan[vapply(loan, function(x) length(unique(x)) > 1, logical(1L))]

my_fun <- function(x) {
  if(length(unique(x)) > 1) {
    return(logical(1L))
  }else {
    return(logical(0L))
  }
}

lapply(loan, my_fun)

#Now learn abount few R library
