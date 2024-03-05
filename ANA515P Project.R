#ANA 515P Data Preparation Project
#Author: Tiger Wang
#Dataset: Presidential Polling


#Loading Tidyverse
library(tidyverse)


#Loading the datasets
library(readxl)
dataset1 <- read_excel("C:/Users/totog/Downloads/presidential_polls_2020.xlsx", sheet = "Sheet1")
dataset2 <- read_excel("C:/Users/totog/Downloads/presidential_polls_2020.xlsx", sheet = "Sheet2")

#Merging the datasets
data <- merge(dataset1, dataset2, all = TRUE)
#Merge function will replace NA values with "0", but we can safely assume all 0 values are NA because no correct input in the dataset should have a value of 0
#We can replace 0 values with NA again so that we can use functions that will find missing values later in the project
data[data == 0] <- NA

#Removing entire rows of missing values
library(janitor)
data <- remove_empty(data, "rows")
#Through visual inspection of the dataset we can see there are many rows of missing values, so we begin by removing these empty rows


#Cleaning "tracking"
#The column tracking has a large number of missing values so we need to clean this first, from further research on FiveThirtyEight we realize that this is a T/F binary variable
#The missing values likely indicate F as the value, so we replace the missing values with "F"
table(unlist(data$tracking))
data$tracking <- replace(data$tracking, data$tracking == "TR", "T")
data$tracking <- replace(data$tracking, data$tracking == "X", "T")
data$tracking <- replace(data$tracking, is.na(data$tracking), "F")
#Checking results
table(unlist(data$tracking))


#Removing rows with more than two missing values
delete.na <- function(DF, n=0) {DF[rowSums(is.na(DF)) <= n,]}
data <- delete.na(data, 2)
#We want to set a threshold for the quality of data we use in the analysis, and we determine that we will only keep rows with less than 2 missing variables so that the data quality is sufficient


#Cleaning duplicates
data <- unique(data)
#The unique function will delete duplicate rows


#Next we begin cleaning the dataset on a column by column basis


#Cleaning "cycle"
#Correcting inconsistencies in typos, spelling, and formatting
table(unlist(data$cycle))
data$cycle <- replace(data$cycle, data$cycle == 20, 2020)
#Checking results
table(unlist(data$cycle))
#The table function will list out all unique inputs and their frequencies, the replace function is used to correct errors and inconsistencies


#Cleaning "state"
#Correcting inconsistencies in typos, spelling, and formatting
table(unlist(data$state))
data$state <- replace(data$state, data$state == "NATl", "National")
data$state <- replace(data$state, data$state == "NC", "North Carolina")
data$state <- replace(data$state, data$state == "PA", "Pennsylvania")
data$state <- replace(data$state, data$state == "WI", "Wisconsin")
data$state <- replace(data$state, data$state == "WY", "Wyoming")
#Checking results
table(unlist(data$state))


#Cleaning "candidate_name"
#Correcting inconsistencies in typos, spelling, and formatting
table(unlist(data$candidate_name))
data$candidate_name <- replace(data$candidate_name, data$candidate_name == "Biden", "Joseph R. Biden Jr.")
#Checking results
table(unlist(data$candidate_name))


#Cleaning "pollster"
#Correcting inconsistencies in typos, spelling, and formatting
table(unlist(data$pollster))
data$pollster <- replace(data$pollster, data$pollster == "Isos", "Ipsos")
data$pollster <- replace(data$pollster, data$pollster == "Morning Con", "Morning Consult")
data$pollster <- replace(data$pollster, data$pollster == "MorningConsult", "Morning Consult")
data$pollster <- replace(data$pollster, data$pollster == "Survey Monkey", "SurveyMonkey")
#Checking results
table(unlist(data$pollster))


#Cleaning "samplesize"
#Using visualization to check for anomalies in sample size
hist(data$samplesize, xlim = c(0,35000), ylim = c(0,30), breaks = 200)
#From the frequency distribution we see that sample sizes range from near 0 to 35,000. Since this reflects sample size in political polls, surveys up to 35,000 samples do not seem out of the ordinary, but sample sizes near 0 are likely anomalies.
#We can replace the near 0 sample sizes with the average sample size in the polls to correct the anomalies.
data$samplesize <- replace(data$samplesize, data$samplesize < 20, round(mean(data$samplesize, na.rm = TRUE)))


#Cleaning "population"
#Check for missing values
is.na(data$population)
#Check for other errors
table(unlist(data$population))
data$population <- replace(data$population, data$population == "l", "lv")
data$population <- replace(data$population, data$population == "LV", "lv")
#There is 1 missing value in the column, we can replace it with the mode value "lv", which is significantly more common than other values in this column
#The missing value is manually inputed as "NA" so we replace with the following code:
data$population <- replace(data$population, data$population == "NA", "lv")
#Checking results
table(unlist(data$population))


#Cleaning "pct", "house_adjusted_pct", and "trend_and_house_adjusted_pct"
#Check for missing values
is.na(data$pct)
is.na(data$house_adjusted_pct)
is.na(data$trend_and_house_adjusted_pct)
#No missing values
#Using visualization to check for anomalies, percentage values should range between 0-100
class(data$pct)
#pct is not numeric, so we need to check for class and change data type to numeric
data$pct <- as.numeric(data$pct)
hist(data$pct, xlim = c(0,100), ylim = c(0,200),
     breaks = 10)
#an NA was introduced, so we realize there is indeed 1 missing value
#we decide to use the average value of its house_adjusted_pct and trend_and_house_adjusted_pct as the three values appear to be similar for most other rows and the pct usually falls in the  middle
data$pct <- replace(data$pct, is.na(data$pct), 51.50)
#plot frequency distribution
hist(data$pct, xlim = c(0,100), ylim = c(0,200), breaks = 10)
#visualizations reveal no anomalies
#Repeating the process for house_adjusted_pct and trend_and_house_adjusted_pct
hist(data$house_adjusted_pct, xlim = c(0,100), ylim = c(0,200), breaks = 10)
hist(data$trend_and_house_adjusted_pct, xlim = c(0,100), ylim = c(0,200), breaks = 10)
#Both of these columns are numeric. Visualization reveals a likely anomaly in trend_and_house_adjusted_pct
#Visual inspection found a value of 99 in row 235 for trend_and_house_adjusted_pct, this is likely an anomaly
#Replace with average value of pct and house_adjusted_pct for row 235
data$trend_and_house_adjusted_pct <- replace(data$trend_and_house_adjusted_pct, data$trend_and_house_adjusted_pct == 99, 50.01)


#Cleaning "modeldate", "startdate", and "enddate"
is.na(data$modeldate)
is.na(data$startdate)
is.na(data$enddate)
#no missing values
#checking for other errors in "modeldate"
table(unlist(data$modeldate))
#there are two anomalies in the column, we replace the values to be consistent with all the other values in the column
data$modeldate <- replace(data$modeldate, data$modeldate == 20, 44138)
data$modeldate <- replace(data$modeldate, data$modeldate == 11111, 44138)
#checking for other errors in "startdate"
table(unlist(data$startdate))
#there are 3 manually inputed NA values, 
#as fixing this will require going back to the data source to determine when each poll began, we leave this part as is for now until startdate becomes a necessary input in the analysis 
#checking for other errors in "enddate"
table(unlist(data$enddate))
#no anomalies


#Cleaning "weight"
is.na(data$weight)
#There are several missing values in the dataset
#Checking for other errors
class(data$weight)
#Data type is character, need to change to numeric
data$weight <- as.numeric(data$weight)
#Checking for anomalies
hist(data$weight)
table(unlist(data$weight))
#there are two anomalies, we replace them with the median value of the column
#we do not use the mean value as the two anomaly values will skew the mean
data$weight <- replace(data$weight, data$weight == 99, median(data$weight, na.rm = TRUE))
data$weight <- replace(data$weight, data$weight == 400, median(data$weight, na.rm = TRUE))
#We can then fix the missing values using the mean value of the column
data$weight <- replace(data$weight, is.na(data$weight), mean(data$weight, na.rm = TRUE))


#Cleaning "influence"
is.na(data$influence)
#A majority of the inputs in influence have missing values, we recommend dropping the column as it is difficult to use a mean, median, or mode to fill the missing values
data <- data[,-11]

#Cleaning "pollid" and "questionid"
is.na(data$pollid)
is.na(data$questionid)
#no missing values


#Exporting the data frame
write.csv(data, "C:/Users/totog/Downloads/PresidentialPolls_Cleaned.csv")