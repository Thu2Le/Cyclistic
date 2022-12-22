# installing the packages and loading the library
install.packages("tidyverse")
install.packages("lubridate")
install.packages("ggplot2")

library(tidyverse)
library(lubridate)
library(ggplot2)

# get the directory that my files was in
getwd()

# downloadeding the CSV file using the "read.csv" function
September_2021 <- read.csv("202109-divvy-tripdata.csv")
October_2021 <- read.csv("202110-divvy-tripdata.csv")
November_2021 <- read.csv("202111-divvy-tripdata.csv")
December_2021 <- read.csv("202112-divvy-tripdata.csv")
January_2022 <- read.csv("202201-divvy-tripdata.csv")
February_2022 <- read.csv("202202-divvy-tripdata.csv")
March_2022 <- read.csv("202203-divvy-tripdata.csv")
April_2022 <- read.csv("202204-divvy-tripdata.csv")
May_2022 <- read.csv("202205-divvy-tripdata.csv")
June_2022 <- read.csv("202206-divvy-tripdata.csv")
July_2022 <- read.csv("202207-divvy-tripdata.csv")
August_2022 <- read.csv("202208-divvy-tripdata.csv")

# comparing the column names of each of the CSV file to make sure that all contains the same number of columns and each columns are labeled according.
colnames(September_2021)
colnames(October_2021)
colnames(November_2021)
colnames(December_2021)
colnames(January_2022)
colnames(February_2022)
colnames(March_2022)
colnames(April_2022)
colnames(May_2022)
colnames(June_2022)
colnames(July_2022)
colnames(August_2022)

# compiling each of the 12 CSV file into one data frame so I can anlayze the data as a whole set instead of analyzing each one of the months seperately.
temp = list.files(pattern="*.csv")
myfiles = lapply(temp, read.csv)

cyclistic_merged <- do.call("rbind", myfiles)

# preview the data by using the head(), colnames() and View() function
head(cyclistic_merged)
colnames(cyclistic_merged)
View(cyclistic_merged)

# inspected the dataframe by using the str() function and look for any incongruencies.
str(cyclistic_merged)

# I converted "ride_id" and "rideable_type" to character so that they can stack correctly.
cyclistic_merged <- mutate(cyclistic_merged, ride_id=as.character(ride_id),rideable_type=as.character(rideable_type))

# renamed the "cyclistic_merged" dataframe to "all_trips".
# removed duplicates such that the "ride_id" column only contains distinct riders.
all_trips <- cyclistic_merged[!duplicated(cyclistic_merged$ride_id),]

print(paste("Removed",nrow(cyclistic_merged) - nrow(all_trips),"duplicated rows"))

str(all_trips)

colnames(all_trips) #List of column names
nrow(all_trips) #How many rows are in data frame?
dim(all_trips) #Dimensions of the data frame
head(all_trips) #Preview the first 6 rows of data frame
tail(all_trips) #Preview last few rows of data frame
str(all_trips) #See list of columns and data types (numeric,character,etc)
summary(all_trips) #Statistical summary of data. Mainly for numerics

all_trips$date <- as.Date(all_trips$started_at) #The default format is yyyy-mm-dd
all_trips$month <- format(as.Date(all_trips$date),"%m")
all_trips$day <- format(as.Date(all_trips$date),"%d")
all_trips$year <- format(as.Date(all_trips$date),"%Y")
all_trips$day_of_week <- format(as.Date(all_trips$date),"%A")

# I calculated the "ride_length" by using the difftime() function. 
# This will find the difference between "ended_at" and "started_at" time in seconds.
all_trips$ride_length <- difftime(all_trips$ended_at,all_trips$started_at)

str(all_trips)

# I convert "ride_length" to a numeric data type to run further calculation on the data.
is.factor(all_trips$ride_length)
all_trips$ride_length <- as.numeric(as.character(all_trips$ride_length))
is.numeric(all_trips$ride_length)

# created a new version of the dataframe, now named, "all_trips_v2" to removed negative observations in the variable "ride_length" or "start_station_name" containing "HQ QR", since this is when the bikes were taken out of rotation for quality check.
all_trips_v2 <- all_trips[!(all_trips$start_station_name == "HQ QR" | all_trips$ride_length<0),]

mean(all_trips_v2$ride_length) #straight average (total ride length/rides) 
median(all_trips_v2$ride_length) #midpoint number in the ascending array of ride lengths  
max(all_trips_v2$ride_length) #longest ride 
min(all_trips_v2$ride_length) #shortest ride

summary(all_trips_v2$ride_length)

# compared the mean, median, max, and min values between members and casual riders.
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = mean)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = median)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = max)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = min)

# average ride time for each day of the week between casual and members riders.
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)

# ordered the day of week starting from Sunday and ending on Saturday. 
all_trips_v2$day_of_week <- ordered(all_trips_v2$day_of_week, levels=c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"))

aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)

# analyze ridership data by type and weekday
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>%
  group_by(member_casual, weekday) %>%
  summarise(number_of_riders = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)

# visualize the number of rides by rider type by the days of week
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_riders = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday) %>% 
  ggplot(aes(x=weekday, y=number_of_riders, fill=member_casual)) + 
  geom_col(position="dodge") +
  labs(title="Number of Riders: Member vs Casual Riders", subtitle = "in Weekday")

# visualize average duration by the days of week 
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_riders = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday) %>%
  ggplot(aes(x=weekday, y=average_duration, fill=member_casual)) + 
  geom_col(position = "dodge") +
  labs(title="Average Ride Length: Member vs Casual", subtitle = "in Weekday")

# usage by members and casual riders by the month
all_trips_v2$month <- ordered(all_trips_v2$month, levels=c("09","10","11","12","01","02","03","04","05","06","07","08"))

# visualize ridership by months. Average Ride Length: Member vs Casual Riders
all_trips_v2 %>% 
  group_by(member_casual, month) %>% 
  summarise(number_of_riders = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, month) %>% 
  ggplot(aes(x=month, y=average_duration, fill=member_casual)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = number_of_riders, angle = 90)) + 
  labs(title = "Average Ride Length: Member vs Casual Riders", subtitle = "Between 09/2021 and 08/2022") +
  facet_wrap(~member_casual)

# number of riders by month. Number of Riders: Member vs Casual Riders 
all_trips_v2 %>% 
  group_by(member_casual, month) %>% 
  summarise(number_of_riders = n()) %>% 
  arrange(member_casual, month) %>% 
  ggplot(aes(x=month, y=number_of_riders, fill=member_casual)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = number_of_riders, angle = 90)) +
  labs(title = "Number of Riders: Member vs Casual Riders", subtitle = "Between 09/2021 and 08/2022") +
  facet_wrap(~member_casual)

# create a csv file that we will visualize further
counts <- aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)
write.csv(counts, file = '~/Desktop/Google_Case_Study_1/avg_ride_length.csv')