# work prepared for Google Data Analytics Certificate - Capstone Project

# load required libraries
library(tidyverse)
library(lubridate)
library(ggplot2)
library(readr)
library(dplyr)
library(gridExtra)

# import csv data files
Trips_2019_Q1 <- read_csv("C:/Google_DA_CaseStudy/case1/Divvy_Trips_2019_Q1.csv")
Trips_2019_Q2 <- read_csv("C:/Google_DA_CaseStudy/case1/Divvy_Trips_2019_Q2.csv")
Trips_2019_Q3 <- read_csv("C:/Google_DA_CaseStudy/case1/Divvy_Trips_2019_Q3.csv")
Trips_2019_Q4 <- read_csv("C:/Google_DA_CaseStudy/case1/Divvy_Trips_2019_Q4.csv")


# check for column names to quick review
colnames(Trips_2019_Q1)
colnames(Trips_2019_Q2)
colnames(Trips_2019_Q3)
colnames(Trips_2019_Q4)


# only column names in Q2 data are differs to others. Rename them accordingly
Trips_2019_Q2 <- rename(Trips_2019_Q2,
                        trip_id = "01 - Rental Details Rental ID",                   
                        start_time = "01 - Rental Details Local Start Time",
                        end_time = "01 - Rental Details Local End Time",
                        bikeid = "01 - Rental Details Bike ID",                     
                        tripduration = "01 - Rental Details Duration In Seconds Uncapped",
                        from_station_id = "03 - Rental Start Station ID",                    
                        from_station_name = "03 - Rental Start Station Name",                  
                        to_station_id = "02 - Rental End Station ID",
                        to_station_name = "02 - Rental End Station Name",
                        usertype= "User Type",
                        gender = "Member Gender",
                        birthyear = "05 - Member Details Member Birthday Year"
                        )


# Inspect the data types for each column. If all matches, then the identical will return 'true'
df1_types <- sapply(Trips_2019_Q1, class)
df2_types <- sapply(Trips_2019_Q2, class)
df3_types <- sapply(Trips_2019_Q3, class)
df4_types <- sapply(Trips_2019_Q4, class)
identical(df1_types, df2_types)
identical(df1_types, df3_types)
identical(df1_types, df4_types)


# Combine all quarterly data into single annual 
Trips_2019 <- bind_rows(Trips_2019_Q1, Trips_2019_Q2, Trips_2019_Q3, Trips_2019_Q4)


# drop irrelevant columns then save to new data frame
Trips_2019v1 <- Trips_2019 %>% select(-c(gender, birthyear))


# add a new column to show used time in minutes
Trips_2019v1$duration_min <- as.numeric(difftime(Trips_2019v1$end_time, Trips_2019v1$start_time, units = "mins"))


# change the column name to show used time in seconds
names(Trips_2019v1)[names(Trips_2019v1) == 'tripduration'] <- "duration_sec"


# move the 'duration_min' to the 4th column next to 'duration_sec'
Trips_2019v1 <- Trips_2019v1[, c(1:3, ncol(Trips_2019v1), 4:(ncol(Trips_2019v1)-1))]


# remove the records where bikes are not actually used (time duration is less than zero)
Trips_2019v1 <- subset(Trips_2019v1, duration_min >= 0)

# Extract the date/time as seperate columns for easier analysis
# make sure that you set the locale to English if your language setting is not in English
# convert the date to the weekdays, in string
Sys.setlocale("LC_TIME", "en_US.UTF-8")
Trips_2019v1$days <- weekdays(Trips_2019v1$start_time)

# Extract year, month, start/end date, start/end time
Trips_2019v1$days <- weekdays(Trips_2019v1$start_time)
Trips_2019v1$year <- as.numeric(format(Trips_2019v1$start_time, "%Y"))
Trips_2019v1$month <- as.numeric(format(Trips_2019v1$start_time, "%m"))
Trips_2019v1$start_date <- as.numeric(format(Trips_2019v1$start_time, "%d"))
Trips_2019v1$end_date <- as.numeric(format(Trips_2019v1$end_time, "%d"))
# convert current start/end times to show time data only
Trips_2019v1$start_time <- format(Trips_2019v1$start_time, "%H:%M:%S")
Trips_2019v1$end_time <- format(Trips_2019v1$end_time, "%H:%M:%S")

# remove redundant data columns, then save to new dataframe
Trips_2019v2 <- Trips_2019v1 %>% select(-c(from_station_name, to_station_name))

# rearrange columns
Trips_2019v2 <- Trips_2019v2[, c("trip_id", 
                                 "bikeid", 
                                 "usertype", 
                                 "year",
                                 "month",
                                 "start_date",
                                 "days",
                                 "start_time",
                                 "end_date",
                                 "end_time",
                                 "from_station_id",
                                 "to_station_id",
                                 "duration_min",
                                 "duration_sec")]

# check the latest dataframe for descriptive analysis
View(Trips_2019v2)
summary(Trips_2019v2)

# check for occurences of each value in the column
table(Trips_2019v2$usertype)

# compare patterns between customers(casual) vs. Subscribers(members)
aggregate(Trips_2019v2$duration_min ~ Trips_2019v2$usertype, FUN=mean)
aggregate(Trips_2019v2$duration_min ~ Trips_2019v2$usertype, FUN=median)
aggregate(Trips_2019v2$duration_min ~ Trips_2019v2$usertype, FUN=max)
aggregate(Trips_2019v2$duration_min ~ Trips_2019v2$usertype, FUN=min)

# reorder to organize the order of days appear
Trips_2019v2$days <- ordered(Trips_2019v2$days, levels=c("Monday",
                                                         "Tuesday",
                                                         "Wednesday", 
                                                         "Thursday",
                                                         "Friday",
                                                         "Saturday", 
                                                         "Sunday"))

# compare average use time by customers vs. subscribers per day
aggregate(Trips_2019v2$duration_min ~ Trips_2019v2$usertype + Trips_2019v2$days, FUN=mean)

# ride data by users and days, no of riders and avg. ride durarion
Trips_2019v2 %>% 
  mutate(weekday = wday(start_date, label = TRUE)) %>% 
  group_by(usertype, days) %>%
  summarise(number_of_rides = n(), average_duration = mean(duration_min)) %>% 
  arrange(usertype, days)

# update the label for 'Customer' to 'Casual'
Trips_2019v2 <- Trips_2019v2 %>% mutate(usertype = recode(usertype, 
                                          "Customer" = 'Casual',
                                          "Subscriber" = 'Subscriber'))


# quick visualization
# Create a summary dataframe
summary_data<- Trips_2019v2 %>% 
  mutate(weekday = wday(start_date, label = TRUE)) %>% 
  group_by(usertype, days) %>% 
  summarise(number_of_rides = n(),
            average_duration = mean(duration_min)) %>% 
  arrange(usertype, days)

# Create the 1st plot to show avg.duration/day
p1 <- summary_data %>% 
  ggplot(aes(x = days, 
             y = average_duration, 
             fill = usertype)) +
  geom_col(position = "dodge") + 
  labs(x = 'Days', y = 'Average duration (min)', 
       title = 'Average duration by user types per day, in minutes') + 
  geom_text(aes(label = round(average_duration, 1)), 
            position = position_dodge(width = 0.9),
            vjust = -0.4,
            size = 3) +
  theme(axis.text = element_text(size=10),
        axis.title = element_text(size=10))

# create 2nd plot to show no of rides/day
p2 <- summary_data %>% 
  ggplot(aes(x = days, 
             y = number_of_rides, 
             fill = usertype)) +
  geom_col(position = "dodge") + 
  labs(x = 'Days', y = 'Number of rides', 
       title = 'Number of rides by user types per day') + 
  geom_text(aes(label = number_of_rides), 
            position = position_dodge(width = 0.9),
            vjust = -0.4,
            size = 3) +
  theme(axis.text = element_text(size=10),
        axis.title = element_text(size=10))

# Display created plots
grid.arrange(p1, p2, ncol=1)

# create the plot for use pattern of Monday
Monday_data <- Trips_2019v2 %>% 
  filter(days == "Monday") %>%
  mutate(hour = hour(hms(start_time))) %>%
  group_by(usertype, hour) %>% 
  summarise(number_of_rides = n(),
            average_duration = mean(duration_min)) %>% 
  arrange(usertype, hour)

# Create the first plot
p1 <- Monday_data %>%
  ggplot(aes(x = hour, 
             y = average_duration,
             color = usertype,
             group = usertype)) +
  geom_line() + 
  labs(x = 'Hour', y = 'Average duration (min)', 
       title = 'Average duration by user type and hour on Monday') 

# Create the second plot
p2 <- Monday_data %>%
  ggplot(aes(x = hour, 
             y = number_of_rides,
             color = usertype,
             group = usertype)) +
  geom_line() + 
  labs(x = 'Hour', y = 'Number of rides', 
       title = 'Number of rides by user type and hour on Monday') 

# Display the plots
grid.arrange(p1, p2, ncol=1)

Sunday_data <- Trips_2019v2 %>% 
  filter(days == "Sunday") %>%
  mutate(hour = hour(hms(start_time))) %>%
  group_by(usertype, hour) %>% 
  summarise(number_of_rides = n(),
            average_duration = mean(duration_min)) %>% 
  arrange(usertype, hour)

# Create the first plot
p1 <- Sunday_data %>%
  ggplot(aes(x = hour, 
             y = average_duration,
             color = usertype,
             group = usertype)) +
  geom_line() + 
  labs(x = 'Hour', y = 'Average duration (min)', 
       title = 'Average duration by user type and hour on Sunday') 

# Create the second plot
p2 <- Sunday_data %>%
  ggplot(aes(x = hour, 
             y = number_of_rides,
             color = usertype,
             group = usertype)) +
  geom_line() + 
  labs(x = 'Hour', y = 'Number of rides', 
       title = 'Number of rides by user type and hour on Sunday') 

# Display the plots
grid.arrange(p1, p2, ncol=1)