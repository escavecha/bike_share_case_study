# For Google Data Analytics Professional Certificate - Capstone Project

# load required libraries
library(tidyverse)
library(lubridate)
library(ggplot2)
library(readr)
library(dplyr)
library(gridExtra)
library(reshape2)

# NOTE: set the working directory accordingly
setwd("C:/Google_DA_CaseStudy/case1")

# import csv data files
Trips_2019_Q1 <- read_csv("Divvy_Trips_2019_Q1.csv")
Trips_2019_Q2 <- read_csv("Divvy_Trips_2019_Q2.csv")
Trips_2019_Q3 <- read_csv("Divvy_Trips_2019_Q3.csv")
Trips_2019_Q4 <- read_csv("Divvy_Trips_2019_Q4.csv")


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
Trips_2019v0 <- bind_rows(Trips_2019_Q1, Trips_2019_Q2, Trips_2019_Q3, Trips_2019_Q4)


# drop irrelevant columns then save to new data frame
Trips_2019v1 <- Trips_2019v0 %>% select(-c(gender, birthyear))


# add a new column to show used time in minutes
Trips_2019v1$duration_min <- as.numeric(difftime(Trips_2019v1$end_time, Trips_2019v1$start_time, units = "mins"))


# change the column name to show used time in seconds
names(Trips_2019v1)[names(Trips_2019v1) == 'tripduration'] <- "duration_sec"


# remove the records where bikes are not actually used (time duration is less than zero)
Trips_2019v1 <- subset(Trips_2019v1, duration_min >= 0)

# Extract the date/time as seperate columns for easier analysis
# make sure that you set the locale to English if your language setting is not in English
# convert the date to the weekdays, in string
Sys.setlocale("LC_TIME", "en_US.UTF-8")

# Extract year, month, start/end date, start/end time
Trips_2019v1$year <- as.integer(format(Trips_2019v1$start_time, "%Y"))
#Trips_2019v1$month <- as.numeric(format(Trips_2019v1$start_time, "%m"))
Trips_2019v1$month <- format(Trips_2019v1$start_time, "%b")
Trips_2019v1$days <- weekdays(Trips_2019v1$start_time)
Trips_2019v1$start_date <- as.integer(format(Trips_2019v1$start_time, "%d"))
Trips_2019v1$end_date <- as.integer(format(Trips_2019v1$end_time, "%d"))
# convert current start/end times to show time data only
Trips_2019v1$start_time <- format(Trips_2019v1$start_time, "%H:%M:%S")
Trips_2019v1$end_time <- format(Trips_2019v1$end_time, "%H:%M:%S")

# convert other columns in numeric to integer
cols_to_int <- c("trip_id",
                 "bikeid",
                  "from_station_id",
                 "to_station_id",
                 "duration_min",
                 "duration_sec")
Trips_2019v1[cols_to_int] <- lapply(Trips_2019v1[cols_to_int], as.integer)

# remove redundant data columns, then save to new dataframe
Trips_2019v2 <- Trips_2019v1 %>%
  select(-c(year,
            duration_sec))

# rearrange columns
Trips_2019v2 <- Trips_2019v2[, c("trip_id", 
                                 "bikeid", 
                                 "usertype",
                                 "month",
                                 "start_date",
                                 "days",
                                 "start_time",
                                 "end_date",
                                 "end_time",
                                 "from_station_id",
                                 "from_station_name",
                                 "to_station_id",
                                 "to_station_name",
                                 "duration_min")]

# check the latest dataframe for descriptive analysis
summary(Trips_2019v2)

# check for occurences of unique value in the column
usertype_counts <- table(Trips_2019v2$usertype)
bikeid_counts <- table(Trips_2019v2$bikeid)
from_station_id_counts <- table(Trips_2019v2$from_station_id)
to_station_id_counts <- table(Trips_2019v2$to_station_id)
sum(usertype_counts)
sum(bikeid_counts)
sum(from_station_id_counts)

# Find the number of unique values in column from_station_id
n_unique_tripid <- length(unique(Trips_2019v2$trip_id))
n_unique_usertype <- length(unique(Trips_2019v2$usertype))
n_unique_bikeid <- length(unique(Trips_2019v2$bikeid))
n_unique_from_station_id <- length(unique(Trips_2019v2$from_station_id))
n_unique_to_station_id <- length(unique(Trips_2019v2$to_station_id))
print(paste("number of total bikes:", n_unique_bikeid)) # 6017
print(paste("number of bike stations:", n_unique_from_station_id)) # 616
print(paste("total trips made in 2019:", n_unique_tripid)) # 3817991



# update the label for 'Customer' to 'Casual'
Trips_2019v2 <- Trips_2019v2 %>% mutate(usertype = recode(usertype,
                                          "Customer" = 'Casual',
                                          "Subscriber" = 'Member'))

# reorder to organize the order of days appear
Trips_2019v2$days <- ordered(Trips_2019v2$days, levels=c("Monday",
                                                         "Tuesday",
                                                         "Wednesday",
                                                         "Thursday",
                                                         "Friday",
                                                         "Saturday",
                                                         "Sunday"))

# compare patterns between customers(casual) vs. Subscribers(members)
aggregate(Trips_2019v2$duration_min ~ Trips_2019v2$usertype, FUN=mean)
aggregate(Trips_2019v2$duration_min ~ Trips_2019v2$usertype, FUN=median)
aggregate(Trips_2019v2$duration_min ~ Trips_2019v2$usertype, FUN=max)
aggregate(Trips_2019v2$duration_min ~ Trips_2019v2$usertype, FUN=min)

# Filter rows where the 'duration_min' column is less than or equal to 50000
#Trips_2019v2_filtered <- subset(Trips_2019v2, duration_min <= 50000)

# Calculate summary statistics
#ggplot(Trips_2019v2_filtered, aes(x = usertype, y = duration_min)) +
#  geom_boxplot() +
#  labs(x = "User Type", y = "Duration (min)")

# Create plot
#ggplot(Trips_2019v2, aes(x = duration_min, fill = usertype)) +
#  geom_histogram(binwidth = 30, position = "dodge") +
#  labs(x = "Duration (min)", y = "Count")



# aggregate(Trips_2019v2$duration_min ~ Trips_2019v2$usertype +
#   Trips_2019v2$days, FUN=mean)

# compare average use time by casual vs. member per day
df_mean_duration <- aggregate(Trips_2019v2$duration_min ~ Trips_2019v2$usertype +
  Trips_2019v2$days, FUN=mean)
colnames(df_mean_duration) <- c("usertype", "days", "mean_duration")

# Create line chart
p1 <- ggplot(df_mean_duration, aes(x = days,
                                   y = mean_duration,
                                   group = usertype,
                                   color = usertype)) +
  geom_line() +
  geom_point() +
  scale_x_discrete(limits = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")) +
  labs(x = "Day of Week",
       y = "Mean Duration (min)",
       title = "Mean Trip Duration by Day and User Type") +
  geom_text(aes(label = round(mean_duration, 1)), vjust = -0.5)

# Display ans save plot
print(p1)
ggsave("p1_duration_weekdays.png", plot = p1)


# Create ordered factor for months
month_levels <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
Trips_2019v2$month <- factor(Trips_2019v2$month, levels = month_levels, ordered = TRUE)

# Calculate mean duration by usertype and month
df_months <- Trips_2019v2 %>%
  group_by(usertype, month) %>%
  summarize(mean_duration = mean(duration_min))

# Create line chart
p2 <- ggplot(df_months, aes(x = month,
                            y = mean_duration,
                            color = usertype,
                            group = usertype)) +
  geom_line() +
  geom_point() +
  labs(x = "Months",
       y = "Mean Duration (min)",
       title = "Mean Trip Duration by Month and User Type") +
  geom_text(aes(label = round(mean_duration, 1)), vjust = -0.5)

print(p2)
ggsave("p2_duration_months.png", plot = p2)

Trips_2019v2_challange <- Trips_2019v2 %>%
  group_by(bikeid) %>%
  filter(!(month == "Mar" & start_date == 1 & n() > 5)) %>%
  ungroup()

# number of users per month
df_monthly_no <- Trips_2019v2 %>%
  group_by(usertype, month) %>%
  summarise(count = n())


# Plot the data as a bar chart
p2a <- ggplot(df_monthly_no, aes(x = month,
                              y = count,
                              color = usertype,
                              group = usertype)) +
  geom_line() +
  geom_point() +
  # scale_y_continuous(labels = scales::label_number()) +
  labs(title = "Number of Rides by User Type and Month, 2019",
       x = "Months",
       y = "Number of Rides") +
  geom_text(aes(label = round(count, 1)), vjust = -0.8)

print(p2a)
ggsave("p2a_ride_no_monthly.png", plot = p2a)

# df_monthly_challange <- Trips_2019v2_challange %>%
#   group_by(usertype, month) %>%
#   summarise(count = n())

df_monthly_mean <- Trips_2019v2 %>%
  group_by(usertype, month) %>%
  summarise(mean_trips = n() / n_unique_from_station_id)

# Plot the data as a line chart with points and labels
p2b <- ggplot(df_monthly_mean, aes(x = month,
                                    y = mean_trips,
                                    color = usertype,
                                    group = usertype)) +
  geom_line() +
  geom_point() +
  labs(title = "Average Number of Trips per Station by User Type and Month, 2019",
       x = "Months",
       y = "Number of Rides") +
  geom_text(aes(label = round(mean_trips, 0)), vjust = -0.8) +
  annotate("text", x = Inf, y = Inf,
           label = paste("Total number of stations:",n_unique_from_station_id),
           hjust = 1.1, vjust = 2)

print(p2b)
ggsave("p2b_monthly_rides_per_station.png", plot = p2b)


# ride data by users and days, no of riders and avg. ride durarion
df_daily_no <- Trips_2019v2 %>% # total no
  group_by(usertype, days) %>%
  summarise(count = n())



# Plot the data as a bar chart
p3 <- ggplot(df_daily_no, aes(x = days,
                              y = count,
                              color = usertype,
                              group = usertype)) +
  geom_line() +
  geom_point() +
  # scale_y_continuous(labels = scales::label_number()) +
  labs(title = "Number of Rides by User Type and Day, 2019",
       x = "Days",
       y = "Number of Rides") +
  geom_text(aes(label = round(count, 1)), vjust = -0.8)

print(p3)
ggsave("p3_ride_no_weekly.png", plot = p3)

df_daily_no_mean <- Trips_2019v2 %>% # mean no per station
  group_by(usertype, days) %>%
  summarise(mean_daily = n() / n_unique_from_station_id)

# Convert usertype and days to factor variables
df_daily_no_mean$usertype <- as.factor(df_daily_no_mean$usertype)
df_daily_no_mean$days <- as.factor(df_daily_no_mean$days)

p3a <- ggplot(df_daily_no_mean, aes(x = days,
                                    y = mean_daily,
                                    color = usertype,
                                    group = usertype)) +
  geom_line() +
  geom_point() +
  labs(title = "Average Number of Rides per Station by User Type and Day, 2019",
       x = "Days",
       y = "Number of Rides") +
  geom_text(aes(label = round(mean_daily, 0)), vjust = -0.8) +
  annotate("text", x = Inf, y = Inf,
           label = paste("Total number of stations:", n_unique_from_station_id),
           hjust = 1.1, vjust = 2)

print(p3a)
ggsave("p3_weekly_rides_per_station.png", plot = p3a)

# create the plot for use pattern of Monday
monday_data <- Trips_2019v2 %>%
  filter(days == "Monday") %>%
  mutate(hour = hour(hms(start_time))) %>%
  group_by(usertype, hour) %>% 
  summarise(number_of_rides = n(),
            average_duration = mean(duration_min)) %>% 
  arrange(usertype, hour)

# Create the first plot
p4a_mon <- monday_data %>%
  ggplot(aes(x = hour, 
             y = average_duration,
             color = usertype,
             group = usertype)) +
  geom_line() +
  geom_point() +
  labs(x = 'Hour', y = 'Average duration (min)', 
       title = 'Average duration by user type and hour on Monday')
  #geom_text(aes(label = round(average_duration, 1)), vjust = -0.1)

# Create the second plot
p4b_mon <- monday_data %>%
  ggplot(aes(x = hour, 
             y = number_of_rides,
             color = usertype,
             group = usertype)) +
  geom_line() +
  geom_point() +
  labs(x = 'Hour', y = 'Number of rides', 
       title = 'Number of rides by user type and hour on Monday') 

# Display the plots
p4 <- grid.arrange(p4a_mon, p4b_mon, ncol=1)
print(p4)
ggsave("p4_monday_hour.png", p4)

# create the plot for use pattern of Tuesday
tuesday_data <- Trips_2019v2 %>%
  filter(days == "Tuesday") %>%
  mutate(hour = hour(hms(start_time))) %>%
  group_by(usertype, hour) %>%
  summarise(number_of_rides = n(),
            average_duration = mean(duration_min)) %>%
  arrange(usertype, hour)

# Create the first plot
p5a_tue <- tuesday_data %>%
  ggplot(aes(x = hour,
             y = average_duration,
             color = usertype,
             group = usertype)) +
  geom_line() +
  geom_point() +
  labs(x = 'Hour', y = 'Average duration (min)',
       title = 'Average duration by user type and hour on Tuesday')

# Create the second plot
p5b_tue <- tuesday_data %>%
  ggplot(aes(x = hour,
             y = number_of_rides,
             color = usertype,
             group = usertype)) +
  geom_line() +
  geom_point() +
  labs(x = 'Hour', y = 'Number of rides',
       title = 'Number of rides by user type and hour on Tuesday')

# Display the plots
p5 <- grid.arrange(p5a_tue, p5b_tue, ncol=1)
print(p5)
ggsave("p5_tuesday_hour.png", p5)


# create the plot for use pattern of Wednesday
wednesday_data <- Trips_2019v2 %>%
  filter(days == "Wednesday") %>%
  mutate(hour = hour(hms(start_time))) %>%
  group_by(usertype, hour) %>%
  summarise(number_of_rides = n(),
            average_duration = mean(duration_min)) %>%
  arrange(usertype, hour)

# Create the first plot
p6a_wed <- wednesday_data %>%
  ggplot(aes(x = hour,
             y = average_duration,
             color = usertype,
             group = usertype)) +
  geom_line() +
  geom_point() +
  labs(x = 'Hour', y = 'Average duration (min)',
       title = 'Average duration by user type and hour on Wednesday')

# Create the second plot
p6b_wed <- wednesday_data %>%
  ggplot(aes(x = hour,
             y = number_of_rides,
             color = usertype,
             group = usertype)) +
  geom_line() +
  geom_point() +
  labs(x = 'Hour', y = 'Number of rides',
       title = 'Number of rides by user type and hour on Wednesday')

# Display the plots
p6 <- grid.arrange(p6a_wed, p6b_wed, ncol=1)
print(p6)
ggsave("p6_wednesday_hour.png", p6)


# create the plot for use pattern of Thursday
thursday_data <- Trips_2019v2 %>%
  filter(days == "Thursday") %>%
  mutate(hour = hour(hms(start_time))) %>%
  group_by(usertype, hour) %>%
  summarise(number_of_rides = n(),
            average_duration = mean(duration_min)) %>%
  arrange(usertype, hour)

# Create the first plot
p7a_thur <- thursday_data %>%
  ggplot(aes(x = hour,
             y = average_duration,
             color = usertype,
             group = usertype)) +
  geom_line() +
  geom_point() +
  labs(x = 'Hour', y = 'Average duration (min)',
       title = 'Average duration by user type and hour on Thursday')

# Create the second plot
p7b_thur <- thursday_data %>%
  ggplot(aes(x = hour,
             y = number_of_rides,
             color = usertype,
             group = usertype)) +
  geom_line() +
  geom_point() +
  labs(x = 'Hour', y = 'Number of rides',
       title = 'Number of rides by user type and hour on Thursday')

# Display the plots
p7 <- grid.arrange(p7a_thur, p7b_thur, ncol=1)
print(p7)
ggsave("p7_thursday_hour.png", p7)


# create the plot for use pattern of Friday
friday_data <- Trips_2019v2 %>%
  filter(days == "Friday") %>%
  mutate(hour = hour(hms(start_time))) %>%
  group_by(usertype, hour) %>%
  summarise(number_of_rides = n(),
            average_duration = mean(duration_min)) %>%
  arrange(usertype, hour)

# Create the first plot
p8a_fri <- friday_data %>%
  ggplot(aes(x = hour,
             y = average_duration,
             color = usertype,
             group = usertype)) +
  geom_line() +
  geom_point() +
  labs(x = 'Hour', y = 'Average duration (min)',
       title = 'Average duration by user type and hour on Friday')

# Create the second plot
p8b_fri <- friday_data %>%
  ggplot(aes(x = hour,
             y = number_of_rides,
             color = usertype,
             group = usertype)) +
  geom_line() +
  geom_point() +
  labs(x = 'Hour', y = 'Number of rides',
       title = 'Number of rides by user type and hour on Friday')

# Display the plots
p8 <- grid.arrange(p8a_fri, p8b_fri, ncol=1)
print(p8)
ggsave("p8_friday_hour.png", p8)


# create the plot for use pattern of Saturday
saturday_data <- Trips_2019v2 %>%
  filter(days == "Saturday") %>%
  mutate(hour = hour(hms(start_time))) %>%
  group_by(usertype, hour) %>%
  summarise(number_of_rides = n(),
            average_duration = mean(duration_min)) %>%
  arrange(usertype, hour)

# Create the first plot
p9a_sat <- saturday_data %>%
  ggplot(aes(x = hour,
             y = average_duration,
             color = usertype,
             group = usertype)) +
  geom_line() +
  geom_point() +
  labs(x = 'Hour', y = 'Average duration (min)',
       title = 'Average duration by user type and hour on Saturday')

# Create the second plot
p9b_sat <- friday_data %>%
  ggplot(aes(x = hour,
             y = number_of_rides,
             color = usertype,
             group = usertype)) +
  geom_line() +
  geom_point() +
  labs(x = 'Hour', y = 'Number of rides',
       title = 'Number of rides by user type and hour on Saturday')

# Display the plots
p9 <- grid.arrange(p9a_sat, p9b_sat, ncol=1)
print(p9)
ggsave("p9_saturday_hour.png", p9)


# create the plot for use pattern of Sunday
sunday_data <- Trips_2019v2 %>%
  filter(days == "Sunday") %>%
  mutate(hour = hour(hms(start_time))) %>%
  group_by(usertype, hour) %>%
  summarise(number_of_rides = n(),
            average_duration = mean(duration_min)) %>%
  arrange(usertype, hour)

# Create the first plot
p10a_sun <- sunday_data %>%
  ggplot(aes(x = hour,
             y = average_duration,
             color = usertype,
             group = usertype)) +
  geom_line() +
  geom_point() +
  labs(x = 'Hour', y = 'Average duration (min)',
       title = 'Average duration by user type and hour on Sunday')

# Create the second plot
p10b_sun <- sunday_data %>%
  ggplot(aes(x = hour,
             y = number_of_rides,
             color = usertype,
             group = usertype)) +
  geom_line() +
  geom_point() +
  labs(x = 'Hour', y = 'Number of rides',
       title = 'Number of rides by user type and hour on Sunday')

# Display the plots
p10 <- grid.arrange(p10a_sun, p10b_sun, ncol=1)
print(p10)
ggsave("p10_sunday_hour.png", p10)

# TODO weekends hourly

# gather and plot for weekdays data
weekday_data <- Trips_2019v2 %>%
  filter(days %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")) %>%
  mutate(hour = hour(hms(start_time))) %>%
  group_by(usertype, hour) %>%
  summarise(number_of_rides = n(),
            average_duration = mean(duration_min)) %>%
  arrange(usertype, hour)

# Create the first plot
p11a_weekday <- weekday_data %>%
  ggplot(aes(x = hour,
             y = average_duration,
             color = usertype,
             group = usertype)) +
  geom_line() +
  geom_point() +
  labs(x = 'Hour', y = 'Average duration (min)',
       title = 'Average duration by user type and hour on Weekdays')

# Create the second plot
p11b_weekday <- weekday_data %>%
  ggplot(aes(x = hour,
             y = number_of_rides,
             color = usertype,
             group = usertype)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  labs(x = 'Hour', y = 'Number of rides',
       title = 'Number of rides by user type and hour on Weekdays')

# Display the plots
p11 <- grid.arrange(p11a_weekday, p11b_weekday, ncol=1)
print(p11)
ggsave("p11_weekday_hour.png", p11)

# TODO fix p11a to average per station
# for average week data
weekday_data_mean <- Trips_2019v2 %>%
  filter(days %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")) %>%
  mutate(hour = hour(hms(start_time))) %>%
  group_by(usertype, hour) %>%
  summarise(number_of_rides = n() / n_unique_from_station_id,
            average_duration = mean(duration_min)) %>%
  arrange(usertype, hour)

# Create the first plot
p11a_weekday_mean <- weekday_data_mean %>%
  ggplot(aes(x = hour,
             y = average_duration,
             color = usertype,
             group = usertype)) +
  geom_line() +
  geom_point() +
  labs(x = 'Hour', y = 'Average duration (min)',
       title = 'Average duration by user type and hour on Weekdays')

# Create the second plot
p11b_weekday_mean <- weekday_data_mean %>%
  ggplot(aes(x = hour,
             y = number_of_rides,
             color = usertype,
             group = usertype)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  labs(x = 'Hour', y = 'Number of rides',
       title = 'Number of rides by user type per station, hourly on Weekdays') +
  annotate("text", x = 0, y = Inf,
           label = paste("Total number of stations:", n_unique_from_station_id),
           hjust = 0, vjust = 0.9)

p11_weekdays_mean <- grid.arrange(p11a_weekday_mean, p11b_weekday_mean, ncol=1)
print(p11_weekdays_mean)
ggsave("p11_weekdays_average_rides_per_station.png", plot = p11_weekdays_mean)


# TODO fix p12a to average per station
# gather and plot for weekens data
weekend_data <- Trips_2019v2 %>%
  filter(days %in% c("Saturday", "Sunday")) %>%
  mutate(hour = hour(hms(start_time))) %>%
  group_by(usertype, hour) %>%
  summarise(number_of_rides = n(),
            average_duration = mean(duration_min)) %>%
  arrange(usertype, hour)

# Create the first plot
p12a_weekend <- weekend_data %>%
  ggplot(aes(x = hour,
             y = average_duration,
             color = usertype,
             group = usertype)) +
  geom_line() +
  geom_point() +
  labs(x = 'Hour', y = 'Average duration (min)',
       title = 'Average duration by user type and hour on Weekends')

# Create the second plot
p12b_weekend <- weekend_data %>%
  ggplot(aes(x = hour,
             y = number_of_rides,
             color = usertype,
             group = usertype)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  labs(x = 'Hour', y = 'Number of rides',
       title = 'Number of rides by user type and hour on Weekends')

# Display the plots
p12 <- grid.arrange(p12a_weekend, p12b_weekend, ncol=1)
print(p12)
ggsave("p12_weekend_hour.png", p12)


# for average weekend data
weekend_data_mean <- Trips_2019v2 %>%
  filter(days %in% c("Saturday", "Sunday")) %>%
  mutate(hour = hour(hms(start_time))) %>%
  group_by(usertype, hour) %>%
  summarise(number_of_rides = n() / n_unique_from_station_id,
            average_duration = mean(duration_min)) %>%
  arrange(usertype, hour)

# Create the first plot
p12a_weekend_mean <- weekend_data_mean %>%
  ggplot(aes(x = hour,
             y = average_duration,
             color = usertype,
             group = usertype)) +
  geom_line() +
  geom_point() +
  labs(x = 'Hour', y = 'Average duration (min)',
       title = 'Average duration by user type and hour on Weekends')

# Create the second plot
p12b_weekend_mean <- weekend_data_mean %>%
  ggplot(aes(x = hour,
             y = number_of_rides,
             color = usertype,
             group = usertype)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  labs(x = 'Hour', y = 'Number of rides',
       title = 'Number of rides by user type per station, hourly on Weekends') +
  annotate("text", x = 0, y = Inf,
           label = paste("Total number of stations:", n_unique_from_station_id),
           hjust = 0, vjust = 0.9)

p12_weekends_mean <- grid.arrange(p12a_weekend_mean, p12b_weekend_mean, ncol=1)
print(p12_weekends_mean)
ggsave("p12_weekends_average_rides_per_station.png", plot = p12_weekends_mean)



# Load required libraries

install.packages("ggmap")
library(ggmap)
#NOTE https://developers.google.com/maps/documentation/javascript/get-api-key#console



# Set your Google API key
# NOTE https://support.google.com/googleapi/answer/6158862?hl=en
api_key <- "AIzaSyCJqFR_aM_A_xQ0VFqoiNFpDVejL7ADTQ0"


register_google(key = api_key)

Trips_2019v3 <- Trips_2019v2 %>%
  mutate(
    from_station_name = paste(from_station_name, "chicago, il, usa"),
    to_station_name = paste(to_station_name, "Chicago, il, usa")
  )

# Get unique station names
stations <- unique(c(Trips_2019v3$from_station_name, Trips_2019v3$to_station_name))

# Geocode station addresses
geocoded_stations <- geocode(stations, output = "latlona", source = "google")

# Add station names to geocoded data
geocoded_stations$station_name <- stations

# Merge geocoded data with trips data
trips_geo <- merge(Trips_2019v3, geocoded_stations, by.x = "from_station_name", by.y = "station_name")
trips_geo <- merge(trips_geo, geocoded_stations, by.x = "to_station_name", by.y = "station_name", suffixes = c(".from", ".to"))

# Remove rows with missing values
trips_geo <- na.omit(trips_geo)
geocoded_stations <- na.omit(geocoded_stations)

# Define the bounding box of the Chicago area
chicago_bbox <- c(left = -87.82, bottom = 41.65, right = -87.5, top = 42.1)

# Filter trips_geo to only include rows within the Chicago bounding box
trips_geo <- trips_geo %>%
  filter(
    between(lon.from, chicago_bbox["left"], chicago_bbox["right"]) &
    between(lat.from, chicago_bbox["bottom"], chicago_bbox["top"]) &
    between(lon.to, chicago_bbox["left"], chicago_bbox["right"]) &
    between(lat.to, chicago_bbox["bottom"], chicago_bbox["top"])
  )


# p13 <- ggmap(get_map(location = c(lon = mean(c(chicago_bbox["left"], chicago_bbox["right"])),
#                                   lat = mean(c(chicago_bbox["bottom"], chicago_bbox["top"]))),
#                      zoom = 11)) +
#   geom_point(data = trips_geo, aes(x = lon.from, y = lat.from, color = usertype), alpha = 0.5) +
#   geom_point(data = trips_geo, aes(x = lon.to, y = lat.to, color = usertype), alpha = 0.5) +
#   scale_color_manual(values = c("red", "blue"), name = "User Type") +
#   theme(legend.position = "bottom")
#
# print(p13)
# ggsave("p13_map.png", p13)

# Create a map plot of geolocations colored by user type and geolocation
p13 <- ggmap(get_map(location = c(lon = mean(c(chicago_bbox["left"], chicago_bbox["right"])),
                                  lat = mean(c(chicago_bbox["bottom"], chicago_bbox["top"]))),
                     zoom = 11)) +
  geom_point(data = trips_geo, aes(x = lon.from, y = lat.from, color = usertype),
                                   alpha = 0.5, size = 3) +
  geom_point(data = trips_geo, aes(x = lon.to, y = lat.to, color = usertype,
                                   alpha = 0.5), size = 3) +
  scale_color_manual(values = c("red", "blue"), name = "User Type") +
  # scale_alpha_continuous(range = c(0.1, 1), name = "User Type Count") +
  facet_wrap(~usertype) +
  theme(legend.position = "bottom")

print(p13)
ggsave("p13_map.png", p13)


# # Count the number of occurrences of each from_station_id value
# from_station_id_counts <- table(Trips_2019v3$from_station_id)
#
# # Convert the from_station_id_counts table to a data frame
# from_station_id_counts_df <- as.data.frame(from_station_id_counts)
#
# # Set the column names of the data frame
# names(from_station_id_counts_df) <- c("from_station_id", "count")
#
# # Filter the rows of the from_station_id_counts_df data frame
# most_frequent_station_df <- from_station_id_counts_df %>% filter(count > 8921) %>% arrange(desc(count))
# least_frequent_station_df <- from_station_id_counts_df %>% filter(count < 674) %>% arrange(desc(count))


# Count the number of occurrences of each from_station_id value and usertype
from_station_id_usertype_counts <- table(Trips_2019v3$from_station_id, Trips_2019v3$usertype)

# Convert the from_station_id_usertype_counts table to a data frame
from_station_id_usertype_counts_df <- as.data.frame(from_station_id_usertype_counts)

# Set the column names of the data frame
names(from_station_id_usertype_counts_df) <- c("from_station_id", "usertype", "count")

# Filter the rows of the from_station_id_usertype_counts_df data frame
most_frequent_station_df <- from_station_id_usertype_counts_df %>% filter(count > 8921) %>% arrange(desc(count))
least_frequent_station_df <- from_station_id_usertype_counts_df %>% filter(count < 674) %>% arrange(desc(count))

# Create a list of colors to use for each usertype
usertype_colors <- c("blue", "red")

# Calculate the mean value of count
mean_count <- mean(from_station_id_usertype_counts_df$count)

# Plot a histogram of the number of occurrences of each from_station_id value per usertype
p14 <- ggplot(#least_frequent_station_df,
              from_station_id_usertype_counts_df,
              aes(x = from_station_id, y = count, fill = usertype)) +
  geom_bar(stat = "identity") +
  xlab("From Station ID") +
  ylab("Count") +
  ggtitle("Histogram of From Station ID per Usertype") +
  scale_fill_manual(values = usertype_colors) +
  facet_wrap(~usertype) +
  geom_hline(yintercept = mean_count, linetype = "dashed", color = "black")

grid.arrange(p14)

print(p14)








