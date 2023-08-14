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

# check for occurences of each value in the column
table(Trips_2019v2$usertype)

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
p1 <- ggplot(df_mean_duration, aes(x = days, y = mean_duration, group = usertype, color = usertype)) +
  geom_line() +
  geom_point() +
  scale_x_discrete(limits = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")) +
  labs(x = "Day of Week", y = "Mean Duration (min)", title = "Mean Trip Duration by Day and User Type")

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
p2 <- ggplot(df_months, aes(x = month, y = mean_duration, color = usertype, group = usertype)) +
  geom_line() +
  labs(x = "Month", y = "Mean Duration (min)") +
  theme_bw()

ggsave("duration_months.png", plot = p2)



# ride data by users and days, no of riders and avg. ride durarion
# Convert the 'days' variable to a factor with ordered levels
df_daily_no$days <- factor(df_daily_no$days,
                           levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"),
                           ordered = TRUE)

# Plot the data as a bar chart
p3 <- ggplot(df_daily_no, aes(x = days,
                              y = number_of_rides,
                              fill = usertype)) +
  geom_col(position = "dodge") +
  scale_y_continuous(labels = scales::label_number()) +
  labs(title = "Number of Rides by User Type and Day",
       x = "Day",
       y = "Number of Rides")

ggsave("ride_no_weekly.png", plot = p3)

# quick visualization
# Create a summary dataframe
# summary_data<- Trips_2019v2 %>%
#   mutate(weekday = wday(start_date, label = TRUE)) %>%
#   group_by(usertype, days) %>%
#   summarise(number_of_rides = n(),
#             average_duration = mean(duration_min)) %>%
#   arrange(usertype, days)

# Create the 1st plot to show avg.duration/day
# p1 <- summary_data %>%
#   ggplot(aes(x = days,
#              y = average_duration,
#              fill = usertype)) +
#   geom_col(position = "dodge") +
#   labs(x = 'Days', y = 'Average duration (min)',
#        title = 'Average duration by user types per day, in minutes') +
#   geom_text(aes(label = round(average_duration, 1)),
#             position = position_dodge(width = 0.9),
#             vjust = -0.4,
#             size = 3) +
#   theme(axis.text = element_text(size=10),
#         axis.title = element_text(size=10))
#
# # create 2nd plot to show no of rides/day
# p2 <- summary_data %>%
#   ggplot(aes(x = days,
#              y = number_of_rides,
#              fill = usertype)) +
#   geom_col(position = "dodge") +
#   labs(x = 'Days', y = 'Number of rides',
#        title = 'Number of rides by user types per day') +
#   geom_text(aes(label = number_of_rides),
#             position = position_dodge(width = 0.9),
#             vjust = -0.4,
#             size = 3) +
#   theme(axis.text = element_text(size=10),
#         axis.title = element_text(size=10))
#
# # Display created plots
# grid.arrange(p1, p2, ncol=1)


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
  labs(x = 'Hour', y = 'Average duration (min)', 
       title = 'Average duration by user type and hour on Monday') 

# Create the second plot
p4b_mon <- monday_data %>%
  ggplot(aes(x = hour, 
             y = number_of_rides,
             color = usertype,
             group = usertype)) +
  geom_line() + 
  labs(x = 'Hour', y = 'Number of rides', 
       title = 'Number of rides by user type and hour on Monday') 

# Display the plots
p4 <- grid.arrange(p4a_mon, p4b_mon, ncol=1)
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
  labs(x = 'Hour', y = 'Average duration (min)',
       title = 'Average duration by user type and hour on Tuesday')

# Create the second plot
p5b_tue <- tuesday_data %>%
  ggplot(aes(x = hour,
             y = number_of_rides,
             color = usertype,
             group = usertype)) +
  geom_line() +
  labs(x = 'Hour', y = 'Number of rides',
       title = 'Number of rides by user type and hour on Tuesday')

# Display the plots
p5 <- grid.arrange(p5a_tue, p5b_tue, ncol=1)
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
  labs(x = 'Hour', y = 'Average duration (min)',
       title = 'Average duration by user type and hour on Wednesday')

# Create the second plot
p6b_wed <- wednesday_data %>%
  ggplot(aes(x = hour,
             y = number_of_rides,
             color = usertype,
             group = usertype)) +
  geom_line() +
  labs(x = 'Hour', y = 'Number of rides',
       title = 'Number of rides by user type and hour on Wednesday')

# Display the plots
p6 <- grid.arrange(p6a_wed, p6b_wed, ncol=1)
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
  labs(x = 'Hour', y = 'Average duration (min)',
       title = 'Average duration by user type and hour on Thursday')

# Create the second plot
p7b_thur <- thursday_data %>%
  ggplot(aes(x = hour,
             y = number_of_rides,
             color = usertype,
             group = usertype)) +
  geom_line() +
  labs(x = 'Hour', y = 'Number of rides',
       title = 'Number of rides by user type and hour on Thursday')

# Display the plots
p7 <- grid.arrange(p7a_thur, p7b_thur, ncol=1)
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
  labs(x = 'Hour', y = 'Average duration (min)',
       title = 'Average duration by user type and hour on Friday')

# Create the second plot
p8b_fri <- friday_data %>%
  ggplot(aes(x = hour,
             y = number_of_rides,
             color = usertype,
             group = usertype)) +
  geom_line() +
  labs(x = 'Hour', y = 'Number of rides',
       title = 'Number of rides by user type and hour on Friday')

# Display the plots
p8 <- grid.arrange(p8a_fri, p8b_fri, ncol=1)
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
  labs(x = 'Hour', y = 'Average duration (min)',
       title = 'Average duration by user type and hour on Saturday')

# Create the second plot
p9b_sat <- friday_data %>%
  ggplot(aes(x = hour,
             y = number_of_rides,
             color = usertype,
             group = usertype)) +
  geom_line() +
  labs(x = 'Hour', y = 'Number of rides',
       title = 'Number of rides by user type and hour on Saturday')

# Display the plots
p9 <- grid.arrange(p9a_sat, p9b_sat, ncol=1)
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
  labs(x = 'Hour', y = 'Average duration (min)',
       title = 'Average duration by user type and hour on Sunday')

# Create the second plot
p10b_sun <- friday_data %>%
  ggplot(aes(x = hour,
             y = number_of_rides,
             color = usertype,
             group = usertype)) +
  geom_line() +
  labs(x = 'Hour', y = 'Number of rides',
       title = 'Number of rides by user type and hour on Sunday')

# Display the plots
p10 <- grid.arrange(p10a_sun, p10b_sun, ncol=1)
ggsave("p10_sunday_hour.png", p10)

# TODO weekdays hourly
# TODO weekends hourly

install.packages("tidygeocoder") # TODO check
library(tidygeocoder)
install.packages("leaflet")
library(leaflet)

# Geocode the street addresses
df <- data.frame(address = c("Eiffel Tower, Paris, France", "Arc de Triomphe, Paris, France"),
                 usertype = c("Subscriber", "Customer"))
df_geo <- geocode(df, address = address)

# Plot the points on a map
m <- leaflet() %>% addTiles() %>% addCircleMarkers(data = df_geo,
                   lat = ~lat,
                   lng = ~lon, color = "red")
m
