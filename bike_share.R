# For Google Data Analytics Professional Certificate - Capstone Project


# Data Pre-processing
# Step 1. Import libraries
######################################################
setwd("C:/Google_DA_CaseStudy") # use your working directory here
library(tidyverse)
library(lubridate)
library(ggplot2)
library(readr)
library(dplyr)
library(gridExtra)


# Step 2. Import data from downloaded files
######################################################
Trips_2019_Q1 <- read_csv("Divvy_Trips_2019_Q1.csv")
Trips_2019_Q2 <- read_csv("Divvy_Trips_2019_Q2.csv")
Trips_2019_Q3 <- read_csv("Divvy_Trips_2019_Q3.csv")
Trips_2019_Q4 <- read_csv("Divvy_Trips_2019_Q4.csv")


# Step 3. Check for column names to review
######################################################
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
                        birthyear = "05 - Member Details Member Birthday Year")

# Inspect the data types for each column. If all match, then the identical functions will return ‘TRUE.’
df1_types <- sapply(Trips_2019_Q1, class)
df2_types <- sapply(Trips_2019_Q2, class)
df3_types <- sapply(Trips_2019_Q3, class)
df4_types <- sapply(Trips_2019_Q4, class)
identical(df1_types, df2_types)
identical(df1_types, df3_types)
identical(df1_types, df4_types)

# Combine all quarterly data into a single dataframe to show annual data. Keep this original dataframe for backup.
Trips_2019v0 <- bind_rows(Trips_2019_Q1, Trips_2019_Q2, Trips_2019_Q3, Trips_2019_Q4)


# Step 4. Clean up data to prepare analysis
######################################################
# drop irrelevant columns then save to new data frame
Trips_2019v1 <- Trips_2019v0 %>% select(-c(gender, birthyear))


# add a new column to show used time in minutes
Trips_2019v1$duration_min <- as.numeric(difftime(Trips_2019v1$end_time, 
                                                 time2 = Trips_2019v1$start_time, 
                                                 units = "mins"))

# change the column name to show used time in seconds
names(Trips_2019v1)[names(Trips_2019v1) == 'tripduration'] <- "duration_sec"

# Check the dataframe Trip_2019v1 using the summary function.
summary(Trips_2019v1)

# remove the records where bikes are not actually used (time duration is less than zero)
Trips_2019v1 <- subset(Trips_2019v1, duration_min >= 0)

# I will assume that the data with duration_min is more than 24 hours (=1,440 minutes) is abnormal instances
count <- sum(Trips_2019v1$duration_min > 1440)
print(count)

# Luckily, those ‘unusual’ rides take a negligible portion of the data set. Delete those rows, likewise.
Trips_2019v1 <- subset(Trips_2019v1, duration_min <= 1440)

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

# convert other numeric columns to integers.
cols_to_int <- c("trip_id",
                 "bikeid",
                  "from_station_id",
                 "to_station_id",
                 "duration_min",
                 "duration_sec")
Trips_2019v1[cols_to_int] <- lapply(Trips_2019v1[cols_to_int], as.integer)

# remove redundant data columns, then save to new dataframe
Trips_2019v2 <- subset(Trips_2019v1, select = -c(duration_sec))

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

# check what kind of variables exist in the usertype.
table(Trips_2019v2$usertype)

# Update the terms to align with the words in the scenario (casual, member).
Trips_2019v2 <- Trips_2019v2 %>% mutate(usertype = recode(usertype,
                                                          "Customer" = 'Casual',
                                                          "Subscriber" = 'Member'))

names(Trips_2019v2)[names(Trips_2019v2) == 'usertype'] <- "membership"

# Reorder the order of days to appear.
Trips_2019v2$days <- ordered(Trips_2019v2$days, levels=c("Monday",
                                                         "Tuesday",
                                                         "Wednesday",
                                                         "Thursday",
                                                         "Friday",
                                                         "Saturday",
                                                         "Sunday"))

# Find the number of unique values in column from_station_id
n_unique_tripid <- length(unique(Trips_2019v2$trip_id))
n_unique_usertype <- length(unique(Trips_2019v2$usertype))
n_unique_bikeid <- length(unique(Trips_2019v2$bikeid))
n_unique_from_station_id <- length(unique(Trips_2019v2$from_station_id))
n_unique_to_station_id <- length(unique(Trips_2019v2$to_station_id))
print(paste("number of total bikes:", n_unique_bikeid)) # 6017
print(paste("number of bike stations:", n_unique_from_station_id)) # 616
print(paste("total trips made in 2019:", n_unique_tripid)) # 3817991


# Data Analysis
# Step 1. Check for statistics
#####################################################
# analyze the percentage of rides made by membership.
membership_table <- table(Trips_2019v2$membership) # Calculate the table of membership
membership_fraction <- prop.table(membership_table) # Calculate the fraction in membership
result <- cbind(membership_table, membership_fraction) # Combine the tables
print(result)

# Calculate the mean, median, max, and min of duration_min 
mean_duration <- aggregate(Trips_2019v2$duration_min ~ Trips_2019v2$usertype, FUN=mean)
median_duration <- aggregate(Trips_2019v2$duration_min ~ Trips_2019v2$usertype, FUN=median)
max_duration <- aggregate(Trips_2019v2$duration_min ~ Trips_2019v2$usertype, FUN=max)
min_duration <- aggregate(Trips_2019v2$duration_min ~ Trips_2019v2$usertype, FUN=min)

# Combine the results into a single data frame
result <- data.frame(Usertype = mean_duration[, 1],
                     Mean = mean_duration[, 2],
                     Median = median_duration[, 2],
                     Max = max_duration[, 2],
                     Min = min_duration[, 2])

# Print the result
print(result)


# Step 2. Monthly patterns
# Average Ride Duration by membership, Monthly (2019)
#####################################################
# Create ordered factor for months
month_levels <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
Trips_2019v2$month <- factor(Trips_2019v2$month, levels = month_levels, ordered = TRUE)

# Calculate mean duration by membership and month
df_months <- Trips_2019v2 %>%
  group_by(membership, month) %>%
  summarize(mean_duration = mean(duration_min), .groups = "drop")

# Create line chart
p1_monthly_mean_duration <- ggplot(df_months, 
                                   aes(x = month, y = mean_duration, color = membership, group = membership)) +
  geom_line() +
  labs(x = "Months", y = "Avg. Duration (min)",
       title = "Fig 1. Average Ride Duration by Membership, Monthly (2019)") +
  geom_text(aes(label = round(mean_duration, 1)), vjust = -0.5)

print(p1_monthly_mean_duration)
ggsave("p1_monthly_mean_duration.png", plot = p1_monthly_mean_duration, 
       width = 7, height = 4.4 )


# Average Number of Rides per Station by Membership, Monthly (2019)
######################################################
df_monthly_mean <- Trips_2019v2 %>%
  group_by(membership, month) %>%
  summarise(mean_trips = n() / n_unique_from_station_id, .groups = "drop")

# Plot the data as a line chart with points and labels
p2_monthly_no_station <- ggplot(df_monthly_mean, aes(x = month, y = mean_trips,
                                                     color = membership, group = membership)) +
  geom_line() +
  geom_point() +
  labs(title = "Fig 2. Average Number of Rides per Station by Membership, Monthly (2019)",
       x = "Months", y = "Avg. Number of Rides / Station") +
  geom_text(aes(label = round(mean_trips, 0)), vjust = -0.8) +
  annotate("text", x = 6, y = Inf,
           label = paste("Total number of stations:",n_unique_from_station_id),
           hjust = 1.1, vjust = 2)

print(p2_monthly_no_station)
ggsave("p2_monthly_no_station.png", plot = p2_monthly_no_station, 
       width = 7.3, height = 4.6)


# Step 3. Daily patterns
# Average Ride Duration by membership, Daily (2019)
######################################################
df_daily_mean_duration <- aggregate(Trips_2019v2$duration_min ~ Trips_2019v2$membership +
                                      Trips_2019v2$days, FUN=mean)
colnames(df_daily_mean_duration) <- c("membership", "days", "mean_duration")

p3_daily_mean_duration <- ggplot(df_daily_mean_duration, aes(x = days,
                                                             y = mean_duration, group = membership, 
                                                             color = membership)) +
  geom_line() +
  geom_point() +
  scale_x_discrete(limits = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")) +
  labs(x = "Days", y = "Avg. Duration (min)",
       title = "Fig 3. Average Ride Duration by Membership, daily (2019)") +
  geom_text(aes(label = round(mean_duration, 1)), vjust = -0.5)

print(p3_daily_mean_duration)
ggsave("p3_daily_mean_duration.png", plot = p3_daily_mean_duration, 
       width = 7.3, height = 4.6)


# Average Number of Rides per Station by membership, Daily (2019)
######################################################
df_daily_no_mean <- Trips_2019v2 %>% # mean no per station
  group_by(membership, days) %>%
  summarise(mean_daily = n() / n_unique_from_station_id, .groups = "drop")

# Convert membership and days to factor variables
df_daily_no_mean$membership <- as.factor(df_daily_no_mean$membership)
df_daily_no_mean$days <- as.factor(df_daily_no_mean$days)

p4_daily_no_mean <- ggplot(df_daily_no_mean, aes(x = days, y = mean_daily,
                                                 color = membership, group = membership)) +
  geom_line() +
  geom_point() +
  labs(title = "Fig 4. Average Number of Rides per Station by membership, Daily (2019)",
       x = "Days",
       y = "Avg. No. of Rides / Station") +
  geom_text(aes(label = round(mean_daily, 0)), vjust = -0.8) +
  annotate("text", x = Inf, y = Inf,
           label = paste("Total number of stations:", n_unique_from_station_id),
           hjust = 1.1, vjust = 2)

print(p4_daily_no_mean)
ggsave("p4_daily_no_mean.png", plot = p4_daily_no_mean, width = 7.3, height = 4.6)


# Step 4. Hourly patterns
# Avg. duration & No of Rides / station by Membership on Weekdays, hourly
######################################################
weekday_data_mean <- Trips_2019v2 %>%
  filter(days %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")) %>%
  mutate(hour = hour(hms(start_time))) %>%
  group_by(membership, hour) %>%
  summarise(number_of_rides = n() / n_unique_from_station_id,
            average_duration = mean(duration_min), .groups = "drop") %>%
  arrange(membership, hour)

# Create the first plot
p5a_weekday_mean <- weekday_data_mean %>%
  ggplot(aes(x = hour, y = average_duration, color = membership, group = membership)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks=seq(0, 24, 2)) +
  labs(x = 'Hour', y = 'Avg. Duration (min)',
       title = 'Fig 5a. Average Duration by Membership on Weekdays, Hourly (2019)')

# Create the second plot
p5b_weekday_mean <- weekday_data_mean %>%
  ggplot(aes(x = hour, y = number_of_rides, color = membership, group = membership)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks=seq(0, 24, 2)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  labs(x = 'Hour', y = 'No. of rides',
       title = 'Fig 5b. Number of Rides per Station by Membership on Weekdays, Hourly (2019)') +
  annotate("text", x = 0, y = Inf,
           label = paste("Total number of stations:", n_unique_from_station_id),
           hjust = 0, vjust = 0.9)

p5_weekdays_mean <- grid.arrange(p5a_weekday_mean, p5b_weekday_mean, ncol=1)
print(p5_weekdays_mean)
ggsave("p5_weekdays_mean.png", plot = p5_weekdays_mean, width = 7.3, height = 4.6)


# Avg. duration & No of Rides / station by Membership on Weekends, hourly
######################################################
weekend_data_mean <- Trips_2019v2 %>%
  filter(days %in% c("Saturday", "Sunday")) %>%
  mutate(hour = hour(hms(start_time))) %>%
  group_by(membership, hour) %>%
  summarise(number_of_rides = n() / n_unique_from_station_id,
            average_duration = mean(duration_min), .groups = "drop") %>%
  arrange(membership, hour)

# Create the first plot
p6a_weekend_mean <- weekend_data_mean %>%
  ggplot(aes(x = hour, y = average_duration, color = membership, group = membership)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks=seq(0, 24, 2)) +
  labs(x = 'Hour', y = 'Avg. Duration (min)',
       title = 'Fig 6a. Average Duration by Membership on Weekend, Hourly (2019)')

# Create the second plot
p6b_weekend_mean <- weekend_data_mean %>%
  ggplot(aes(x = hour, y = number_of_rides, color = membership, group = membership)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks=seq(0, 24, 2)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  labs(x = 'Hour', y = 'No. of Rides',
       title = 'Fig 6b. Number of Rides per Station by Membership on Weekend, Hourly (2019)') +
  annotate("text", x = 0, y = Inf,
           label = paste("Total number of stations:", n_unique_from_station_id),
           hjust = 0, vjust = 0.9)

p6_weekend_mean <- grid.arrange(p6a_weekend_mean, p6b_weekend_mean, ncol=1)
print(p6_weekend_mean)
ggsave("p6_weekend_mean.png", plot = p6_weekend_mean, width = 7.3, height = 4.6)








