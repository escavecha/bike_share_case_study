## Background

The content presented here results from the capstone project completed for the *Google Data Analytics Professional
Certificate.* ([link](https://www.coursera.org/professional-certificates/google-data-analytics)) It is a fictional case
study packet that draws inspiration from the *Divvy* case study titled "'Sophisticated, Clear, and Polished': Divvy and
Data Visualization," authored by Kevin Hartman. ([link](https://artscience.blog/home/divvy-dataviz-case-study))

The following section highlights the bike-sharing user pattern variations observed in a particular data set.
Additionally, it offers insights and recommendations based on the analysis results.

> Note: *Divvy* is a bicycle-sharing system operating in Chicago and Evanston. It has a large fleet of bikes locked into
> a network of docking stations throughout the area. The bikes can be unlocked from one station and returned to any
> other station within the system. *Divvy* is managed by the Chicago Department of Transportation (CDOT), which owns the
> city's bikes, stations, and vehicles. ([Link](https://divvybikes.com/about))

## Scenario

You are a junior data analyst in the marketing analyst team at *Cyclistic*, a bike-share company in Chicago. The
marketing director believes the company's future success depends on maximizing the number of annual memberships.
Therefore, your team wants to understand how casual riders and annual members use *Cyclistic* bikes differently.

From these insights, your team will design a new marketing strategy to convert casual riders into annual members. But
first, *Cyclistic* executives must approve your recommendations, so they must be backed up with compelling data insights
and professional data visualizations.

In 2016, *Cyclistic* launched a bike-share offering. Since then, the program has grown to a fleet of 5,824 geo-tracked
bicycles and locked into a network of 692 stations across Chicago. The bikes can be unlocked from one station and
returned to any other station in the system.

Until now, *Cyclistic's* marketing strategy relied on building general awareness and appealing to broad consumer
segments. One approach that helped make these things possible was the flexibility of its pricing plans: single-ride
passes, full-day passes, and annual memberships. Customers who purchase single-ride or full-day passes are called
"Casual riders" (*casual*). Customers who buy yearly memberships are "Cyclistic members" (*member*).

*Cyclistic's* finance analysts have concluded that annual *members* are much more profitable than *casual* riders.
Although the pricing flexibility helps *Cyclistic* attract more customers, the marketing director believes that
maximizing the number of annual members will be critical to future growth.

Rather than creating a marketing campaign that targets all-new customers, she believes there is an excellent chance to
convert *casual* riders into *members*. She notes that *casual* riders are already aware of the *Cyclistic* program.

The marketing director set a clear goal: Design marketing strategies to convert casual riders into annual members. To do
that, however, the marketing analyst team needs to understand better:

1.  How annual members and casual riders differ
2.  Why casual riders would buy a membership
3.  And how digital media could affect their marketing tactics.

She and her team are interested in analyzing the *Cyclistic* historical bike trip data to identify trends. You have
assigned the first question to the answer: **How do annual members and casual riders use Cyclistic bikes differently?**

## Statement of Purpose

Use the given data set for 12 months, analyze different patterns, and present findings and suggestions along with the
analysis result. R has been used for this work.

## Data source

By the time I have completed this case study, *Cyclistic's* historical trip data from 2013 to July 2023 has been
provided for analysis: ([link](https://divvy-tripdata.s3.amazonaws.com/index.html))

> Note: The datasets have a different name, as *Cyclistic* is a fictional company. The data has been made available for
> the case study by *Motivate International Inc.*

In this study, I have downloaded quarterly data files for 2019. Each file is provided in `zip` format. each file
contains a `csv` file.

## Data Pre-processing

Data pre-processing work is followed in the below steps.

1.  Import libraries for use.
2.  Import data from downloaded files.
3.  Check variable names and attributes in data sets to merge them into a single dataframe.
4.  Clean up data to prepare analysis.

### Step 1. Import libraries

```         
setwd("C:/Google_DA_CaseStudy") # use your working directory here
# suppressed messages due to Unicode encoding issues
suppressPackageStartupMessages(library(tidyverse)) 
suppressPackageStartupMessages(library(lubridate)) 
suppressPackageStartupMessages(library(ggplot2)) 
suppressPackageStartupMessages(library(readr)) 
suppressPackageStartupMessages(library(dplyr)) 
suppressPackageStartupMessages(library(gridExtra)) 
suppressPackageStartupMessages(library(gridExtra)) 
```

### Step 2. Import data from downloaded files

I chose `Divvy_Trips_2019_Q1.zip` to `Divvy_Trips_2019_Q4.zip` files to use. Each `zip` file contains a `csv` file.

```         
Trips_2019_Q1 <- read_csv("Divvy_Trips_2019_Q1.csv", show_col_types = FALSE)
Trips_2019_Q2 <- read_csv("Divvy_Trips_2019_Q2.csv", show_col_types = FALSE)
Trips_2019_Q3 <- read_csv("Divvy_Trips_2019_Q3.csv", show_col_types = FALSE)
Trips_2019_Q4 <- read_csv("Divvy_Trips_2019_Q4.csv", show_col_types = FALSE)
```

### Step 3. Check for column names to review

Column names and attributes should be consistent with merging data frames into one.

```         
colnames(Trips_2019_Q1)

##  [1] "trip_id"           "start_time"        "end_time"          "bikeid"            "tripduration"     
##  [6] "from_station_id"   "from_station_name" "to_station_id"     "to_station_name"   "usertype"         
## [11] "gender"            "birthyear"

colnames(Trips_2019_Q2)

##  [1] "01 - Rental Details Rental ID"                    "01 - Rental Details Local Start Time"            
##  [3] "01 - Rental Details Local End Time"               "01 - Rental Details Bike ID"                     
##  [5] "01 - Rental Details Duration In Seconds Uncapped" "03 - Rental Start Station ID"                    
##  [7] "03 - Rental Start Station Name"                   "02 - Rental End Station ID"                      
##  [9] "02 - Rental End Station Name"                     "User Type"                                       
## [11] "Member Gender"                                    "05 - Member Details Member Birthday Year"

colnames(Trips_2019_Q3)

##  [1] "trip_id"           "start_time"        "end_time"          "bikeid"            "tripduration"     
##  [6] "from_station_id"   "from_station_name" "to_station_id"     "to_station_name"   "usertype"         
## [11] "gender"            "birthyear"

colnames(Trips_2019_Q4)

##  [1] "trip_id"           "start_time"        "end_time"          "bikeid"            "tripduration"     
##  [6] "from_station_id"   "from_station_name" "to_station_id"     "to_station_name"   "usertype"         
## [11] "gender"            "birthyear"
```

Except for `Trips_2019_Q2`, column names in other dataframes are identical. While `Trips_2019_Q2` shows different column
names, the attributes look the same. Rename the column names to match the others.

```         
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
```

Inspect the data types for each column. If all match, then the identical functions will return 'TRUE.'

```         
df1_types <- sapply(Trips_2019_Q1, class)
df2_types <- sapply(Trips_2019_Q2, class)
df3_types <- sapply(Trips_2019_Q3, class)
df4_types <- sapply(Trips_2019_Q4, class)
identical(df1_types, df2_types)

## [1] TRUE

identical(df1_types, df3_types)

## [1] TRUE

identical(df1_types, df4_types)

## [1] TRUE
```

Combine all quarterly data into a single dataframe to show annual data. Keep this original dataframe for backup.

```         
Trips_2019v0 <- bind_rows(Trips_2019_Q1, Trips_2019_Q2, Trips_2019_Q3, Trips_2019_Q4)
```

### Step 4. Clean up data to prepare analysis.

Drop irrelevant columns (`gender`,`birthyear`) then save to the new data frame.

```         
Trips_2019v1 <- Trips_2019v0 %>% select(-c(gender, birthyear))
```

Add a new column `duration_min` to show trip duration time taken in minutes.

```         
Trips_2019v1$duration_min <- as.numeric(difftime(Trips_2019v1$end_time, 
                                                 time2 = Trips_2019v1$start_time, 
                                                 units = "mins"))
```

According to the values in a new column `duration_min`, current `tripduration` column indicates values in seconds. To
avoid confusion, change the column name accordingly.

```         
names(Trips_2019v1)[names(Trips_2019v1) == 'tripduration'] <- "duration_sec"
```

Check the dataframe `Trip_2019v1` using the `summary` function.

```         
summary(Trips_2019v1)

##     trip_id           start_time                        end_time                          bikeid    
##  Min.   :21742443   Min.   :2019-01-01 00:04:37.00   Min.   :2019-01-01 00:11:07.00   Min.   :   1  
##  1st Qu.:22873787   1st Qu.:2019-05-29 15:49:26.50   1st Qu.:2019-05-29 16:09:28.25   1st Qu.:1727  
##  Median :23962320   Median :2019-07-25 17:50:54.00   Median :2019-07-25 18:12:23.00   Median :3451  
##  Mean   :23915629   Mean   :2019-07-19 21:47:37.11   Mean   :2019-07-19 22:11:47.56   Mean   :3380  
##  3rd Qu.:24963703   3rd Qu.:2019-09-15 06:48:05.75   3rd Qu.:2019-09-15 08:30:13.25   3rd Qu.:5046  
##  Max.   :25962904   Max.   :2019-12-31 23:57:17.00   Max.   :2020-01-21 13:54:35.00   Max.   :6946  
##   duration_sec      from_station_id from_station_name  to_station_id   to_station_name      usertype        
##  Min.   :      61   Min.   :  1.0   Length:3818004     Min.   :  1.0   Length:3818004     Length:3818004    
##  1st Qu.:     411   1st Qu.: 77.0   Class :character   1st Qu.: 77.0   Class :character   Class :character  
##  Median :     709   Median :174.0   Mode  :character   Median :174.0   Mode  :character   Mode  :character  
##  Mean   :    1450   Mean   :201.7                      Mean   :202.6                                        
##  3rd Qu.:    1283   3rd Qu.:289.0                      3rd Qu.:291.0                                        
##  Max.   :10628400   Max.   :673.0                      Max.   :673.0                                        
##   duration_min      
##  Min.   :   -56.37  
##  1st Qu.:     6.85  
##  Median :    11.82  
##  Mean   :    24.17  
##  3rd Qu.:    21.40  
##  Max.   :177200.37
```

There are two things noticed from `duration_min`.

First, The Minimum value in `duration_min` is -56.37. A negative value seems strange since this column has been
calculated based on the time difference between `end_time` and `start_time`. The packet explains that those values are
from the bikes taken out of service. So I will delete those rows from the data.

```         
Trips_2019v1 <- subset(Trips_2019v1, duration_min >= 0)
```

Second, Compared to Mean and 3rd Quarterly value, the Max value is far too extreme, which means that the bike has turned
in 177,200 minutes later (or 123 days) from `start_time`. How to handle those cases?

In the real world, *Divvy* mentions in FAQ as below:

> "Do not under any circumstances leave your bike unattended; if you left your bike outside of a station please return
> immediately and re-dock your bike. Please know that bikes missing for longer than 24 hours can result in a \$1,200 fee
> (plus tax) charged to the account holder that took out the bike
> ([link](https://help.divvybikes.com/hc/en-us/articles/360033123412-My-bike-was-lost-or-stolen))."

Thus, I will assume that the data with `duration_min` is more than 24 hours (=1,440 minutes) is abnormal instances and
also excluded from the scope of analysis. Let's check how many rows fit these criteria.

```         
count <- sum(Trips_2019v1$duration_min > 1440)
print(count)

## [1] 1849
```

Luckily, those 'unusual' rides take a negligible portion of the data set. Delete those rows, likewise.

```         
Trips_2019v1 <- subset(Trips_2019v1, duration_min <= 1440)
```

(Optional) If your OS language differs from English, it would be better to put them in English when extracting dates.

```         
Sys.setlocale("LC_TIME", "en_US.UTF-8")

## [1] "en_US.UTF-8"
```

Extract data and time information from `start_time` and `end_time`. I want to get the columns for `year`, `month`,
`days`, `start_date`, `end_date`.

```         
Trips_2019v1$year <- as.integer(format(Trips_2019v1$start_time, "%Y"))
Trips_2019v1$month <- format(Trips_2019v1$start_time, "%b")
Trips_2019v1$days <- weekdays(Trips_2019v1$start_time)
Trips_2019v1$start_date <- as.integer(format(Trips_2019v1$start_time, "%d"))
Trips_2019v1$end_date <- as.integer(format(Trips_2019v1$end_time, "%d"))
```

And convert current `start_time` and `end_time` to show time data only.

```         
Trips_2019v1$start_time <- format(Trips_2019v1$start_time, "%H:%M:%S")
Trips_2019v1$end_time <- format(Trips_2019v1$end_time, "%H:%M:%S")
```

As we don't need float values, convert other numeric columns to integers.

```         
cols_to_int <- c("trip_id",
                 "bikeid",
                 "from_station_id",
                 "to_station_id",
                 "duration_min",
                 "duration_sec")
Trips_2019v1[cols_to_int] <- lapply(Trips_2019v1[cols_to_int], as.integer)
```

I will remove the redundant `duration_sec` column, then make a Copy of a new data frame.

```         
Trips_2019v2 <- subset(Trips_2019v1, select = -c(duration_sec))
```

Rearrange columns for better visibility of titles.

```         
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
                                 "from_station_name",
                                 "to_station_id",
                                 "to_station_name",
                                 "duration_min")]
```

Let's check what kind of variables exist in the `usertype`.

```         
table(Trips_2019v2$usertype)

## 
##   Customer Subscriber 
##     879283    2936859
```

There are only two types of variables in the `usertype`. Update the terms to align with the words in the scenario
(*casual*, *member*). At the same time, change the column name to `membership`.

```         
Trips_2019v2 <- Trips_2019v2 %>% mutate(usertype = recode(usertype,
                          "Customer" = 'Casual',
                          "Subscriber" = 'Member'))

names(Trips_2019v2)[names(Trips_2019v2) == 'usertype'] <- "membership"
```

Reorder the order of days to appear.

```         
Trips_2019v2$days <- ordered(Trips_2019v2$days, levels=c("Monday",
                                                         "Tuesday",
                                                         "Wednesday",
                                                         "Thursday",
                                                         "Friday",
                                                         "Saturday",
                                                         "Sunday"))
```

Check for the occurrences of values in this column.

```         
# Find the number of unique values in column from_station_id
n_unique_tripid <- length(unique(Trips_2019v2$trip_id))
n_unique_usertype <- length(unique(Trips_2019v2$membership))
n_unique_bikeid <- length(unique(Trips_2019v2$bikeid))
n_unique_from_station_id <- length(unique(Trips_2019v2$from_station_id))
n_unique_to_station_id <- length(unique(Trips_2019v2$to_station_id))
print(paste("Number of total bikes in 2019:", n_unique_bikeid)) # 6017

## [1] "Number of total bikes in 2019: 6017"

print(paste("Number of bike stations in 2019:", n_unique_from_station_id)) # 616

## [1] "Number of bike stations in 2019: 616"

print(paste("Total trips made in 2019:", n_unique_tripid)) # 3816142

## [1] "Total trips made in 2019: 3816142"
```

Compared to the description in the scenario, the fleet size has grown from 5,824 to 6,017 while the number of stations
has decreased from 692 to 616 since 2016.

The pre-processing part is now complete. So far, the following works have been done:

1.  Import data files and convert them to dataframe
2.  Match attributes in each dataframe to merge them into one
3.  Remove, add, and modify the variables and features as necessary
4.  Build a clean dataframe from the original for further analysis.

Now, continue with the data analysis.

## Data Analysis

This analysis needs to provide the answer to given question: How do *members* and *casual* riders use bikes differently?
We'll also analyze usage patterns over different periods.

### Step 1. Check for statistics

To begin, let's analyze the percentage of rides made by membership.

```         
membership_table <- table(Trips_2019v2$membership) # Calculate the table of membership
membership_fraction <- prop.table(membership_table) # Calculate the fraction in membership
result <- cbind(membership_table, membership_fraction) # Combine the tables
print(result)

##        membership_table membership_fraction
## Casual           879283           0.2304115
## Member          2936859           0.7695885
```

It is evident that most of the rides, approximately 77%, are made by members. Let's compare statistics for riding times
in minutes `duration_min`.

```         
# Calculate the mean, median, max, and min of duration_min grouped by membership
mean_duration <- aggregate(Trips_2019v2$duration_min ~ Trips_2019v2$membership, FUN=mean)
median_duration <- aggregate(Trips_2019v2$duration_min ~ Trips_2019v2$membership,FUN=median)
max_duration <- aggregate(Trips_2019v2$duration_min ~ Trips_2019v2$membership, FUN=max)
min_duration <- aggregate(Trips_2019v2$duration_min ~ Trips_2019v2$membership, FUN=min)

# Combine the results into a single data frame
result <- data.frame(Membership = mean_duration[, 1],
                     Mean = mean_duration[, 2],
                     Median = median_duration[, 2],
                     Max = max_duration[, 2],
                     Min = min_duration[, 2])
print(result)

##   Membership     Mean Median  Max Min
## 1     Casual 38.93788     25 1439   1
## 2     Member 12.44071      9 1439   1
```

Comparing riding times in `duration_min`, maximum and minimum values are identical for both membership types. However,
the mean value indicates a difference between users. *Casual* users ride longer than *members* per ride, although they
make up less than 25% of total usage. On the other hand, *members* ride for shorter distances but more frequently.

### Step 2. Monthly patterns

To gain a more detailed understanding of these patterns, I will analyze the data on a monthly and hourly scale in the
following analysis. Let's visualize the monthly patterns by membership.

```         
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
```

![fig1](https://github.com/escavecha/bike_share_case_study/blob/master/p1_monthly_mean_duration.png)


**Key findings:**

-   On average, *casual* riders use the bike for approximately 29 to 41 minutes per ride.
-   On average, *members* use the bike for around 11 to 14 minutes per ride.
-   *Casual* riders have the lowest average ride duration in February, while *members* have the lowest average in
    November and December.
-   The variance in ride duration trend is more minor for *members* than *casual* riders.

We can plot the average number of rides per station per month to gain different insights.

```         
# calculate average number of rides per station
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
```

![fig2](https://github.com/escavecha/bike_share_case_study/blob/master/p2_monthly_no_station.png)


**Key findings:**

-   The least rides are made in February, while the most are made in August for both membership types.
-   Ride usage is similar to the monthly weather condition. People ride less during winter, while they ride more during
    summer.

### Step 3. Daily patterns

Let's take a look in detail for daily patterns. Plot a line chart for daily usage patterns.

```         
df_daily_mean_duration <- aggregate(Trips_2019v2$duration_min ~ Trips_2019v2$membership +
  Trips_2019v2$days, FUN=mean)
colnames(df_daily_mean_duration) <- c("membership", "days", "mean_duration")

p3_daily_mean_duration <- ggplot(df_daily_mean_duration, aes(x = days,
                                   y = mean_duration, group = membership, color = membership)) +
  geom_line() +
  geom_point() +
  scale_x_discrete(limits = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")) +
  labs(x = "Days", y = "Avg. Duration (min)",
       title = "Fig 3. Average Ride Duration by Membership, daily (2019)") +
  geom_text(aes(label = round(mean_duration, 1)), vjust = -0.5)

print(p3_daily_mean_duration)
ggsave("p3_daily_mean_duration.png", plot = p3_daily_mean_duration, 
       width = 7.3, height = 4.6)
```

![fig3](https://github.com/escavecha/bike_share_case_study/blob/master/p3_daily_mean_duration.png)


**Key findings:**

-   The most prolonged duration is observed on Saturday for *casual* and *member*.
-   *Casual* riders take about 25 minutes more than *members*.
-   From Monday to Friday, the average duration time is almost the same for *members*.

Consistency in the patterns may be related to the commuting purpose. To verify this hypothesis, continue to analyze the
daily patterns for the number of rides.

```         
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
```

![fig4](https://github.com/escavecha/bike_share_case_study/blob/master/p4_daily_no_mean.png)


**Key findings:**

-   The average number of rides for *members* is highest along the weekdays. On weekends, those numbers drop in half.
-   On the contrary, *casual* riders' average number of rides is highest on Saturday.

The pattern is in line with the hypothesis from *Figure 3.* Those rides are mainly used for commuting for *Members*,
while rides are increased during weekends for *casual* riders who use them for leisurely purposes on weekends. Still,
*casual* riders are using the bike on weekdays too.

### Step 4. Hourly patterns

Let's take a look at the patterns on an hourly basis each day to verify this finding. I will plot the charts for
weekdays and weekends to see the difference.

```         
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
```
```         
print(p5_weekdays_mean)

## TableGrob (2 x 1) "arrange": 2 grobs
##   z     cells    name           grob
## 1 1 (1-1,1-1) arrange gtable[layout]
## 2 2 (2-2,1-1) arrange gtable[layout]

ggsave("p5_weekdays_mean.png", plot = p5_weekdays_mean, width = 7.3, height = 4.6)
```
![fig5](https://github.com/escavecha/bike_share_case_study/blob/master/p5_weekdays_mean.png)

**Key findings:**

-   The average duration for *Members* is stable in the range of 10 to 15 minutes.
-   The average duration for *casual* riders is shortest in the morning time (at 6 to 7 am) and peaks at 10 am.
    Generally, *casual* riders take a more extended trip for their use.
-   Most frequent rides are made at 8 am and 5 pm for *members*.
-   The most frequent ride is made at 5 pm for *casual* riders.
-   There are the least rides for any membership during midnight to 4 am.

From the patterns, it is more apparent that the primary purpose of rides for *members* is commuting. Unlike *casual*
rider patterns, *members* have consistent patterns. To confirm, visualize the exact data for weekends.

```         
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
```
```         
print(p6_weekend_mean)

## TableGrob (2 x 1) "arrange": 2 grobs
##   z     cells    name           grob
## 1 1 (1-1,1-1) arrange gtable[layout]
## 2 2 (2-2,1-1) arrange gtable[layout]

ggsave("p6_weekend_mean.png", plot = p6_weekend_mean, width = 7.3, height = 4.6)
```
![fig6](https://github.com/escavecha/bike_share_case_study/blob/master/p6_weekend_mean.png)

**Key findings:**

-   The average duration is consistent at around 15 minutes for *members*.
-   Unlike weekday patterns, the average duration is 40 minutes for *casual* riders.
-   Most rides are made in the afternoon for both *members* and *casual* riders.

*Figures 5b* and *6b* show that the least busy hours are from midnight to 4 am daily. Therefore if any maintenance or
the relocation of the bikes should be made, this will be the most optimum window. If the night shift is unavailable, the
4 hours window from 10 am to 2 pm on weekdays will be an alternative to the schedule.

## Analysis Summary

-   *Members* take about 77% of total rides in 2019.
-   *Casual* riders tend to take longer trips than *members*.
-   The ride pattern is analogous to the monthly climate patterns for both *members* and *casual* riders.
-   Most *members* use the bike for commuting on weekdays; *Casual* riders ride most during the weekend(Saturday).
-   The busiest times are 8 am and 5 pm on weekdays.

### Suggestions

-   Considering the monthly ride patterns, schedule the annual maintenance plan of the fleets focused on during winter
    time to allow more availability of the bikes for the rest of the seasons.
-   Schedule daily services & maintenance on the fleets from midnight to 4 am (daily) or 10 am to 2 pm (weekdays).

### Next steps

*Figure 5b* shows the busiest times are around 8 am and 5 pm. An adequate number of bikes should be prepared at the
stations to attract people to subscribe to membership.

As the number of bikes in the fleets is limited, I suggest relocating the bikes from the least busy stations to the
busiest stations to accommodate most usage. Further analysis will focus on identifying the stations' loads and figuring
out how to provide flexibility for the fleets across the stations.
