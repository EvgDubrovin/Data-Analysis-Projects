## PREPARE

# Download the needed packages
install.packages("tidyverse")
library(tidyverse) 
library(lubridate) # for work with time data

# install.packages("janitor")
# library(janitor)

install.packages("skimr") # for describing statistics of data
library(skimr)

library(scales) # for adjusting display of values on plot axis 

# Display our working directory
getwd()

# Let's change our working directory for easier file downloading
setwd("D:/BSNS/STUDY/Coursera/Google Data Analyze Certificate/Part 8 - Case Study/CS1_Cyclistics/data")

# Import our bike trips data into R 
trips_202010 <- read.csv("202010-divvy-tripdata.csv")
trips_202011 <- read.csv("202011-divvy-tripdata.csv")
trips_202012 <- read.csv("202012-divvy-tripdata.csv")
trips_202101 <- read.csv("202101-divvy-tripdata.csv")
trips_202102 <- read.csv("202102-divvy-tripdata.csv")
trips_202103 <- read.csv("202103-divvy-tripdata.csv")
trips_202104 <- read.csv("202104-divvy-tripdata.csv")
trips_202105 <- read.csv("202105-divvy-tripdata.csv")
trips_202106 <- read.csv("202106-divvy-tripdata.csv")
trips_202107 <- read.csv("202107-divvy-tripdata.csv")
trips_202108 <- read.csv("202108-divvy-tripdata.csv")
trips_202109 <- read.csv("202109-divvy-tripdata.csv")

# View a summary about each df sequentially
glimpse(trips_202010)
glimpse(trips_202011)
glimpse(trips_202012)
glimpse(trips_202101)
glimpse(trips_202102)
glimpse(trips_202103)
glimpse(trips_202104)
glimpse(trips_202105)
glimpse(trips_202106)
glimpse(trips_202107)
glimpse(trips_202108)
glimpse(trips_202109)

# Note: We need to unite our monthly data into one data frame, but start_station_id and end_station_id 
# columns in October and November of 2020 are integer, though in other tables they are characters. 
# Let' fix this for correct joining.

trips_202010 %>%
  as_tibble() %>% 
  mutate(start_station_id = as.character(start_station_id), 
         end_station_id = as.character(end_station_id))

trips_202011 %>%
  as_tibble() %>% 
  mutate(start_station_id = as.character(start_station_id), 
         end_station_id = as.character(end_station_id))

# Unite all the df's into one
trips_total_raw <- rbind(trips_202010, trips_202011, trips_202012, trips_202101, trips_202102, 
                     trips_202103, trips_202104, trips_202105, trips_202106, trips_202107, 
                     trips_202108, trips_202109)

# Explore the structure of the combined table
str(trips_total_raw)
summary(trips_total_raw)

# What is docked_bike?


# I noticed some missing values in the data (start_station_name, start_station_id, end_station_name,  end_station_id, 
# end_lat and end_lng). Let's check it out
print("Duplicates in ride_id:")
sum(duplicated(trips_total_raw$ride_id))
length(trips_total_raw$ride_id)
length(unique(trips_total_raw$ride_id))
length(unique(trips_total_raw$ride_id)) == nrow(trips_total_raw)

trips_total_raw %>% 
  duplicated(ride_id)

# Removing duplicates
#checking the duplicated values by ride_id
duplicate_ride_id <- trips_total_raw[duplicated(trips_total_raw$ride_id),]
duplicate_ride_id

#glimpse of data structure which clears that there are messy and inaccurate values (209)
glimpse(duplicate_ride_id)

#extracting duplicates and inaccurate data
non_duplicated_rides <- trips_total_raw[!duplicated(trips_total_raw$ride_id),]

#checking out if the dates are still inaccurately recordered.
glimpse(non_duplicated_rides)
summary(non_duplicated_rides)
sum(duplicated(non_duplicated_rides$ride_id))

# Check for NA's
colSums(is.na(non_duplicated_rides)) #checking the NA values in the variables


sum(trips_total_raw$start_station_name == "")

sum(trips_total_raw$start_station_id == "", na.rm = TRUE)
sum(is.na(trips_total_raw$start_station_id))

sum(trips_total_raw$end_station_name == "")

sum(trips_total_raw$end_station_id == "", na.rm = TRUE)
sum(is.na(trips_total_raw$end_station_id))

na_lat <- sum(is.na(trips_total_raw$end_lat))
na_lng <- sum(is.na(trips_total_raw$end_lng))

na_start_station <- sum(is.na(trips_total_raw$start_station_id))
na_end_station <- sum(is.na(trips_total_raw$end_station_id))

# I don't think we'll need all the station names, but we might need correct latitude and longitude for geo data
# analysis. So, I'll remove empty rows from the df
lat_lng_data_loss_percent <- round(na_lat / nrow(trips_total_raw) * 100, 3) # counting percent of data going to remove
lat_lng_data_loss_percent # the lost data is less than 0.1 percent, so we can remove it

start_station_data_loss_percent <- round(na_start_station / nrow(trips_total_raw) * 100, 3)
start_station_data_loss_percent

end_station_data_loss_percent <- round(na_end_station / nrow(trips_total_raw) * 100, 3)
end_station_data_loss_percent

print("Percent of dropped data:")
non_duplicated_rides %>% 
  mutate(na_lat = sum(is.na(end_lat))) %>% 
  mutate(na_lng = sum(is.na(end_lng)) %>%
  mutate(loss_percent_lat = na_lat/nrow(end_lat)) %>% 
  mutate(loss_percent_lng = na_lng/nrow(end_lng))

  
# Deleting empty values of latitude and longitude 
trips_total <- trips_total_raw %>%
  drop_na(start_station_id, end_station_id , end_lat, end_lng)
# OR
trips_total <- drop_na(trips_total_raw)

data_loss_percent <- (nrow(trips_total_raw)-nrow(trips_total)) / nrow(trips_total_raw) * 100
data_loss_percent # The lost data is less than 2%, so we can delete it

# Let's explore summary of our resulting df
summary(trips_total)

## PROCESS
# Let's count a ride_length value and create a new column for it
# First, we need to convert our time values (started_at and ended_at) from character to date
class(trips_total$started_at) # check the class of our time columns
trips_total$started_at <- ymd_hms(trips_total$started_at)
trips_total$ended_at <- ymd_hms(trips_total$ended_at)
# Recheck the time columns class
class(trips_total$started_at) 
class(trips_total$ended_at)
summary(trips_total)

# Counting a ride length in a column
trips_total$ride_length <- (trips_total$ended_at) - (trips_total$started_at)

head(trips_total)

summary(trips_total)

# I noticed some negative values in ride_length column. These are errors (resulted by incorrect date recording). Let's count them
sum(trips_total$ride_length < 0)

# Also, I suppose to delete too short rides - let's say less than 10 secs, inclusive. 
# Let's delete these errors
trips_total_filtered <- trips_total %>% 
  filter(ride_length > 5)

# Let's create a tibble to facilitate our work with the table
t_f <- as_tibble(trips_total_filtered)
t_f

# I noticed some suspicious station names contained "TESTING". I assume, these are test rides by Cyclistic's specialists. We won't include this in our analysis.

trips_tested <- t_f %>% 
  filter(grepl("TEST", start_station_name) | grepl("TEST", end_station_name))

trips_tested # we have 97 test trips. Let's remove them from our table

# Remove test trips from our table

t_f_v2 <- t_f %>% 
  filter(!(grepl("TEST", start_station_name) | grepl("TEST", end_station_name)))

View(t_f_v2)

# Also, I suppose to remove trips that are longer than one day (24*60*60), because they're probably an error and not representative
t_f_v2 <- t_f_v2[!(t_f_v2$ride_length > (24*60*60)), ]
head(sort(t_f_v2$ride_length, decreasing = TRUE), n=50) # check

# Let's create a "day_of_week" column and calculate day of the week, month, year that each ride started - 
# to provide additional opportunities to aggregate the data

# Change language of weekday output
Sys.setlocale("LC_TIME", "English")

t_f_v2$day_of_week <- wday(t_f_v2$started_at, label = TRUE, abbr = FALSE)

t_f_v2$month <- month(t_f_v2$started_at, label = TRUE, abbr = FALSE)

t_f_v2$year <- year(t_f_v2$started_at)

# Removing needless objects
remove(trips_docked)
remove(trips_total_filtered_v2)
remove(trips_total_filtered_v3)

# Do we have problems with "member_casual" column?
table(t_f_v2$member_casual) # No, we don't

## ANALYZE
# Convert "ride_length" from Factor to numeric so we can run calculations on the data
# t_f_v2$ride_length <- as.numeric(as.character(t_f_v2$ride_length))
# is.numeric(t_f_v2$ride_length)

# Descriptive analysis of ride_length

mean(t_f_v2$ride_length) #straight average (total ride length / rides)
median(t_f_v2$ride_length) #midpoint number in the ascending array of ride lengths
min(t_f_v2$ride_length)  #shortest ride
max(t_f_v2$ride_length) #longest ride

summary(trips_total_filtered$ride_length) # I don't know why, but it doesn't work

t_f_v2 %>% 
  summarise(min_ride_length = min(t_f_v2$ride_length),
            max_ride_length = max(t_f_v2$ride_length),
            mean_ride_length = mean(t_f_v2$ride_length),
            median_ride_length = median(t_f_v2$ride_length),
            first_quantile = quantile(t_f_v2$ride_length, 0.25),
            third_quantile = quantile(t_f_v2$ride_length, 0.75))

# Compare members and casual users
aggregate(t_f_v2$ride_length ~ t_f_v2$member_casual, FUN = mean)
aggregate(t_f_v2$ride_length ~ t_f_v2$member_casual, FUN = median)
aggregate(t_f_v2$ride_length ~ t_f_v2$member_casual, FUN = max)
aggregate(t_f_v2$ride_length ~ t_f_v2$member_casual, FUN = min)

plot_theme = theme(
  plot.title = element_text(size=20, face = 'bold'), # hjust = 0.5  or #vjust = (you can choose accordingly)
  plot.subtitle = element_text(size=10, color = 'gray', face = 'bold'), # hjust = 0.5  or #vjust = (you can choose accordingly)
  plot.caption = element_text(size=12, color = 'darkgray', face = 'bold'),
  axis.text.x = element_text(size=16),
  axis.text.y = element_text(size=16),
  axis.title.x = element_text(size=18), 
  axis.title.y = element_text(size=18),
  strip.text.x = element_text(size=16), 
  strip.text.y = element_text(size=16),
  legend.title = element_text(size=18), 
  legend.text = element_text(size=16)
)

options(repr.plot.width = 10, repr.plot.height = 8)

# Or like this

# How many customers by type
t_f_v2 %>%
  group_by(member_casual) %>%
  summarize(total = n()) %>%
  mutate(overall_total = sum(total))

# Number of trips by type of client
t_f_v2 %>% 
  group_by(member_casual) %>% 
  summarise(number_of_trips = n())

2712129 / 2308098

# Viz
t_f_v2 %>% 
  group_by(member_casual) %>% 
  summarise(number_of_trips = n()) %>% 
  ggplot(aes(x=member_casual, y=number_of_trips, fill=member_casual)) + 
  geom_col(position = "dodge") + 
  labs(title = "Total trips: Members vs. Casual Riders",
       x = "Type of Rider", y = "Number", fill = "Rider Type", 
       caption = "source: Motivate International Inc.") + 
  scale_y_continuous(label=comma) + 
  geom_text(aes(label=comma(number_of_trips)), position = position_stack(vjust = 0.95), size = 3.5) + 
  plot_theme
# Analyze. Annual members do more rides (in 1.2 times more) in total.

## Visualize total ride duration by type of riders
t_f_v2 %>% 
  group_by(member_casual) %>% 
  summarise(sum_ride_duration = sum(ride_length)/60/60) %>% 
  ggplot(aes(x = member_casual, y = sum_ride_duration, fill=member_casual)) + 
  geom_col(position = "dodge") +
  scale_y_continuous(labels = comma) + 
  labs(x = "Rider Type", y = "Total Ride Duration (hours)",
       title = "Total Ride Duration: Members vs. Casual Riders", 
       caption = "source: Motivate International Inc.") +
  theme(
    plot.title = element_text(size=18, face = 'bold'), 
    plot.caption = element_text(size=12, color = 'darkgray', face = 'bold'),
    axis.text.x = element_text(size=15),
    axis.text.y = element_text(size=15),
    axis.title.x = element_text(size=18), 
    axis.title.y = element_text(size=18),
    legend.title = element_text(size=18),
    legend.text = element_text(size=16),
  )

1076489 / 629103

# Analyze. We can see that casual riders have a way bigger (in 1.71 times) total ride duration than members, so maybe the initial 
# hypothesis of the manager is not correct and Cyclistics don't have to convert all riders to annual members.
# We need more financial data. Do clients pay a fixed price for taking bike or they pay for an hour (minute)?

## How many different customers?
diff_riders <- t_f_v2 %>%
  group_by(member_casual) %>%
  summarize(total_by_type = n()) %>%
  mutate(overall_total = sum(total_by_type)) %>% 
  group_by(member_casual) %>%
  summarize(percent_total = total_by_type/overall_total)

t_f_v2 %>%
  group_by(member_casual) %>%
  summarize(total_by_type = n()) %>%
  mutate(overall_total = sum(total_by_type)) %>% 
  group_by(member_casual) %>%
  summarize(percent_total = total_by_type/overall_total)

# Viz
t_f_v2 %>%
  group_by(member_casual) %>%
  summarize(total_by_type = n()) %>%
  mutate(overall_total = sum(total_by_type)) %>% 
  group_by(member_casual) %>%
  summarize(percent_total = total_by_type/overall_total) %>% 
  ggplot(aes(fill=member_casual, y=percent_total, x="")) + 
  geom_bar(position="fill", stat="identity") +
  geom_text(aes(label = percent(percent_total)), 
            position = position_stack(vjust = 0.5), size = 10) + 
  labs(x = "", y = "Percent",
       title = "Different Customers Distribution", 
       caption = "source: Motivate International Inc.") + 
  theme(
    plot.title = element_text(size=18, face = 'bold'), 
    plot.caption = element_text(size=12, color = 'darkgray', face = 'bold'),
    axis.text.x = element_text(size=15),
    axis.text.y = element_text(size=15),
    axis.title.x = element_text(size=18), 
    axis.title.y = element_text(size=18),
    legend.title = element_text(size=18),
    legend.text = element_text(size=16),
  ) + 
  scale_y_continuous(labels = percent)
  
# Average trips duration by type of client
t_f_v2 %>% 
  group_by(member_casual) %>% 
  summarise(average_ride_length = mean(ride_length))

1679 / 835

# Viz
t_f_v2 %>% 
  group_by(member_casual) %>% 
  summarise(average_ride_length = mean(ride_length)) %>% 
  ggplot(aes(x=member_casual, y=average_ride_length, fill=member_casual)) + 
  geom_col(position = "dodge") + 
  labs(title = "Average Ride Duration: Members vs. Casual Riders",
       x = "Type of Rider", y = "Average Ride Length (s)", fill = "Rider Type", 
       caption = "source: Motivate International Inc.") +
  scale_y_continuous(label=comma) +
  geom_text(aes(label=comma(average_ride_length)), position = position_stack(vjust = 0.95), size = 3.5) + 
  plot_theme
# Analyze. Casual users ride duration is more than double members' duration in average, though members 
# do more rides in total. 

# Median trips duration by type of client
t_f_v2 %>% 
  group_by(member_casual) %>% 
  summarise(median_ride_length = median(ride_length))


# See the average ride time by each day for members vs casual users
t_f_v2 %>% 
  group_by(member_casual, day_of_week) %>% 
  summarise(average_ride_time = format(round(mean(ride_length), 4), nsmall = 4)) %>% 
  arrange(day_of_week)

# Or
aggregate(t_f_v2$ride_length ~ t_f_v2$member_casual + t_f_v2$day_of_week, FUN = mean)

# Let's order our weekdays in a normal order
t_f_v2$day_of_week <- ordered(t_f_v2$day_of_week, levels=c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

# Average ride time and number of rides by client type and day of week
t_f_v2 %>% 
  group_by(member_casual, day_of_week) %>% 
  summarise(num_of_rides = n(),
            average_ride_time = format(round(mean(ride_length), 4), nsmall = 4)) %>% 
  arrange(day_of_week)

## Visualize
# Visualize the number of rides by day and type of rider
t_f_v2 %>% 
  group_by(member_casual, day_of_week) %>% 
  summarise(num_of_rides = n()) %>% 
  arrange(day_of_week)  %>% 
  ggplot(aes(x = day_of_week, y = num_of_rides, fill = member_casual)) + 
  geom_col(position = "dodge") + 
  scale_y_continuous(labels = comma) + # changing y_axis numbers format
  labs(title = "Average Number of Rides During a Week: Members vs. Casual Riders",
       x = "Day of Week", y = "Number", fill = "Rider Type", 
       caption = "source: Motivate International Inc.") +
  theme(
    plot.title = element_text(size=20, face = 'bold'), 
    plot.subtitle = element_text(size=10, color = 'gray', face = 'bold'), 
    plot.caption = element_text(size=12, color = 'darkgray', face = 'bold'),
    axis.text.x = element_text(size=15),
    axis.text.y = element_text(size=15),
    axis.title.x = element_text(size=18), 
    axis.title.y = element_text(size=18),
    strip.text.x = element_text(size=10), 
    strip.text.y = element_text(size=10),
    legend.title = element_text(size=18), 
    legend.text = element_text(size=16)) + 
  theme(axis.text.x = element_text(angle=45))
# Analyze. We can see that casual riders use bikes mostly on weekends - hypothethically for leisure (or exercising). 
# And members use bikes almost equally throughout a week, but with peaks on workdays.

# Visualize average ride duration by day and type of rider
t_f_v2 %>% 
  group_by(member_casual, day_of_week) %>% 
  summarise(avg_ride_time = (mean(ride_length)/60), .groups = 'drop') %>% 
  arrange(day_of_week)  %>% 
  ggplot(aes(x = day_of_week, y = avg_ride_time, fill = member_casual)) + 
  geom_col(position = "dodge") + 
  labs(title = "Average Ride Duration During a Week: Members vs. Casual Riders", 
       x = "Day of Week", y = "Mins", fill = "Rider Type", 
       caption = "source: Motivate International Inc.") + 
  theme(
    plot.title = element_text(size=16, face = 'bold'), 
    plot.caption = element_text(size=12, color = 'darkgray', face = 'bold'),
    axis.text.x = element_text(size=15),
    axis.text.y = element_text(size=15),
    axis.title.x = element_text(size=18), 
    axis.title.y = element_text(size=18),
    legend.title = element_text(size=18),
    legend.text = element_text(size=16)
    ) +
  theme(axis.text.x = element_text(angle=45))
# Analyze. We can see that average ride duration of casual riders is much higher than members'. 
# This reinforces hypothesis that casual riders use bikes mostly for leisure trips (or exercising / tourism) and members 
# use bikes mostly for practical purposes (e.g. get to work).

# Will check in Tableau

# # Seasonal trends
# t_f_v2 %>%
#   group_by(member_casual, month, day_of_week) %>%
#   summarise(num_of_rides = n()) %>%
#   arrange(month, day_of_week)  %>%
#   ggplot() +
#   geom_line(aes(x = month, y = num_of_rides, color = member_casual)) +
#   scale_y_continuous(labels = comma) +
#   labs(x = "Month", y = "Number of Rides", fill = "Rider Type",
#        title = "Seasonal Trends: Members vs. Casual Riders",
#        caption = "source: Motivate International Inc.")

# t_f_v2 %>%
#   group_by(member_casual, month, day_of_week) %>%
#   summarise(num_of_rides = n()) %>%
#   arrange(month, day_of_week)

## Analyze by month

# First, let's order our month in a chronological order
## t_f_v2$month <- month(t_f_v2$started_at, label = TRUE, abbr = FALSE)

# month_year <- format_ISO8601(t_f_v2$started_at, precision = "ym")
# month_year

t_f_v2$day_of_week <- ordered(t_f_v2$day_of_week, levels=c("October", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

# Number of rides by month
t_f_v2 %>% 
  group_by(member_casual, month) %>% 
  summarise(num_of_rides = n(), .groups = 'drop') %>% 
  arrange(month) %>% 
  ggplot(aes(x = month, y = num_of_rides, fill = member_casual)) + 
  geom_col(position = "dodge") + 
  labs(title = "Number of Rides by Months: Members vs. Casual Riders",
       x = "Month", y = "Number", fill = "Rider Type", 
       caption = "source: Motivate International Inc.") +
  scale_y_continuous(labels = comma) + 
  theme(axis.text.x = element_text(angle=45)) + 
  theme(
    plot.title = element_text(size=16, face = 'bold'), 
    plot.caption = element_text(size=12, color = 'darkgray', face = 'bold'),
    axis.text.x = element_text(size=15),
    axis.text.y = element_text(size=15),
    axis.title.x = element_text(size=18), 
    axis.title.y = element_text(size=18),
    legend.title = element_text(size=18),
    legend.text = element_text(size=16)
  )
# Analyze. We see the biggest values in summer (the peak is in July). 
# Thus, it's the best time to start our marketing campaign.

# Visualize average ride duration by month and type of rider
t_f_v2 %>% 
  group_by(member_casual, month) %>% 
  summarise(avg_ride_time = (mean(ride_length)/60)) %>% 
  arrange(month) %>% 
  ggplot(aes(x = month, y = avg_ride_time, fill = member_casual)) + 
  geom_col(position = "dodge") + 
  labs(title = "Average Ride Duration by Months: Members vs. Casual Riders",
       x = "Month", y = "Average Ride Duration (mins)", fill = "Rider Type", 
       caption = "source: Motivate International Inc.") +
  theme(axis.text.x = element_text(angle=45)) + 
  theme(
    plot.title = element_text(size=16, face = 'bold'), 
    plot.caption = element_text(size=12, color = 'darkgray', face = 'bold'),
    axis.text.x = element_text(size=15),
    axis.text.y = element_text(size=15),
    axis.title.x = element_text(size=18), 
    axis.title.y = element_text(size=18),
    legend.title = element_text(size=18),
    legend.text = element_text(size=16)
  )
# Analyze. Members' ride duration stays almost equal throughout a year with a slight decline in December and January.
# Interesting, that the peak month by ride duration for members is February.
# The peaks for casuals is in springtime. Summer weather is too hot for long trips (especially for sport exercising). 
# Later duration growth in autumn confirms it. So, we can suggest that a big amount of casual riders are sportsmen (do fitness).

# Number of rides by day of week during a year
t_f_v2 %>% 
  group_by(month, day_of_week, member_casual) %>% 
  summarise(num_of_rides = n(), .groups = 'drop') %>%
  drop_na() %>% 
  ggplot(aes(x = day_of_week, y = num_of_rides, fill = member_casual)) +
  geom_col(position = "dodge") +
  scale_y_continuous(labels = comma) +
  facet_grid(member_casual~month) +
  labs(x = "Day of Week", y = "Number of Rides", fill = "Member/Casual",
       title = "Number of Rides by Day During a Year: Members vs. Casual Riders", fill = 'Member/Casual') +
  theme(axis.text.x = element_text(angle = 90)) + 
  theme(
    plot.title = element_text(size=16, face = 'bold'), 
    plot.caption = element_text(size=12, color = 'darkgray', face = 'bold'),
    axis.text.x = element_text(size=15),
    axis.text.y = element_text(size=15),
    axis.title.x = element_text(size=18), 
    axis.title.y = element_text(size=18),
    legend.title = element_text(size=18),
    legend.text = element_text(size=16),
    strip.text.x = element_text(size=15),
    strip.text.y = element_text(size=15)
  )
# Analyze. Though total number of rides by annual members is bigger than casual riders, random users need more
# bikes on weekends from may to september.

## Analyze by hour

# Create hour column in our df
t_f_v2$start_hour <- hour(t_f_v2$started_at)
t_f_v2$end_hour <- hour(t_f_v2$ended_at)

# Average number of rides by hour
t_f_v2 %>% 
  group_by(member_casual, start_hour) %>% 
  summarise(num_of_rides = n(), .groups = 'drop') %>% 
  arrange(-num_of_rides)

# Viz
t_f_v2 %>% 
  group_by(member_casual, start_hour) %>% 
  summarise(num_of_rides = n(), .groups = 'drop') %>% 
  arrange(start_hour) %>% 
  ggplot(aes(x=start_hour, y=num_of_rides, fill=member_casual)) + 
  geom_col(position = "dodge") +
  labs(title = "Number of Rides by Hour: Members vs. Casual Riders",
       x = "Day Time (h)", y = "Number of Rides", fill = "Rider Type", 
       caption = "source: Motivate International Inc.") + 
  scale_y_continuous(labels = comma) + 
  scale_x_continuous(breaks = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23)) +
  theme(
    plot.title = element_text(size=18, face = 'bold'), 
    plot.caption = element_text(size=12, color = 'darkgray', face = 'bold'),
    axis.text.x = element_text(size=15),
    axis.text.y = element_text(size=15),
    axis.title.x = element_text(size=18), 
    axis.title.y = element_text(size=18),
    legend.title = element_text(size=18),
    legend.text = element_text(size=16),
  )
# Analyze. Bike usage rises near 6 AM for members (but not for casual riders). 
# Both of them have peaks near 5 PM (3-6PM). Casual riders peaks (more than 150,000) start after 
# 12:00 and least until 19:00. At evening and night time casual riders use bikes more than members.

# Average ride duration by hour
t_f_v2 %>% 
  group_by(member_casual, start_hour) %>% 
  summarise(avg_ride_time = (mean(ride_length)/60), .groups = 'drop') %>% 
  arrange(start_hour) %>% 
  ggplot(aes(x=start_hour, y=avg_ride_time, fill=member_casual)) + 
  geom_col(position = "dodge") +
  labs(x = "Day Time (h)", y = "Average Ride Duration (mins)", fill = "Rider Type", 
       title = "Average Ride Duration by Hour: Members vs. Casual Riders", 
       caption = "source: Motivate International Inc.") + 
  scale_x_continuous(breaks = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23)) +
  theme(
    plot.title = element_text(size=18, face = 'bold'), 
    plot.caption = element_text(size=12, color = 'darkgray', face = 'bold'),
    axis.text.x = element_text(size=15),
    axis.text.y = element_text(size=15),
    axis.title.x = element_text(size=18), 
    axis.title.y = element_text(size=18),
    legend.title = element_text(size=18),
    legend.text = element_text(size=16),
  )
# Analyze. The average casual users ride duration is higher than members' throughout a day. 
# Members average ride duration stays nearly the same (less than 15 minutes) during all day.
# Average casuals ride duration peaks (more than 30 minutes) is nearly 11AM-3PM.
# So, best time for ads is between 11 and 18.  


# t_f_v2 %>% 
#   group_by(member_casual) %>% 
#   summarise(sum_ride_duration = sum(ride_length)/60/60)


## Most popular stations
t_f_v2 %>% 
  group_by(start_station_name, member_casual) %>% 
  summarise(num_of_usage = n(), .groups = 'drop') %>%
  filter(start_station_name != "") %>%
  arrange(-num_of_usage) %>% 
  head(n=10)
# Analyze. Top 5 most popular stations are: 1) "Streeter Dr & Grand Ave"; 2) "Millennium Park"; 3) "Michigan Ave & Oak St";
# 4) "Clark St & Elm St"; 5) "Lake Shore Dr & Monroe St". We can use them for geo targeting our ad campaign.

# Casuals
casuals_top_stations <- t_f_v2 %>% 
  group_by(start_station_name, member_casual) %>% 
  summarise(num_of_usage = n(), .groups = 'drop') %>%
  filter(start_station_name != "") %>%
  filter(member_casual == "casual") %>% 
  arrange(-num_of_usage) %>% 
  head(n=20)

# Members
members_top_stations <- t_f_v2 %>% 
  group_by(start_station_name, member_casual) %>% 
  summarise(num_of_usage = n(), .groups = 'drop') %>%
  filter(start_station_name != "") %>%
  filter(member_casual == "member") %>% 
  arrange(-num_of_usage) %>% 
  head(n=20)

# Visualize most popular stations
# Casuals
t_f_v2 %>% 
  group_by(start_station_name, member_casual) %>% 
  summarise(num_of_usage = n(), .groups = 'drop') %>%
  filter(start_station_name != "") %>%
  filter(member_casual == "casual") %>% 
  arrange(-num_of_usage) %>% 
  head(n=10) %>% 
  ggplot() + 
  geom_col(aes(x = reorder(start_station_name, num_of_usage), y = num_of_usage), fill = "purple") + 
  labs(title = "Top 10 Used Stations by Casuals", y = "Number of Rides", x = "", 
       caption = "source: Motivate International Inc.") + 
  coord_flip() + 
  scale_y_continuous(labels = comma) + 
  theme(
    plot.title = element_text(size=18, face = 'bold'), 
    plot.caption = element_text(size=12, color = 'darkgray', face = 'bold'),
    axis.text.x = element_text(size=15),
    axis.text.y = element_text(size=15),
    axis.title.x = element_text(size=18), 
    axis.title.y = element_text(size=18),
    legend.title = element_text(size=18),
    legend.text = element_text(size=16),
  )

# Members
t_f_v2 %>% 
  group_by(start_station_name, member_casual) %>% 
  summarise(num_of_usage = n(), .groups = 'drop') %>%
  filter(start_station_name != "") %>%
  filter(member_casual == "member") %>% 
  arrange(-num_of_usage) %>% 
  head(n=10) %>% 
  ggplot() + 
  geom_col(aes(x = reorder(start_station_name, num_of_usage), y = num_of_usage), fill = "darkgreen") + 
  labs(title = "Top 10 Used Stations by Members", y = "Number of Rides", x = "", 
       caption = "source: Motivate International Inc.") + 
  coord_flip() + 
  scale_y_continuous(labels = comma) + 
  theme(
    plot.title = element_text(size=18, face = 'bold'), 
    plot.caption = element_text(size=12, color = 'darkgray', face = 'bold'),
    axis.text.x = element_text(size=15),
    axis.text.y = element_text(size=15),
    axis.title.x = element_text(size=18), 
    axis.title.y = element_text(size=18),
    legend.title = element_text(size=18),
    legend.text = element_text(size=16),
  )

## Different types of bike usage
t_f_v2 %>% 
  group_by(rideable_type, member_casual) %>% 
  summarise(num_of_usages = n())

# Visualize bike type usage 
t_f_v2 %>% 
  group_by(rideable_type, member_casual) %>% 
  summarise(num_of_usages = n()) %>% 
  ggplot(aes(x=member_casual, y=num_of_usages, fill = rideable_type)) + 
  geom_col(position = "dodge") + 
  labs(x = "Rider Type", y = "Number of Usages",
       title = "Bike Type Usage: Members vs. Casual Riders", 
       caption = "source: Motivate International Inc.") + 
  scale_y_continuous(labels = comma) + 
  theme(
    plot.title = element_text(size=18, face = 'bold'), 
    plot.caption = element_text(size=12, color = 'darkgray', face = 'bold'),
    axis.text.x = element_text(size=15),
    axis.text.y = element_text(size=15),
    axis.title.x = element_text(size=18), 
    axis.title.y = element_text(size=18),
    legend.title = element_text(size=18),
    legend.text = element_text(size=16),
  ) + 
  geom_text(aes(x = member_casual, y = num_of_usages, label = comma(num_of_usages), group = rideable_type), 
            position = position_dodge(width = 1), vjust = 1, size = 5)
# Analyze. Both type of users prefer classic bikes.

# Visualize bike type usage throughout a week
# t_f_v2 %>% 
#   group_by(rideable_type, member_casual, day_of_week) %>% 
#   summarise(num_of_usages = n())

t_f_v2 %>% 
  group_by(rideable_type, member_casual, day_of_week) %>% 
  summarise(num_of_usages = n()) %>% 
  ggplot(aes(x=day_of_week, y=num_of_usages, fill = rideable_type)) + 
  geom_col(position = "dodge") + 
  facet_wrap(~member_casual) +
  labs(x = "Day of Week", y = "Number of Usages",
       title = "Bike Type Usage during a Week: Members vs. Casual Riders", 
       caption = "source: Motivate International Inc.") + 
  scale_y_continuous(labels = comma) + 
  theme(axis.text.x = element_text(angle=45)) + 
  theme(
    plot.title = element_text(size=18, face = 'bold'), 
    plot.caption = element_text(size=12, color = 'darkgray', face = 'bold'),
    axis.text.x = element_text(size=15),
    axis.text.y = element_text(size=15),
    axis.title.x = element_text(size=18), 
    axis.title.y = element_text(size=18),
    legend.title = element_text(size=18),
    legend.text = element_text(size=16),
  ) + 
  geom_text(aes(x = member_casual, y = num_of_usages, label = comma(num_of_usages), group = rideable_type), 
            position = position_dodge(width = 1), vjust = 1, size = 5)
# Analyze. We can see that casuals tend to use more electric bikes on weekends
# that supports our hypothesis that they use Cyclistics for leisure longer trips.

# Ride duration by type of bike
t_f_v2 %>% 
  group_by(rideable_type, member_casual) %>% 
  summarise(avg_time_spent = mean(ride_length)/60) %>% 
  ggplot(aes(x=rideable_type, y=avg_time_spent, fill = rideable_type)) + 
  geom_col(position = "dodge") + 
  facet_wrap(~member_casual) +
  labs(x = "Bike Type", y = "Ride Duration (mins)",
       title = "Ride Duration by type of bike: Members vs. Casual Riders", 
       caption = "source: Motivate International Inc.") + 
  scale_y_continuous(labels = comma) + 
  theme(axis.text.x = element_text(angle=45)) + 
  theme(
    plot.title = element_text(size=18, face = 'bold'), 
    plot.caption = element_text(size=12, color = 'darkgray', face = 'bold'),
    axis.text.x = element_text(size=15),
    axis.text.y = element_text(size=15),
    axis.title.x = element_text(size=18), 
    axis.title.y = element_text(size=18),
    legend.title = element_text(size=18),
    legend.text = element_text(size=16),
    strip.text.x = element_text(size=15),
    strip.text.y = element_text(size=15)
  ) + 
  geom_text(aes(x = rideable_type, y = avg_time_spent, label = comma(avg_time_spent), group = rideable_type), 
            position = position_dodge(width = 1), vjust = 1, size = 5)

# By month
t_f_v2 %>% 
  group_by(rideable_type, member_casual, month) %>% 
  summarise(num_of_usages = n()) %>% 
  ggplot(aes(x=month, y=num_of_usages, fill = rideable_type)) + 
  geom_col(position = "dodge") + 
  facet_wrap(~member_casual) +
  labs(x = "Month", y = "Number of Usages",
       title = "Bike Type Usage during a Year: Members vs. Casual Riders", 
       caption = "source: Motivate International Inc.") + 
  scale_y_continuous(labels = comma) + 
  theme(axis.text.x = element_text(angle=45))
# Analyze. There are no data for classic bikes in october and november. So, maybe there is a problem with correctness of 
# this months' data. We need to ask our supervisor.


## Analyze. Final conclusions.
# 1. How do annual members and casual riders use Cyclistic bikes differently?
# 2. Why would casual riders buy Cyclistic annual memberships?
# 3. How can Cyclistic use digital media to influence casual riders to become members?

# Three Recommendations
# 1. Form partnerships with local businesses around (within 1km from) top 20 used stations for casual riders 
# (especially top 10 used start stations) to carry out marketing campaigns on digital media, targeting 1) local 
# casual riders, 2) frequent visitors (commuters) to the businesses and stores around, 3) and or those who have 
# similar social persona to Cyclistic members.
# 2. Offer occasional membership discount to new riders on summer and holiday weekends.
# 3. Develop longer term marketing campaigns and digital media marketing message that focusing on communicating 
# the benefits (price, discount, convenience) of riding with Cyclistic on trips longer than 20 minutes.

# Considering a tendency of casual riders to use Cyclistics on weekends we can implement a new type of less
# expensive membership: for those who will use bikes only (or mostly) on weekends.
# Because of casual users tend to ride longer trips, we might suggest some bonuses for trips longer than 30 minutes.


## Also, I'll do some geo analysis in Tableau later. 
# Export our cleaned dataset as a .csv file for future analysis in Tableau
write.csv(t_f_v2,"D:\\BSNS\\STUDY\\Coursera\\Google Data Analyze Certificate\\Part 8 - Case Study\\CS1_Cyclistics\\data\\cleaned_trips_data.csv")

# Export only needed data to ease my work
t_f_v2 %>% 
  select(-ride_id, -start_station_id, -end_station_id, -start_hour, -end_hour)

write.csv(t_f_v2,"D:\\BSNS\\STUDY\\Coursera\\Google Data Analyze Certificate\\Part 8 - Case Study\\CS1_Cyclistics\\data\\cleaned_trips_data_v2.csv")

