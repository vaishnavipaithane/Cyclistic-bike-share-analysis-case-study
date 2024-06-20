# install and load necessary packages

library(tidyverse)
library(ggplot2)
library(geosphere)

# import data in R studio

june23 <- read.csv("C:/Users/vaish/Desktop/Case Study 1/202306-divvy-tripdata.csv")
july23 <- read.csv("C:/Users/vaish/Desktop/Case Study 1/202307-divvy-tripdata.csv")
aug23 <- read.csv("C:/Users/vaish/Desktop/Case Study 1/202308-divvy-tripdata.csv")
sept23 <- read.csv("C:/Users/vaish/Desktop/Case Study 1/202309-divvy-tripdata.csv")
oct23 <- read.csv("C:/Users/vaish/Desktop/Case Study 1/202310-divvy-tripdata.csv")
nov23 <- read.csv("C:/Users/vaish/Desktop/Case Study 1/202311-divvy-tripdata.csv")
dec23 <- read.csv("C:/Users/vaish/Desktop/Case Study 1/202312-divvy-tripdata.csv")
jan24 <- read.csv("C:/Users/vaish/Desktop/Case Study 1/202401-divvy-tripdata.csv")
feb24 <- read.csv("C:/Users/vaish/Desktop/Case Study 1/202402-divvy-tripdata.csv")
mar24 <- read.csv("C:/Users/vaish/Desktop/Case Study 1/202403-divvy-tripdata.csv")
apr24 <- read.csv("C:/Users/vaish/Desktop/Case Study 1/202404-divvy-tripdata.csv")
may24 <- read.csv("C:/Users/vaish/Desktop/Case Study 1/202405-divvy-tripdata.csv")

# check data for consistency

colnames(june23)
colnames(july23)
colnames(aug23)
colnames(sept23)
colnames(oct23)
colnames(nov23)
colnames(dec23)
colnames(jan24)
colnames(feb24)
colnames(mar24)
colnames(apr24)
colnames(may24)

# check data structures

str(june23)
str(july23)
str(aug23)
str(sept23)
str(oct23)
str(nov23)
str(dec23)
str(jan24)
str(feb24)
str(mar24)
str(apr24)
str(may24)

# merge individual monthly data frames into one big data frame

tripdata <- bind_rows(june23, july23, aug23, sept23, oct23, nov23,
                      dec23, jan24, feb24, mar24, apr24, may24)

# remove the 12 individual dataframes from the environment to free up RAM

rm(june23, july23, aug23, sept23, oct23, nov23, dec23, jan24, feb24, mar24,
   apr24, may24)

# Inspect new data frame

colnames(tripdata) #List of column names
head(tripdata) #See the first 6 rows of data frame
tail(tripdata) #See the last 6 rows of data frame
str(tripdata) #See list of columns and data types (numeric, character, etc)
summary(tripdata) #Statistical summary of data. Mainly for numerics
nrow(tripdata) #number of rows in data frame
dim(tripdata) #dimensions of data frame

# convert data to POSIXct

tripdata$started_at <- as.POSIXct(tripdata$started_at)
tripdata$ended_at <- as.POSIXct(tripdata$ended_at)
str(tripdata$started_at)
str(tripdata$ended_at)

# Extract date components and calculate ride length and start time

tripdata <- tripdata %>%
  mutate(year = format(as.Date(started_at), "%Y")) %>% 
  mutate(month = format(as.Date(started_at), "%B")) %>% 
  mutate(date = format(as.Date(started_at), "%d")) %>%
  mutate(day_of_week = format(as.Date(started_at), "%A")) %>%
  mutate(ride_length = difftime(ended_at, started_at)) %>%
  mutate(start_time = strftime(started_at, "%H"))

# Convert ride length from Factor to numeric

tripdata <- tripdata %>% 
  mutate(ride_length = as.numeric(ride_length))
is.numeric(tripdata$ride_length)

# inspect new columns

str(tripdata)

# add ride distance in km

tripdata$ride_distance <- distGeo(matrix(c(tripdata$start_lng, tripdata$start_lat), ncol = 2), 
                                  matrix(c(tripdata$end_lng, tripdata$end_lat), ncol = 2))
tripdata$ride_distance <- tripdata$ride_distance/1000 #distance in km

# Remove "bad" data
# The data frame includes a few hundred entries when bikes were taken out of docks
# and checked for quality by Divvy where ride_length was negative or 'zero'
# created is new version of data frame since data is being removed

tripdata_clean <- tripdata[!(tripdata$start_station_name == "HQ QR"|tripdata$ride_length <= 0),]

write.csv(tripdata_clean, "tripdata_clean.csv",row.names = FALSE)

# check cleaned data frame

str(tripdata_clean)
summary(tripdata_clean)

# Conduct descriptive analysis

# Descriptive analysis on ride_length (all figures in seconds)
# mean = straight average (total ride length / total rides)
# median = midpoint number of ride length array
# max = longest ride
# min = shortest ride

tripdata_clean %>%
  summarise(average_ride_length = mean(ride_length),
            median_ride_length = median(ride_length),
            max_ride_length = max(ride_length),
            min_ride_length = min(ride_length))

## Compare members and casual users

# Comparison between Members and Causal riders depending on ride length

tripdata_clean %>%
  group_by(member_casual)%>%
  summarise(average_ride_length = mean(ride_length),
            median_ride_length = median(ride_length),
            max_ride_length = max(ride_length),
            min_ride_length = min(ride_length))

# Members vs casual riders difference depending on total rides taken

tripdata_clean %>%
  group_by(member_casual)%>%
  summarise(ride_count = length(ride_id),
            ride_percentage = length(ride_id)/nrow(tripdata_clean)*100)

ggplot(tripdata_clean, aes(x = member_casual, fill = member_casual)) +
  geom_bar() +
  labs(x="Casuals vs Members", y="Number Of Rides", title= "Total rides: Casuals vs Members") +
  annotate("text",x=1,y=2000000,label="35.7%",color="black",size=3.0) +
  annotate("text",x=2,y=3000000,label="64.3%",color="black",size=3.0)

# Analysis and visualization of Rideable type Vs. total rides by Members and casual riders

tripdata_clean %>%
  group_by(member_casual, rideable_type) %>%
  summarise(ride_count = length(ride_id))

ggplot(tripdata_clean, aes(x = member_casual, fill = rideable_type)) +
  geom_bar() +
  labs(x="Rideable Type", y="Number Of Rides", title= "Rideable type Vs. total rides by Members and casual riders")

              
# See the total rides and average ride time by each day for members vs casual riders

# Fix days of the week in order

tripdata_clean$day_of_week <- ordered(tripdata_clean$day_of_week, 
                                      levels=c("Sunday", "Monday", "Tuesday", "Wednesday", 
                                               "Thursday", "Friday", "Saturday"))

tripdata_clean %>%
  group_by(member_casual, day_of_week) %>%
  summarise(number_of_rides = n(),
            average_ride_length = mean(ride_length), .groups="drop") %>%
  arrange(member_casual, day_of_week)

# visualize total rides data by type and day of week

tripdata_clean %>%
  group_by(member_casual, day_of_week) %>%
  summarise(number_of_rides = n(), .groups="drop") %>%
  arrange(member_casual, day_of_week) %>%
  ggplot(aes(x = day_of_week, y = number_of_rides, fill = member_casual)) +
  labs(x="Day of Week", y="Number of Rides", title ="Total rides by Members and Casual riders Vs. Day of the week") +
  geom_col(width=0.5, position = position_dodge(width=0.5)) +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))

# visualize average ride time data by type and day of week

tripdata_clean %>%
  group_by(member_casual, day_of_week) %>%
  summarise(average_ride_length = mean(ride_length), .groups="drop") %>%
  arrange(member_casual, day_of_week) %>%
  ggplot(aes(x = day_of_week, y = average_ride_length, fill = member_casual)) +
  labs(x="Day of Week", y="Ride time in Seconds", title ="Average ride time by Members and Casual riders Vs. Day of the week") +
  geom_col(width=0.5, position = position_dodge(width=0.5))

# See the total rides and average ride time by each month for members vs casual riders

# Fix months in order

tripdata_clean$month <- ordered(tripdata_clean$month,
                                levels=c("January", "February", "March", "April", 
                                         "May", "June", "July", "August", 
                                         "September", "October", "November", "December"))

tripdata_clean %>%
  group_by(member_casual, month) %>%
  summarise(number_of_rides = n(),
            average_ride_length = mean(ride_length), .groups="drop") %>%
  arrange(member_casual, month)

# visualize total rides data by type and month

tripdata_clean %>%  
  group_by(member_casual, month) %>% 
  summarise(number_of_rides = n(),.groups="drop") %>% 
  arrange(member_casual, month)  %>% 
  ggplot(aes(x = month, y = number_of_rides, fill = member_casual)) +
  labs(title ="Total rides by Members and Casual riders Vs. Month", x = "Month", y= "Number Of Rides") +
  theme(axis.text.x = element_text(angle = 30)) +
  geom_col(width=0.5, position = position_dodge(width=0.5)) +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))


# visualize average ride time data by type and month

tripdata_clean %>%  
  group_by(member_casual, month) %>% 
  summarise(average_ride_length = mean(ride_length), .groups="drop") %>% 
  arrange(member_casual, month)  %>% 
  ggplot(aes(x = month, y = average_ride_length, fill = member_casual)) +
  labs(title ="Average ride time by Members and Casual riders Vs. Month",x = "Month", y= "Ride time in seconds" ) +
  theme(axis.text.x = element_text(angle = 30)) +
  geom_col(width=0.5, position = position_dodge(width=0.5))

# Comparison between Members and Casual riders depending on ride distance

tripdata_clean %>% 
  group_by(member_casual) %>% 
  drop_na() %>%
  summarise(average_ride_distance = mean(ride_distance)) %>%
  ggplot() + 
  geom_col(mapping= aes(x= member_casual,y= average_ride_distance,fill=member_casual), show.legend = FALSE)+
  labs(title = "Mean travel distance by Members and Casual riders", x="Member and Casual riders", y="Average distance In Km")


# Analysis and visualization on cyclist's bike demand by hour in a day

tripdata_clean %>%
  ggplot(aes(start_time, fill= member_casual)) +
  labs(x="Hour of the day",y="Number of Rides",title="Cyclistic's Bike demand by hour in a day") +
  geom_bar()
  

# Analysis and visualization on cyclistic's bike demand per hour by day of the week

tripdata_clean %>%
  ggplot(aes(start_time, fill=member_casual)) +
  geom_bar() +
  labs(x="Hour of the day",y="Number of Rides",title="Cyclistic's bike demand per hour by day of the week") +
  facet_wrap(~ day_of_week)
  



  














