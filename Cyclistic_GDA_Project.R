### Cyclistic_Full_Year_Analysis ###

# This analysis is based on the Divvy case study "'Sophisticated, Clear, and Polished’: Divvy and Data Visualization" written by Kevin Hartman (found here: https://artscience.blog/home/divvy-dataviz-case-study). 
#The purpose of this script is to consolidate downloaded Divvy data into a single dataframe and then conduct simple analysis to help answer the key question: “In what ways do members and casual riders use Divvy bikes differently?”

# # # # # # # # # # # # # # # # # # # # # # # 
# Install required packages
# tidyverse for data import and wrangling
# lubridate for date functions
# ggplot for visualization
# # # # # # # # # # # # # # # # # # # # # # #  
library(tidyverse)  #helps wrangle data
library(lubridate)  #helps wrangle date attributes
library(ggplot2)  #helps visualize data
library(geosphere) #helps manage distance, direction, area, etc for geographic coordinates
getwd() #displays your working directory
setwd("~/Projects/cyclistic-case-study") #sets your working directory to simplify calls to data

#=====================
# STEP 1: COLLECT DATA
#=====================
# Upload datasets (csv files) here
cyclistic_df1 <- read_csv("202109-divvy-tripdata.csv")
cyclistic_df2 <- read_csv("202110-divvy-tripdata.csv")
cyclistic_df3 <- read_csv("202111-divvy-tripdata.csv")
cyclistic_df4 <- read_csv("202112-divvy-tripdata.csv")
cyclistic_df5 <- read_csv("202201-divvy-tripdata.csv")
cyclistic_df6 <- read_csv("202202-divvy-tripdata.csv")
cyclistic_df7 <- read_csv("202203-divvy-tripdata.csv")
cyclistic_df8 <- read_csv("202204-divvy-tripdata.csv")
cyclistic_df9 <- read_csv("202205-divvy-tripdata.csv")
cyclistic_df10 <- read_csv("202206-divvy-tripdata.csv")
cyclistic_df11 <- read_csv("202207-divvy-tripdata.csv")
cyclistic_df12 <- read_csv("202208-divvy-tripdata.csv")

#====================================================
# STEP 2: WRANGLE DATA AND COMBINE INTO A SINGLE FILE
#====================================================
# Compare column names each of the files
# While the names don't have to be in the same order, they DO need to match perfectly before we can use a command to join them into one file
colnames(cyclistic_df1)
colnames(cyclistic_df2)
colnames(cyclistic_df3)
colnames(cyclistic_df4)
colnames(cyclistic_df5)
colnames(cyclistic_df6)
colnames(cyclistic_df7)
colnames(cyclistic_df8)
colnames(cyclistic_df9)
colnames(cyclistic_df10)
colnames(cyclistic_df11)
colnames(cyclistic_df12)

# There's no need to rename columns, all columns in the data frames are consistent

# Inspect the dataframes and look for inconsistencies
str(cyclistic_df1)
str(cyclistic_df2)
str(cyclistic_df3)
str(cyclistic_df4)
str(cyclistic_df5)
str(cyclistic_df6)
str(cyclistic_df7)
str(cyclistic_df8)
str(cyclistic_df9)
str(cyclistic_df10)
str(cyclistic_df11)
str(cyclistic_df12)


# Stack individual monthly's data frames into one big data frame
## The data frame contains 5,883,043
cyclistic_df <- rbind(cyclistic_df1, cyclistic_df2, cyclistic_df3, cyclistic_df4, cyclistic_df5,
                      cyclistic_df6, cyclistic_df7, cyclistic_df8, cyclistic_df9, cyclistic_df10,
                      cyclistic_df11, cyclistic_df12)

nrow(cyclistic_df)

# Remove NA data
cyclistic_df_v1 <- drop_na(cyclistic_df)

#======================================================
# STEP 3: CLEAN UP AND ADD DATA TO PREPARE FOR ANALYSIS
#======================================================
# Inspect the new table that has been created
colnames(cyclistic_df_v1)  #List of column names
nrow(cyclistic_df_v1)  #How many rows are in data frame? 4,560,146 left after removing empty data
dim(cyclistic_df_v1)  #Dimensions of the data frame?
head(cyclistic_df_v1)  #See the first 6 rows of data frame.  Also tail(all_trips)
str(cyclistic_df_v1)  #See list of columns and data types (numeric, character, etc)
summary(cyclistic_df_v1)  #Statistical summary of data. Mainly for numerics

# There are a few problems we will need to fix:
# (1) I'll add additional columns of data -- such as day, month, year -- that provide additional opportunities to aggregate the data.
# (2) I'll add a calculated column for length of ride called("ride_length") to the entire dataframe for consistency.
# (3) There are some rides where tripduration shows up as negative, including several hundred rides where the bikes were took out of circulation for Quality Control reasons. We will want to delete these rides.


# Add columns that list the date, month, day, and year of each ride
# This will allow us to aggregate ride data for each month, day, or year ... before completing these operations we could only aggregate at the ride level
# https://www.statmethods.net/input/dates.html more on date formats in R found at that link
cyclistic_df_v1$date <- as.Date(cyclistic_df_v1$started_at) #The default format is yyyy-mm-dd
cyclistic_df_v1$month <- format(as.Date(cyclistic_df_v1$date), "%m")
cyclistic_df_v1$day <- format(as.Date(cyclistic_df_v1$date), "%d")
cyclistic_df_v1$year <- format(as.Date(cyclistic_df_v1$date), "%Y")
cyclistic_df_v1$day_of_week <- format(as.Date(cyclistic_df_v1$date), "%A")

# Add a "ride_length" calculation to cyclistic_df_v1 (in seconds)
# Add ride_distance using coordinates from start_lat to end_lat
# https://stat.ethz.ch/R-manual/R-devel/library/base/html/difftime.html
cyclistic_df_v1$ride_length <- difftime(cyclistic_df_v1$ended_at, cyclistic_df_v1$started_at)
cyclistic_df_v1$ride_distance <- distGeo(matrix(c(cyclistic_df_v1$start_lng, cyclistic_df_v1$start_lat), ncol = 2), 
                        matrix(c(cyclistic_df_v1$end_lng, cyclistic_df_v1$end_lat), ncol = 2))
cyclistic_df_v1$ride_speed <- c(cyclistic_df_v1$ride_distance)/as.numeric(c(cyclistic_df_v1$ride_length), units="hours")

cyclistic_df_v1$ride_distance <- cyclistic_df_v1$ride_distance/1000

# Inspect the structure of the columns
str(cyclistic_df_v1)

# Convert "ride_length" from Factor to numeric so we can run calculations on the data
is.factor(cyclistic_df_v1$ride_length)
cyclistic_df_v1$ride_length <- as.numeric(as.character(cyclistic_df_v1$ride_length))
is.numeric(cyclistic_df_v1$ride_length)

# Remove "bad" data
# The dataframe includes a few hundred entries when bikes were taken out of docks and checked for quality or ride_length was negative
# I will create a new version of the dataframe (v2) since data is being removed
# https://www.datasciencemadesimple.com/delete-or-drop-rows-in-r-with-conditions-2/
cyclistic_df_v2 <- cyclistic_df_v1[!(cyclistic_df_v1$start_station_name == "HQ QR" | cyclistic_df_v1$ride_length <=0),]

dim(cyclistic_df_v2) # 4559855 rows left after removing negative ride_length values

#=====================================
# STEP 4: CONDUCT DESCRIPTIVE ANALYSIS
#=====================================
# Descriptive analysis on ride_length (all figures in seconds)
mean(cyclistic_df_v2$ride_length) #straight average (total ride length / rides)
median(cyclistic_df_v2$ride_length) #midpoint number in the ascending array of ride lengths
max(cyclistic_df_v2$ride_length) #longest ride
min(cyclistic_df_v2$ride_length) #shortest ride

# You can condense the four lines above to one line using summary() on the specific attribute
summary(cyclistic_df_v2$ride_length)

# Compare members and casual users
aggregate(cyclistic_df_v2$ride_length ~ cyclistic_df_v2$member_casual, FUN = mean)
aggregate(cyclistic_df_v2$ride_length ~ cyclistic_df_v2$member_casual, FUN = median)
aggregate(cyclistic_df_v2$ride_length ~ cyclistic_df_v2$member_casual, FUN = min)

# See the average ride time by each day for members vs casual users
aggregate(cyclistic_df_v2$ride_length ~ cyclistic_df_v2$member_casual + cyclistic_df_v2$day_of_week, FUN = mean)

# Notice that the days of the week are out of order. Let's fix that.
cyclistic_df_v2$day_of_week <- ordered(cyclistic_df_v2$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

# Now, let's run the average ride time by each day for members vs casual users
aggregate(cyclistic_df_v2$ride_length ~ cyclistic_df_v2$member_casual + cyclistic_df_v2$day_of_week, FUN = mean)

# analyze ridership data by type and weekday
cyclistic_df_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>%  #creates weekday field using wday()
  group_by(member_casual, weekday) %>%  #groups by usertype and weekday
  summarise(number_of_rides = n()							#calculates the number of rides and average duration 
            ,average_duration = mean(ride_length)) %>% 		# calculates the average duration
  arrange(member_casual, weekday)								# sorts


#Summary 
getmode <- function(day_of_week) {
  uniqv <- unique(day_of_week)
  uniqv[which.max(tabulate(match(day_of_week, uniqv)))]
}
clean_cyclistic_df_summary <- 
  cyclistic_df_v2 %>%
  group_by(member_casual) %>% 
  summarise(average_ride_length = mean(ride_length),
            min_ride_length = min(ride_length),
            max_ride_length = max(ride_length),
            average_ride_distance = mean(ride_distance),
            min_ride_distance = min(ride_distance),
            max_ride_distance = max(ride_distance),
            average_ride_speed = mean(ride_speed),
            min_ride_speed = min(ride_speed),
            max_ride_speed = max(ride_speed),
            mode_dow = getmode(day_of_week))

clean_cyclistic_df_summary


# Let's visualize the number of rides by rider type
cyclistic_df_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge") + 
  labs(title="Cyclistic: Number of Rides", 
       subtitle = "Number of rides per type of user")

# Let's create a visualization for average duration
cyclistic_df_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n(),
            average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge") +
  labs(title="Cyclistic: Average duration per day", subtitle = "Average rides duration per type of user")

#=================================================
# STEP 5: EXPORT SUMMARY FILE FOR FURTHER ANALYSIS
#=================================================
# Create a csv file that we will visualize in Excel, Tableau, or my presentation software
counts <- aggregate(cyclistic_df_v2$ride_length ~ cyclistic_df_v2$member_casual + cyclistic_df_v2$day_of_week, FUN = mean)
write.csv(counts, file = '~/Projects/cyclistic-case-study/avg_ride_length.csv')

