install.packages("geosphere")

# Install basic libraries
library(tidyverse)
library(janitor)
library(geosphere)
library(ggplot2)

# Loading dataset into data frames
cyclistic_df1 <- read_csv("~/Documents/CyclisticDatasets/Cyclistic/cyclistic-tripdata-202108.csv")
cyclistic_df2 <- read_csv("~/Documents/CyclisticDatasets/Cyclistic/cyclistic-tripdata-202109.csv")
cyclistic_df3 <- read_csv("~/Documents/CyclisticDatasets/Cyclistic/cyclistic-tripdata-202110.csv")
cyclistic_df4 <- read_csv("~/Documents/CyclisticDatasets/Cyclistic/cyclistic-tripdata-202111.csv")
cyclistic_df5 <- read_csv("~/Documents/CyclisticDatasets/Cyclistic/cyclistic-tripdata-202112.csv")
cyclistic_df6 <- read_csv("~/Documents/CyclisticDatasets/Cyclistic/cyclistic-tripdata-202201.csv")
cyclistic_df7 <- read_csv("~/Documents/CyclisticDatasets/Cyclistic/cyclistic-tripdata-202202.csv")
cyclistic_df8 <- read_csv("~/Documents/CyclisticDatasets/Cyclistic/cyclistic-tripdata-202203.csv")
cyclistic_df9 <- read_csv("~/Documents/CyclisticDatasets/Cyclistic/cyclistic-tripdata-202204.csv")
cyclistic_df10 <- read_csv("~/Documents/CyclisticDatasets/Cyclistic/cyclistic-tripdata-202205.csv")
cyclistic_df11 <- read_csv("~/Documents/CyclisticDatasets/Cyclistic/cyclistic-tripdata-202206.csv")
cyclistic_df12 <- read_csv("~/Documents/CyclisticDatasets/Cyclistic/cyclistic-tripdata-202207.csv")
cyclistic_df13 <- read_csv("~/Documents/CyclisticDatasets/Cyclistic/cyclistic-tripdata-202208.csv")



# Create unique data set that includes trip data of the last year from August 2021 to August 2022

## The data frame contains 6687395
cyclistic_df <- rbind(cyclistic_df1, cyclistic_df2, cyclistic_df3, cyclistic_df4, cyclistic_df5,
                  cyclistic_df6, cyclistic_df7, cyclistic_df8, cyclistic_df9, cyclistic_df10,
                  cyclistic_df11, cyclistic_df12, cyclistic_df13)



# INSERT HERE COMMENTS FROM CYCLISTIC CASE STUDY.DOCX

dim(cyclistic_df)

head(cyclistic_df)

str(cyclistic_df)

glimpse(cyclistic_df)

colnames(cyclistic_df)

# END OF COMMENT No.1



# Cleaning the data frame
## Drop the NA, observations left after cleaning 5234555
cyclistic_df_v1 <- drop_na(cyclistic_df)
dim(cyclistic_df_v1)


# Changing started_at and ended_at format. Adding column ride_length, subtracting started_at from ended_at
## formatting data columns
data.frame(cyclistic_df_v1) %>%
  format(cyclistic_df_v1$started_at, format = '%d/%m/%Y:%H:%M:%S') %>% 
  format(cyclistic_df_v1$ended_at, format = '%d/%m/%Y:%H:%M:%S')

## Adding new columns ride_length, ride_distance, ride_speed, day_of_week
cyclistic_df_v2 <- data.frame(cyclistic_df_v1) %>% 
  mutate(ride_length = difftime(cyclistic_df_v1$ended_at, cyclistic_df_v1$started_at),
         ride_distance = distGeo(matrix(c(start_lng, start_lat), ncol = 2), 
                                  matrix(c(end_lng, end_lat), ncol = 2)),
         ride_speed = c(ride_distance)/as.numeric(c(ride_length), units="hours"),
         day_of_week = format(cyclistic_df_v1$ended_at, '%A')
         )


cyclistic_df_v2$ride_distance <- cyclistic_df_v2$ride_distance/1000

head(cyclistic_df_v2)
dim(cyclistic_df_v2)


### ride_length with negative values, meaning the ride has started after it ended which is not correct 323 rows
positive_ride_length <- cyclistic_df_v2 %>% count(ride_length <= 0)
positive_ride_length

### rows to be deleted from the data frame where ride_length >= 0
cyclistic_df_v2 %>% 
  select(started_at, ended_at, ride_length) %>%  
  filter(ride_length <= 0)%>% 
  arrange(desc(ride_length))


cyclistic_df_v3 <- cyclistic_df_v2 %>% filter(ride_length > 0)


### Check for inaccurate ride_length
cyclistic_df_v3 %>% summarise(min(ride_length))
dim(cyclistic_df_v3)


### Verify if ride_length was cleaned correctly, removed 323 rows, after removing there are 5234232 left
cyclistic_df_v3 %>% 
  select(started_at, ended_at, ride_length) %>%  
  filter(ride_length <= 0)%>% 
  arrange(desc(ride_length))

cyclistic_df_v3 %>% count(ride_length <= 0)


# Type of user
## Count members / casual
### members 3014905
members <- cyclistic_df_v3 %>% count(member_casual == "member")
members

### casual 2219327
casual <- cyclistic_df_v3 %>% count(member_casual =="casual")
casual


clean_cyclistic_df <- cyclistic_df_v3

### Unique values per column
print("Unique values per column")
clean_cyclistic_df %>% summarise_all(list(~n_distinct(.)))


### Check for min, max, mean ride_length and mode day_of_week
#### Create the function.
getmode <- function(day_of_week) {
  uniqv <- unique(day_of_week)
  uniqv[which.max(tabulate(match(day_of_week, uniqv)))]
}
clean_cyclistic_df_summary <- 
  clean_cyclistic_df %>%
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


# Visualization

ggplot(data=clean_cyclistic_df)+
  geom_bar(mapping = aes(x=member_casual, color = member_casual)) +
  facet_wrap(~rideable_type ) +
  labs(title="Cyclistic: Rides per type of User", 
       subtitle = "Number of rides per type of user")

