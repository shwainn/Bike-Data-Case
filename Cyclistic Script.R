install.packages("tidyverse")
library(tidyverse)
library(tidyr)
library(readxl)
library(lubridate)
library(rstudioapi)
library(ggplot2)

current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path ))
getwd() # confirm working directory is location of script
bike.data <- read_excel("bikedata.xlsx")
head(bike.data)

bike.data$rideable_type <- as.factor(bike.data$rideable_type)
bike.data$member_casual <- as.factor(bike.data$member_casual)
str(bike.data)

# altering columns
bike.data <- bike.data %>%
  select(-ride_id)

# remove observations where ended_at is more than 48 hours after start_at
# likely indicates user forgot to rerack bike
# remove 9 data points where ended_at is before started_at
# and remove 63 data points where the times are the same
# indicates some error or no ride, should be removed
filtered_bike.data <- bike.data %>%
  filter(ended_at <= started_at + days(2)) %>%
  arrange(ended_at - started_at)
filtered_bike.data <- filtered_bike.data %>%
  filter(ended_at > started_at) %>%
  arrange(ended_at - started_at)
filtered_bike.data <- filtered_bike.data %>%
  filter(ended_at >= started_at + seconds(60)) %>%
  arrange(ended_at - started_at)
View(filtered_bike.data)

# add a column for length of bike ride
filtered_bike.data <- filtered_bike.data %>%
  mutate(ride_length = ended_at - started_at)

# add a column for estimating ride displacement using Haversine formula
# haversine function
haversine <- function(lat1, lat2, long1, long2){
  p <- pi / 180
  a <- 0.5 - cos((lat2 - lat1) * p) / 2 +
      cos(lat1 * p) * cos(lat2 * p) * 
      (1 - cos((long2 - long1) * p)) / 2
  
  return(2 * 6371 * asin(sqrt(a)))
}

filtered_bike.data <- filtered_bike.data %>%
  mutate(displacement_km = haversine(start_lat, end_lat, start_lng, end_lng)) %>%
  arrange(desc(displacement_km))

ggplot(data=filtered_bike.data)+geom_point(mapping=aes(x=displacement_km, y=ride_length, color=member_casual))
  
# TODO: what data do I want to keep? Does it matter if some values are NA?
# TODO: what's up with the rides that are super long?