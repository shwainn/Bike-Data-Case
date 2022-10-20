install.packages("tidyverse")
library(tidyverse)
library(tidyr)

bike.data <- bikedata.csv
head(bike.data)

# altering columns
bike.alter <- bike.data %>%
  select(-ride_id)

bike.alter <- drop_na(bike.alter)
View(bike.alter)

# see if there are any observations where member_casual is na
ggplot(bike.alter, aes(x=member_casual))+geom_bar()

# see if any bike rides are span different days
separate(bike.alter, col=started_at, into=c('start_day', 'start_time'), sep=' ')

head(bike.alter)

# TODO: separate doesn't work; why?
# TODO: what data do I want to keep? Does it matter if some values are NA?