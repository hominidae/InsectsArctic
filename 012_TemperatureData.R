# Read climate data from Cambridge Bay
# Install the weathercan package if necessary, which allows for easy downloads of Environment and Climate Change Canada data
#install.packages("weathercan", 
#                 repos = c("https://ropensci.r-universe.dev", 
#                           "https://cloud.r-project.org"))

# Load library
library(tidyverse)
library(ggplot2)
library(weathercan)
library(lubridate)

# Load data that we can already easily access
cbay_weatherold <- read_csv("data/en_climate_monthly_NU_2400600_1929-2015_P1M.csv")

# Get a list of the weather data that we're interested in
stations_search(coords = c(69, -105), dist = 20, interval = "day")

# Bummer. It appears we can get a monthly summary from 1929 to 2015.
# However, to access 2015 to 2023 data we will need to combine those.

# Let's download the data and add it to a data frame
# Note: I've commented this out, simply use the station search above to select your location ID and download your selected data using the command below
#cbay_weathernew <- weather_dl(station_ids = 53512, start = "2015-03-01", end = "2023-01-01")

# save it so we don't stress Environment and Climate Change Canada's servers
#write_csv(x = cbay_weathernew, "data/en_climate_daily_NU_2015-2023.csv")

# Read in case we need it back
cbay_weathernew <- read_csv("data/en_climate_daily_NU_2015-2023.csv")

# We need to simplify. Every temperature reading on March 1st. We need the mean maximum and the mean minimum
cbay_new <- cbay_weathernew %>%
  select(date,time,temp) %>%
  group_by(date) %>%
  summarise(maxtemp = max(temp),mintemp = min(temp))

# Make a copy of the old data
cbay_copy <- cbay_weatherold %>%
  select("Date/Time", "Mean Max Temp (°C)", "Mean Min Temp (°C)")

# Change names so they're readable
names(cbay_copy) <- c("date","maxtemp","mintemp")

# Remove NA's because apparently 1929 was inconsistent AF. Up until 1950 at least.
# As it turns out, having someone around to record the weather was important.
cbay_copy <- cbay_copy %>%
  drop_na(maxtemp)
cbay_copy <- cbay_copy %>%
  drop_na(mintemp)

# Let's do some data preparation first though.
cbay_dates <- cbay_copy$date
cbay_dates <- ym(cbay_dates)

# Re-combine with cbay_copy
cbay_copy <- data.frame(
  date = cbay_dates,
  maxtemp = cbay_copy$maxtemp,
  mintemp = cbay_copy$mintemp
)

# Before we join the new data and the old data, we want to average the data out even further by month instead of days.
# Create a new column in cbay_new_compare from cbay_new
cbay_new_compare <- cbay_new %>%
  mutate(
    date_new = floor_date(cbay_new$date, "month"),
    maxtemp = cbay_new$maxtemp,
    mintemp = cbay_new$mintemp
  )

# Before we go on, remove some NA's
cbay_new_compare <- cbay_new_compare %>%
  drop_na(mintemp,maxtemp)

# Try this way
cbay_new_test <- cbay_new_compare %>%
  group_by(date_new) %>%
  summarise(across(c(mintemp,maxtemp), mean)) %>%
  mutate(
    mintemp = round(mintemp, digits = 1),
    maxtemp = round(maxtemp, digits = 1)
  )

# Change from date_new to date
names(cbay_new_test) <- c("date","mintemp","maxtemp")

# However, before we move on, we need to convert to year month again.
cbay_new_dates <- cbay_new_test$date

# 
cbay_new_test <- data.frame(
  date = cbay_new_dates,
  maxtemp = cbay_new_test$maxtemp,
  mintemp = cbay_new_test$mintemp
)

# Alright, now we need to join them with our new data. 
cbay_complete <- rbind(cbay_copy,cbay_new_test)

# Some garbage collection
rm(cbay_new,cbay_copy,cbay_new_compare,cbay_weathernew,cbay_weatherold,cbay_new_test)
rm(cbay_dates,cbay_new_dates)

# Let's split cbay from complete data starting in 1949
cbay <- cbay_complete %>%
  filter(date >= "1949-01-01" & date <= "2023-01-01")

# Let's do a test plot of all those temperature ranges from 1929 to 2023
ggplot(cbay_complete, aes(x = mintemp, y = maxtemp)) +
  geom_point()

# Very cool. Let's make another GGplot from 1949
ggplot(cbay, aes(x = date, y = maxtemp)) +
  geom_line()
