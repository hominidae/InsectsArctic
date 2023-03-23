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
# I downloaded this from the CSV file from the Environment and Climate Change Canada website.
cbay_weatherold <- read_csv("data/en_climate_monthly_CBAY_1929-2015.csv")

# Get a list of the weather data that we're interested in. Specifically Cambridge Bay.
stations_search(coords = c(69, -105), dist = 20, interval = "day")

# Bummer. It appears we can get a monthly summary from 1929 to 2015.
# However, to access 2015 to 2023 data we will need to combine those.

# Let's download the data and add it to a data frame
# Note: I've commented this out, simply use the station search above to select your location ID and download your selected data using the command below
cbay_weathernew <- weather_dl(station_ids = 53512, start = "2015-03-01", end = "2023-01-01")

# save it so we don't stress Environment and Climate Change Canada's servers
#write_csv(x = cbay_weathernew, "data/en_climate_daily_NU_2015-2023.csv")

# Read in case we need it back
#cbay_weathernew <- read_csv("data/en_climate_daily_CBAY_2015-2023.csv")

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

# Very cool. Let's make another GGplot from 1949
ggplot(cbay, aes(x = date, y = maxtemp)) +
  geom_line(color = "red") +
  geom_point(color = "red") +
  geom_line(aes(y = mintemp), color = "blue") +
  geom_line(aes(y = mintemp), color = "blue") +
  scale_x_date(breaks = as.Date(c("1950-01-01","1955-01-01","1960-01-01","1965-01-01","1970-01-01","1975-01-01","1980-01-01","1985-01-01",
                                  "1990-01-01","1995-01-01","2000-01-01","2005-01-01","2010-01-01","2015-01-01","2020-01-01","2023-01-01")),
               minor_breaks = as.Date(c("1949-01-01")),
               date_labels = "%Y") +
  scale_y_continuous(breaks = as.integer(c(-45,-40,-35,-30,-25,-20,-15,-10,-5,0,5,10,15,20,25,30,34,40))) +
  labs(x = "Date", y = "Temperature", title="Temperature Chart for Cambridge Bay")

# An interesting chart. We'll come back to it later. Let's see if we have weather data for the other three communities too.
# But if you're interested in watching an interesting video on what I'll be doing with this data.
# Here is a video from Dr Pat Schloss on reproducing a visualization of monthly temperature anomalies from NASA's Earth Observatory Earth Matters blog.
# https://www.youtube.com/watch?v=DrNQMaIVEVo
# In short, we don't necessarily want a chart showing the high's and low's. We want to visualize what the differences are over time.
# Before we move onto that however, let's see if we have data from the other communites. Starting with Kugluktuk

# Starting with Kugluktuk, which is positioned at 67.826667, -115.093333
stations_search(coords = c(67.82, -115.09), dist = 20, interval = "day")

# The data from Kugluktuk seems to have the same issue, these weather data span from 1977 to 2014.
# Note: Coppermine was the name of the community prior to it's change to Kugluktuk on Jan 1st 1996.
kugl_weatherold <- weather_dl(station_ids = 1641, start = "1977-01-01", end = "2014-12-31")

# Save it so we don't need to download it again
#write_csv(x = kugl_weatherold, "data/en_climate_daily_KUGL_1977-2014.csv")

# Let's download the new data too.
kugl_weathernew <- weather_dl(station_ids = 53335, start = "2014-01-01", end = "2023-01-01")

# Save it so we don't need to download it again
#write_csv(x = kugl_weathernew, "data/en_climate_daily_KUGL_2014-2023.csv")

# Next let's do the same for Gjoa Haven, which is at 68.625, -95.877778
stations_search(coords = c(68.625, -95.8777), dist = 20, interval = "day")

# Download that data
gjoa_weatherold <- weather_dl(station_ids = 1715, start = "1984-01-01", end = "2014-12-31")

# Save it so we don't need to download it again 
write_csv(x = gjoa_weatherold, "data/en_climate_daily_GJOA_1984-2014.csv")

# Same for the new data
gjoa_weathernew <- weather_dl(station_ids = 51079, start = "2014-01-01", end = "2023-01-01")

# Save it so we don't need to download it again 
write_csv(x = gjoa_weathernew, "data/en_climate_daily_GJOA_2014-2023.csv")

# Do Kugaaruk next, which is at 68.534722, -89.825
stations_search(coords = c(68.534, -89.825), dist = 20, interval = "day")

# Download the oldest data
kuga_weatherold <- weather_dl(station_ids = 1718, start = "1957-01-01", end = "1992-12-31")

# Save it so we don't need to download it again
write_csv(x = kuga_weatherold, "data/en_climate_daily_KUGA_1957-1992.csv")

# Download more data
kuga_weathernew <- weather_dl(station_ids = 10847, start = "1994-01-01", end = "2023-01-01")

# Save it so we don't need to download it again
write_csv(x = kuga_weathernew, "data/en_climate_daily_KUGA_1994-2023.csv")

