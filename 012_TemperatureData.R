# Read climate data from Cambridge Bay
# Install the weathercan package if necessary, which allows for easy downloads of Environment and Climate Change Canada data
#install.packages("weathercan", 
#                 repos = c("https://ropensci.r-universe.dev", 
#                           "https://cloud.r-project.org"))

### Load libraries ----------------------------------------------------------
library(tidyverse)
library(ggplot2)
library(weathercan)
library(lubridate)
library(gganimate)

# Download data for Cambridge bay ----------------------------------------------------------
# Get a list of the weather data that we're interested in.
# Specifically from Cambridge Bay, which is the nearest result from it's approximate GPS coordinates at 69.00, -105.00
stations_search(coords = c(69, -105), dist = 20, interval = "day")

# Firstly, notice two things. There is monthly data available from 1929 to 2015.
# But that there are no monthly averages produced after 2015.
# This means we'll need to do it ourselves.

# Let's download the old Cambridge Bay weather day in a monthly format
cbay_weatherold <- weather_dl(station_ids = 1786, start = "1953-01-01", end ="2015-12-31")

# We've got a problem. Looking at the data, we have it starting not in 1929 but in 1953.
# Let's save it for now, then download the earlier data too.
#write_csv(x = cbay_weatherold, "data/en_climate_daily_CBAY_1953-2015.csv")

# And reload it if necessary
cbay_weatherold <- read_csv("data/en_climate_daily_CBAY_1953-2015.csv")

# Let's repeat the search but select "day" and see if data exists for that period.
stations_search(coords = c(69, -105), dist = 20, interval = "month")

# Download the 1929 to 1953 data. It starts in 1953-01-01, so we need anything before that too.
cbay_weatherolder <- weather_dl(station_ids = 1786, start = "1949-01-01", end = "1952-12-31", interval = "month")
# Keep this in mind for later. In order to use the older data, we need to combine it with the newer data which was performed hourly from 1953.
# So, we'll get the mean minimum temp, mean maximum temp for all those data and combine them.

# save it so we don't stress Environment and Climate Change Canada's servers
#write_csv(x = cbay_weatherolder, "data/en_climate_monthly_CBAY_1949-1953.csv")

# And reload it if necessary
cbay_weatherolder <- read_csv("data/en_climate_monthly_CBAY_1949-1953.csv")

# Great! We have data that we can combine with our old data.
# Let's download the new weather data and add it to a data frame. Since the 1929 - 2015 data set ended on 2015-02-12, we'll need from that date onwards.
cbay_weathernew <- weather_dl(station_ids = 53512, start = "2015-02-13", end = "2023-03-01")

# save it so we don't stress Environment and Climate Change Canada's servers
#write_csv(x = cbay_weathernew, "data/en_climate_daily_CBAY_2015-2023.csv")

# And reload it if necessary
cbay_weathernew <- read_csv("data/en_climate_daily_CBAY_2015-2023.csv")

# Download data for Kugluktuk ----------------------------------------------------------
# Starting with Kugluktuk, which is positioned at 67.826667, -115.093333
stations_search(coords = c(67.82, -115.09), dist = 20, interval = "day")

# The data from Kugluktuk seems to have the same issue.
# Note: Coppermine was the name of the community prior to it's change to Kugluktuk on Jan 1st 1996.
# So we have monthly data available from 1930 to 1977. Let's download that.
kugl_weatherolder <- weather_dl(station_ids = 1640, start = "1930-01-01", end = "1977-12-31")

# Let's save it so we can use it later.
#write_csv(x = kugl_weatherolder, "data/en_climate_daily_KUGL_1930-1977.csv")

# And reload it if necessary
kugl_weatherolder <- read_csv("data/en_climate_daily_KUGL_1930-1977.csv")

# We also have data from the end of 1977 to the end of 2014.
kugl_weatherold <- weather_dl(station_ids = 1641, start ="1978-01-01", end = "2014-12-31")

# Save it so we don't need to download it again
#write_csv(x = kugl_weatherold, "data/en_climate_daily_KUGL_1978-2014.csv")

# And reload it if necessary
kugl_weatherold <- read_csv("data/en_climate_daily_KUGL_1978-2014.csv")

# Let's download the new data too.
kugl_weathernew <- weather_dl(station_ids = 53335, start = "2014-12-05", end = "2023-03-01")

# Save it so we don't need to download it again
#write_csv(x = kugl_weathernew, "data/en_climate_daily_KUGL_2015-2023.csv")

# And reload it if necessary
kugl_weathernew <- read_csv("data/en_climate_daily_KUGL_2015-2023.csv")

# Download data for Gjoa Haven ----------------------------------------------------------
# Next let's do the same for Gjoa Haven, which is at 68.625, -95.877778
stations_search(coords = c(68.625, -95.8777), dist = 20, interval = "month")

# Interesting. We have monthly weather from 1984 to 2007. Then nothing afterwards.
# That isn't very useful. Let's look at daily instead.
stations_search(coords = c(68.625, -95.8777), dist = 20, interval = "day")

# Great. We have more complete data here.
# Download that data from 1984 to 2014 first.
gjoa_weatherold <- weather_dl(station_ids = 1715, start = "1984-01-01", end = "2013-01-10")

# Save it so we don't need to download it again 
#write_csv(x = gjoa_weatherold, "data/en_climate_daily_GJOA_1984-2013.csv")

# And reload it if necessary
gjoa_weatherold <- read_csv("data/en_climate_daily_GJOA_1984-2013.csv")

# Same for the new data
gjoa_weathernew <- weather_dl(station_ids = 51079, start = "2013-01-11", end = "2023-03-01")

# Save it so we don't need to download it again 
#write_csv(x = gjoa_weathernew, "data/en_climate_daily_GJOA_2013-2023.csv")

# And reload it if necessary
gjoa_weathernew <- read_csv("data/en_climate_daily_GJOA_2013-2023.csv")

# Download data for Kugaaruk ----------------------------------------------------------
# Do Kugaaruk next, which is at 68.534722, -89.825
stations_search(coords = c(68.534, -89.825), dist = 20, interval = "hour")

# Download the oldest data. Once again, Pelly Bay changed it's name to Kugaaruk in 1992.
kuga_weatherolder <- weather_dl(station_ids = 1718, start = "1957-01-01", end = "1992-05-31")

# Save it so we don't need to download it again
#write_csv(x = kuga_weatherolder, "data/en_climate_daily_KUGA_1957-1992.csv")

# And reload it if necessary
kuga_weatherolder <- read_csv("data/en_climate_daily_KUGA_1957-1992.csv")

# Download interim data. Looking at kuga_weatherolder it ends on 1992-05-31, so we set the next day as the start date
kuga_weatherold <- weather_dl(station_ids = 1719, start = "1992-06-01", end = "2012-12-31")

# Save it so don't need to download it again
#write_csv(x = kuga_weatherold, "data/en_climate_daily_KUGA_1992-2012.csv")

# And reload it if necessary
kuga_weatherold <- read_csv("data/en_climate_daily_KUGA_1992-2012.csv")

# Let's get that newer data too.
kuga_weathernew <- weather_dl(station_ids = 10847, start = "2013-01-01", end = "2015-02-12")

# Save it so we don't need to download it again
#write_csv(x = kuga_weathernew, "data/en_climate_daily_KUGA_2013-2015.csv")

# And reload it if necessary
kuga_weathernew <- read_csv("data/en_climate_daily_KUGA_2013-2015.csv")

# Download more data, since the old data ends on 1992-05-31, we'll set the start date to end 
kuga_weathernewer <- weather_dl(station_ids = 53518, start = "2015-02-12", end = "2023-03-01")

# Save it so we don't need to download it again
#write_csv(x = kuga_weathernewer, "data/en_climate_daily_KUGA_2015-2023.csv")

# And reload it if necessary
kuga_weathernewer <- read_csv("data/en_climate_daily_KUGA_2015-2023.csv")

# Process Cambridge Bay ----------------------------------------------------------
# Start with fixing cbay data
# Looking at the data:
#  We need to stitch together hourly reporting from Cambridge Bay spanning 1953-01-01 to 2023-03-01
#  We also have monthly reporting data from Cambridge Bay spanning 1949-01-01 to 1953-12-310

# Let's start by loading the data and dropping anything with NA's in temp
cbay_new <- cbay_weathernew %>%
  drop_na(temp)

# Let's combine all the hourly reports into a single day
cbay_new <- cbay_new %>%
  select(date,time,temp) %>%
  group_by(date) %>%
  summarise(maxtemp = max(temp),mintemp = min(temp))
# Great. We've used the hourly reporting data to produce a minimum mean temperature and maximum mean temperature

# Create a new column, and we'll use it to join all the days within that month together by their mean min and  mean max 
cbay_new <- cbay_new %>%
  mutate(
    date = floor_date(cbay_new$date, "month"),
    mintemp = cbay_new$mintemp,
    maxtemp = cbay_new$maxtemp
  )

# Then we average out the mean maximum and mean minimum temperatures across days into months
cbay_new <- cbay_new %>%
  group_by(date) %>%
  summarise(across(c(mintemp,maxtemp), mean)) %>%
  mutate(
    mintemp = round(mintemp, digits = 1),
    maxtemp = round(maxtemp, digits = 1)
  )

# Let's load the cbay_old data and remove NA's
cbay_old <- cbay_weatherold %>%
  drop_na(temp)

# Let's combine the hourly reports into daily reports
cbay_old <- cbay_old %>%
  select(date,time,temp) %>%
  group_by(date) %>%
  summarise(maxtemp = max(temp),mintemp = min(temp))

# Create a new column, and we'll use it to join all the days within that month together by their mean min and  mean max 
cbay_old <- cbay_old %>%
  mutate(
    date = floor_date(cbay_old$date, "month"),
    mintemp = cbay_old$mintemp,
    maxtemp = cbay_old$maxtemp
  )

# Average out the reports to months too
cbay_old <- cbay_old %>%
  group_by(date) %>%
  summarise(across(c(mintemp,maxtemp), mean)) %>%
  mutate(
    mintemp = round(mintemp, digits = 1),
    maxtemp = round(maxtemp, digits = 1)
  )

# Let's load the cbay_older data
cbay_older <- cbay_weatherolder %>%
  drop_na(mean_max_temp,mean_min_temp)

# We will also need to change the names
cbay_older <- data.frame(
  date = cbay_older$date,
  maxtemp = cbay_older$mean_max_temp,
  mintemp = cbay_older$mean_min_temp
)

# Alright, now we need to join them with our new data. 
cbay <- rbind(cbay_older,cbay_old,cbay_new)

# Do some garbage collection
rm(cbay_new,cbay_old,cbay_older)

# Very cool. Let's make another GGplot from 1949 to 2023
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
# Before we do that however, let's do the same thing with the other three communities and check that the graphs from the other communities are complete

# Process Kugluktuk ----------------------------------------------------------
# Let's start by loading the data and dropping anything with NA's in temp
kugl_new <- kugl_weathernew %>%
  drop_na(temp)

# Let's combine all the hourly reports into a single day
kugl_new <- kugl_new %>%
  select(date,time,temp) %>%
  group_by(date) %>%
  summarise(maxtemp = max(temp),mintemp = min(temp))
# Great. We've used the hourly reporting data to produce a minimum mean temperature and maximum mean temperature

# Create a new column, and we'll use it to join all the days within that month together by their mean min and  mean max 
kugl_new <- kugl_new %>%
  mutate(
    date = floor_date(kugl_new$date, "month"),
    mintemp = kugl_new$mintemp,
    maxtemp = kugl_new$maxtemp
  )

# Then we average out the mean maximum and mean minimum temperatures across days into months
kugl_new <- kugl_new %>%
  group_by(date) %>%
  summarise(across(c(mintemp,maxtemp), mean)) %>%
  mutate(
    mintemp = round(mintemp, digits = 1),
    maxtemp = round(maxtemp, digits = 1)
  )

# Let's load the kugl_old data and remove NA's
kugl_old <- kugl_weatherold %>%
  drop_na(temp)

# Let's combine the hourly reports into daily reports
kugl_old <- kugl_old %>%
  select(date,time,temp) %>%
  group_by(date) %>%
  summarise(maxtemp = max(temp),mintemp = min(temp))

# Create a new column, and we'll use it to join all the days within that month together by their mean min and  mean max 
kugl_old <- kugl_old %>%
  mutate(
    date = floor_date(kugl_old$date, "month"),
    mintemp = kugl_old$mintemp,
    maxtemp = kugl_old$maxtemp
  )

# Average out the reports to months too
kugl_old <- kugl_old %>%
  group_by(date) %>%
  summarise(across(c(mintemp,maxtemp), mean)) %>%
  mutate(
    mintemp = round(mintemp, digits = 1),
    maxtemp = round(maxtemp, digits = 1)
  )

# Let's load the kugl_older data
kugl_older <- kugl_weatherolder %>%
  drop_na(temp)

# Let's combine the hourly reports into daily reports
kugl_older <- kugl_older %>%
  select(date,time,temp) %>%
  group_by(date) %>%
  summarise(maxtemp = max(temp),mintemp = min(temp))

# Create a new column, and we'll use it to join all the days within that month together by their mean min and  mean max 
kugl_older <- kugl_older %>%
  mutate(
    date = floor_date(kugl_older$date, "month"),
    mintemp = kugl_older$mintemp,
    maxtemp = kugl_older$maxtemp
  )

# Average out the reports to months too
kugl_older <- kugl_older %>%
  group_by(date) %>%
  summarise(across(c(mintemp,maxtemp), mean)) %>%
  mutate(
    mintemp = round(mintemp, digits = 1),
    maxtemp = round(maxtemp, digits = 1)
  )

# Alright, now we need to join them with our new data. 
kugl <- rbind(kugl_older,kugl_old,kugl_new)

# Do some garbage collection
rm(kugl_new,kugl_old,kugl_older)

# Very cool. Let's make another GGplot from 1953 to 2023
ggplot(kugl, aes(x = date, y = maxtemp)) +
  geom_line(color = "red") +
  geom_point(color = "red") +
  geom_line(aes(y = mintemp), color = "blue") +
  geom_line(aes(y = mintemp), color = "blue") +
  scale_x_date(breaks = as.Date(c("1950-01-01","1955-01-01","1960-01-01","1965-01-01","1970-01-01","1975-01-01","1980-01-01","1985-01-01",
                                  "1990-01-01","1995-01-01","2000-01-01","2005-01-01","2010-01-01","2015-01-01","2020-01-01","2023-01-01")),
               minor_breaks = as.Date(c("1949-01-01")),
               date_labels = "%Y") +
  scale_y_continuous(breaks = as.integer(c(-45,-40,-35,-30,-25,-20,-15,-10,-5,0,5,10,15,20,25,30,34,40))) +
  labs(x = "Date", y = "Temperature", title="Temperature Chart for Kugluktuk")

# Process Gjoa Haven ----------------------------------------------------------
# Let's start by loading the data and dropping anything with NA's in temp
gjoa_new <- gjoa_weathernew %>%
  drop_na(temp)

# Let's combine all the hourly reports into a single day
gjoa_new <- gjoa_new %>%
  select(date,time,temp) %>%
  group_by(date) %>%
  summarise(maxtemp = max(temp),mintemp = min(temp))
# Great. We've used the hourly reporting data to produce a minimum mean temperature and maximum mean temperature

# Create a new column, and we'll use it to join all the days within that month together by their mean min and  mean max 
gjoa_new <- gjoa_new %>%
  mutate(
    date = floor_date(gjoa_new$date, "month"),
    mintemp = gjoa_new$mintemp,
    maxtemp = gjoa_new$maxtemp
  )

# Then we average out the mean maximum and mean minimum temperatures across days into months
gjoa_new <- gjoa_new %>%
  group_by(date) %>%
  summarise(across(c(mintemp,maxtemp), mean)) %>%
  mutate(
    mintemp = round(mintemp, digits = 1),
    maxtemp = round(maxtemp, digits = 1)
  )

# Let's load the gjoa_old data and remove NA's
gjoa_old <- gjoa_weatherold %>%
  drop_na(temp)

# Let's combine the hourly reports into daily reports
gjoa_old <- gjoa_old %>%
  select(date,time,temp) %>%
  group_by(date) %>%
  summarise(maxtemp = max(temp),mintemp = min(temp))

# Create a new column, and we'll use it to join all the days within that month together by their mean min and  mean max 
gjoa_old <- gjoa_old %>%
  mutate(
    date = floor_date(gjoa_old$date, "month"),
    mintemp = gjoa_old$mintemp,
    maxtemp = gjoa_old$maxtemp
  )

# Average out the reports to months too
gjoa_old <- gjoa_old %>%
  group_by(date) %>%
  summarise(across(c(mintemp,maxtemp), mean)) %>%
  mutate(
    mintemp = round(mintemp, digits = 1),
    maxtemp = round(maxtemp, digits = 1)
  )

# Alright, now we need to join them with our new data. 
gjoa <- rbind(gjoa_old,gjoa_new)

# Do some garbage collection
rm(gjoa_new,gjoa_old)

# Very cool. Let's make another GGplot from 1985 to 2023
ggplot(gjoa, aes(x = date, y = maxtemp)) +
  geom_line(color = "red") +
  geom_point(color = "red") +
  geom_line(aes(y = mintemp), color = "blue") +
  geom_line(aes(y = mintemp), color = "blue") +
  scale_x_date(breaks = as.Date(c("1950-01-01","1955-01-01","1960-01-01","1965-01-01","1970-01-01","1975-01-01","1980-01-01","1985-01-01",
                                  "1990-01-01","1995-01-01","2000-01-01","2005-01-01","2010-01-01","2015-01-01","2020-01-01","2023-01-01")),
               minor_breaks = as.Date(c("1949-01-01")),
               date_labels = "%Y") +
  scale_y_continuous(breaks = as.integer(c(-45,-40,-35,-30,-25,-20,-15,-10,-5,0,5,10,15,20,25,30,34,40))) +
  labs(x = "Date", y = "Temperature", title="Temperature Chart for Gjoa Haven")

# Process kuga Haven ----------------------------------------------------------
# Let's start by loading the data and dropping anything with NA's in temp
kuga_new <- kuga_weathernew %>%
  drop_na(temp)

# Let's combine all the hourly reports into a single day
kuga_new <- kuga_new %>%
  select(date,time,temp) %>%
  group_by(date) %>%
  summarise(maxtemp = max(temp),mintemp = min(temp))
# Great. We've used the hourly reporting data to produce a minimum mean temperature and maximum mean temperature

# Create a new column, and we'll use it to join all the days within that month together by their mean min and  mean max 
kuga_new <- kuga_new %>%
  mutate(
    date = floor_date(kuga_new$date, "month"),
    mintemp = kuga_new$mintemp,
    maxtemp = kuga_new$maxtemp
  )

# Then we average out the mean maximum and mean minimum temperatures across days into months
kuga_new <- kuga_new %>%
  group_by(date) %>%
  summarise(across(c(mintemp,maxtemp), mean)) %>%
  mutate(
    mintemp = round(mintemp, digits = 1),
    maxtemp = round(maxtemp, digits = 1)
  )

# Let's start by loading the data and dropping anything with NA's in temp
kuga_newer <- kuga_weathernewer %>%
  drop_na(temp)

# Let's combine all the hourly reports into a single day
kuga_newer <- kuga_newer %>%
  select(date,time,temp) %>%
  group_by(date) %>%
  summarise(maxtemp = max(temp),mintemp = min(temp))
# Great. We've used the hourly reporting data to produce a minimum mean temperature and maximum mean temperature

# Create a new column, and we'll use it to join all the days within that month together by their mean min and  mean max 
kuga_newer <- kuga_newer %>%
  mutate(
    date = floor_date(kuga_newer$date, "month"),
    mintemp = kuga_newer$mintemp,
    maxtemp = kuga_newer$maxtemp
  )

# Then we average out the mean maximum and mean minimum temperatures across days into months
kuga_newer <- kuga_newer %>%
  group_by(date) %>%
  summarise(across(c(mintemp,maxtemp), mean)) %>%
  mutate(
    mintemp = round(mintemp, digits = 1),
    maxtemp = round(maxtemp, digits = 1)
  )

# Let's load the kuga_old data and remove NA's
kuga_old <- kuga_weatherold %>%
  drop_na(temp)

# Let's combine the hourly reports into daily reports
kuga_old <- kuga_old %>%
  select(date,time,temp) %>%
  group_by(date) %>%
  summarise(maxtemp = max(temp),mintemp = min(temp))

# Create a new column, and we'll use it to join all the days within that month together by their mean min and  mean max 
kuga_old <- kuga_old %>%
  mutate(
    date = floor_date(kuga_old$date, "month"),
    mintemp = kuga_old$mintemp,
    maxtemp = kuga_old$maxtemp
  )

# Average out the reports to months too
kuga_old <- kuga_old %>%
  group_by(date) %>%
  summarise(across(c(mintemp,maxtemp), mean)) %>%
  mutate(
    mintemp = round(mintemp, digits = 1),
    maxtemp = round(maxtemp, digits = 1)
  )

# Let's load the kuga_old data and remove NA's
kuga_older <- kuga_weatherolder %>%
  drop_na(temp)

# Let's combine the hourly reports into daily reports
kuga_older <- kuga_older %>%
  select(date,time,temp) %>%
  group_by(date) %>%
  summarise(maxtemp = max(temp),mintemp = min(temp))

# Create a new column, and we'll use it to join all the days within that month together by their mean min and  mean max 
kuga_older <- kuga_older %>%
  mutate(
    date = floor_date(kuga_older$date, "month"),
    mintemp = kuga_older$mintemp,
    maxtemp = kuga_older$maxtemp
  )

# Average out the reports to months too
kuga_older <- kuga_older %>%
  group_by(date) %>%
  summarise(across(c(mintemp,maxtemp), mean)) %>%
  mutate(
    mintemp = round(mintemp, digits = 1),
    maxtemp = round(maxtemp, digits = 1)
  )

# Alright, now we need to join them with our new data. 
kuga <- rbind(kuga_older,kuga_old,kuga_new,kuga_newer)

# Do some garbage collection
rm(kuga_older,kuga_old,kuga_new,kuga_newer)

# Very cool. Let's make another GGplot from 1957 to 2023
ggplot(kuga, aes(x = date, y = maxtemp)) +
  geom_line(color = "red") +
  geom_point(color = "red") +
  geom_line(aes(y = mintemp), color = "blue") +
  geom_line(aes(y = mintemp), color = "blue") +
  scale_x_date(breaks = as.Date(c("1950-01-01","1955-01-01","1960-01-01","1965-01-01","1970-01-01","1975-01-01","1980-01-01","1985-01-01",
                                  "1990-01-01","1995-01-01","2000-01-01","2005-01-01","2010-01-01","2015-01-01","2020-01-01","2023-01-01")),
               minor_breaks = as.Date(c("1949-01-01")),
               date_labels = "%Y") +
  scale_y_continuous(breaks = as.integer(c(-45,-40,-35,-30,-25,-20,-15,-10,-5,0,5,10,15,20,25,30,34,40))) +
  labs(x = "Date", y = "Temperature", title="Temperature Chart for Kugaaruk")

# Now for the fun stuff ----------------------------------------------------------

# Test animation
# Let's start with Cambridge Bay.
test_cbay <- cbay %>%
  mutate(year = year(cbay$date),
    month = month(cbay$date),
    month = month.abb[month],
    maxtemp = cbay$maxtemp,
    mintemp = cbay$mintemp) %>%
  select(year,month,maxtemp,mintemp) %>%
  mutate(month = factor(month, levels = month.abb))

p <- test_cbay %>%
  ggplot(aes(x = month, y = maxtemp, group=year)) +
  geom_line()

a <- p +
  geom_label(aes(x = 7, y = 0, label=year), fontface="bold", label.size=0) +
  transition_manual(year, cumulative = TRUE)

# Create animation
animate(a, width=6, height=4, unit="in", res=300)

# Save it
anim_save("fig/cbay_monthlytemperature.gif", a)

# Let's get serious though ----------------------------------------------------------
# We need to generate for each community, an average mean temperature from the start of the data to the end of the data
# For Cambridge Bay, it's the monthly average from 1949 to 2015

# So let's select the months that exist between those years to produce something can can compare the annual difference to
# I'm not the smarted tool in the shed, so I'm going to do this in an incredibly crude way.
# Start with Jan
target_cbay_jan <- test_cbay %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1949 & year <= 2015) %>%
  filter(month == "Jan") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
# Do Feb
target_cbay_feb <- test_cbay %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1949 & year <= 2015) %>%
  filter(month == "Feb") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
# Do Mar
target_cbay_mar <- test_cbay %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1949 & year <= 2015) %>%
  filter(month == "Mar") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
# Do Apr
target_cbay_apr <- test_cbay %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1949 & year <= 2015) %>%
  filter(month == "Apr") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
# Do May
target_cbay_may <- test_cbay %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1949 & year <= 2015) %>%
  filter(month == "May") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
# Do Jun
target_cbay_jun <- test_cbay %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1949 & year <= 2015) %>%
  filter(month == "Jun") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
# Do Jul
target_cbay_jul <- test_cbay %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1949 & year <= 2015) %>%
  filter(month == "Jul") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
# Do Aug
target_cbay_aug <- test_cbay %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1949 & year <= 2015) %>%
  filter(month == "Aug") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
# Do Sep
target_cbay_sep <- test_cbay %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1949 & year <= 2015) %>%
  filter(month == "Sep") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
# Do Oct
target_cbay_oct <- test_cbay %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1949 & year <= 2015) %>%
  filter(month == "Oct") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
# Do Nov
target_cbay_nov <- test_cbay %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1949 & year <= 2015) %>%
  filter(month == "Nov") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
# Do Dec
target_cbay_dec <- test_cbay %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1949 & year <= 2015) %>%
  filter(month == "Dec") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )

# Next, make a new data frame with those values
maxavg_cbay <- data.frame(
  Jan = c(target_cbay_jan$maxavg[1]),
  Feb = c(target_cbay_feb$maxavg[1]),
  Mar = c(target_cbay_mar$maxavg[1]),
  Apr = c(target_cbay_apr$maxavg[1]),
  May = c(target_cbay_may$maxavg[1]),
  Jun = c(target_cbay_jun$maxavg[1]),
  Jul = c(target_cbay_jul$maxavg[1]),
  Aug = c(target_cbay_aug$maxavg[1]),
  Sep = c(target_cbay_sep$maxavg[1]),
  Oct = c(target_cbay_oct$maxavg[1]),
  Nov = c(target_cbay_nov$maxavg[1]),
  Dec = c(target_cbay_dec$maxavg[1])
) %>%
  pivot_longer(-0,names_to = "month", values_to = "maxavg")
# Let's do the same with the min averages
minavg_cbay <- data.frame(
  Jan = c(target_cbay_jan$minavg[1]),
  Feb = c(target_cbay_feb$minavg[1]),
  Mar = c(target_cbay_mar$minavg[1]),
  Apr = c(target_cbay_apr$minavg[1]),
  May = c(target_cbay_may$minavg[1]),
  Jun = c(target_cbay_jun$minavg[1]),
  Jul = c(target_cbay_jul$minavg[1]),
  Aug = c(target_cbay_aug$minavg[1]),
  Sep = c(target_cbay_sep$minavg[1]),
  Oct = c(target_cbay_oct$minavg[1]),
  Nov = c(target_cbay_nov$minavg[1]),
  Dec = c(target_cbay_dec$minavg[1])
) %>%
  pivot_longer(-0,names_to = "month", values_to = "minavg")
# Let's bind 'em together
cbay_avg <- inner_join(minavg_cbay,maxavg_cbay, by="month")
# Let's do some garbage collection
rm(target_cbay_jan,target_cbay_feb,target_cbay_mar,target_cbay_apr,target_cbay_may,target_cbay_jun,target_cbay_jul,target_cbay_aug,target_cbay_sep,target_cbay_oct,target_cbay_nov,target_cbay_dec)
rm(maxavg_cbay,minavg_cbay)

# That was a lot of work. Why are what should be intuitively easy solutions sometimes impossible to think of?
# Let's produce another data frame where we take the reported values for every year and create a new column called t_diff?
# Before we do that, let's maybe round to a nice nearest decimal place
cbay_avg <- cbay_avg %>%
  mutate(
    minavg = round(minavg, digits = 1),
    maxavg = round(maxavg, digits = 1)
  )

# If it looks stupid and like it's a really long way of doing something, that's generally a sign I haven't thought of a better way of doing it. Alas, here we are.
# But now that we have our minimum mean and maximum mean temperature averages from 1949 to 2015 for Cambridge Bay, let's take our data and see what the differences are based on those averages

# Let's try something silly. Let's subset our data by months.
cbay_jan <- subset(test_cbay, month == "Jan")
cbay_feb <- subset(test_cbay, month == "Feb")
cbay_mar <- subset(test_cbay, month == "Mar")
cbay_apr <- subset(test_cbay, month == "Apr")
cbay_may <- subset(test_cbay, month == "May")
cbay_jun <- subset(test_cbay, month == "Jun")
cbay_jul <- subset(test_cbay, month == "Jul")
cbay_aug <- subset(test_cbay, month == "Aug")
cbay_sep <- subset(test_cbay, month == "Sep")
cbay_oct <- subset(test_cbay, month == "Oct")
cbay_nov <- subset(test_cbay, month == "Nov")
cbay_dec <- subset(test_cbay, month == "Dec")
# Great. That's dirty AF. I love it. Now let's do our shitty math with our monthly averages from 1949 to 2015.
cbay_jan <- cbay_jan %>%
  mutate(
    maxavg = maxtemp-cbay_avg$maxavg[1],
    minavg = mintemp-cbay_avg$minavg[1]
  )
cbay_feb <- cbay_feb %>%
  mutate(
    maxavg = maxtemp-cbay_avg$maxavg[2],
    minavg = mintemp-cbay_avg$minavg[2]
  )
cbay_mar <- cbay_mar %>%
  mutate(
    maxavg = maxtemp-cbay_avg$maxavg[3],
    minavg = mintemp-cbay_avg$minavg[3]
  )
cbay_apr <- cbay_apr %>%
  mutate(
    maxavg = maxtemp-cbay_avg$maxavg[4],
    minavg = mintemp-cbay_avg$minavg[4]
  )
cbay_may <- cbay_may %>%
  mutate(
    maxavg = maxtemp-cbay_avg$maxavg[5],
    minavg = mintemp-cbay_avg$minavg[5]
  )
cbay_jun <- cbay_jun %>%
  mutate(
    maxavg = maxtemp-cbay_avg$maxavg[6],
    minavg = mintemp-cbay_avg$minavg[6]
  )
cbay_jul <- cbay_jul %>%
  mutate(
    maxavg = maxtemp-cbay_avg$maxavg[7],
    minavg = mintemp-cbay_avg$minavg[7]
  )
cbay_aug <- cbay_aug %>%
  mutate(
    maxavg = maxtemp-cbay_avg$maxavg[8],
    minavg = mintemp-cbay_avg$minavg[8]
  )
cbay_sep <- cbay_sep %>%
  mutate(
    maxavg = maxtemp-cbay_avg$maxavg[9],
    minavg = mintemp-cbay_avg$minavg[9]
  )
cbay_oct <- cbay_oct %>%
  mutate(
    maxavg = maxtemp-cbay_avg$maxavg[10],
    minavg = mintemp-cbay_avg$minavg[10]
  )
cbay_nov <- cbay_nov %>%
  mutate(
    maxavg = maxtemp-cbay_avg$maxavg[11],
    minavg = mintemp-cbay_avg$minavg[11]
  )
cbay_dec <- cbay_dec %>%
  mutate(
    maxavg = maxtemp-cbay_avg$maxavg[12],
    minavg = mintemp-cbay_avg$minavg[12]
  )
# Great. We did something. Let's stitch 'em all back together again.
cbay_data <- rbind(cbay_jan,cbay_feb,cbay_mar,cbay_apr,cbay_may,cbay_jun,cbay_jul,cbay_aug,cbay_sep,cbay_oct,cbay_nov,cbay_dec)
# Do some garbage collection
rm(cbay_jan,cbay_feb,cbay_mar,cbay_apr,cbay_may,cbay_jun,cbay_jul,cbay_aug,cbay_sep,cbay_oct,cbay_nov,cbay_dec)
# Right, let's sort that by year and month
cbay_data <- cbay_data %>%
  arrange(year,month)
