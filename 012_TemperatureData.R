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

# Now for the fun stuff, make an animation ----------------------------------------------------------

# Create a test animation, let's start with Cambridge Bay.
test_cbay <- cbay %>%
  mutate(year = year(cbay$date),
    month = month(cbay$date),
    month = month.abb[month],
    maxtemp = cbay$maxtemp,
    mintemp = cbay$mintemp) %>%
  select(year,month,maxtemp,mintemp) %>%
  mutate(month = factor(month, levels = month.abb))

# We'll re-use this for the cbay data shortly, but let's try an animation to see what these data look like graphed.
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
# How would that be? By doing it manually, one at a time until I get what I'm looking for. I'm sure smarter people than I can do this easier, but for now I'll go with
# an approach that lets me copy and paste the entire section and use the search and replace feature of Notepad++ in another window to do it for the other communities,

# Start with Jan
target_cbay_jan <- test_cbay %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1949 & year <= 2015) %>%
  filter(month == "Jan") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_cbay_feb <- test_cbay %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1949 & year <= 2015) %>%
  filter(month == "Feb") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_cbay_mar <- test_cbay %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1949 & year <= 2015) %>%
  filter(month == "Mar") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_cbay_apr <- test_cbay %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1949 & year <= 2015) %>%
  filter(month == "Apr") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_cbay_may <- test_cbay %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1949 & year <= 2015) %>%
  filter(month == "May") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_cbay_jun <- test_cbay %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1949 & year <= 2015) %>%
  filter(month == "Jun") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_cbay_jul <- test_cbay %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1949 & year <= 2015) %>%
  filter(month == "Jul") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_cbay_aug <- test_cbay %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1949 & year <= 2015) %>%
  filter(month == "Aug") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_cbay_sep <- test_cbay %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1949 & year <= 2015) %>%
  filter(month == "Sep") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_cbay_oct <- test_cbay %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1949 & year <= 2015) %>%
  filter(month == "Oct") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_cbay_nov <- test_cbay %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1949 & year <= 2015) %>%
  filter(month == "Nov") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_cbay_dec <- test_cbay %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1949 & year <= 2015) %>%
  filter(month == "Dec") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
# Next, make a new data frame with those values
# These are the reported averages from 1949 to 2015.
# We'll be using these to produce the difference later. It's a pain in the ass doing it this way,
# but I'm not a clever man so instead of an elegant solution I'll just stick with being dilegent/logical.
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

# Great, we have our averages. Let's bind 'em together
cbay_avg <- inner_join(minavg_cbay,maxavg_cbay, by="month")

# Let's do some garbage collection
rm(target_cbay_jan,target_cbay_feb,target_cbay_mar,target_cbay_apr,target_cbay_may,target_cbay_jun,target_cbay_jul,target_cbay_aug,target_cbay_sep,target_cbay_oct,target_cbay_nov,target_cbay_dec)
rm(maxavg_cbay,minavg_cbay)
# That was a lot of work. Why are what should be intuitively easy solutions sometimes impossible to think of?

# Before we do that, let's maybe round to a nice nearest decimal place
cbay_avg <- cbay_avg %>%
  mutate(
    minavg = round(minavg, digits = 1),
    maxavg = round(maxavg, digits = 1)
  )

# If it looks stupid and like it's a really long way of doing something, that's generally a sign I haven't thought of a better way of doing it. Alas, here we are.
# But now that we have our minimum mean and maximum mean temperature averages from 1949 to 2015 for Cambridge Bay, let's take our data and see what the differences are based on those averages

# Let's try something silly. Let's subset our data by months. This will make it easier to process. At least for me. As indicated earlier, there has to be a more elegant solution.
# But since I haven't thought of it, we'll do it the long way.
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
# Great. That way is dirty AF. I love it. Now let's do our shitty math with our monthly averages from 1949 to 2015 to produce our reported differences
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
# Great. We did something. Now let's stitch 'em all back together again.
cbay_data <- rbind(cbay_jan,cbay_feb,cbay_mar,cbay_apr,cbay_may,cbay_jun,cbay_jul,cbay_aug,cbay_sep,cbay_oct,cbay_nov,cbay_dec)
# Gotta do some garbage collection
rm(cbay_jan,cbay_feb,cbay_mar,cbay_apr,cbay_may,cbay_jun,cbay_jul,cbay_aug,cbay_sep,cbay_oct,cbay_nov,cbay_dec)
# Right, that's done. Now let's sort that newly minted data by year and month
cbay_data <- cbay_data %>%
  arrange(year,month)
# Now that We did that for Cambridge Bay. Let's do it for Kugluktuk, Gjoa Haven, and Kugaaruk too.

# Let's do Kugluktuk ----------------------------------------------------------
# The data for Kugluktuk spans from 1953 to 2023
test_kugl <- kugl %>%
  mutate(year = year(kugl$date),
         month = month(kugl$date),
         month = month.abb[month],
         maxtemp = kugl$maxtemp,
         mintemp = kugl$mintemp) %>%
  select(year,month,maxtemp,mintemp) %>%
  mutate(month = factor(month, levels = month.abb))
target_kugl_jan <- test_kugl %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1953 & year <= 2015) %>%
  filter(month == "Jan") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_kugl_feb <- test_kugl %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1953 & year <= 2015) %>%
  filter(month == "Feb") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_kugl_mar <- test_kugl %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1953 & year <= 2015) %>%
  filter(month == "Mar") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_kugl_apr <- test_kugl %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1953 & year <= 2015) %>%
  filter(month == "Apr") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_kugl_may <- test_kugl %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1953 & year <= 2015) %>%
  filter(month == "May") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_kugl_jun <- test_kugl %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1953 & year <= 2015) %>%
  filter(month == "Jun") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_kugl_jul <- test_kugl %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1953 & year <= 2015) %>%
  filter(month == "Jul") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_kugl_aug <- test_kugl %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1953 & year <= 2015) %>%
  filter(month == "Aug") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_kugl_sep <- test_kugl %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1953 & year <= 2015) %>%
  filter(month == "Sep") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_kugl_oct <- test_kugl %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1953 & year <= 2015) %>%
  filter(month == "Oct") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_kugl_nov <- test_kugl %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1953 & year <= 2015) %>%
  filter(month == "Nov") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_kugl_dec <- test_kugl %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1953 & year <= 2015) %>%
  filter(month == "Dec") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
maxavg_kugl <- data.frame(
  Jan = c(target_kugl_jan$maxavg[1]),
  Feb = c(target_kugl_feb$maxavg[1]),
  Mar = c(target_kugl_mar$maxavg[1]),
  Apr = c(target_kugl_apr$maxavg[1]),
  May = c(target_kugl_may$maxavg[1]),
  Jun = c(target_kugl_jun$maxavg[1]),
  Jul = c(target_kugl_jul$maxavg[1]),
  Aug = c(target_kugl_aug$maxavg[1]),
  Sep = c(target_kugl_sep$maxavg[1]),
  Oct = c(target_kugl_oct$maxavg[1]),
  Nov = c(target_kugl_nov$maxavg[1]),
  Dec = c(target_kugl_dec$maxavg[1])
) %>%
  pivot_longer(-0,names_to = "month", values_to = "maxavg")
minavg_kugl <- data.frame(
  Jan = c(target_kugl_jan$minavg[1]),
  Feb = c(target_kugl_feb$minavg[1]),
  Mar = c(target_kugl_mar$minavg[1]),
  Apr = c(target_kugl_apr$minavg[1]),
  May = c(target_kugl_may$minavg[1]),
  Jun = c(target_kugl_jun$minavg[1]),
  Jul = c(target_kugl_jul$minavg[1]),
  Aug = c(target_kugl_aug$minavg[1]),
  Sep = c(target_kugl_sep$minavg[1]),
  Oct = c(target_kugl_oct$minavg[1]),
  Nov = c(target_kugl_nov$minavg[1]),
  Dec = c(target_kugl_dec$minavg[1])
) %>%
  pivot_longer(-0,names_to = "month", values_to = "minavg")
kugl_avg <- inner_join(minavg_kugl,maxavg_kugl, by="month")
rm(target_kugl_jan,target_kugl_feb,target_kugl_mar,target_kugl_apr,target_kugl_may,target_kugl_jun,target_kugl_jul,target_kugl_aug,target_kugl_sep,target_kugl_oct,target_kugl_nov,target_kugl_dec)
rm(maxavg_kugl,minavg_kugl)
kugl_avg <- kugl_avg %>%
  mutate(
    minavg = round(minavg, digits = 1),
    maxavg = round(maxavg, digits = 1)
  )
kugl_jan <- subset(test_kugl, month == "Jan")
kugl_feb <- subset(test_kugl, month == "Feb")
kugl_mar <- subset(test_kugl, month == "Mar")
kugl_apr <- subset(test_kugl, month == "Apr")
kugl_may <- subset(test_kugl, month == "May")
kugl_jun <- subset(test_kugl, month == "Jun")
kugl_jul <- subset(test_kugl, month == "Jul")
kugl_aug <- subset(test_kugl, month == "Aug")
kugl_sep <- subset(test_kugl, month == "Sep")
kugl_oct <- subset(test_kugl, month == "Oct")
kugl_nov <- subset(test_kugl, month == "Nov")
kugl_dec <- subset(test_kugl, month == "Dec")
kugl_jan <- kugl_jan %>%
  mutate(
    maxavg = maxtemp-kugl_avg$maxavg[1],
    minavg = mintemp-kugl_avg$minavg[1]
  )
kugl_feb <- kugl_feb %>%
  mutate(
    maxavg = maxtemp-kugl_avg$maxavg[2],
    minavg = mintemp-kugl_avg$minavg[2]
  )
kugl_mar <- kugl_mar %>%
  mutate(
    maxavg = maxtemp-kugl_avg$maxavg[3],
    minavg = mintemp-kugl_avg$minavg[3]
  )
kugl_apr <- kugl_apr %>%
  mutate(
    maxavg = maxtemp-kugl_avg$maxavg[4],
    minavg = mintemp-kugl_avg$minavg[4]
  )
kugl_may <- kugl_may %>%
  mutate(
    maxavg = maxtemp-kugl_avg$maxavg[5],
    minavg = mintemp-kugl_avg$minavg[5]
  )
kugl_jun <- kugl_jun %>%
  mutate(
    maxavg = maxtemp-kugl_avg$maxavg[6],
    minavg = mintemp-kugl_avg$minavg[6]
  )
kugl_jul <- kugl_jul %>%
  mutate(
    maxavg = maxtemp-kugl_avg$maxavg[7],
    minavg = mintemp-kugl_avg$minavg[7]
  )
kugl_aug <- kugl_aug %>%
  mutate(
    maxavg = maxtemp-kugl_avg$maxavg[8],
    minavg = mintemp-kugl_avg$minavg[8]
  )
kugl_sep <- kugl_sep %>%
  mutate(
    maxavg = maxtemp-kugl_avg$maxavg[9],
    minavg = mintemp-kugl_avg$minavg[9]
  )
kugl_oct <- kugl_oct %>%
  mutate(
    maxavg = maxtemp-kugl_avg$maxavg[10],
    minavg = mintemp-kugl_avg$minavg[10]
  )
kugl_nov <- kugl_nov %>%
  mutate(
    maxavg = maxtemp-kugl_avg$maxavg[11],
    minavg = mintemp-kugl_avg$minavg[11]
  )
kugl_dec <- kugl_dec %>%
  mutate(
    maxavg = maxtemp-kugl_avg$maxavg[12],
    minavg = mintemp-kugl_avg$minavg[12]
  )
kugl_data <- rbind(kugl_jan,kugl_feb,kugl_mar,kugl_apr,kugl_may,kugl_jun,kugl_jul,kugl_aug,kugl_sep,kugl_oct,kugl_nov,kugl_dec)
rm(kugl_jan,kugl_feb,kugl_mar,kugl_apr,kugl_may,kugl_jun,kugl_jul,kugl_aug,kugl_sep,kugl_oct,kugl_nov,kugl_dec)
kugl_data <- kugl_data %>%
  arrange(year,month)

# Let's do Gjoa Haven ----------------------------------------------------------
# The data spans from 1984 to 2023
test_gjoa <- gjoa %>%
  mutate(year = year(gjoa$date),
         month = month(gjoa$date),
         month = month.abb[month],
         maxtemp = gjoa$maxtemp,
         mintemp = gjoa$mintemp) %>%
  select(year,month,maxtemp,mintemp) %>%
  mutate(month = factor(month, levels = month.abb))
target_gjoa_jan <- test_gjoa %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1984 & year <= 2015) %>%
  filter(month == "Jan") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_gjoa_feb <- test_gjoa %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1984 & year <= 2015) %>%
  filter(month == "Feb") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_gjoa_mar <- test_gjoa %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1984 & year <= 2015) %>%
  filter(month == "Mar") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_gjoa_apr <- test_gjoa %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1984 & year <= 2015) %>%
  filter(month == "Apr") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_gjoa_may <- test_gjoa %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1984 & year <= 2015) %>%
  filter(month == "May") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_gjoa_jun <- test_gjoa %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1984 & year <= 2015) %>%
  filter(month == "Jun") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_gjoa_jul <- test_gjoa %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1984 & year <= 2015) %>%
  filter(month == "Jul") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_gjoa_aug <- test_gjoa %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1984 & year <= 2015) %>%
  filter(month == "Aug") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_gjoa_sep <- test_gjoa %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1984 & year <= 2015) %>%
  filter(month == "Sep") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_gjoa_oct <- test_gjoa %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1984 & year <= 2015) %>%
  filter(month == "Oct") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_gjoa_nov <- test_gjoa %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1984 & year <= 2015) %>%
  filter(month == "Nov") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_gjoa_dec <- test_gjoa %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1984 & year <= 2015) %>%
  filter(month == "Dec") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
maxavg_gjoa <- data.frame(
  Jan = c(target_gjoa_jan$maxavg[1]),
  Feb = c(target_gjoa_feb$maxavg[1]),
  Mar = c(target_gjoa_mar$maxavg[1]),
  Apr = c(target_gjoa_apr$maxavg[1]),
  May = c(target_gjoa_may$maxavg[1]),
  Jun = c(target_gjoa_jun$maxavg[1]),
  Jul = c(target_gjoa_jul$maxavg[1]),
  Aug = c(target_gjoa_aug$maxavg[1]),
  Sep = c(target_gjoa_sep$maxavg[1]),
  Oct = c(target_gjoa_oct$maxavg[1]),
  Nov = c(target_gjoa_nov$maxavg[1]),
  Dec = c(target_gjoa_dec$maxavg[1])
) %>%
  pivot_longer(-0,names_to = "month", values_to = "maxavg")
minavg_gjoa <- data.frame(
  Jan = c(target_gjoa_jan$minavg[1]),
  Feb = c(target_gjoa_feb$minavg[1]),
  Mar = c(target_gjoa_mar$minavg[1]),
  Apr = c(target_gjoa_apr$minavg[1]),
  May = c(target_gjoa_may$minavg[1]),
  Jun = c(target_gjoa_jun$minavg[1]),
  Jul = c(target_gjoa_jul$minavg[1]),
  Aug = c(target_gjoa_aug$minavg[1]),
  Sep = c(target_gjoa_sep$minavg[1]),
  Oct = c(target_gjoa_oct$minavg[1]),
  Nov = c(target_gjoa_nov$minavg[1]),
  Dec = c(target_gjoa_dec$minavg[1])
) %>%
  pivot_longer(-0,names_to = "month", values_to = "minavg")
gjoa_avg <- inner_join(minavg_gjoa,maxavg_gjoa, by="month")
rm(target_gjoa_jan,target_gjoa_feb,target_gjoa_mar,target_gjoa_apr,target_gjoa_may,target_gjoa_jun,target_gjoa_jul,target_gjoa_aug,target_gjoa_sep,target_gjoa_oct,target_gjoa_nov,target_gjoa_dec)
rm(maxavg_gjoa,minavg_gjoa)
gjoa_avg <- gjoa_avg %>%
  mutate(
    minavg = round(minavg, digits = 1),
    maxavg = round(maxavg, digits = 1)
  )
gjoa_jan <- subset(test_gjoa, month == "Jan")
gjoa_feb <- subset(test_gjoa, month == "Feb")
gjoa_mar <- subset(test_gjoa, month == "Mar")
gjoa_apr <- subset(test_gjoa, month == "Apr")
gjoa_may <- subset(test_gjoa, month == "May")
gjoa_jun <- subset(test_gjoa, month == "Jun")
gjoa_jul <- subset(test_gjoa, month == "Jul")
gjoa_aug <- subset(test_gjoa, month == "Aug")
gjoa_sep <- subset(test_gjoa, month == "Sep")
gjoa_oct <- subset(test_gjoa, month == "Oct")
gjoa_nov <- subset(test_gjoa, month == "Nov")
gjoa_dec <- subset(test_gjoa, month == "Dec")
gjoa_jan <- gjoa_jan %>%
  mutate(
    maxavg = maxtemp-gjoa_avg$maxavg[1],
    minavg = mintemp-gjoa_avg$minavg[1]
  )
gjoa_feb <- gjoa_feb %>%
  mutate(
    maxavg = maxtemp-gjoa_avg$maxavg[2],
    minavg = mintemp-gjoa_avg$minavg[2]
  )
gjoa_mar <- gjoa_mar %>%
  mutate(
    maxavg = maxtemp-gjoa_avg$maxavg[3],
    minavg = mintemp-gjoa_avg$minavg[3]
  )
gjoa_apr <- gjoa_apr %>%
  mutate(
    maxavg = maxtemp-gjoa_avg$maxavg[4],
    minavg = mintemp-gjoa_avg$minavg[4]
  )
gjoa_may <- gjoa_may %>%
  mutate(
    maxavg = maxtemp-gjoa_avg$maxavg[5],
    minavg = mintemp-gjoa_avg$minavg[5]
  )
gjoa_jun <- gjoa_jun %>%
  mutate(
    maxavg = maxtemp-gjoa_avg$maxavg[6],
    minavg = mintemp-gjoa_avg$minavg[6]
  )
gjoa_jul <- gjoa_jul %>%
  mutate(
    maxavg = maxtemp-gjoa_avg$maxavg[7],
    minavg = mintemp-gjoa_avg$minavg[7]
  )
gjoa_aug <- gjoa_aug %>%
  mutate(
    maxavg = maxtemp-gjoa_avg$maxavg[8],
    minavg = mintemp-gjoa_avg$minavg[8]
  )
gjoa_sep <- gjoa_sep %>%
  mutate(
    maxavg = maxtemp-gjoa_avg$maxavg[9],
    minavg = mintemp-gjoa_avg$minavg[9]
  )
gjoa_oct <- gjoa_oct %>%
  mutate(
    maxavg = maxtemp-gjoa_avg$maxavg[10],
    minavg = mintemp-gjoa_avg$minavg[10]
  )
gjoa_nov <- gjoa_nov %>%
  mutate(
    maxavg = maxtemp-gjoa_avg$maxavg[11],
    minavg = mintemp-gjoa_avg$minavg[11]
  )
gjoa_dec <- gjoa_dec %>%
  mutate(
    maxavg = maxtemp-gjoa_avg$maxavg[12],
    minavg = mintemp-gjoa_avg$minavg[12]
  )
gjoa_data <- rbind(gjoa_jan,gjoa_feb,gjoa_mar,gjoa_apr,gjoa_may,gjoa_jun,gjoa_jul,gjoa_aug,gjoa_sep,gjoa_oct,gjoa_nov,gjoa_dec)
rm(gjoa_jan,gjoa_feb,gjoa_mar,gjoa_apr,gjoa_may,gjoa_jun,gjoa_jul,gjoa_aug,gjoa_sep,gjoa_oct,gjoa_nov,gjoa_dec)
gjoa_data <- gjoa_data %>%
  arrange(year,month)

# Lastly, let's do Kugaaruk ----------------------------------------------------------
# The data spans from 1957 to 2023
test_kuga <- kuga %>%
  mutate(year = year(kuga$date),
         month = month(kuga$date),
         month = month.abb[month],
         maxtemp = kuga$maxtemp,
         mintemp = kuga$mintemp) %>%
  select(year,month,maxtemp,mintemp) %>%
  mutate(month = factor(month, levels = month.abb))
target_kuga_jan <- test_kuga %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1957 & year <= 2015) %>%
  filter(month == "Jan") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_kuga_feb <- test_kuga %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1957 & year <= 2015) %>%
  filter(month == "Feb") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_kuga_mar <- test_kuga %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1957 & year <= 2015) %>%
  filter(month == "Mar") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_kuga_apr <- test_kuga %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1957 & year <= 2015) %>%
  filter(month == "Apr") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_kuga_may <- test_kuga %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1957 & year <= 2015) %>%
  filter(month == "May") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_kuga_jun <- test_kuga %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1957 & year <= 2015) %>%
  filter(month == "Jun") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_kuga_jul <- test_kuga %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1957 & year <= 2015) %>%
  filter(month == "Jul") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_kuga_aug <- test_kuga %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1957 & year <= 2015) %>%
  filter(month == "Aug") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_kuga_sep <- test_kuga %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1957 & year <= 2015) %>%
  filter(month == "Sep") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_kuga_oct <- test_kuga %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1957 & year <= 2015) %>%
  filter(month == "Oct") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_kuga_nov <- test_kuga %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1957 & year <= 2015) %>%
  filter(month == "Nov") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_kuga_dec <- test_kuga %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1957 & year <= 2015) %>%
  filter(month == "Dec") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
maxavg_kuga <- data.frame(
  Jan = c(target_kuga_jan$maxavg[1]),
  Feb = c(target_kuga_feb$maxavg[1]),
  Mar = c(target_kuga_mar$maxavg[1]),
  Apr = c(target_kuga_apr$maxavg[1]),
  May = c(target_kuga_may$maxavg[1]),
  Jun = c(target_kuga_jun$maxavg[1]),
  Jul = c(target_kuga_jul$maxavg[1]),
  Aug = c(target_kuga_aug$maxavg[1]),
  Sep = c(target_kuga_sep$maxavg[1]),
  Oct = c(target_kuga_oct$maxavg[1]),
  Nov = c(target_kuga_nov$maxavg[1]),
  Dec = c(target_kuga_dec$maxavg[1])
) %>%
  pivot_longer(-0,names_to = "month", values_to = "maxavg")
minavg_kuga <- data.frame(
  Jan = c(target_kuga_jan$minavg[1]),
  Feb = c(target_kuga_feb$minavg[1]),
  Mar = c(target_kuga_mar$minavg[1]),
  Apr = c(target_kuga_apr$minavg[1]),
  May = c(target_kuga_may$minavg[1]),
  Jun = c(target_kuga_jun$minavg[1]),
  Jul = c(target_kuga_jul$minavg[1]),
  Aug = c(target_kuga_aug$minavg[1]),
  Sep = c(target_kuga_sep$minavg[1]),
  Oct = c(target_kuga_oct$minavg[1]),
  Nov = c(target_kuga_nov$minavg[1]),
  Dec = c(target_kuga_dec$minavg[1])
) %>%
  pivot_longer(-0,names_to = "month", values_to = "minavg")
kuga_avg <- inner_join(minavg_kuga,maxavg_kuga, by="month")
rm(target_kuga_jan,target_kuga_feb,target_kuga_mar,target_kuga_apr,target_kuga_may,target_kuga_jun,target_kuga_jul,target_kuga_aug,target_kuga_sep,target_kuga_oct,target_kuga_nov,target_kuga_dec)
rm(maxavg_kuga,minavg_kuga)
kuga_avg <- kuga_avg %>%
  mutate(
    minavg = round(minavg, digits = 1),
    maxavg = round(maxavg, digits = 1)
  )
kuga_jan <- subset(test_kuga, month == "Jan")
kuga_feb <- subset(test_kuga, month == "Feb")
kuga_mar <- subset(test_kuga, month == "Mar")
kuga_apr <- subset(test_kuga, month == "Apr")
kuga_may <- subset(test_kuga, month == "May")
kuga_jun <- subset(test_kuga, month == "Jun")
kuga_jul <- subset(test_kuga, month == "Jul")
kuga_aug <- subset(test_kuga, month == "Aug")
kuga_sep <- subset(test_kuga, month == "Sep")
kuga_oct <- subset(test_kuga, month == "Oct")
kuga_nov <- subset(test_kuga, month == "Nov")
kuga_dec <- subset(test_kuga, month == "Dec")
kuga_jan <- kuga_jan %>%
  mutate(
    maxavg = maxtemp-kuga_avg$maxavg[1],
    minavg = mintemp-kuga_avg$minavg[1]
  )
kuga_feb <- kuga_feb %>%
  mutate(
    maxavg = maxtemp-kuga_avg$maxavg[2],
    minavg = mintemp-kuga_avg$minavg[2]
  )
kuga_mar <- kuga_mar %>%
  mutate(
    maxavg = maxtemp-kuga_avg$maxavg[3],
    minavg = mintemp-kuga_avg$minavg[3]
  )
kuga_apr <- kuga_apr %>%
  mutate(
    maxavg = maxtemp-kuga_avg$maxavg[4],
    minavg = mintemp-kuga_avg$minavg[4]
  )
kuga_may <- kuga_may %>%
  mutate(
    maxavg = maxtemp-kuga_avg$maxavg[5],
    minavg = mintemp-kuga_avg$minavg[5]
  )
kuga_jun <- kuga_jun %>%
  mutate(
    maxavg = maxtemp-kuga_avg$maxavg[6],
    minavg = mintemp-kuga_avg$minavg[6]
  )
kuga_jul <- kuga_jul %>%
  mutate(
    maxavg = maxtemp-kuga_avg$maxavg[7],
    minavg = mintemp-kuga_avg$minavg[7]
  )
kuga_aug <- kuga_aug %>%
  mutate(
    maxavg = maxtemp-kuga_avg$maxavg[8],
    minavg = mintemp-kuga_avg$minavg[8]
  )
kuga_sep <- kuga_sep %>%
  mutate(
    maxavg = maxtemp-kuga_avg$maxavg[9],
    minavg = mintemp-kuga_avg$minavg[9]
  )
kuga_oct <- kuga_oct %>%
  mutate(
    maxavg = maxtemp-kuga_avg$maxavg[10],
    minavg = mintemp-kuga_avg$minavg[10]
  )
kuga_nov <- kuga_nov %>%
  mutate(
    maxavg = maxtemp-kuga_avg$maxavg[11],
    minavg = mintemp-kuga_avg$minavg[11]
  )
kuga_dec <- kuga_dec %>%
  mutate(
    maxavg = maxtemp-kuga_avg$maxavg[12],
    minavg = mintemp-kuga_avg$minavg[12]
  )
kuga_data <- rbind(kuga_jan,kuga_feb,kuga_mar,kuga_apr,kuga_may,kuga_jun,kuga_jul,kuga_aug,kuga_sep,kuga_oct,kuga_nov,kuga_dec)
rm(kuga_jan,kuga_feb,kuga_mar,kuga_apr,kuga_may,kuga_jun,kuga_jul,kuga_aug,kuga_sep,kuga_oct,kuga_nov,kuga_dec)
kuga_data <- kuga_data %>%
  arrange(year,month)

# Let's make some cool graphs with those differences ----------------------------------------------------------
