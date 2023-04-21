# Weather average by month script

### Load libraries ----------------------------------------------------------
library(tidyverse)
library(ggplot2)
library(weathercan)
library(lubridate)
library(gganimate)
library(dplyr)
library(RColorBrewer)

### Load Data ----------------------------------------------------------
cbay <- read_csv("data/en_climate_monthly_CBAY_1949-2023.csv")
kugl <- read_csv("data/en_climate_monthly_KUGL_1953-2023.csv")
gjoa <- read_csv("data/en_climate_monthly_GJOA_1985-2023.csv")
kuga <- read_csv("data/en_climate_monthly_KUGA_1957-2023.csv")

# This script is a little different than the previous one. Rather than generating a GIF, we just want a plain old graph.
# However, within the chart we want to plot the average temperature per decade.

# Start with Cambridge Bay ----
# This is what I was missing.
test_cbay <- cbay %>%
  mutate(year = year(cbay$date),
         month = month(cbay$date),
         month = month.abb[month],
         maxtemp = cbay$maxtemp,
         mintemp = cbay$mintemp) %>%
  select(year,month,maxtemp,mintemp) %>%
  mutate(month = factor(month, levels = month.abb))

# Start with the 1950's
# Start with Jan
target_50_cbay_jan <- test_cbay %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1950 & year <= 1959) %>%
  filter(month == "Jan") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_50_cbay_feb <- test_cbay %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1950 & year <= 1959) %>%
  filter(month == "Feb") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_50_cbay_mar <- test_cbay %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1950 & year <= 1959) %>%
  filter(month == "Mar") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_50_cbay_apr <- test_cbay %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1950 & year <= 1959) %>%
  filter(month == "Apr") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_50_cbay_may <- test_cbay %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1950 & year <= 1959) %>%
  filter(month == "May") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_50_cbay_jun <- test_cbay %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1950 & year <= 1959) %>%
  filter(month == "Jun") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_50_cbay_jul <- test_cbay %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1950 & year <= 1959) %>%
  filter(month == "Jul") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_50_cbay_aug <- test_cbay %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1950 & year <= 1959) %>%
  filter(month == "Aug") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_50_cbay_sep <- test_cbay %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1950 & year <= 1959) %>%
  filter(month == "Sep") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_50_cbay_oct <- test_cbay %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1950 & year <= 1959) %>%
  filter(month == "Oct") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_50_cbay_nov <- test_cbay %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1950 & year <= 1959) %>%
  filter(month == "Nov") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_50_cbay_dec <- test_cbay %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1950 & year <= 1959) %>%
  filter(month == "Dec") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )

# Let's do the 1960's next.
# Start with Jan
target_60_cbay_jan <- test_cbay %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1960 & year <= 1969) %>%
  filter(month == "Jan") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_60_cbay_feb <- test_cbay %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1960 & year <= 1969) %>%
  filter(month == "Feb") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_60_cbay_mar <- test_cbay %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1960 & year <= 1969) %>%
  filter(month == "Mar") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_60_cbay_apr <- test_cbay %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1960 & year <= 1969) %>%
  filter(month == "Apr") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_60_cbay_may <- test_cbay %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1960 & year <= 1969) %>%
  filter(month == "May") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_60_cbay_jun <- test_cbay %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1960 & year <= 1969) %>%
  filter(month == "Jun") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_60_cbay_jul <- test_cbay %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1960 & year <= 1969) %>%
  filter(month == "Jul") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_60_cbay_aug <- test_cbay %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1960 & year <= 1969) %>%
  filter(month == "Aug") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_60_cbay_sep <- test_cbay %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1960 & year <= 1969) %>%
  filter(month == "Sep") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_60_cbay_oct <- test_cbay %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1960 & year <= 1969) %>%
  filter(month == "Oct") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_60_cbay_nov <- test_cbay %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1960 & year <= 1969) %>%
  filter(month == "Nov") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_60_cbay_dec <- test_cbay %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1960 & year <= 1969) %>%
  filter(month == "Dec") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )

# Now let's do the 1970's
# Start with Jan
target_70_cbay_jan <- test_cbay %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1970 & year <= 1979) %>%
  filter(month == "Jan") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_70_cbay_feb <- test_cbay %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1970 & year <= 1979) %>%
  filter(month == "Feb") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_70_cbay_mar <- test_cbay %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1970 & year <= 1979) %>%
  filter(month == "Mar") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_70_cbay_apr <- test_cbay %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1970 & year <= 1979) %>%
  filter(month == "Apr") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_70_cbay_may <- test_cbay %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1970 & year <= 1979) %>%
  filter(month == "May") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_70_cbay_jun <- test_cbay %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1970 & year <= 1979) %>%
  filter(month == "Jun") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_70_cbay_jul <- test_cbay %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1970 & year <= 1979) %>%
  filter(month == "Jul") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_70_cbay_aug <- test_cbay %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1970 & year <= 1979) %>%
  filter(month == "Aug") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_70_cbay_sep <- test_cbay %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1970 & year <= 1979) %>%
  filter(month == "Sep") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_70_cbay_oct <- test_cbay %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1970 & year <= 1979) %>%
  filter(month == "Oct") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_70_cbay_nov <- test_cbay %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1970 & year <= 1979) %>%
  filter(month == "Nov") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_70_cbay_dec <- test_cbay %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1970 & year <= 1979) %>%
  filter(month == "Dec") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )

# Now let's do the 1980's
# Start with Jan
target_80_cbay_jan <- test_cbay %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1980 & year <= 1989) %>%
  filter(month == "Jan") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_80_cbay_feb <- test_cbay %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1980 & year <= 1989) %>%
  filter(month == "Feb") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_80_cbay_mar <- test_cbay %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1980 & year <= 1989) %>%
  filter(month == "Mar") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_80_cbay_apr <- test_cbay %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1980 & year <= 1989) %>%
  filter(month == "Apr") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_80_cbay_may <- test_cbay %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1980 & year <= 1989) %>%
  filter(month == "May") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_80_cbay_jun <- test_cbay %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1980 & year <= 1989) %>%
  filter(month == "Jun") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_80_cbay_jul <- test_cbay %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1980 & year <= 1989) %>%
  filter(month == "Jul") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_80_cbay_aug <- test_cbay %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1980 & year <= 1989) %>%
  filter(month == "Aug") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_80_cbay_sep <- test_cbay %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1980 & year <= 1989) %>%
  filter(month == "Sep") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_80_cbay_oct <- test_cbay %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1980 & year <= 1989) %>%
  filter(month == "Oct") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_80_cbay_nov <- test_cbay %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1980 & year <= 1989) %>%
  filter(month == "Nov") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_80_cbay_dec <- test_cbay %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1980 & year <= 1989) %>%
  filter(month == "Dec") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )

# Right, now let's do the 90's baby!
# Start with Jan
target_90_cbay_jan <- test_cbay %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1990 & year <= 1999) %>%
  filter(month == "Jan") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_90_cbay_feb <- test_cbay %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1990 & year <= 1999) %>%
  filter(month == "Feb") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_90_cbay_mar <- test_cbay %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1990 & year <= 1999) %>%
  filter(month == "Mar") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_90_cbay_apr <- test_cbay %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1990 & year <= 1999) %>%
  filter(month == "Apr") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_90_cbay_may <- test_cbay %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1990 & year <= 1999) %>%
  filter(month == "May") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_90_cbay_jun <- test_cbay %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1990 & year <= 1999) %>%
  filter(month == "Jun") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_90_cbay_jul <- test_cbay %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1990 & year <= 1999) %>%
  filter(month == "Jul") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_90_cbay_aug <- test_cbay %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1990 & year <= 1999) %>%
  filter(month == "Aug") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_90_cbay_sep <- test_cbay %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1990 & year <= 1999) %>%
  filter(month == "Sep") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_90_cbay_oct <- test_cbay %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1990 & year <= 1999) %>%
  filter(month == "Oct") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_90_cbay_nov <- test_cbay %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1990 & year <= 1999) %>%
  filter(month == "Nov") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_90_cbay_dec <- test_cbay %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1990 & year <= 1999) %>%
  filter(month == "Dec") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )

# Fabulous. Now let's do the 2000's
# Start with Jan
target_00_cbay_jan <- test_cbay %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 2000 & year <= 2009) %>%
  filter(month == "Jan") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_00_cbay_feb <- test_cbay %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 2000 & year <= 2009) %>%
  filter(month == "Feb") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_00_cbay_mar <- test_cbay %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 2000 & year <= 2009) %>%
  filter(month == "Mar") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_00_cbay_apr <- test_cbay %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 2000 & year <= 2009) %>%
  filter(month == "Apr") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_00_cbay_may <- test_cbay %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 2000 & year <= 2009) %>%
  filter(month == "May") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_00_cbay_jun <- test_cbay %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 2000 & year <= 2009) %>%
  filter(month == "Jun") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_00_cbay_jul <- test_cbay %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 2000 & year <= 2009) %>%
  filter(month == "Jul") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_00_cbay_aug <- test_cbay %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 2000 & year <= 2009) %>%
  filter(month == "Aug") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_00_cbay_sep <- test_cbay %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 2000 & year <= 2009) %>%
  filter(month == "Sep") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_00_cbay_oct <- test_cbay %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 2000 & year <= 2009) %>%
  filter(month == "Oct") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_00_cbay_nov <- test_cbay %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 2000 & year <= 2009) %>%
  filter(month == "Nov") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_00_cbay_dec <- test_cbay %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 2000 & year <= 2009) %>%
  filter(month == "Dec") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )

# Now the 2010's
# Start with Jan
target_10_cbay_jan <- test_cbay %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 2010 & year <= 2019) %>%
  filter(month == "Jan") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_10_cbay_feb <- test_cbay %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 2010 & year <= 2019) %>%
  filter(month == "Feb") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_10_cbay_mar <- test_cbay %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 2010 & year <= 2019) %>%
  filter(month == "Mar") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_10_cbay_apr <- test_cbay %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 2010 & year <= 2019) %>%
  filter(month == "Apr") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_10_cbay_may <- test_cbay %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 2010 & year <= 2019) %>%
  filter(month == "May") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_10_cbay_jun <- test_cbay %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 2010 & year <= 2019) %>%
  filter(month == "Jun") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_10_cbay_jul <- test_cbay %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 2010 & year <= 2019) %>%
  filter(month == "Jul") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_10_cbay_aug <- test_cbay %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 2010 & year <= 2019) %>%
  filter(month == "Aug") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_10_cbay_sep <- test_cbay %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 2010 & year <= 2019) %>%
  filter(month == "Sep") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_10_cbay_oct <- test_cbay %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 2010 & year <= 2019) %>%
  filter(month == "Oct") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_10_cbay_nov <- test_cbay %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 2010 & year <= 2019) %>%
  filter(month == "Nov") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_10_cbay_dec <- test_cbay %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 2010 & year <= 2019) %>%
  filter(month == "Dec") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )

# Great. That's all done. What next? Well, we combine the averages.
# Starting with the 1950s'
maxavg_cbay_50 <- data.frame(
  Jan = c(target_50_cbay_jan$maxavg[1]),
  Feb = c(target_50_cbay_feb$maxavg[1]),
  Mar = c(target_50_cbay_mar$maxavg[1]),
  Apr = c(target_50_cbay_apr$maxavg[1]),
  May = c(target_50_cbay_may$maxavg[1]),
  Jun = c(target_50_cbay_jun$maxavg[1]),
  Jul = c(target_50_cbay_jul$maxavg[1]),
  Aug = c(target_50_cbay_aug$maxavg[1]),
  Sep = c(target_50_cbay_sep$maxavg[1]),
  Oct = c(target_50_cbay_oct$maxavg[1]),
  Nov = c(target_50_cbay_nov$maxavg[1]),
  Dec = c(target_50_cbay_dec$maxavg[1])
) %>%
  pivot_longer(-0,names_to = "month", values_to = "maxavg")

# Let's do the same with the min averages
minavg_cbay_50 <- data.frame(
  Jan = c(target_50_cbay_jan$minavg[1]),
  Feb = c(target_50_cbay_feb$minavg[1]),
  Mar = c(target_50_cbay_mar$minavg[1]),
  Apr = c(target_50_cbay_apr$minavg[1]),
  May = c(target_50_cbay_may$minavg[1]),
  Jun = c(target_50_cbay_jun$minavg[1]),
  Jul = c(target_50_cbay_jul$minavg[1]),
  Aug = c(target_50_cbay_aug$minavg[1]),
  Sep = c(target_50_cbay_sep$minavg[1]),
  Oct = c(target_50_cbay_oct$minavg[1]),
  Nov = c(target_50_cbay_nov$minavg[1]),
  Dec = c(target_50_cbay_dec$minavg[1])
) %>%
  pivot_longer(-0,names_to = "month", values_to = "minavg")

# Next, let's do the 1960's
maxavg_cbay_60 <- data.frame(
  Jan = c(target_60_cbay_jan$maxavg[1]),
  Feb = c(target_60_cbay_feb$maxavg[1]),
  Mar = c(target_60_cbay_mar$maxavg[1]),
  Apr = c(target_60_cbay_apr$maxavg[1]),
  May = c(target_60_cbay_may$maxavg[1]),
  Jun = c(target_60_cbay_jun$maxavg[1]),
  Jul = c(target_60_cbay_jul$maxavg[1]),
  Aug = c(target_60_cbay_aug$maxavg[1]),
  Sep = c(target_60_cbay_sep$maxavg[1]),
  Oct = c(target_60_cbay_oct$maxavg[1]),
  Nov = c(target_60_cbay_nov$maxavg[1]),
  Dec = c(target_60_cbay_dec$maxavg[1])
) %>%
  pivot_longer(-0,names_to = "month", values_to = "maxavg")

# Let's do the same with the min averages
minavg_cbay_60 <- data.frame(
  Jan = c(target_60_cbay_jan$minavg[1]),
  Feb = c(target_60_cbay_feb$minavg[1]),
  Mar = c(target_60_cbay_mar$minavg[1]),
  Apr = c(target_60_cbay_apr$minavg[1]),
  May = c(target_60_cbay_may$minavg[1]),
  Jun = c(target_60_cbay_jun$minavg[1]),
  Jul = c(target_60_cbay_jul$minavg[1]),
  Aug = c(target_60_cbay_aug$minavg[1]),
  Sep = c(target_60_cbay_sep$minavg[1]),
  Oct = c(target_60_cbay_oct$minavg[1]),
  Nov = c(target_60_cbay_nov$minavg[1]),
  Dec = c(target_60_cbay_dec$minavg[1])
) %>%
  pivot_longer(-0,names_to = "month", values_to = "minavg")

# Next, the 1970's
maxavg_cbay_70 <- data.frame(
  Jan = c(target_70_cbay_jan$maxavg[1]),
  Feb = c(target_70_cbay_feb$maxavg[1]),
  Mar = c(target_70_cbay_mar$maxavg[1]),
  Apr = c(target_70_cbay_apr$maxavg[1]),
  May = c(target_70_cbay_may$maxavg[1]),
  Jun = c(target_70_cbay_jun$maxavg[1]),
  Jul = c(target_70_cbay_jul$maxavg[1]),
  Aug = c(target_70_cbay_aug$maxavg[1]),
  Sep = c(target_70_cbay_sep$maxavg[1]),
  Oct = c(target_70_cbay_oct$maxavg[1]),
  Nov = c(target_70_cbay_nov$maxavg[1]),
  Dec = c(target_70_cbay_dec$maxavg[1])
) %>%
  pivot_longer(-0,names_to = "month", values_to = "maxavg")

# Let's do the same with the min averages
minavg_cbay_70 <- data.frame(
  Jan = c(target_70_cbay_jan$minavg[1]),
  Feb = c(target_70_cbay_feb$minavg[1]),
  Mar = c(target_70_cbay_mar$minavg[1]),
  Apr = c(target_70_cbay_apr$minavg[1]),
  May = c(target_70_cbay_may$minavg[1]),
  Jun = c(target_70_cbay_jun$minavg[1]),
  Jul = c(target_70_cbay_jul$minavg[1]),
  Aug = c(target_70_cbay_aug$minavg[1]),
  Sep = c(target_70_cbay_sep$minavg[1]),
  Oct = c(target_70_cbay_oct$minavg[1]),
  Nov = c(target_70_cbay_nov$minavg[1]),
  Dec = c(target_70_cbay_dec$minavg[1])
) %>%
  pivot_longer(-0,names_to = "month", values_to = "minavg")

# Next, the 1980's
maxavg_cbay_80 <- data.frame(
  Jan = c(target_80_cbay_jan$maxavg[1]),
  Feb = c(target_80_cbay_feb$maxavg[1]),
  Mar = c(target_80_cbay_mar$maxavg[1]),
  Apr = c(target_80_cbay_apr$maxavg[1]),
  May = c(target_80_cbay_may$maxavg[1]),
  Jun = c(target_80_cbay_jun$maxavg[1]),
  Jul = c(target_80_cbay_jul$maxavg[1]),
  Aug = c(target_80_cbay_aug$maxavg[1]),
  Sep = c(target_80_cbay_sep$maxavg[1]),
  Oct = c(target_80_cbay_oct$maxavg[1]),
  Nov = c(target_80_cbay_nov$maxavg[1]),
  Dec = c(target_80_cbay_dec$maxavg[1])
) %>%
  pivot_longer(-0,names_to = "month", values_to = "maxavg")

# Let's do the same with the min averages
minavg_cbay_80 <- data.frame(
  Jan = c(target_80_cbay_jan$minavg[1]),
  Feb = c(target_80_cbay_feb$minavg[1]),
  Mar = c(target_80_cbay_mar$minavg[1]),
  Apr = c(target_80_cbay_apr$minavg[1]),
  May = c(target_80_cbay_may$minavg[1]),
  Jun = c(target_80_cbay_jun$minavg[1]),
  Jul = c(target_80_cbay_jul$minavg[1]),
  Aug = c(target_80_cbay_aug$minavg[1]),
  Sep = c(target_80_cbay_sep$minavg[1]),
  Oct = c(target_80_cbay_oct$minavg[1]),
  Nov = c(target_80_cbay_nov$minavg[1]),
  Dec = c(target_80_cbay_dec$minavg[1])
) %>%
  pivot_longer(-0,names_to = "month", values_to = "minavg")

# Next, the 1990's
maxavg_cbay_90 <- data.frame(
  Jan = c(target_90_cbay_jan$maxavg[1]),
  Feb = c(target_90_cbay_feb$maxavg[1]),
  Mar = c(target_90_cbay_mar$maxavg[1]),
  Apr = c(target_90_cbay_apr$maxavg[1]),
  May = c(target_90_cbay_may$maxavg[1]),
  Jun = c(target_90_cbay_jun$maxavg[1]),
  Jul = c(target_90_cbay_jul$maxavg[1]),
  Aug = c(target_90_cbay_aug$maxavg[1]),
  Sep = c(target_90_cbay_sep$maxavg[1]),
  Oct = c(target_90_cbay_oct$maxavg[1]),
  Nov = c(target_90_cbay_nov$maxavg[1]),
  Dec = c(target_90_cbay_dec$maxavg[1])
) %>%
  pivot_longer(-0,names_to = "month", values_to = "maxavg")

# Let's do the same with the min averages
minavg_cbay_90 <- data.frame(
  Jan = c(target_90_cbay_jan$minavg[1]),
  Feb = c(target_90_cbay_feb$minavg[1]),
  Mar = c(target_90_cbay_mar$minavg[1]),
  Apr = c(target_90_cbay_apr$minavg[1]),
  May = c(target_90_cbay_may$minavg[1]),
  Jun = c(target_90_cbay_jun$minavg[1]),
  Jul = c(target_90_cbay_jul$minavg[1]),
  Aug = c(target_90_cbay_aug$minavg[1]),
  Sep = c(target_90_cbay_sep$minavg[1]),
  Oct = c(target_90_cbay_oct$minavg[1]),
  Nov = c(target_90_cbay_nov$minavg[1]),
  Dec = c(target_90_cbay_dec$minavg[1])
) %>%
  pivot_longer(-0,names_to = "month", values_to = "minavg")

# Next, the 2000's
maxavg_cbay_00 <- data.frame(
  Jan = c(target_00_cbay_jan$maxavg[1]),
  Feb = c(target_00_cbay_feb$maxavg[1]),
  Mar = c(target_00_cbay_mar$maxavg[1]),
  Apr = c(target_00_cbay_apr$maxavg[1]),
  May = c(target_00_cbay_may$maxavg[1]),
  Jun = c(target_00_cbay_jun$maxavg[1]),
  Jul = c(target_00_cbay_jul$maxavg[1]),
  Aug = c(target_00_cbay_aug$maxavg[1]),
  Sep = c(target_00_cbay_sep$maxavg[1]),
  Oct = c(target_00_cbay_oct$maxavg[1]),
  Nov = c(target_00_cbay_nov$maxavg[1]),
  Dec = c(target_00_cbay_dec$maxavg[1])
) %>%
  pivot_longer(-0,names_to = "month", values_to = "maxavg")

# Let's do the same with the min averages
minavg_cbay_00 <- data.frame(
  Jan = c(target_00_cbay_jan$minavg[1]),
  Feb = c(target_00_cbay_feb$minavg[1]),
  Mar = c(target_00_cbay_mar$minavg[1]),
  Apr = c(target_00_cbay_apr$minavg[1]),
  May = c(target_00_cbay_may$minavg[1]),
  Jun = c(target_00_cbay_jun$minavg[1]),
  Jul = c(target_00_cbay_jul$minavg[1]),
  Aug = c(target_00_cbay_aug$minavg[1]),
  Sep = c(target_00_cbay_sep$minavg[1]),
  Oct = c(target_00_cbay_oct$minavg[1]),
  Nov = c(target_00_cbay_nov$minavg[1]),
  Dec = c(target_00_cbay_dec$minavg[1])
) %>%
  pivot_longer(-0,names_to = "month", values_to = "minavg")

# Next, the 2010's
maxavg_cbay_10 <- data.frame(
  Jan = c(target_10_cbay_jan$maxavg[1]),
  Feb = c(target_10_cbay_feb$maxavg[1]),
  Mar = c(target_10_cbay_mar$maxavg[1]),
  Apr = c(target_10_cbay_apr$maxavg[1]),
  May = c(target_10_cbay_may$maxavg[1]),
  Jun = c(target_10_cbay_jun$maxavg[1]),
  Jul = c(target_10_cbay_jul$maxavg[1]),
  Aug = c(target_10_cbay_aug$maxavg[1]),
  Sep = c(target_10_cbay_sep$maxavg[1]),
  Oct = c(target_10_cbay_oct$maxavg[1]),
  Nov = c(target_10_cbay_nov$maxavg[1]),
  Dec = c(target_10_cbay_dec$maxavg[1])
) %>%
  pivot_longer(-0,names_to = "month", values_to = "maxavg")

# Let's do the same with the min averages
minavg_cbay_10 <- data.frame(
  Jan = c(target_10_cbay_jan$minavg[1]),
  Feb = c(target_10_cbay_feb$minavg[1]),
  Mar = c(target_10_cbay_mar$minavg[1]),
  Apr = c(target_10_cbay_apr$minavg[1]),
  May = c(target_10_cbay_may$minavg[1]),
  Jun = c(target_10_cbay_jun$minavg[1]),
  Jul = c(target_10_cbay_jul$minavg[1]),
  Aug = c(target_10_cbay_aug$minavg[1]),
  Sep = c(target_10_cbay_sep$minavg[1]),
  Oct = c(target_10_cbay_oct$minavg[1]),
  Nov = c(target_10_cbay_nov$minavg[1]),
  Dec = c(target_10_cbay_dec$minavg[1])
) %>%
  pivot_longer(-0,names_to = "month", values_to = "minavg")

# Great, we have our averages. Let's bind 'em together
cbay_50_avg <- inner_join(minavg_cbay_50,maxavg_cbay_50, by="month")
cbay_60_avg <- inner_join(minavg_cbay_60,maxavg_cbay_60, by="month")
cbay_70_avg <- inner_join(minavg_cbay_70,maxavg_cbay_70, by="month")
cbay_80_avg <- inner_join(minavg_cbay_80,maxavg_cbay_80, by="month")
cbay_90_avg <- inner_join(minavg_cbay_90,maxavg_cbay_90, by="month")
cbay_00_avg <- inner_join(minavg_cbay_00,maxavg_cbay_00, by="month")
cbay_10_avg <- inner_join(minavg_cbay_10,maxavg_cbay_10, by="month")

# Let's do some mass garbage collection
rm(list=ls(pattern="target"))
rm(list=ls(pattern="maxavg"))
rm(list=ls(pattern="minavg"))

# Next, Kugluktuk ----
# This is what I was missing.
test_kugl <- kugl %>%
  mutate(year = year(kugl$date),
         month = month(kugl$date),
         month = month.abb[month],
         maxtemp = kugl$maxtemp,
         mintemp = kugl$mintemp) %>%
  select(year,month,maxtemp,mintemp) %>%
  mutate(month = factor(month, levels = month.abb))

# Start with the 1950's
# Start with Jan
target_50_kugl_jan <- test_kugl %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1950 & year <= 1959) %>%
  filter(month == "Jan") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_50_kugl_feb <- test_kugl %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1950 & year <= 1959) %>%
  filter(month == "Feb") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_50_kugl_mar <- test_kugl %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1950 & year <= 1959) %>%
  filter(month == "Mar") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_50_kugl_apr <- test_kugl %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1950 & year <= 1959) %>%
  filter(month == "Apr") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_50_kugl_may <- test_kugl %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1950 & year <= 1959) %>%
  filter(month == "May") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_50_kugl_jun <- test_kugl %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1950 & year <= 1959) %>%
  filter(month == "Jun") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_50_kugl_jul <- test_kugl %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1950 & year <= 1959) %>%
  filter(month == "Jul") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_50_kugl_aug <- test_kugl %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1950 & year <= 1959) %>%
  filter(month == "Aug") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_50_kugl_sep <- test_kugl %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1950 & year <= 1959) %>%
  filter(month == "Sep") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_50_kugl_oct <- test_kugl %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1950 & year <= 1959) %>%
  filter(month == "Oct") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_50_kugl_nov <- test_kugl %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1950 & year <= 1959) %>%
  filter(month == "Nov") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_50_kugl_dec <- test_kugl %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1950 & year <= 1959) %>%
  filter(month == "Dec") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )

# Let's do the 1960's next.
# Start with Jan
target_60_kugl_jan <- test_kugl %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1960 & year <= 1969) %>%
  filter(month == "Jan") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_60_kugl_feb <- test_kugl %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1960 & year <= 1969) %>%
  filter(month == "Feb") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_60_kugl_mar <- test_kugl %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1960 & year <= 1969) %>%
  filter(month == "Mar") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_60_kugl_apr <- test_kugl %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1960 & year <= 1969) %>%
  filter(month == "Apr") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_60_kugl_may <- test_kugl %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1960 & year <= 1969) %>%
  filter(month == "May") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_60_kugl_jun <- test_kugl %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1960 & year <= 1969) %>%
  filter(month == "Jun") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_60_kugl_jul <- test_kugl %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1960 & year <= 1969) %>%
  filter(month == "Jul") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_60_kugl_aug <- test_kugl %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1960 & year <= 1969) %>%
  filter(month == "Aug") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_60_kugl_sep <- test_kugl %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1960 & year <= 1969) %>%
  filter(month == "Sep") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_60_kugl_oct <- test_kugl %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1960 & year <= 1969) %>%
  filter(month == "Oct") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_60_kugl_nov <- test_kugl %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1960 & year <= 1969) %>%
  filter(month == "Nov") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_60_kugl_dec <- test_kugl %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1960 & year <= 1969) %>%
  filter(month == "Dec") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )

# Now let's do the 1970's
# Start with Jan
target_70_kugl_jan <- test_kugl %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1970 & year <= 1979) %>%
  filter(month == "Jan") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_70_kugl_feb <- test_kugl %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1970 & year <= 1979) %>%
  filter(month == "Feb") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_70_kugl_mar <- test_kugl %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1970 & year <= 1979) %>%
  filter(month == "Mar") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_70_kugl_apr <- test_kugl %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1970 & year <= 1979) %>%
  filter(month == "Apr") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_70_kugl_may <- test_kugl %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1970 & year <= 1979) %>%
  filter(month == "May") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_70_kugl_jun <- test_kugl %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1970 & year <= 1979) %>%
  filter(month == "Jun") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_70_kugl_jul <- test_kugl %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1970 & year <= 1979) %>%
  filter(month == "Jul") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_70_kugl_aug <- test_kugl %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1970 & year <= 1979) %>%
  filter(month == "Aug") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_70_kugl_sep <- test_kugl %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1970 & year <= 1979) %>%
  filter(month == "Sep") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_70_kugl_oct <- test_kugl %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1970 & year <= 1979) %>%
  filter(month == "Oct") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_70_kugl_nov <- test_kugl %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1970 & year <= 1979) %>%
  filter(month == "Nov") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_70_kugl_dec <- test_kugl %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1970 & year <= 1979) %>%
  filter(month == "Dec") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )

# Now let's do the 1980's
# Start with Jan
target_80_kugl_jan <- test_kugl %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1980 & year <= 1989) %>%
  filter(month == "Jan") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_80_kugl_feb <- test_kugl %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1980 & year <= 1989) %>%
  filter(month == "Feb") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_80_kugl_mar <- test_kugl %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1980 & year <= 1989) %>%
  filter(month == "Mar") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_80_kugl_apr <- test_kugl %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1980 & year <= 1989) %>%
  filter(month == "Apr") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_80_kugl_may <- test_kugl %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1980 & year <= 1989) %>%
  filter(month == "May") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_80_kugl_jun <- test_kugl %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1980 & year <= 1989) %>%
  filter(month == "Jun") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_80_kugl_jul <- test_kugl %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1980 & year <= 1989) %>%
  filter(month == "Jul") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_80_kugl_aug <- test_kugl %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1980 & year <= 1989) %>%
  filter(month == "Aug") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_80_kugl_sep <- test_kugl %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1980 & year <= 1989) %>%
  filter(month == "Sep") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_80_kugl_oct <- test_kugl %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1980 & year <= 1989) %>%
  filter(month == "Oct") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_80_kugl_nov <- test_kugl %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1980 & year <= 1989) %>%
  filter(month == "Nov") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_80_kugl_dec <- test_kugl %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1980 & year <= 1989) %>%
  filter(month == "Dec") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )

# Right, now let's do the 90's baby!
# Start with Jan
target_90_kugl_jan <- test_kugl %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1990 & year <= 1999) %>%
  filter(month == "Jan") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_90_kugl_feb <- test_kugl %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1990 & year <= 1999) %>%
  filter(month == "Feb") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_90_kugl_mar <- test_kugl %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1990 & year <= 1999) %>%
  filter(month == "Mar") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_90_kugl_apr <- test_kugl %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1990 & year <= 1999) %>%
  filter(month == "Apr") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_90_kugl_may <- test_kugl %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1990 & year <= 1999) %>%
  filter(month == "May") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_90_kugl_jun <- test_kugl %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1990 & year <= 1999) %>%
  filter(month == "Jun") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_90_kugl_jul <- test_kugl %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1990 & year <= 1999) %>%
  filter(month == "Jul") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_90_kugl_aug <- test_kugl %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1990 & year <= 1999) %>%
  filter(month == "Aug") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_90_kugl_sep <- test_kugl %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1990 & year <= 1999) %>%
  filter(month == "Sep") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_90_kugl_oct <- test_kugl %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1990 & year <= 1999) %>%
  filter(month == "Oct") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_90_kugl_nov <- test_kugl %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1990 & year <= 1999) %>%
  filter(month == "Nov") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_90_kugl_dec <- test_kugl %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1990 & year <= 1999) %>%
  filter(month == "Dec") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )

# Fabulous. Now let's do the 2000's
# Start with Jan
target_00_kugl_jan <- test_kugl %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 2000 & year <= 2009) %>%
  filter(month == "Jan") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_00_kugl_feb <- test_kugl %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 2000 & year <= 2009) %>%
  filter(month == "Feb") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_00_kugl_mar <- test_kugl %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 2000 & year <= 2009) %>%
  filter(month == "Mar") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_00_kugl_apr <- test_kugl %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 2000 & year <= 2009) %>%
  filter(month == "Apr") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_00_kugl_may <- test_kugl %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 2000 & year <= 2009) %>%
  filter(month == "May") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_00_kugl_jun <- test_kugl %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 2000 & year <= 2009) %>%
  filter(month == "Jun") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_00_kugl_jul <- test_kugl %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 2000 & year <= 2009) %>%
  filter(month == "Jul") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_00_kugl_aug <- test_kugl %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 2000 & year <= 2009) %>%
  filter(month == "Aug") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_00_kugl_sep <- test_kugl %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 2000 & year <= 2009) %>%
  filter(month == "Sep") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_00_kugl_oct <- test_kugl %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 2000 & year <= 2009) %>%
  filter(month == "Oct") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_00_kugl_nov <- test_kugl %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 2000 & year <= 2009) %>%
  filter(month == "Nov") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_00_kugl_dec <- test_kugl %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 2000 & year <= 2009) %>%
  filter(month == "Dec") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )

# Now the 2010's
# Start with Jan
target_10_kugl_jan <- test_kugl %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 2010 & year <= 2019) %>%
  filter(month == "Jan") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_10_kugl_feb <- test_kugl %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 2010 & year <= 2019) %>%
  filter(month == "Feb") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_10_kugl_mar <- test_kugl %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 2010 & year <= 2019) %>%
  filter(month == "Mar") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_10_kugl_apr <- test_kugl %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 2010 & year <= 2019) %>%
  filter(month == "Apr") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_10_kugl_may <- test_kugl %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 2010 & year <= 2019) %>%
  filter(month == "May") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_10_kugl_jun <- test_kugl %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 2010 & year <= 2019) %>%
  filter(month == "Jun") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_10_kugl_jul <- test_kugl %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 2010 & year <= 2019) %>%
  filter(month == "Jul") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_10_kugl_aug <- test_kugl %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 2010 & year <= 2019) %>%
  filter(month == "Aug") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_10_kugl_sep <- test_kugl %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 2010 & year <= 2019) %>%
  filter(month == "Sep") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_10_kugl_oct <- test_kugl %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 2010 & year <= 2019) %>%
  filter(month == "Oct") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_10_kugl_nov <- test_kugl %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 2010 & year <= 2019) %>%
  filter(month == "Nov") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_10_kugl_dec <- test_kugl %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 2010 & year <= 2019) %>%
  filter(month == "Dec") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )

# Great. That's all done. What next? Well, we combine the averages.
# Starting with the 1950s'
maxavg_kugl_50 <- data.frame(
  Jan = c(target_50_kugl_jan$maxavg[1]),
  Feb = c(target_50_kugl_feb$maxavg[1]),
  Mar = c(target_50_kugl_mar$maxavg[1]),
  Apr = c(target_50_kugl_apr$maxavg[1]),
  May = c(target_50_kugl_may$maxavg[1]),
  Jun = c(target_50_kugl_jun$maxavg[1]),
  Jul = c(target_50_kugl_jul$maxavg[1]),
  Aug = c(target_50_kugl_aug$maxavg[1]),
  Sep = c(target_50_kugl_sep$maxavg[1]),
  Oct = c(target_50_kugl_oct$maxavg[1]),
  Nov = c(target_50_kugl_nov$maxavg[1]),
  Dec = c(target_50_kugl_dec$maxavg[1])
) %>%
  pivot_longer(-0,names_to = "month", values_to = "maxavg")

# Let's do the same with the min averages
minavg_kugl_50 <- data.frame(
  Jan = c(target_50_kugl_jan$minavg[1]),
  Feb = c(target_50_kugl_feb$minavg[1]),
  Mar = c(target_50_kugl_mar$minavg[1]),
  Apr = c(target_50_kugl_apr$minavg[1]),
  May = c(target_50_kugl_may$minavg[1]),
  Jun = c(target_50_kugl_jun$minavg[1]),
  Jul = c(target_50_kugl_jul$minavg[1]),
  Aug = c(target_50_kugl_aug$minavg[1]),
  Sep = c(target_50_kugl_sep$minavg[1]),
  Oct = c(target_50_kugl_oct$minavg[1]),
  Nov = c(target_50_kugl_nov$minavg[1]),
  Dec = c(target_50_kugl_dec$minavg[1])
) %>%
  pivot_longer(-0,names_to = "month", values_to = "minavg")

# Next, let's do the 1960's
maxavg_kugl_60 <- data.frame(
  Jan = c(target_60_kugl_jan$maxavg[1]),
  Feb = c(target_60_kugl_feb$maxavg[1]),
  Mar = c(target_60_kugl_mar$maxavg[1]),
  Apr = c(target_60_kugl_apr$maxavg[1]),
  May = c(target_60_kugl_may$maxavg[1]),
  Jun = c(target_60_kugl_jun$maxavg[1]),
  Jul = c(target_60_kugl_jul$maxavg[1]),
  Aug = c(target_60_kugl_aug$maxavg[1]),
  Sep = c(target_60_kugl_sep$maxavg[1]),
  Oct = c(target_60_kugl_oct$maxavg[1]),
  Nov = c(target_60_kugl_nov$maxavg[1]),
  Dec = c(target_60_kugl_dec$maxavg[1])
) %>%
  pivot_longer(-0,names_to = "month", values_to = "maxavg")

# Let's do the same with the min averages
minavg_kugl_60 <- data.frame(
  Jan = c(target_60_kugl_jan$minavg[1]),
  Feb = c(target_60_kugl_feb$minavg[1]),
  Mar = c(target_60_kugl_mar$minavg[1]),
  Apr = c(target_60_kugl_apr$minavg[1]),
  May = c(target_60_kugl_may$minavg[1]),
  Jun = c(target_60_kugl_jun$minavg[1]),
  Jul = c(target_60_kugl_jul$minavg[1]),
  Aug = c(target_60_kugl_aug$minavg[1]),
  Sep = c(target_60_kugl_sep$minavg[1]),
  Oct = c(target_60_kugl_oct$minavg[1]),
  Nov = c(target_60_kugl_nov$minavg[1]),
  Dec = c(target_60_kugl_dec$minavg[1])
) %>%
  pivot_longer(-0,names_to = "month", values_to = "minavg")

# Next, the 1970's
maxavg_kugl_70 <- data.frame(
  Jan = c(target_70_kugl_jan$maxavg[1]),
  Feb = c(target_70_kugl_feb$maxavg[1]),
  Mar = c(target_70_kugl_mar$maxavg[1]),
  Apr = c(target_70_kugl_apr$maxavg[1]),
  May = c(target_70_kugl_may$maxavg[1]),
  Jun = c(target_70_kugl_jun$maxavg[1]),
  Jul = c(target_70_kugl_jul$maxavg[1]),
  Aug = c(target_70_kugl_aug$maxavg[1]),
  Sep = c(target_70_kugl_sep$maxavg[1]),
  Oct = c(target_70_kugl_oct$maxavg[1]),
  Nov = c(target_70_kugl_nov$maxavg[1]),
  Dec = c(target_70_kugl_dec$maxavg[1])
) %>%
  pivot_longer(-0,names_to = "month", values_to = "maxavg")

# Let's do the same with the min averages
minavg_kugl_70 <- data.frame(
  Jan = c(target_70_kugl_jan$minavg[1]),
  Feb = c(target_70_kugl_feb$minavg[1]),
  Mar = c(target_70_kugl_mar$minavg[1]),
  Apr = c(target_70_kugl_apr$minavg[1]),
  May = c(target_70_kugl_may$minavg[1]),
  Jun = c(target_70_kugl_jun$minavg[1]),
  Jul = c(target_70_kugl_jul$minavg[1]),
  Aug = c(target_70_kugl_aug$minavg[1]),
  Sep = c(target_70_kugl_sep$minavg[1]),
  Oct = c(target_70_kugl_oct$minavg[1]),
  Nov = c(target_70_kugl_nov$minavg[1]),
  Dec = c(target_70_kugl_dec$minavg[1])
) %>%
  pivot_longer(-0,names_to = "month", values_to = "minavg")

# Next, the 1980's
maxavg_kugl_80 <- data.frame(
  Jan = c(target_80_kugl_jan$maxavg[1]),
  Feb = c(target_80_kugl_feb$maxavg[1]),
  Mar = c(target_80_kugl_mar$maxavg[1]),
  Apr = c(target_80_kugl_apr$maxavg[1]),
  May = c(target_80_kugl_may$maxavg[1]),
  Jun = c(target_80_kugl_jun$maxavg[1]),
  Jul = c(target_80_kugl_jul$maxavg[1]),
  Aug = c(target_80_kugl_aug$maxavg[1]),
  Sep = c(target_80_kugl_sep$maxavg[1]),
  Oct = c(target_80_kugl_oct$maxavg[1]),
  Nov = c(target_80_kugl_nov$maxavg[1]),
  Dec = c(target_80_kugl_dec$maxavg[1])
) %>%
  pivot_longer(-0,names_to = "month", values_to = "maxavg")

# Let's do the same with the min averages
minavg_kugl_80 <- data.frame(
  Jan = c(target_80_kugl_jan$minavg[1]),
  Feb = c(target_80_kugl_feb$minavg[1]),
  Mar = c(target_80_kugl_mar$minavg[1]),
  Apr = c(target_80_kugl_apr$minavg[1]),
  May = c(target_80_kugl_may$minavg[1]),
  Jun = c(target_80_kugl_jun$minavg[1]),
  Jul = c(target_80_kugl_jul$minavg[1]),
  Aug = c(target_80_kugl_aug$minavg[1]),
  Sep = c(target_80_kugl_sep$minavg[1]),
  Oct = c(target_80_kugl_oct$minavg[1]),
  Nov = c(target_80_kugl_nov$minavg[1]),
  Dec = c(target_80_kugl_dec$minavg[1])
) %>%
  pivot_longer(-0,names_to = "month", values_to = "minavg")

# Next, the 1990's
maxavg_kugl_90 <- data.frame(
  Jan = c(target_90_kugl_jan$maxavg[1]),
  Feb = c(target_90_kugl_feb$maxavg[1]),
  Mar = c(target_90_kugl_mar$maxavg[1]),
  Apr = c(target_90_kugl_apr$maxavg[1]),
  May = c(target_90_kugl_may$maxavg[1]),
  Jun = c(target_90_kugl_jun$maxavg[1]),
  Jul = c(target_90_kugl_jul$maxavg[1]),
  Aug = c(target_90_kugl_aug$maxavg[1]),
  Sep = c(target_90_kugl_sep$maxavg[1]),
  Oct = c(target_90_kugl_oct$maxavg[1]),
  Nov = c(target_90_kugl_nov$maxavg[1]),
  Dec = c(target_90_kugl_dec$maxavg[1])
) %>%
  pivot_longer(-0,names_to = "month", values_to = "maxavg")

# Let's do the same with the min averages
minavg_kugl_90 <- data.frame(
  Jan = c(target_90_kugl_jan$minavg[1]),
  Feb = c(target_90_kugl_feb$minavg[1]),
  Mar = c(target_90_kugl_mar$minavg[1]),
  Apr = c(target_90_kugl_apr$minavg[1]),
  May = c(target_90_kugl_may$minavg[1]),
  Jun = c(target_90_kugl_jun$minavg[1]),
  Jul = c(target_90_kugl_jul$minavg[1]),
  Aug = c(target_90_kugl_aug$minavg[1]),
  Sep = c(target_90_kugl_sep$minavg[1]),
  Oct = c(target_90_kugl_oct$minavg[1]),
  Nov = c(target_90_kugl_nov$minavg[1]),
  Dec = c(target_90_kugl_dec$minavg[1])
) %>%
  pivot_longer(-0,names_to = "month", values_to = "minavg")

# Next, the 2000's
maxavg_kugl_00 <- data.frame(
  Jan = c(target_00_kugl_jan$maxavg[1]),
  Feb = c(target_00_kugl_feb$maxavg[1]),
  Mar = c(target_00_kugl_mar$maxavg[1]),
  Apr = c(target_00_kugl_apr$maxavg[1]),
  May = c(target_00_kugl_may$maxavg[1]),
  Jun = c(target_00_kugl_jun$maxavg[1]),
  Jul = c(target_00_kugl_jul$maxavg[1]),
  Aug = c(target_00_kugl_aug$maxavg[1]),
  Sep = c(target_00_kugl_sep$maxavg[1]),
  Oct = c(target_00_kugl_oct$maxavg[1]),
  Nov = c(target_00_kugl_nov$maxavg[1]),
  Dec = c(target_00_kugl_dec$maxavg[1])
) %>%
  pivot_longer(-0,names_to = "month", values_to = "maxavg")

# Let's do the same with the min averages
minavg_kugl_00 <- data.frame(
  Jan = c(target_00_kugl_jan$minavg[1]),
  Feb = c(target_00_kugl_feb$minavg[1]),
  Mar = c(target_00_kugl_mar$minavg[1]),
  Apr = c(target_00_kugl_apr$minavg[1]),
  May = c(target_00_kugl_may$minavg[1]),
  Jun = c(target_00_kugl_jun$minavg[1]),
  Jul = c(target_00_kugl_jul$minavg[1]),
  Aug = c(target_00_kugl_aug$minavg[1]),
  Sep = c(target_00_kugl_sep$minavg[1]),
  Oct = c(target_00_kugl_oct$minavg[1]),
  Nov = c(target_00_kugl_nov$minavg[1]),
  Dec = c(target_00_kugl_dec$minavg[1])
) %>%
  pivot_longer(-0,names_to = "month", values_to = "minavg")

# Next, the 2010's
maxavg_kugl_10 <- data.frame(
  Jan = c(target_10_kugl_jan$maxavg[1]),
  Feb = c(target_10_kugl_feb$maxavg[1]),
  Mar = c(target_10_kugl_mar$maxavg[1]),
  Apr = c(target_10_kugl_apr$maxavg[1]),
  May = c(target_10_kugl_may$maxavg[1]),
  Jun = c(target_10_kugl_jun$maxavg[1]),
  Jul = c(target_10_kugl_jul$maxavg[1]),
  Aug = c(target_10_kugl_aug$maxavg[1]),
  Sep = c(target_10_kugl_sep$maxavg[1]),
  Oct = c(target_10_kugl_oct$maxavg[1]),
  Nov = c(target_10_kugl_nov$maxavg[1]),
  Dec = c(target_10_kugl_dec$maxavg[1])
) %>%
  pivot_longer(-0,names_to = "month", values_to = "maxavg")

# Let's do the same with the min averages
minavg_kugl_10 <- data.frame(
  Jan = c(target_10_kugl_jan$minavg[1]),
  Feb = c(target_10_kugl_feb$minavg[1]),
  Mar = c(target_10_kugl_mar$minavg[1]),
  Apr = c(target_10_kugl_apr$minavg[1]),
  May = c(target_10_kugl_may$minavg[1]),
  Jun = c(target_10_kugl_jun$minavg[1]),
  Jul = c(target_10_kugl_jul$minavg[1]),
  Aug = c(target_10_kugl_aug$minavg[1]),
  Sep = c(target_10_kugl_sep$minavg[1]),
  Oct = c(target_10_kugl_oct$minavg[1]),
  Nov = c(target_10_kugl_nov$minavg[1]),
  Dec = c(target_10_kugl_dec$minavg[1])
) %>%
  pivot_longer(-0,names_to = "month", values_to = "minavg")

# Great, we have our averages. Let's bind 'em together
kugl_50_avg <- inner_join(minavg_kugl_50,maxavg_kugl_50, by="month")
kugl_60_avg <- inner_join(minavg_kugl_60,maxavg_kugl_60, by="month")
kugl_70_avg <- inner_join(minavg_kugl_70,maxavg_kugl_70, by="month")
kugl_80_avg <- inner_join(minavg_kugl_80,maxavg_kugl_80, by="month")
kugl_90_avg <- inner_join(minavg_kugl_90,maxavg_kugl_90, by="month")
kugl_00_avg <- inner_join(minavg_kugl_00,maxavg_kugl_00, by="month")
kugl_10_avg <- inner_join(minavg_kugl_10,maxavg_kugl_10, by="month")

# Let's do some mass garbage collection
rm(list=ls(pattern="target"))
rm(list=ls(pattern="maxavg"))
rm(list=ls(pattern="minavg"))

# Next, Gjoa Haven ----
# This is what I was missing.
test_gjoa <- gjoa %>%
  mutate(year = year(gjoa$date),
         month = month(gjoa$date),
         month = month.abb[month],
         maxtemp = gjoa$maxtemp,
         mintemp = gjoa$mintemp) %>%
  select(year,month,maxtemp,mintemp) %>%
  mutate(month = factor(month, levels = month.abb))

# Start with the 1980's
# Start with Jan
target_80_gjoa_jan <- test_gjoa %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1980 & year <= 1989) %>%
  filter(month == "Jan") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_80_gjoa_feb <- test_gjoa %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1980 & year <= 1989) %>%
  filter(month == "Feb") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_80_gjoa_mar <- test_gjoa %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1980 & year <= 1989) %>%
  filter(month == "Mar") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_80_gjoa_apr <- test_gjoa %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1980 & year <= 1989) %>%
  filter(month == "Apr") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_80_gjoa_may <- test_gjoa %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1980 & year <= 1989) %>%
  filter(month == "May") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_80_gjoa_jun <- test_gjoa %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1980 & year <= 1989) %>%
  filter(month == "Jun") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_80_gjoa_jul <- test_gjoa %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1980 & year <= 1989) %>%
  filter(month == "Jul") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_80_gjoa_aug <- test_gjoa %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1980 & year <= 1989) %>%
  filter(month == "Aug") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_80_gjoa_sep <- test_gjoa %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1980 & year <= 1989) %>%
  filter(month == "Sep") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_80_gjoa_oct <- test_gjoa %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1980 & year <= 1989) %>%
  filter(month == "Oct") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_80_gjoa_nov <- test_gjoa %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1980 & year <= 1989) %>%
  filter(month == "Nov") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_80_gjoa_dec <- test_gjoa %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1980 & year <= 1989) %>%
  filter(month == "Dec") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )

# Right, now let's do the 90's baby!
# Start with Jan
target_90_gjoa_jan <- test_gjoa %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1990 & year <= 1999) %>%
  filter(month == "Jan") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_90_gjoa_feb <- test_gjoa %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1990 & year <= 1999) %>%
  filter(month == "Feb") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_90_gjoa_mar <- test_gjoa %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1990 & year <= 1999) %>%
  filter(month == "Mar") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_90_gjoa_apr <- test_gjoa %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1990 & year <= 1999) %>%
  filter(month == "Apr") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_90_gjoa_may <- test_gjoa %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1990 & year <= 1999) %>%
  filter(month == "May") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_90_gjoa_jun <- test_gjoa %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1990 & year <= 1999) %>%
  filter(month == "Jun") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_90_gjoa_jul <- test_gjoa %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1990 & year <= 1999) %>%
  filter(month == "Jul") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_90_gjoa_aug <- test_gjoa %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1990 & year <= 1999) %>%
  filter(month == "Aug") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_90_gjoa_sep <- test_gjoa %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1990 & year <= 1999) %>%
  filter(month == "Sep") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_90_gjoa_oct <- test_gjoa %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1990 & year <= 1999) %>%
  filter(month == "Oct") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_90_gjoa_nov <- test_gjoa %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1990 & year <= 1999) %>%
  filter(month == "Nov") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_90_gjoa_dec <- test_gjoa %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1990 & year <= 1999) %>%
  filter(month == "Dec") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )

# Fabulous. Now let's do the 2000's
# Start with Jan
target_00_gjoa_jan <- test_gjoa %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 2000 & year <= 2009) %>%
  filter(month == "Jan") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_00_gjoa_feb <- test_gjoa %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 2000 & year <= 2009) %>%
  filter(month == "Feb") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_00_gjoa_mar <- test_gjoa %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 2000 & year <= 2009) %>%
  filter(month == "Mar") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_00_gjoa_apr <- test_gjoa %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 2000 & year <= 2009) %>%
  filter(month == "Apr") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_00_gjoa_may <- test_gjoa %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 2000 & year <= 2009) %>%
  filter(month == "May") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_00_gjoa_jun <- test_gjoa %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 2000 & year <= 2009) %>%
  filter(month == "Jun") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_00_gjoa_jul <- test_gjoa %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 2000 & year <= 2009) %>%
  filter(month == "Jul") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_00_gjoa_aug <- test_gjoa %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 2000 & year <= 2009) %>%
  filter(month == "Aug") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_00_gjoa_sep <- test_gjoa %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 2000 & year <= 2009) %>%
  filter(month == "Sep") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_00_gjoa_oct <- test_gjoa %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 2000 & year <= 2009) %>%
  filter(month == "Oct") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_00_gjoa_nov <- test_gjoa %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 2000 & year <= 2009) %>%
  filter(month == "Nov") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_00_gjoa_dec <- test_gjoa %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 2000 & year <= 2009) %>%
  filter(month == "Dec") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )

# Now the 2010's
# Start with Jan
target_10_gjoa_jan <- test_gjoa %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 2010 & year <= 2019) %>%
  filter(month == "Jan") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_10_gjoa_feb <- test_gjoa %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 2010 & year <= 2019) %>%
  filter(month == "Feb") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_10_gjoa_mar <- test_gjoa %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 2010 & year <= 2019) %>%
  filter(month == "Mar") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_10_gjoa_apr <- test_gjoa %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 2010 & year <= 2019) %>%
  filter(month == "Apr") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_10_gjoa_may <- test_gjoa %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 2010 & year <= 2019) %>%
  filter(month == "May") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_10_gjoa_jun <- test_gjoa %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 2010 & year <= 2019) %>%
  filter(month == "Jun") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_10_gjoa_jul <- test_gjoa %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 2010 & year <= 2019) %>%
  filter(month == "Jul") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_10_gjoa_aug <- test_gjoa %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 2010 & year <= 2019) %>%
  filter(month == "Aug") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_10_gjoa_sep <- test_gjoa %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 2010 & year <= 2019) %>%
  filter(month == "Sep") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_10_gjoa_oct <- test_gjoa %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 2010 & year <= 2019) %>%
  filter(month == "Oct") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_10_gjoa_nov <- test_gjoa %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 2010 & year <= 2019) %>%
  filter(month == "Nov") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_10_gjoa_dec <- test_gjoa %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 2010 & year <= 2019) %>%
  filter(month == "Dec") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )

# Great. That's all done. What next? Well, we combine the averages.
# Starting with the 1950s'
# This is what I was missing.
test_gjoa <- gjoa %>%
  mutate(year = year(gjoa$date),
         month = month(gjoa$date),
         month = month.abb[month],
         maxtemp = gjoa$maxtemp,
         mintemp = gjoa$mintemp) %>%
  select(year,month,maxtemp,mintemp) %>%
  mutate(month = factor(month, levels = month.abb))

# Start with the 1980's
# Start with Jan
target_80_gjoa_jan <- test_gjoa %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1980 & year <= 1989) %>%
  filter(month == "Jan") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_80_gjoa_feb <- test_gjoa %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1980 & year <= 1989) %>%
  filter(month == "Feb") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_80_gjoa_mar <- test_gjoa %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1980 & year <= 1989) %>%
  filter(month == "Mar") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_80_gjoa_apr <- test_gjoa %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1980 & year <= 1989) %>%
  filter(month == "Apr") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_80_gjoa_may <- test_gjoa %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1980 & year <= 1989) %>%
  filter(month == "May") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_80_gjoa_jun <- test_gjoa %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1980 & year <= 1989) %>%
  filter(month == "Jun") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_80_gjoa_jul <- test_gjoa %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1980 & year <= 1989) %>%
  filter(month == "Jul") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_80_gjoa_aug <- test_gjoa %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1980 & year <= 1989) %>%
  filter(month == "Aug") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_80_gjoa_sep <- test_gjoa %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1980 & year <= 1989) %>%
  filter(month == "Sep") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_80_gjoa_oct <- test_gjoa %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1980 & year <= 1989) %>%
  filter(month == "Oct") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_80_gjoa_nov <- test_gjoa %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1980 & year <= 1989) %>%
  filter(month == "Nov") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_80_gjoa_dec <- test_gjoa %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1980 & year <= 1989) %>%
  filter(month == "Dec") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )

# Right, now let's do the 90's baby!
# Start with Jan
target_90_gjoa_jan <- test_gjoa %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1990 & year <= 1999) %>%
  filter(month == "Jan") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_90_gjoa_feb <- test_gjoa %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1990 & year <= 1999) %>%
  filter(month == "Feb") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_90_gjoa_mar <- test_gjoa %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1990 & year <= 1999) %>%
  filter(month == "Mar") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_90_gjoa_apr <- test_gjoa %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1990 & year <= 1999) %>%
  filter(month == "Apr") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_90_gjoa_may <- test_gjoa %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1990 & year <= 1999) %>%
  filter(month == "May") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_90_gjoa_jun <- test_gjoa %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1990 & year <= 1999) %>%
  filter(month == "Jun") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_90_gjoa_jul <- test_gjoa %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1990 & year <= 1999) %>%
  filter(month == "Jul") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_90_gjoa_aug <- test_gjoa %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1990 & year <= 1999) %>%
  filter(month == "Aug") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_90_gjoa_sep <- test_gjoa %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1990 & year <= 1999) %>%
  filter(month == "Sep") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_90_gjoa_oct <- test_gjoa %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1990 & year <= 1999) %>%
  filter(month == "Oct") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_90_gjoa_nov <- test_gjoa %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1990 & year <= 1999) %>%
  filter(month == "Nov") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_90_gjoa_dec <- test_gjoa %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1990 & year <= 1999) %>%
  filter(month == "Dec") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )

# Fabulous. Now let's do the 2000's
# Start with Jan
target_00_gjoa_jan <- test_gjoa %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 2000 & year <= 2009) %>%
  filter(month == "Jan") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_00_gjoa_feb <- test_gjoa %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 2000 & year <= 2009) %>%
  filter(month == "Feb") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_00_gjoa_mar <- test_gjoa %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 2000 & year <= 2009) %>%
  filter(month == "Mar") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_00_gjoa_apr <- test_gjoa %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 2000 & year <= 2009) %>%
  filter(month == "Apr") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_00_gjoa_may <- test_gjoa %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 2000 & year <= 2009) %>%
  filter(month == "May") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_00_gjoa_jun <- test_gjoa %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 2000 & year <= 2009) %>%
  filter(month == "Jun") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_00_gjoa_jul <- test_gjoa %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 2000 & year <= 2009) %>%
  filter(month == "Jul") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_00_gjoa_aug <- test_gjoa %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 2000 & year <= 2009) %>%
  filter(month == "Aug") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_00_gjoa_sep <- test_gjoa %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 2000 & year <= 2009) %>%
  filter(month == "Sep") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_00_gjoa_oct <- test_gjoa %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 2000 & year <= 2009) %>%
  filter(month == "Oct") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_00_gjoa_nov <- test_gjoa %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 2000 & year <= 2009) %>%
  filter(month == "Nov") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_00_gjoa_dec <- test_gjoa %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 2000 & year <= 2009) %>%
  filter(month == "Dec") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )

# Now the 2010's
# Start with Jan
target_10_gjoa_jan <- test_gjoa %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 2010 & year <= 2019) %>%
  filter(month == "Jan") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_10_gjoa_feb <- test_gjoa %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 2010 & year <= 2019) %>%
  filter(month == "Feb") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_10_gjoa_mar <- test_gjoa %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 2010 & year <= 2019) %>%
  filter(month == "Mar") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_10_gjoa_apr <- test_gjoa %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 2010 & year <= 2019) %>%
  filter(month == "Apr") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_10_gjoa_may <- test_gjoa %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 2010 & year <= 2019) %>%
  filter(month == "May") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_10_gjoa_jun <- test_gjoa %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 2010 & year <= 2019) %>%
  filter(month == "Jun") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_10_gjoa_jul <- test_gjoa %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 2010 & year <= 2019) %>%
  filter(month == "Jul") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_10_gjoa_aug <- test_gjoa %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 2010 & year <= 2019) %>%
  filter(month == "Aug") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_10_gjoa_sep <- test_gjoa %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 2010 & year <= 2019) %>%
  filter(month == "Sep") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_10_gjoa_oct <- test_gjoa %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 2010 & year <= 2019) %>%
  filter(month == "Oct") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_10_gjoa_nov <- test_gjoa %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 2010 & year <= 2019) %>%
  filter(month == "Nov") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_10_gjoa_dec <- test_gjoa %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 2010 & year <= 2019) %>%
  filter(month == "Dec") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )

# Great. That's all done. What next? Well, we combine the averages.
# Start with the 1980's
maxavg_gjoa_80 <- data.frame(
  Jan = c(target_80_gjoa_jan$maxavg[1]),
  Feb = c(target_80_gjoa_feb$maxavg[1]),
  Mar = c(target_80_gjoa_mar$maxavg[1]),
  Apr = c(target_80_gjoa_apr$maxavg[1]),
  May = c(target_80_gjoa_may$maxavg[1]),
  Jun = c(target_80_gjoa_jun$maxavg[1]),
  Jul = c(target_80_gjoa_jul$maxavg[1]),
  Aug = c(target_80_gjoa_aug$maxavg[1]),
  Sep = c(target_80_gjoa_sep$maxavg[1]),
  Oct = c(target_80_gjoa_oct$maxavg[1]),
  Nov = c(target_80_gjoa_nov$maxavg[1]),
  Dec = c(target_80_gjoa_dec$maxavg[1])
) %>%
  pivot_longer(-0,names_to = "month", values_to = "maxavg")

# Let's do the same with the min averages
minavg_gjoa_80 <- data.frame(
  Jan = c(target_80_gjoa_jan$minavg[1]),
  Feb = c(target_80_gjoa_feb$minavg[1]),
  Mar = c(target_80_gjoa_mar$minavg[1]),
  Apr = c(target_80_gjoa_apr$minavg[1]),
  May = c(target_80_gjoa_may$minavg[1]),
  Jun = c(target_80_gjoa_jun$minavg[1]),
  Jul = c(target_80_gjoa_jul$minavg[1]),
  Aug = c(target_80_gjoa_aug$minavg[1]),
  Sep = c(target_80_gjoa_sep$minavg[1]),
  Oct = c(target_80_gjoa_oct$minavg[1]),
  Nov = c(target_80_gjoa_nov$minavg[1]),
  Dec = c(target_80_gjoa_dec$minavg[1])
) %>%
  pivot_longer(-0,names_to = "month", values_to = "minavg")

# Next, the 1990's
maxavg_gjoa_90 <- data.frame(
  Jan = c(target_90_gjoa_jan$maxavg[1]),
  Feb = c(target_90_gjoa_feb$maxavg[1]),
  Mar = c(target_90_gjoa_mar$maxavg[1]),
  Apr = c(target_90_gjoa_apr$maxavg[1]),
  May = c(target_90_gjoa_may$maxavg[1]),
  Jun = c(target_90_gjoa_jun$maxavg[1]),
  Jul = c(target_90_gjoa_jul$maxavg[1]),
  Aug = c(target_90_gjoa_aug$maxavg[1]),
  Sep = c(target_90_gjoa_sep$maxavg[1]),
  Oct = c(target_90_gjoa_oct$maxavg[1]),
  Nov = c(target_90_gjoa_nov$maxavg[1]),
  Dec = c(target_90_gjoa_dec$maxavg[1])
) %>%
  pivot_longer(-0,names_to = "month", values_to = "maxavg")

# Let's do the same with the min averages
minavg_gjoa_90 <- data.frame(
  Jan = c(target_90_gjoa_jan$minavg[1]),
  Feb = c(target_90_gjoa_feb$minavg[1]),
  Mar = c(target_90_gjoa_mar$minavg[1]),
  Apr = c(target_90_gjoa_apr$minavg[1]),
  May = c(target_90_gjoa_may$minavg[1]),
  Jun = c(target_90_gjoa_jun$minavg[1]),
  Jul = c(target_90_gjoa_jul$minavg[1]),
  Aug = c(target_90_gjoa_aug$minavg[1]),
  Sep = c(target_90_gjoa_sep$minavg[1]),
  Oct = c(target_90_gjoa_oct$minavg[1]),
  Nov = c(target_90_gjoa_nov$minavg[1]),
  Dec = c(target_90_gjoa_dec$minavg[1])
) %>%
  pivot_longer(-0,names_to = "month", values_to = "minavg")

# Next, the 2000's
maxavg_gjoa_00 <- data.frame(
  Jan = c(target_00_gjoa_jan$maxavg[1]),
  Feb = c(target_00_gjoa_feb$maxavg[1]),
  Mar = c(target_00_gjoa_mar$maxavg[1]),
  Apr = c(target_00_gjoa_apr$maxavg[1]),
  May = c(target_00_gjoa_may$maxavg[1]),
  Jun = c(target_00_gjoa_jun$maxavg[1]),
  Jul = c(target_00_gjoa_jul$maxavg[1]),
  Aug = c(target_00_gjoa_aug$maxavg[1]),
  Sep = c(target_00_gjoa_sep$maxavg[1]),
  Oct = c(target_00_gjoa_oct$maxavg[1]),
  Nov = c(target_00_gjoa_nov$maxavg[1]),
  Dec = c(target_00_gjoa_dec$maxavg[1])
) %>%
  pivot_longer(-0,names_to = "month", values_to = "maxavg")

# Let's do the same with the min averages
minavg_gjoa_00 <- data.frame(
  Jan = c(target_00_gjoa_jan$minavg[1]),
  Feb = c(target_00_gjoa_feb$minavg[1]),
  Mar = c(target_00_gjoa_mar$minavg[1]),
  Apr = c(target_00_gjoa_apr$minavg[1]),
  May = c(target_00_gjoa_may$minavg[1]),
  Jun = c(target_00_gjoa_jun$minavg[1]),
  Jul = c(target_00_gjoa_jul$minavg[1]),
  Aug = c(target_00_gjoa_aug$minavg[1]),
  Sep = c(target_00_gjoa_sep$minavg[1]),
  Oct = c(target_00_gjoa_oct$minavg[1]),
  Nov = c(target_00_gjoa_nov$minavg[1]),
  Dec = c(target_00_gjoa_dec$minavg[1])
) %>%
  pivot_longer(-0,names_to = "month", values_to = "minavg")

# Next, the 2010's
maxavg_gjoa_10 <- data.frame(
  Jan = c(target_10_gjoa_jan$maxavg[1]),
  Feb = c(target_10_gjoa_feb$maxavg[1]),
  Mar = c(target_10_gjoa_mar$maxavg[1]),
  Apr = c(target_10_gjoa_apr$maxavg[1]),
  May = c(target_10_gjoa_may$maxavg[1]),
  Jun = c(target_10_gjoa_jun$maxavg[1]),
  Jul = c(target_10_gjoa_jul$maxavg[1]),
  Aug = c(target_10_gjoa_aug$maxavg[1]),
  Sep = c(target_10_gjoa_sep$maxavg[1]),
  Oct = c(target_10_gjoa_oct$maxavg[1]),
  Nov = c(target_10_gjoa_nov$maxavg[1]),
  Dec = c(target_10_gjoa_dec$maxavg[1])
) %>%
  pivot_longer(-0,names_to = "month", values_to = "maxavg")

# Let's do the same with the min averages
minavg_gjoa_10 <- data.frame(
  Jan = c(target_10_gjoa_jan$minavg[1]),
  Feb = c(target_10_gjoa_feb$minavg[1]),
  Mar = c(target_10_gjoa_mar$minavg[1]),
  Apr = c(target_10_gjoa_apr$minavg[1]),
  May = c(target_10_gjoa_may$minavg[1]),
  Jun = c(target_10_gjoa_jun$minavg[1]),
  Jul = c(target_10_gjoa_jul$minavg[1]),
  Aug = c(target_10_gjoa_aug$minavg[1]),
  Sep = c(target_10_gjoa_sep$minavg[1]),
  Oct = c(target_10_gjoa_oct$minavg[1]),
  Nov = c(target_10_gjoa_nov$minavg[1]),
  Dec = c(target_10_gjoa_dec$minavg[1])
) %>%
  pivot_longer(-0,names_to = "month", values_to = "minavg")

# Great, we have our averages. Let's bind 'em together
gjoa_80_avg <- inner_join(minavg_gjoa_80,maxavg_gjoa_80, by="month")
gjoa_90_avg <- inner_join(minavg_gjoa_90,maxavg_gjoa_90, by="month")
gjoa_00_avg <- inner_join(minavg_gjoa_00,maxavg_gjoa_00, by="month")
gjoa_10_avg <- inner_join(minavg_gjoa_10,maxavg_gjoa_10, by="month")

# Let's do some mass garbage collection
rm(list=ls(pattern="target"))
rm(list=ls(pattern="maxavg"))
rm(list=ls(pattern="minavg"))

# Lastly, do Kugaaruk ----
# This is what I was missing.
test_kuga <- kuga %>%
  mutate(year = year(kuga$date),
         month = month(kuga$date),
         month = month.abb[month],
         maxtemp = kuga$maxtemp,
         mintemp = kuga$mintemp) %>%
  select(year,month,maxtemp,mintemp) %>%
  mutate(month = factor(month, levels = month.abb))

# Start with the 1950's
# Start with Jan
target_50_kuga_jan <- test_kuga %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1950 & year <= 1959) %>%
  filter(month == "Jan") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_50_kuga_feb <- test_kuga %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1950 & year <= 1959) %>%
  filter(month == "Feb") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_50_kuga_mar <- test_kuga %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1950 & year <= 1959) %>%
  filter(month == "Mar") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_50_kuga_apr <- test_kuga %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1950 & year <= 1959) %>%
  filter(month == "Apr") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_50_kuga_may <- test_kuga %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1950 & year <= 1959) %>%
  filter(month == "May") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_50_kuga_jun <- test_kuga %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1950 & year <= 1959) %>%
  filter(month == "Jun") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_50_kuga_jul <- test_kuga %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1950 & year <= 1959) %>%
  filter(month == "Jul") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_50_kuga_aug <- test_kuga %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1950 & year <= 1959) %>%
  filter(month == "Aug") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_50_kuga_sep <- test_kuga %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1950 & year <= 1959) %>%
  filter(month == "Sep") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_50_kuga_oct <- test_kuga %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1950 & year <= 1959) %>%
  filter(month == "Oct") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_50_kuga_nov <- test_kuga %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1950 & year <= 1959) %>%
  filter(month == "Nov") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_50_kuga_dec <- test_kuga %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1950 & year <= 1959) %>%
  filter(month == "Dec") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )

# Let's do the 1960's next.
# Start with Jan
target_60_kuga_jan <- test_kuga %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1960 & year <= 1969) %>%
  filter(month == "Jan") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_60_kuga_feb <- test_kuga %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1960 & year <= 1969) %>%
  filter(month == "Feb") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_60_kuga_mar <- test_kuga %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1960 & year <= 1969) %>%
  filter(month == "Mar") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_60_kuga_apr <- test_kuga %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1960 & year <= 1969) %>%
  filter(month == "Apr") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_60_kuga_may <- test_kuga %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1960 & year <= 1969) %>%
  filter(month == "May") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_60_kuga_jun <- test_kuga %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1960 & year <= 1969) %>%
  filter(month == "Jun") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_60_kuga_jul <- test_kuga %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1960 & year <= 1969) %>%
  filter(month == "Jul") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_60_kuga_aug <- test_kuga %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1960 & year <= 1969) %>%
  filter(month == "Aug") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_60_kuga_sep <- test_kuga %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1960 & year <= 1969) %>%
  filter(month == "Sep") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_60_kuga_oct <- test_kuga %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1960 & year <= 1969) %>%
  filter(month == "Oct") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_60_kuga_nov <- test_kuga %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1960 & year <= 1969) %>%
  filter(month == "Nov") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_60_kuga_dec <- test_kuga %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1960 & year <= 1969) %>%
  filter(month == "Dec") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )

# Now let's do the 1970's
# Start with Jan
target_70_kuga_jan <- test_kuga %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1970 & year <= 1979) %>%
  filter(month == "Jan") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_70_kuga_feb <- test_kuga %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1970 & year <= 1979) %>%
  filter(month == "Feb") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_70_kuga_mar <- test_kuga %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1970 & year <= 1979) %>%
  filter(month == "Mar") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_70_kuga_apr <- test_kuga %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1970 & year <= 1979) %>%
  filter(month == "Apr") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_70_kuga_may <- test_kuga %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1970 & year <= 1979) %>%
  filter(month == "May") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_70_kuga_jun <- test_kuga %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1970 & year <= 1979) %>%
  filter(month == "Jun") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_70_kuga_jul <- test_kuga %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1970 & year <= 1979) %>%
  filter(month == "Jul") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_70_kuga_aug <- test_kuga %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1970 & year <= 1979) %>%
  filter(month == "Aug") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_70_kuga_sep <- test_kuga %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1970 & year <= 1979) %>%
  filter(month == "Sep") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_70_kuga_oct <- test_kuga %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1970 & year <= 1979) %>%
  filter(month == "Oct") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_70_kuga_nov <- test_kuga %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1970 & year <= 1979) %>%
  filter(month == "Nov") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_70_kuga_dec <- test_kuga %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1970 & year <= 1979) %>%
  filter(month == "Dec") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )

# Now let's do the 1980's
# Start with Jan
target_80_kuga_jan <- test_kuga %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1980 & year <= 1989) %>%
  filter(month == "Jan") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_80_kuga_feb <- test_kuga %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1980 & year <= 1989) %>%
  filter(month == "Feb") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_80_kuga_mar <- test_kuga %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1980 & year <= 1989) %>%
  filter(month == "Mar") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_80_kuga_apr <- test_kuga %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1980 & year <= 1989) %>%
  filter(month == "Apr") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_80_kuga_may <- test_kuga %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1980 & year <= 1989) %>%
  filter(month == "May") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_80_kuga_jun <- test_kuga %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1980 & year <= 1989) %>%
  filter(month == "Jun") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_80_kuga_jul <- test_kuga %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1980 & year <= 1989) %>%
  filter(month == "Jul") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_80_kuga_aug <- test_kuga %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1980 & year <= 1989) %>%
  filter(month == "Aug") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_80_kuga_sep <- test_kuga %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1980 & year <= 1989) %>%
  filter(month == "Sep") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_80_kuga_oct <- test_kuga %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1980 & year <= 1989) %>%
  filter(month == "Oct") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_80_kuga_nov <- test_kuga %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1980 & year <= 1989) %>%
  filter(month == "Nov") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_80_kuga_dec <- test_kuga %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1980 & year <= 1989) %>%
  filter(month == "Dec") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )

# Right, now let's do the 90's baby!
# Start with Jan
target_90_kuga_jan <- test_kuga %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1990 & year <= 1999) %>%
  filter(month == "Jan") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_90_kuga_feb <- test_kuga %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1990 & year <= 1999) %>%
  filter(month == "Feb") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_90_kuga_mar <- test_kuga %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1990 & year <= 1999) %>%
  filter(month == "Mar") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_90_kuga_apr <- test_kuga %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1990 & year <= 1999) %>%
  filter(month == "Apr") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_90_kuga_may <- test_kuga %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1990 & year <= 1999) %>%
  filter(month == "May") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_90_kuga_jun <- test_kuga %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1990 & year <= 1999) %>%
  filter(month == "Jun") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_90_kuga_jul <- test_kuga %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1990 & year <= 1999) %>%
  filter(month == "Jul") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_90_kuga_aug <- test_kuga %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1990 & year <= 1999) %>%
  filter(month == "Aug") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_90_kuga_sep <- test_kuga %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1990 & year <= 1999) %>%
  filter(month == "Sep") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_90_kuga_oct <- test_kuga %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1990 & year <= 1999) %>%
  filter(month == "Oct") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_90_kuga_nov <- test_kuga %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1990 & year <= 1999) %>%
  filter(month == "Nov") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_90_kuga_dec <- test_kuga %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 1990 & year <= 1999) %>%
  filter(month == "Dec") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )

# Fabulous. Now let's do the 2000's
# Start with Jan
target_00_kuga_jan <- test_kuga %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 2000 & year <= 2009) %>%
  filter(month == "Jan") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_00_kuga_feb <- test_kuga %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 2000 & year <= 2009) %>%
  filter(month == "Feb") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_00_kuga_mar <- test_kuga %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 2000 & year <= 2009) %>%
  filter(month == "Mar") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_00_kuga_apr <- test_kuga %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 2000 & year <= 2009) %>%
  filter(month == "Apr") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_00_kuga_may <- test_kuga %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 2000 & year <= 2009) %>%
  filter(month == "May") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_00_kuga_jun <- test_kuga %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 2000 & year <= 2009) %>%
  filter(month == "Jun") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_00_kuga_jul <- test_kuga %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 2000 & year <= 2009) %>%
  filter(month == "Jul") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_00_kuga_aug <- test_kuga %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 2000 & year <= 2009) %>%
  filter(month == "Aug") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_00_kuga_sep <- test_kuga %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 2000 & year <= 2009) %>%
  filter(month == "Sep") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_00_kuga_oct <- test_kuga %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 2000 & year <= 2009) %>%
  filter(month == "Oct") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_00_kuga_nov <- test_kuga %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 2000 & year <= 2009) %>%
  filter(month == "Nov") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_00_kuga_dec <- test_kuga %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 2000 & year <= 2009) %>%
  filter(month == "Dec") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )

# Now the 2010's
# Start with Jan
target_10_kuga_jan <- test_kuga %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 2010 & year <= 2019) %>%
  filter(month == "Jan") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_10_kuga_feb <- test_kuga %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 2010 & year <= 2019) %>%
  filter(month == "Feb") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_10_kuga_mar <- test_kuga %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 2010 & year <= 2019) %>%
  filter(month == "Mar") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_10_kuga_apr <- test_kuga %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 2010 & year <= 2019) %>%
  filter(month == "Apr") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_10_kuga_may <- test_kuga %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 2010 & year <= 2019) %>%
  filter(month == "May") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_10_kuga_jun <- test_kuga %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 2010 & year <= 2019) %>%
  filter(month == "Jun") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_10_kuga_jul <- test_kuga %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 2010 & year <= 2019) %>%
  filter(month == "Jul") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_10_kuga_aug <- test_kuga %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 2010 & year <= 2019) %>%
  filter(month == "Aug") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_10_kuga_sep <- test_kuga %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 2010 & year <= 2019) %>%
  filter(month == "Sep") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_10_kuga_oct <- test_kuga %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 2010 & year <= 2019) %>%
  filter(month == "Oct") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_10_kuga_nov <- test_kuga %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 2010 & year <= 2019) %>%
  filter(month == "Nov") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )
target_10_kuga_dec <- test_kuga %>%
  select(year,month,maxtemp,mintemp) %>%
  filter(year >= 2010 & year <= 2019) %>%
  filter(month == "Dec") %>%
  mutate(
    maxavg = mean(maxtemp),
    minavg = mean(mintemp)
  )

# Great. That's all done. What next? Well, we combine the averages.
# Starting with the 1950s'
maxavg_kuga_50 <- data.frame(
  Jan = c(target_50_kuga_jan$maxavg[1]),
  Feb = c(target_50_kuga_feb$maxavg[1]),
  Mar = c(target_50_kuga_mar$maxavg[1]),
  Apr = c(target_50_kuga_apr$maxavg[1]),
  May = c(target_50_kuga_may$maxavg[1]),
  Jun = c(target_50_kuga_jun$maxavg[1]),
  Jul = c(target_50_kuga_jul$maxavg[1]),
  Aug = c(target_50_kuga_aug$maxavg[1]),
  Sep = c(target_50_kuga_sep$maxavg[1]),
  Oct = c(target_50_kuga_oct$maxavg[1]),
  Nov = c(target_50_kuga_nov$maxavg[1]),
  Dec = c(target_50_kuga_dec$maxavg[1])
) %>%
  pivot_longer(-0,names_to = "month", values_to = "maxavg")

# Let's do the same with the min averages
minavg_kuga_50 <- data.frame(
  Jan = c(target_50_kuga_jan$minavg[1]),
  Feb = c(target_50_kuga_feb$minavg[1]),
  Mar = c(target_50_kuga_mar$minavg[1]),
  Apr = c(target_50_kuga_apr$minavg[1]),
  May = c(target_50_kuga_may$minavg[1]),
  Jun = c(target_50_kuga_jun$minavg[1]),
  Jul = c(target_50_kuga_jul$minavg[1]),
  Aug = c(target_50_kuga_aug$minavg[1]),
  Sep = c(target_50_kuga_sep$minavg[1]),
  Oct = c(target_50_kuga_oct$minavg[1]),
  Nov = c(target_50_kuga_nov$minavg[1]),
  Dec = c(target_50_kuga_dec$minavg[1])
) %>%
  pivot_longer(-0,names_to = "month", values_to = "minavg")

# Next, let's do the 1960's
maxavg_kuga_60 <- data.frame(
  Jan = c(target_60_kuga_jan$maxavg[1]),
  Feb = c(target_60_kuga_feb$maxavg[1]),
  Mar = c(target_60_kuga_mar$maxavg[1]),
  Apr = c(target_60_kuga_apr$maxavg[1]),
  May = c(target_60_kuga_may$maxavg[1]),
  Jun = c(target_60_kuga_jun$maxavg[1]),
  Jul = c(target_60_kuga_jul$maxavg[1]),
  Aug = c(target_60_kuga_aug$maxavg[1]),
  Sep = c(target_60_kuga_sep$maxavg[1]),
  Oct = c(target_60_kuga_oct$maxavg[1]),
  Nov = c(target_60_kuga_nov$maxavg[1]),
  Dec = c(target_60_kuga_dec$maxavg[1])
) %>%
  pivot_longer(-0,names_to = "month", values_to = "maxavg")

# Let's do the same with the min averages
minavg_kuga_60 <- data.frame(
  Jan = c(target_60_kuga_jan$minavg[1]),
  Feb = c(target_60_kuga_feb$minavg[1]),
  Mar = c(target_60_kuga_mar$minavg[1]),
  Apr = c(target_60_kuga_apr$minavg[1]),
  May = c(target_60_kuga_may$minavg[1]),
  Jun = c(target_60_kuga_jun$minavg[1]),
  Jul = c(target_60_kuga_jul$minavg[1]),
  Aug = c(target_60_kuga_aug$minavg[1]),
  Sep = c(target_60_kuga_sep$minavg[1]),
  Oct = c(target_60_kuga_oct$minavg[1]),
  Nov = c(target_60_kuga_nov$minavg[1]),
  Dec = c(target_60_kuga_dec$minavg[1])
) %>%
  pivot_longer(-0,names_to = "month", values_to = "minavg")

# Next, the 1970's
maxavg_kuga_70 <- data.frame(
  Jan = c(target_70_kuga_jan$maxavg[1]),
  Feb = c(target_70_kuga_feb$maxavg[1]),
  Mar = c(target_70_kuga_mar$maxavg[1]),
  Apr = c(target_70_kuga_apr$maxavg[1]),
  May = c(target_70_kuga_may$maxavg[1]),
  Jun = c(target_70_kuga_jun$maxavg[1]),
  Jul = c(target_70_kuga_jul$maxavg[1]),
  Aug = c(target_70_kuga_aug$maxavg[1]),
  Sep = c(target_70_kuga_sep$maxavg[1]),
  Oct = c(target_70_kuga_oct$maxavg[1]),
  Nov = c(target_70_kuga_nov$maxavg[1]),
  Dec = c(target_70_kuga_dec$maxavg[1])
) %>%
  pivot_longer(-0,names_to = "month", values_to = "maxavg")

# Let's do the same with the min averages
minavg_kuga_70 <- data.frame(
  Jan = c(target_70_kuga_jan$minavg[1]),
  Feb = c(target_70_kuga_feb$minavg[1]),
  Mar = c(target_70_kuga_mar$minavg[1]),
  Apr = c(target_70_kuga_apr$minavg[1]),
  May = c(target_70_kuga_may$minavg[1]),
  Jun = c(target_70_kuga_jun$minavg[1]),
  Jul = c(target_70_kuga_jul$minavg[1]),
  Aug = c(target_70_kuga_aug$minavg[1]),
  Sep = c(target_70_kuga_sep$minavg[1]),
  Oct = c(target_70_kuga_oct$minavg[1]),
  Nov = c(target_70_kuga_nov$minavg[1]),
  Dec = c(target_70_kuga_dec$minavg[1])
) %>%
  pivot_longer(-0,names_to = "month", values_to = "minavg")

# Next, the 1980's
maxavg_kuga_80 <- data.frame(
  Jan = c(target_80_kuga_jan$maxavg[1]),
  Feb = c(target_80_kuga_feb$maxavg[1]),
  Mar = c(target_80_kuga_mar$maxavg[1]),
  Apr = c(target_80_kuga_apr$maxavg[1]),
  May = c(target_80_kuga_may$maxavg[1]),
  Jun = c(target_80_kuga_jun$maxavg[1]),
  Jul = c(target_80_kuga_jul$maxavg[1]),
  Aug = c(target_80_kuga_aug$maxavg[1]),
  Sep = c(target_80_kuga_sep$maxavg[1]),
  Oct = c(target_80_kuga_oct$maxavg[1]),
  Nov = c(target_80_kuga_nov$maxavg[1]),
  Dec = c(target_80_kuga_dec$maxavg[1])
) %>%
  pivot_longer(-0,names_to = "month", values_to = "maxavg")

# Let's do the same with the min averages
minavg_kuga_80 <- data.frame(
  Jan = c(target_80_kuga_jan$minavg[1]),
  Feb = c(target_80_kuga_feb$minavg[1]),
  Mar = c(target_80_kuga_mar$minavg[1]),
  Apr = c(target_80_kuga_apr$minavg[1]),
  May = c(target_80_kuga_may$minavg[1]),
  Jun = c(target_80_kuga_jun$minavg[1]),
  Jul = c(target_80_kuga_jul$minavg[1]),
  Aug = c(target_80_kuga_aug$minavg[1]),
  Sep = c(target_80_kuga_sep$minavg[1]),
  Oct = c(target_80_kuga_oct$minavg[1]),
  Nov = c(target_80_kuga_nov$minavg[1]),
  Dec = c(target_80_kuga_dec$minavg[1])
) %>%
  pivot_longer(-0,names_to = "month", values_to = "minavg")

# Next, the 1990's
maxavg_kuga_90 <- data.frame(
  Jan = c(target_90_kuga_jan$maxavg[1]),
  Feb = c(target_90_kuga_feb$maxavg[1]),
  Mar = c(target_90_kuga_mar$maxavg[1]),
  Apr = c(target_90_kuga_apr$maxavg[1]),
  May = c(target_90_kuga_may$maxavg[1]),
  Jun = c(target_90_kuga_jun$maxavg[1]),
  Jul = c(target_90_kuga_jul$maxavg[1]),
  Aug = c(target_90_kuga_aug$maxavg[1]),
  Sep = c(target_90_kuga_sep$maxavg[1]),
  Oct = c(target_90_kuga_oct$maxavg[1]),
  Nov = c(target_90_kuga_nov$maxavg[1]),
  Dec = c(target_90_kuga_dec$maxavg[1])
) %>%
  pivot_longer(-0,names_to = "month", values_to = "maxavg")

# Let's do the same with the min averages
minavg_kuga_90 <- data.frame(
  Jan = c(target_90_kuga_jan$minavg[1]),
  Feb = c(target_90_kuga_feb$minavg[1]),
  Mar = c(target_90_kuga_mar$minavg[1]),
  Apr = c(target_90_kuga_apr$minavg[1]),
  May = c(target_90_kuga_may$minavg[1]),
  Jun = c(target_90_kuga_jun$minavg[1]),
  Jul = c(target_90_kuga_jul$minavg[1]),
  Aug = c(target_90_kuga_aug$minavg[1]),
  Sep = c(target_90_kuga_sep$minavg[1]),
  Oct = c(target_90_kuga_oct$minavg[1]),
  Nov = c(target_90_kuga_nov$minavg[1]),
  Dec = c(target_90_kuga_dec$minavg[1])
) %>%
  pivot_longer(-0,names_to = "month", values_to = "minavg")

# Next, the 2000's
maxavg_kuga_00 <- data.frame(
  Jan = c(target_00_kuga_jan$maxavg[1]),
  Feb = c(target_00_kuga_feb$maxavg[1]),
  Mar = c(target_00_kuga_mar$maxavg[1]),
  Apr = c(target_00_kuga_apr$maxavg[1]),
  May = c(target_00_kuga_may$maxavg[1]),
  Jun = c(target_00_kuga_jun$maxavg[1]),
  Jul = c(target_00_kuga_jul$maxavg[1]),
  Aug = c(target_00_kuga_aug$maxavg[1]),
  Sep = c(target_00_kuga_sep$maxavg[1]),
  Oct = c(target_00_kuga_oct$maxavg[1]),
  Nov = c(target_00_kuga_nov$maxavg[1]),
  Dec = c(target_00_kuga_dec$maxavg[1])
) %>%
  pivot_longer(-0,names_to = "month", values_to = "maxavg")

# Let's do the same with the min averages
minavg_kuga_00 <- data.frame(
  Jan = c(target_00_kuga_jan$minavg[1]),
  Feb = c(target_00_kuga_feb$minavg[1]),
  Mar = c(target_00_kuga_mar$minavg[1]),
  Apr = c(target_00_kuga_apr$minavg[1]),
  May = c(target_00_kuga_may$minavg[1]),
  Jun = c(target_00_kuga_jun$minavg[1]),
  Jul = c(target_00_kuga_jul$minavg[1]),
  Aug = c(target_00_kuga_aug$minavg[1]),
  Sep = c(target_00_kuga_sep$minavg[1]),
  Oct = c(target_00_kuga_oct$minavg[1]),
  Nov = c(target_00_kuga_nov$minavg[1]),
  Dec = c(target_00_kuga_dec$minavg[1])
) %>%
  pivot_longer(-0,names_to = "month", values_to = "minavg")

# Next, the 2010's
maxavg_kuga_10 <- data.frame(
  Jan = c(target_10_kuga_jan$maxavg[1]),
  Feb = c(target_10_kuga_feb$maxavg[1]),
  Mar = c(target_10_kuga_mar$maxavg[1]),
  Apr = c(target_10_kuga_apr$maxavg[1]),
  May = c(target_10_kuga_may$maxavg[1]),
  Jun = c(target_10_kuga_jun$maxavg[1]),
  Jul = c(target_10_kuga_jul$maxavg[1]),
  Aug = c(target_10_kuga_aug$maxavg[1]),
  Sep = c(target_10_kuga_sep$maxavg[1]),
  Oct = c(target_10_kuga_oct$maxavg[1]),
  Nov = c(target_10_kuga_nov$maxavg[1]),
  Dec = c(target_10_kuga_dec$maxavg[1])
) %>%
  pivot_longer(-0,names_to = "month", values_to = "maxavg")

# Let's do the same with the min averages
minavg_kuga_10 <- data.frame(
  Jan = c(target_10_kuga_jan$minavg[1]),
  Feb = c(target_10_kuga_feb$minavg[1]),
  Mar = c(target_10_kuga_mar$minavg[1]),
  Apr = c(target_10_kuga_apr$minavg[1]),
  May = c(target_10_kuga_may$minavg[1]),
  Jun = c(target_10_kuga_jun$minavg[1]),
  Jul = c(target_10_kuga_jul$minavg[1]),
  Aug = c(target_10_kuga_aug$minavg[1]),
  Sep = c(target_10_kuga_sep$minavg[1]),
  Oct = c(target_10_kuga_oct$minavg[1]),
  Nov = c(target_10_kuga_nov$minavg[1]),
  Dec = c(target_10_kuga_dec$minavg[1])
) %>%
  pivot_longer(-0,names_to = "month", values_to = "minavg")

# Great, we have our averages. Let's bind 'em together
kuga_50_avg <- inner_join(minavg_kuga_50,maxavg_kuga_50, by="month")
kuga_60_avg <- inner_join(minavg_kuga_60,maxavg_kuga_60, by="month")
kuga_70_avg <- inner_join(minavg_kuga_70,maxavg_kuga_70, by="month")
kuga_80_avg <- inner_join(minavg_kuga_80,maxavg_kuga_80, by="month")
kuga_90_avg <- inner_join(minavg_kuga_90,maxavg_kuga_90, by="month")
kuga_00_avg <- inner_join(minavg_kuga_00,maxavg_kuga_00, by="month")
kuga_10_avg <- inner_join(minavg_kuga_10,maxavg_kuga_10, by="month")

# Let's do some mass garbage collection
rm(list=ls(pattern="target"))
rm(list=ls(pattern="maxavg"))
rm(list=ls(pattern="minavg"))

# This is a pain in the ass. But we need to round to the nearest decimal place.
# Round measurements to nearest decimal place ----
cbay_50_avg <- cbay_50_avg %>%
  mutate(
    minavg = round(minavg, digits = 1),
    maxavg = round(maxavg, digits = 1)
  )
cbay_60_avg <- cbay_60_avg %>%
  mutate(
    minavg = round(minavg, digits = 1),
    maxavg = round(maxavg, digits = 1)
  )
cbay_70_avg <- cbay_70_avg %>%
  mutate(
    minavg = round(minavg, digits = 1),
    maxavg = round(maxavg, digits = 1)
  )
cbay_80_avg <- cbay_80_avg %>%
  mutate(
    minavg = round(minavg, digits = 1),
    maxavg = round(maxavg, digits = 1)
  )
cbay_90_avg <- cbay_90_avg %>%
  mutate(
    minavg = round(minavg, digits = 1),
    maxavg = round(maxavg, digits = 1)
  )
cbay_00_avg <- cbay_00_avg %>%
  mutate(
    minavg = round(minavg, digits = 1),
    maxavg = round(maxavg, digits = 1)
  )
cbay_10_avg <- cbay_10_avg %>%
  mutate(
    minavg = round(minavg, digits = 1),
    maxavg = round(maxavg, digits = 1)
  )
# Do it again for Kugluktuk
kugl_50_avg <- kugl_50_avg %>%
  mutate(
    minavg = round(minavg, digits = 1),
    maxavg = round(maxavg, digits = 1)
  )
kugl_60_avg <- kugl_60_avg %>%
  mutate(
    minavg = round(minavg, digits = 1),
    maxavg = round(maxavg, digits = 1)
  )
kugl_70_avg <- kugl_70_avg %>%
  mutate(
    minavg = round(minavg, digits = 1),
    maxavg = round(maxavg, digits = 1)
  )
kugl_80_avg <- kugl_80_avg %>%
  mutate(
    minavg = round(minavg, digits = 1),
    maxavg = round(maxavg, digits = 1)
  )
kugl_90_avg <- kugl_90_avg %>%
  mutate(
    minavg = round(minavg, digits = 1),
    maxavg = round(maxavg, digits = 1)
  )
kugl_00_avg <- kugl_00_avg %>%
  mutate(
    minavg = round(minavg, digits = 1),
    maxavg = round(maxavg, digits = 1)
  )
kugl_10_avg <- kugl_10_avg %>%
  mutate(
    minavg = round(minavg, digits = 1),
    maxavg = round(maxavg, digits = 1)
  )
# Next, do Gjoa Haven
gjoa_80_avg <- gjoa_80_avg %>%
  mutate(
    minavg = round(minavg, digits = 1),
    maxavg = round(maxavg, digits = 1)
  )
gjoa_90_avg <- gjoa_90_avg %>%
  mutate(
    minavg = round(minavg, digits = 1),
    maxavg = round(maxavg, digits = 1)
  )
gjoa_00_avg <- gjoa_00_avg %>%
  mutate(
    minavg = round(minavg, digits = 1),
    maxavg = round(maxavg, digits = 1)
  )
gjoa_10_avg <- gjoa_10_avg %>%
  mutate(
    minavg = round(minavg, digits = 1),
    maxavg = round(maxavg, digits = 1)
  )
# Last, do Kugaaruk
kuga_50_avg <- kuga_50_avg %>%
  mutate(
    minavg = round(minavg, digits = 1),
    maxavg = round(maxavg, digits = 1)
  )
kuga_60_avg <- kuga_60_avg %>%
  mutate(
    minavg = round(minavg, digits = 1),
    maxavg = round(maxavg, digits = 1)
  )
kuga_70_avg <- kuga_70_avg %>%
  mutate(
    minavg = round(minavg, digits = 1),
    maxavg = round(maxavg, digits = 1)
  )
kuga_80_avg <- kuga_80_avg %>%
  mutate(
    minavg = round(minavg, digits = 1),
    maxavg = round(maxavg, digits = 1)
  )
kuga_90_avg <- kuga_90_avg %>%
  mutate(
    minavg = round(minavg, digits = 1),
    maxavg = round(maxavg, digits = 1)
  )
kuga_00_avg <- kuga_00_avg %>%
  mutate(
    minavg = round(minavg, digits = 1),
    maxavg = round(maxavg, digits = 1)
  )
kuga_10_avg <- kuga_10_avg %>%
  mutate(
    minavg = round(minavg, digits = 1),
    maxavg = round(maxavg, digits = 1)
  )
# Great, now that we've rounded to the nearest decimal place let's move on.

# Let's try some graphs
t_data_50 <- cbay_50_avg %>%
  mutate(
    month = factor(month, levels = month.abb)
  )
t_data_60 <- cbay_60_avg %>%
  mutate(
    month = factor(month, levels = month.abb)
  )
t_data_70 <- cbay_70_avg %>%
  mutate(
    month = factor(month, levels = month.abb)
  )
t_data_80 <- cbay_80_avg %>%
  mutate(
    month = factor(month, levels = month.abb)
  )
t_data_90 <- cbay_90_avg %>%
  mutate(
    month = factor(month, levels = month.abb)
  )
t_data_00 <- cbay_00_avg %>%
  mutate(
    month = factor(month, levels = month.abb)
  )
t_data_10 <- cbay_10_avg %>%
  mutate(
    month = factor(month, levels = month.abb)
  )

# Make test_cbay again
test_cbay <- cbay %>%
  mutate(year = year(cbay$date),
         month = month(cbay$date),
         month = month.abb[month],
         maxtemp = cbay$maxtemp,
         mintemp = cbay$mintemp) %>%
  select(year,month,maxtemp,mintemp) %>%
  mutate(month = factor(month, levels = month.abb))

# We need something to group data by.
cbay_50_avg <- cbay_50_avg %>%
  mutate(
    year = "1950s",
    month = factor(month, levels = month.abb),
    minavg = cbay_50_avg$minavg,
    maxavg = cbay_50_avg$maxavg
  )
cbay_60_avg <- cbay_60_avg %>%
  mutate(
    year = "1960s",
    month = factor(month, levels = month.abb),
    minavg = cbay_60_avg$minavg,
    maxavg = cbay_60_avg$maxavg
  )
cbay_70_avg <- cbay_70_avg %>%
  mutate(
    year = "1970s",
    month = factor(month, levels = month.abb),
    minavg = cbay_70_avg$minavg,
    maxavg = cbay_70_avg$maxavg
  )
cbay_80_avg <- cbay_80_avg %>%
  mutate(
    year = "1980s",
    month = factor(month, levels = month.abb),
    minavg = cbay_80_avg$minavg,
    maxavg = cbay_80_avg$maxavg
  )
cbay_90_avg <- cbay_90_avg %>%
  mutate(
    year = "1990s",
    month = factor(month, levels = month.abb),
    minavg = cbay_90_avg$minavg,
    maxavg = cbay_90_avg$maxavg
  )
cbay_00_avg <- cbay_00_avg %>%
  mutate(
    year = "2000s",
    month = factor(month, levels = month.abb),
    minavg = cbay_00_avg$minavg,
    maxavg = cbay_00_avg$maxavg
  )
cbay_10_avg <- cbay_10_avg %>%
  mutate(
    year = "2010s",
    month = factor(month, levels = month.abb),
    minavg = cbay_00_avg$minavg,
    maxavg = cbay_00_avg$maxavg
  )

# Bind the rows together
cbay_avg <- rbind(cbay_50_avg,cbay_60_avg,cbay_70_avg,cbay_80_avg,cbay_90_avg,cbay_00_avg,cbay_10_avg)

# Let's subset our data.
cbay_summer <- subset(cbay_avg, month %in% c('May','Jun','Jul','Aug','Sep','Oct'))
cbay_winter <- subset(cbay_avg, month %in% c('Nov','Dec','Jan','Feb','Mar','Apr'))

# Make some graphs ----
# Let's make some custom colors
month_gradient = palette(brewer.pal(n = 7, name = "Dark2"))

# Generate plot for Cambridge Bay summer temperatures from 1950 - 2019
cbay_summer_plot <- ggplot(cbay_summer, aes(x = month, y = maxavg, group=year)) +
  #geom_smooth(method = "loess", aes(color=year)) +
  geom_line(aes(color=year)) +
  geom_point(aes(color=year)) +
  scale_y_continuous(breaks = seq(-10, 20, 2)) +
  scale_x_discrete(expand = c(0,0)) +
  labs(x = "Month", y = "Temperature", title="Cambridge Bay temperature anomalies (\u00B0C)",
       subtitle = "(Summer difference from annual mean max temperature by decade)",
       color = "Decades") +
  scale_color_manual(values = c("1950s" = month_gradient[7],
                                "1960s" = month_gradient[6],
                                "1970s" = month_gradient[5],
                                "1980s" = month_gradient[4],
                                "1990s" = month_gradient[3],
                                "2000s" = month_gradient[2],
                                "2010s" = month_gradient[1])) +
  theme(
    panel.background = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "gray", linetype = "dotted", linewidth=0.25),
    plot.title.position = "plot",
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(color = "gray", size = 10),
    plot.margin = margin(t = 10, r = 15, b = 10, l = 10)
  )
cbay_summer_plot

# Before we plot the winter months though, we need to re-order the factor
# We're going to need a custom order for the levels in winter to show up correctly
# Namely, it's a different order since we've snipped out the summer months.
# We start with Nov, Dec, Jan, Feb, Mar, Apr instead
cbay_winter$month <- factor(cbay_winter$month, levels=c('Nov','Dec','Jan','Feb','Mar','Apr','May'))

# Let's try the winter plot
cbay_winter_plot <- ggplot(cbay_winter, aes(x = month, y = maxavg, group=year)) +
  geom_smooth(method = "loess", aes(color=year)) +
  geom_point(aes(color=year)) +
  scale_y_continuous(breaks = seq(-45, -15, 1)) +
  scale_x_discrete(expand = c(0,0)) +
  labs(x = "Month", y = "Temperature", title="Cambridge Bay temperature anomalies (\u00B0C)",
       subtitle = "(Winter difference from annual mean max temperature by decade)",
       color = "Decades") +
  scale_color_manual(values = c("1950s" = month_gradient[7],
                                "1960s" = month_gradient[6],
                                "1970s" = month_gradient[5],
                                "1980s" = month_gradient[4],
                                "1990s" = month_gradient[3],
                                "2000s" = month_gradient[2],
                                "2010s" = month_gradient[1])) +
  theme(
    panel.background = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "gray", linetype = "dotted", linewidth=0.25),
    plot.title.position = "plot",
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(color = "gray", size = 10),
    plot.margin = margin(t = 10, r = 15, b = 10, l = 10)
  )
cbay_winter_plot
