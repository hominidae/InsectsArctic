# Process own data from Cambridge Bay
# Objective:
#  Characterize data collected in Kitikmeot region of Nunavut

# The data sets involved:
# BCHAR - Malaise Trap Samples 2018
#  Cambridge Bay, 4001 specimens
# CCHAR - IMA 2018
#  Cambridge Bay, 7295 specimens
# DCHAR - Water Lake Site 2018
#  Cambridge Bay, 8552 specimens
# FCHAR - General Terrestrial Collection 2019
#  Cambridge Bay, 2986 specimens
#  Kugluktuk, 4788 specimens
# GCHAR - Freshwater Aquatic 2019
# HCHAR - Marine Aquatic Collection 2019
# CBAY - 2021 Collection
#  Cambridge Bay, 66 specimens 
# KUGA - 2021 Collection
#  Kugaaruk, 1044 specimens
# GJOA - 2021 Collection
#  Gjoa Haven, 3708 specimens
# KUGL - Kugluktuk, Malaise and general collection 2021

# Load libraries
library(tidyverse)
library(ggplot2)
library(dplyr)
library(scales)

# Load our working data set
workingdata <- read_tsv("data/kitikmeot_data_arth.tsv")

# Remove any NA's from bin.uri in workingdata
workingdata <- workingdata %>%
  drop_na(bin.uri)
# Note: There are 30373 specimens
# After removing any NA's in bin.uri, there are 28410 left. A difference of 1963.

# Remove any sector that does not conform to a few selected communities by renaming them.
# To do that, we'll replace any instance where "3km NW Cambridge Bay, Water Lake Site" as just "Cambridge Bay" instead
workingdata$Sector[workingdata$Sector == "3 km NW Cambridge Bay, Water Lake site"] <- "Cambridge Bay"

# Trust, but verify.
table(workingdata$Sector)

# Okay cool, that worked. "Victoria Island" will need to be dropped. "North Shaler Mountains" will also need to be dropped too.
# Let's try that. To do that, we'll create by community then re-combine.
test <- workingdata %>%
  filter(Sector == "Cambridge Bay")
test1 <- workingdata %>%
  filter(Sector == "Gjoa Haven")
test2 <- workingdata %>%
  filter(Sector == "Kugaaruk")
test3 <- workingdata %>%
  filter(Sector == "Kugluktuk")
test4 <- rbind(test,test1)
test5 <- rbind(test4,test2)
test6 <- rbind(test5,test3)
workingdata <- test6
rm(test,test1,test2,test3,test4,test5,test6)

# Filter out arthopoda first
workingdata <- workingdata %>%
  filter(Phylum == "Arthropoda")

# Before we try plotting a time series, let's rename "Collection Date" to something that actually works as a date.
names(workingdata)[names(workingdata) == "Collection Date"] <- "CollectionDate"

# We also need to change the CollectionDate column type from Character to Date
workingdata <- workingdata %>%
  mutate(CollectionDate = as.Date(CollectionDate, format = "%d-%b-%Y"))

# This works!
# Let's create a graph showing what we've got by order.
ggplot(workingdata, aes(y = Sector)) +
  geom_bar(aes(fill = Order)) +
  labs(x = "# of Specimens", y = "Community") +
  theme(legend.position = "top") +
  geom_text(stat='count', aes(label=..count..))

# Save our state
write_tsv(x = workingdata, "data/workingdata_2022_12_20.tsv")
