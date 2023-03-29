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

# Before we remove NA's let's look at what we're losing
nonworkingdata <- (workingdata[is.na(workingdata$bin.uri),])

# Let's get a count of the order represented in that data
out <- nonworkingdata %>%
  select(Class,Order) %>%
  count(Class,Order)
# We did this to record those values for later:
# 547 Diptera, 425 Hymenoptera, 365 Arachnida, 78 Araneae, 189 Hemiptera, 232 Collembola, 137 other
# Do some garbage collection
rm(out,nonworkingdata)

# Remove any NA's from bin.uri in workingdata
workingdata <- workingdata %>%
  drop_na(bin.uri)
# Note: There are 31833 specimens
# After removing any NA's in bin.uri, there are 29860 left. A difference of 1973.

# Remove any sector that does not conform to a few selected communities by renaming them.
# To do that, we'll replace any instance where "3km NW Cambridge Bay, Water Lake Site" as just "Cambridge Bay" instead
workingdata$Sector[workingdata$Sector == "3 km NW Cambridge Bay, Water Lake site"] <- "Cambridge Bay"

# Trust, but verify.
table(workingdata$Sector)

# There are a few odd ones left. 1 from the North Shaler Mountains, 29 from Icebreaker Channel
# We'll effectively ignore those as well, but let's have a look at them while we're at it.
location1 <- workingdata %>%
  filter(Sector == "Icebreaker Channel")
location2 <- workingdata %>%
  filter(Sector == "North Shaler Mountains")
rm(location1,location2)

# The Icebreaker Channel data isn't what we're looking for. So we'll discard it. In fact, we'll remove it when we select down to the Phylum Arthropoda later.
# However, the springtail specimen was noticed during unrelated plant pressing at CHARS. It might be interesting since it was successfully barcoded.
# Before we proceed, let's see if there's a BIN match to the rest of our data. It returned a bin_uri of BOLD:AAI8142
lookatme <- workingdata %>%
  filter(bin.uri == "BOLD:AAI8142")
# Interesting. 40 matches to our data from the 4 communities.
# Nonetheless, Let's put it aside since it's not necessary to work with it. But it is interesting to know that the other matching locations for this BIN are in Cambridge Bay, Gjoa Haven, and Kugaaruk.
rm(lookatme)

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
# So, we've eliminated 30 specimens from places not in our target location(s) of Cambridge Bay, Kugluktuk, Gjoa Haven, and Kugaaruk

# Let's filter that down even further to just Arthropoda
workingdata <- workingdata %>%
  filter(Phylum == "Arthropoda")

# Before we try plotting a time series, let's rename "Collection Date" to something that actually works as a date.
names(workingdata)[names(workingdata) == "Collection Date"] <- "CollectionDate"

# We also need to change the CollectionDate column type from Character to Date
workingdata <- workingdata %>%
  mutate(CollectionDate = as.Date(CollectionDate, format = "%d-%b-%Y"))

# Let's create a graph showing what we've got by order. We'll remove NA's and narrow it down to arthropoda we're interested in the next few scripts.
ggplot(workingdata, aes(y = Sector)) +
  geom_bar(aes(fill = Order)) +
  labs(x = "# of Specimens", y = "Community", title = "Specimens prior to filtering out all aquatic invertebrates") +
  theme(legend.position = "top") +
  geom_text(stat='count', aes(label=after_stat(count)))

# Save our state
write_tsv(x = workingdata, "data/workingdata_2022_12_20.tsv")
