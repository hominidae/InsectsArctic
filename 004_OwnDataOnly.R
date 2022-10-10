# Process own data from Cambridge Bay
# Objective:

# BCHAR - Malaise Trap Samples 2018
#  Cambridge Bay
# CCHAR - IMA 2018
#  Cambridge Bay
# DCHAR - Water Lake Site 2018
#  Cambridge Bay
# ECHAR - Marine Sampling 2018
#  Cambridge Bay
# FCHAR - General Terrestrial Collection 2019
#  Cambridge Bay
#  Kugluktuk

# Load libraries
library(tidyverse)
library(ggVennDiagram)
library(dplyr)

# Load our working data set
workingdata <- read_tsv("D:/R/InsectsArctic/Data/working_dataset.tsv")

# Remove any NA's from bin.uri in workingdata
workingdata <- workingdata %>%
  drop_na(bin.uri)

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

# Let's have a peek at that data
table(workingdata$Phylum)

# Filter out arthopoda first
work_arthropoda <- workingdata %>%
  filter(Phylum == "Arthropoda")

# Filter out various things
work_arachnida <- work_arthropoda %>%
  filter(Class == "Arachnida")
work_branchiopoda <- work_arthropoda %>%
  filter(Class == "Branchiopoda")
work_collembola <- work_arthropoda %>%
  filter(Class == "Collembola")
work_copepoda <- work_arthropoda %>%
  filter(Class == "Copepoda")
work_malacostraca <- work_arthropoda %>%
  filter(Class == "Malacostraca")
work_ostracode <- work_arthropoda %>%
  filter(Class == "Ostracoda")

# Next filter out branches in Insecta, we left it for last because there are....a lot!
work_insecta <- work_arthropoda %>%
  filter(Class == "Insecta")

# Since there are many branches in Insecta, let's separate them out individually
insecta_coleoptera <- work_insecta %>%
  filter(Order == "Coleoptera")
insecta_diptera <- work_insecta %>%
  filter(Order == "Diptera")
insecta_ephemeroptera <- work_insecta %>%
  filter(Order == "Ephemeroptera")
insecta_hemiptera <- work_insecta %>%
  filter(Order == "Hemiptera")
insecta_hymenoptera <- work_insecta %>%
  filter(Order == "Hymenoptera")
insecta_lepidoptera <- work_insecta %>%
  filter(Order == "Lepidoptera")
insecta_neuroptera <- work_insecta %>%
  filter(Order == "Neuroptera")
insecta_orthopetera <- work_insecta %>%
  filter(Order == "Orthoptera")
insecta_plecoptera <- work_insecta %>%
  filter(Order == "Plecoptera")
insecta_siphonaptera <- work_insecta %>%
  filter(Order == "Siphonaptera")
insecta_thysanoptera <- work_insecta %>%
  filter(Order == "Thysanoptera")

# Let's create a bar chart using ggplot. We want the chart to communicate the number of samples by location.
library(ggplot2)

# Okay, let's try something weird. Let's plot three things.
ggplot(work_arthropoda, aes(y = Sector)) +
  geom_bar(aes(fill = Order), position = position_stack(reverse = TRUE)) +
  labs(x = "# of Specimens", y = "Community") +
  theme(legend.position = "top") +
  geom_text(stat='count', aes(label=..count..), vjust=-1)
