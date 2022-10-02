# Combine data sets
# Objective: Combine datasets all data sets into one containing only own data
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
#

# Load libraries
library(tidyverse)
library(ggVennDiagram)
library(dplyr)

# Load our working data set
workingdata <- read_tsv("D:/R/InsectsArctic/Data/working_dataset.tsv")

# Remove any NA's from bin.uri in workingdata
workingdata <- workingdata %>%
  drop_na(bin.uri)

# Phylum, Class, Order, Family, Subfamily, Tribe, Genus, Species, Subspecies
table(workingdata$Phylum)

# Filter out arthopoda first
work_arthropoda <- workingdata %>%
  filter(Phylum == "Arthropoda")
