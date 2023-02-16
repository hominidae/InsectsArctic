# Process data from GBIF
# OBJECTIVE:
#  - Take GBIF human observations data and prepare it for comparison against own collected data

# Note:
#  This is for research grade observations.

# Load libraries ####
library(tidyverse)
library(ggplot2)
library(scales)

# Load the GBIF data set containing only human observations from iNaturalist
gbif_data <- read_tsv("C:/R//InsectsArctic/data/GBIF_Canada_Arthropod_2022_10_15.csv")

# Let's have a peek at it.
table(gbif_data$stateProvince)

# Great. Looks clean. To be expected for research grade data.
# One pesky thing to get out of the way first. Non accent ague, sil vous plait.
gbif_data$stateProvince[gbif_data$stateProvince == "Québec"] <- "Quebec"

# Remove any NA's in stateProvince since that's apparently a thing with research grade observations.
gbif_data <- gbif_data %>%
  drop_na(stateProvince)

# Remove any NA's for unidentified orders as well, but let's have a look first to see we aren't doing something foolish.
gbif_datana <- gbif_data %>%
  filter(is.na(order))

# Yikes. We have a problem. The GBIF Research Grade Observations seem to have introduced NA's for Collembola.
# How many are missing? Compare against the number of observations classed as Collembola.
gbif_datacol <- gbif_data %>%
  filter(class == "Collembola")

# Interesting. 836 - 693 is 143. We're going to need to re-classify Collembola with a missing order and fit them within their respective orders.
# To do that, we'll need to make use of mutate.
# Entomobryidae -> class to Entomobryomorpha
# Orchesellidae -> class to Entomobryomorpha
# Paronellidae ->
# Tomoceridae ->
#gbif_datafixed <- gbif_datana %>%
#  mutate()

######################### Left off here. Continue to repair the research grade observations?
# Not so sure that all Entomobryidae are Entomobryomoprha. Something funky is going on.

# Let's generate a plot of the data
ggplot(gbif_data, aes(y = stateProvince)) +
  geom_bar(aes(fill = order), position = position_stack(reverse = TRUE)) +
  labs(x = "# of Specimens", y = "Province", fill = "Order") +
  theme(legend.position = "top", plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(labels = comma) +
  geom_text(stat='count', aes(label=after_stat(count))) +
  ggtitle("GBIF Research Grade Observations via iNaturalist by Seek")

# Unique identifications from GBIF
uniquespecies <- gbif_data %>%
  select(phylum,class,order,family,genus,species,stateProvince) %>%
  drop_na(species)

# Drop down to unique only
uniquespecies <- uniquespecies %>% distinct(species, .keep_all = TRUE)

# Plot those unique identifications by province
ggplot(uniquespecies, aes(y = stateProvince)) +
  geom_bar(aes(fill = order), position = position_stack(reverse = TRUE)) +
  labs(x = "# of Unique identifications", y = "Province", fill = "Order") +
  theme(legend.position = "top", plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(labels = comma) +
  geom_text(stat='count', aes(label=after_stat(count))) +
  ggtitle("Unique GBIF Research Grade Observations via iNaturalist by Seek")

gbif_rgo_data <- gbif_data
rm(gbif_data)

# Great, that is pretty interesting. Especially when compared against public BOLD data.
# Now, What about the unofficial observations?

# Process data from GBIF
# OBJECTIVE:
#  - Take GBIF occurrence data and prepare it for comparison against own collected data

# Note:
#  This is intended for the non-research grade data for Canadian Arthropoda
#  Simply search for arthropoda from Canada and select Human Observations only to cut down on the download size
#  The data I wrote this for was originally from April 2022

# Here's the DOI for that:
# GBIF.org (18 October 2022) GBIF Occurrence Download  https://doi.org/10.15468/dl.mxvn32

# Load the GBIF data set containing only human observations from iNaturalist
gbif_data <- read_tsv("C:/R//ProcessGBIFPublicData/data/Canada_GBIF_Occurrences_2022_10_18.csv")

# Let's look at that too before we proceed further
table(gbif_data$stateProvince)

# Great. A lot of bullshit. Let's get to replacing bullshit with useful names.
# 1 - Fix Alberta ####
gbif_data_ab1 <- gbif_data %>%
  filter(stateProvince == "Ab")
gbif_data_ab2 <- gbif_data %>%
  filter(stateProvince == "Alberta")
gbif_data_ab3 <- gbif_data %>%
  filter(stateProvince == "Alta.")
gbif_data_ab4 <- rbind(gbif_data_ab1, gbif_data_ab2)
gbif_data_ab5 <- rbind(gbif_data_ab4, gbif_data_ab3)
gbif_data_ab <- gbif_data_ab5
rm(gbif_data_ab1,gbif_data_ab2,gbif_data_ab3,gbif_data_ab4,gbif_data_ab5)
# Let's check what we need to replace in Alberta
table(gbif_data_ab$stateProvince)
# Great, let's replace those.
gbif_data_ab$stateProvince[gbif_data_ab$stateProvince == "Ab"] <- "Alberta"
gbif_data_ab$stateProvince[gbif_data_ab$stateProvince == "Alta."] <- "Alberta"
# Next check if they're all fixed.
table(gbif_data_ab$stateProvince)

# 2 - Fix British Columbia ####
gbif_data_bc1 <- gbif_data %>%
  filter(stateProvince == "Bc")
gbif_data_bc2 <- gbif_data %>%
  filter(stateProvince == "BC.")
gbif_data_bc3 <- gbif_data %>%
  filter(stateProvince == "British Columbia")
# Put 'em all back together again
gbif_data_bc4 <- rbind(gbif_data_bc1, gbif_data_bc2)
gbif_data_bc5 <- rbind(gbif_data_bc4, gbif_data_bc3)
gbif_data_bc <- gbif_data_bc5
rm(gbif_data_bc1,gbif_data_bc2,gbif_data_bc3,gbif_data_bc4,gbif_data_bc5)
# Check if we're good
table(gbif_data_bc$stateProvince)
# Rename 'em all
gbif_data_bc$stateProvince[gbif_data_bc$stateProvince == "Bc"] <- "British Columbia"
gbif_data_bc$stateProvince[gbif_data_bc$stateProvince == "BC."] <- "British Columbia"

# 3 - Fix Saskatchewan ####
gbif_data_sk1 <- gbif_data %>%
  filter(stateProvince == "Sask.")
gbif_data_sk2 <- gbif_data %>%
  filter(stateProvince == "Saskatchewan")
gbif_data_sk3 <- gbif_data %>%
  filter(stateProvince == "Sk")
# Put 'em all back together again
gbif_data_sk4 <- rbind(gbif_data_sk1,gbif_data_sk2)
gbif_data_sk5 <- rbind(gbif_data_sk4,gbif_data_sk3)
gbif_data_sk <- gbif_data_sk5
# Check if we're good
table(gbif_data_sk$stateProvince)
# Rename 'em all
gbif_data_sk$stateProvince[gbif_data_sk$stateProvince == "Sask."] <- "Saskatchewan"
gbif_data_sk$stateProvince[gbif_data_sk$stateProvince == "Sk"] <- "Saskatchewan"
rm(gbif_data_sk1,gbif_data_sk2,gbif_data_sk3,gbif_data_sk4,gbif_data_sk5)

# 4 - Fix Manitoba ####
gbif_data_mb1 <- gbif_data %>%
  filter(stateProvince == "Man.")
gbif_data_mb2 <- gbif_data %>%
  filter(stateProvince == "Manitoba")
gbif_data_mb3 <- gbif_data %>%
  filter(stateProvince == "Mb")
# Put 'em all back together again
gbif_data_mb4 <- rbind(gbif_data_mb1,gbif_data_mb2)
gbif_data_mb5 <- rbind(gbif_data_mb4,gbif_data_mb3)
gbif_data_mb <- gbif_data_mb5
# Check the data
table(gbif_data_mb$stateProvince)
# Rename 'em all
gbif_data_mb$stateProvince[gbif_data_mb$stateProvince == "Man."] <- "Manitoba"
gbif_data_mb$stateProvince[gbif_data_mb$stateProvince == "Mb"] <- "Manitoba"
rm(gbif_data_mb1,gbif_data_mb2,gbif_data_mb3,gbif_data_mb4,gbif_data_mb5)

# 5 - Fix  Ontario ####
gbif_data_on1 <- gbif_data %>%
  filter(stateProvince == "Ont")
gbif_data_on2 <- gbif_data %>%
  filter(stateProvince == "Ont.")
gbif_data_on3 <- gbif_data %>%
  filter(stateProvince == "Ontario")
# Put 'em all together
gbif_data_on4 <- rbind(gbif_data_on1,gbif_data_on2)
gbif_data_on5 <- rbind(gbif_data_on4,gbif_data_on3)
gbif_data_on <- gbif_data_on5
# Check 'em
table(gbif_data_on$stateProvince)
# Rename 'em all
gbif_data_on$stateProvince[gbif_data_on$stateProvince == "Ont"] <- "Ontario"
gbif_data_on$stateProvince[gbif_data_on$stateProvince == "Ont."] <- "Ontario"
rm(gbif_data_on1,gbif_data_on2,gbif_data_on3,gbif_data_on4,gbif_data_on5)

# 6 - Fix Quebec ####
gbif_data_qc1 <- gbif_data %>%
  filter(stateProvince == "Qc")
gbif_data_qc2 <- gbif_data %>%
  filter(stateProvince == "Que.")
gbif_data_qc3 <- gbif_data %>%
  filter(stateProvince == "Québec")
# Stich 'em together
gbif_data_qc4 <- rbind(gbif_data_qc1,gbif_data_qc2)
gbif_data_qc5 <- rbind(gbif_data_qc4,gbif_data_qc3)
gbif_data_qc <- gbif_data_qc5
# Check 'em
table(gbif_data_qc$stateProvince)
# Rename em all
gbif_data_qc$stateProvince[gbif_data_qc$stateProvince == "Qc"] <- "Quebec"
gbif_data_qc$stateProvince[gbif_data_qc$stateProvince == "Que."] <- "Quebec"
gbif_data_qc$stateProvince[gbif_data_qc$stateProvince == "Québec"] <- "Quebec"
rm(gbif_data_qc1,gbif_data_qc2,gbif_data_qc3,gbif_data_qc4,gbif_data_qc5)

# 7 - Fix NWT ####
gbif_data_nwt1 <- gbif_data %>%
  filter(stateProvince == "Nwt")
gbif_data_nwt2 <- gbif_data %>%
  filter(stateProvince == "NWT.")
gbif_data_nwt3 <- gbif_data %>%
  filter(stateProvince == "Northwest Territories")
# Stich 'em together
gbif_data_nwt4 <- rbind(gbif_data_nwt1,gbif_data_nwt2)
gbif_data_nwt5 <- rbind(gbif_data_nwt4,gbif_data_nwt3)
gbif_data_nwt <- gbif_data_nwt5
# Check em
table(gbif_data_nwt$stateProvince)
# Rename 'em all
gbif_data_nwt$stateProvince[gbif_data_nwt$stateProvince == "Nwt"] <- "Northwest Territories"
gbif_data_nwt$stateProvince[gbif_data_nwt$stateProvince == "NWT."] <- "Northwest Territories"
rm(gbif_data_nwt1,gbif_data_nwt2,gbif_data_nwt3,gbif_data_nwt4,gbif_data_nwt5)

# 8 - Fix Nunavut ####
gbif_data_nt1 <- gbif_data %>%
  filter(stateProvince == "Nt")
gbif_data_nt2 <- gbif_data %>%
  filter(stateProvince == "Nunavut")
# Stich 'em together
gbif_data_nt3 <- rbind(gbif_data_nt1,gbif_data_nt2)
gbif_data_nt <- gbif_data_nt3
# Check 'em
table(gbif_data_nt$stateProvince)
# Rename em
gbif_data_nt$stateProvince[gbif_data_nt$stateProvince == "Nt"] <- "Nunavut"
rm(gbif_data_nt1,gbif_data_nt2,gbif_data_nt3)

# 9 - Fix Yukon ####
gbif_data_yk1 <- gbif_data %>%
  filter(stateProvince == "Yk")
gbif_data_yk2 <- gbif_data %>%
  filter(stateProvince == "Yt")
gbif_data_yk3 <- gbif_data %>%
  filter(stateProvince == "YT.")
gbif_data_yk4 <- gbif_data %>%
  filter(stateProvince == "Yukon")
# Stich 'em togehter
gbif_data_yk5 <- rbind(gbif_data_yk1,gbif_data_yk2)
gbif_data_yk6 <- rbind(gbif_data_yk5,gbif_data_yk3)
gbif_data_yk7 <- rbind(gbif_data_yk6,gbif_data_yk4)
gbif_data_yk <- gbif_data_yk7
# Check em
table(gbif_data_yk$stateProvince)
# Rename 'em
gbif_data_yk$stateProvince[gbif_data_yk$stateProvince == "Yk"] <- "Yukon"
gbif_data_yk$stateProvince[gbif_data_yk$stateProvince == "Yt"] <- "Yukon"
gbif_data_yk$stateProvince[gbif_data_yk$stateProvince == "YT."] <- "Yukon"
rm(gbif_data_yk1,gbif_data_yk2,gbif_data_yk3,gbif_data_yk4,gbif_data_yk5,gbif_data_yk6,gbif_data_yk7)

# 10 - Fix New Brunswick ####
gbif_data_nb1 <- gbif_data %>%
  filter(stateProvince == "Nb")
gbif_data_nb2 <- gbif_data %>%
  filter(stateProvince == "NB.")
gbif_data_nb3 <- gbif_data %>%
  filter(stateProvince == "New Brunswick")
# Stich 'em together
gbif_data_nb4 <- rbind(gbif_data_nb1,gbif_data_nb2)
gbif_data_nb5 <- rbind(gbif_data_nb4,gbif_data_nb3)
gbif_data_nb <- gbif_data_nb5
# Check em
table(gbif_data_nb$stateProvince)
# Rename 'em
gbif_data_nb$stateProvince[gbif_data_nb$stateProvince == "Nb"] <- "New Brunswick"
gbif_data_nb$stateProvince[gbif_data_nb$stateProvince == "NB."] <- "New Brunswick"
rm(gbif_data_nb1,gbif_data_nb2,gbif_data_nb3,gbif_data_nb4,gbif_data_nb5)

# 11 - Fix Prince Edward Island ####
gbif_data_pei1 <- gbif_data %>%
  filter(stateProvince == "Pe")
gbif_data_pei2 <- gbif_data %>%
  filter(stateProvince == "PEI.")
gbif_data_pei3 <- gbif_data %>%
  filter(stateProvince == "Prince Edward Island")
# Stitch 'em together
gbif_data_pei4 <- rbind(gbif_data_pei1,gbif_data_pei2)
gbif_data_pei5 <- rbind(gbif_data_pei4,gbif_data_pei3)
gbif_data_pei <- gbif_data_pei5
# Check em
table(gbif_data_pei$stateProvince)
# Rename 'em
gbif_data_pei$stateProvince[gbif_data_pei$stateProvince == "Pe"] <- "Prince Edward Island"
gbif_data_pei$stateProvince[gbif_data_pei$stateProvince == "PEI."] <- "Prince Edward Island"
rm(gbif_data_pei1,gbif_data_pei2,gbif_data_pei3,gbif_data_pei4,gbif_data_pei5)

# 12 - Fix Nova Scotia ####
gbif_data_ns1 <- gbif_data %>%
  filter(stateProvince == "Nova Scotia")
gbif_data_ns2 <- gbif_data %>%
  filter(stateProvince == "Ns")
gbif_data_ns3 <- gbif_data %>%
  filter(stateProvince == "NS.")
# Bind em together
gbif_data_ns4 <- rbind(gbif_data_ns1,gbif_data_ns2)
gbif_data_ns5 <- rbind(gbif_data_ns4,gbif_data_ns3)
gbif_data_ns <- gbif_data_ns5
# Check data
table(gbif_data_ns$stateProvince)
# Rename
gbif_data_ns$stateProvince[gbif_data_ns$stateProvince == "Ns"] <- "Nova Scotia"
gbif_data_ns$stateProvince[gbif_data_ns$stateProvince == "NS."] <- "Nova Scotia"
rm(gbif_data_ns1,gbif_data_ns2,gbif_data_ns3,gbif_data_ns4,gbif_data_ns5)

# 13 - Fix Newfoundland and Labrador ####
gbif_data_nf1 <- gbif_data %>%
  filter(stateProvince == "Lab.")
gbif_data_nf2 <- gbif_data %>%
  filter(stateProvince == "Newfoundland and Labrador")
gbif_data_nf3 <- gbif_data %>%
  filter(stateProvince == "Nf")
gbif_data_nf4 <- gbif_data %>%
  filter(stateProvince == "NF.")
# Stich 'em
gbif_data_nf5 <- rbind(gbif_data_nf1,gbif_data_nf2)
gbif_data_nf6 <- rbind(gbif_data_nf5,gbif_data_nf3)
gbif_data_nf7 <- rbind(gbif_data_nf6,gbif_data_nf4)
gbif_data_nf <- gbif_data_nf7
# Check em
table(gbif_data_nf$stateProvince)
# Rename em
gbif_data_nf$stateProvince[gbif_data_nf$stateProvince == "Lab."] <- "Newfoundland and Labrador"
gbif_data_nf$stateProvince[gbif_data_nf$stateProvince == "Nf"] <- "Newfoundland and Labrador"
gbif_data_nf$stateProvince[gbif_data_nf$stateProvince == "NF."] <- "Newfoundland and Labrador"
rm(gbif_data_nf1,gbif_data_nf2,gbif_data_nf3,gbif_data_nf4,gbif_data_nf5,gbif_data_nf6,gbif_data_nf7)

# 14 - Combine provinces and territories ####
gbif_data1 <- rbind(gbif_data_ab,gbif_data_sk)
gbif_data2 <- rbind(gbif_data1,gbif_data_bc)
gbif_data3 <- rbind(gbif_data2,gbif_data_mb)
gbif_data4 <- rbind(gbif_data3,gbif_data_nb)
gbif_data5 <- rbind(gbif_data4,gbif_data_nf)
gbif_data6 <- rbind(gbif_data5,gbif_data_ns)
gbif_data7 <- rbind(gbif_data6,gbif_data_nt)
gbif_data8 <- rbind(gbif_data7,gbif_data_nwt)
gbif_data9 <- rbind(gbif_data8,gbif_data_on)
gbif_data10 <- rbind(gbif_data9,gbif_data_qc)
gbif_data11 <- rbind(gbif_data10,gbif_data_sk)
gbif_data12 <- rbind(gbif_data11,gbif_data_yk)
gbif_data13 <- rbind(gbif_data12,gbif_data_pei)
gbif_data <- gbif_data13
# Check 'em
table(gbif_data$stateProvince)
rm(gbif_data1,gbif_data2,gbif_data3,gbif_data4,gbif_data5,gbif_data6,gbif_data7,gbif_data8,gbif_data9,gbif_data10,gbif_data11,gbif_data12,gbif_data13)
rm(gbif_data_ab,gbif_data_sk,gbif_data_mb,gbif_data_on,gbif_data_qc,gbif_data_nt,gbif_data_nwt,gbif_data_yk,gbif_data_pei,gbif_data_nb,gbif_data_nf,gbif_data_bc,gbif_data_ns)

# Great. All our public observations are cleaned up a bit.
# Let's remove anything without province data
gbif_data <- gbif_data %>%
  drop_na(stateProvince)

# Also, remove any NA's for lat/lon.
gbif_data <- gbif_data %>%
  drop_na(decimalLatitude,decimalLongitude)

# Let's go the extra step and remove any NA's for no identifications too
gbif_data <- gbif_data %>%
  drop_na(species)

# Let's generate a plot of the data
ggplot(gbif_data, aes(y = stateProvince)) +
  geom_bar(aes(fill = order), position = position_stack(reverse = TRUE)) +
  labs(x = "# of Specimens", y = "Province", fill = "Order") +
  theme(legend.position = "top", plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(labels = comma) +
  geom_text(stat='count', aes(label=after_stat(count))) +
  ggtitle("GBIF Observations via iNaturalist by Seek")

# Unique identifications from GBIF
uniquespecies <- gbif_data %>%
  select(phylum,class,order,family,genus,species,stateProvince) %>%
  drop_na(species)

# Drop down to unique only
uniquespecies <- uniquespecies %>% distinct(species, .keep_all = TRUE)

# Plot those unique identifications by province
ggplot(uniquespecies, aes(y = stateProvince)) +
  geom_bar(aes(fill = order), position = position_stack(reverse = TRUE)) +
  labs(x = "# of Unique identifications", y = "Province", fill = "Order") +
  theme(legend.position = "top", plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(labels = comma) +
  geom_text(stat='count', aes(label=after_stat(count))) +
  ggtitle("Unique GBIF Observations via iNaturalist by Seek")
