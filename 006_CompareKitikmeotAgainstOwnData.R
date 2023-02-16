# Graph Kitikmeot data to compare against own data set

# Load libraries
library(tidyverse)
library(ggplot2)

# Read Canada BOLD data
canada_data <- read_tsv("C:/R/InsectsArctic/data/Canada_data_clean_december.tsv")
kitikmeot_bold <- read_tsv("C:/R/InsectsArctic/data/workingdata_2022_12_20.tsv")

# Filter out Arthropoda
canada_data_arthropoda <- canada_data %>%
  filter(phylum_name == "Arthropoda")
rm(canada_data)

nunavut_data_arthropoda <- canada_data_arthropoda %>%
  filter(province_state == "Nunavut")
rm(canada_data_arthropoda)

# Region filter
kitikmeot <- nunavut_data_arthropoda %>%
  filter(region == "Kitikmeot")

# Rename a faulty sector in Cambridge Bay
kitikmeot$sector[kitikmeot$sector == "3 km NW Cambridge Bay, Water Lake site"] <- "Cambridge Bay"

# Copy off the same sectors
cambridgebay <- kitikmeot %>%
  filter(sector == "Cambridge Bay")
kugluktuk <- kitikmeot %>%
  filter(sector == "Kugluktuk")
kugaaruk <- kitikmeot %>%
  filter(sector == "Kugaaruk")
gjoahaven <- kitikmeot %>%
  filter(sector == "Gjoa Haven")

# Recombine the frames
test1 <- rbind(cambridgebay,kugluktuk)
test2 <- rbind(test1,kugaaruk)
test3 <- rbind(test2,gjoahaven)
kitikmeot_public <- test3
rm(test1,test2,test3,cambridgebay,kugluktuk,kugaaruk,gjoahaven)

# Change Sector to sector though
names(kitikmeot_bold)[names(kitikmeot_bold) == "Sector"] <- "sector"

# Graph the BOLD Public data from the Kitikmeot region
ggplot(kitikmeot_public, aes(y = sector)) +
  geom_bar(aes(fill = order_name)) +
  labs(x = "# of Specimens", y = "Community", fill = "Order") +
  theme(legend.position = "top") +
  geom_text(stat='count', aes(label=..count..))
# 10600 Kugluktuk
# 15 Kugaaruk
# 4498 Gjoa Haven
# 35770 Cambridge Bay

# Graph BOLD ARCBIO-POLAR data
ggplot(kitikmeot_bold, aes(y = sector)) +
  geom_bar(aes(fill = Order)) +
  labs(x = "# of Specimens", y = "Community", fill = "Order") +
  theme(legend.position = "top") +
  geom_text(stat='count', aes(label=..count..))
# 4996 Kugluktuk, diff of 5604
# 1002 Kugaaruk, diff of 987
# 3635 Gjoa Haven, diff of 863
# 20197 Cambridge Bay, diff of 15573

# Some discrepancies it seems
table(kitikmeot_public$sector)
table(kitikmeot_bold$sector)

# Let's save our data so we can use it again in the next script
write_tsv(x = kitikmeot_public, "C:/R/InsectsArctic/data/kitikmeot_public.tsv")
write_tsv(x = kitikmeot_bold, "C:/R/InsectsArctic/data/kitikmeot_bold.tsv")
