# Process data from public BOLD data
# OBJECTIVE:
#  - Take public BOLD data and prepare it for comparison against own collected data
# Note:
#  This code takes advantage of code from ProcessBOLDPublicData code from this github link:
#  https://github.com/hominidae/ProcessBOLDPublicData
#  "canada_data_october.csv" is that data

# Load libraries
library(tidyverse)
library(dplyr)

# Load the data set containing publicly available BOLD data, warning uses about 4.61GB of memory
# This data was stitched together from across Canada using code from my other ProcessBOLDPublicData code
# It involves downloading BOLD data from various provinces and stitching it all back together.
canada_data <- read_tsv("D:/R/InsectsArctic/data/canada_data_october.tsv")

# Have a quick peek to see how easy this will be.
table(canada_data$province_state)
# Easy peasy. We just need to remove the isolated items from odd places.

canada_data_alberta <- canada_data %>%
  filter(province_state == "Alberta")
canada_data_bc <- canada_data %>%
  filter(province_state == "British Columbia")
canada_data_manitoba <- canada_data %>%
  filter(province_state == "Manitoba")
canada_data_nb <- canada_data %>%
  filter(province_state == "New Brunswick")
canada_data_nf <- canada_data %>%
  filter(province_state == "Newfoundland and Labrador")
canada_data_nwt <- canada_data %>%
  filter(province_state == "Northwest Territories")
canada_data_ns <- canada_data %>%
  filter(province_state == "Nova Scotia")
canada_data_nt <- canada_data %>%
  filter(province_state == "Nunavut")
canada_data_on <- canada_data %>%
  filter(province_state == "Ontario")
canada_data_qc <- canada_data %>%
  filter(province_state == "Quebec")
canada_data_yk <- canada_data %>%
  filter(province_state == "Yukon Territory")

# Great, we've got 'em isolated. Now put 'em all back together.
can1 <- rbind(canada_data_alberta, canada_data_bc)
can2 <- rbind(can1, canada_data_manitoba)
can3 <- rbind(can2, canada_data_nb)
can4 <- rbind(can3, canada_data_nf)
can5 <- rbind(can4, canada_data_nwt)
can6 <- rbind(can5, canada_data_ns)
can7 <- rbind(can6, canada_data_nt)
can8 <- rbind(can7, canada_data_on)
can9 <- rbind(can8, canada_data_qc)
can10 <- rbind(can9, canada_data_yk)
canada_data <- can10

# Garbage clean-up
rm(can1,can2,can3,can4,can5,can6,can7,can8,can9,can10)
rm(canada_data_alberta,canada_data_bc,canada_data_manitoba,canada_data_nb,canada_data_nf,canada_data_ns,canada_data_nt,canada_data_nwt,canada_data_on,canada_data_qc,canada_data_yk)

# Righteous. Clean data. Let's save it before it gets lost.
write_tsv(x = canada_data, "D:/R/InsectsArctic/data/Canada_data_clean_october.tsv")

# Reload here if necessary.
#canada_data <- read_tsv("D:/R/InsectsArctic/data/Canada_data_october.tsv")

# Let's filter down to just arthropods
canada_data_arthropoda <- canada_data %>%
  filter(phylum_name == "Arthropoda")
rm(canada_data)

# Load ggplot2
library(ggplot2)
library(scales)

# Let's generate a plot of the data
ggplot(canada_data_arthropoda, aes(y = province_state)) +
  geom_bar(aes(fill = order_name), position = position_stack(reverse = TRUE)) +
  labs(x = "# of Specimens", y = "Province", fill = "Order") +
  theme(legend.position = "top") +
  scale_x_continuous(labels = comma) +
  geom_text(stat='count', aes(label=..count..))
