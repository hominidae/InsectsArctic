# Graph Kitikmeot data to compare against own data set

# Load libraries
library(tidyverse)
library(ggplot2)
library(ggmap)

# Read Canada BOLD data
canada_data <- read_tsv("data/canada_data_clean.tsv")
# Read the data I was involved with collecting as well
kitikmeot_owndata <- read_tsv("data/workingdata_2022_12_20.tsv")

# Filter out Arthropoda
canada_data_arthropoda <- canada_data %>%
  filter(phylum_name == "Arthropoda")
rm(canada_data)

# Let's filter that Canada data down to Nunavut
nunavut_data_arthropoda <- canada_data_arthropoda %>%
  filter(province_state == "Nunavut")
rm(canada_data_arthropoda)

# Region filter for the Kitikmeot region of Nunavut
kitikmeot <- nunavut_data_arthropoda %>%
  filter(region == "Kitikmeot")

# Let's look at sectors within the Kitikmeot
table(kitikmeot$sector)
# We'll need to fix the Cambridge Bay item. We'll drop the others later. But for now that's acceptable

# Rename a faulty sector in Cambridge Bay
kitikmeot$sector[kitikmeot$sector == "3 km NW Cambridge Bay, Water Lake site"] <- "Cambridge Bay"
# Note, we had to do this with our data which means some of our data is in the public BOLD data. Which is completely fine. Public funds for public science is how science should always be done.
# It's not really a big deal, as we're only going to focus on our data. The extra 5000 or so records from Kugluktuk would be nice, but not strictly necessary.

# Copy off the same sectors.
cambridgebay <- kitikmeot %>%
  filter(sector == "Cambridge Bay")
cambridgebay1 <- kitikmeot %>%
  filter(exactsite == "Cambridge Bay")
cambridgebay <- rbind(cambridgebay,cambridgebay1)
# Moving on.
kugluktuk <- kitikmeot %>%
  filter(sector == "Kugluktuk")
kugaaruk <- kitikmeot %>%
  filter(sector == "Kugaaruk")
gjoahaven <- kitikmeot %>%
  filter(sector == "Gjoa Haven")

# Right off the bat, there's a problem. Some of the sites have the community as exactsite rather than sector. Why do we only have 15 records for Kugaaruk?
# Looking at the data, it appears that there are only a handful of sites from my own duplicate data present in the public BOLD data for Kugaaruk.
# That's interesting AF. It basically means the specimens collected there are some of the only barcodes in existence. That's pretty cool.
# Also, since they weren't published publically it means they were probably just in the process.
# No worries.

# Recombine the frames
test1 <- rbind(cambridgebay,kugluktuk)
test2 <- rbind(test1,kugaaruk)
test3 <- rbind(test2,gjoahaven)
kitikmeot_public <- test3
rm(test1,test2,test3,cambridgebay,cambridgebay1,kugluktuk,kugaaruk,gjoahaven)
# Great. Now we have any specimens from the public BOLD data for the Kitikmeot region of Nunavut
# We're not really going to use them, but it is nice to know we have more to compare against later.

# Change Sector to sector though
names(kitikmeot_owndata)[names(kitikmeot_owndata) == "Sector"] <- "sector"

# Graph the BOLD Public data from the Kitikmeot region
ggplot(kitikmeot_public, aes(y = sector)) +
  geom_bar(aes(fill = order_name)) +
  labs(x = "# of Specimens", y = "Community", fill = "Order") +
  theme(legend.position = "top") +
  geom_text(stat='count', aes(label=after_stat(count)))
# 10600 Kugluktuk
# 15 Kugaaruk
# 4498 Gjoa Haven
# 35770 Cambridge Bay

# Graph BOLD ARCBIO-POLAR data
ggplot(kitikmeot_owndata, aes(y = sector)) +
  geom_bar(aes(fill = Order)) +
  labs(x = "# of Specimens", y = "Community", fill = "Order") +
  theme(legend.position = "top") +
  geom_text(stat='count', aes(label=after_stat(count)))
# 4996 Kugluktuk, diff of 5604
# 1002 Kugaaruk, diff of 987
# 3635 Gjoa Haven, diff of 863
# 20197 Cambridge Bay, a diff of 15573

# If you are feeling like you'd like to explore the 79,636 DNA Barcode's from all of Nunavut, it'd certainly be interesting.
# However, I'll stick with what I'm working with. We'll save the Nunavut Arthropoda data though for later.

# Some discrepancies it seems
table(kitikmeot_public$sector)
table(kitikmeot_owndata$sector)
table(nunavut_data_arthropoda$sector)

# Let's save our data so we can use it again in the next few script
write_tsv(x = kitikmeot_public, "data/kitikmeot_public.tsv")
write_tsv(x = kitikmeot_owndata, "data/kitikmeot_bold.tsv")
write_tsv(x = nunavut_data_arthropoda, "data/nunavut_data_arthropoda.tsv")
