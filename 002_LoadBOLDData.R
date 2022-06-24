# Process data from projects in Cambridge Bay
# OBJECTIVE:
#  - Recombine bchar, cchar, dchar, echar, etc into singular tsv files

# Load library
library(tidyverse)
library(dplyr)

# Load DNA Barcoding data from BOLD
# BCHAR Biodiversity Survey - Malaise Trap Samples 2018
bchar1 <- read_tsv("D:/R/InsectsArctic/Data/BCHAR/collection_data.tsv")
bchar2 <- read_tsv("D:/R/InsectsArctic/Data/BCHAR/merged_custom_fields.tsv")
bchar3 <- read_tsv("D:/R/InsectsArctic/Data/BCHAR/specimen_details.tsv")
bchar4 <- read_tsv("D:/R/InsectsArctic/Data/BCHAR/tags.tsv")
bchar5 <- read_tsv("D:/R/InsectsArctic/Data/BCHAR/taxonomy.tsv")
bchar6 <- read_tsv("D:/R/InsectsArctic/Data/BCHAR/voucher.tsv")

# Load more DNA Barcoding data from BOLD
# CCHAR Biodiversity Survey - Standardized Sampling - Intensive Monitoring Area Site 2018
cchar1 <- read_tsv("D:/R/InsectsArctic/Data/CCHAR/collection_data.tsv")
cchar2 <- read_tsv("D:/R/InsectsArctic/Data/CCHAR/merged_custom_fields.tsv")
cchar3 <- read_tsv("D:/R/InsectsArctic/Data/CCHAR/specimen_details.tsv")
cchar4 <- read_tsv("D:/R/InsectsArctic/Data/CCHAR/tags.tsv")
cchar5 <- read_tsv("D:/R/InsectsArctic/Data/CCHAR/taxonomy.tsv")
cchar6 <- read_tsv("D:/R/InsectsArctic/Data/CCHAR/voucher.tsv")
cchar7 <- read_tsv("D:/R/InsectsArctic/Data/CCHAR/cchar-sequencedata.tsv")

# Load more DNA Barcoding data from BOLD
# DCHAR Biodiversity Survey - Standardized Sampling - Water Lake Site 2018
dchar1 <- read_tsv("D:/R/InsectsArctic/Data/DCHAR/collection_data.tsv")
dchar2 <- read_tsv("D:/R/InsectsArctic/Data/DCHAR/merged_custom_fields.tsv")
dchar3 <- read_tsv("D:/R/InsectsArctic/Data/DCHAR/specimen_details.tsv")
dchar4 <- read_tsv("D:/R/InsectsArctic/Data/DCHAR/tags.tsv")
dchar5 <- read_tsv("D:/R/InsectsArctic/Data/DCHAR/taxonomy.tsv")
dchar6 <- read_tsv("D:/R/InsectsArctic/Data/DCHAR/voucher.tsv")

# Load more DNA Barcoding data from BOLD
# ECHAR Biodiversity Survey - Marine Sampling 2018
echar1 <- read_tsv("D:/R/InsectsArctic/Data/ECHAR/collection_data.tsv")
echar2 <- read_tsv("D:/R/InsectsArctic/Data/ECHAR/merged_custom_fields.tsv")
echar3 <- read_tsv("D:/R/InsectsArctic/Data/ECHAR/specimen_details.tsv")
echar4 <- read_tsv("D:/R/InsectsArctic/Data/ECHAR/tags.tsv")
echar5 <- read_tsv("D:/R/InsectsArctic/Data/ECHAR/taxonomy.tsv")
echar6 <- read_tsv("D:/R/InsectsArctic/Data/ECHAR/voucher.tsv")

# Load more DNA Barcoding data from BOLD
# FCHAR Biodiversity Survey 2019 - General Terrestrial Collection
fchar1 <- read_tsv("D:/R/InsectsArctic/Data/FCHAR/collection_data.tsv")
fchar2 <- read_tsv("D:/R/InsectsArctic/Data/FCHAR/merged_custom_fields.tsv")
fchar3 <- read_tsv("D:/R/InsectsArctic/Data/FCHAR/specimen_details.tsv")
fchar4 <- read_tsv("D:/R/InsectsArctic/Data/FCHAR/tags.tsv")
fchar5 <- read_tsv("D:/R/InsectsArctic/Data/FCHAR/taxonomy.tsv")
fchar6 <- read_tsv("D:/R/InsectsArctic/Data/FCHAR/voucher.tsv")
fchar7 <- read_tsv("D:/R/InsectsArctic/Data/FCHAR/fchar-sequencedata.tsv")

# Data available so far, Cambridge Bay 2018
# Kugluktuk 2019
# Search for Cam bay, Gjoa Haven, Kugaaruk 2021

# Next, we need to combine bchar, cchar, dchar, echar, and fchar into their own respective combined dataframe
# There are some issues, in that there are more objects in merged_custom_fields
bchar_collembola <- bchar5 %>%
  filter(Class == "Collembola")
# cchar has collembola
cchar_collembola <- cchar5 %>%
  filter(Class == "Collembola")
# dchar does as well
dchar_collembola <- dchar5 %>%
  filter(Class == "Collembola")
# echar is marine sampling, so probs no Collembola
echar_collembola <- echar5 %>%
  filter(Class == "Collembola")
# fchar does as well
fchar_collembola <- fchar5 %>%
  filter(Class == "Collembola")

# Before we proceed though, let's combine the data frames into one.
# We need to rename the sample.id column in cchar7 though.
names(cchar7) <- c("seq.data","seq.text","process.id","taxon","Sample ID","bin.uri")

# Right, now that cchar7 sample.id has been changed to Sample ID to match the others we can use it during a join.
# Let's perform a join combining the sequencing data to the specimen data
test <- inner_join(cchar5, cchar7, by="Sample ID")

# Interesting, since we started with only 7295 records of sequencing data we have about 1201 missing records.
# That will definitely need to be something to look into later.
# But for now, let's now combine the test data frame to the location data
test1 <- inner_join(test, cchar1, by="Sample ID")

# Okay that's great! We still have the 7295 records we sorta started with so that's a good sign.
# Let's remove everything except for the Collembola.
temp <- data.frame(cchar_collembola$"Sample ID")
names(temp) <- c("Sample ID")

test2 <- inner_join(test1, temp, by="Sample ID")

cchar_final <- test2
rm(test,test1,test2)

# That's interesting. We went from 265 records and after cross referencing with available sequencing data that number dropped to 218.

# Before we go further though, let's do the same for any sequencing data for fchar_collembola.
# Done? Okay, move on.
names(fchar7) <- c("seq.data","seq.text","process.id","taxon","Sample ID","bin.uri")

test <- inner_join(fchar5, fchar7, by="Sample ID")

test1 <- inner_join(test, fchar1, by="Sample ID")

temp <- data.frame(fchar_collembola$'Sample ID')
names(temp) <- c("Sample ID")
 
test2 <- inner_join(test1, temp, by="Sample ID")

fchar_final <- test2
rm(test,test1,test2)

# We're going to create a Venn Diagram Collembola collected in Cambridge Bay and Kugluktuk respectively.
# So, we're going to move every record in Kugluktuk from cchar and fchar into cambay and kugluk respectively.
# We do have more data, but we'll go with the two for now.
cchar_cambay <- cchar_final %>%
  filter(Sector == "Cambridge Bay")
# Note: cchar only has Cambridge Bay specimens since it's from 2018.
# So there's no point sorting for Kugluktuk

# But let's do fchar for cambay next
fchar_cambay <- fchar_final %>%
  filter(Sector == "Cambridge Bay")

# Let's combine cchar_cambay and fchar_cambay
cambay <- rbind(cchar_cambay, fchar_cambay)

# Now let's do Kugluktuk, since cchar has only cam bay let's not worry about it.
fchar_kugluk <- fchar_final %>%
  filter(Sector == "Kugluktuk")

# Create a kugluk dataframe
kugluk <- fchar_kugluk

# Now we can do some Venn Diagram stuff!
# To do that, we need to create vectors.
# Let's count some things.
orders_cambay <- count(cambay, vars = "Order")
orders_kugluk <- count(kugluk, vars = "Order")

# Let's do families next
families_cambay <- count(cambay, vars = "Family")
families_kugluk <- count(kugluk, vars = "Family")

# Let's do subfamilies too
subfamilies_cambay <- count(cambay, vars = "Subfamily")
subfamilies_kugluk <- count(kugluk, vars = "Subfamily")

# Pretty interesting so far, but let's focus on the cool stuff. Identified BINS!
bins_cambay <- count(cambay, vars = "bin.uri")
bins_kugluk <- count(kugluk, vars = "bin.uri")

# What is the overlap between Cam Bay and Kugluktuk?
# To figure out that, we're going to use the BIN to perform an inner join between the two
combined_bins <- join(bins_cambay, bins_kugluk, by="bin.uri", type="inner")
names(combined_bins) <- c("BIN", "Cambridge Bay", "Kugluktuk")

# And these can now be used to perform interesting data analysis.
# Let's load ggVennDiagram
library(ggVennDiagram)
