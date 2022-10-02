# Process data from projects in Cambridge Bay
# OBJECTIVE:
#  - Recombine bchar, cchar, dchar, echar, tsv files into a singular tsv files

# Todo:
# - Cleanup this code now that you've figured it out
# - Save processed data as a csv file

# Load Libraries ----------------------------------------------------------
library(tidyverse)
library(dplyr)

# Load Data ---------------------------------------------------------------
#list.files("D:/R/InsectsArctic/Data/BCHAR/", pattern = ".tsv", full.names = T) #%>%
#  map(read_csv)
# Look at using this in the future instead?

# BCHAR Biodiversity Survey - Malaise Trap Samples 2018
bchar1 <- read_tsv("D:/R/InsectsArctic/Data/BCHAR/collection_data.tsv")
bchar5 <- read_tsv("D:/R/InsectsArctic/Data/BCHAR/taxonomy.tsv")
bchar7 <- read_tsv("D:/R/InsectsArctic/Data/BCHAR/bchar-sequencedata.tsv")
# CCHAR Biodiversity Survey - Standardized Sampling - Intensive Monitoring Area Site 2018
cchar1 <- read_tsv("D:/R/InsectsArctic/Data/CCHAR/collection_data.tsv")
cchar5 <- read_tsv("D:/R/InsectsArctic/Data/CCHAR/taxonomy.tsv")
cchar7 <- read_tsv("D:/R/InsectsArctic/Data/CCHAR/cchar-sequencedata.tsv")
# DCHAR Biodiversity Survey - Standardized Sampling - Water Lake Site 2018
dchar1 <- read_tsv("D:/R/InsectsArctic/Data/DCHAR/collection_data.tsv")
dchar5 <- read_tsv("D:/R/InsectsArctic/Data/DCHAR/taxonomy.tsv")
dchar7 <- read_tsv("D:/R/InsectsArctic/Data/DCHAR/dchar-sequencedata.tsv")
# ECHAR Biodiversity Survey - Marine Sampling 2018 / Jeremy DeWaard, Kara Layton(?), et al
echar1 <- read_tsv("D:/R/InsectsArctic/Data/ECHAR/collection_data.tsv")
echar5 <- read_tsv("D:/R/InsectsArctic/Data/ECHAR/taxonomy.tsv")
echar7 <- read_tsv("D:/R/InsectsArctic/Data/ECHAR/echar-sequencedata.tsv")
# FCHAR Biodiversity Survey 2019 - General Terrestrial Collection (Cambridge Bay & Kugluktuk)
fchar1 <- read_tsv("D:/R/InsectsArctic/Data/FCHAR/collection_data.tsv")
fchar5 <- read_tsv("D:/R/InsectsArctic/Data/FCHAR/taxonomy.tsv")
fchar7 <- read_tsv("D:/R/InsectsArctic/Data/FCHAR/fchar-sequencedata.tsv")
# CBAY ARCBIO 2021 - Cambridge Bay, Nunavut - Malaise and general collection
cbay1 <- read_tsv("D:/R/InsectsArctic/Data/CBAY/collection_data.tsv")
cbay5 <- read_tsv("D:/R/InsectsArctic/Data/CBAY/taxonomy.tsv")
cbay7 <- read_tsv("D:/R/InsectsArctic/Data/CBAY/cbay-sequencedata.tsv")
# KUGA ARCBIO 2021 - Kugaaruk, Nunavut - Malaise and general collection
kuga1 <- read_tsv("D:/R/InsectsArctic/Data/KUGA/collection_data.tsv")
kuga5 <- read_tsv("D:/R/InsectsArctic/Data/KUGA/taxonomy.tsv")
kuga7 <- read_tsv("D:/R/InsectsArctic/Data/KUGA/kuga-sequencedata.tsv")
# GJOA ARCBIO 2021 - Gjoa Haven, Nunavut - Malaise and general collection
gjoa1 <- read_tsv("D:/R/InsectsArctic/Data/GJOA/collection_data.tsv")
gjoa5 <- read_tsv("D:/R/InsectsArctic/Data/GJOA/taxonomy.tsv")
gjoa7 <- read_tsv("D:/R/InsectsArctic/Data/GJOA/gjoa-sequencedata.tsv")

# Let's separate out Collembola
bchar_collembola <- bchar5 %>%
  filter(Class == "Collembola")
cchar_collembola <- cchar5 %>%
  filter(Class == "Collembola")
dchar_collembola <- dchar5 %>%
  filter(Class == "Collembola")
echar_collembola <- echar5 %>%
  filter(Class == "Collembola")
fchar_collembola <- fchar5 %>%
  filter(Class == "Collembola")
cbay_collembola <- cbay5 %>%
  filter(Class == "Collembola")
kuga_collembola <- kuga5 %>%
  filter(Class == "Collembola")
gjoa_collembola <- gjoa5 %>%
  filter(Class == "Collembola")

# Rename/Join columns ----------------------------------------------------
########## BCHAR
names(bchar7) <- c("seq.data","seq.text","process.id","taxon","Sample ID","bin.uri")
test <- inner_join(bchar5, bchar7, by="Sample ID")
test1 <- inner_join(test, bchar1, by="Sample ID")
temp <- data.frame(bchar_collembola$"Sample ID")
names(temp) <- c("Sample ID")
test2 <- inner_join(test1, temp, by="Sample ID")

bchar_final <- test1
bchar_collembola <- test2
rm(test,test1,test2)

########## CCHAR
names(cchar7) <- c("seq.data","seq.text","process.id","taxon","Sample ID","bin.uri")
test <- inner_join(cchar5, cchar7, by="Sample ID")
test1 <- inner_join(test, cchar1, by="Sample ID")
temp <- data.frame(cchar_collembola$"Sample ID")
names(temp) <- c("Sample ID")
test2 <- inner_join(test1, temp, by="Sample ID")

cchar_final <- test1
cchar_collembola <- test2
rm(test,test1,test2)

########## DCHAR
names(dchar7) <- c("seq.data","seq.text","process.id","taxon","Sample ID","bin.uri")
test <- inner_join(dchar5, dchar7, by="Sample ID")
test1 <- inner_join(test, dchar1, by="Sample ID")
temp <- data.frame(dchar_collembola$"Sample ID")
names(temp) <- c("Sample ID")
test2 <- inner_join(test1, temp, by="Sample ID")

dchar_final <- test1
dchar_collembola <- test2
rm(test,test1,test2)

########## ECHAR
names(echar7) <- c("seq.data","seq.text","process.id","taxon","Sample ID","bin.uri")
test <- inner_join(echar5, echar7, by="Sample ID")
test1 <- inner_join(test, echar1, by="Sample ID")
temp <- data.frame(echar_collembola$"Sample ID")
names(temp) <- c("Sample ID")
test2 <- inner_join(test1, temp, by="Sample ID")

echar_final <- test1
echar_collembola <- test2
rm(test,test1,test2)

########## FCHAR
names(fchar7) <- c("seq.data","seq.text","process.id","taxon","Sample ID","bin.uri")
test <- inner_join(fchar5, fchar7, by="Sample ID")
test1 <- inner_join(test, fchar1, by="Sample ID")
temp <- data.frame(fchar_collembola$'Sample ID')
names(temp) <- c("Sample ID")
test2 <- inner_join(test1, temp, by="Sample ID")

fchar_final <- test1
fchar_collembola <- test2
rm(test,test1,test2)

########## CBAY
names(cbay7) <- c("seq.data","seq.text","process.id","taxon","Sample ID","bin.uri")
test <- inner_join(cbay5, cbay7, by="Sample ID")
test1 <- inner_join(test, cbay1, by="Sample ID")
temp <- data.frame(cbay_collembola$"Sample ID")
names(temp) <- c("Sample ID")
test2 <- inner_join(test1, temp, by="Sample ID")

cbay_final <- test1
cbay_collembola <- test2
rm(test,test1,test2)

########## KUGA
names(kuga7) <- c("seq.data","seq.text","process.id","taxon","Sample ID","bin.uri")
test <- inner_join(kuga5, kuga7, by="Sample ID")
test1 <- inner_join(test, kuga1, by="Sample ID")
temp <- data.frame(kuga_collembola$"Sample ID")
names(temp) <- c("Sample ID")
test2 <- inner_join(test1, temp, by="Sample ID")

kuga_final <- test1
kuga_collembola <- test2
rm(test,test1,test2)

########## GJOA
names(gjoa7) <- c("seq.data","seq.text","process.id","taxon","Sample ID","bin.uri")
test <- inner_join(gjoa5, gjoa7, by="Sample ID")
test1 <- inner_join(test, gjoa1, by="Sample ID")
temp <- data.frame(gjoa_collembola$"Sample ID")
names(temp) <- c("Sample ID")
test2 <- inner_join(test1, temp, by="Sample ID")

gjoa_final <- test1
gjoa_collembola <- test2
rm(test,test1,test2)

# Combine all the finals together
test <- rbind(bchar_final,cchar_final)
test1 <- rbind(test,dchar_final)
test2 <- rbind(test1,echar_final)
test3 <- rbind(test2,fchar_final)
test4 <- rbind(test3,cbay_final)
test5 <- rbind(test4,kuga_final)
test6 <- rbind(test5,gjoa_final)
final_final <- test6

# Load our data set, contains traps from Cambridge Bay, Kugluktuk, Gjoa Haven, Kugaaruk
write_tsv(x = final_final, "D:/R/InsectsArctic/Data/working_dataset.tsv")
