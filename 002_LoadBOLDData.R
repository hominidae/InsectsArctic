# Process data from collection efforts in Cambridge Bay

# OBJECTIVE:
#  - Recombine tsv files into singular tsv files

# Load Libraries ####
library(tidyverse)
library(dplyr)

# Load Data ####
# BCHAR Biodiversity Survey - Malaise Trap Samples 2018
bchar1 <- read_tsv("Data/BCHAR/collection_data.tsv")
bchar5 <- read_tsv("Data/BCHAR/taxonomy.tsv")
bchar7 <- read_tsv("Data/BCHAR/bchar-sequencedata.tsv")
# CCHAR Biodiversity Survey - Standardized Sampling - Intensive Monitoring Area Site 2018
cchar1 <- read_tsv("Data/CCHAR/collection_data.tsv")
cchar5 <- read_tsv("Data/CCHAR/taxonomy.tsv")
cchar7 <- read_tsv("Data/CCHAR/cchar-sequencedata.tsv")
# DCHAR Biodiversity Survey - Standardized Sampling - Water Lake Site 2018
dchar1 <- read_tsv("Data/DCHAR/collection_data.tsv")
dchar5 <- read_tsv("Data/DCHAR/taxonomy.tsv")
dchar7 <- read_tsv("Data/DCHAR/dchar-sequencedata.tsv")
# FCHAR Biodiversity Survey 2019 - General Terrestrial Collection (Cambridge Bay & Kugluktuk)
fchar1 <- read_tsv("Data/FCHAR/collection_data.tsv")
fchar5 <- read_tsv("Data/FCHAR/taxonomy.tsv")
fchar7 <- read_tsv("Data/FCHAR/fchar-sequencedata.tsv")
# GCHAR Biodiversity Survey 2019 - Freshwater Aquatic Collection
gchar1 <- read_tsv("Data/GCHAR/collection_data.tsv")
gchar5 <- read_tsv("Data/GCHAR/taxonomy.tsv")
gchar7 <- read_tsv("Data/GCHAR/gchar-sequencedata.tsv")
# HCHAR Biodiversity Survey 2019 - Marine Aquatic Collection 2019 (Likely not the target, but will need to see)
hchar1 <- read_tsv("Data/HCHAR/collection_data.tsv")
hchar5 <- read_tsv("Data/HCHAR/taxonomy.tsv")
hchar7 <- read_tsv("Data/HCHAR/hchar-sequencedata.tsv")
# CBAY ARCBIO 2021 - Cambridge Bay, Nunavut - Malaise and general collection
cbay1 <- read_tsv("Data/CBAY/collection_data.tsv")
cbay5 <- read_tsv("Data/CBAY/taxonomy.tsv")
cbay7 <- read_tsv("Data/CBAY/cbay-sequencedata.tsv")
# KUGA ARCBIO 2021 - Kugaaruk, Nunavut - Malaise and general collection
kuga1 <- read_tsv("Data/KUGA/collection_data.tsv")
kuga5 <- read_tsv("Data/KUGA/taxonomy.tsv")
kuga7 <- read_tsv("Data/KUGA/kuga-sequencedata.tsv")
# GJOA ARCBIO 2021 - Gjoa Haven, Nunavut - Malaise and general collection
gjoa1 <- read_tsv("Data/GJOA/collection_data.tsv")
gjoa5 <- read_tsv("Data/GJOA/taxonomy.tsv")
gjoa7 <- read_tsv("Data/GJOA/gjoa-sequencedata.tsv")
# KUGL ARCBIO 2021 - Malaise and general collection 2021
kugl1 <- read_tsv("Data/KUGL/collection_data.tsv")
kugl5 <- read_tsv("Data/KUGL/taxonomy.tsv")
kugl7 <- read_tsv("Data/KUGL/kugl-sequencedata.tsv")

# Let's separate out all arthropods.
bchar_arth <- bchar5 %>%
  filter(Phylum == "Arthropoda")
cchar_arth <- cchar5 %>%
  filter(Phylum == "Arthropoda")
dchar_arth <- dchar5 %>%
  filter(Phylum == "Arthropoda")
fchar_arth <- fchar5 %>%
  filter(Phylum == "Arthropoda")
gchar_arth <- gchar5 %>%
  filter(Phylum == "Arthropoda")
hchar_arth <- hchar5 %>%
  filter(Phylum == "Arthropoda")
cbay_arth <- cbay5 %>%
  filter(Phylum == "Arthropoda")
kuga_arth <- kuga5 %>%
  filter(Phylum == "Arthropoda")
gjoa_arth <- gjoa5 %>%
  filter(Phylum == "Arthropoda")
kugl_arth <- kugl5 %>%
  filter(Phylum == "Arthropoda")

# Rename/Join columns ####
########## BCHAR
names(bchar7) <- c("seq.data","seq.text","process.id","taxon","Sample ID","bin.uri")
test <- inner_join(bchar5, bchar7, by="Sample ID")
test1 <- inner_join(test, bchar1, by="Sample ID")
temp <- data.frame(bchar_arth$"Sample ID")
names(temp) <- c("Sample ID")
test2 <- inner_join(test1, temp, by="Sample ID")

bchar_final <- test1
bchar_arth <- test2
rm(temp,test,test1,test2)
rm(bchar1,bchar5,bchar7)

########## CCHAR
names(cchar7) <- c("seq.data","seq.text","process.id","taxon","Sample ID","bin.uri")
test <- inner_join(cchar5, cchar7, by="Sample ID")
test1 <- inner_join(test, cchar1, by="Sample ID")
temp <- data.frame(cchar_arth$"Sample ID")
names(temp) <- c("Sample ID")
test2 <- inner_join(test1, temp, by="Sample ID")

cchar_final <- test1
cchar_arth <- test2
rm(temp,test,test1,test2)
rm(cchar1,cchar5,cchar7)

########## DCHAR
names(dchar7) <- c("seq.data","seq.text","process.id","taxon","Sample ID","bin.uri")
test <- inner_join(dchar5, dchar7, by="Sample ID")
test1 <- inner_join(test, dchar1, by="Sample ID")
temp <- data.frame(dchar_arth$"Sample ID")
names(temp) <- c("Sample ID")
test2 <- inner_join(test1, temp, by="Sample ID")

dchar_final <- test1
dchar_arth <- test2
rm(temp,test,test1,test2)
rm(dchar1,dchar5,dchar7)

########## FCHAR
names(fchar7) <- c("seq.data","seq.text","process.id","taxon","Sample ID","bin.uri")
test <- inner_join(fchar5, fchar7, by="Sample ID")
test1 <- inner_join(test, fchar1, by="Sample ID")
temp <- data.frame(fchar_arth$'Sample ID')
names(temp) <- c("Sample ID")
test2 <- inner_join(test1, temp, by="Sample ID")

fchar_final <- test1
fchar_arth <- test2
rm(temp,test,test1,test2)
rm(fchar1,fchar5,fchar7)

########## GCHAR
names(gchar7) <- c("seq.data","seq.text","process.id","taxon","Sample ID","bin.uri")
test <- inner_join(gchar5, gchar7, by="Sample ID")
test1 <- inner_join(test, gchar1, by="Sample ID")
temp <- data.frame(gchar_arth$'Sample ID')
names(temp) <- c("Sample ID")
test2 <- inner_join(test1, temp, by="Sample ID")

gchar_final <- test1
gchar_arth <- test2
rm(temp,test,test1,test2)
rm(gchar1,gchar5,gchar7)

########## HCHAR
names(hchar7) <- c("seq.data","seq.text","process.id","taxon","Sample ID","bin.uri")
test <- inner_join(hchar5, hchar7, by="Sample ID")
test1 <- inner_join(test, hchar1, by="Sample ID")
temp <- data.frame(hchar_arth$'Sample ID')
names(temp) <- c("Sample ID")
test2 <- inner_join(test1, temp, by="Sample ID")

hchar_final <- test1
hchar_arth <- test2
rm(temp,test,test1,test2)
rm(hchar1,hchar5,hchar7)

########## CBAY
names(cbay7) <- c("seq.data","seq.text","process.id","taxon","Sample ID","bin.uri")
test <- inner_join(cbay5, cbay7, by="Sample ID")
test1 <- inner_join(test, cbay1, by="Sample ID")
temp <- data.frame(cbay_arth$"Sample ID")
names(temp) <- c("Sample ID")
test2 <- inner_join(test1, temp, by="Sample ID")

cbay_final <- test1
cbay_arth <- test2
rm(temp,test,test1,test2)
rm(cbay1,cbay5,cbay7)

########## KUGA
names(kuga7) <- c("seq.data","seq.text","process.id","taxon","Sample ID","bin.uri")
test <- inner_join(kuga5, kuga7, by="Sample ID")
test1 <- inner_join(test, kuga1, by="Sample ID")
temp <- data.frame(kuga_arth$"Sample ID")
names(temp) <- c("Sample ID")
test2 <- inner_join(test1, temp, by="Sample ID")

kuga_final <- test1
kuga_arth <- test2
rm(temp,test,test1,test2)
rm(kuga1,kuga5,kuga7)

########## GJOA
names(gjoa7) <- c("seq.data","seq.text","process.id","taxon","Sample ID","bin.uri")
test <- inner_join(gjoa5, gjoa7, by="Sample ID")
test1 <- inner_join(test, gjoa1, by="Sample ID")
temp <- data.frame(gjoa_arth$"Sample ID")
names(temp) <- c("Sample ID")
test2 <- inner_join(test1, temp, by="Sample ID")

gjoa_final <- test1
gjoa_arth <- test2
rm(temp,test,test1,test2)
rm(gjoa1,gjoa5,gjoa7)

########## KUGL
names(kugl7) <- c("seq.data","seq.text","process.id","taxon","Sample ID","bin.uri")
test <- inner_join(kugl5, kugl7, by="Sample ID")
test1 <- inner_join(test, kugl1, by="Sample ID")
temp <- data.frame(kugl_arth$"Sample ID")
names(temp) <- c("Sample ID")
test2 <- inner_join(test1, temp, by="Sample ID")

kugl_final <- test1
kugl_arth <- test2
rm(temp,test,test1,test2)
rm(kugl1,kugl5,kugl7)

# Combine all the finals together ####
test <- rbind(bchar_final,cchar_final)
test1 <- rbind(test,dchar_final)
test2 <- rbind(test1,fchar_final)
test3 <- rbind(test2,gchar_final)
test4 <- rbind(test3,hchar_final)
test5 <- rbind(test4,cbay_final)
test6 <- rbind(test5,kuga_final)
test7 <- rbind(test6,gjoa_final)
test8 <- rbind(test7,kugl_final)
final_final <- test5
rm(test,test1,test2,test3,test4,test5,test6,test7,test8)

# Combine arthropods together
test <- rbind(bchar_arth,cchar_arth)
test1 <- rbind(test,dchar_arth)
test2 <- rbind(test1,fchar_arth)
test3 <- rbind(test2,gchar_arth)
test4 <- rbind(test3,hchar_arth)
test5 <- rbind(test4,cbay_arth)
test6 <- rbind(test5,kuga_final)
test7 <- rbind(test6,gjoa_arth)
test8 <- rbind(test7,kugl_arth)
final_arth <- test8
rm(test,test1,test2,test3,test4,test5,test6,test7,test8)

# Write out our tsv file
write_tsv(x = final_final, "Data/kitikmeot_data.tsv")
rm(bchar_final,cchar_final,dchar_final,fchar_final,gchar_final,hchar_final,cbay_final,kuga_final,gjoa_final,kugl_final,final_final)

# Write out another tsv containing just the arthropods
write_tsv(x = final_arth, "Data/kitikmeot_data_arth.tsv")
rm(bchar_arth,cchar_arth,dchar_arth,fchar_arth,gchar_arth,hchar_arth,cbay_arth,kuga_arth,gjoa_arth,kugl_arth,final_arth)
