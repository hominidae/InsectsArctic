# Process data from projects in Cambridge Bay
# OBJECTIVE:
#  - Load sequencing data in FASTA format

# Load package
install.packages("phylotools")
library(phylotools)
install.packages("tidyverse")
library(tidyverse)
install.packages("stringr")
library(stringr)

# Load sequencing data using read.fasta
cchar_sequencedata <- read.fasta(file = "D:/R/InsectsArctic/Data/CCHAR/SequenceData.fas")

# Now that it's loaded, we have an interesting problem.
# We need to separate out the data into their own fields.
# To do that, we're going to use str_extract.

# We need to separate out the following into their own columns.
# Here is what the data inside the first column of dataset looks like:
# ACHAR1776-18|Psammitis deichmanni|CHARS00069-C01|BOLD:AAB7094
# ACHAR1776-18
# Psammitis deichmanni
# CHARS500069-C01
# BOLD:AAB7094

# First, copy the seq.name column to x temporarily
x <- cchar_sequencedata$seq.name

# Next, let's use str_split to take the x list and split it by the | character
y <- x %>%
  str_split("\\|")

# Now that we have that, let's use a simple solution to turn the data from being wide to being tall instead when we turn it into a data frame
cchar_splitdata <- data.frame(Reduce(rbind, y))

cchar_expanded <- data.frame(cchar_sequencedata$seq.text,cchar_sequencedata$seq.name,cchar_splitdata$X1,cchar_splitdata$X2,cchar_splitdata$X3,cchar_splitdata$X4)

# Great! Now that that's done, let's rename all the columns
names(cchar_expanded) <- c("seq.data","seq.text","process.id","taxon","sample.id","bin.uri")

# Next up, we need replace blank entries for BIN URI's with NA's
# Why? Because we're classy like that.
cchar_expanded_na <- cchar_expanded %>%
  mutate_all(na_if,"")

# Fabulous. Now that we have the sequencing data sorted let's do some cool shit.
# That's it for this script. But before we go, let's save our work.
write_tsv(x = cchar_expanded_na, "D:/R/InsectsArctic/Data/CCHAR/cchar-sequencedata.tsv")

# Let's clean up our workspace.
rm(x,y,cchar_sequencedata,cchar_splitdata,cchar_expanded,cchar_expanded_na)

# We'll do the same for FCHAR
fchar_sequencedata <- read.fasta(file = "D:/R/InsectsArctic/Data/FCHAR/SequenceData.fas")

x <- fchar_sequencedata$seq.name

y <- x %>%
  str_split("\\|")

fchar_splitdata <- data.frame(Reduce(rbind, y))

fchar_expanded <- data.frame(fchar_sequencedata$seq.text,fchar_sequencedata$seq.name,fchar_splitdata$X1,fchar_splitdata$X2,fchar_splitdata$X3,fchar_splitdata$X4)

names(fchar_expanded) <- c("seq.data","seq.text","process.id","taxon","sample.id","bin.uri")

fchar_expanded_na <- fchar_expanded %>%
  mutate_all(na_if,"")

write_tsv(x = fchar_expanded_na, "D:/R/InsectsArctic/Data/FCHAR/fchar-sequencedata.tsv")

rm(x,y,fchar_sequencedata,fchar_splitdata,fchar_expanded,fchar_expanded_na)
