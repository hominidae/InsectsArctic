# Generate a species accumulation curve using iNEXT
# The species accumulation curves indicate based on the available abundance data how well sampling is going relative to community sample size

# Load libraries
library(tidyverse)
library(iNEXT)
library(dplyr)
library(gridExtra)
library(grid)

# Load the data
kitikmeot_flying <- read_csv("data/kitikmeot_flying.csv")
kitikmeot_nonflying <- read_csv("data/kitikmeot_nonflying.csv")

# Let's create a S by N abundance matrix, S is number of species, N is the number of sites
out <- kitikmeot_flying %>%
  group_by(bin_uri, sector) %>%
  summarise(abundance = n()) %>%
  pivot_wider(names_from = sector, values_from = abundance, values_fill = 0) %>%
  ungroup() %>%
  select(!bin_uri) %>%
  as.data.frame()

# Generate a species accumulation curve graph
outc <- iNEXT(out, q=0, datatype="abundance", nboot=100)
p1 <- ggiNEXT(outc, type=1)

# Next, do the non-flying arthropods
out_nf <- kitikmeot_nonflying %>%
  group_by(bin_uri, sector) %>%
  summarise(abundance = n()) %>%
  pivot_wider(names_from = sector, values_from = abundance, values_fill = 0) %>%
  ungroup() %>%
  select(!bin_uri) %>%
  as.data.frame()

# Generate species richness rarefaction curves aka species accumulation curve
outnfc <- iNEXT(out_nf, q=0, datatype="abundance")
# Visualize 
p2 <- ggiNEXT(outnfc, type=1)

# Setup side by side
grid.arrange(arrangeGrob(p1,top=textGrob("Flying species accumulation curve"),ncol=1),
             arrangeGrob(p2, top=textGrob("Non-flying species accumulation curve"),ncol=1),
             ncol=2)
