# Process data from projects in Cambridge Bay/Kugluktuk
# OBJECTIVE:
#  - Turn available BOLD data into a Venn Diagram

# Load libraries
library(tidyverse)
library(ggVennDiagram)

# Load the processed data from 002_LoadBOLDData.R
camkug <- read_tsv("D:/R/InsectsArctic/Data/camkug.tsv")

# Assign it to a vector list
x <- camkug %>%
  select(bin.uri,Sector) %>%
  drop_na() %>%
  group_by(Sector) %>%
  nest() %>%
  pull() %>%
  map(unique) %>%
  map(pull)

ggVennDiagram(x, category.names = c("Cambridge Bay","Kugluktuk"),
              label_alpha = 0) +
              guides(fill = guide_legend(title = "BINs")) +
                theme(legend.title = element_text(color = "black"),
                      legend.position = "right")
