# Unit Test of ggVennDiagram
# Objective:
#  - Perform a simple test to determine if ggVennDiagram is doing what is intended
#  - Prior to using the ggVennDiagram library to produce graphs illustrating difference between unique BIN's, make sure it does what is intended.

# Load library
library(tidyverse)
library(ggVennDiagram)

# Load the unit test data set
unit_test <- read_csv("D:/R/InsectsArctic/Data/UnitTest.csv") 

# Assign it to a vector list variable called x
x <- unit_test %>%
  select(Location,Surname) %>%
  drop_na() %>%
  group_by(Location) %>%
  nest() %>%
  pull() %>%
  map(unique) %>%
  map(pull)

# Unit test #1
# 1 unique to Kugluktuk
# 3 unique to Cambridge Bay
# 3 that are shared between Cambridge Bay and Kugluktuk
ggVennDiagram(x, category.names = c("Cambridge Bay","Kugluktuk"),
              label_alpha = 0) +
  guides(fill = guide_legend(title = "Unique Surnames")) +
  theme(legend.title = element_text(color = "black"),
        legend.position = "right")

# Unit test #2 load data
unit_test2 <- read_csv("D:/R/InsectsArctic/Data/UnitTest2.csv")

# Assign that data to a vector list variable called y
y <- unit_test2 %>%
  select(Location,Surname) %>%
  drop_na() %>%
  group_by(Location) %>%
  nest() %>%
  pull() %>%
  map(unique) %>%
  map(pull)

# Unit test #2
# 5 unique names to Cambridge Bay
# 3 names shared between Cambridge Bay and Kugluktuk
# 1 unique name to Kugluktuk
ggVennDiagram(y, category.names = c("Cambridge Bay","Kugluktuk"),
              label_alpha = 0) +
  guides(fill = guide_legend(title = "Unique Surnames")) +
  theme(legend.title = element_text(color = "black"),
        legend.position = "right")

#Unit test #3 load data
unit_test3 <- read_csv("D:/R/InsectsArctic/Data/UnitTest3.csv")

# Assign that data to a vector list variable called y
e <- unit_test3 %>%
  select(Location,Surname) %>%
  drop_na() %>%
  group_by(Location) %>%
  nest() %>%
  pull() %>%
  map(unique) %>%
  map(pull)

# Unit test #3
# 5 unique names to Cambridge Bay
# 4 names shared between Cambridge Bay and Kugluktuk
# 1 unique name to Kugluktuk
ggVennDiagram(e, category.names = c("Cambridge Bay","Kugluktuk"),
              label_alpha = 0) +
  guides(fill = guide_legend(title = "Unique Surnames")) +
  theme(legend.title = element_text(color = "black"),
        legend.position = "right")
