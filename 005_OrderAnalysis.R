# Order Analysis
#

# Load library
library(tidyverse)

# Load data
bold_orderanalysis <- read_csv("D:/R/InsectsArctic/Data/BOLD-OrderAnalysis.csv") 

names(bold_orderanalysis) <- c("Order","BIN","Site")

bchar_site <- bold_orderanalysis %>%
  filter(Site == "BCHAR")

dchar_site <- bold_orderanalysis %>%
  filter(Site == "DCHAR")

# Echar not necessary since no arthropoda were present in marine samples

fchar_site <- bold_orderanalysis %>%
  filter(Site == "FCHAR")

cbay_site <- bold_orderanalysis %>%
  filter(Site == "CBAY")

kuga_site <- bold_orderanalysis %>%
  filter(Site == "KUGA")

gjoa_site <- bold_orderanalysis %>%
  filter(Site == "GJOA")

# We simply want a representation of every arthopoda order present in the Kitikmeot region
# Once we have those numbers we can select and perform a count of what is present in all the DNA barcoding data
# Across all the samples, we want to combine order and bin into their own data table

# Table joining test
inner_test <- inner_join(bchar_site, dchar_site, by="Order")
left_test <- left_join(bchar_site, dchar_site, by="Order")
right_test <- right_join(bchar_site, dchar_site, by="Order")
full_test <- full_join(bchar_site, dchar_site, by="Order")
anti_test <- anti_join(bchar_site, dchar_site, by="Order")
semi_test <- semi_join(bchar_site, dchar_site, by="Order")

# Now we need to do more full joins
full_test2 <- full_join(full_test, fchar_site, by="Order")
full_test3 <- full_join(full_test2, cbay_site, by="Order")
full_test4 <- full_join(full_test3, kuga_site, by="Order")
full_test5 <- full_join(full_test4, gjoa_site, by="Order")

# Remove column
new_test <- full_test5 %>%
  select(Order, BIN.x, BIN.y, BIN.x.x, BIN.y.y, BIN.x.x.x, BIN.y.y.y)

names(new_test) <- c("Order","BCHAR","DCHAR","FCHAR","CBAY","KUGA","GJOA")

# Maybe don't use dplyr?
count_new <- new_test %>%
  replace(is.na(.), 0) %>%
  mutate(SUM = rowSums(across(where(is.numeric))))

# Let's write out the data for use later
write_csv(x = count_new, "D:/R/InsectsArctic/Data/BOLD_DataAnalysisSum.csv")
