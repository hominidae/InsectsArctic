

# Load libraries
library(tidyverse)
library(ggmap)
library(gridExtra)

# Great. Now we need to work on community level links within the Kitikmeot to each other.
# Let's load the data set we created earlier.
kitikmeot_bold <- read_tsv("C:/R/InsectsArctic/data/kitikmeot_bold_target.tsv")

# Fix naming issues
names(kitikmeot_bold)[names(kitikmeot_bold) == "Order"] <- "order_name"
names(kitikmeot_bold)[names(kitikmeot_bold) == "bin.uri"] <- "bin_uri"
names(kitikmeot_bold)[names(kitikmeot_bold) == "Lat"] <- "lat"
names(kitikmeot_bold)[names(kitikmeot_bold) == "Lon"] <- "lon"

# Split by communities again with the corrected names
cambridgebay <- kitikmeot_bold %>%
  filter(sector == "Cambridge Bay")
kugluktuk <- kitikmeot_bold %>%
  filter(sector == "Kugluktuk")
gjoahaven <- kitikmeot_bold %>%
  filter(sector == "Gjoa Haven")
kugaaruk <- kitikmeot_bold %>%
  filter(sector == "Kugaaruk")

# Right, now that we have the communities separated let's extract unique bin's from each and save as a vector
cbay_bins <- unique(cambridgebay$bin_uri)
kugl_bins <- unique(kugluktuk$bin_uri)
kuga_bins <- unique(kugaaruk$bin_uri)
gjoa_bins <- unique(gjoahaven$bin_uri)

# Turn them into a data frame and name the columns
cbay_bins <- data.frame(
  bin_uri = cbay_bins
)
kugl_bins <- data.frame(
  bin_uri = kugl_bins
)
kuga_bins <- data.frame(
  bin_uri = kuga_bins
)
gjoa_bins <- data.frame(
  bin_uri = gjoa_bins
)

# Here, we're going to join three communities to match against the unique bins present in other communities
cbay_target <- rbind(kugluktuk,gjoahaven,kugaaruk)
kugl_target <- rbind(cambridgebay,gjoahaven,kugaaruk)
gjoa_target <- rbind(cambridgebay,kugluktuk,kugaaruk)
kuga_target <- rbind(cambridgebay,gjoahaven,kugluktuk)

# Alright, let's do the community level BIN comparisons. *_bins are the unique bins present in each location
cbay_sharedbins <- cbay_target %>%
  filter(bin_uri %in% cbay_bins[,1])
# Next let's do Kugluktuk to the other locations
kugl_sharedbins <- kugl_target %>%
  filter(bin_uri %in% kugl_bins[,1])
# Gjoa Haven to the other locations
gjoa_sharedbins <- gjoa_target %>%
  filter(bin_uri %in% gjoa_bins[,1])
# Kugaaruk to other locations
kuga_sharedbins <- kuga_target %>%
  filter(bin_uri %in% kuga_bins[,1])

# Create a data frame containing our lat's and lon's for the center of each communitiy
communities <- data.frame(
  community = c("Cambridge Bay", "Kugluktuk","Gjoa Haven","Kugaaruk"),
  lat = c(69.1181,67.8241,68.6352,68.5366),
  lon = c(-105.0615,-115.1006,-95.8474,-89.8174)
)

# Before we map that, let's adjust our focus
map_bold <- get_map(
  location = c(left = -120, bottom = 60, right = -80, top = 80),
  scale = "auto",
  zoom = 4,
  source = "google",
  force = TRUE)

# let's have a look at it and assign it to a callable variable
ggmap(map_bold)
mp <- ggmap(map_bold, extent = "panel") +
  #scale_y_continuous(limits = c(60,75), expand=c(0,0)) +
  #scale_x_continuous(limits = c(-130,-85), expand=c(0,0)) +
  geom_point(data = communities, aes(x = lon, y = lat)) +
  geom_text(data = communities, aes(label = community),vjust = -1,hjust = 0.5, size=5)
mp

# Let's see what these look like on a map
cbay2kitikmeot_mp <- mp+
  geom_segment(data = cbay_sharedbins,
               aes(x = lon, y = lat,
                   color=order_name,
                   xend = communities$lon[1],
                   yend = communities$lat[1]
               )) +
  geom_point(data = cbay_sharedbins,
             aes(x = lon, y = lat,
                 color=order_name), size=1) +
  labs(x = "Longitude", y = "Latitude", color="Orders",
       title="Exact BIN matches between Cambridge Bay and other communities")
cbay2kitikmeot_mp

# Let's map Kugluktuk community level links next
kugl2kitikmeot_mp <- mp+
  geom_segment(data = kugl_sharedbins,
               aes(x = lon, y = lat,
                   color=order_name,
                   xend = communities$lon[2],
                   yend = communities$lat[2]
               )) +
  geom_point(data = kugl_sharedbins,
             aes(x = lon, y = lat,
                 color=order_name), size=1) +
  labs(x = "Longitude", y = "Latitude", color="Orders",
       title="Exact BIN matches between Kugluktuk and other communities")
kugl2kitikmeot_mp

# Let's do Gjoa Haven next
gjoa2kitikmeot_mp <- mp+
  geom_segment(data = gjoa_sharedbins,
               aes(x = lon, y = lat,
                   color=order_name,
                   xend = communities$lon[3],
                   yend = communities$lat[3]
               )) +
  geom_point(data = gjoa_sharedbins,
             aes(x = lon, y = lat,
                 color=order_name), size=1) +
  labs(x = "Longitude", y = "Latitude", color="Orders",
       title="Exact BIN matches between Gjoa Haven and other communities")
gjoa2kitikmeot_mp

# Last, let's do Kugaaruk
kuga2kitikmeot_mp <- mp+
  geom_segment(data = kuga_sharedbins,
               aes(x = lon, y = lat,
                   color=order_name,
                   xend = communities$lon[4],
                   yend = communities$lat[4]
               )) +
  geom_point(data = kuga_sharedbins,
             aes(x = lon, y = lat,
                 color=order_name), size=1) +
  labs(x = "Longitude", y = "Latitude", color="Orders",
       title="Exact BIN matches between Kugaaruk and other communities")
kuga2kitikmeot_mp

# Because generating a map with lines pointing to each community is kinda boring, let's try a different approach.
# Basically, we want a box plot per community.
# The boxplot should show unique bin's and the count of those bin's per community.

# Let's make some hasty counts, we want a count of the number of BIN's
cbay_count <- as.data.frame(table(cambridgebay$bin_uri))
names(cbay_count)[names(cbay_count) == "Var1"] <- "bin_uri"
# Next, do Kugluktuk
kugl_count <- as.data.frame(table(kugluktuk$bin_uri))
names(kugl_count)[names(kugl_count) == "Var1"] <- "bin_uri"
# Next, do Gjoa Haven
gjoa_count <- as.data.frame(table(gjoahaven$bin_uri))
names(gjoa_count)[names(gjoa_count) == "Var1"] <- "bin_uri"
# Last, do Kugaaruk
kuga_count <- as.data.frame(table(kugaaruk$bin_uri))
names(kuga_count)[names(kuga_count) == "Var1"] <- "bin_uri"

# Test with an inner join, Cambridge Bay and Kugluktuk
comparecbay2kugl <- inner_join(cbay_count,kugl_count)
comparecbay2gjoa <- inner_join(cbay_count,gjoa_count)
comparecbay2kuga <- inner_join(cbay_count,kuga_count)
