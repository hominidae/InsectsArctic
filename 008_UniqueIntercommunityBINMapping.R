# 

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

# Let's make a data frame containing the BINs that are present in all four communities
list_kitikmeot = list(cbay_bins,kugl_bins,gjoa_bins,kuga_bins)
kitikmeot_bins <- list_kitikmeot %>%
  reduce(inner_join, by="bin_uri")

# Take that list of BINs that are present in all four communities and produce a new target
kitikmeot_sharedbins <- kitikmeot_bold %>%
  filter(bin_uri %in% kitikmeot_bins[,1])

# Let's perform a count of those BINs from the kitikmeot
kitikmeot_count <- as.data.frame(table(kitikmeot_sharedbins$bin_uri))
names(kitikmeot_count)[names(kitikmeot_count) == "Var1"] <- "bin_uri"
write_csv(x = kitikmeot_count, "C:/R/InsectsArctic/data/kitikmeot_count.csv")

# We need to make a combination of records for the land bound communitites, Kugluktuk and Kugaaruk
land_target <- rbind(kugluktuk,kugaaruk)

# Let's do the same but shared BINs between the land bound locations (Kugluktuk, Kugaaruk)
list_land = list(kugl_bins,kuga_bins)
land_bins <- list_land %>%
  reduce(inner_join, by="bin_uri")
# Filter BINs for land bound communities
land_sharedbins <- land_target %>%
  filter(bin_uri %in% land_bins[,1])
# Let's perform a count and save
land_count <- as.data.frame(table(land_sharedbins$bin_uri))
names(land_count)[names(land_count) == "Var1"] <- "bin_uri"
write_csv(x = land_count, "C:/R/InsectsArctic/data/land_count.csv")

# We also want to make a combination of records for the island communities, Cambridge Bay and GJoa Haven
island_target <- rbind(cambridgebay,gjoahaven)

# Let's do the same but shared BINs between the island locations (Cambridge Bay, Gjoa Haven)
list_island = list(cbay_bins,gjoa_bins)
island_bins <- list_island %>%
  reduce(inner_join, by="bin_uri")
# Filter BINs for island communities
island_sharedbins <- island_target %>%
  filter(bin_uri %in% island_bins[,1])
# Let's do a count and save
island_count <- as.data.frame(table(island_sharedbins$bin_uri))
names(island_count)[names(island_count) == "Var1"] <- "bin_uri"
write_csv(x = island_count, "C:/R/InsectsArctic/data/island_count.csv")

# You'll need to enter a Google Maps API key
register_google(key = "YOURKEYHERE")

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

# Right, so drawing lines from communities doesn't really accomplish the same thing it does when the GPS coordinates are more disparate.
# Let's switch tacks and try something else instead.

# To do that, we need to separate kitikmeot_bold into communities, select our BINs then go from there
# Let's do all of the Kitikmeot once
kitikmeot_diptera <- kitikmeot_bold %>%
  filter(order_name == "Diptera")
kitikmeot_hemiptera <- kitikmeot_bold %>%
  filter(order_name == "Hemiptera")
kitikmeot_hymenoptera <- kitikmeot_bold %>%
  filter(order_name == "Hymenoptera")
kitikmeot_lepidoptera <- kitikmeot_bold %>%
  filter(order_name == "Lepidoptera")
kitikmeot_thysanoptera <- kitikmeot_bold %>%
  filter(order_name == "Thysanoptera")
kitikmeot_neuroptera <- kitikmeot_bold %>%
  filter(order_name == "Neuroptera")
kitikmeot_orthoptera <- kitikmeot_bold %>%
  filter(order_name == "Orthoptera")
kitikmeot_flying <- rbind(kitikmeot_diptera,kitikmeot_hemiptera,kitikmeot_hymenoptera,kitikmeot_lepidoptera,kitikmeot_thysanoptera,kitikmeot_neuroptera,kitikmeot_orthoptera)
rm(kitikmeot_diptera,kitikmeot_hemiptera,kitikmeot_hymenoptera,kitikmeot_lepidoptera,kitikmeot_thysanoptera,kitikmeot_neuroptera,kitikmeot_orthoptera)

# Next, make a non-flying list too
kitikmeot_araneae <- kitikmeot_bold %>%
  filter(order_name == "Araneae")
kitikmeot_coleoptera <- kitikmeot_bold %>%
  filter(order_name == "Coleoptera")
kitikmeot_entomobryomorpha <- kitikmeot_bold %>%
  filter(order_name == "Entomobryomorpha")
kitikmeot_poduromorpha <- kitikmeot_bold %>%
  filter(order_name == "Poduromorpha")
kitikmeot_symphypleona <- kitikmeot_bold %>%
  filter(order_name == "Symphypleona")
kitikmeot_sarcoptiformes <- kitikmeot_bold %>%
  filter(order_name == "Sarcoptiformes")
kitikmeot_trombidiformes <- kitikmeot_bold %>%
  filter(order_name == "Trombidiformes")
kitikmeot_mesostigmata <- kitikmeot_bold %>%
  filter(order_name == "Mesostigmata")
kitikmeot_nonflying <- rbind(kitikmeot_araneae,kitikmeot_coleoptera,kitikmeot_entomobryomorpha,kitikmeot_poduromorpha,kitikmeot_symphypleona,kitikmeot_sarcoptiformes,kitikmeot_trombidiformes,kitikmeot_mesostigmata)
rm(kitikmeot_araneae,kitikmeot_coleoptera,kitikmeot_entomobryomorpha,kitikmeot_poduromorpha,kitikmeot_symphypleona,kitikmeot_sarcoptiformes,kitikmeot_trombidiformes,kitikmeot_mesostigmata)

# Before we move on though, let's save our data as kitikmeot_flying.csv and kitikmeot_nonflying.csv
write_csv(x = kitikmeot_flying, "C:/R/InsectsArctic/data/kitikmeot_flying.csv")
write_csv(x = kitikmeot_nonflying, "C:/R/InsectsArctic/data/kitikmeot_nonflying.csv")

# Install ggVennDiagram
install.packages("ggVennDiagram")

# Load ggVennDiagram
library(ggVennDiagram)

# First things first, we have cambridgebay, kugluktuk, gjoahaven, and kugaaruk
# These were parsed out from kitikmeot_bold
# Assign our variables to a vector list variable x
x <- kitikmeot_bold %>%
  select(sector,bin_uri) %>%
  drop_na() %>%
  group_by(sector) %>%
  nest() %>%
  pull() %>%
  map(unique) %>%
  map(pull)

# Generate a simple colour blind friendly Venn Diagram
p <- ggVennDiagram(x, category.names = c("Cambridge Bay","Gjoa Haven","Kugaaruk","Kugluktuk"),
              label_alpha = 0) +
  guides(fill = guide_legend(title = "# of Unique BINs")) +
  theme(legend.title = element_text(color = "black"),
        legend.position = "right") +
  scale_fill_distiller(palette = "RdBu") +
  labs(title = "Unique matching BINs per community")
  
 
# Don't just take ggVennDiagram at it's word though, double-check that against our total unique BIN counts performed earlier
# Cambridge Bay: 971 Unique BINs
#  620+17+15+159+32+23+16+89=971
# Gjoa Haven: 240 Unique BINs
#  71+89+16+23+32+6+3=240
# Kugaaruk: 105 Unique BINs
#  21+10+3+16+23+17+15=105
# Kugluktuk: 1074 Unique BINs
#  829+6+32+159+15+23+10=1074

# Let's go ahead and create two more and put 'em side by side. One for flying, one for non-flying
# To do that, we need to combine the four flying together
kitikmeot_flying <- read_csv("C:/R/InsectsArctic/data/kitikmeot_flying.csv")
kitikmeot_nonflying <- read_csv("C:/R/InsectsArctic/data/kitikmeot_nonflying.csv")

# Let's do the same thing again, but this time let's stitch 'em together side by side.
# First, let's load gridExtra to do that
library(gridExtra)

# Let's create a plot for matching flying arthropod BINs
fly <- kitikmeot_flying  %>%
  select(sector,bin_uri) %>%
  drop_na() %>%
  group_by(sector) %>%
  nest() %>%
  pull() %>%
  map(unique) %>%
  map(pull)

flyp <- ggVennDiagram(fly, category.names = c("Cambridge Bay","Gjoa Haven","Kugaaruk","Kugluktuk"),
                         label_alpha = 0) +
  guides(fill = guide_legend(title = "# of Unique BINs")) +
  theme(legend.title = element_text(color = "black"),
        legend.position = "right") +
  scale_fill_distiller(palette = "RdBu") +
  labs(title = "Unique matching flying BINs per community")

# Let's do non-flying next
nofly <- kitikmeot_nonflying  %>%
  select(sector,bin_uri) %>%
  drop_na() %>%
  group_by(sector) %>%
  nest() %>%
  pull() %>%
  map(unique) %>%
  map(pull)

noflyp <- ggVennDiagram(nofly, category.names = c("Cambridge Bay","Gjoa Haven","Kugaaruk","Kugluktuk"),
                      label_alpha = 0) +
  guides(fill = guide_legend(title = "# of Unique BINs")) +
  theme(legend.title = element_text(color = "black"),
        legend.position = "right") +
  scale_fill_distiller(palette = "RdBu") +
  labs(title = "Unique matching non-flying BINs per community")

# Alright, let's plot 'em together
grid.arrange(p, arrangeGrob(flyp, noflyp), ncol = 2)
