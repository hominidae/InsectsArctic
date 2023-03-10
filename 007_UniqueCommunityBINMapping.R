# Take unique bin's from kitikmeot region to compare against nearest occurrences in BOLD Canada data

# Load libraries
library(tidyverse)
library(ggmap)

# Load the data
kitikmeot_bold <- read_tsv("data/kitikmeot_bold.tsv")
canada_bold <- read_tsv("data/Canada_data_clean_december.tsv")

# We need to remove aquatic invertabrates and aquatic insects since that's not really what we're looking for.
kitikmeot_arachnida <- kitikmeot_bold %>%
  filter(Class == "Arachnida")
kitikmeot_collembola <- kitikmeot_bold %>%
  filter(Class == "Collembola")
kitikmeot_insecta <- kitikmeot_bold %>%
  filter(Class == "Insecta")

# Filter out our targets from kitikmeot_insecta
ki1 <- kitikmeot_insecta %>%
  filter(Order == "Coleoptera")
ki2 <- kitikmeot_insecta %>%
  filter(Order == "Diptera")

# Before we combine Diptera with the others though, let's remove any aquatic inverts
dip1 <-  ki2 %>%
  filter(Family == "Agromyzidae")
dip2 <-  ki2 %>%
  filter(Family == "Anthomyiidae")
dip3 <-  ki2 %>%
  filter(Family == "Bibionidae")
dip4 <-  ki2 %>%
  filter(Family == "Bolitophilidae")
dip5 <-  ki2 %>%
  filter(Family == "Brachystomatidae")
dip6 <-  ki2 %>%
  filter(Family == "Calliphoridae")
dip7 <-  ki2 %>%
  filter(Family == "Canacidae")
dip8 <-  ki2 %>%
  filter(Family == "Carnidae")
dip9 <-  ki2 %>%
  filter(Family == "Cecidomyiidae")
dip10 <-  ki2 %>%
  filter(Family == "Ceratopogonidae")
dip11 <-  ki2 %>%
  filter(Family == "Chloropidae")
dip11 <-  ki2 %>%
  filter(Family == "Dolichopodidae")
dip12 <-  ki2 %>%
  filter(Family == "Empididae")
dip13 <-  ki2 %>%
  filter(Family == "Fanniidae")
dip14 <-  ki2 %>%
  filter(Family == "Heleomyzidae")
dip15 <-  ki2 %>%
  filter(Family == "Hybotidae")
dip16 <-  ki2 %>%
  filter(Family == "Keroplatidae")
dip17 <-  ki2 %>%
  filter(Family == "Lauxaniidae")
dip18 <-  ki2 %>%
  filter(Family == "Micropezidae")
dip19 <-  ki2 %>%
  filter(Family == "Milichiidae")
dip20 <-  ki2 %>%
  filter(Family == "Muscidae")
dip21 <-  ki2 %>%
  filter(Family == "Mycetophilidae")
dip22 <-  ki2 %>%
  filter(Family == "Phoridae")
dip23 <-  ki2 %>%
  filter(Family == "Piophilidae")
dip24 <-  ki2 %>%
  filter(Family == "Psychodidae")
dip25 <-  ki2 %>%
  filter(Family == "Rhagionidae")
dip26 <-  ki2 %>%
  filter(Family == "Sarcophagidae")
dip27 <-  ki2 %>%
  filter(Family == "Scathophagidae")
dip28 <-  ki2 %>%
  filter(Family == "Scatopsidae")
dip29 <-  ki2 %>%
  filter(Family == "Sciaridae")
dip30 <-  ki2 %>%
  filter(Family == "Sepsidae")
dip31 <-  ki2 %>%
  filter(Family == "Simuliidae")
dip32 <-  ki2 %>%
  filter(Family == "Sphaeroceridae")
dip33 <-  ki2 %>%
  filter(Family == "Syrphidae")
dip34 <-  ki2 %>%
  filter(Family == "Tabanidae")
dip35 <-  ki2 %>%
  filter(Family == "Tachinidae")
dip36 <-  ki2 %>%
  filter(Family == "Therevidae")
dip37 <-  ki2 %>%
  filter(Family == "Trichoceridae")
dip38 <-  ki2 %>%
  filter(Family == "Ulidiidae")
dip39 <- bind_rows(dip1,dip2,dip3,dip4,dip5,dip6,dip7,dip8,dip9,dip10,dip11,dip12,dip13,dip14,dip15,dip16,dip17,dip18,dip19,dip20,dip21,dip22,dip23,dip24,dip25,dip26,dip27,dip28,dip29,dip30,dip31,dip32,dip33,dip34,dip35,dip36,dip37,dip38)

# Important, this is where we swap out insecta with our filtered list
ki2 <- dip39
rm(dip1,dip2,dip3,dip4,dip5,dip6,dip7,dip8,dip9,dip10,dip11,dip12,dip13,dip14,dip15,dip16,dip17,dip18,dip19,dip20,dip21,dip22,dip23,dip24,dip25,dip26,dip27,dip28,dip29,dip30,dip31,dip32,dip33,dip34,dip35,dip36,dip37,dip38,dip39)

# Next up, select specific orders within insecta
ki3  <- kitikmeot_insecta %>%
  filter(Order == "Hemiptera")
ki4  <- kitikmeot_insecta %>%
  filter(Order == "Hymenoptera")
ki5 <- kitikmeot_insecta %>%
  filter(Order == "Lepidoptera")
ki6  <- kitikmeot_insecta %>%
  filter(Order == "Neuroptera")
ki7 <- kitikmeot_insecta %>%
  filter(Order == "Orthoptera")
ki8  <- kitikmeot_insecta %>%
  filter(Order == "Siphonaptera")
ki9 <- kitikmeot_insecta %>%
  filter(Order == "Thysanoptera")
ki10 <- bind_rows(ki1,ki2,ki3,ki4,ki5,ki6,ki7,ki8,ki9)

# Rename before processing
kitikmeot_insecta <- ki10
rm(ki1,ki2,ki3,ki4,ki5,ki6,ki7,ki8,ki9,ki10)

# Join 'em all back together again as our target
kitikmeot_bold <- bind_rows(kitikmeot_arachnida,kitikmeot_collembola,kitikmeot_insecta)

# Some garbage collection
rm(kitikmeot_arachnida,kitikmeot_collembola,kitikmeot_insecta)

# Change bin.uri to bin_uri for the matching
names(kitikmeot_bold)[names(kitikmeot_bold) == "bin.uri"] <- "bin_uri"

# Before we proceed further though, let's save our modified target data.
write_tsv(x = kitikmeot_bold, "data/kitikmeot_bold_target.tsv")

# Reload in case it's needed
#kitikmeot_bold <- read_tsv("data/kitikmeot_bold_target.tsv")

# Separate out the communities
cambridgebay <- kitikmeot_bold %>%
  filter(sector == "Cambridge Bay")
kugluktuk <- kitikmeot_bold %>%
  filter(sector == "Kugluktuk")
kugaaruk <- kitikmeot_bold %>%
  filter(sector == "Kugaaruk")
gjoahaven <- kitikmeot_bold %>%
  filter(sector == "Gjoa Haven")
rm(kitikmeot_bold)

# Right, now that we have the communities separated let's extract unique bin's from each and save as a vector
cbay_bins <- unique(cambridgebay$bin_uri)
kugl_bins <- unique(kugluktuk$bin_uri)
kuga_bins <- unique(kugaaruk$bin_uri)
gjoa_bins <- unique(gjoahaven$bin_uri)
canada_bold_bins <- unique(canada_bold$bin_uri)

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
canada_bold_bins <- data.frame(
  bin_uri = canada_bold_bins
)

# Remove NA's from both first though
canada_bold <- canada_bold %>%
  drop_na(bin_uri)

# Remove NA's from GPS coords
canada_bold <- canada_bold %>%
  drop_na(lat) %>%
  drop_na(lon)

# Before we use Canada Public BOLD data, let's remove regions from the Kitikmeot from it too.
canada_bold <- canada_bold %>%
  filter(!region == "Kitikmeot")
canada_bold <- canada_bold %>%
  filter(!region == "Kitikmeot Region")

# Let's save that too. Remember, "Kitikmeot" and "Kitikmeot Region" have been removed.
write_tsv(x = canada_bold, "data/Canada_data_clean_december_gps.tsv")

# Reload in case that's needed
#canada_bold <- read_tsv("D:/R/InsectsArctic/data/Canada_data_clean_october_gps.tsv")

# Next up, let's create GPS coordinates with those matching bin's in the rest of Canada
cbay_sharedbins <- canada_bold %>%
  filter(bin_uri %in% cbay_bins[,1])
kugl_sharedbins <- canada_bold %>%
  filter(bin_uri %in% kugl_bins[,1])
kuga_sharedbins <- canada_bold %>%
  filter(bin_uri %in% kuga_bins[,1])
gjoa_sharedbins <- canada_bold %>%
  filter(bin_uri %in% gjoa_bins[,1])

# Let's combine and save those unique BINs, Remember these are BINs present in those communities that have an exact BIN match elsewhere in Canada
kitikmeot_bins <- bind_rows(cbay_sharedbins,kugl_sharedbins,kuga_sharedbins,gjoa_sharedbins)

# Save this as a file so we can use it later. Specifically for a phylogenetic tree analysis
write_tsv(x = kitikmeot_bins, "data/kitikmeot_sharedBOLDbins.tsv")
write_tsv(x = cbay_sharedbins, "data/cbay_sharedBOLDbins.tsv")
write_tsv(x = kugl_sharedbins, "data/kugl_sharedBOLDbins.tsv")
write_tsv(x = gjoa_sharedbins, "data/gjoa_sharedBOLDbins.tsv")
write_tsv(x = kuga_sharedbins, "data/kuga_sharedBOLDbins.tsv")

# Register Goggle Maps API
register_google(key = "AIzaSyAN1NbGpO6bZqJKx8EAqek_wK-Rg2C7geo")

# Setup ggmap
map_bold <- get_map(
  #location = c(left = -140, bottom = 40, right = -50, top = 83.25),
  location = c(left = -140, bottom = 40, right = -50, top = 83.25),
  scale = "auto",
  zoom = 3,
  source = "google",
  force = TRUE)

# let's have a look at it and assign it to a callable variable
ggmap(map_bold)
mp <- ggmap(map_bold, extent = "panel") +
  scale_y_continuous(limits = c(42,79), expand=c(0,0)) +
  scale_x_continuous(limits = c(-140,-50), expand=c(0,0))

# Create a data frame containing our lat's and lon's for the center of each communitiy
communities <- data.frame(
  community = c("Cambridge Bay", "Kugluktuk","Gjoa Haven","Kugaaruk"),
  lat = c(69.1181,67.8241,68.6352,68.5366),
  lon = c(-105.0615,-115.1006,-95.8474,-89.8174)
)

# Common links
common_links <- data.frame(
  community = c("Churchill"),
  lat = c(58.7463),
  lon = c(-94.1908)  
)

# Draw for Cambridge Bay BIN match to Sable Island
cbay_links <- data.frame(
  community = c("Sable Island"),
  lat = c(43.9327),
  lon = c(-59.9124)
)

# Draw for Kugluktuk link to Herschel Island
kugl_links <- data.frame(
  community = c("Herschel Island"),
  lat = c(69.5635),
  lon = c(-139.1209)
)

# Draw for Gjoa Haven
gjoa_links <- data.frame(
  community = c("Qikiqtarjuaq","Kuururjuaq National Park","Pond Inlet"),
  lat = c(67.5738,58.6638,72.7097),
  lon = c(-64.0224,-64.3780,-77.9357)
)

kuga_links <- data.frame(
  community = c("Pond Inlet"),
  lat = c(72.7097),
  lon = c(-77.9357)
)

# Let's add our communities
mp <- mp+ geom_point(data = communities, aes(x = lon, y = lat))
mp <- mp+ geom_text(data = communities, aes(label = community),vjust = -1,hjust = 0.5, size=5)
mp
community_map <- mp

# We're going to use the kitikmeotorder_mp map to perform the line drawings.
# However, we also want the colour of the lines to match the specimen order.
# To do that, We're matching community names vs the GPS coordinates
cbayorder_mp <- mp+ geom_segment(data = cbay_sharedbins,
                                 aes(x = lon, y = lat,
                                     color=order_name,
                                     xend = communities$lon[1],
                                     yend = communities$lat[1],
                                 )) +
  geom_point(data = cbay_sharedbins,
             aes(x = lon, y = lat,
                 color=order_name), size=3) +
  geom_point(data = common_links, aes(x = lon, y = lat)) +
  geom_text(data = common_links, aes(label = community),vjust = 1, hjust = 0.5, size=5) +
  geom_point(data = cbay_links, aes(x = lon, y = lat)) +
  geom_text(data = cbay_links, aes(label = community),vjust = 1, hjust = 0.5, size=5)
cbayorder_mp

# Next, let's do Kugluktuk
kuglorder_mp <- mp+
  geom_segment(data = kugl_sharedbins,
               aes(x = lon, y = lat,
                   color=order_name,
                   xend = communities$lon[2],
                   yend = communities$lat[2]
               )) +
  geom_point(data = kugl_sharedbins,
             aes(x = lon, y = lat,
                 color=order_name), size=3) +
  geom_point(data = common_links, aes(x = lon, y = lat)) +
  geom_text(data = common_links, aes(label = community),vjust = 1, hjust = 0.5, size=5) +
  geom_point(data = kugl_links, aes(x = lon, y = lat)) +
  geom_text(data = kugl_links, aes(label = community),vjust = -1, hjust = -0.005, size=5)
kuglorder_mp

# Next, let's do Gjoa Haven
gjoaorder_mp <- mp+
  geom_segment(data = gjoa_sharedbins,
               aes(x = lon, y = lat,
                   color=order_name,
                   xend = communities$lon[3],
                   yend = communities$lat[3]
               )) +
  geom_point(data = gjoa_sharedbins,
             aes(x = lon, y = lat,
                 color=order_name), size=3) +
  geom_point(data = common_links, aes(x = lon, y = lat)) +
  geom_text(data = common_links, aes(label = community),vjust = 1, hjust = 0.5, size=5) +
  geom_point(data = gjoa_links, aes(x = lon, y = lat)) +
  geom_text(data = gjoa_links, aes(label = community),vjust = -1, hjust = 0.5, size=5)
gjoaorder_mp

# Then, let's do Kugaaruk
kugaorder_mp <- mp+
  geom_segment(data = kuga_sharedbins,
               aes(x = lon, y = lat,
                   color=order_name,
                   xend = communities$lon[4],
                   yend = communities$lat[4]
               )) +
  geom_point(data = kuga_sharedbins,
             aes(x = lon, y = lat,
                 color=order_name), size=3) +
  geom_point(data = common_links, aes(x = lon, y = lat)) +
  geom_text(data = common_links, aes(label = community),vjust = 1, hjust = 0.5, size=5) +
  geom_point(data = kuga_links, aes(x = lon, y = lat)) +
  geom_text(data = kuga_links, aes(label = community),vjust = -1, hjust = 0.5, size=5)  
kugaorder_mp

# Last, let's map 'em alltogether
kitikmeotorder_mp <- mp+
  geom_segment(data = cbay_sharedbins,
               aes(x = lon, y = lat,
                   color=order_name,
                   xend = communities$lon[1],
                   yend = communities$lat[1]
               )) +
  geom_segment(data = kugl_sharedbins,
               aes(x = lon, y = lat,
                   color=order_name,
                   xend = communities$lon[2],
                   yend = communities$lat[2]
               )) +
  geom_segment(data = gjoa_sharedbins,
               aes(x = lon, y = lat,
                   color=order_name,
                   xend = communities$lon[3],
                   yend = communities$lat[3]
               )) +
  geom_segment(data = kuga_sharedbins,
               aes(x = lon, y = lat,
                   color=order_name,
                   xend = communities$lon[4],
                   yend = communities$lat[4]
               )) +
  labs(x = "Longitude", y = "Latitude", color="Orders",
       title="BOLD BINs shared with the Kitikmeot")
kitikmeotorder_mp

# We already have the matching BINs from the rest of Canada, let's cross reference the BIN's from those communities with the BIN's from my data
# Let's start with Cambridge Bay, flying list
cbay_diptera <- cbay_sharedbins %>%
  filter(order_name == "Diptera")
cbay_hemiptera <- cbay_sharedbins %>%
  filter(order_name == "Hemiptera")
cbay_hymenoptera <- cbay_sharedbins %>%
  filter(order_name == "Hymenoptera")
cbay_lepidoptera <- cbay_sharedbins %>%
  filter(order_name == "Lepidoptera")
cbay_thysanoptera <- cbay_sharedbins %>%
  filter(order_name == "Thysanoptera")
cbay_neuroptera <- cbay_sharedbins %>%
  filter(order_name == "Neuroptera")
cbay_orthoptera <- cbay_sharedbins %>%
  filter(order_name == "Orthoptera")
cbay_flying <- rbind(cbay_diptera,cbay_hemiptera,cbay_hymenoptera,cbay_lepidoptera,cbay_thysanoptera,cbay_neuroptera,cbay_orthoptera)
rm(cbay_diptera,cbay_hemiptera,cbay_hymenoptera,cbay_lepidoptera,cbay_thysanoptera,cbay_neuroptera,cbay_orthoptera)

# Next, make a non-flying list too
cbay_araneae <- cbay_sharedbins %>%
  filter(order_name == "Araneae")
cbay_coleoptera <- cbay_sharedbins %>%
  filter(order_name == "Coleoptera")
cbay_entomobryomorpha <- cbay_sharedbins %>%
  filter(order_name == "Entomobryomorpha")
cbay_poduromorpha <- cbay_sharedbins %>%
  filter(order_name == "Poduromorpha")
cbay_symphypleona <- cbay_sharedbins %>%
  filter(order_name == "Symphypleona")
cbay_sarcoptiformes <- cbay_sharedbins %>%
  filter(order_name == "Sarcoptiformes")
cbay_trombidiformes <- cbay_sharedbins %>%
  filter(order_name == "Trombidiformes")
cbay_mesostigmata <- cbay_sharedbins %>%
  filter(order_name == "Mesostigmata")
cbay_nonflying <- rbind(cbay_araneae,cbay_coleoptera,cbay_entomobryomorpha,cbay_poduromorpha,cbay_symphypleona,cbay_sarcoptiformes,cbay_trombidiformes,cbay_mesostigmata)
rm(cbay_araneae,cbay_coleoptera,cbay_entomobryomorpha,cbay_poduromorpha,cbay_symphypleona,cbay_sarcoptiformes,cbay_trombidiformes,cbay_mesostigmata)

# Let's do the same for Kugluktuk, flying first
kugl_diptera <- kugl_sharedbins %>%
  filter(order_name == "Diptera")
kugl_hemiptera <- kugl_sharedbins %>%
  filter(order_name == "Hemiptera")
kugl_hymenoptera <- kugl_sharedbins %>%
  filter(order_name == "Hymenoptera")
kugl_lepidoptera <- kugl_sharedbins %>%
  filter(order_name == "Lepidoptera")
kugl_thysanoptera <- kugl_sharedbins %>%
  filter(order_name == "Thysanoptera")
kugl_neuroptera <- kugl_sharedbins %>%
  filter(order_name == "Neuroptera")
kugl_orthoptera <- kugl_sharedbins %>%
  filter(order_name == "Orthoptera")
kugl_flying <- rbind(kugl_diptera,kugl_hemiptera,kugl_hymenoptera,kugl_lepidoptera,kugl_thysanoptera,kugl_neuroptera,kugl_orthoptera)
rm(kugl_diptera,kugl_hemiptera,kugl_hymenoptera,kugl_lepidoptera,kugl_thysanoptera,kugl_neuroptera,kugl_orthoptera)

# Next, make a non-flying list too
kugl_araneae <- kugl_sharedbins %>%
  filter(order_name == "Araneae")
kugl_coleoptera <- kugl_sharedbins %>%
  filter(order_name == "Coleoptera")
kugl_entomobryomorpha <- kugl_sharedbins %>%
  filter(order_name == "Entomobryomorpha")
kugl_poduromorpha <- kugl_sharedbins %>%
  filter(order_name == "Poduromorpha")
kugl_symphypleona <- kugl_sharedbins %>%
  filter(order_name == "Symphypleona")
kugl_sarcoptiformes <- kugl_sharedbins %>%
  filter(order_name == "Sarcoptiformes")
kugl_trombidiformes <- kugl_sharedbins %>%
  filter(order_name == "Trombidiformes")
kugl_mesostigmata <- kugl_sharedbins %>%
  filter(order_name == "Mesostigmata")
kugl_nonflying <- rbind(kugl_araneae,kugl_coleoptera,kugl_entomobryomorpha,kugl_poduromorpha,kugl_symphypleona,kugl_sarcoptiformes,kugl_trombidiformes,kugl_mesostigmata)
rm(kugl_araneae,kugl_coleoptera,kugl_entomobryomorpha,kugl_poduromorpha,kugl_symphypleona,kugl_sarcoptiformes,kugl_trombidiformes,kugl_mesostigmata)

# Next, let's do Gjoa Haven flying
gjoa_diptera <- gjoa_sharedbins %>%
  filter(order_name == "Diptera")
gjoa_hemiptera <- gjoa_sharedbins %>%
  filter(order_name == "Hemiptera")
gjoa_hymenoptera <- gjoa_sharedbins %>%
  filter(order_name == "Hymenoptera")
gjoa_lepidoptera <- gjoa_sharedbins %>%
  filter(order_name == "Lepidoptera")
gjoa_thysanoptera <- gjoa_sharedbins %>%
  filter(order_name == "Thysanoptera")
gjoa_neuroptera <- gjoa_sharedbins %>%
  filter(order_name == "Neuroptera")
gjoa_orthoptera <- gjoa_sharedbins %>%
  filter(order_name == "Orthoptera")
gjoa_flying <- rbind(gjoa_diptera,gjoa_hemiptera,gjoa_hymenoptera,gjoa_lepidoptera,gjoa_thysanoptera,gjoa_neuroptera,gjoa_orthoptera)
rm(gjoa_diptera,gjoa_hemiptera,gjoa_hymenoptera,gjoa_lepidoptera,gjoa_thysanoptera,gjoa_neuroptera,gjoa_orthoptera)

# Next, make a non-flying list too
gjoa_araneae <- gjoa_sharedbins %>%
  filter(order_name == "Araneae")
gjoa_coleoptera <- gjoa_sharedbins %>%
  filter(order_name == "Coleoptera")
gjoa_entomobryomorpha <- gjoa_sharedbins %>%
  filter(order_name == "Entomobryomorpha")
gjoa_poduromorpha <- gjoa_sharedbins %>%
  filter(order_name == "Poduromorpha")
gjoa_symphypleona <- gjoa_sharedbins %>%
  filter(order_name == "Symphypleona")
gjoa_sarcoptiformes <- gjoa_sharedbins %>%
  filter(order_name == "Sarcoptiformes")
gjoa_trombidiformes <- gjoa_sharedbins %>%
  filter(order_name == "Trombidiformes")
gjoa_mesostigmata <- gjoa_sharedbins %>%
  filter(order_name == "Mesostigmata")
gjoa_nonflying <- rbind(gjoa_araneae,gjoa_coleoptera,gjoa_entomobryomorpha,gjoa_poduromorpha,gjoa_symphypleona,gjoa_sarcoptiformes,gjoa_trombidiformes,gjoa_mesostigmata)
rm(gjoa_araneae,gjoa_coleoptera,gjoa_entomobryomorpha,gjoa_poduromorpha,gjoa_symphypleona,gjoa_sarcoptiformes,gjoa_trombidiformes,gjoa_mesostigmata)

# Lastly, let's do Kugaaruk
kuga_diptera <- kuga_sharedbins %>%
  filter(order_name == "Diptera")
kuga_hemiptera <- kuga_sharedbins %>%
  filter(order_name == "Hemiptera")
kuga_hymenoptera <- kuga_sharedbins %>%
  filter(order_name == "Hymenoptera")
kuga_lepidoptera <- kuga_sharedbins %>%
  filter(order_name == "Lepidoptera")
kuga_thysanoptera <- kuga_sharedbins %>%
  filter(order_name == "Thysanoptera")
kuga_neuroptera <- kuga_sharedbins %>%
  filter(order_name == "Neuroptera")
kuga_orthoptera <- kuga_sharedbins %>%
  filter(order_name == "Orthoptera")
kuga_flying <- rbind(kuga_diptera,kuga_hemiptera,kuga_hymenoptera,kuga_lepidoptera,kuga_thysanoptera,kuga_neuroptera,kuga_orthoptera)
rm(kuga_diptera,kuga_hemiptera,kuga_hymenoptera,kuga_lepidoptera,kuga_thysanoptera,kuga_neuroptera,kuga_orthoptera)

# Next, make a non-flying list too
kuga_araneae <- kuga_sharedbins %>%
  filter(order_name == "Araneae")
kuga_coleoptera <- kuga_sharedbins %>%
  filter(order_name == "Coleoptera")
kuga_entomobryomorpha <- kuga_sharedbins %>%
  filter(order_name == "Entomobryomorpha")
kuga_poduromorpha <- kuga_sharedbins %>%
  filter(order_name == "Poduromorpha")
kuga_symphypleona <- kuga_sharedbins %>%
  filter(order_name == "Symphypleona")
kuga_sarcoptiformes <- kuga_sharedbins %>%
  filter(order_name == "Sarcoptiformes")
kuga_trombidiformes <- kuga_sharedbins %>%
  filter(order_name == "Trombidiformes")
kuga_mesostigmata <- kuga_sharedbins %>%
  filter(order_name == "Mesostigmata")
kuga_nonflying <- rbind(kuga_araneae,kuga_coleoptera,kuga_entomobryomorpha,kuga_poduromorpha,kuga_symphypleona,kuga_sarcoptiformes,kuga_trombidiformes,kuga_mesostigmata)
rm(kuga_araneae,kuga_coleoptera,kuga_entomobryomorpha,kuga_poduromorpha,kuga_symphypleona,kuga_sarcoptiformes,kuga_trombidiformes,kuga_mesostigmata)

# Let's generate a map for flying insects
kitikmeotflying_mp <- mp+
  geom_segment(data = cbay_flying,
               aes(x = lon, y = lat,
                   color=order_name,
                   xend = communities$lon[1],
                   yend = communities$lat[1]
               )) +
  geom_point(data = cbay_flying,
             aes(x = lon, y = lat,
                 color=order_name), size=1) +
  geom_point(data = common_links, aes(x = lon, y = lat)) +
  geom_text(data = common_links, aes(label = community),vjust = 1, hjust = 0.5, size=5) +
  geom_point(data = cbay_links, aes(x = lon, y = lat)) +
  geom_text(data = cbay_links, aes(label = community),vjust = 1, hjust = 0.5, size=5) +
  geom_segment(data = kugl_flying,
               aes(x = lon, y = lat,
                   color=order_name,
                   xend = communities$lon[2],
                   yend = communities$lat[2]
               )) +
  geom_point(data = kugl_flying,
             aes(x = lon, y = lat,
                 color=order_name), size=1) +
  geom_point(data = kugl_links, aes(x = lon, y = lat)) +
  geom_text(data = kugl_links, aes(label = community),vjust = -1, hjust = -0.005, size=5) +
  geom_segment(data = gjoa_flying,
               aes(x = lon, y = lat,
                   color=order_name,
                   xend = communities$lon[3],
                   yend = communities$lat[3]
               )) +
  geom_point(data = gjoa_flying,
             aes(x = lon, y = lat,
                 color=order_name), size=1) +
  geom_segment(data = kuga_flying,
               aes(x = lon, y = lat,
                   color=order_name,
                   xend = communities$lon[4],
                   yend = communities$lat[4]
               )) +
  geom_point(data = kuga_flying,
             aes(x = lon, y = lat,
                 color=order_name), size=1) +
  labs(x = "Longitude", y = "Latitude", color="Orders",
       title="Exact BOLD BIN matches shared with the Kitikmeot")
kitikmeotflying_mp

# Let's generate a map for non-flying insects
kitikmeotnonflying_mp <- mp+
  geom_segment(data = cbay_nonflying,
               aes(x = lon, y = lat,
                   color=order_name,
                   xend = communities$lon[1],
                   yend = communities$lat[1]
               )) +
  geom_point(data = cbay_nonflying,
             aes(x = lon, y = lat,
                 color=order_name), size=1) +
  geom_point(data = common_links, aes(x = lon, y = lat)) +
  geom_text(data = common_links, aes(label = community),vjust = 1, hjust = 0.5, size=5) +
  geom_segment(data = kugl_nonflying,
               aes(x = lon, y = lat,
                   color=order_name,
                   xend = communities$lon[2],
                   yend = communities$lat[2]
               )) +
  geom_point(data = kugl_nonflying,
             aes(x = lon, y = lat,
                 color=order_name), size=1) +
  geom_segment(data = gjoa_nonflying,
               aes(x = lon, y = lat,
                   color=order_name,
                   xend = communities$lon[3],
                   yend = communities$lat[3]
               )) +
  geom_point(data = gjoa_nonflying,
             aes(x = lon, y = lat,
                 color=order_name), size=1) +
  geom_segment(data = kuga_nonflying,
               aes(x = lon, y = lat,
                   color=order_name,
                   xend = communities$lon[4],
                   yend = communities$lat[4]
               )) +
  geom_point(data = kuga_nonflying,
             aes(x = lon, y = lat,
                 color=order_name), size=1) +
  labs(x = "Longitude", y = "Latitude", color="Orders",
       title="Exact BOLD BIN matches shared with the Kitikmeot")
kitikmeotnonflying_mp

# Let's do some clean-up before we move forward.
rm(kuglorder_mp,kugl_sharedbins,kugl_nonflying,kugl_flying,kugl_bins)
rm(kugaorder_mp,kuga_sharedbins,kuga_nonflying,kuga_flying,kuga_bins)
rm(kitikmeotorder_mp,kitikmeotflying_mp,kitikmeotnonflying_mp,kitikmeot_bins)
rm(gjoaorder_mp,gjoa_sharedbins,gjoa_nonflying,gjoa_flying,gjoa_bins)
rm(cbayorder_mp,cbay_sharedbins,cbay_nonflying,cbay_flying,cbay_bins)
rm(canada_bold,canada_bold_bins)

# Let's perform a different kind of map.
