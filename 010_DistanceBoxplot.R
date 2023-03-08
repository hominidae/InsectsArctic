# Generate distances to nearest links
library(tidyverse)
library(geosphere)
library(measurements)
library(ggplot2)
library(dplyr)
library(gridExtra)
library(grid)

# Load our shared links with communities in the Kitikmeot
cbay_sharedbins <- read_tsv("C:/R/InsectsArctic/data/cbay_sharedBOLDbins.tsv")
kugl_sharedbins <- read_tsv("C:/R/InsectsArctic/data/kugl_sharedBOLDbins.tsv")
gjoa_sharedbins <- read_tsv("C:/R/InsectsArctic/data/gjoa_sharedBOLDbins.tsv")
kuga_sharedbins <- read_tsv("C:/R/InsectsArctic/data/kuga_sharedBOLDbins.tsv")

# Load our flying and non-flying BIN matches too
kitikmeot_flying <- read_csv("C:/R/InsectsArctic/data/kitikmeot_flying.csv")
kitikmeot_nonflying <- read_csv("C:/R/InsectsArctic/data/kitikmeot_nonflying.csv")

# Bummer, we want matches for the sharedbins to flying and non-flying too.
# Let's do a simple match and split cbay_sharedbins into *_sharedbins_f and *_sharedbins_nf by matching BINs against kitikmeot_flying and kitikmeot_nonfyling
cbay_sharedbins_f <- subset(cbay_sharedbins, bin_uri %in% kitikmeot_flying$bin_uri)
cbay_sharedbins_nf <- subset(cbay_sharedbins, bin_uri %in% kitikmeot_nonflying$bin_uri)
kugl_sharedbins_f <- subset(kugl_sharedbins, bin_uri %in% kitikmeot_flying$bin_uri)
kugl_sharedbins_nf <- subset(kugl_sharedbins, bin_uri %in% kitikmeot_nonflying$bin_uri)
gjoa_sharedbins_f <- subset(gjoa_sharedbins, bin_uri %in% kitikmeot_flying$bin_uri)
gjoa_sharedbins_nf <- subset(gjoa_sharedbins, bin_uri %in% kitikmeot_nonflying$bin_uri)
kuga_sharedbins_f <- subset(kuga_sharedbins, bin_uri %in% kitikmeot_flying$bin_uri)
kuga_sharedbins_nf <- subset(kuga_sharedbins, bin_uri %in% kitikmeot_nonflying$bin_uri)

# Setup our end points
communities <- data.frame(
  community = c("Cambridge Bay", "Kugluktuk","Gjoa Haven","Kugaaruk"),
  lat = c(69.1181,67.8241,68.6352,68.5366),
  lon = c(-105.0615,-115.1006,-95.8474,-89.8174)
)

# Great, now we're ready to start generating a distance matrix
# *_sharedbins contain the starting endpoints
# communities contains the GPS coordinates for the ending endpoints
# We want to start with a simple dataframe of each with just the gps lon and lat
cbay_links_f <- cbay_sharedbins_f %>%
  select(bin_uri,lon,lat)
kugl_links_f <- kugl_sharedbins_f %>%
  select(bin_uri,lon,lat)
gjoa_links_f <- gjoa_sharedbins_f %>%
  select(bin_uri,lon,lat)
kuga_links_f <- kuga_sharedbins_f %>%
  select(bin_uri,lon,lat)
cbay_links_nf <- cbay_sharedbins_nf %>%
  select(bin_uri,lon,lat)
kugl_links_nf <- kugl_sharedbins_nf %>%
  select(bin_uri,lon,lat)
gjoa_links_nf <- gjoa_sharedbins_nf %>%
  select(bin_uri,lon,lat)
kuga_links_nf <- kuga_sharedbins_nf %>%
  select(bin_uri,lon,lat)

# Next, create a distance matrix with the distance to the GPS coordinates for the community endpoint
cbay_distmatrix_f <- distm(c(-105.0615, 69.1181), as.matrix(cbay_links_f[, -1]), fun=distGeo)
cbay_distm_f <- as.vector(cbay_distmatrix_f)
cbay_distkm_f <- conv_unit(cbay_distm_f, "m", "km")

# Re-join the distances to new dataframe
cbay_linkdist_f <- cbay_sharedbins_f %>%
  select(bin_uri,lon,lat) %>%
  mutate(distance = cbay_distkm_f)
rm(cbay_distmatrix_f,cbay_links_f,cbay_distm_f,cbay_distkm_f)

# Now do non-flying arthropods
cbay_distmatrix_nf <- distm(c(-105.0615, 69.1181), as.matrix(cbay_links_nf[, -1]), fun=distGeo)
cbay_distm_nf <- as.vector(cbay_distmatrix_nf)
cbay_distkm_nf <- conv_unit(cbay_distm_nf, "m", "km")

# Re-join the distances to new dataframe
cbay_linkdist_nf <- cbay_sharedbins_nf %>%
  select(bin_uri,lon,lat) %>%
  mutate(distance = cbay_distkm_nf)
rm(cbay_distmatrix_nf,cbay_links_nf,cbay_distm_nf,cbay_distkm_nf)

# Do the same for Kugluktuk
kugl_distmatrix_f <- distm(c(-105.0615, 69.1181), as.matrix(kugl_links_f[, -1]), fun=distGeo)
kugl_distm_f <- as.vector(kugl_distmatrix_f)
kugl_distkm_f <- conv_unit(kugl_distm_f, "m", "km")

# Re-join the distances to new dataframe
kugl_linkdist_f <- kugl_sharedbins_f %>%
  select(bin_uri,lon,lat) %>%
  mutate(distance = kugl_distkm_f)
rm(kugl_distmatrix_f,kugl_links_f,kugl_distm_f,kugl_distkm_f)

# Now do non-flying arthropods
kugl_distmatrix_nf <- distm(c(-105.0615, 69.1181), as.matrix(kugl_links_nf[, -1]), fun=distGeo)
kugl_distm_nf <- as.vector(kugl_distmatrix_nf)
kugl_distkm_nf <- conv_unit(kugl_distm_nf, "m", "km")

# Re-join the distances to new dataframe
kugl_linkdist_nf <- kugl_sharedbins_nf %>%
  select(bin_uri,lon,lat) %>%
  mutate(distance = kugl_distkm_nf)
rm(kugl_distmatrix_nf,kugl_links_nf,kugl_distm_nf,kugl_distkm_nf)

# Next, do the same for Gjoa Haven
gjoa_distmatrix_f <- distm(c(-105.0615, 69.1181), as.matrix(gjoa_links_f[, -1]), fun=distGeo)
gjoa_distm_f <- as.vector(gjoa_distmatrix_f)
gjoa_distkm_f <- conv_unit(gjoa_distm_f, "m", "km")

# Re-join the distances to new dataframe
gjoa_linkdist_f <- gjoa_sharedbins_f %>%
  select(bin_uri,lon,lat) %>%
  mutate(distance = gjoa_distkm_f)
rm(gjoa_distmatrix_f,gjoa_links_f,gjoa_distm_f,gjoa_distkm_f)

# Now do non-flying arthropods
gjoa_distmatrix_nf <- distm(c(-105.0615, 69.1181), as.matrix(gjoa_links_nf[, -1]), fun=distGeo)
gjoa_distm_nf <- as.vector(gjoa_distmatrix_nf)
gjoa_distkm_nf <- conv_unit(gjoa_distm_nf, "m", "km")

# Re-join the distances to new dataframe
gjoa_linkdist_nf <- gjoa_sharedbins_nf %>%
  select(bin_uri,lon,lat) %>%
  mutate(distance = gjoa_distkm_nf)
rm(gjoa_distmatrix_nf,gjoa_links_nf,gjoa_distm_nf,gjoa_distkm_nf)

# Last, do it for Kugaaruk
kuga_distmatrix_f <- distm(c(-105.0615, 69.1181), as.matrix(kuga_links_f[, -1]), fun=distGeo)
kuga_distm_f <- as.vector(kuga_distmatrix_f)
kuga_distkm_f <- conv_unit(kuga_distm_f, "m", "km")

# Re-join the distances to new dataframe
kuga_linkdist_f <- kuga_sharedbins_f %>%
  select(bin_uri,lon,lat) %>%
  mutate(distance = kuga_distkm_f)
rm(kuga_distmatrix_f,kuga_links_f,kuga_distm_f,kuga_distkm_f)

# Now do non-flying arthropods
kuga_distmatrix_nf <- distm(c(-105.0615, 69.1181), as.matrix(kuga_links_nf[, -1]), fun=distGeo)
kuga_distm_nf <- as.vector(kuga_distmatrix_nf)
kuga_distkm_nf <- conv_unit(kuga_distm_nf, "m", "km")

# Re-join the distances to new dataframe
kuga_linkdist_nf <- kuga_sharedbins_nf %>%
  select(bin_uri,lon,lat) %>%
  mutate(distance = kuga_distkm_nf)
rm(kuga_distmatrix_nf,kuga_links_nf,kuga_distm_nf,kuga_distkm_nf)

# Before we move on though, we should make a new matrix. We want community by distances
# We could do this in two ways, but we'll try the easiest way first. Separately.
ggplot(cbay_linkdist_f) +
  geom_boxplot(aes(distance)) +
  xlab("Distance")
ggplot(cbay_linkdist_nf) +
  geom_boxplot(aes(distance)) +
  xlab("Distance")

# Rather bland. We don't want a one dimensional box plot.

# To do that, let's combine the flying and non-flying distances. We also want the frequency count to add as a jitter or violin field
cbay_linkdist_f <- cbay_linkdist_f %>%
  add_column(type = "Flying")
cbay_linkdist_nf <- cbay_linkdist_nf %>%
  add_column(type = "Non-flying")
kugl_linkdist_f <- kugl_linkdist_f %>%
  add_column(type = "Flying")
kugl_linkdist_nf <- kugl_linkdist_nf %>%
  add_column(type = "Non-flying")
gjoa_linkdist_f <- gjoa_linkdist_f %>%
  add_column(type = "Flying")
gjoa_linkdist_nf <- gjoa_linkdist_nf %>%
  add_column(type = "Non-flying")
kuga_linkdist_f <- kuga_linkdist_f %>%
  add_column(type = "Flying")
kuga_linkdist_nf <- kuga_linkdist_nf %>%
  add_column(type = "Non-flying")

# This is rather a pain in the ass to do things this way, but what the hell.
# Let's select just the type and distance column for *_linkdist_f and *_linkdist_nf so we can rowbind 'em together
cbay_distances_f <- cbay_linkdist_f %>%
  select(bin_uri,type,distance)
cbay_distances_nf <- cbay_linkdist_nf %>%
  select(bin_uri,type,distance)
cbay_distances <- rbind(cbay_distances_f,cbay_distances_nf)
# Let's do Kugluktuk
kugl_distances_f <- kugl_linkdist_f %>%
  select(bin_uri,type,distance)
kugl_distances_nf <- kugl_linkdist_nf %>%
  select(bin_uri,type,distance)
kugl_distances <- rbind(kugl_distances_f,kugl_distances_nf)
# And Gjoa Haven
gjoa_distances_f <- gjoa_linkdist_f %>%
  select(bin_uri,type,distance)
gjoa_distances_nf <- gjoa_linkdist_nf %>%
  select(bin_uri,type,distance)
gjoa_distances <- rbind(gjoa_distances_f,gjoa_distances_nf)
# Last, Kugaaruk
kuga_distances_f <- kuga_linkdist_f %>%
  select(bin_uri,type,distance)
kuga_distances_nf <- kuga_linkdist_nf %>%
  select(bin_uri,type,distance)
kuga_distances <- rbind(kuga_distances_f,kuga_distances_nf)

# Setup colors
col = c("#1b9e77", "#d95f02", "#7570b3") 
# Setup a theme
theme_nice <- theme(legend.position = 'none',
                       panel.grid.minor = element_blank(),
                       panel.background = element_blank(), 
                       panel.border = element_rect(colour = "black", fill=NA, size = 1))

# Createa distance boxplot Cambridge Bay
cbay_boxplot <- ggplot(cbay_distances, aes(type, 
                     distance, 
                     color = type)) + 
  geom_boxplot(outlier.color = NA) + ## outlier.color = NA removes the outlier points that are automatically assigned with geom_boxplot
  geom_jitter(alpha = 0.5) + ## The use of geom_jitter is what adds the points
  geom_violin(alpha = 0.5) + ## The use of geom_violin adds the violin plot, which includes lines to demonstrate sample size
  ylab("Distance in km") +
  xlab("Type") +
  scale_color_manual(values = col) +
  theme_nice
# Generate one for Kugluktuk
kugl_boxplot <- ggplot(kugl_distances, aes(type, 
                                           distance, 
                                           color = type)) + 
  geom_boxplot(outlier.color = NA) + ## outlier.color = NA removes the outlier points that are automatically assigned with geom_boxplot
  geom_jitter(alpha = 0.5) + ## The use of geom_jitter is what adds the points
  geom_violin(alpha = 0.5) + ## The use of geom_violin adds the violin plot, which includes lines to demonstrate sample size
  ylab("Distance in km") +
  xlab("Type") +
  scale_color_manual(values = col) +
  theme_nice
# Do Gjoa Haven
gjoa_boxplot <- ggplot(gjoa_distances, aes(type, 
                                           distance, 
                                           color = type)) + 
  geom_boxplot(outlier.color = NA) + ## outlier.color = NA removes the outlier points that are automatically assigned with geom_boxplot
  geom_jitter(alpha = 0.5) + ## The use of geom_jitter is what adds the points
  geom_violin(alpha = 0.5) + ## The use of geom_violin adds the violin plot, which includes lines to demonstrate sample size
  ylab("Distance in km") +
  xlab("Type") +
  scale_color_manual(values = col) +
  theme_nice
# Last, do Kugaaruk
kuga_boxplot <- ggplot(kuga_distances, aes(type, 
                                           distance, 
                                           color = type)) + 
  geom_boxplot(outlier.color = NA) + ## outlier.color = NA removes the outlier points that are automatically assigned with geom_boxplot
  geom_jitter(alpha = 0.5) + ## The use of geom_jitter is what adds the points
  geom_violin(alpha = 0.5) + ## The use of geom_violin adds the violin plot, which includes lines to demonstrate sample size
  ylab("Distance in km") +
  xlab("Type") +
  scale_color_manual(values = col) +
  theme_nice

# Let's grid arrange those
grid.arrange(arrangeGrob(cbay_boxplot,top=textGrob("Cambridge Bay"),ncol=1),
             arrangeGrob(kugl_boxplot,top=textGrob("Kugluktuk"),ncol=1),
             arrangeGrob(gjoa_boxplot,top=textGrob("Gjoa Haven"),ncol=1),
             arrangeGrob(kuga_boxplot,top=textGrob("Kugaaruk"),ncol=1),
             ncol=2,nrow=2)
