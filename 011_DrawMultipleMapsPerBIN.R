# We want to iterate over a list of BIN's and draw individual maps.

# Load libraries
library(tidyverse)
library(ggmap)
library(RColorBrewer)

# Load our data
kitikmeot_bold <- read_tsv("data/kitikmeot_bold_target.tsv")

# Load the shared BIN data from Canada
kitikmeot_bins <- read_tsv("data/kitikmeot_sharedBOLDbins.tsv")
cbay_sharedbins <- read_tsv("data/cbay_sharedBOLDbins.tsv")
kugl_sharedbins <- read_tsv("data/kugl_sharedBOLDbins.tsv")
gjoa_sharedbins <- read_tsv("data/gjoa_sharedBOLDbins.tsv")
kuga_sharedbins <- read_tsv("data/kuga_sharedBOLDbins.tsv")

# Just in case, load this since it'll be easier.
kitikmeot_flying <- read_csv("data/kitikmeot_flying.csv")
kitikmeot_nonflying <- read_csv("data/kitikmeot_nonflying.csv")

# Just noticed this. Fix this in the earlier code.
cbay_sharedbins <- cbay_sharedbins %>%
  drop_na(lat) %>%
  drop_na(lon)
kugl_sharedbins <- kugl_sharedbins %>%
  drop_na(lat) %>%
  drop_na(lon)
gjoa_sharedbins <- gjoa_sharedbins %>%
  drop_na(lat) %>%
  drop_na(lon)
kuga_sharedbins <- kuga_sharedbins %>%
  drop_na(lat) %>%
  drop_na(lon)

# Let's do a simple match and split cbay_sharedbins into *_sharedbins_f and *_sharedbins_nf by matching BINs against kitikmeot_flying and kitikmeot_nonfyling
cbay_sharedbins_f <- subset(cbay_sharedbins, bin_uri %in% kitikmeot_flying$bin_uri)
cbay_sharedbins_nf <- subset(cbay_sharedbins, bin_uri %in% kitikmeot_nonflying$bin_uri)
kugl_sharedbins_f <- subset(kugl_sharedbins, bin_uri %in% kitikmeot_flying$bin_uri)
kugl_sharedbins_nf <- subset(kugl_sharedbins, bin_uri %in% kitikmeot_nonflying$bin_uri)
gjoa_sharedbins_f <- subset(gjoa_sharedbins, bin_uri %in% kitikmeot_flying$bin_uri)
gjoa_sharedbins_nf <- subset(gjoa_sharedbins, bin_uri %in% kitikmeot_nonflying$bin_uri)
kuga_sharedbins_f <- subset(kuga_sharedbins, bin_uri %in% kitikmeot_flying$bin_uri)
kuga_sharedbins_nf <- subset(kuga_sharedbins, bin_uri %in% kitikmeot_nonflying$bin_uri)

# Load Google Maps key
#register_google(key = "YOURKEYHERE")

# Setup Google Maps
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

# Create a data frame containing our lat's and lon's for the center of each community
communities <- data.frame(
  community = c("Cambridge Bay", "Kugluktuk","Gjoa Haven","Kugaaruk"),
  lat = c(69.1181,67.8241,68.6352,68.5366),
  lon = c(-105.0615,-115.1006,-95.8474,-89.8174)
)

# Let's add our communities so we can reference them by column number
mp <- mp+ geom_point(data = communities, aes(x = lon, y = lat))
mp <- mp+ geom_text(data = communities, aes(label = community),vjust = -1,hjust = 0.5, size=2)
mp
community_map <- mp

# Great, let's see that we're all setup with a before and after
kitikmeotflying_mp <- mp+
  geom_segment(data = cbay_sharedbins_f,
               aes(x = lon, y = lat,
                   color=order_name,
                   xend = communities$lon[1],
                   yend = communities$lat[1]
               )) +
  geom_point(data = cbay_sharedbins_f,
             aes(x = lon, y = lat,
                 color=order_name), size=1) +
  geom_segment(data = kugl_sharedbins_f,
               aes(x = lon, y = lat,
                   color=order_name,
                   xend = communities$lon[2],
                   yend = communities$lat[2]
               )) +
  geom_point(data = kugl_sharedbins_f,
             aes(x = lon, y = lat,
                 color=order_name), size=1) +
  geom_segment(data = gjoa_sharedbins_f,
               aes(x = lon, y = lat,
                   color=order_name,
                   xend = communities$lon[3],
                   yend = communities$lat[3]
               )) +
  geom_point(data = gjoa_sharedbins_f,
             aes(x = lon, y = lat,
                 color=order_name), size=1) +
  geom_segment(data = kuga_sharedbins_f,
               aes(x = lon, y = lat,
                   color=order_name,
                   xend = communities$lon[4],
                   yend = communities$lat[4]
               )) +
  geom_point(data = kuga_sharedbins_f,
             aes(x = lon, y = lat,
                 color=order_name), size=1) +
  labs(x = "Longitude", y = "Latitude", color="Orders",
       title="Exact BOLD BIN matches shared with the Kitikmeot")
kitikmeotflying_mp

# Let's generate a before map for non-flying insects too
kitikmeotnonflying_mp <- mp+
  geom_segment(data = cbay_sharedbins_nf,
               aes(x = lon, y = lat,
                   color=order_name,
                   xend = communities$lon[1],
                   yend = communities$lat[1]
               )) +
  geom_point(data = cbay_sharedbins_nf,
             aes(x = lon, y = lat,
                 color=order_name), size=1) +
  geom_segment(data = kugl_sharedbins_nf,
               aes(x = lon, y = lat,
                   color=order_name,
                   xend = communities$lon[2],
                   yend = communities$lat[2]
               )) +
  geom_point(data = kugl_sharedbins_nf,
             aes(x = lon, y = lat,
                 color=order_name), size=1) +
  geom_segment(data = gjoa_sharedbins_nf,
               aes(x = lon, y = lat,
                   color=order_name,
                   xend = communities$lon[3],
                   yend = communities$lat[3]
               )) +
  geom_point(data = gjoa_sharedbins_nf,
             aes(x = lon, y = lat,
                 color=order_name), size=1) +
  geom_segment(data = kuga_sharedbins_nf,
               aes(x = lon, y = lat,
                   color=order_name,
                   xend = communities$lon[4],
                   yend = communities$lat[4]
               )) +
  geom_point(data = kuga_sharedbins_nf,
             aes(x = lon, y = lat,
                 color=order_name), size=1) +
  labs(x = "Longitude", y = "Latitude", color="Orders",
       title="Exact BOLD BIN matches shared with the Kitikmeot")
kitikmeotnonflying_mp

# Let's start with flying arthropods from Cambridge Bay
cbay_sharedbins_count <- cbay_sharedbins_f %>%
  group_by(bin_uri) %>%
  summarise(n = n()) %>%
  arrange(-n)

# Let's replace the ":" with "-" so that it can be used for the out filenames in a for loop function
cbay_sharedbins_list <- cbay_sharedbins_count %>%
  select(bin_uri) %>%
  mutate(bin_uri = str_replace(bin_uri, "BOLD:", ""))

# Let's try manually assigning colors to the order_name types.
# Manually assign two different palletes for the 8 possible order types for flying and non-flying orders
col_fly = palette(brewer.pal(n = 8, name = "Set1"))
col_nofly = palette(brewer.pal(n = 8, name = "Dark2"))

# Next, we need to manually assign order type to those palletes.
# We can do that by calling scale_color_manual.
#   scale_color_manual(values = c("setosa" = "purple",
#                                 "versicolor="orange",
#                                "virginica"="steelblue")) 

# Map each BIN and save as a PNG
for (i in cbay_sharedbins_count$bin_uri) {
  bins <- cbay_sharedbins_f %>%
    filter(bin_uri == i)
  j <- str_replace(i, "BOLD:", "")
  print(j)
  # Generate the plot
  my_plot <- mp + geom_segment(data = bins, aes(x = lon, y = lat, # Add the drawn lines
                                     xend = communities$lon[1],
                                     yend = communities$lat[1], color = order_name), size = 0.5) +
    geom_point(data = bins, aes(x = lon, y = lat, color = order_name), size=0.5) + # Then add a point at the end
    # Add our custom colors for order's
    scale_color_manual(values = c("Diptera" = col_fly[1],
                                  "Hemiptera" = col_fly[2],
                                  "Hymenoptera" = col_fly[3],
                                  "Lepidoptera" = col_fly[4],
                                  "Thysanoptera" = col_fly[7])) +
    # Title and labels
    labs(x = "Longitude", y = "Latitude", title=paste("Exact BIN matches for ",i, "to Cambridge Bay", sep = " "), color="Order")
  ggsave(filename = paste("fig/CBAY_Flying_PNG/BOLD-",j,"_Matches.png", sep = ""), plot = my_plot, width=3084, height=2160, units = "px")
}

# Next, let's do Kugluktuk
kugl_sharedbins_count <- kugl_sharedbins_f %>%
  group_by(bin_uri) %>%
  summarise(n = n()) %>%
  arrange(-n)

# Let's replace the ":" with "-" so that it can be used for the out filenames in a for loop function
kugl_sharedbins_list <- kugl_sharedbins_count %>%
  select(bin_uri) %>%
  mutate(bin_uri = str_replace(bin_uri, "BOLD:", ""))

# Map each BIN and save as a PNG
for (i in kugl_sharedbins_count$bin_uri) {
  bins <- kugl_sharedbins_f %>%
    filter(bin_uri == i)
  j <- str_replace(i, "BOLD:", "")
  print(j)
  # Generate the plot
  my_plot <- mp + geom_segment(data = bins, aes(x = lon, y = lat, # Add the drawn lines
                                                xend = communities$lon[2],
                                                yend = communities$lat[2], color = order_name), size = 0.5) +
    geom_point(data = bins, aes(x = lon, y = lat, color = order_name), size=0.5) + # Then add a point at the end
    # Add our custom colors for order's
    scale_color_manual(values = c("Diptera" = col_fly[1],
                                  "Hemiptera" = col_fly[2],
                                  "Hymenoptera" = col_fly[3],
                                  "Lepidoptera" = col_fly[4],
                                  "Neuroptera" = col_fly[5],
                                  "Orthoptera" = col_fly[6],
                                  "Thysanoptera" = col_fly[7])) +
    # Title and labels
    labs(x = "Longitude", y = "Latitude", title=paste("Exact BIN matches for ",i, "to Kugluktuk", sep = " "), color="Order")
  ggsave(filename = paste("fig/KUGL_Flying_PNG/BOLD-",j,"_Matches.png", sep = ""), plot = my_plot, width=3084, height=2160, units = "px")
}

# Next, let's do Gjoa Haven
gjoa_sharedbins_count <- gjoa_sharedbins_f %>%
  group_by(bin_uri) %>%
  summarise(n = n()) %>%
  arrange(-n)

# Let's replace the ":" with "-" so that it can be used for the out filenames in a for loop function
gjoa_sharedbins_list <- gjoa_sharedbins_count %>%
  select(bin_uri) %>%
  mutate(bin_uri = str_replace(bin_uri, "BOLD:", ""))

# Map each BIN and save as a PNG
for (i in gjoa_sharedbins_count$bin_uri) {
  bins <- gjoa_sharedbins_f %>%
    filter(bin_uri == i)
  j <- str_replace(i, "BOLD:", "")
  print(j)
  # Generate the plot
  my_plot <- mp + geom_segment(data = bins, aes(x = lon, y = lat, # Add the drawn lines
                                                xend = communities$lon[3],
                                                yend = communities$lat[3], color = order_name), size = 0.5) +
    geom_point(data = bins, aes(x = lon, y = lat, color = order_name), size=0.5) + # Then add a point at the end
    # Title and labels
    scale_color_manual(values = c("Diptera" = col_fly[1],
                                  "Hemiptera" = col_fly[2],
                                  "Hymenoptera" = col_fly[3],
                                  "Lepidoptera" = col_fly[4]))
    labs(x = "Longitude", y = "Latitude", title=paste("Exact BIN matches for ",i, "to Gjoa Haven", sep = " "), color="Order")
  ggsave(filename = paste("fig/GJOA_Flying_PNG/BOLD-",j,"_Matches.png", sep = ""), plot = my_plot, width=3084, height=2160, units = "px")
}

# Lastly, let's do Kugaaruk
kuga_sharedbins_count <- kuga_sharedbins_f %>%
  group_by(bin_uri) %>%
  summarise(n = n()) %>%
  arrange(-n)

# Let's replace the ":" with "-" so that it can be used for the out filenames in a for loop function
kuga_sharedbins_list <- kuga_sharedbins_count %>%
  select(bin_uri) %>%
  mutate(bin_uri = str_replace(bin_uri, "BOLD:", ""))

# Map each BIN and save as a PNG
for (i in kuga_sharedbins_count$bin_uri) {
  bins <- kuga_sharedbins_f %>%
    filter(bin_uri == i)
  j <- str_replace(i, "BOLD:", "")
  print(j)
  # Generate the plot
  my_plot <- mp + geom_segment(data = bins, aes(x = lon, y = lat, # Add the drawn lines
                                                xend = communities$lon[4],
                                                yend = communities$lat[4], color = order_name), size = 0.5) +
    geom_point(data = bins, aes(x = lon, y = lat, color = order_name), size=0.5) + # Then add a point at the end
    # Title and labels
    scale_color_manual(values = c("Diptera" = col_fly[1],
                                  "Hemiptera" = col_fly[2],
                                  "Hymenoptera" = col_fly[3],
                                  "Lepidoptera" = col_fly[4],
                                  "Thysanoptera" = col_fly[5],
                                  "Neuroptera" = col_fly[6],
                                  "Orthoptera" = col_fly[7]))
    labs(x = "Longitude", y = "Latitude", title=paste("Exact BIN matches for ",i, "to Kugaaruk", sep = " "), color="Order")
  ggsave(filename = paste("fig/KUGA_Flying_PNG/BOLD-",j,"_Matches.png", sep = ""), plot = my_plot, width=3084, height=2160, units = "px")
}
