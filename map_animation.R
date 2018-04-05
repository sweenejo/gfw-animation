# This file uses data from Global Fishing Watch to animate the fishing activity off
# the west coast of the US in 2016.

library(ggmap)
library(tidyverse)
library(gganimate)
library(magick)

# Load data
files <- list.files(path = "data/", pattern="*.csv")
files <- paste0("data/", files) %>%
  .[grepl("^data.*2016", .)] # keep only 2016
dat <- lapply(files, read_csv) %>% 
  bind_rows()

dat <- dat %>%
  mutate(lat_bin = lat_bin / 10) %>%
  mutate(lon_bin = lon_bin / 10) %>%
  filter(lon_bin >= -160 & lon_bin <= -90) %>%
  filter(lat_bin >= 10 & lat_bin <= 70) %>% # keep only -160 to -90 lon and 10 to 70 lat
  group_by(lat_bin, lon_bin) %>% # add date to animate
  summarize(fishing_hours = sum(fishing_hours))

# Create map
map <- get_googlemap(center = c(lon = -124.4, lat = 39.5), 
                     zoom = 4,
                     maptype = "satellite",
                     color = "bw")
ggmap(map) +
  geom_tile(data = dat, aes(x = lon_bin, y = lat_bin,
                              fill = fishing_hours)) +
  scale_fill_distiller(palette = "Spectral")

ggsave("figures/agg_effort_2016.jpg")

# Now for the animation
dat2 <- lapply(files, read_csv) %>% 
  bind_rows()

dat2 <- dat2 %>%
  mutate(lat_bin = lat_bin / 10) %>%
  mutate(lon_bin = lon_bin / 10) %>%
  filter(lon_bin >= -160 & lon_bin <= -90) %>%
  filter(lat_bin >= 10 & lat_bin <= 70) %>% # keep only -160 to -90 lon and 10 to 70 lat
  group_by(lat_bin, lon_bin, date) %>% # add date to animate
  summarize(fishing_hours = sum(fishing_hours)) %>%
  arrange(date)

p <- ggmap(map) +
  geom_tile(data = dat2, aes(x = lon_bin, y = lat_bin,
                             fill = fishing_hours, frame = date)) + # Ignore the error message
  scale_fill_distiller(palette = "Spectral")

gganimate(p, interval = 0.05, filename = "figures/agg_effort_2016_ani.gif")
