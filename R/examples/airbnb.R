# From http://insideairbnb.com/get-the-data.html
# Nice Geo-Example: https://shiny.rstudio.com/gallery/superzip-example.html

library(tidyverse)
library(leaflet)
library(leaflet.extras)
library(googleway)
library(rgdal)
# apt-get install libjq-dev libjpeg-dev libgdal-dev

dat <- read_rds("data/airbnb/crete.rds")
summary(dat)
dat_proc <- dat %>% 
  mutate(price = as.numeric(sub("$", "", price, fixed = TRUE)))

hist(dat_proc$price)

leaflet() %>% 
  addTiles() %>% 
  addMarkers(lng=dat_proc$longitude, 
             lat=dat_proc$latitude, popup=dat_proc$name, 
             clusterOptions = markerClusterOptions())

# Beware that the heatmap is NOT interpolated -> each point is plotted on its own
# -> regions with many listings will automatically appear "hotter"
# See also https://stackoverflow.com/questions/44749346/r-heatmap-stat-density2d-ggmap-vs-addheatmap-shiny-leaflet
# https://gis.stackexchange.com/questions/168886/r-how-to-build-heatmap-with-the-leaflet-package
dat_proc$dummy <- 1
leaflet() %>% 
  addTiles() %>% 
  addMarkers(lng=dat_proc$longitude, 
             lat=dat_proc$latitude, popup=dat_proc$name, 
             clusterOptions = markerClusterOptions()) %>%
  addHeatmap(lng = dat_proc$longitude, lat = dat_proc$latitude, intensity = dat_proc$price,
             blur = 20, max = 0.05, radius = 15)

## Alternative maps using googleway
# Note: Slow for more than 1000 elements, again, not interpolated "properly"
key <- "AIzaSyBCGvNSks4_NvBcAwdRLw9hXM0J0RkQhQg"
dat_proc_head <- head(dat_proc, 100)
data_proc_head <- dat_proc_head[!is.na(dat_proc_head$price), ]
google_map(data = dat_proc_head, key = key) %>%
  add_markers(lat = "latitude", lon = "longitude", info_window = "name", cluster = TRUE) %>%
  add_heatmap(lat = "latitude", lon = "longitude", weight = "price", option_radius = 0.15, legend = TRUE)

