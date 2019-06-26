library(owmr)
library(rnoaa)
library(leaflet)
library(tidyverse)
#library(rwunderground) -> would be nice!

apikey <- "d7eae13fe954ea0e04b0c40a172c4a10"
owmr_settings(apikey)

# Current Temperature by City
cities <- c("Crete", "Rome", "Vienna", "Lisbon", "Palma De Mallorca")
res <- lapply(cities, function(x) {
  get_current(x, units = "metric") %>% owmr_as_tibble()
})
names(res) <- cities
sapply(res, function(x) x$temp_max)

# Get Forecast by City
fc <- lapply(cities, owmr::get_forecast)
names(fc) <- cities
# Forecast for Vienna
fc_vienna <- fc$Vienna$list
fc_vienna %>% mutate(dt = as.POSIXct(dt, origin = "1970-01-01")) %>% 
  select(dt, main.temp, main.temp_min, main.temp_max, clouds.all, wind.speed, rain.3h) %>%
  reshape2::melt(id.vars = "dt") %>%
  mutate(type = case_when(grepl("temp", variable) ~ "Temperature",
                          grepl("wind", variable) ~ "Wind",
                          grepl("clouds", variable) ~ "Clouds")) %>%
  ggplot() + 
    geom_line(aes(dt, value, color = variable, group = variable)) + 
    facet_wrap(~type, scales = "free_y", ncol = 1)

# Reverse geo-lookup
# library(googleway)
# key <- "AIzaSyBCGvNSks4_NvBcAwdRLw9hXM0J0RkQhQg"
# set_key(key = key)
# google_geocode(address = "Vienna, Austria") -> (16.37382, 48.20817)
# google_geocode(address = "Crete, Greece") -> (24.80927, 35.24012)
# google_geocode(address = "Rome, Italy") -> (12.49637, 41.90278)
# google_geocode(address = "Mallorca, Spain") -> (3.017571, 39.69526)
# google_geocode(address = "Lisbon, Portugal") -> (-9.139337, 38.72225)

## Temperature map
leaflet() %>%
  addTiles() %>%
  addProviderTiles(providers$OpenWeatherMap.Temperature,
                   options = providerTileOptions(apiKey=apikey)) %>%
  setView(24.80927, 35.24012, zoom = 9)

## Rain map
leaflet() %>%
  addTiles() %>%
  addProviderTiles(providers$OpenWeatherMap.Rain,
                   options = providerTileOptions(apiKey=apikey)) %>%
  setView(24.80927, 35.24012, zoom = 9)

## Wind map
leaflet() %>%
  addTiles() %>%
  addProviderTiles(providers$OpenWeatherMap.Wind,
                   options = providerTileOptions(apiKey=apikey)) %>%
  setView(24.80927, 35.24012, zoom = 9)

## Pressure map
leaflet() %>%
  addTiles() %>%
  addProviderTiles(providers$OpenWeatherMap.Pressure,
                   options = providerTileOptions(apiKey=apikey)) %>%
  setView(24.80927, 35.24012, zoom = 9)

## Clouds map
leaflet() %>%
  addTiles() %>%
  addProviderTiles(providers$OpenWeatherMap.Clouds,
                   options = providerTileOptions(apiKey=apikey)) %>%
  setView(24.80927, 35.24012, zoom = 9)

## Historical Weather Data
# PRCP = Precipitation (tenths of mm)
# SNOW = Snowfall (mm)
# SNWD = Snow depth (mm)
# TMAX = Maximum temperature (tenths of degrees C)
# TMIN = Minimum temperature (tenths of degrees C)

dat <- read_rds("data/weather/crete.rds")

dat %>% 
  mutate(date = as.Date(date)) %>% 
  ggplot() + 
  geom_line(aes(date, value, color = datatype, group = datatype)) +  
  facet_wrap(~datatype, scales = "free_y", ncol = 1)

# More recent -> last year
dat %>% 
  mutate(date = as.Date(date)) %>% 
  filter(date >= as.Date("2018-01-01")) %>% 
  ggplot() + 
  geom_line(aes(date, value, color = datatype, group = datatype)) + 
  facet_wrap(~datatype, scales = "free_y", ncol = 1)
  
