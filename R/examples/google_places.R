# See also https://cran.r-project.org/web/packages/googleway/vignettes/googleway-vignette.html
library(googleway)
library(tidyr)

dat <- read_rds("data/google_places/crete.rds")
restaurants <- dat[["Restaurants"]]

restaurants$info <- paste0("<b>Restaurant Name: </b>", restaurants$name)
google_map(data = restaurants, key = key) %>%
  add_markers(lat = "lat", lon = "lng", info_window = "info") %>%
  add_heatmap(lat = "lat", lon = "lng", weight = "user_ratings_total", option_radius = 0.15, legend = TRUE)

bars <- dat[["Bars"]]
bars$info <- paste0("<b>Bar Name: </b>", bars$name)
google_map(data = bars, key = key) %>%
  add_markers(lat = "lat", lon = "lng", info_window = "info") %>%
  add_heatmap(lat = "lat", lon = "lng", weight = "user_ratings_total", option_radius = 0.15, legend = TRUE)


# Get spots based on geolocation
# res <- google_places(location = c(-37.918, 144.968),
#                      keyword = "Cinema",
#                      radius = 5000,
#                      key = key)
