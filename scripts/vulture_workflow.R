source("scripts/functions.R") # pulls in all the functions (written by Orr and Ryan)
library(tidyverse)
library(sf)

load("data/month_data.Rda")

week_data <- month_data %>%
  filter(timestamp < min(month_data$timestamp) + 7 * 24 * 60 * 60)

mask <- sf::st_read("data/CutoffRegion.kml")
week_data <- vultureUtils::cleanData(week_data, mask, idCol = "trackId")

# week_data <- week_data[c("trackId", "timestamp", "location_long.1", "location_lat.1")]

week_data$date <- lubridate::date(week_data$timestamp)
week_data$time <- stringr::str_extract(week_data$timestamp, pattern = "[0-9]{2}\\:[0-9]{2}\\:[0-9]{2}")
week_data$time <- replace_na(week_data$time, "00:00:00")

n <- 50
sm <- 5 # can shift 5 days in either direction, 10 day range total
sm2 <- 10

realizations_conveyor_v <- vector(mode = "list", length = n)

for(i in 1:n){
  cat(".")
  realizations_conveyor_v[[i]] <- rotate_data_table(dataset = week_data, shiftMax = 5, idCol = "trackId", dateCol = "date", timeCol = "time")
}
random_data <- week_data

data.table::setDT(random_data)
realizations_random_v <- as.data.frame(randomizations(DT = random_data, type = "trajectory", id = "trackId", datetime = "timestamp", coords = c("location_lat.1", "location_long.1"), iterations = n)) %>%
  filter(iteration != 0) %>% group_split(iteration, .keep = TRUE)

rotated_data <- realizations_conveyor_v[[1]]
rotated_data_sf <- st_as_sf(rotated_data, coords=c("location_long.1", "location_lat.1"), crs="WGS84")
roostPolygons <- read_sf("./data/roosts50_kde95_cutOffRegion.kml")
flight_edges_rotated <- getFlightEdges(rotated_data_sf, roostPolygons, idCol = "trackId", return="edges")


