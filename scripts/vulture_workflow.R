source("scripts/functions.R") # pulls in all the functions (written by Orr and Ryan)
library(tidyverse)
library(sf)
library(vultureUtils)

load("data/month_data.Rda")
roostPolygons <- read_sf("./data/roosts50_kde95_cutOffRegion.kml")

week_data <- month_data %>%
  filter(timestamp < min(month_data$timestamp) + 7 * 24 * 60 * 60)

mask <- sf::st_read("data/CutoffRegion.kml")
week_data <- vultureUtils::cleanData(week_data, mask, idCol = "trackId")

# Make date and time columns that can be used for rotations
week_data$date <- lubridate::date(week_data$timestamp)
week_data$time <- stringr::str_extract(week_data$timestamp, pattern = "[0-9]{2}\\:[0-9]{2}\\:[0-9]{2}")
week_data$time <- replace_na(week_data$time, "00:00:00")

n <- 50 # number of permutations
sm <- 5 # can shift 5 days in either direction, 10 day range total

# Run the conveyor permutations
realizations_conveyor_v <- vector(mode = "list", length = n)
for(i in 1:n){
  cat(".")
  realizations_conveyor_v[[i]] <- rotate_data_table(dataset = week_data, shiftMax = 5, idCol = "trackId", dateCol = "date", timeCol = "time")
} 

# realizations_conveyor_v <- realizations_conveyor_v %>%
#   map(., ~.x %>%
#         rename(oldtimestamp = timestamp,
#                timestamp = newdatetime))
save(realizations_conveyor_v, file = "data/vulture_permutations/realizations_conveyor_v.Rda")
load("data/vulture_permutations/realizations_conveyor_v.Rda")

# Run the random permutations
random_data <- week_data
data.table::setDT(random_data)
toJoin <- random_data %>%
  select(ground_speed, location_lat, location_long, date, time)
realizations_random_v <- as.data.frame(randomizations(DT = random_data, type = "trajectory", id = "trackId", datetime = "timestamp", coords = c("location_lat.1", "location_long.1"), iterations = n)) %>%
  rename(oldtimestamp = timestamp,
         timestamp = randomtimestamp) %>%
  filter(iteration != 0) %>% group_split(iteration, .keep = TRUE) %>%
  map(., ~bind_cols(.x, toJoin))
realizations_random_v <- map(realizations_random_v, ~.x %>% mutate(dateOnly = lubridate::date(timestamp)))
save(realizations_random_v, file = "data/vulture_permutations/realizations_random_v.Rda")
load("data/vulture_permutations/realizations_random_v.Rda")


# Transform both to sf
conveyor_v_sf <- map(realizations_conveyor_v, ~.x %>% st_as_sf(., coords = c("location_long.1", "location_lat.1"), crs = "WGS84"))
random_v_sf <- map(realizations_random_v, ~.x %>% st_as_sf(., coords = c("location_long", "location_lat"), crs = "WGS84", remove = F))

# fe_conveyor <- map(conveyor_v_sf, ~{
#   getFlightEdges(dataset = .x, roostPolygons = roostPolygons, roostBuffer = 50, consecThreshold = 2, distThreshold = 1000, speedThreshUpper = NULL, speedThreshLower = 5, timeThreshold = "10 minutes", idCol = "trackId", quiet = T, includeAllVertices = F, daytimeOnly = T, return = "sri", getLocs = F)
# })
# save(fe_conveyor, file = "data/vulture_permutations/fe_conveyor.Rda")
load("data/vulture_permutations/fe_conveyor.Rda")

# fe_random <- map(random_v_sf, ~{
#   getFlightEdges(dataset = .x, roostPolygons = roostPolygons, roostBuffer = 50, consecThreshold = 2, distThreshold = 1000, speedThreshUpper = NULL, speedThreshLower = 5, timeThreshold = "10 minutes", idCol = "trackId", quiet = T, includeAllVertices = F, daytimeOnly = T, return = "sri", getLocs = F)
# })
# save(fe_random, file = "data/vulture_permutations/fe_random.Rda")
load("data/vulture_permutations/fe_random.Rda")

n <- length(fe_conveyor)
fe_conveyor_stats <- map2(.x = fe_conveyor, .y = 1:n ~{filter(.x, sri > 0) %>% mutate(iteration = .y)}) %>% discard(~nrow(.x) == 0)



# calculate the stats
# save the stats
# make plots

