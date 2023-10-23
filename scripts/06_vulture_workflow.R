source("scripts/functions.R") # pulls in all the functions (written by Orr and Ryan)
library(tidyverse)
library(sf)
library(vultureUtils)

load("data/seasons.Rda") # load the cleaned seasons data taken from the MvmtSoc project
seasonNames <- map_chr(seasons, ~as.character(.x$seasonUnique[1]))
season_data <- seasons[[which(seasonNames == "2022_summer")]]
roostPolygons <- read_sf("./data/roosts50_kde95_cutOffRegion.kml")

# Make date and time columns that can be used for rotations
season_data$date <- lubridate::date(season_data$timestamp)
season_data$time <- stringr::str_extract(season_data$timestamp, pattern = "[0-9]{2}\\:[0-9]{2}\\:[0-9]{2}")
season_data$time <- replace_na(season_data$time, "00:00:00")
season_data <- st_drop_geometry(season_data)

n <- 50 # number of permutations
sm <- 5 # can shift 5 days in either direction, 10 day range total

# Run the conveyor permutations
realizations_conveyor_v <- vector(mode = "list", length = n)
for(i in 1:n){
  cat(".")
  realizations_conveyor_v[[i]] <- rotate_data_table(dataset = season_data, shiftMax = 5, idCol = "trackId", dateCol = "date", timeCol = "time")
} 

realizations_conveyor_v <- realizations_conveyor_v %>%
  map(., ~.x %>%
        rename(oldtimestamp = timestamp,
               timestamp = newdatetime))
save(realizations_conveyor_v, file = "data/vulture_permutations/realizations_conveyor_v.Rda")
load("data/vulture_permutations/realizations_conveyor_v.Rda")

# Run the random permutations
random_data <- season_data
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

fe_conveyor <- map(conveyor_v_sf, ~{
  getFlightEdges(dataset = .x, roostPolygons = roostPolygons, roostBuffer = 50, consecThreshold = 2, distThreshold = 1000, speedThreshUpper = NULL, speedThreshLower = 5, timeThreshold = "10 minutes", idCol = "trackId", quiet = T, includeAllVertices = F, daytimeOnly = T, return = "sri", getLocs = F)
})
save(fe_conveyor, file = "data/vulture_permutations/fe_conveyor.Rda")
load("data/vulture_permutations/fe_conveyor.Rda")

fe_random <- map(random_v_sf, ~{
  getFlightEdges(dataset = .x, roostPolygons = roostPolygons, roostBuffer = 50, consecThreshold = 2, distThreshold = 1000, speedThreshUpper = NULL, speedThreshLower = 5, timeThreshold = "10 minutes", idCol = "trackId", quiet = T, includeAllVertices = F, daytimeOnly = T, return = "sri", getLocs = F)
})
save(fe_random, file = "data/vulture_permutations/fe_random.Rda")
load("data/vulture_permutations/fe_random.Rda")

# Calculate the stats
# we defined n above--number of permutations
fe_conveyor_df <- map2(.x = fe_conveyor, .y = 1:n, ~{filter(.x, sri > 0) %>% mutate(iteration = .y)}) %>% discard(~nrow(.x) == 0) %>% purrr::list_rbind()
flipped_c <- fe_conveyor_df
names(flipped_c)[1:2] <- c("ID2", "ID1") # flip the order to make sure we're able to group just by ID1
bound_c <- bind_rows(fe_conveyor_df, flipped_c)

stats_conveyor <- bound_c %>%
  group_by(iteration, ID1) %>%
  summarize(deg = length(unique(ID2)),
            n = n(),
            str = sum(sri))

fe_random_df <- map2(.x = fe_random, .y = 1:n, ~{filter(.x, sri > 0) %>% mutate(iteration = .y)}) %>% discard(~nrow(.x) == 0) %>% purrr::list_rbind()
flipped_r <- fe_random_df
names(flipped_r)[1:2] <- c("ID2", "ID1") # flip the order to make sure we're able to group just by ID1
bound_r <- bind_rows(fe_random_df, flipped_r)

stats_random <- bound_r %>%
  group_by(iteration, ID1) %>%
  summarize(deg = length(unique(ID2)),
            n = n(),
            str = sum(sri))

vulture_stats_perms <- stats_conveyor %>%
  mutate(type = "conveyor") %>%
  bind_rows(stats_random %>%
              mutate(type = "random"))

save(vulture_stats_perms, file = "data/vulture_permutations/vulture_stats_perms.Rda")
load("data/vulture_permutations/vulture_stats_perms.Rda")

# Get observed stats
season_data <- season_data %>%
  sf::st_as_sf(coords = c("location_long", "location_lat"), remove = F, crs = "WGS84")
fe <- getFlightEdges(dataset = season_data, roostPolygons = roostPolygons, roostBuffer = 50,
                    consecThreshold = 2, distThreshold = 1000, speedThreshUpper = NULL, speedThreshLower = 5, timeThreshold = "10 minutes", idCol = "trackId", quiet = T, includeAllVertices = F, daytimeOnly = T, return = "sri", getLocs = F)

fe_obs_df <- fe %>%
  filter(sri > 0)
flipped_o <- fe_obs_df
names(flipped_o)[1:2] <- c("ID2", "ID1") # flip the order to make sure we're able to group just by ID1
bound_o <- bind_rows(fe_obs_df, flipped_o)

vulture_stats_obs <- bound_o %>%
  group_by(ID1) %>% 
  summarize(deg = length(unique(ID2)),
            n = n(),
            str = sum(sri))
save(vulture_stats_obs, file = "data/vulture_permutations/vulture_stats_obs.Rda")
load("data/vulture_permutations/vulture_stats_obs.Rda")
