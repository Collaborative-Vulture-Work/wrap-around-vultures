source("scripts/00.1_functions.R") # pulls in all the functions (written by Orr and Ryan)
library(tidyverse)
library(sf)
library(vultureUtils)

# load("data/seasons.Rda") # load the cleaned seasons data taken from the MvmtSoc project
# seasonNames <- map_chr(seasons, ~as.character(.x$seasonUnique[1]))
# season_data <- seasons[[which(seasonNames == "2022_summer")]]
# save(season_data, file = "data/vulture_permutations/season_data.Rda")
load("data/vulture_permutations/season_data.Rda")
roostPolygons <- read_sf("./data/roosts50_kde95_cutOffRegion.kml")

# Make date and time columns that can be used for rotations
season_data$date <- lubridate::date(season_data$timestamp)
season_data$time <- stringr::str_extract(season_data$timestamp, pattern = "[0-9]{2}\\:[0-9]{2}\\:[0-9]{2}")
season_data$time <- replace_na(season_data$time, "00:00:00")
season_data <- st_drop_geometry(season_data)

# Calculate movement metrics ----------------------------------------------
# How many days were individuals tracked?
season_data %>% group_by(Nili_id) %>% sf::st_drop_geometry() %>% summarize(n = length(unique(dateOnly))) %>% arrange(desc(n)) %>% summarize(mn = mean(n), min = min(n), max = max(n), sd = sd(n))

# Daily distance traveled and max displacement
head(season_data)
# flightmetrics_summer2022 <- calc_metrics(season_data)
# save(flightmetrics_summer2022, file = "data/vulture_permutations/flightmetrics_summer2022.Rda")
load("data/vulture_permutations/flightmetrics_summer2022.Rda")

# Validate
fm <- flightmetrics_summer2022
fm <- fm %>%
  mutate(across(c("dmd", "dd", "ddt"), ~.x/1000))

# Look at the histogram of daily distance traveled for each individual
fm %>%
  ggplot(aes(x = ddt, group = Nili_id))+
  geom_density(alpha = 0.5)+
  theme_classic()+
  theme(legend.position = "none")+
  xlab("daily distance traveled (flight)")

fm %>% group_by(Nili_id) %>% summarize(mn = mean(ddt)) %>% ungroup() %>% arrange(mn)

flightmetrics_summer2022 %>%
  ungroup() %>%
  summarize(mn_ddt = mean(ddt),
            sd_ddt = sd(ddt),
            min_ddt = min(ddt),
            max_ddt = max(ddt),
            mn_dd = mean(dd),
            sd_dd = sd(dd),
            min_dd = min(dd),
            max_dd = max(dd)) %>%
  pivot_longer(cols = everything(), names_to = "measure", values_to = "value") %>%
  mutate(value = value/1000)

# Make a plot of the vulture trajectories
vultures <- unique(season_data$Nili_id)
set.seed(3)
random_vultures <- sample(vultures, 5)
mindate <- min(season_data$dateOnly)
maxdate <- mindate + 50

forplot <- season_data %>%
  sf::st_as_sf(coords = c("location_long", "location_lat"), crs = "WGS84", remove = F) %>%
  filter(dateOnly >= mindate, dateOnly <= maxdate) %>%
  dplyr::mutate(toshow = dplyr::case_when(Nili_id %in% random_vultures ~ T,
                            TRUE ~F)) %>%
  #sf::st_transform(32636) %>%
  mutate(x = sf::st_coordinates(.)[,1],
         y = sf::st_coordinates(.)[,2]) %>%
  sf::st_drop_geometry() %>%
  dplyr::select(Nili_id, toshow, timestamp, x, y) %>%
  dplyr::arrange(Nili_id, timestamp)

vultures <- ggplot(data = forplot %>% filter(toshow), aes(x, y, col = Nili_id))+
  geom_path(data = forplot %>% filter(!toshow), alpha = 0.3, linewidth = 0.3, col = "black")+
  geom_path(linewidth = 1.5, alpha = 0.8)+
  theme_minimal()+
  theme(legend.position = "none",
        axis.title = element_blank())+
  coord_equal()
vultures
ggsave(vultures, filename = "fig/vulture_permutations_plots/vultures.png", width = 7, height = 7)

# Look at the vultures' home ranges and space use ---------------------------------------
szn <- season_data %>%
  sf::st_as_sf(coords = c("location_long", "location_lat"), crs = "WGS84") %>%
  sf::st_transform(32636)

szn <- cbind(szn, st_coordinates(szn))

# get the daily centroid for each individual vulture
centroids <- szn %>%
  group_by(trackId, date) %>%
  summarize(geometry = st_union(geometry)) %>%
  st_centroid()

centroids <- cbind(centroids, st_coordinates(centroids))

centroids %>% 
  arrange(trackId, date) %>%
  filter(trackId %in% unique(centroids$trackId)[1:6], date %in% unique(centroids$date)[1:20]) %>% 
  ggplot(aes(x = X, y = Y, col = date, group = trackId))+
  geom_point(size = 2)+
  geom_path()+
  theme_minimal()+
  geom_point(data = szn %>% filter(trackId %in% unique(centroids$trackId)[1:6], date %in% unique(centroids$date)[1:20]), aes(x = X, y = Y, col = date, group = trackId), size = 0.5)+
  geom_path(data = szn %>%
              filter(trackId %in% unique(centroids$trackId)[1:6], date %in% unique(centroids$date)[1:20]),
            aes(x = X, y = Y, col = date, group = trackId),
            linewidth = 0.7, alpha = 0.7)+
  facet_wrap(~trackId)+
  scale_color_gradientn(colors = c("darkred", "red", "darkorange", "yellow", "green", "blue", "darkblue"))


centroids %>% 
  arrange(trackId, date) %>%
  filter(trackId %in% unique(centroids$trackId)[1:6], date %in% unique(centroids$date)[1:20]) %>% 
  ggplot(aes(x = X, y = Y, col = trackId, group = trackId))+
  geom_point(size = 2)+
  geom_path()+
  theme_minimal()+
  geom_point(data = szn %>% filter(trackId %in% unique(centroids$trackId)[1:6], date %in% unique(centroids$date)[1:20]), aes(x = X, y = Y, col = trackId, group = trackId), size = 0.5)+
  geom_path(data = szn %>%
              filter(trackId %in% unique(centroids$trackId)[1:6], date %in% unique(centroids$date)[1:20]),
            aes(x = X, y = Y, col = trackId, group = trackId),
            linewidth = 1, alpha = 0.7)

# so, the vultures share a lot of space, but they also have relatively constant areas of use. Like a combination of scenarios 1 and 2. 
# the key parameter that's different is the home range step size relative to the agent step size. It looks like they should be drawn from the same distribution. We used a factor of 10 and (essentially) 0, but it looks like we need a factor of 1.
  


# Do some permutations ----------------------------------------------------

n <- 100 # number of permutations
length(unique(season_data$dateOnly)) #124 days
# Let's do roughly 20% of that... so 24ish days. so sm should be 12.
sm <- 12 # can shift 12 days in either direction, 24 day range total
sm2 <- 1 # running it again with just 1 day allowed in either direction

# Run the conveyor permutations
realizations_conveyor_v <- vector(mode = "list", length = n)
for(i in 1:n){
  cat(".")
  realizations_conveyor_v[[i]] <- rotate_data_table(dataset = season_data, shiftMax = sm, idCol = "trackId", dateCol = "date", timeCol = "time")
} 

realizations_conveyor_v2 <- vector(mode = "list", length = n) # again with a smaller shift range
for(i in 1:n){
  cat(".")
  realizations_conveyor_v2[[i]] <- rotate_data_table(dataset = season_data, shiftMax = sm2, idCol = "trackId", dateCol = "date", timeCol = "time")
} 

realizations_conveyor_v <- realizations_conveyor_v %>%
  map(., ~.x %>%
        rename(oldtimestamp = timestamp,
               timestamp = newdatetime))
save(realizations_conveyor_v, file = "data/vulture_permutations/realizations_conveyor_v.Rda")

realizations_conveyor_v2 <- realizations_conveyor_v2 %>%
  map(., ~.x %>%
        rename(oldtimestamp = timestamp,
               timestamp = newdatetime))
save(realizations_conveyor_v2, file = "data/vulture_permutations/realizations_conveyor_v2.Rda")

# load("data/vulture_permutations/realizations_conveyor_v.Rda")
# load("data/vulture_permutations/realizations_conveyor_v2.Rda")

# Run the random permutations
# random_data <- season_data
# data.table::setDT(random_data)
# toJoin <- random_data %>%
#   select(ground_speed, location_lat, location_long, date, time)
# realizations_random_v <- as.data.frame(randomizations(DT = random_data, type = "trajectory", id = "trackId", datetime = "timestamp", coords = c("location_lat.1", "location_long.1"), iterations = n)) %>%
#   rename(oldtimestamp = timestamp,
#          timestamp = randomtimestamp) %>%
#   filter(iteration != 0) %>% group_split(iteration, .keep = TRUE) %>%
#   map(., ~bind_cols(.x, toJoin))
# realizations_random_v <- map(realizations_random_v, ~.x %>% mutate(dateOnly = lubridate::date(timestamp)))
# save(realizations_random_v, file = "data/vulture_permutations/realizations_random_v.Rda")
load("data/vulture_permutations/realizations_random_v.Rda")


# Transform both to sf
conveyor_v_sf <- map(realizations_conveyor_v, ~.x %>% st_as_sf(., coords = c("location_long.1", "location_lat.1"), crs = "WGS84"))
rm(realizations_conveyor_v)
gc()
conveyor_v2_sf <- map(realizations_conveyor_v2, ~.x %>% st_as_sf(., coords = c("location_long.1", "location_lat.1"), crs = "WGS84"))
rm(realizations_conveyor_v2)
gc()
random_v_sf <- map(realizations_random_v, ~.x %>% st_as_sf(., coords = c("location_long", "location_lat"), crs = "WGS84", remove = F))
rm(realizations_random_v)
gc()

fe_conveyor <- map(conveyor_v_sf, ~{
  getFlightEdges(dataset = .x, roostPolygons = roostPolygons, roostBuffer = 50, consecThreshold = 2, distThreshold = 1000, speedThreshUpper = NULL, speedThreshLower = 5, timeThreshold = "10 minutes", idCol = "trackId", quiet = T, includeAllVertices = F, daytimeOnly = T, return = "sri", getLocs = F)
})
save(fe_conveyor, file = "data/vulture_permutations/fe_conveyor.Rda")
rm(conveyor_v_sf)
#load("data/vulture_permutations/fe_conveyor.Rda")

fe_conveyor_2 <- map(conveyor_v2_sf, ~{
  getFlightEdges(dataset = .x, roostPolygons = roostPolygons, roostBuffer = 50, consecThreshold = 2, distThreshold = 1000, speedThreshUpper = NULL, speedThreshLower = 5, timeThreshold = "10 minutes", idCol = "trackId", quiet = T, includeAllVertices = F, daytimeOnly = T, return = "sri", getLocs = F)
})
save(fe_conveyor_2, file = "data/vulture_permutations/fe_conveyor_2.Rda")
rm(conveyor_v2_sf)
#load("data/vulture_permutations/fe_conveyor_2.Rda")

fe_random <- map(random_v_sf, ~{
  getFlightEdges(dataset = .x, roostPolygons = roostPolygons, roostBuffer = 50, consecThreshold = 2, distThreshold = 1000, speedThreshUpper = NULL, speedThreshLower = 5, timeThreshold = "10 minutes", idCol = "trackId", quiet = T, includeAllVertices = F, daytimeOnly = T, return = "sri", getLocs = F)
})
save(fe_random, file = "data/vulture_permutations/fe_random.Rda")
rm(random_v_sf)
#load("data/vulture_permutations/fe_random.Rda")

# Something weird happened here where some of the data frames returned have 6 variables instead of 3 and seem to be edge lists instead of SRI. I think all of those have 0 rows, though...
map_dbl(fe_conveyor, ncol)
fe_conveyor[map_dbl(fe_conveyor, ncol) > 3] # yeah, these all have 0 rows

# Calculate the stats
# we defined n above--number of permutations
fe_conveyor_df <- purrr::map2(.x = fe_conveyor, .y = 1:n, ~{.x %>% mutate(iteration = .y)}) %>% discard(~nrow(.x) == 0) %>% purrr::list_rbind() %>%
  filter(sri > 0) # that makes more sense...
flipped_c <- fe_conveyor_df
names(flipped_c)[1:2] <- c("ID2", "ID1") # flip the order to make sure we're able to group just by ID1
bound_c <- bind_rows(fe_conveyor_df, flipped_c)

stats_conveyor <- bound_c %>%
  group_by(iteration, ID1) %>%
  summarize(deg = length(unique(ID2)),
            n = n(),
            str = sum(sri))

fe_conveyor_df_2 <- purrr::map2(.x = fe_conveyor_2, .y = 1:n, ~{.x %>% mutate(iteration = .y)}) %>% discard(~nrow(.x) == 0) %>% purrr::list_rbind() %>%
  filter(sri > 0) # that makes more sense...
flipped_c2 <- fe_conveyor_df_2
names(flipped_c2)[1:2] <- c("ID2", "ID1") # flip the order to make sure we're able to group just by ID1
bound_c2 <- bind_rows(fe_conveyor_df_2, flipped_c2)

stats_conveyor2 <- bound_c2 %>%
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

vulture_stats_perms_2 <- stats_conveyor2 %>%
  mutate(type = "conveyor") %>%
  bind_rows(stats_random %>%
              mutate(type = "random"))

save(vulture_stats_perms, file = "data/vulture_permutations/vulture_stats_perms.Rda")
save(vulture_stats_perms_2, file = "data/vulture_permutations/vulture_stats_perms_2.Rda")
load("data/vulture_permutations/vulture_stats_perms.Rda")
load("data/vulture_permutations/vulture_stats_perms_2.Rda")

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
