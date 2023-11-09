library(tidyverse)
library(tidytext)
library(patchwork)
source("scripts/00.1_functions.R")

# Load data ---------------------------------------------------------------
load("data/simulations/sim1_ns.Rda")
load("data/simulations/sim1_s.Rda")
load("data/simulations/sim2_ns.Rda")
load("data/simulations/sim2_s.Rda")
load("data/simulations/sim3_ns.Rda")
load("data/simulations/sim3_s.Rda")

simulations <- rep(1:3, each = 2)
soc_nonsoc <- rep(c("ns", "s"), 3)

# Make a list
sims <- list(sim1_ns, sim1_s, sim2_ns, sim2_s, sim3_ns, sim3_s)
rm(sim1_ns, sim1_s, sim2_ns, sim2_s, sim3_ns, sim3_s)

# Extract just the XY coordinates
sims_xy <- map(sims, "XY")

# Fix the times for each one
sims_xy <- map(sims_xy, fix_times)

# Label the simulation data
sims_xy <- map2(sims_xy, simulations, ~.x %>% mutate(sim = .y))
sims_xy <- map2(sims_xy, soc_nonsoc, ~.x %>% mutate(sns = .y))
rm(sims)

# Create separate date and time columns
sims_xy <- map(sims_xy, ~.x %>% mutate(date = lubridate::date(datetime),
                                       time = stringr::str_extract(datetime, pattern = "[0-9]{2}\\:[0-9]{2}\\:[0-9]{2}"),
                                       time = tidyr::replace_na(time, "00:00:00")))

# Downsampling
sims_xy <- map(sims_xy, ~.x %>% group_by(date) %>% mutate(stepinday = 1:n()))
# starting with 50 steps per day. Let's also do 25, 10, and 5 steps per day
sims_xy_25spd <- map(sims_xy, ~.x %>% filter(stepinday %% 2 == 0))
sims_xy_10spd <- map(sims_xy, ~.x %>% filter(stepinday %% 5 == 0))
sims_xy_5spd <- map(sims_xy, ~.x %>% filter(stepinday %% 10 == 0))
save(sims_xy, file = "data/simulations/sims_xy.Rda")
save(sims_xy_25spd, file = "data/simulations/sims_xy_25spd.Rda")
save(sims_xy_10spd, file = "data/simulations/sims_xy_10spd.Rda")
save(sims_xy_5spd, file = "data/simulations/sims_xy_5spd.Rda")

load("data/simulations/sims_xy.Rda")
load("data/simulations/sims_xy_25spd.Rda")
load("data/simulations/sims_xy_10spd.Rda")
load("data/simulations/sims_xy_5spd.Rda")

# PERMUTATIONS ------------------------------------------------------------
n <- 50 # how many permutations?

# Conveyor permutations ---------------------------------------------------
sms <- seq(from = 1, to = 25, by = 1)

convey <- function(sims, shifts, n){
  vec <- vector(mode = "list", length = length(shifts))
  for(i in 1:length(shifts)){
    shift <- shifts[i]
    cat("shiftmax = ", shift, "\n")
    test <- map(sims, ~{
      realizations <- vector(mode = "list", length = n)
      for(j in 1:n){
        cat(".")
        realizations[[j]] <- rotate_data_table(dataset = .x, shiftMax = shift,
                                               idCol = "indiv", dateCol = "date",
                                               timeCol = "time")
      }
      cat("\n")
      return(realizations)
    })
    vec[[i]] <- test
  }
  return(vec)
}

# Run the conveyor simulations
conveyor_sms_50spd <- convey(sims = sims_xy, shifts = sms, n = n)
conveyor_sms_25spd <- convey(sims = sims_xy_25spd, shifts = sms, n = n)
conveyor_sms_10spd <- convey(sims = sims_xy_10spd, shifts = sms, n = n)
conveyor_sms_5spd <- convey(sims = sims_xy_5spd, shifts = sms, n = n)

save(conveyor_sms_50spd, file = "data/simulations/conveyor_sms_50spd.Rda")
save(conveyor_sms_25spd, file = "data/simulations/conveyor_sms_25spd.Rda")
save(conveyor_sms_10spd, file = "data/simulations/conveyor_sms_10spd.Rda")
save(conveyor_sms_5spd, file = "data/simulations/conveyor_sms_5spd.Rda")

load("data/simulations/conveyor_sms_50spd.Rda")
load("data/simulations/conveyor_sms_25spd.Rda")
load("data/simulations/conveyor_sms_10spd.Rda")
load("data/simulations/conveyor_sms_5spd.Rda")

# Random permutations -----------------------------------------------------
sims_xy_dt <- map(sims_xy, setDT)
sims_xy_25spd_dt <- map(sims_xy_25spd, setDT)
sims_xy_10spd_dt <- map(sims_xy_10spd, setDT)
sims_xy_5spd_dt <- map(sims_xy_5spd, setDT)

sims_xy_dt <- map(sims_xy_dt, ~mutate(.x, datetime = as.POSIXct(datetime)))
sims_xy_25spd_dt <- map(sims_xy_25spd_dt, ~mutate(.x, datetime = as.POSIXct(datetime)))
sims_xy_10spd_dt <- map(sims_xy_10spd_dt, ~mutate(.x, datetime = as.POSIXct(datetime)))
sims_xy_5spd_dt <- map(sims_xy_5spd_dt, ~mutate(.x, datetime = as.POSIXct(datetime)))

rand <- function(sims, n){
  out <- map(sims, ~{
    r <- spatsoc::randomizations(DT = .x, type = "trajectory", id = "indiv",
                                 datetime = "datetime", coords = c("X", "Y"),
                                 iterations = n) %>%
      filter(iteration != 0) %>% group_by(iteration) %>%
      group_split(.keep = TRUE)
    cat("*")
    return(r)
  })
  return(out)
}

random_50spd <- rand(sims_xy_dt, n)
random_25spd <- rand(sims_xy_25spd_dt, n)
random_10spd <- rand(sims_xy_10spd_dt, n)
random_5spd <- rand(sims_xy_5spd_dt, n)

save(random_50spd, file = "data/simulations/random_50spd.Rda")
save(random_25spd, file = "data/simulations/random_25spd.Rda")
save(random_10spd, file = "data/simulations/random_10spd.Rda")
save(random_5spd, file = "data/simulations/random_5spd.Rda")

load("data/simulations/random_50spd.Rda")
load("data/simulations/random_25spd.Rda")
load("data/simulations/random_10spd.Rda")
load("data/simulations/random_5spd.Rda")


# STATS -------------------------------------------------------------------
# 1. observed simulations
obs_stats_50spd <- map(sims_xy, ~get_stats(data = as.data.frame(.x), edgelist = get_edgelist(.x, idCol = "indiv", dateCol = "datetime"), idCol = "indiv"), .progress = T)
obs_stats_25spd <- map(sims_xy_25spd, ~get_stats(data = as.data.frame(.x), edgelist = get_edgelist(.x, idCol = "indiv", dateCol = "datetime"), idCol = "indiv"), .progress = T)
obs_stats_10spd <- map(sims_xy_10spd, ~get_stats(data = as.data.frame(.x), edgelist = get_edgelist(.x, idCol = "indiv", dateCol = "datetime"), idCol = "indiv"), .progress = T)
obs_stats_5spd <- map(sims_xy_5spd, ~get_stats(data = as.data.frame(.x), edgelist = get_edgelist(.x, idCol = "indiv", dateCol = "datetime"), idCol = "indiv"), .progress = T)

label <- function(stats){
  out <- map2(stats, simulations, ~.x %>% mutate(sim = .y))
  out <- map2(out, soc_nonsoc, ~.x %>% mutate(sns = .y))
  out <- out %>% purrr::list_rbind() %>% mutate(uniquesim = paste(sim, sns, sep = "_"))
  return(out)
}

obs_stats_50spd <- label(obs_stats_50spd)
obs_stats_25spd <- label(obs_stats_25spd)
obs_stats_10spd <- label(obs_stats_10spd)
obs_stats_5spd <- label(obs_stats_5spd)

# Label the stats
save(obs_stats_50spd, file = "data/simulations/obs_stats_50spd.Rda")
save(obs_stats_25spd, file = "data/simulations/obs_stats_25spd.Rda")
save(obs_stats_10spd, file = "data/simulations/obs_stats_10spd.Rda")
save(obs_stats_5spd, file = "data/simulations/obs_stats_5spd.Rda")

load("data/simulations/obs_stats_50spd.Rda")
load("data/simulations/obs_stats_25spd.Rda")
load("data/simulations/obs_stats_10spd.Rda")
load("data/simulations/obs_stats_5spd.Rda")

# 2. conveyor permutations
names(sms) <- as.character(1:length(sms))
names(simulations) <- as.character(1:length(simulations))
names(soc_nonsoc) <- as.character(1:length(soc_nonsoc))

statsconveyor <- function(conveyorlist){
  out <- map(conveyorlist, ~{
    simulations <- .x
    stats_simulations <- map(simulations, ~{
      iterations <- .x
      stats_iterations <- map(iterations, ~{
        data <- .x
        edges <- get_edgelist(data = data, idCol = "indiv", dateCol = "newdatetime")
        stats <- get_stats(edgelist = edges, data = data, idCol = "indiv")
        return(stats)
      }, .progress = T) %>% purrr::list_rbind(names_to = "iteration")
    }) %>% purrr::list_rbind(names_to = "simulation")
  }) %>% purrr::list_rbind(names_to = "shift") %>%
    mutate(shift = sms[as.character(shift)],
           sim = simulations[as.character(simulation)],
           sns = soc_nonsoc[as.character(simulation)]) %>%
    select(-simulation)
  return(out)
}

stats_conveyor_50spd <- statsconveyor(conveyor_sms_50spd)
stats_conveyor_25spd <- statsconveyor(conveyor_sms_25spd)
stats_conveyor_10spd <- statsconveyor(conveyor_sms_10spd)
stats_conveyor_5spd <- statsconveyor(conveyor_sms_5spd)

save(stats_conveyor_50spd, file = "data/simulations/stats_conveyor_50spd.Rda")
save(stats_conveyor_25spd, file = "data/simulations/stats_conveyor_25spd.Rda")
save(stats_conveyor_10spd, file = "data/simulations/stats_conveyor_10spd.Rda")
save(stats_conveyor_5spd, file = "data/simulations/stats_conveyor_5spd.Rda")

load("data/simulations/stats_conveyor_50spd.Rda")
load("data/simulations/stats_conveyor_25spd.Rda")
load("data/simulations/stats_conveyor_10spd.Rda")
load("data/simulations/stats_conveyor_5spd.Rda")

statsrandom <- function(randomlist){
  out <- vector(mode = "list", length = length(randomlist))
  for(i in 1:length(randomlist)){
    edges <- map(randomlist[[i]], ~get_edgelist(data = .x, idCol = "indiv", dateCol = "randomdatetime"))
    stats <- map2(.x = randomlist[[i]], .y = edges, ~get_stats(edgelist = .y, data = .x, idCol = "indiv")) %>%
      purrr::list_rbind(names_to = "iteration")
    out[[i]] <- stats
  }
  return(out)
}

stats_random_50spd <- statsrandom(random_50spd)
stats_random_25spd <- statsrandom(random_25spd)
stats_random_10spd <- statsrandom(random_10spd)
stats_random_5spd <- statsrandom(random_5spd)

stats_random_50spd <- label(stats_random_50spd)
stats_random_25spd <- label(stats_random_25spd)
stats_random_10spd <- label(stats_random_10spd)
stats_random_5spd <- label(stats_random_5spd)

save(stats_random_50spd, file = "data/simulations/stats_random_50spd.Rda")
save(stats_random_25spd, file = "data/simulations/stats_random_25spd.Rda")
save(stats_random_10spd, file = "data/simulations/stats_random_10spd.Rda")
save(stats_random_5spd, file = "data/simulations/stats_random_5spd.Rda")

load("data/simulations/stats_random_50spd.Rda")
load("data/simulations/stats_random_25spd.Rda")
load("data/simulations/stats_random_10spd.Rda")
load("data/simulations/stats_random_5spd.Rda")

# Combine all the stats
stats_obs_downsampled <- obs_stats_50spd %>% mutate(spd = 50) %>%
  bind_rows(obs_stats_25spd %>% mutate(spd = 25)) %>%
  bind_rows(obs_stats_10spd %>% mutate(spd = 10)) %>%
  bind_rows(obs_stats_5spd %>% mutate(spd = 5))

stats_conveyor <- stats_conveyor_50spd %>% mutate(spd = 50) %>%
  bind_rows(stats_conveyor_25spd %>% mutate(spd = 25)) %>%
  bind_rows(stats_conveyor_10spd %>% mutate(spd = 10)) %>%
  bind_rows(stats_conveyor_5spd %>% mutate(spd = 5))

stats_random <- stats_random_50spd %>% mutate(spd = 50) %>%
  bind_rows(stats_random_25spd %>% mutate(spd = 25)) %>%
  bind_rows(stats_random_10spd %>% mutate(spd = 10)) %>%
  bind_rows(stats_random_5spd %>% mutate(spd = 5))

stats_perm_downsampled <- stats_conveyor %>%
  mutate(type = "conveyor") %>%
  bind_rows(stats_random %>% mutate(type = "random"))

save(stats_obs_downsampled, file = "data/simulations/stats_obs_downsampled.Rda")
save(stats_perm_downsampled, file = "data/simulations/stats_perm_downsampled.Rda")
load("data/simulations/stats_perm_downsampled.Rda")
load("data/simulations/stats_obs_downsampled.Rda")
