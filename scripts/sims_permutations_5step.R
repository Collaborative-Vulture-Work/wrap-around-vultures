library(tidyverse)
library(tidytext)
library(patchwork)
source("scripts/functions.R")

# Load data ---------------------------------------------------------------
load("data/simulations/five_steps_per_day/sim1_ns_5step.Rda")
load("data/simulations/five_steps_per_day/sim1_s_5step.Rda")
load("data/simulations/five_steps_per_day/sim2_ns_5step.Rda")
load("data/simulations/five_steps_per_day/sim2_s_5step.Rda")
load("data/simulations/five_steps_per_day/sim3_ns_5step.Rda")
load("data/simulations/five_steps_per_day/sim3_s_5step.Rda")

simulations <- rep(1:3, each = 2)
soc_nonsoc <- rep(c("ns", "s"), 3)

# Make a list
sims_5step <- list(sim1_ns_5step, sim1_s_5step, sim2_ns_5step, sim2_s_5step, sim3_ns_5step, sim3_s_5step)

# Extract just the XY coordinates
sims_xy_5step <- map(sims_5step, "XY")

# Fix the times for each one
sims_xy_5step <- map(sims_xy_5step, fix_times)

# Label the simulation data
sims_xy_5step <- map2(sims_xy_5step, simulations, ~.x %>% mutate(sim = .y))
sims_xy_5step <- map2(sims_xy_5step, soc_nonsoc, ~.x %>% mutate(sns = .y))

# Create separate date and time columns
sims_xy_5step <- map(sims_xy_5step, ~.x %>% mutate(date = lubridate::date(datetime),
                                       time = stringr::str_extract(datetime, pattern = "[0-9]{2}\\:[0-9]{2}\\:[0-9]{2}"),
                                       time = tidyr::replace_na(time, "00:00:00")))


# PERMUTATIONS ------------------------------------------------------------
n <- 50 # how many permutations?

# Conveyor permutations ---------------------------------------------------
sms <- seq(from = 1, to = 25, by = 1)

sms_sims_conveyor_5step <- vector(mode = "list", length = length(sms))
for(i in 1:length(sms)){
  shift <- sms[i]
  cat("shiftmax = ", shift, "\n")
  sims_conveyor_5step <- map(sims_xy_5step, ~{
    realizations_5step <- vector(mode = "list", length = n)
    for(j in 1:n){
      cat(".")
      realizations_5step[[j]] <- rotate_data_table(dataset = .x, shiftMax = shift,
                                             idCol = "indiv", dateCol = "date",
                                             timeCol = "time")
    }
    cat("\n")
    return(realizations_5step)
  })
  sms_sims_conveyor_5step[[i]] <- sims_conveyor_5step
}
save(sms_sims_conveyor_5step, file = "data/simulations/sms_sims_conveyor_5step.Rda")
load("data/simulations/sms_sims_conveyor_5step.Rda")

# Random permutations -----------------------------------------------------
sims_xy_dt_5step <- map(sims_xy_5step, setDT) # make them into data tables so we can use the spatsoc function properly
sims_xy_dt_5step <- map(sims_xy_dt_5step, ~.x %>% mutate(datetime = as.POSIXct(datetime)))

sims_random_5step <- map(sims_xy_dt_5step, ~{
  r <- spatsoc::randomizations(DT = .x, type = "trajectory", id = "indiv",
                               datetime = "datetime", coords = c("X", "Y"),
                               iterations = n) %>%
    filter(iteration != 0) %>% # remove the original data (iteration 0)
    group_by(iteration) %>%
    group_split(.keep = TRUE)
  cat("*")
  return(r)
})
save(sims_random_5step, file = "data/simulations/sims_random_5step.Rda")
load("data/simulations/sims_random_5step.Rda")


# STATS -------------------------------------------------------------------
# 1. observed simulations
obs_stats_5step <- map(sims_xy_5step,
                 ~get_stats(data = .x,
                            edgelist = get_edgelist(.x, idCol = "indiv",
                                                    dateCol = "datetime")))
# Label the stats
obs_stats_5step <- map2(obs_stats_5step, simulations, ~.x %>% mutate(sim = .y))
obs_stats_5step <- map2(obs_stats_5step, soc_nonsoc, ~.x %>% mutate(sns = .y))
obs_stats_df_5step <- obs_stats_5step %>% purrr::list_rbind() %>%
  mutate(uniquesim = paste(sim, sns, sep = "_"))
save(obs_stats_df_5step, file = "data/simulations/obs_stats_df_5step.Rda")
load("data/simulations/obs_stats_df_5step.Rda")

# 2. conveyor permutations
stats_shifts_5step <- map(sms_sims_conveyor_5step, ~{
  simulations <- .x
  stats_simulations <- map(simulations, ~{
    iterations <- .x
    stats_iterations <- map(iterations, ~{
      data <- .x
      edges <- get_edgelist(data = data, idCol = "indiv", dateCol = "newdatetime")
      stats <- get_stats(edgelist = edges, data = data)
      return(stats)
    }) %>% purrr::list_rbind(names_to = "iteration")
  }) %>% purrr::list_rbind(names_to = "simulation")
}) %>% purrr::list_rbind(names_to = "shift")

names(sms) <- as.character(1:length(sms))
names(simulations) <- as.character(1:length(simulations))
names(soc_nonsoc) <- as.character(1:length(soc_nonsoc))

sms_conveyor_stats_df_5step <- stats_shifts_5step %>%
  mutate(shift = sms[as.character(shift)],
         sim = simulations[as.character(simulation)],
         sns = soc_nonsoc[as.character(simulation)]) %>%
  select(-simulation)

save(sms_conveyor_stats_df_5step, file = "data/simulations/sms_conveyor_stats_df_5step.Rda")
load("data/simulations/sms_conveyor_stats_df_5step.Rda")

# 3. random permutations
random_stats_5step <- vector(mode = "list", length = length(sims_random_5step))
for(i in 1:length(sims_random_5step)){
  edges <- map(sims_random_5step[[i]], ~get_edgelist(data = .x, idCol = "indiv", dateCol = "randomdatetime"))
  stats_5step <- map2(.x = sims_random_5step[[i]], .y = edges, ~get_stats(edgelist = .y, data = .x)) %>%
    purrr::list_rbind(names_to = "iteration")
  random_stats_5step[[i]] <- stats_5step
}
save(random_stats_5step, file = "data/simulations/random_stats_5step.Rda")
load("data/simulations/random_stats_5step.Rda")

# Label the stats
random_stats_5step <- map2(random_stats_5step, simulations, ~.x %>% mutate(sim = .y))
random_stats_5step <- map2(random_stats_5step, soc_nonsoc, ~.x %>% mutate(sns = .y))
random_stats_df_5step <- random_stats_5step %>% purrr::list_rbind()
save(random_stats_df_5step, file = "data/simulations/random_stats_df_5step.Rda")
load("data/simulations/random_stats_df_5step.Rda")

# Combine all the stats
stats_perm_5step <- sms_conveyor_stats_df_5step %>% mutate(type = "conveyor") %>%
  bind_rows(random_stats_df_5step %>% mutate(type = "random")) %>%
  mutate(uniquesim = paste(sim, sns, sep = "_"))
save(stats_perm_5step, file = "data/simulations/stats_perm_5step.Rda")
load("data/simulations/stats_perm_5step.Rda")