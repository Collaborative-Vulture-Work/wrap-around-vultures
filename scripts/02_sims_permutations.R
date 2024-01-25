library(tidyverse)
library(tidytext)
library(patchwork)
library(gt)
library(gtExtras)
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
rm(sim1_ns, sim1_s, sim2_ns, sim2_s, sim3_ns, sim3_s) # remove these because we're not using them anymore

# Extract just the XY coordinates
sims_xy <- map(sims, "XY")

# Fix the times for each one
sims_xy <- map(sims_xy, fix_times)

# Label the simulation data
sims_xy <- map2(sims_xy, simulations, ~.x %>% mutate(sim = .y))
sims_xy <- map2(sims_xy, soc_nonsoc, ~.x %>% mutate(sns = .y))

# Create separate date and time columns
sims_xy <- map(sims_xy, ~.x %>% mutate(date = lubridate::date(datetime),
                                       time = stringr::str_extract(datetime, pattern = "[0-9]{2}\\:[0-9]{2}\\:[0-9]{2}"),
                                       time = tidyr::replace_na(time, "00:00:00")))
rm(sims)
save(sims_xy, file = "data/simulations/sims_xy.Rda")

# PERMUTATIONS ------------------------------------------------------------
n <- 100 # how many permutations?

# Conveyor permutations ---------------------------------------------------
sms <- seq(from = 1, to = 25, by = 1)

sms_sims_conveyor <- vector(mode = "list", length = length(sms))
for(i in 1:length(sms)){
  shift <- sms[i]
  cat("shiftmax = ", shift, "\n")
  sims_conveyor <- map(sims_xy, ~{
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
  sms_sims_conveyor[[i]] <- sims_conveyor
}
save(sms_sims_conveyor, file = "data/simulations/sms_sims_conveyor.Rda")
load("data/simulations/sms_sims_conveyor.Rda")

# Random permutations -----------------------------------------------------
sims_xy_dt <- map(sims_xy, setDT) # make them into data tables so we can use the spatsoc function properly
sims_xy_dt <- map(sims_xy_dt, ~.x %>% mutate(datetime = as.POSIXct(datetime)))

sims_random <- map(sims_xy_dt, ~{
  r <- spatsoc::randomizations(DT = .x, type = "trajectory", id = "indiv",
                               datetime = "datetime", coords = c("X", "Y"),
                               iterations = n) %>%
    filter(iteration != 0) %>% # remove the original data (iteration 0)
    group_by(iteration) %>%
    group_split(.keep = TRUE)
  cat("*")
  return(r)
})
save(sims_random, file = "data/simulations/sims_random.Rda")
load("data/simulations/sims_random.Rda")

# STATS -------------------------------------------------------------------
# 1. observed simulations
obs_stats <- map(sims_xy,
                 ~get_stats(data = .x,
                            edgelist = get_edgelist(.x, idCol = "indiv",
                                                    dateCol = "datetime"),
                            idCol = "indiv"), 
                 .progress = T)
# Label the stats
obs_stats <- map2(obs_stats, simulations, ~.x %>% mutate(sim = .y))
obs_stats <- map2(obs_stats, soc_nonsoc, ~.x %>% mutate(sns = .y))
obs_stats_df <- obs_stats %>% purrr::list_rbind() %>%
  mutate(uniquesim = paste(sim, sns, sep = "_"))
save(obs_stats_df, file = "data/simulations/obs_stats_df.Rda")
load("data/simulations/obs_stats_df.Rda")

# 2. conveyor permutations
stats_shifts <- map(sms_sims_conveyor, ~{
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
}) %>% purrr::list_rbind(names_to = "shift")

names(sms) <- as.character(1:length(sms))
names(simulations) <- as.character(1:length(simulations))
names(soc_nonsoc) <- as.character(1:length(soc_nonsoc))

sms_conveyor_stats_df <- stats_shifts %>%
  mutate(shift = sms[as.character(shift)],
         sim = simulations[as.character(simulation)],
         sns = soc_nonsoc[as.character(simulation)]) %>%
  select(-simulation)

save(sms_conveyor_stats_df, file = "data/simulations/sms_conveyor_stats_df.Rda")
load("data/simulations/sms_conveyor_stats_df.Rda")

# 3. random permutations
random_stats <- vector(mode = "list", length = length(sims_random))
for(i in 1:length(sims_random)){
  edges <- map(sims_random[[i]], ~get_edgelist(data = .x, idCol = "indiv", dateCol = "randomdatetime"))
  stats <- map2(.x = sims_random[[i]], .y = edges, ~get_stats(edgelist = .y, data = .x, idCol = "indiv")) %>%
    purrr::list_rbind(names_to = "iteration")
  random_stats[[i]] <- stats
}
save(random_stats, file = "data/simulations/random_stats.Rda")
load("data/simulations/random_stats.Rda")

# Label the stats
random_stats <- map2(random_stats, simulations, ~.x %>% mutate(sim = .y))
random_stats <- map2(random_stats, soc_nonsoc, ~.x %>% mutate(sns = .y))
random_stats_df <- random_stats %>% purrr::list_rbind()
save(random_stats_df, file = "data/simulations/random_stats_df.Rda")
load("data/simulations/random_stats_df.Rda")

# Combine all the stats
stats_perm <- sms_conveyor_stats_df %>% mutate(type = "conveyor") %>%
  bind_rows(random_stats_df %>% mutate(type = "random")) %>%
  mutate(uniquesim = paste(sim, sns, sep = "_"))
save(stats_perm, file = "data/simulations/stats_perm.Rda")
load("data/simulations/stats_perm.Rda")

# Zscores -----------------------------------------------------------------
stat <- stats_perm %>%
  left_join(obs_stats_df %>% select(uniquesim, ID1, "degree_obs" = degree, "strength_obs" = strength), by = c("uniquesim", "ID1"))
nrow(stat) == nrow(stats_perm)
test <- stat %>%
  group_by(uniquesim, sim, sns, type, shift, iteration) %>%
  summarize(mndeg = mean(degree),
            mnstr = mean(strength),
            mndeg_obs = mean(degree_obs),
            mnstr_obs = mean(strength_obs)) %>%
  mutate(sd_mndeg = sd(mndeg),
         sd_mnstr = sd(mnstr),
         diff_deg = mndeg_obs-mndeg,
         diff_str = mnstr_obs-mnstr,
         D_deg = diff_deg/sd_mndeg,
         D_str = diff_str/sd_mnstr,
         Z_deg = (mndeg_obs-mean(mndeg))/sd_mndeg,
         Z_str = (mnstr_obs-mean(mnstr))/sd_mnstr)

test_summ <- test %>%
  group_by(uniquesim, sim, sns, type, shift, Z_deg, Z_str) %>%
  summarize(mnD_deg = mean(D_deg),
         mnD_str = mean(D_str)) %>%
  pivot_longer(cols = c("Z_deg", "Z_str", "mnD_deg", "mnD_str"), names_to = "param", values_to = "value") %>%
  mutate(measure = str_extract(param, pattern = "deg|str"),
         param = str_extract(param, pattern = "mnD|Z")) %>%
  ungroup()

test_summ %>%
  filter(param == "Z") %>%
  ggplot(aes(x = shift, y = value, col = param, linetype = sns, group = interaction(param, sns)))+
  geom_line()+
  facet_grid(rows = vars(sim),
             cols = vars(measure))+
  theme_classic()

fortables <- test_summ %>%
  filter(param == "Z", type == "conveyor") %>%
  mutate("Time-shift range (days)" = shift*2) %>%
  mutate(uniquesim = case_when(uniquesim == "1_ns" ~ "Static HR (non-sociable)",
                               uniquesim == "1_s" ~ "Static HR (sociable)",
                               uniquesim == "2_ns" ~ "Locally changing HR (non-sociable)",
                               uniquesim == "2_s" ~ "Locally changing HR (sociable)",
                               uniquesim == "3_ns" ~ "Directionally changing HR (non-sociable)",
                               uniquesim == "3_s" ~ "Directionally changing HR (sociable)"),
         value = round(value, 3))

zscore_table_deg <- fortables %>%
  filter(measure == "deg") %>%
  select(uniquesim, `Time-shift range (days)`, value) %>%
  pivot_wider(id_cols = `Time-shift range (days)`, 
              names_from = "uniquesim", values_from = "value") %>%
  gt() %>%
  tab_header(
    title = "Population mean degree",
    subtitle = "Wrap-around method, z-scores"
  )
zscore_table_deg
tmp <- tempfile(fileext = ".html")
zscore_table_deg %>% gtsave(tmp)
webshot::webshot(tmp, "fig/zscore_table_deg.png")

zscore_table_str <- fortables %>%
  filter(measure == "str") %>%
  select(uniquesim, `Time-shift range (days)`, value) %>%
  pivot_wider(id_cols = `Time-shift range (days)`, 
              names_from = "uniquesim", values_from = "value") %>%
  gt() %>%
  tab_header(
    title = "Population mean strength",
    subtitle = "Wrap-around method, z-scores"
  )
zscore_table_str
tmp <- tempfile(fileext = ".html")
zscore_table_str %>% gtsave(tmp)
webshot::webshot(tmp, "fig/zscore_table_str.png")

# For path shuffling:
pathShufflingTable <- stat %>%
  group_by(uniquesim, sim, sns, type, shift, iteration) %>%
  summarize(mndeg = mean(degree),
            mnstr = mean(strength),
            mndeg_obs = mean(degree_obs),
            mnstr_obs = mean(strength_obs)) %>%
  mutate(sd_mndeg = sd(mndeg),
         sd_mnstr = sd(mnstr),
         diff_deg = mndeg_obs-mndeg,
         diff_str = mnstr_obs-mnstr,
         D_deg = diff_deg/sd_mndeg,
         D_str = diff_str/sd_mnstr,
         Z_deg = (mndeg_obs-mean(mndeg))/sd_mndeg,
         Z_str = (mnstr_obs-mean(mnstr))/sd_mnstr) %>%
  filter(type == "random") %>%
  ungroup() %>%
  select(uniquesim, Z_deg, Z_str) %>%
  group_by(uniquesim) %>%
  slice(1) %>% mutate(uniquesim = case_when(uniquesim == "1_ns" ~ "Static HR (non-sociable)",
                               uniquesim == "1_s" ~ "Static HR (sociable)",
                               uniquesim == "2_ns" ~ "Locally changing HR (non-sociable)",
                               uniquesim == "2_s" ~ "Locally changing HR (sociable)",
                               uniquesim == "3_ns" ~ "Directionally changing HR (non-sociable)",
                               uniquesim == "3_s" ~ "Directionally changing HR (sociable)"),
         Z_deg = round(Z_deg, 3),
         Z_str = round(Z_str, 3)) %>%
  pivot_longer(c("Z_deg", "Z_str"), names_to = "Measure", values_to = "value") %>%
  mutate(Measure = case_when(Measure == "Z_deg" ~ "Degree",
                             Measure == "Z_str" ~ "Strength")) %>%
  pivot_wider(id_cols = Measure, names_from = uniquesim, values_from = value) %>%
  gt() %>%
  tab_header(
    title = "Population mean degree and strength",
    subtitle = "Path shuffling method, z-scores"
  )
pathShufflingTable
tmp <- tempfile(fileext = ".html")
pathShufflingTable %>% gtsave(tmp)
webshot::webshot(tmp, "fig/pathShufflingTable.png")
