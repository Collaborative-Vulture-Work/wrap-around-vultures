# Multiple levels of sociality
library(tidyverse)
source("scripts/00.1_functions.R")

# Generate the simulations ------------------------------------------------
# We already have ns, and 0.75 social, for each of the three scenarios. Now let's do the other sociality levels
socLevels <- seq(from = 0, to = 1, by = 0.1)
seed <- 9252023
N <- 30
Days <- 50
DayLength <- 50
Soc_Percep_Rng = 1000
Scl = 1000
EtaCRW = 0.7
StpStd_ind <- 5
hre <- 0.7

## Sim 1 -------------------------------------------------------------------
r <- 0.01 # home range centers effectively not moving
baseAgentStep <- 7
HRStpSize <- baseAgentStep*r
HRStpStd <- HRStpSize*0.75 # leaving this here for now--could go back and change later if we want. 
hrk <- 0.01

# sim1_socLevels <- map(socLevels, ~{
#   cat("Running simulation with social preference level =", .x, "\n")
#   sim <- simulateAgents(N = N, Days = Days, DayLength = DayLength, 
#                         Soc_Percep_Rng = Soc_Percep_Rng,
#                         Scl = Scl, seed = seed, EtaCRW = EtaCRW, 
#                         StpSize_ind = baseAgentStep, StpStd_ind = StpStd_ind,
#                         Kappa_ind = 4, quiet = F, sim_3 = F, socialWeight = .x,
#                         HREtaCRW = hre, HRStpSize = HRStpSize, HRStpStd = HRStpStd,
#                         HRKappa_ind = hrk)
#   return(sim)
# })
# save(sim1_socLevels, file = "data/simulations/sim1_socLevels.Rda")

## Sim 2 -------------------------------------------------------------------
r <- 10 # home range steps are 10x the size of agent steps
baseAgentStep <- 7
HRStpSize <- baseAgentStep*r
HRStpStd <- HRStpSize*0.75 # leaving this here for now--could go back and change later if we want. 
hrk <- 0.01 # effectively k = 0, random direction for home range movement.

# sim2_socLevels <- map(socLevels, ~{
#   cat("Running simulation with social preference level =", .x, "\n")
#   sim <- simulateAgents(N = N, Days = Days, DayLength = DayLength, 
#                         Soc_Percep_Rng = Soc_Percep_Rng,
#                         Scl = Scl, seed = seed, EtaCRW = EtaCRW, 
#                         StpSize_ind = baseAgentStep, StpStd_ind = StpStd_ind,
#                         Kappa_ind = 4, quiet = F, sim_3 = T, socialWeight = .x,
#                         HREtaCRW = hre, HRStpSize = HRStpSize, HRStpStd = HRStpStd,
#                         HRKappa_ind = hrk)
#   return(sim)
# })
# save(sim2_socLevels, file = "data/simulations/sim2_socLevels.Rda")

## Sim 3 -------------------------------------------------------------------
r <- 10 # home range steps are 10x the size of agent steps
baseAgentStep <- 7
HRStpSize <- baseAgentStep*r
HRStpStd <- HRStpSize*0.75 # leaving this here for now--could go back and change later if we want. 
hrk <- 20 # k = 20, highly directional

# sim3_socLevels <- map(socLevels, ~{
#   cat("Running simulation with social preference level =", .x, "\n")
#   sim <- simulateAgents(N = N, Days = Days, DayLength = DayLength, 
#                         Soc_Percep_Rng = Soc_Percep_Rng,
#                         Scl = Scl, seed = seed, EtaCRW = EtaCRW, 
#                         StpSize_ind = baseAgentStep, StpStd_ind = StpStd_ind,
#                         Kappa_ind = 4, quiet = F, sim_3 = T, socialWeight = .x,
#                         HREtaCRW = hre, HRStpSize = HRStpSize, HRStpStd = HRStpStd,
#                         HRKappa_ind = hrk)
#   return(sim)
# })
# save(sim3_socLevels, file = "data/simulations/sim3_socLevels.Rda")

# Run permutations --------------------------------------------------
load("data/simulations/sim1_socLevels.Rda")
load("data/simulations/sim2_socLevels.Rda")
load("data/simulations/sim3_socLevels.Rda")

simulations <- 1:3
soc_nonsoc <- rep("s", 3)

# Make a list
sims <- list(sim1_socLevels, sim2_socLevels, sim3_socLevels)

# Extract just the XY coordinates
sims_xy <- map(sims, ~map(.x, "XY"))
sims_xy <- map(sims_xy, ~(setNames(.x, socLevels) %>%
                            purrr::list_rbind(names_to = "socLevel")))
names(sims_xy) <- paste(simulations, soc_nonsoc, sep = "_")
df <- sims_xy %>%
  purrr::list_rbind(names_to = "simulation")

# Fix times
df_fixedTimes <- fix_times(df)

# Create separate date and time columns
df_fixedDateTime <- df_fixedTimes %>%
  mutate(date = lubridate::date(datetime),
         time = stringr::str_extract(datetime, pattern = "[0-9]{2}\\:[0-9]{2}\\:[0-9]{2}"),
         time = tidyr::replace_na(time, "00:00:00"))

# Need to reattach the columns we lost
toReattach <- df %>%
  select(simulation, socLevel, timestep, day, StepInDay)

df <- cbind(toReattach, df_fixedDateTime)

# Set number of permuations
n <- 100 # will up this to 50 or 100

# Split the dataset into chunks to permute
chunks <- df %>%
  group_by(simulation, socLevel) %>%
  group_split()

## Conveyor permutations ---------------------------------------------------
conveyors <- vector(mode = "list", length = length(chunks))
for(i in 1:length(chunks)){
  chunk <- chunks[[i]]
  cat("simulation = ", chunk$simulation[1], "; socLevel = ", chunk$socLevel[1], "\n", sep = "")
  realizations <- vector(mode = "list", length = n)
  for(j in 1:n){
    cat(".")
    realizations[[j]] <- rotate_data_table(dataset = chunk,
                                           shiftMax = 25,
                                           idCol = "indiv",
                                           dateCol = "date",
                                           timeCol = "time")
  }
  cat("\n")
  conveyors[[i]] <- realizations
}

save(conveyors, file = "data/simulations/conveyors.Rda")

## Random ------------------------------------------------------------------
chunksdt <- map(chunks, setDT)
chunksdt <- map(chunksdt, ~.x %>% mutate(datetime = as.POSIXct(datetime)))

randoms <- map(chunksdt, ~{
  r <- spatsoc::randomizations(DT = .x, type = "trajectory", id = "indiv",
                               datetime = "datetime", coords = c("X", "Y"),
                               iterations = n) %>%
    filter(iteration != 0) %>%
    group_by(iteration) %>%
    group_split(.keep = T)
  cat("*")
  return(r)
})

save(randoms, file = "data/simulations/randoms.Rda")

# Calculate stats ---------------------------------------------------------
## Observed -------------------------------------------------------------
obs_stats <- map(chunks, ~get_stats(data = .x, edgelist = 
                                      get_edgelist(.x, idCol = "indiv",
                                                   dateCol = "datetime"),
                                    idCol = "indiv"),
                 .progress = T)
obs_stats_summ <- map(obs_stats, ~.x %>% summarize(mndeg = mean(degree, na.rm = T),
                                                mnstr = mean(strength, na.rm = T)))

## Conveyor -------------------------------------------------------------
conveyor_stats <- map(conveyors, ~map(.x, ~{
  get_stats(data = .x, edgelist = get_edgelist(.x, 
                                               idCol = "indiv",
                                               dateCol = "newdatetime"),
            idCol = "indiv")},
  .progress = T))
save(conveyor_stats, file = "data/socLevels/conveyor_stats.Rda")

conveyor_stats_summ <- map(conveyor_stats, ~map_dfr(.x, ~{
  .x %>% summarize(mndeg = mean(degree, na.rm = T),
                   mnstr = mean(strength, na.rm = T))
}))

## Random -------------------------------------------------------------
random_stats <- map(randoms, ~map(.x, ~{
  get_stats(data = .x, edgelist = get_edgelist(.x, 
                                               idCol = "indiv",
                                               dateCol = "randomdatetime"),
            idCol = "indiv")},
  .progress = T))
save(random_stats, file = "data/socLevels/random_stats.Rda")

random_stats_summ <- map(random_stats, ~map_dfr(.x, ~{
  .x %>% summarize(mndeg = mean(degree, na.rm = T),
                   mnstr = mean(strength, na.rm = T))
}))

# Join back the chunk info ------------------------------------------------
chunks_tojoin <- map(chunks, ~select(.x, simulation, socLevel) %>% slice(1))
conveyor_stats_summ <- map2(conveyor_stats_summ, chunks_tojoin, ~cbind(.x, .y) %>% 
                              mutate(iteration = 1:n())) %>%
  purrr::list_rbind()
random_stats_summ <- map2(random_stats_summ, chunks_tojoin, ~cbind(.x, .y) %>% 
                            mutate(iteration = 1:n())) %>%
  purrr::list_rbind()

obs_stats_summ <- map2(obs_stats_summ, chunks_tojoin, ~cbind(.x, .y)) %>%
  purrr::list_rbind() %>%
  rename("mndeg_obs" = "mndeg",
         "mnstr_obs" = "mnstr")

conveyor_stats_summ <- left_join(conveyor_stats_summ, obs_stats_summ, by = c("simulation", "socLevel"))
random_stats_summ <- left_join(random_stats_summ, obs_stats_summ, by = c("simulation", "socLevel"))

# Calculate true positives ------------------------------------------------
tp_conveyor <- conveyor_stats_summ %>%
  group_by(simulation, socLevel) %>%
  summarize(less_deg = sum(mndeg <= mndeg_obs),
            less_str = sum(mnstr <= mnstr_obs),
            quant_deg = less_deg/n(),
            quant_str = less_str/n()) %>%
  ungroup() %>%
  rowwise() %>%
  mutate(p2_deg = min(quant_deg, (1-quant_deg))*2,
         p2_str = min(quant_str, (1-quant_str))*2) %>%
  pivot_longer(cols = c(p2_deg, p2_str), 
               names_to = "measure", values_to = "pval") %>%
  mutate(likelihood_detect_social = 1-pval) %>%
  mutate(type = "Wrap-around")

tp_random <- random_stats_summ %>%
  group_by(simulation, socLevel) %>%
  summarize(less_deg = sum(mndeg <= mndeg_obs),
            less_str = sum(mnstr <= mnstr_obs),
            quant_deg = less_deg/n(),
            quant_str = less_str/n()) %>%
  ungroup() %>%
  rowwise() %>%
  mutate(p2_deg = min(quant_deg, (1-quant_deg))*2,
         p2_str = min(quant_str, (1-quant_str))*2) %>%
  pivot_longer(cols = c(p2_deg, p2_str), 
               names_to = "measure", values_to = "pval") %>%
  mutate(likelihood_detect_social = 1-pval) %>%
  mutate(type = "Path shuffling")

tp <- bind_rows(tp_conveyor, tp_random)

tpplot <- tp %>%
  mutate(measure = case_when(measure == "p2_deg" ~ "Degree",
                             measure == "p2_str" ~ "Strength")) %>%
  ggplot(aes(x = socLevel, y = likelihood_detect_social, col = simulation, 
             group = interaction(simulation, type)))+
  geom_path(aes(group = interaction(simulation, type), linetype = type), 
            linewidth = 2)+
  facet_wrap(~measure)+
  scale_color_brewer(name = "Simulation", palette = "Dark2")+
  theme_classic()+
  ylab("Likelihood to detect social effect")+
  xlab("Social weight")+
  theme(text = element_text(size = 14))
ggsave(tpplot, file = "fig/socLevels/tpplot.png", width = 7, height = 5)

fnplot <- tp %>%
  mutate(measure = case_when(measure == "p2_deg" ~ "Degree",
                             measure == "p2_str" ~ "Strength")) %>%
  ggplot(aes(x = socLevel, y = pval, col = simulation, 
             group = interaction(simulation, type)))+
  geom_path(aes(group = interaction(simulation, type), linetype = type), 
            linewidth = 2)+
  facet_wrap(~measure)+
  scale_color_brewer(name = "Simulation", palette = "Dark2")+
  theme_classic()+
  ylab("Likelihood to not detect social effect")+
  xlab("Social weight")+
  theme(text = element_text(size = 14))
ggsave(fnplot, file = "fig/socLevels/fnplot.png", width = 7, height = 5)

