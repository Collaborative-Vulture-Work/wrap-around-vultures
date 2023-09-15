# Script for comparing the two methods under different levels of social attraction

# Setup
source("scripts/functions.R") # pulls in all the functions (written by Orr and Ryan)
library(tidyverse) # for data wrangling and plotting
library(spatsoc) # to implement the trajectory randomization method as described in Orr's paper

# Run simulations with different values of EtaCRW
# 1. Run the simulation to obtain simulated data --------------------------
eta_values <- seq(0, 1, by = 0.1) # not sure what the boundary values of EtaCRW are--will need to track down the supporting information and figure this out. XXX maybe task?
sims_forsocial <- map(eta_values, ~simulateAgents(N_indv = 30, DaysToSimulate = 50, Kappa_ind = 3, quiet = T, ToPlot = 0, Social_Pecrt_rng = 2000))
save(sims_forsocial, file = "data/sims_forsocial.Rda")
load("data/sims_forsocial.Rda")

# Extract the XY coordinates from each one
xys <- map(sims_forsocial, "XY") %>% map(., fix_times)
xys <- map(xys, ~{
  .x$time <- stringr::str_extract(.x$datetime, pattern = "[0-9]{2}\\:[0-9]{2}\\:[0-9]{2}")
  .x$time <- replace_na(.x$time, "00:00:00")
  .x <- .x %>% mutate(date = lubridate::date(datetime))
  return(.x)
})

# 2. Get observed stats ------------------------------------------------------
obs_stats <- map(xys, ~get_stats(data = .x, edgelist = get_edgelist(data = .x, idCol = "indiv", dateCol = "datetime"))) %>% map2(., eta_values, ~.x %>% mutate(eta = .y)) %>% purrr::list_rbind()

# Permutations ------------------------------------------------------------
n <- 100 # while we get the code working
# 3. Random permutations --------------------------------------------------
random <- vector(mode = "list", length = length(eta_values))
for(i in 1:length(eta_values)){
  cat(paste0("Eta = ", eta_values[i], "\n"))
  dat <- xys[[i]]
  setDT(dat)
  realizations <- as.data.frame(randomizations(DT = dat, type = "trajectory", id = "indiv", datetime = "datetime", coords = c("x", "y"), iterations = n)) %>%
    filter(iteration != 0) %>% group_split(iteration, .keep = TRUE)
  random[[i]] <- realizations
}

# 4. Conveyor permutations ------------------------------------------------
conveyor <- vector(mode = "list", length = length(eta_values))
for(i in 1:length(eta_values)){
  cat(paste0("Eta = ", eta_values[i], "\n"))
  dat <- xys[[i]]
  realizations <- vector(mode = "list", length = n)
  for(realization in 1:n){
    cat(paste0("realization ", realization, "\n"))
    realizations[[realization]] <- as.data.frame(rotate_data_table(dataset = dat, idCol = "indiv", dateCol = "date", shiftMax = 5, timeCol = "time"))
  }
  conveyor[[i]] <- realizations
}

# Stats -------------------------------------------------------------------
## define a function to get the stats for this list--each element of `random` or `conveyor` is a list of n iterations corresponding to one of the eta values.
fn <- function(eta_value_list, date){
  out <- vector(mode = "list", length = length(eta_value_list))
  for(i in 1:length(out)){
    dat <- setDT(eta_value_list[[i]])
    out[[i]] <- get_stats(data = dat, edgelist = get_edgelist(data = dat, idCol = "indiv", dateCol = date))
  }
  return(out)
}

rand_stats <- map(random, ~fn(.x, date = "randomdatetime"))
rand_stats <- map(rand_stats, ~purrr::list_rbind(.x, names_to = "iteration"))
rand_stats <- map2(rand_stats, eta_values, ~.x %>% mutate(eta = .y)) %>% purrr::list_rbind()
conv_stats <- map(conveyor, ~fn(.x, date = "newdatetime")) 
conv_stats <- map(conv_stats, ~purrr::list_rbind(.x, names_to = "iteration"))
conv_stats <- map2(conv_stats, eta_values,~.x %>% mutate(eta = .y)) %>% purrr::list_rbind()

stats <- rand_stats %>% mutate(type = "random") %>% bind_rows(conv_stats %>% mutate(type = "conveyor")) %>% bind_rows(obs_stats %>% mutate(iteration = 0, type = "observed"))

# Plotting ----------------------------------------------------------------
stats %>%
  filter(iteration == 0) %>%
  pivot_longer(cols = c("degree", "strength"), names_to = "measure", values_to = "value") %>% 
  ggplot(aes(x = factor(eta), y = value))+
  geom_boxplot(fill = "lightgray")+
  facet_wrap(~measure, scales = "free_y")+
  theme_classic()

stats_means <- stats %>%
  group_by(type, iteration, eta) %>%
  summarize(mn_associations = mean(associations, na.rm = T),
            mn_degree = mean(degree, na.rm = T),
            mn_strength = mean(strength, na.rm = T)) %>%
  ungroup() %>%
  arrange(eta, type, iteration) %>%
  pivot_longer(cols = starts_with("mn"), names_to = "metric", values_to = "value")

stats_means %>%
  filter(type != "observed") %>%
  ggplot(aes(x = factor(eta), y = value, fill = type))+
  facet_wrap(~metric, scales = "free_y")+
  geom_boxplot(position = position_dodge())+
  geom_point(data = stats_means %>% filter(type == "observed"), inherit.aes = F, aes(x = factor(eta), y = value), col = "red", size = 2)+
  theme_classic() + 
  scale_fill_manual(values = c("yellowgreen", "skyblue"))
