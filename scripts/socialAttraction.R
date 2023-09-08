# Script for comparing the two methods under different levels of social attraction

# Setup
source("scripts/functions.R") # pulls in all the functions (written by Orr and Ryan)
library(tidyverse) # for data wrangling and plotting
library(spatsoc) # to implement the trajectory randomization method as described in Orr's paper

# Run simulations with different values of EtaCRW
# 1. Run the simulation to obtain simulated data --------------------------
eta_values <- seq(0, 1, by = 0.1) # not sure what the boundary values of EtaCRW are--will need to track down the supporting information and figure this out. XXX maybe task?
#sims_forsocial <- map(eta_values, ~simulateAgents(N_indv = 15, DaysToSimulate = 25, Kappa_ind = 3, quiet = T, ToPlot = 0, Social_Pecrt_rng = 2000))
#save(sims_forsocial, file = "data/sims_forsocial.Rda")
load("data/sims_forsocial.Rda")

# Extract the XY coordinates from each one
xys <- map(sims, "XY") %>% map(., fix_times)

# 2. Get observed stats ------------------------------------------------------
obs_stats <- map(xys, ~get_stats(data = .x, edgelist = get_edgelist(data = .x, idCol = "indiv", dateCol = "datetime"))) %>% map2(., eta_values, ~.x %>% mutate(eta = .y)) %>% purrr::list_rbind()
  
# Permutations ------------------------------------------------------------
n <- 10 # while we get the code working
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

# random edges, random stats

# 4. Conveyor permutations ------------------------------------------------
conveyor <- vector(mode = "list", length = length(eta_values))
for(i in 1:length(eta_values)){
  cat(paste0("Eta = ", eta_values[i], "\n"))
  dat <- xys[[i]]
  realizations <- vector(mode = "list", length = n)
  for(realization in 1:n){
    cat(paste0("realization ", realization, "\n"))
    realizations[[realization]] <- as.data.frame(rotate_data_table(data = dat, idCol = "indiv", dateCol = "datetime"))
  }
}

# conveyor edges, conveyor stats

