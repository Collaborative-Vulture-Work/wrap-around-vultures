# Workflow
# Created by Kaija Gahm. Order of steps is adapted from the steps in the main() function that Ryan wrote initially over in datastream_shifter.R.
source("scripts/functions.R") # pulls in all the functions (written by Orr and Ryan)
library(tidyverse) # for data wrangling and plotting
library(spatsoc) # to implement the trajectory randomization method as described in Orr's paper

# 1. Run the simulation to obtain simulated data --------------------------
# RANDOM
sim_data <- simulateAgents(N_indv = 10, DaysToSimulate = 10, Kappa_ind = 3, quiet = T, ToPlot = 0)
str(sim_data, 1) # we end up with a list: file names, and the things to save.
# Save R data:
save(sim_data, file = "data/sim_data.Rda") # XXXK come back to this
# Save Matlab data: (note: this can take a really long time!)
# R.matlab::writeMat(con = paste0("data/", sim_data$matlabName), XY = sim_data$XY, HRCntXY = sim_data$HRCntXY)


# 2. Load the simulated data and extract the xy coords --------------------
load("data/sim_data.Rda")
simulation_data <- sim_data$XY # extract just the XY coords
simulation_data <- fix_times(simulation_data)

# 3. Get permutation realizations -----------------------------------------
n <- 100
# CONVEYOR
realizations_conveyor <- vector(mode = "list", length = n)
for(i in 1:n){
  realizations_conveyor[[i]] <- rotate_data_table(data = simulation_data, idCol = "indiv", dateCol = "datetime")
}
realizations_conveyor <- map(realizations_conveyor, as.data.frame) # turn back to data frame

# RANDOM
# prepare simulation data for randomization method
data.table::setDT(simulation_data)
simulation_data$datetime <- as.POSIXct(simulation_data$datetime)
realizations_random <- randomizations(DT = simulation_data, type = "trajectory", id = "indiv", datetime = "datetime", coords = c("x", "y"), iterations = n) %>%
  filter(iteration != 0) %>% as.data.frame() # remove the original, since the original is `simulation_data`
realizations_random <- realizations_random %>%
  group_split(iteration, .keep = TRUE)

# 4. Get stats ------------------------------------------------------------
obs <- get_stats(data = simulation_data, edgelist = get_edgelist(data = simulation_data, idCol = "indiv", dateCol = "datetime"))
conv_edges <- map(realizations_conveyor, ~get_edgelist(data = .x, idCol = "indiv", dateCol = "datetime"))
conv_stats <- map2(.x = realizations_conveyor, .y = conv_edges, ~get_stats(edgelist = .y, data = .x)) %>%
  purrr::list_rbind(names_to = "iteration")
rand_edges <- map(realizations_random, ~get_edgelist(data = .x, idCol = "indiv", dateCol = "datetime"))
rand_stats <- map2(.x = realizations_random, .y = rand_edges, ~get_stats(edgelist = .y, data = .x)) %>%
  purrr::list_rbind(names_to = "iteration")

perms <- conv %>% mutate(type = "conveyor") %>% bind_rows(rand %>% mutate(type = "random"))

# 5. Make plot ---------------------------------------------------------------
ord <- levels(reorder(perms$ID1, perms$degree, mean, decreasing = T))
obs$ID1 <- factor(obs$ID1, levels = ord)
perms %>%
  ggplot()+
  geom_boxplot(aes(x = reorder(ID1, degree, mean, decreasing = T), y = degree, 
                   col = type, fill = type), position = "dodge")+
  theme_classic()+
  ylab("Degree") + xlab("Ranked agents")+
  scale_color_manual(name = "Permutation type", values = c("lightgreen", "lightblue")) + 
  scale_fill_manual(name = "Permutation type", values = c("lightgreen", "lightblue"))+
  geom_point(data = obs, aes(x = ID1, y = degree))

perms %>%
  ggplot()+
  geom_boxplot(aes(x = reorder(ID1, strength, mean, decreasing = T), y = strength, 
                   col = type, fill = type), position = "dodge")+
  theme_classic()+
  ylab("Strength") + xlab("Ranked agents")+
  scale_color_manual(name = "Permutation type", values = c("lightgreen", "lightblue")) + 
  scale_fill_manual(name = "Permutation type", values = c("lightgreen", "lightblue"))+
  geom_point(data = obs, aes(x = ID1, y = strength))



