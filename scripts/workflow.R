# Workflow
# Created by Kaija Gahm. Order of steps is adapted from the steps in the main() function that Ryan wrote initially over in datastream_shifter.R.
source("scripts/functions.R") # pulls in all the functions (written by Orr and Ryan)
library(tidyverse) # for data wrangling and plotting

# 1. Run the simulation to obtain simulated data --------------------------
sim_data <- simulateAgents(N_indv = 6, DaysToSimulate = 10, Kappa_ind = 3, quiet = T, ToPlot = 0)
str(sim_data, 1) # we end up with a list: file names, and the things to save.
# Save R data:
save(sim_data, file = "data/sim_data.Rda") # XXXK come back to this
# Save Matlab data: (note: this can take a really long time!)
# R.matlab::writeMat(con = paste0("data/", sim_data$matlabName), XY = sim_data$XY, HRCntXY = sim_data$HRCntXY)


# 2. Load the simulated data and extract the xy coords --------------------
load("data/sim_data.Rda")
simulation_data <- sim_data$XY # extract just the XY coords
simulation_data <- fix_times(simulation_data)

# Get permutation realizations --------------------------------------------
realizations <- get_realization_data(simulation_data, n = 100, shift = 5)
