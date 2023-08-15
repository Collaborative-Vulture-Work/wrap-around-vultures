library(dplyr)
library(lubridate)
library(data.table)
library(spatsoc)

# STEP_TO_SECONDS <- 1728 # Because there are 50 steps in a day from the simulation, each step is 1728 seconds

SAMPLING_INTERVAL <- 10 # from matlab code

load_data <- function(){
  load('xyFromSimulationForSNanalysis_5000_60_70_7_0_.RData')
  simulation_data <- XYind_log2
  start_time <- as.POSIXct("2023-08-12 00:00")  
  simulation_data <- simulation_data %>%
    dplyr::mutate(datetime = start_time + lubridate::days(Day) + lubridate::minutes(StepInDay * SAMPLING_INTERVAL)) %>%
    dplyr::select(indiv, x, y, datetime)
  data.table::setDT(simulation_data)
  simulation_data <- spatsoc::group_times(simulation_data, datetime = "datetime", threshold = "10 minutes")
  edges <- spatsoc::edge_dist(simulation_data, threshold = 14, id = "indiv", coords = c('x','y'), timegroup = "timegroup", returnDist = FALSE, fillNA = FALSE)
  edges
  
  ## graph network graph from simulation
  
  ## run conveyor / randomizations
  
  ## use spatsoc to get network graph again
  
}

load_data()

