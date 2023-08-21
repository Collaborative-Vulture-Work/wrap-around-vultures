library(dplyr)
library(lubridate)
library(data.table)
library(spatsoc)

SAMPLING_INTERVAL <- 10 # "minutes", from matlab code; 10 minutes per timestep with 50 timesteps gives about 8hrs of data

loop_days <- function(data, min, max, shift){
  shifted <- data + shift
  if(shifted > max){
    shifted <- min + (shift - as.period(floor(difftime(max, data, units="days"))))
  }
  shifted
}

load_data <- function(){
  load('xyFromSimulationForSNanalysis_5000_60_70_7_0_.RData')
  simulation_data <- XYind_log2
  start_time <- as.POSIXct("2023-08-12 00:00")  # note simulation data starts on day 1 so the mindate will be 8-13 00:10
  simulation_data <- simulation_data %>%
    dplyr::mutate(datetime = start_time + lubridate::days(Day) + lubridate::minutes(StepInDay * SAMPLING_INTERVAL)) %>%
    dplyr::select(indiv, x, y, datetime)
  # timegroup_data <- data.frame(simulation_data)
  # data.table::setDT(timegroup_data)
  # timegroup_data <- spatsoc::group_times(timegroup_data, datetime = "datetime", threshold = "10 minutes")
  # original_edges <- spatsoc::edge_dist(timegroup_data, threshold = 14, id = "indiv", coords = c('x','y'), timegroup = "timegroup", returnDist = FALSE, fillNA = FALSE)
  
  
  ## graph network graph from simulation
  
  ## run conveyor / randomizations
  
  # shifting forward n same as shifting forward and back n/2 ?
  
  ## SET SEED
  # set.seed(2023)
  
  sampled_shift <- simulation_data %>%
    dplyr::group_by(indiv) %>%
    dplyr::summarise(mindate = min(datetime), maxdate = max(datetime), sampledShift = lubridate::days(sample(1:floor(difftime(max(datetime), min(datetime), units="days") - 1), 1)))
  
  simulation_data <- dplyr::inner_join(simulation_data, sampled_shift, by = dplyr::join_by(indiv))
  
  loop_days <- Vectorize(loop_days)
  
  simulation_data <- simulation_data %>%
    dplyr::mutate(datetime = as.POSIXct(loop_days(datetime, mindate, maxdate, sampledShift)), .by = indiv) %>%
    # dplyr::select(indiv, x, y, datetime) %>%
    dplyr::filter(indiv == 1)
  
  simulation_data
  ## use spatsoc to get network graph again                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               
  
  # shifted_timegroup_data <- data.frame(simulation_data)
  # data.table::setDT(shifted_timegroup_data)
  # shifted_timegroup_data <- spatsoc::group_times(shifted_timegroup_data, datetime = "datetime", threshold = "10 minutes")
  # shifted_edges <- spatsoc::edge_dist(shifted_timegroup_data, threshold = 14, id = "indiv", coords = c('x','y'), timegroup = "timegroup", returnDist = FALSE, fillNA = FALSE)
  
}

data <- load_data()

