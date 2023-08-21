library(dplyr)
library(lubridate)
library(data.table)
library(spatsoc)

SAMPLING_INTERVAL <- 10 # "minutes", from matlab code; 10 minutes per timestep with 50 timesteps gives about 8hrs of data

# loops a single posix around min and max
loop_days <- function(data, min, max, shift){
  shifted <- data + shift
  if(shifted > max){ 
    shifted <- min + shift - as.period(difftime(max, data, units="days")) - difftime(ceiling_date(max, unit="day"), max, units="hours") + hours(1)
  }
  shifted
}

# gets network graph
get_edgelist <- function(data){
  timegroup_data <- data.frame(data)
  data.table::setDT(timegroup_data)
  timegroup_data <- spatsoc::group_times(timegroup_data, datetime = "datetime", threshold = "10 minutes")
  spatsoc::edge_dist(timegroup_data, threshold = 14, id = "indiv", coords = c('x','y'), timegroup = "timegroup", returnDist = FALSE, fillNA = FALSE)
}

# loads data and changes day and step to posix
load_data <- function(){
  load('xyFromSimulationForSNanalysis_5000_60_70_7_0_.RData')
  simulation_data <- XYind_log2
  start_time <- as.POSIXct("2023-08-11 23:50")  # note simulation data starts on day 1 step 1 so the mindate will be 8-13 00:00
  simulation_data <- simulation_data %>%
    dplyr::mutate(datetime = start_time + lubridate::days(Day) + lubridate::minutes(StepInDay * SAMPLING_INTERVAL)) %>%
    dplyr::select(indiv, x, y, datetime)
  simulation_data
}

rotate_data <- function(data){
  # shifting forward n same as shifting forward and back n/2 ?
  
  ## SET SEED
  set.seed(2023)
  
  sampled_shift <- data %>%
    dplyr::group_by(indiv) %>%
    dplyr::summarise(mindate = min(datetime), maxdate = max(datetime), sampledShift = lubridate::days(sample(1:floor(difftime(max(datetime), min(datetime), units="days") - 1), 1)))
  
  data <- dplyr::inner_join(data, sampled_shift, by = dplyr::join_by(indiv))
  
  loop_days <- Vectorize(loop_days)
  
  data <- data %>%
    dplyr::mutate(datetime = as.POSIXct(loop_days(datetime, mindate, maxdate, sampledShift)), .by = indiv) %>%
    dplyr::select(indiv, x, y, datetime)
  data
}

main <- function(){
  sim_data <- load_data()
  original_edgelist <- get_edgelist(sim_data)
  # rotated_data <- rotate_data(sim_data)
  load("rotated_data.Rdata")
  rotated_edgelist <- get_edgelist(rotated_data)
  save(original_edgelist, file="original_edgelist.Rdata")
  save(rotated_edgelist, file="rotated_edgelist.Rdata")
}

main()

