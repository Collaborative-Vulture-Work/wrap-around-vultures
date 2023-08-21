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
  # set.seed(2023)
  
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

mean_stats <- function(stats){
  stats %>%
    dplyr::summarise(mean_associations = mean(associations), mean_degree = mean(degree), mean_sri = mean(average_sri))
}

get_stats <- function(edgelist){
  associations <- edgelist %>%
    dplyr::count(ID1) %>%
    dplyr::rename("associations" = "n")
  
  degree <- edgelist %>%
    dplyr::group_by(ID1) %>%
    dplyr::summarise(degree = n_distinct(ID2))
  
  largest_timegroup <- max(edgelist$timegroup)
  
  sri_per_edge <- edgelist %>%
    dplyr::group_by(ID1, ID2) %>%
    dplyr::summarise(sri = n()/largest_timegroup) # this works now, but will need to update it to literally count the number of time groups when both A and B are tracked, for the case when not everyone is tracked in every time group. Denominator should be [total number of timegroups where both individuals exist in the dataset (were tracked), regardless of whether they interact at that timegroup or not.]
  
  average_sri <- sri_per_edge %>%
    dplyr::group_by(ID1) %>%
    dplyr::summarise(average_sri = mean(sri),
                     strength = sum(sri, na.rm = T))
  
  stats <- dplyr::inner_join(associations, degree, by = dplyr::join_by(ID1)) %>% dplyr::inner_join(., average_sri, by=dplyr::join_by(ID1))
  stats
}

main <- function(){
  sim_data <- load_data()
  realization_data <- data.frame()
  for (x in 1:5){
    cat("Working on", x, "realization\n")
    rotated_data <- rotate_data(sim_data)
    rotated_edgelist <- get_edgelist(rotated_data)
    stats <- get_stats(rotated_edgelist)
    average_stats <- mean_stats(stats)
    realization_data <- rbind(realization_data, average_stats)
  }
  save(realization_data, file="realization_data.Rdata")
}
main()

# Compare to observed
load("original_edgelist.Rdata")
mn <- mean_stats(get_stats(original_edgelist))

# Plot
degree <- realization_data %>%
  ggplot(aes(x = mean_degree))+
  geom_histogram()+
  theme_classic()+
  geom_vline(xintercept = mn$mean_degree, linewidth = 1.5, color = "red")+
  xlab("Mean Degree")+ylab("")
ggsave(degree, filename = "fig/degreehist.png")

assoc <- realization_data %>%
  ggplot(aes(x = mean_associations))+
  geom_histogram()+
  theme_classic()+
  geom_vline(xintercept = mn$mean_associations, linewidth = 1.5, color = "red")+
  xlab("Mean # of Associations")+ylab("")
ggsave(assoc, filename = "fig/assochist.png")

edgeweight <- realization_data %>% 
  ggplot(aes(x = mean_sri))+
  geom_histogram()+
  theme_classic()+
  geom_vline(xintercept = mn$mean_sri, linewidth = 1.5, color = "red")+
  xlab("Mean Edge Weight")+ylab("")
ggsave(edgeweight, filename = "fig/edgeweight.png")
# note: this one is a little funky. It's actually the mean *mean* edge weight--took means by individual and then mean across individuals. There's the problem we discussed before with needing to draw the total number of time periods from the data instead of assuming all indivs were present for all time periods, which might be affecting this. But really, we want to do strength, not mean edge weight, here.
