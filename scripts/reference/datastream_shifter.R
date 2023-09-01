library(dplyr)
library(lubridate)
library(data.table)
library(spatsoc)
library(multidplyr)

# loops a single posix around min and max
loop_days <- function(data, min, max, shift){
  # shift <- lubridate::days(shift)
  shift <- 24 * 60 * 60 * shift
  shifted <- data + shift
  shifted <- ifelse(shifted > max, min + shift - difftime(max, data, units="days") - difftime(ceiling_date(max, unit="day"), max, units="hours"), shifted)
  # if(shifted > max)
  #   shifted <- min + shift - difftime(max, data, units="days") - difftime(ceiling_date(max, unit="day"), max, units="hours")
  shifted
}

# gets network graph
get_edgelist <- function(data, idCol, dateCol){
  if(is.data.frame(data))
    data <- data.table::setDT(data)
  timegroup_data <- spatsoc::group_times(data, datetime = dateCol, threshold = "10 minutes") # could be 4 minutes; see Window variable in matlab code
  spatsoc::edge_dist(timegroup_data, threshold = 14, id = idCol, coords = c('x','y'), timegroup = "timegroup", returnDist = FALSE, fillNA = FALSE)
}

# loads data and changes day and step to posix
load_data <- function(filename){
  load(filename)
  simulation_data <- XYind_log2
  start_time <- as.POSIXct("2023-08-11 23:50")  # note simulation data starts on day 1 step 1 so the mindate will be 8-13 00:00
  simulation_data <- simulation_data %>%
    dplyr::mutate(datetime = start_time + lubridate::days(Day) + lubridate::minutes(StepInDay * SAMPLING_INTERVAL)) %>%
    dplyr::select(indiv, x, y, datetime)
  simulation_data
}

rotate_data_table <- function(data, idCol, dateCol, shift=NULL){
  ## SET SEED
  # set.seed(2023)
  data_table <- data.frame(data)
  data_table <- data.table::setDT(data_table)
  if(is.null(shift)){
    data_table[, c("mindate", "maxdate", "sampledShift") := list(min(get(dateCol)),max(get(dateCol)), sample(1:floor(difftime(max(get(dateCol)), min(get(dateCol)), units="days") - 1), 1)), by = get(idCol)]
  } else {
    data_table[, sampledshift := sample(1:shift), by = get(idCol)]
  }
  
  
  # loop_days <- Vectorize(loop_days)
  # data_table[, eval(dateCol) := as.POSIXct(mapply(function(w, x, y, z) loop_days(w, x, y, z), get(dateCol), mindate, maxdate, sampledShift))]
  data_table[, eval(dateCol) := as.POSIXct(loop_days(get(dateCol), mindate, maxdate, sampledShift))]
  data_table[, -c("mindate", "maxdate", "sampledShift")]
  data_table
}

mean_stats <- function(stats){
  stats %>%
    dplyr::summarise(mean_associations = mean(associations), mean_degree = mean(degree), mean_sri = mean(mean_sri), mean_strength =mean(strength))
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
  
  mean_sri_and_strength <- sri_per_edge %>%
    dplyr::group_by(ID1) %>%
    dplyr::summarise(mean_sri = mean(sri),
                     strength = sum(sri, na.rm = T))
  
  stats <- dplyr::inner_join(associations, degree, by = dplyr::join_by(ID1)) %>%
    dplyr::inner_join(., mean_sri_and_strength, by=dplyr::join_by(ID1))
  stats
}

main <- function(){
  sim_data <- load_data("./simdata/6i_10d_socialable.rdata")
  # sim_data <- sim_data %>%
  #   dplyr::filter(indiv == 1)
  realization_data <- data.frame()
  time_to_rotate <- Sys.time()
  for (x in 1:100){
    print(paste("Working on realization", x))

    rotated_data <- rotate_data_table(sim_data, idCol = "indiv", dateCol = "datetime")
    rotated_edgelist <- get_edgelist(rotated_data, idCol = "indiv", dateCol = "datetime")
    stats <- get_stats(rotated_edgelist)
    average_stats <- mean_stats(stats)
    realization_data <- rbind(realization_data, average_stats)
  }
  print(Sys.time() - time_to_rotate)
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
