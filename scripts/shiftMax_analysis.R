# Sensitivity analysis: random vs. conveyor with different numbers of days
source("scripts/functions.R") # pulls in all the functions (written by Orr and Ryan)
library(tidyverse) # for data wrangling and plotting
library(spatsoc)

load("data/sim_data_s.Rda")
sd_s <- sim_data_s$XY # extract just the XY coords
sd_s <- fix_times(sd_s)
sd_s$date <- lubridate::date(sd_s$datetime)
sd_s$time <- stringr::str_extract(sd_s$datetime, pattern = "[0-9]{2}\\:[0-9]{2}\\:[0-9]{2}")
sd_s$time <- replace_na(sd_s$time, "00:00:00")

# 3. Get permutation realizations -----------------------------------------
n <- 10
shiftMax_values <- seq(from = 1, to = 25, by = 1)
## Conveyor
c_realizations <- vector(mode = "list", length = length(shiftMax_values))
for(i in 1:length(c_realizations)){
  realizations <- vector(mode = "list", length = n)
  for(j in 1:n){
    cat(".")
    realizations[[j]] <- rotate_data_table(dataset = sd_s, shiftMax = shiftMax_values[i], idCol = "indiv", dateCol = "date", timeCol = "time")
  }
  c_realizations[[i]] <- realizations
}

## Random
data.table::setDT(sd_s)
sd_s$datetime <- as.POSIXct(sd_s$datetime)
realizations_random_s <- as.data.frame(randomizations(DT = sd_s, type = "trajectory", id = "indiv", datetime = "datetime", coords = c("x", "y"), iterations = n)) %>%
  filter(iteration != 0) %>% group_split(iteration, .keep = TRUE)
