library(tidyverse)
library(tidytext)
library(patchwork)
source("scripts/functions.R")

# Load data ---------------------------------------------------------------
load("data/simulations/sim1_ns.Rda")
load("data/simulations/sim1_s.Rda")
load("data/simulations/sim2_ns.Rda")
load("data/simulations/sim2_s.Rda")
load("data/simulations/sim3_ns.Rda")
load("data/simulations/sim3_s.Rda")

simulations <- rep(1:3, each = 2)
soc_nonsoc <- rep(c("ns", "s"), 3)

# Make a list
sims <- list(sim1_ns, sim1_s, sim2_ns, sim2_s, sim3_ns, sim3_s)

# Extract just the XY coordinates
sims_xy <- map(sims, "XY")

# Fix the times for each one
sims_xy <- map(sims_xy, fix_times)

# Label the simulation data
sims_xy <- map2(sims_xy, simulations, ~.x %>% mutate(sim = .y))
sims_xy <- map2(sims_xy, soc_nonsoc, ~.x %>% mutate(sns = .y))

# Create separate date and time columns
sims_xy <- map(sims_xy, ~.x %>% mutate(date = lubridate::date(datetime),
                                     time = stringr::str_extract(datetime, pattern = "[0-9]{2}\\:[0-9]{2}\\:[0-9]{2}"),
                                     time = tidyr::replace_na(time, "00:00:00")))


# PERMUTATIONS ------------------------------------------------------------
n <- 5 # how many permutations?

# Conveyor permutations ---------------------------------------------------
sms <- seq(from = 1, to = 25, by = 5)

sms_sims_conveyor <- vector(mode = "list", length = length(sms))
for(i in 1:length(sms)){
  shift <- sms[i]
  cat("shiftmax = ", shift, "\n")
  sims_conveyor <- map(sims_xy, ~{
    realizations <- vector(mode = "list", length = n)
    for(j in 1:n){
      cat(".")
      realizations[[j]] <- rotate_data_table(dataset = .x, shiftMax = sm, 
                                             idCol = "indiv", dateCol = "date", 
                                             timeCol = "time")
    }
    cat("\n")
    return(realizations)
  })
  sms_sims_conveyor[[i]] <- sims_conveyor
}


# Random permutations -----------------------------------------------------
sims_xy_dt <- map(sims_xy, setDT) # make them into data tables so we can use the spatsoc function properly
sims_xy_dt <- map(sims_xy_dt, ~.x %>% mutate(datetime = as.POSIXct(datetime)))

sims_random <- map(sims_xy_dt, ~{
  r <- spatsoc::randomizations(DT = .x, type = "trajectory", id = "indiv",
                          datetime = "datetime", coords = c("X", "Y"), 
                          iterations = n) %>%
    filter(iteration != 0) %>% # remove the original data (iteration 0)
    group_by(iteration) %>%
    group_split(.keep = TRUE)
  cat("*")
  return(r)
})


# STATS -------------------------------------------------------------------
# 1. observed simulations
obs_stats <- map(sims_xy, 
                 ~get_stats(data = .x, 
                            edgelist = get_edgelist(.x, idCol = "indiv", 
                                                    dateCol = "datetime")))
# Label the stats
obs_stats <- map2(obs_stats, simulations, ~.x %>% mutate(sim = .y))
obs_stats <- map2(obs_stats, soc_nonsoc, ~.x %>% mutate(sns = .y))
obs_stats_df <- obs_stats %>% purrr::list_rbind() %>%
  mutate(uniquesim = paste(sim, sns, sep = "_"))

# 2. conveyor permutations
sms_conveyor_stats <- vector(mode = "list", length = length(sms))
for(i in 1:length(sms)){
  conveyor_stats <- vector(mode = "list", length = length(sims_conveyor))
  for(j in 1:length(sims_conveyor)){
    edges <- map(sims_conveyor[[j]], ~get_edgelist(data = .x, idCol = "indiv", dateCol = "newdatetime"))
    stats <- map2(.x = sims_conveyor[[j]], .y = edges, ~get_stats(edgelist = .y, data = .x)) %>%
      purrr::list_rbind(names_to = "iteration")
    conveyor_stats[[j]] <- stats
  }
  
  # Label the stats
  conveyor_stats <- map2(conveyor_stats, simulations, ~.x %>% mutate(sim = .y))
  conveyor_stats <- map2(conveyor_stats, soc_nonsoc, ~.x %>% mutate(sns = .y))
  conveyor_stats_df <- conveyor_stats %>% purrr::list_rbind()
  sms_conveyor_stats[[i]] <- conveyor_stats_df
} 

sms_conveyor_stats_df <- sms_conveyor_stats %>% 
  map2(.x = ., .y = sms, ~.x %>% 
         mutate(shiftMax = .y)) %>%
  purrr::list_rbind()
                  
# 3. random permutations
random_stats <- vector(mode = "list", length = length(sims_random))
for(i in 1:length(sims_random)){
  edges <- map(sims_random[[i]], ~get_edgelist(data = .x, idCol = "indiv", dateCol = "randomdatetime"))
  stats <- map2(.x = sims_random[[i]], .y = edges, ~get_stats(edgelist = .y, data = .x)) %>%
    purrr::list_rbind(names_to = "iteration")
  random_stats[[i]] <- stats
}

# Label the stats
random_stats <- map2(random_stats, simulations, ~.x %>% mutate(sim = .y))
random_stats <- map2(random_stats, soc_nonsoc, ~.x %>% mutate(sns = .y))
random_stats_df <- random_stats %>% purrr::list_rbind()

# Combine all the stats
stats_perm <- sms_conveyor_stats_df %>% mutate(type = "conveyor") %>%
  bind_rows(random_stats_df %>% mutate(type = "random")) %>%
  mutate(uniquesim = paste(sim, sns, sep = "_"))

# 5. Make plot ---------------------------------------------------------------
# 6-panel boxplot
uniquesims <- unique(obs_stats_df$uniquesim)
plots <- vector(mode = "list", length = length(uniquesims))
for(i in 1:length(uniquesims)){
  usim <- unique(obs_stats_df$uniquesim)[i]
  datobs <- obs_stats_df %>% filter(uniquesim == usim) %>% as.data.frame()
  
  # Get the order for the x axis (based on observed), and arrange both observed and simulated to adhere to this order.
  ord <- datobs %>%
    arrange(desc(strength)) %>%
    pull(ID1)
  datobs <- datobs %>%
    mutate(ID1 = factor(ID1, levels = ord))
  datperm <- stats_perm %>%
    filter(uniquesim == usim) %>%
    mutate(ID1 = factor(ID1, levels = ord))
  
  # Make the plot
  p <- datperm %>%
    filter(!is.na(ID1)) %>% # XXX need to deal with NA values for ID1--they didn't have stats calculated for them
    ggplot()+
    geom_boxplot(aes(x = ID1, y = strength, col = type, fill = type), position = position_dodge())+
    theme_classic()+
    geom_point(data = datobs, aes(x = ID1, y = strength), col = "black")+
    theme(axis.ticks.x = element_blank(),
          axis.text.x = element_blank(),
          legend.position = "none",
          axis.title = element_blank())+
    ggtitle(usim)
  plots[[i]] <- p
}

test <- patchwork::wrap_plots(plots[[1]], plots[[2]], plots[[3]], plots[[4]], plots[[5]], plots[[6]], ncol = 2, byrow = T)
gt <- patchwork::patchworkGrob(test)
gridExtra::grid.arrange(gt, left = "Strength", bottom = "Ranked agents")

# Histograms: mean value vs. black
summ <- stats_perm %>%
  group_by(uniquesim, type, iteration) %>%
  summarize(mndeg = mean(degree, na.rm = T),
            mnstr = mean(strength, na.rm = T))
obs_summ <- obs_stats_df %>%
  group_by(uniquesim) %>%
  summarize(mndeg = mean(degree, na.rm = T),
            mnstr = mean(strength, na.rm = T))

summ %>%
  ggplot(aes(x = mnstr, col = type, fill = type))+
  geom_histogram()+
  facet_wrap(~uniquesim, ncol = 2, nrow = 3)+
  theme_classic()+
  theme(legend.position = "none")+
  geom_vline(data = obs_summ, aes(xintercept = mnstr), linetype = 2)
