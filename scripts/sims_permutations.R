library(tidyverse)
library(tidytext)
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
sm <- 5 # can shift 5 days in either direction, 10 day range total

sims_conveyor <- map(sims_xy, ~{
  realizations <- vector(mode = "list", length = n)
  for(i in 1:n){
    cat(".")
    realizations[[i]] <- rotate_data_table(dataset = .x, shiftMax = sm, 
                                           idCol = "indiv", dateCol = "date", 
                                           timeCol = "time")
  }
  cat("\n")
  return(realizations)
})

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
conveyor_stats <- vector(mode = "list", length = length(sims_conveyor))
for(i in 1:length(sims_conveyor)){
  edges <- map(sims_conveyor[[i]], ~get_edgelist(data = .x, idCol = "indiv", dateCol = "newdatetime"))
  stats <- map2(.x = sims_conveyor[[i]], .y = edges, ~get_stats(edgelist = .y, data = .x)) %>%
    purrr::list_rbind(names_to = "iteration")
  conveyor_stats[[i]] <- stats
}

# Label the stats
conveyor_stats <- map2(conveyor_stats, simulations, ~.x %>% mutate(sim = .y))
conveyor_stats <- map2(conveyor_stats, soc_nonsoc, ~.x %>% mutate(sns = .y))
conveyor_stats_df <- conveyor_stats %>% purrr::list_rbind()
                  
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
stats_perm <- conveyor_stats_df %>% mutate(type = "conveyor") %>%
  bind_rows(random_stats_df %>% mutate(type = "random")) %>%
  mutate(uniquesim = paste(sim, sns, sep = "_"))

# Plots -------------------------------------------------------------------
test <- conveyor_stats_df %>%
  group_by(ID1, sim, sns) %>%
  summarize(mnstr = mean(strength, na.rm = T)) %>%
  ungroup() %>%
  group_by(sim, sns) %>%
  arrange(desc(mnstr), .by_group = T) %>%
  mutate(rank = 1:n()) %>%
  select(-mnstr)

sp_test <- left_join(stats_perm, test, by = c("ID1", "sim", "sns"))
obs_test <- left_join(obs_stats_df, test, by = c("ID1", "sim", "sns")) %>%
  mutate(uniquesim = paste(sim, sns, sep = "_")) %>%
  mutate(uniquesim = as.factor(uniquesim),
         ID1 = reorder_within(ID1, rank, uniquesim))

sp_test %>%
  mutate(uniquesim = paste(sim, sns, sep = "_")) %>%
  mutate(uniquesim = as.factor(uniquesim),
         ID1 = reorder_within(ID1, rank, uniquesim)) %>%
  ggplot()+
  geom_boxplot(aes(x = ID1, y = strength, col = type, fill = type), position = "dodge")+
  #geom_point(data = obs_stats_df, aes(x = ID1, y = strength))+
  facet_wrap(~uniquesim, scales = "free", nrow = 3, ncol = 2)+
  scale_x_reordered()+
  theme_classic()

# 5. Make plot ---------------------------------------------------------------

for(i in 1:length(unique(obs_stats_df$uniquesim))){
  us <- unique(obs_stats_df$uniquesim)[i]
  datobs <- obs_stats_df %>% filter(uniquesim == us)
  datperm <- stats_perm %>% filter(uniquesim == us)
  dat <- bind_rows(datobs, datperm)
  dat %>%
    group_by(ID1, type) %>%
    arrange(desc(degree), .by_group = T) %>%
    
}




ord_deg_s <- obs_stats_s %>% arrange(desc(degree)) %>% pull(ID1)
ord_str_s <- obs_stats_s %>% arrange(desc(strength)) %>% pull(ID1)
ord_deg_ns <- obs_stats_ns %>% arrange(desc(degree)) %>% pull(ID1)
ord_str_ns <- obs_stats_ns %>% arrange(desc(strength)) %>% pull(ID1)

deg_boxplot <- perms_stats %>%
  mutate(ID1 = factor(ID1, levels = ord_deg_s)) %>%
  ggplot()+
  geom_boxplot(aes(x = ID1, y = degree, 
                   col = type, fill = type), position = "dodge")+
  theme_classic()+
  ylab("Degree") + xlab("Ranked agents")+
  scale_color_manual(name = "Permutation type", values = c("yellowgreen", "skyblue")) + 
  scale_fill_manual(name = "Permutation type", values = c("yellowgreen", "skyblue"))+
  geom_point(data = obs_stats %>% mutate(ID1 = factor(ID1, levels = ord_deg_s)), 
             aes(x = ID1, y = degree))+
  facet_wrap(~sociable)+
  theme(text = element_text(size = 16), axis.text.x = element_blank(), axis.ticks.x = element_blank())
deg_boxplot
ggsave(deg_boxplot, file = "fig/degree_boxplot.png", width = 7, height = 4)

str_boxplot <- perms_stats %>%
  mutate(ID1 = factor(ID1, levels = ord_str_s)) %>%
  ggplot()+
  geom_boxplot(aes(x = ID1, y = strength, 
                   col = type, fill = type), position = "dodge")+
  theme_classic()+
  ylab("Strength") + xlab("Ranked agents")+
  scale_color_manual(name = "Permutation type", values = c("yellowgreen", "skyblue")) + 
  scale_fill_manual(name = "Permutation type", values = c("yellowgreen", "skyblue"))+
  geom_point(data = obs_stats %>% mutate(ID1 = factor(ID1, levels = ord_str_s)), 
             aes(x = ID1, y = strength))+
  facet_wrap(~sociable)+
  theme(text = element_text(size = 16), axis.text.x = element_blank(), axis.ticks.x = element_blank())
str_boxplot
ggsave(str_boxplot, file = "fig/strength_boxplot.png", width = 7, height = 4)

