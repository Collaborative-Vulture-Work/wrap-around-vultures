# Workflow
# Created by Kaija Gahm. Order of steps is adapted from the steps in the main() function that Ryan wrote initially over in datastream_shifter.R.
source("scripts/functions.R") # pulls in all the functions (written by Orr and Ryan)
library(tidyverse) # for data wrangling and plotting
library(spatsoc) # to implement the trajectory randomization method as described in Orr's paper
library(viridis)

# 1. Run the simulation to obtain simulated data --------------------------
# NON-SOCIABLE
sim_data_ns <- simulateAgents(N = 30, Days = 50, Kappa_ind = 3, quiet = T, ToPlot = 0, Soc_Percep_Rng = 0)
str(sim_data_ns, 1) # we end up with a list: file names, and the things to save.
# Save R data:
save(sim_data_ns, file = "data/sim_data_ns.Rda") # XXXK come back to this
# Save Matlab data: (note: this can take a really long time!)
# R.matlab::writeMat(con = paste0("data/", sim_data_ns$matlabName), XY = sim_data_ns$XY, HRCntXY = sim_data_ns$HRCntXY)

# SOCIABLE
sim_data_s <- simulateAgents(N = 30, Days = 50, Kappa_ind = 3, quiet = T, ToPlot = 0, Soc_Percep_Rng = 500, socialWeight = 0.75) #
# Save R data:
save(sim_data_s, file = "data/sim_data_s.Rda")

# tests for HR moving 2023-09-19
# Sim 2: Randomly changing HR's
test <- simulateAgents(N = 6, Days = 6, DayLength = 50, Soc_Percep_Rng = 1000, PairedAgents = 0, PairStartDist = 0, Scl = 1000, seed = NULL, EtaCRW = 0.7, StpSize_ind =9, StpStd_ind = 5, Kappa_ind = 4, ToPlot = 0, quiet = F, sim_3 = T, socialWeight = 0, HRStpSize = 0, HRStpStd = 0, HRChangeRadius = 100)
test$XY %>% ggplot(aes(x = X, y = Y, col = day))+geom_point(alpha = 0.7)+geom_point(data = test$HRCent, pch = 19, size = 5)+facet_wrap(~indiv, scales = "free")+theme_minimal()+theme(legend.position = "none")+scale_color_viridis()


# Sim 3: HR's moving in a line
# Non-sociable
test <- simulateAgents(N = 6, Days = 6, DayLength = 50, 
                       Soc_Percep_Rng = 1000, PairedAgents = 0, 
                       PairStartDist = 0, Scl = 1000, 
                       seed = NULL, EtaCRW = 0.7, 
                       StpSize_ind =9, StpStd_ind = 5, 
                       Kappa_ind = 4, ToPlot = 0, 
                       quiet = F, sim_3 = T, socialWeight = 0, 
                       HRStpSize = 200, HRStpStd = 50, 
                       HRKappa_ind = 4, HREtaCRW = 0.7)
test$XY %>% ggplot(aes(x = X, y = Y, col = day))+
  geom_point(alpha = 0.7)+
  geom_point(data = test$HRCent, pch = 19, size = 5)+
  facet_wrap(~indiv, scales = "free")+
  theme_minimal()+
  theme(legend.position = "none")+
  scale_color_viridis()
#Seems wrong that the agents are so far from their home ranges. Shouldn't they be starting on their first days' home ranges?

# Sociable
test <- simulateAgents(N = 6, Days = 6, DayLength = 50, 
                       Soc_Percep_Rng = 1000, PairedAgents = 0, 
                       PairStartDist = 0, Scl = 1000, 
                       seed = NULL, EtaCRW = 0.7, 
                       StpSize_ind =9, StpStd_ind = 5, 
                       Kappa_ind = 4, ToPlot = 0, 
                       quiet = F, sim_3 = T, socialWeight = 0.3, 
                       HRStpSize = 100, HRStpStd = 50,
                       HRKappa_ind = 4, HREtaCRW = 0.7)
test$XY %>% ggplot(aes(x = X, y = Y, col = day))+geom_point(alpha = 0.7)+geom_point(data = test$HRCent, pch = 19, size = 5)+facet_wrap(~indiv, scales = "free")+theme_minimal()+theme(legend.position = "none")+scale_color_viridis()
test$XY %>% ggplot(aes(x = X, y = Y, col = factor(indiv)))+geom_point(alpha = 0.7)+theme_minimal()+theme(legend.position = "none")
#Seems wrong that the agents are so far from their home ranges. Shouldn't they be starting on their first days' home ranges?

# 2. Load the simulated data and extract the xy coords --------------------
load("data/sim_data_ns.Rda")
sd_ns <- sim_data_ns$XY # extract just the XY coords
sd_ns <- fix_times(sd_ns)
save(sd_ns, file = "data/sd_ns.Rda")

load("data/sim_data_s.Rda")
sd_s <- sim_data_s$XY # extract just the XY coords
sd_s <- fix_times(sd_s)
save(sd_s, file = "data/sd_s.Rda")
ggplot(sd_s, aes(x = X, y = Y, col = indiv, alpha = datetime))+geom_point()+theme_minimal() + theme(legend.position = "none")
ggplot(sd_ns, aes(x = X, y = Y, col = indiv, alpha = datetime))+geom_point()+theme_minimal() + theme(legend.position = "none") # okay, it looks like these individuals are taking the first day and wandering wherever with no bias, and then they start to have a bias toward their home range after that.

# 2.5 create separate date and time columns -------------------------------
# Need this as a precursor for rotate_data_table.
sd_ns$date <- lubridate::date(sd_ns$datetime)
sd_ns$time <- stringr::str_extract(sd_ns$datetime, pattern = "[0-9]{2}\\:[0-9]{2}\\:[0-9]{2}")
sd_ns$time <- replace_na(sd_ns$time, "00:00:00")

sd_s$date <- lubridate::date(sd_s$datetime)
sd_s$time <- stringr::str_extract(sd_s$datetime, pattern = "[0-9]{2}\\:[0-9]{2}\\:[0-9]{2}")
sd_s$time <- replace_na(sd_s$time, "00:00:00")

# 3. Get permutation realizations -----------------------------------------
n <- 50
sm <- 5 # can shift 5 days in either direction, 10 day range total
sm2 <- 10
# CONVEYOR
## Conveyor: NON-SOCIABLE
realizations_conveyor_ns <- vector(mode = "list", length = n)
for(i in 1:n){
  cat(".")
  realizations_conveyor_ns[[i]] <- rotate_data_table(dataset = sd_ns, shiftMax = 5, idCol = "indiv", dateCol = "date", timeCol = "time")
}

realizations_conveyor_ns_2 <- vector(mode = "list", length = n)
for(i in 1:n){
  cat(".")
  realizations_conveyor_ns_2[[i]] <- rotate_data_table(dataset = sd_ns, shiftMax = sm2, idCol = "indiv", dateCol = "date", timeCol = "time")
}

## Conveyor: SOCIABLE
realizations_conveyor_s <- vector(mode = "list", length = n)
for(i in 1:n){
  cat(".")
  realizations_conveyor_s[[i]] <- rotate_data_table(dataset = sd_s, shiftMax = 5, idCol = "indiv", dateCol = "date", timeCol = "time")
}

realizations_conveyor_s_2<- vector(mode = "list", length = n)
for(i in 1:n){
  cat(".")
  realizations_conveyor_s_2[[i]] <- rotate_data_table(dataset = sd_s, shiftMax = sm2, idCol = "indiv", dateCol = "date", timeCol = "time")
}

# RANDOM
## Random: NON-SOCIABLE
data.table::setDT(sd_ns)
sd_ns$datetime <- as.POSIXct(sd_ns$datetime)
realizations_random_ns <- as.data.frame(randomizations(DT = sd_ns, type = "trajectory", id = "indiv", datetime = "datetime", coords = c("X", "Y"), iterations = n)) %>%
  filter(iteration != 0) %>% group_split(iteration, .keep = TRUE)

## Random: SOCIABLE
data.table::setDT(sd_s)
sd_s$datetime <- as.POSIXct(sd_s$datetime)
realizations_random_s <- as.data.frame(randomizations(DT = sd_s, type = "trajectory", id = "indiv", datetime = "datetime", coords = c("X", "Y"), iterations = n)) %>%
  filter(iteration != 0) %>% group_split(iteration, .keep = TRUE)

# 4. Get stats ------------------------------------------------------------
# NON-SOCIABLE
obs_stats_ns <- get_stats(data = sd_ns, edgelist = get_edgelist(data = sd_ns, idCol = "indiv", dateCol = "datetime"))
conv_edges_ns <- map(realizations_conveyor_ns, ~get_edgelist(data = .x, idCol = "indiv", dateCol = "newdatetime"))
conv_stats_ns <- map2(.x = realizations_conveyor_ns, .y = conv_edges_ns, ~get_stats(edgelist = .y, data = .x)) %>%
  purrr::list_rbind(names_to = "iteration")
conv_edges_ns_2 <- map(realizations_conveyor_ns_2, ~get_edgelist(data = .x, idCol = "indiv", dateCol = "newdatetime"))
conv_stats_ns_2 <- map2(.x = realizations_conveyor_ns_2, .y = conv_edges_ns_2, ~get_stats(edgelist = .y, data = .x)) %>%
  purrr::list_rbind(names_to = "iteration")
rand_edges_ns <- map(realizations_random_ns, ~get_edgelist(data = .x, idCol = "indiv", dateCol = "randomdatetime"))
rand_stats_ns <- map2(.x = realizations_random_ns, .y = rand_edges_ns, ~get_stats(edgelist = .y, data = .x)) %>%
  purrr::list_rbind(names_to = "iteration")

perms_stats_ns <- conv_stats_ns %>% mutate(type = "conveyor", shiftMax = sm) %>% bind_rows(conv_stats_ns_2 %>% mutate(type = "conveyor", shiftMax = sm2)) %>% bind_rows(rand_stats_ns %>% mutate(type = "random"))

# SOCIABLE
obs_stats_s <- get_stats(data = sd_s, edgelist = get_edgelist(data = sd_s, idCol = "indiv", dateCol = "datetime"))
conv_edges_s <- map(realizations_conveyor_s, ~get_edgelist(data = .x, idCol = "indiv", dateCol = "newdatetime"))
conv_stats_s <- map2(.x = realizations_conveyor_s, .y = conv_edges_s, ~get_stats(edgelist = .y, data = .x)) %>%
  purrr::list_rbind(names_to = "iteration")
conv_edges_s_2 <- map(realizations_conveyor_s_2, ~get_edgelist(data = .x, idCol = "indiv", dateCol = "newdatetime"))
conv_stats_s_2 <- map2(.x = realizations_conveyor_s_2, .y = conv_edges_s_2, ~get_stats(edgelist = .y, data = .x)) %>%
  purrr::list_rbind(names_to = "iteration")
rand_edges_s <- map(realizations_random_s, ~get_edgelist(data = .x, idCol = "indiv", dateCol = "randomdatetime"))
rand_stats_s <- map2(.x = realizations_random_s, .y = rand_edges_s, ~get_stats(edgelist = .y, data = .x)) %>%
  purrr::list_rbind(names_to = "iteration")

perms_stats_s <- conv_stats_s %>% mutate(type = "conveyor", shiftMax = sm) %>% bind_rows(conv_stats_s_2 %>% mutate(type = "conveyor", shiftMax = sm2)) %>% bind_rows(rand_stats_s %>% mutate(type = "random"))

# TOGETHER
perms_stats <- perms_stats_ns %>% mutate(sociable = "non-sociable") %>% bind_rows(perms_stats_s %>% mutate(sociable = "sociable"))

obs_stats <- obs_stats_ns %>% mutate(sociable = "non-sociable") %>% bind_rows(obs_stats_s %>% mutate(sociable = "sociable"))

# 5. Make plot ---------------------------------------------------------------
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

# Insets: mean values for entire population per realization
obs_mns <- obs_stats %>%
  group_by(sociable) %>%
  summarize(mndeg = mean(degree),
            mnstr = mean(strength))

perm_mns <- perms_stats %>%
  group_by(iteration, sociable, type) %>%
  summarize(mndeg = mean(degree),
            mnstr = mean(strength))

deg_hist <- perm_mns %>%
  ggplot(aes(x = mndeg, col = type, fill = type))+
  geom_histogram(alpha = 0.5, position = "identity")+
  facet_wrap(~sociable, scales = "free")+
  theme_classic()+
  geom_vline(data = obs_mns, aes(xintercept = mndeg), linewidth = 1, linetype = 2)+
  scale_color_manual(name = "Permutation type", values = c("yellowgreen", "skyblue"))+
  scale_fill_manual(name = "Permutation type", values = c("yellowgreen", "skyblue"))+
  ylab("")+ xlab("Mean degree")+ # as expected, we see no difference from either of the permutations for the non-sociable agents, but we do see a difference for the sociable agents. Intriguingly, we also see a difference between the results for the conveyor belt vs. random permutations! Though it's worth noting, that difference goes in the opposite direction I would have expected--I would have thought conveyour would be more similar to observed, not more different.
  theme(text = element_text(size = 16))
ggsave(deg_hist, file = "fig/deg_hist.png", width = 7, height = 4)

str_hist <- perm_mns %>%
  ggplot(aes(x = mnstr, col = type, fill = type))+
  geom_histogram(alpha = 0.5, position = "identity")+
  facet_wrap(~sociable, scales = "free")+
  theme_classic()+
  geom_vline(data = obs_mns, aes(xintercept = mnstr), linewidth = 1, linetype = 2)+
  scale_color_manual(name = "Permutation type", values = c("yellowgreen", "skyblue"))+
  scale_fill_manual(name = "Permutation type", values = c("yellowgreen", "skyblue"))+
  ylab("")+ xlab("Mean strength")+ # X axis is weird on the sociable histogram because the line is far enough over to make everything else really crunched up.
  theme(text = element_text(size = 16))
str_hist
ggsave(str_hist, file = "fig/str_hist.png", width = 7, height = 4)

# As a point of comparison, let's examine just the sociable strength histogram to compare conveyor vs. random, without the line
perm_mns %>%
  filter(sociable == "sociable") %>%
  ggplot(aes(x = mnstr, col = type, fill = type))+
  geom_histogram(alpha = 0.5, position = "identity")+
  theme_classic()+
  scale_color_manual(name = "Permutation type", values = c("yellowgreen", "skyblue"))+
  scale_fill_manual(name = "Permutation type", values = c("yellowgreen", "skyblue"))+
  ylab("")+ xlab("Mean strength")+ # huh, interesting, not as much differentiation here! Conveyor gives a wider range of values than random, and the distribution might be a bit different too (skewed vs. normal)
  theme(text = element_text(size = 16))

# Next figure: x axis = level of sociability. y axis = [obs-random delta], col = permutation type, only sociable. Facets: degree/strength
# For this to work, we need to do permutations with different levels of sociability. I think this is controlled by EtaCRW (and also Kappa, but mostly EtaCRW) 
