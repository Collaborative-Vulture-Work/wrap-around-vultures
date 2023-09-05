# Workflow
# Created by Kaija Gahm. Order of steps is adapted from the steps in the main() function that Ryan wrote initially over in datastream_shifter.R.
source("scripts/functions.R") # pulls in all the functions (written by Orr and Ryan)
library(tidyverse) # for data wrangling and plotting
library(spatsoc) # to implement the trajectory randomization method as described in Orr's paper

# 1. Run the simulation to obtain simulated data --------------------------
# NON-SOCIABLE
sim_data_ns <- simulateAgents(N_indv = 30, DaysToSimulate = 50, Kappa_ind = 3, quiet = T, ToPlot = 0, Social_Pecrt_rng = 0)
str(sim_data_ns, 1) # we end up with a list: file names, and the things to save.
# Save R data:
save(sim_data_ns, file = "data/sim_data_ns.Rda") # XXXK come back to this
# Save Matlab data: (note: this can take a really long time!)
# R.matlab::writeMat(con = paste0("data/", sim_data_ns$matlabName), XY = sim_data_ns$XY, HRCntXY = sim_data_ns$HRCntXY)

# SOCIABLE
sim_data_s <- simulateAgents(N_indv = 30, DaysToSimulate = 50, Kappa_ind = 3, quiet = T, ToPlot = 0, Social_Pecrt_rng = 2000)
# Save R data:
save(sim_data_s, file = "data/sim_data_s.Rda")

# 2. Load the simulated data and extract the xy coords --------------------
load("data/sim_data_ns.Rda")
sd_ns <- sim_data_ns$XY # extract just the XY coords
sd_ns <- fix_times(sd_ns)

load("data/sim_data_s.Rda")
sd_s <- sim_data_s$XY # extract just the XY coords
sd_s <- fix_times(sd_s)

# 3. Get permutation realizations -----------------------------------------
n <- 100
# CONVEYOR
## Conveyor: NON-SOCIABLE
realizations_conveyor_ns <- vector(mode = "list", length = n)
for(i in 1:n){
  realizations_conveyor_ns[[i]] <- rotate_data_table(data = sd_ns, idCol = "indiv", dateCol = "datetime")
}
realizations_conveyor_ns <- map(realizations_conveyor_ns, as.data.frame) # turn back to data frame

## Conveor: SOCIABLE
realizations_conveyor_s <- vector(mode = "list", length = n)
for(i in 1:n){
  realizations_conveyor_s[[i]] <- rotate_data_table(data = sd_s, idCol = "indiv", dateCol = "datetime")
}
realizations_conveyor_s <- map(realizations_conveyor_s, as.data.frame) # turn back to data frame

# RANDOM
## Random: NON-SOCIABLE
data.table::setDT(sd_ns)
sd_ns$datetime <- as.POSIXct(sd_ns$datetime)
realizations_random_ns <- randomizations(DT = sd_ns, type = "trajectory", id = "indiv", datetime = "datetime", coords = c("x", "y"), iterations = n) %>%
  filter(iteration != 0) %>% as.data.frame() # remove the original, since the original is `simulation_data`
realizations_random_ns <- realizations_random_ns %>%
  group_split(iteration, .keep = TRUE)

## Random: SOCIABLE
data.table::setDT(sd_s)
sd_s$datetime <- as.POSIXct(sd_s$datetime)
realizations_random_s <- randomizations(DT = sd_s, type = "trajectory", id = "indiv", datetime = "datetime", coords = c("x", "y"), iterations = n) %>%
  filter(iteration != 0) %>% as.data.frame() # remove the original, since the original is `simulation_data`
realizations_random_s <- realizations_random_s %>%
  group_split(iteration, .keep = TRUE)

# 4. Get stats ------------------------------------------------------------
# NON-SOCIABLE
obs_stats_ns <- get_stats(data = sd_ns, edgelist = get_edgelist(data = sd_ns, idCol = "indiv", dateCol = "datetime"))
conv_edges_ns <- map(realizations_conveyor_ns, ~get_edgelist(data = .x, idCol = "indiv", dateCol = "datetime"))
conv_stats_ns <- map2(.x = realizations_conveyor_ns, .y = conv_edges_ns, ~get_stats(edgelist = .y, data = .x)) %>%
  purrr::list_rbind(names_to = "iteration")
rand_edges_ns <- map(realizations_random_ns, ~get_edgelist(data = .x, idCol = "indiv", dateCol = "randomdatetime"))
rand_stats_ns <- map2(.x = realizations_random_ns, .y = rand_edges_ns, ~get_stats(edgelist = .y, data = .x)) %>%
  purrr::list_rbind(names_to = "iteration")

perms_stats_ns <- conv_stats_ns %>% mutate(type = "conveyor") %>% bind_rows(rand_stats_ns %>% mutate(type = "random"))

# SOCIABLE
obs_stats_s <- get_stats(data = sd_s, edgelist = get_edgelist(data = sd_s, idCol = "indiv", dateCol = "datetime"))
conv_edges_s <- map(realizations_conveyor_s, ~get_edgelist(data = .x, idCol = "indiv", dateCol = "datetime"))
conv_stats_s <- map2(.x = realizations_conveyor_s, .y = conv_edges_s, ~get_stats(edgelist = .y, data = .x)) %>%
  purrr::list_rbind(names_to = "iteration")
rand_edges_s <- map(realizations_random_s, ~get_edgelist(data = .x, idCol = "indiv", dateCol = "randomdatetime"))
rand_stats_s <- map2(.x = realizations_random_s, .y = rand_edges_s, ~get_stats(edgelist = .y, data = .x)) %>%
  purrr::list_rbind(names_to = "iteration")

perms_stats_s <- conv_stats_s %>% mutate(type = "conveyor") %>% bind_rows(rand_stats_s %>% mutate(type = "random"))

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
  facet_wrap(~sociable)
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
  facet_wrap(~sociable)
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
  ylab("")+ xlab("Mean degree") # as expected, we see no difference from either of the permutations for the non-sociable agents, but we do see a difference for the sociable agents. Intriguingly, we also see a difference between the results for the conveyor belt vs. random permutations! Though it's worth noting, that difference goes in the opposite direction I would have expected--I would have thought conveyour would be more similar to observed, not more different.
ggsave(deg_hist, file = "fig/deg_hist.png", width = 7, height = 4)

str_hist <- perm_mns %>%
  ggplot(aes(x = mnstr, col = type, fill = type))+
  geom_histogram(alpha = 0.5, position = "identity")+
  facet_wrap(~sociable, scales = "free")+
  theme_classic()+
  geom_vline(data = obs_mns, aes(xintercept = mnstr), linewidth = 1, linetype = 2)+
  scale_color_manual(name = "Permutation type", values = c("yellowgreen", "skyblue"))+
  scale_fill_manual(name = "Permutation type", values = c("yellowgreen", "skyblue"))+
  ylab("")+ xlab("Mean strength") # X axis is weird on the sociable histogram because the line is far enough over to make everything else really crunched up.
ggsave(str_hist, file = "fig/str_hist.png", width = 7, height = 4)

# As a point of comparison, let's examine just the sociable strength histogram to compare conveyor vs. random, without the line
perm_mns %>%
  filter(sociable == "sociable") %>%
  ggplot(aes(x = mnstr, col = type, fill = type))+
  geom_histogram(alpha = 0.5, position = "identity")+
  theme_classic()+
  scale_color_manual(name = "Permutation type", values = c("yellowgreen", "skyblue"))+
  scale_fill_manual(name = "Permutation type", values = c("yellowgreen", "skyblue"))+
  ylab("")+ xlab("Mean strength") # huh, interesting, not as much differentiation here! Conveyor gives a wider range of values than random, and the distribution might be a bit different too (skewed vs. normal)

# Next figure: x axis = level of sociability. y axis = [obs-random delta], col = permutation type, only sociable. Facets: degree/strength
# For this to work, we need to do permutations with different levels of sociability. I think this is controlled by kappa. 