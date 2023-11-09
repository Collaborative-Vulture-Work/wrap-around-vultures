library(tidyverse)
library(viridis)
library(geosphere)
library(lemon)
spds <- c(50, 25, 10, 5)
load("data/simulations/stats_perm_downsampled.Rda")
load("data/simulations/stats_obs_downsampled.Rda")
load("data/simulations/sims_xy.Rda")
load("data/simulations/sims_xy_25spd.Rda")
load("data/simulations/sims_xy_10spd.Rda")
load("data/simulations/sims_xy_5spd.Rda")
source("scripts/00.1_functions.R")

# # Load raw permutation data so we can calculate distances
# # load("data/simulations/conveyor_sms_50spd.Rda")
# # conveyor_sms_50spd_reduced <- conveyor_sms_50spd[c(5, 10, 25)]
# # rm(conveyor_sms_50spd)
# # save(conveyor_sms_50spd_reduced, file = "data/simulations/conveyor_sms_50spd_reduced.Rda")
# # 
# # load("data/simulations/conveyor_sms_25spd.Rda")
# # conveyor_sms_25spd_reduced <- conveyor_sms_25spd[c(5, 10, 25)]
# # rm(conveyor_sms_25spd)
# # save(conveyor_sms_25spd_reduced, file = "data/simulations/conveyor_sms_25spd_reduced.Rda")
# # 
# # load("data/simulations/conveyor_sms_10spd.Rda")
# # conveyor_sms_10spd_reduced <- conveyor_sms_10spd[c(5, 10, 25)]
# # rm(conveyor_sms_10spd)
# # save(conveyor_sms_10spd_reduced, file = "data/simulations/conveyor_sms_10spd_reduced.Rda")
# # 
# # load("data/simulations/conveyor_sms_5spd.Rda")
# # conveyor_sms_5spd_reduced <- conveyor_sms_5spd[c(5, 10, 25)]
# # rm(conveyor_sms_5spd)
# # save(conveyor_sms_5spd_reduced, file = "data/simulations/conveyor_sms_5spd_reduced.Rda")
# 
# # sms_reduced <- c(10)
# # load("data/simulations/conveyor_sms_50spd_reduced.Rda")
# # load("data/simulations/conveyor_sms_25spd_reduced.Rda")
# # load("data/simulations/conveyor_sms_10spd_reduced.Rda")
# # load("data/simulations/conveyor_sms_5spd_reduced.Rda")
# # conveyors <- list(conveyor_sms_50spd_reduced, conveyor_sms_25spd_reduced, conveyor_sms_10spd_reduced, conveyor_sms_5spd_reduced)
# # 
# # load("data/simulations/random_50spd.Rda")
# # load("data/simulations/random_25spd.Rda")
# # load("data/simulations/random_10spd.Rda")
# # load("data/simulations/random_5spd.Rda")
# # randoms <- list(random_50spd, random_25spd, random_10spd, random_5spd)
# 
# # Calculate distances for the random data
# # randomsdistdf <- map(randoms, ~{
# #   spd <- .x
# #   spddf <- map(spd, ~{
# #     sim <- .x
# #     simdf <- purrr::list_rbind(sim)
# #     return(simdf)
# #   }) %>% purrr::list_rbind(names_to = "simulation")
# #   return(spddf)
# # }) %>% purrr::map2(., .y = spds, ~.x %>% mutate(spd = .y)) %>%
# #   purrr::list_rbind() %>%
# #   ungroup()
# 
# ## now assign the distances
# # test <- randomsdistdf %>%
# #   group_by(simulation, indiv, iteration, spd) %>%
# #   arrange(randomdatetime, .by_group = T) %>%
# #   mutate(step = 1:n(),
# #          dist = sqrt((X-lag(X))^2 + (Y-lag(Y))^2)) %>%
# #   ungroup() %>%
# #   mutate(newday = case_when(randomJul-lag(randomJul) == 1 ~ T,
# #                             TRUE ~ F)) # is this or is this not a change in day?
# 
# # save(randomsdistdf, file = "data/simulations/randomsdistdf.Rda")
# 
# # test %>%
# #   filter(iteration %in% 1:10, indiv == 1) %>%
# #   ggplot(aes(x = newday, y = dist, fill = factor(spd)))+geom_boxplot(position = position_dodge())+
# #   facet_wrap(~simulation, nrow = 3, scales = "free") + theme_minimal() # okay, this confirms that the jumps are higher between days vs. within days. As we suspected. (Note: this is just for random--would expect much less deviation for conveyor.)
# 
# ## Now, what proportion of the locations are newday == T, for each spd?
# # test %>%
# #   filter(iteration %in% 1:10) %>%
# #   group_by(simulation, indiv, iteration, spd) %>%
# #   summarize(propnewday = sum(newday ==T)/n()) %>%
# #   ggplot(aes(x = spd, y = propnewday, col  =factor(spd)))+
# #   geom_boxplot()+
# #   facet_wrap(~simulation, nrow = 3) #yeah, okay, this is completely unsurprising and deterministic. We know that for 5spd, exactly 20% of the distance values will be from a new day. And that that proportion will decline from there.
# 
# # so, why do our density plots/histograms look so darn weird?? The boxplot makes it seem like there should be more of a disjunction between the "regular" distance values and the "jumps".
# 
# ## But that's not what we see when we examine a histogram. We can just look at a single iteration and a single individual, iter 1
# # test %>%
# #   filter(iteration %in% 1:100, indiv == 1) %>%
# #   ggplot(aes(x = dist, col = factor(spd)))+geom_density()+facet_wrap(~simulation, scales = "free", nrow = 3)+
# #   theme_minimal()
# 
# ## Just zeroing in on, say, simulation 6 (which we know should have jumps)
# # test %>%
# #   filter(iteration %in% 1:100, indiv == 1, simulation == 6) %>%
# #   ggplot(aes(x = dist, fill = factor(spd), col = factor(spd)))+geom_histogram()+facet_wrap(~spd, scales = "free")+ # is it just because there's way more variance in the high-distance values??
# #   theme_minimal() # still not a disjunction, basically just a continuous histogram. Maybe if we zoom in more? Is there even a categorical distinction necessarily?
# 
# # test %>%
# #   filter(iteration %in% 1:100, indiv == 1, simulation == 6) %>%
# #   ggplot(aes(x = dist, fill = factor(spd), col = factor(spd)))+geom_histogram()+facet_wrap(~spd, scales = "free")+ 
# #   theme_minimal()+
# #   xlim(c(10, 500)) # zooming in on just a portion of the histogram here. It's still continuous, no dip at all.
# 
# # We can confirm this by going back to the boxplots and doing it a little differently
# # test %>%
# #   filter(iteration %in% 1:10, indiv == 1) %>%
# #   ggplot(aes(x = factor(spd), y = dist, fill = factor(newday)))+geom_boxplot(position = position_dodge())+
# #   facet_wrap(~simulation, nrow = 3, scales = "free") + theme_minimal() # Yeah, the distributions for all of these overlap, with no clear disjunction.
# 
# # So, we can't clearly differentiate a "jump" from a "non-jump" except by knowing a priori that they are day transitions. This is true even though the boxplots are very distinct from each other. 
# # 
# # test %>%
# #   mutate(step = step*(50/spd)) %>%
# #   filter(iteration == 1, indiv == 1) %>%
# #   ggplot(aes(x = step, y = dist, col = factor(spd)))+
# #   geom_point(aes(pch = newday))+
# #   facet_wrap(~simulation, nrow = 3)+
# #   theme_minimal()
# # 
# # load("data/simulations/randomsdistdf.Rda")
# # randomsdistdf_reduced <- randomsdistdf %>%
# #   filter(iteration %in% 1:10)
# # save(randomsdistdf_reduced, file = "data/simulations/randomsdistdf_reduced.Rda")
# load("data/simulations/randomsdistdf_reduced.Rda")
# 
# # Calculate distances for the conveyor data
# # conveyorsdistdf <- map(conveyors, ~{
# #   spd <- .x
# #   spddf <- map(spd, ~{
# #     shiftmax <- .x
# #     shiftmaxdf <- map(shiftmax, ~{
# #       sim <- .x
# #       simdf <- purrr::list_rbind(sim, names_to = "iteration")
# #       return(simdf)
# #     }) %>% purrr::list_rbind(., names_to = "simulation")
# #     return(shiftmaxdf)
# #   }) %>% purrr::map2(., .y = sms_reduced, ~.x %>% mutate(shiftmax = .y)) %>%
# #     purrr::list_rbind()
# #   return(spddf)
# # }) %>% purrr::map2(., .y = spds, ~.x %>% mutate(spd = .y)) %>%
# #   purrr::list_rbind() %>%
# #   ungroup() %>%
# #   group_by(simulation, indiv, iteration, spd, shiftmax) %>%
# #   arrange(newdatetime, .by_group = T) %>%
# #   mutate(diffx = X-lag(X),
# #          diffy = Y-lag(Y),
# #          dist = sqrt(diffx^2 + diffy^2)) %>%
# #   ungroup()
# # save(conveyorsdistdf, file = "data/simulations/conveyorsdistdf.Rda")
# # load("data/simulations/conveyorsdistdf.Rda")
# # conveyorsdistdf_reduced <- conveyorsdistdf %>%
# #   filter(iteration %in% 1:10)
# # save(conveyorsdistdf_reduced, file = "data/simulations/conveyorsdistdf_reduced.Rda")
# load("data/simulations/conveyorsdistdf_reduced.Rda")
# 
# # Calculate distances for the observed data
# sims_xy <- sims_xy %>% purrr::list_rbind() %>% ungroup()
# sims_xy_25spd <- sims_xy_25spd %>% purrr::list_rbind() %>% ungroup()
# sims_xy_10spd <- sims_xy_10spd %>% purrr::list_rbind() %>% ungroup()
# sims_xy_5spd <- sims_xy_5spd %>% purrr::list_rbind() %>% ungroup()
# 
# sims <- list(sims_xy, sims_xy_25spd, sims_xy_10spd, sims_xy_5spd)
# simsdf <- map2(sims, spds, ~.x %>% mutate(spd = .y)) %>% purrr::list_rbind()
# simsdf <- simsdf %>%
#   group_by(sim, sns, spd, indiv) %>%
#   mutate(diffx = X-lag(X),
#          diffy = Y-lag(Y),
#          dist = sqrt(diffx^2 + diffy^2)) %>%
#   mutate(uniquesim = paste(sim, sns, sep = "_"))
# 
# # Sanity check: for the observed, make sure the distributions look correct.
# simsdf %>%
#   ggplot(aes(x = dist))+
#   geom_density(aes(col = factor(spd)))+
#   facet_wrap(~uniquesim, nrow = 3, ncol = 2)+
#   theme_minimal()
# 
# # Look at the step distributions for the random-shuffled data
# min <- min(randomsdistdf_reduced$datetime)
# max <- min + days(5)
# testrandom <- randomsdistdf_reduced %>%
#   filter(simulation == 1, iteration %in% 1:10, indiv %in% as.character(1:10))  %>%
#   select(-c(diffx, diffy, datetime, jul, observed, simulation)) %>%
#   filter(!is.na(dist), randomdatetime >= min, randomdatetime <= max) %>%
#   group_by(indiv, iteration, spd) %>%
#   mutate(step = 1:n()) %>%
#   ungroup()
# testrandom %>% filter(iteration ==1, indiv =="1") %>% ggplot(aes(x = step, y = dist))+geom_point(size = 0.5)+facet_wrap(~spd, scales = "free")+
#   theme_minimal()+
#   geom_path()
# 
# testconveyor <- conveyorsdistdf_reduced %>%
#   filter(simulation == 1, iteration %in% 1:10, indiv %in% as.character(1:10))  %>%
#   select(-c(diffx, diffy, datetime, simulation, sim, sns, stepinday, )) %>%
#   filter(!is.na(dist), newdatetime >= min, newdatetime <= max) %>%
#   group_by(indiv, iteration, spd, shiftmax) %>%
#   mutate(step = 1:n())
# testconveyor %>% filter(iteration ==1, indiv =="1") %>% ggplot(aes(x = step, y = dist))+geom_point(size = 0.5)+facet_wrap(~spd, scales = "free")+
#   theme_minimal()+
#   geom_path()
# 
# # This is confusing. Let's just take one single individual and get observed, conveyor (10shiftmax) and random data for it. Choosing individual 1. Just one iteration for random and conveyor.
# ## get the data
# id1_obs <- simsdf %>% filter(indiv == 1)
# id1_con <- conveyorsdistdf_reduced %>%
#   filter(indiv == 1, shiftmax == 10, iteration == 1)
# id1_ran <- randomsdistdf_reduced %>%
#   filter(indiv == 1, iteration == 1)
# 
# ## Make sure it's in different orders.
# head(id1_obs) #datetime is the actual datetime column
# id1_obs <- id1_obs %>%
#   group_by(indiv, uniquesim) %>%
#   arrange(datetime, .by_group = T) %>%
#   mutate(step = 1:n())
# head(id1_con) #newdatetime is the new datetime column
# id1_con$newdatetime
# glimpse(id1_con)
# id1_con <- id1_con %>%
#   group_by(indiv, simulation, iteration, shiftmax, spd) %>%
#   mutate(step = 1:n())
# table(id1_con$step) # good--the steps have been calculated for each of the 6 simulations.
# head(id1_ran) # randomdatetime is the relevant column
# id1_ran <- id1_ran %>%
#   group_by(simulation, indiv, iteration, spd) %>%
#   arrange(randomdatetime, .by_group = T) %>%
#   mutate(step = 1:n())
# table(id1_ran$step) # good--the steps have been calculated for each of the 6 simulations.
# 
# # Okay, now let's make a single ggplot that shows all of these with the distances. 
# # Let's start with just one simulation: the first one. And let's just take the first 100 steps.
# 
# ggplot(data = id1_obs %>% filter(uniquesim == "1_ns", step %in% 1:100))+
#   geom_line(aes(x = step, y =dist))+ theme_minimal()+ # okay, the transition from day 1 to day 2 should happen around step 50. We don't see much difference, which is as it should be.
#   geom_line(data = id1_con %>% filter(simulation == 1, step %in% 1:100, spd == 50), aes(x = step, y = dist), col = "blue")+
#   geom_line(data = id1_ran %>% filter(simulation == 1, step %in% 1:100, spd == 50), aes(x = step, y = dist), col = "red")

# False positive rate vs. jump percentage ---------------------------------
head(stats_obs_downsampled)
head(stats_perm_downsampled)
stats_perm_downsampled <- stats_perm_downsampled %>%
  mutate(uniquesim = paste(sim, sns, sep = "_"))

# Make some plots so we can see what happens to the false positive rate
stats_perm_downsampled %>%
  filter(type == "conveyor", grepl("ns", uniquesim)) %>%
  group_by(uniquesim, shift, spd) %>%
  mutate(mn = mean(degree)) %>%
  ggplot(aes(x = degree, col = shift, group = shift))+
  geom_density()+
  geom_vline(aes(xintercept = mn, col = shift, group = shift))+
  facet_grid(rows = vars(spd), cols = vars(uniquesim), scales = "free_x")+
  theme_minimal()+
  geom_vline(data = stats_obs_downsampled %>%
               filter(sns == "ns") %>%
               group_by(uniquesim, spd) %>%
               summarize(mn = mean(degree)),
             aes(xintercept = mn), linetype = 2, col = "red")

stats_perm_downsampled %>%
  filter(type == "conveyor", grepl("ns", uniquesim)) %>%
  group_by(uniquesim, shift, spd) %>%
  mutate(mn = mean(strength)) %>%
  ggplot(aes(x = strength, col = shift, group = shift))+
  geom_density()+
  geom_vline(aes(xintercept = mn, col = shift, group = shift))+
  facet_grid(rows = vars(spd), cols = vars(uniquesim), scales = "free_x")+
  theme_minimal()+
  geom_vline(data = stats_obs_downsampled %>%
               filter(sns == "ns") %>%
               group_by(uniquesim, spd) %>%
               summarize(mn = mean(strength)),
             aes(xintercept = mn), linetype = 2, col = "red")

# Okay this might show it, but what about the difference...
stats_perm_downsampled %>%
  filter(type == "conveyor", grepl("ns", uniquesim)) %>%
  group_by(uniquesim, shift, spd) %>%
  summarize(mn = mean(degree)) %>%
  ggplot(aes(x = spd, y = mn, col = shift))+
  geom_point()+
  geom_line(aes(group = shift))+
  facet_wrap(~uniquesim, ncol = 1, scales = "free_y")+
  theme_minimal()+
  ylab("Mean degree")+
  xlab("Points per day")+
  geom_point(data = stats_obs_downsampled %>%
               filter(sns == "ns") %>%
               group_by(uniquesim, spd) %>%
               summarize(mn = mean(degree)),
             aes(x = spd, y = mn), col = "red")+
  geom_line(data = stats_obs_downsampled %>%
              filter(sns == "ns") %>%
              group_by(uniquesim, spd) %>%
              summarize(mn = mean(degree)),
            aes(x = spd, y = mn), col = "red", linetype = 2)

stats_perm_downsampled %>%
  filter(type == "conveyor", grepl("ns", uniquesim)) %>%
  group_by(uniquesim, shift, spd) %>%
  summarize(mn = mean(strength)) %>%
  ggplot(aes(x = spd, y = mn, col = shift))+
  geom_point()+
  geom_line(aes(group = shift))+
  facet_wrap(~uniquesim, ncol = 1, scales = "free_y")+
  theme_minimal()+
  ylab("Mean strength")+
  xlab("Points per day")+
  geom_point(data = stats_obs_downsampled %>%
               filter(sns == "ns") %>%
               group_by(uniquesim, spd) %>%
               summarize(mn = mean(strength)),
             aes(x = spd, y = mn), col = "red")+
  geom_line(data = stats_obs_downsampled %>%
              filter(sns == "ns") %>%
              group_by(uniquesim, spd) %>%
              summarize(mn = mean(strength)),
            aes(x = spd, y = mn), col = "red", linetype = 2)

########

perm_means <- stats_perm_downsampled %>%
  group_by(uniquesim, spd, shift, type, iteration) %>%
  summarize(mndeg = mean(degree, na.rm = T),
            mnstr = mean(strength, na.rm = T)) %>%
  ungroup()

obs_means <- stats_obs_downsampled %>%
  group_by(uniquesim, spd) %>%
  summarize(mndeg_obs = mean(degree, na.rm = T),
            mnstr_obs = mean(strength, na.rm = T)) %>%
  ungroup()

means <- left_join(perm_means, obs_means, by = c("uniquesim", "spd"))
nrow(means) == nrow(perm_means)

# Calculate the 2-tailed p-value for each of these shiftMaxes and levels of rarefaction
pvals <- means %>% 
  group_by(uniquesim, spd, shift, type) %>%
  summarize(less_deg = sum(mndeg <= mndeg_obs),
            less_str = sum(mnstr <= mnstr_obs),
            quant_deg = less_deg/n(),
            quant_str = less_str/n()) %>%
  ungroup() %>%
  rowwise() %>% # have to do this in order to calculate the min for each row, instead of the min overall.
  mutate(p2_deg = min(quant_deg, (1-quant_deg))*2,
         p2_str = min(quant_str, (1-quant_str))*2) %>%
  mutate(sampling_frequency = spd/50) %>%
  pivot_longer(cols = c(p2_deg, p2_str), names_to = "measure", values_to = "pval") %>%
  mutate(measure = fct_recode(factor(measure), 
                              "Degree" = "p2_deg",
                              "Strength" = "p2_str")) %>%
  mutate(shiftprop = (shift*2)/50)

ns_deg <- pvals %>% filter(grepl("ns", uniquesim), measure == "Degree")
ns_str <- pvals %>% filter(grepl("ns", uniquesim), measure == "Strength")
ns <- pvals %>% filter(grepl("ns", uniquesim))

pvals_path <- ns %>%
  filter(!is.na(shift)) %>% # first, plotting only conveyor
  ggplot(aes(x = sampling_frequency, y = 1-pval, col = shiftprop, group = factor(shiftprop)))+
  scale_color_gradientn(name = "Shift proportion", colors = continuousColors)+
  #geom_point()+
  geom_path(aes(group = factor(shiftprop)), linewidth = 1.5)+
  facet_rep_grid(rows = vars(uniquesim), cols = vars(measure), scales = "free_y")+
  geom_hline(aes(yintercept = 0.95), col = "gray", lwd = 0.7)+
  theme_minimal()+
  geom_path(data = ns %>% filter(is.na(shift)), aes(x = sampling_frequency, y = 1-pval), col = permutationColors[2], linewidth = 1.5)+
  theme(strip.text.y = element_blank(),
        strip.background.y = element_blank(),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 10),
        legend.title = element_text(size = 12),
        strip.text.x = element_text(size = 12),
        axis.line = element_line(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background.x = element_blank(),
        legend.position = "bottom")+
  ylab("Likelihood to detect a social effect")+
  xlab("Sampling frequency")
pvals_path
ggsave(pvals_path, filename = "fig/sims_plots/pvals_path_ns.png", width = 6, height = 8)


pvals_lm <- ns %>%
  filter(!is.na(shift)) %>% # first, plotting only conveyor
  ggplot(aes(x = sampling_frequency, y = 1-pval, col = shiftprop, group = factor(shiftprop)))+
  scale_color_gradientn(name = "Shift proportion", colors = continuousColors)+
  #geom_point()+
  geom_smooth(method = "lm", se = F, aes(group = factor(shiftprop)), linewidth = 1.5)+
  facet_rep_grid(rows = vars(uniquesim), cols = vars(measure), scales = "free_y")+
  geom_hline(aes(yintercept = 0.95), col = "gray", lwd = 0.7)+
  theme_minimal()+
  geom_smooth(method = "lm", se = F, data = ns %>% filter(is.na(shift)), aes(x = sampling_frequency, y = 1-pval), col = permutationColors[2], linewidth = 1.5)+
  theme(strip.text.y = element_blank(),
        strip.background.y = element_blank(),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 10),
        legend.title = element_text(size = 12),
        strip.text.x = element_text(size = 12),
        axis.line = element_line(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background.x = element_blank(),
        legend.position = "bottom")+
  ylab("Likelihood to detect a social effect")+
  xlab("Sampling frequency")
pvals_lm
ggsave(pvals_lm, filename = "fig/sims_plots/pvals_lm_ns.png", width = 6, height = 8)


