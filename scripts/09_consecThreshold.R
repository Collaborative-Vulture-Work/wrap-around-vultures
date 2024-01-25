library(tidyverse)
load("data/simulations/sims_xy.Rda") # XXX save this in other script
#load("data/simulations/sms_sims_conveyor.Rda") # this takes forever :(
# Created "conv" lower down as a subset; will load that later
load("data/simulations/sims_random.Rda")
load("data/simulations/obs_stats_df.Rda") # stats without consecThreshold, for comparison (obs)
load("data/simulations/stats_perm.Rda") # stats without consecThreshold, for comparison (conv and rand)
simnames <- unique(obs_stats_df$uniquesim)

# CALCULATE STATS WITH CONSECTHRESHOLD = 2
# Observed ----------------------------------------------------------------
obs <- sims_xy
os <- map(obs, ~get_stats(data = .x, 
                          edgelist = get_edgelist(
                            .x,
                            idCol = "indiv",
                            dateCol = "datetime",
                            consecThreshold = 2),
                          idCol = "indiv"),
          .progress = T) %>%
  purrr::list_rbind(names_to = "sim")
save(os, file = "data/consecThreshold/os.Rda")

# Conveyor ----------------------------------------------------------------
# We only want the 50-day window, so shift = 25
#conv <- sms_sims_conveyor[[length(sms_sims_conveyor)]] # only want the full timeshift range, 25 days x2
#save(conv, file = "data/consecThreshold/conv.Rda")
load("data/consecThreshold/conv.Rda")
cs <- map(conv, ~map(.x, ~{get_stats(data = .x,
                                     edgelist = get_edgelist(
                                       .x,
                                       idCol = "indiv",
                                       dateCol = "newdatetime",
                                       consecThreshold = 2),
                                     idCol = "indiv")}))
save(cs, file = "data/consecThreshold/cs.Rda")
load("data/consecThreshold/cs.Rda")
csdf <- map(cs, ~purrr::list_rbind(.x, names_to = "iteration")) %>%
  map2(., simnames, ~.x %>% mutate(uniquesim = .y)) %>%
  purrr::list_rbind() %>%
  mutate(type = "conveyor")

# Path shuffling ----------------------------------------------------------
# rand <- sims_random
rs <- map(rand, ~map(.x, ~{get_stats(data = .x,
                                     edgelist = get_edgelist(
                                       .x,
                                       idCol = "indiv",
                                       dateCol = "randomdatetime",
                                       consecThreshold = 2),
                                     idCol = "indiv")}))
save(rs, file = "data/consecThreshold/rs.Rda")
load("data/consecThreshold/rs.Rda")
rsdf <- map(rs, ~purrr::list_rbind(.x, names_to = "iteration")) %>%
  map2(., simnames, ~.x %>% mutate(uniquesim = .y)) %>%
  purrr::list_rbind() %>%
  mutate(type = "random")

# JOIN TOGETHER
stats_perm_2 <- bind_rows(csdf, rsdf) %>%
  mutate(consecThreshold = 2)
stats_perm_1 <- stats_perm %>%
  filter(shift == 25 | is.na(shift)) %>%
  select(iteration, ID1, degree, strength, uniquesim, type) %>%
  mutate(consecThreshold = 1)
stats_perm_joined <- bind_rows(stats_perm_1, stats_perm_2)
stats_perm_joined_summ <- stats_perm_joined %>%
  group_by(uniquesim, type, consecThreshold, iteration) %>%
  summarize(mndeg = mean(degree),
            mnstr = mean(strength))

stats_obs_2 <- os %>%
  group_split(sim) %>%
  map2(., simnames, ~.x %>% mutate(uniquesim = .y)) %>%
  purrr::list_rbind() %>%
  mutate(type = "observed",
         consecThreshold = 2) %>%
  select(ID1, degree, strength, uniquesim, type, consecThreshold)

stats_obs_1 <- obs_stats_df %>%
  select(ID1, degree, strength, uniquesim) %>%
  mutate(type = "observed",
         consecThreshold = 1)

stats_obs_joined <- bind_rows(stats_obs_1, stats_obs_2)
stats_obs_joined_summ <- stats_obs_joined %>%
  group_by(uniquesim, type, consecThreshold) %>%
  summarize(mndeg = mean(degree),
            mnstr = mean(strength))

# MAKE PLOTS TO COMPARE

deg_comparison <- stats_perm_joined_summ %>%
  mutate(line = paste(type, consecThreshold)) %>%
  mutate(line = case_when(line == "conveyor 1" ~ "Wrap-around 1",
                          line == "conveyor 2" ~ "Wrap-around 2",
                          line == "random 1" ~ "Path shuffling 1",
                          line == "random 2" ~ "Path shuffling 2"),
         line = factor(line, levels = c("Wrap-around 1", "Wrap-around 2", "Path shuffling 1", "Path shuffling 2"))) %>%
  ggplot(aes(x = mndeg, linetype = line, col = type))+
  geom_density(linewidth = 1)+
  facet_wrap(~uniquesim, ncol = 2, scales = "free")+
  theme_classic()+
  geom_vline(data = stats_obs_joined_summ, 
             aes(xintercept = mndeg, linetype = factor(consecThreshold)),
             linewidth = 1)+
  scale_linetype_manual(name = "Consecutive\n interactions", values = c(2, 1, 2, 1, 2, 1))+
  scale_color_manual(name = "", values = c(continuousColors[2], permutationColors[2]))+
  ylab("")+
  xlab("Population mean degree")+
  theme(strip.text = element_blank(),
        text = element_text(size = 14),
        legend.position = "none")
deg_comparison
ggsave(deg_comparison, file = "fig/deg_comparison.png", width = 7, height = 5)

str_comparison <- stats_perm_joined_summ %>%
  mutate(line = paste(type, consecThreshold)) %>%
  mutate(line = case_when(line == "conveyor 1" ~ "Wrap-around 1",
                          line == "conveyor 2" ~ "Wrap-around 2",
                          line == "random 1" ~ "Path shuffling 1",
                          line == "random 2" ~ "Path shuffling 2"),
         line = factor(line, levels = c("Wrap-around 1", "Wrap-around 2", "Path shuffling 1", "Path shuffling 2"))) %>%
  ggplot(aes(x = mnstr, linetype = line, col = type))+
  geom_density(linewidth = 1)+
  facet_wrap(~uniquesim, ncol = 2, scales = "free")+
  theme_classic()+
  geom_vline(data = stats_obs_joined_summ, 
             aes(xintercept = mnstr, linetype = factor(consecThreshold)),
             linewidth = 1)+
  scale_linetype_manual(name = "Consecutive\n interactions", values = c(2, 1, 2, 1, 2, 1))+
  scale_color_manual(name = "", values = c(continuousColors[2], permutationColors[2]))+
  ylab("")+
  xlab("Population mean strength")+
  theme(strip.text = element_blank(),
        text = element_text(size = 14),
        legend.position = "none")
str_comparison
ggsave(str_comparison, file = "fig/str_comparison.png", width = 7, height = 5)






# Stats with a consecThreshold --------------------------------------------
# Just taking the full time-shift range for conveyor, and the random, and the observed
test <- os %>% filter(sim==3) %>%
  left_join(obs_stats_df %>% filter(uniquesim == "2_ns") %>% select(ID1, degree, strength), by = "ID1") %>%
  filter(strength.x != strength.y)
# for the ones where strength changes, it always increases
test %>% ggplot(aes(x = strength.x, y = strength.y))+geom_point()+geom_abline(slope = 1, intercept = 0)

os %>% filter(sim==2) %>%
  left_join(obs_stats_df %>% filter(uniquesim == "2_ns") %>% select(ID1, degree, strength), by = "ID1") %>%
  mutate(same = strength.x == strength.y) %>%
  ggplot(aes(x = log(strength.x), y = log(strength.y)))+
  geom_point(alpha = 0.5, aes(col = same))+
  geom_abline(slope = 1, intercept = 0)+
  theme_classic() # almost all of the individuals have their strength altered upwards, but only a little bit.



