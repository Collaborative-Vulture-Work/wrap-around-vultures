library(tidyverse)
library(viridis)
library(geosphere)
load("data/simulations/stats_perm_downsampled.Rda")
load("data/simulations/stats_obs_downsampled.Rda")
load("data/simulations/sims_xy.Rda")
load("data/simulations/sims_xy_25spd.Rda")
load("data/simulations/sims_xy_10spd.Rda")
load("data/simulations/sims_xy_5spd.Rda")

sims_xy <- sims_xy %>% purrr::list_rbind() %>% ungroup()
sims_xy_25spd <- sims_xy_25spd %>% purrr::list_rbind() %>% ungroup()
sims_xy_10spd <- sims_xy_10spd %>% purrr::list_rbind() %>% ungroup()
sims_xy_5spd <- sims_xy_5spd %>% purrr::list_rbind() %>% ungroup()

spds <- c(50, 25, 10, 5)
sims <- list(sims_xy, sims_xy_25spd, sims_xy_10spd, sims_xy_5spd)
simsdf <- map2(sims, spds, ~.x %>% mutate(spd = .y)) %>% purrr::list_rbind()

simsdf <- simsdf %>%
  group_by(sim, sns, spd, indiv) %>%
  mutate(diffx = X-lag(X),
         diffy = Y-lag(Y),
         dist = sqrt(diffx^2 + diffy^2)) %>%
  mutate(uniquesim = paste(sim, sns, sep = "_"))

# Sanity check: for the observed, make sure the distributions look correct.
simsdf %>%
  ggplot(aes(x = dist))+
  geom_density(aes(col = factor(spd)))+
  facet_wrap(~uniquesim)

# Look at the step distributions for the random-shuffled data

# Look at the step distributions for the conveyor-shuffled data





days <- 50

stats_perm_downsampled <- stats_perm_downsampled %>%
  mutate(uniquesim = paste(sim, sns, sep = "_"))

stats_obs_downsampled <- stats_obs_downsampled %>%
  mutate(uniquesim = paste(sim, sns, sep = "_"))
