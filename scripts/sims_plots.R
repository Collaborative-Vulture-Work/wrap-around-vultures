library(tidyverse)
library(viridis)
load("data/simulations/stats_perm.Rda")
load("data/simulations/obs_stats_df.Rda")

# 5. Make plot ---------------------------------------------------------------
uniquesims <- unique(obs_stats_df$uniquesim)
# 6-panel boxplot: strength
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
    filter(uniquesim == usim,
           shift %in% c(2, NA)) %>%
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
str_6panel <- gridExtra::grid.arrange(gt, left = "Strength", bottom = "Ranked agents")
ggsave(str_6panel, filename = "fig/sims_plots/str_6panel.png", width = 7, height = 6)

# 6-panel boxplot: degree
plots <- vector(mode = "list", length = length(uniquesims))
for(i in 1:length(uniquesims)){
  usim <- unique(obs_stats_df$uniquesim)[i]
  datobs <- obs_stats_df %>% filter(uniquesim == usim) %>% as.data.frame()
  
  # Get the order for the x axis (based on observed), and arrange both observed and simulated to adhere to this order.
  ord <- datobs %>%
    arrange(desc(degree)) %>%
    pull(ID1)
  datobs <- datobs %>%
    mutate(ID1 = factor(ID1, levels = ord))
  datperm <- stats_perm %>%
    filter(uniquesim == usim,
           shift %in% c(2, NA)) %>%
    mutate(ID1 = factor(ID1, levels = ord))
  
  # Make the plot
  p <- datperm %>%
    filter(!is.na(ID1)) %>% # XXX need to deal with NA values for ID1--they didn't have stats calculated for them
    ggplot()+
    geom_boxplot(aes(x = ID1, y = degree, col = type, fill = type), position = position_dodge())+
    theme_classic()+
    geom_point(data = datobs, aes(x = ID1, y = degree), col = "black")+
    theme(axis.ticks.x = element_blank(),
          axis.text.x = element_blank(),
          legend.position = "none",
          axis.title = element_blank())+
    ggtitle(usim)
  plots[[i]] <- p
}

test <- patchwork::wrap_plots(plots[[1]], plots[[2]], plots[[3]], plots[[4]], plots[[5]], plots[[6]], ncol = 2, byrow = T)
gt <- patchwork::patchworkGrob(test)
deg_6panel <- gridExtra::grid.arrange(gt, left = "Degree", bottom = "Ranked agents")
ggsave(deg_6panel, filename = "fig/sims_plots/deg_6panel.png", width = 7, height = 6)

# Histograms: mean value vs. black
summ <- stats_perm %>%
  group_by(uniquesim, type, iteration, shift) %>%
  summarize(mndeg = mean(degree, na.rm = T),
            mnstr = mean(strength, na.rm = T)) %>%
  mutate(shiftprop = (2*shift)/50)
obs_summ <- obs_stats_df %>%
  group_by(uniquesim) %>%
  summarize(mndeg = mean(degree, na.rm = T),
            mnstr = mean(strength, na.rm = T))

# Shift histograms
shifthistrs_str <- summ %>%
  filter(type == "conveyor") %>%
  ggplot(aes(x = mnstr, group = factor(shiftprop)))+
  geom_density(aes(col = shiftprop))+
  facet_wrap(~uniquesim, ncol = 2, nrow = 3, scales = "free")+
  theme_classic()+
  theme(legend.title = element_text("Shiftmax \n(prop. of total dur)"))+
  geom_density(data = summ %>% filter(type == "random"), col = "red")+
  #theme(legend.position = "none")+
  geom_vline(data = obs_summ, aes(xintercept = mnstr), linetype = 2)+
  ylab("")+xlab("Mean strength")
ggsave(shifthistrs_str, filename = "fig/sims_plots/shifhistrs_str.png", width = 9, height = 8)

shifthistrs_deg <- summ %>%
  filter(type == "conveyor") %>%
  ggplot(aes(x = mndeg, group = factor(shiftprop)))+
  geom_density(aes(col = shiftprop))+
  facet_wrap(~uniquesim, ncol = 2, nrow = 3, scales = "free")+
  theme_classic()+
  theme(legend.title = element_text("Shiftmax \n(prop. of total dur)"))+
  geom_density(data = summ %>% filter(type == "random"), col = "red")+
  #theme(legend.position = "none")+
  geom_vline(data = obs_summ, aes(xintercept = mndeg), linetype = 2)+
  ylab("")+xlab("Mean degree")
ggsave(shifthistrs_deg, filename = "fig/sims_plots/shifhistrs_deg.png", width = 9, height = 8)

# Deltas
# Referring back to the histograms: let's look at the difference between each individual's real value and the mean of its values in the permutations. 
glimpse(stats_perm)
deltas_summ <- stats_perm %>%
  group_by(uniquesim, type, shift, ID1) %>%
  summarize(mndeg = mean(degree, na.rm = T),
            sddeg = sd(degree, na.rm = T),
            mnstr = mean(strength, na.rm = T),
            sdstr = sd(strength, na.rm = T)) %>%
  mutate(shiftprop = (shift*2)/50)

deltas <- deltas_summ %>%
  left_join(obs_stats_df %>% select(ID1, degree, strength, uniquesim), by = c("ID1", "uniquesim")) %>%
  mutate(delta_deg_zscore = (degree-mndeg)/sddeg,
         delta_str_zscore = (strength-mnstr)/sdstr)

deltahists_deg <- deltas %>%
  filter(!is.na(shiftprop)) %>% # remove the random-shuffled data
  ggplot(aes(x = delta_deg_zscore, group = factor(shiftprop)))+
  geom_density(aes(col = shiftprop))+
  #scale_color_viridis()+
  facet_wrap(~uniquesim, nrow = 3)+
  theme_classic()+
  geom_vline(aes(xintercept = 0))+
  # add back random-shuffled data
  geom_density(data = deltas %>% filter(is.na(shiftprop)), 
               aes(x = delta_deg_zscore), col = "red")+
  ylab("")+xlab("Observed degree z-score")
ggsave(deltahists_deg, filename = "fig/sims_plots/deltahists_deg.png", width = 6, height = 7)

deltahists_str <- deltas %>%
  filter(!is.na(shiftprop)) %>% # remove the random-shuffled data
  ggplot(aes(x = delta_str_zscore, group = factor(shiftprop)))+
  geom_density(aes(col = shiftprop))+
  #scale_color_viridis()+
  facet_wrap(~uniquesim, nrow = 3, scales = "free")+
  theme_classic()+
  geom_vline(aes(xintercept = 0))+
  # add back random-shuffled data
  geom_density(data = deltas %>% filter(is.na(shiftprop)), aes(x = delta_str_zscore), col = "red")+
  ylab("")+xlab("Obs - mean permuted, Strength")
ggsave(deltahists_str, filename = "fig/sims_plots/deltahists_str.png", width = 6, height = 7)

# Do more emergently-social agents deviate more/less from the permutation means than less emergently-social agents?
# First, let's calculate the rank for the emergent sociality of each individual
ranked <- deltas %>%
  group_by(uniquesim, type, shift) %>%
  arrange(desc(degree), .by_group = T) %>%
  mutate(rank_deg = 1:n()) %>%
  arrange(desc(strength), .by_group = T) %>%
  mutate(rank_str = 1:n())

deg_soc_z <- ranked %>%
  filter(!is.na(shiftprop)) %>% # remove the random-shuffled one
  ggplot(aes(x = rank_deg, y = delta_deg_zscore, group = factor(shiftprop)))+
  geom_smooth(method = "lm", se = F, aes(col = shiftprop))+
  geom_point(aes(col = shiftprop), size = 0.5)+
  theme_classic()+
  geom_point(data = ranked %>% filter(is.na(shiftprop)), aes(x = rank_deg, y = delta_deg_zscore), col = "red", size = 0.5, pch = 4)+
  geom_smooth(data = ranked %>% filter(is.na(shiftprop)), aes(x = rank_deg, y = delta_deg_zscore), col = "red", method = "lm", se = F)+
  facet_wrap(~uniquesim, nrow = 3) +# should *not* set scales to "free"
  ylab("Observed degree z-score") + xlab("Agent rank (degree)")
ggsave(deg_soc_z, filename = "fig/sims_plots/deg_soc_z.png", height = 7, width = 6)

str_soc_z <- ranked %>%
  filter(!is.na(shiftprop)) %>% # remove the random-shuffled one
  ggplot(aes(x = rank_str, y = delta_str_zscore, group = factor(shiftprop)))+
  geom_smooth(method = "lm", se = F, aes(col = shiftprop))+
  geom_point(aes(col = shiftprop), size = 0.5)+
  theme_classic()+
  geom_point(data = ranked %>% filter(is.na(shiftprop)), aes(x = rank_str, y = delta_str_zscore), col = "red", size = 0.5, pch = 4)+
  geom_smooth(data = ranked %>% filter(is.na(shiftprop)), aes(x = rank_str, y = delta_str_zscore), col = "red", method = "lm", se = F)+
  facet_wrap(~uniquesim, nrow = 3) +# should *not* set scales to "free"
  ylab("Observed strength z-score") + xlab("Agent rank (strength)")
ggsave(str_soc_z, filename = "fig/sims_plots/str_soc_z.png", height = 7, width = 6)
