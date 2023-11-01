library(tidyverse)
library(viridis)
library(grid)
library(cowplot)
library(patchwork)
library(gridExtra)
source("scripts/00.1_functions.R")
load("data/simulations/stats_perm.Rda")
load("data/simulations/obs_stats_df.Rda")
days <- 50

stats_perm <- stats_perm %>% mutate(type = fct_recode(factor(type),
                         "Shuffled" = "random",
                         "Wrap-around" = "conveyor"))

# 5. Make plot ---------------------------------------------------------------
uniquesims <- unique(obs_stats_df$uniquesim)
whichshift <- 5
# 6-panel boxplot: strength
plots <- vector(mode = "list", length = length(uniquesims))
insets <- vector(mode = "list", length = length(uniquesims))
for(i in 1:length(uniquesims)){
  usim <- unique(obs_stats_df$uniquesim)[i]
  letter <- letters[i]
  datobs <- obs_stats_df %>% filter(uniquesim == usim) %>% as.data.frame()
  
  # Get the order for the x axis (based on observed), and arrange both observed and simulated to adhere to this order.
  ord <- datobs %>%
    arrange(desc(strength)) %>%
    pull(ID1)
  datobs <- datobs %>%
    mutate(ID1 = factor(ID1, levels = ord))
  datperm <- stats_perm %>%
    filter(uniquesim == usim,
           shift %in% c(whichshift, NA)) %>% 
    mutate(ID1 = factor(ID1, levels = ord))
  
  # Make the plot
  p <- datperm %>%
    filter(!is.na(ID1)) %>% # XXX need to deal with NA values for ID1--they didn't have stats calculated for them
    ggplot()+
    geom_boxplot(aes(x = ID1, y = strength, col = type, fill = type), position = position_dodge(), outlier.size = 0.5)+
    theme_classic()+
    geom_point(data = datobs, aes(x = ID1, y = strength), col = "black", size = 2, pch = 1)+
    theme(axis.ticks.x = element_blank(),
          axis.text.x = element_blank(),
          axis.title = element_blank(),
          legend.position = "bottom",
          panel.background = element_rect(fill = "transparent"),
          plot.background = element_rect(fill = "transparent", color = NA)
    )+
    scale_fill_manual(name = "Randomization", values = permutationColors)+
    scale_color_manual(name = "Randomization", values = permutationColors)+
    labs(title = letter)+
    NULL
  plots[[i]] <- p
  
  # Now make the inset
  summ <- datperm %>%
    filter(shift %in% c(whichshift, NA)) %>%
    group_by(iteration, type) %>%
    summarize(mnstr = mean(strength)) %>%
    ungroup()
  obsval <- datobs %>%
    pull(strength) %>%
    mean()
  
  miniplot <- summ %>%
    ggplot(aes(x = mnstr))+
    geom_density(aes(col = type), linewidth = 1)+
    geom_vline(xintercept = obsval, lty = 2, linewidth = 0.5)+
    theme_classic()+
    coord_flip()+
    theme(axis.text.y = element_text(size = 6),
          axis.ticks.x = element_blank(),
          axis.text.x = element_blank(),
          legend.position = "none")+
    {if(i != 6)theme(axis.title.x = element_blank(),
                     axis.title.y = element_blank())}+
    {if(i == 6)ylab("Mean strength")}+
    {if(i == 6)xlab("Frequency")}+
    scale_color_manual(name = "Randomization", values = permutationColors)
  insets[[i]] <- miniplot
}

fulls <- map2(plots, insets, ~.x + theme(legend.position = "none") + patchwork::inset_element(.y, 0.5, 0.5, 1, 1.1, clip = FALSE, on_top = FALSE))

test <- ggpubr::ggarrange(plotlist = fulls, ncol = 2, nrow = 3#, 
                          #common.legend = TRUE, legend = "bottom"
) # Common legend doesn't work if we do it like this. Let's just paste on another one.
legend <- ggpubr::get_legend(plots[[1]])
ggsave(legend, filename = "fig/sims_plots/sims_plots_legend.png", width = 7, height = 6)
str_6panel <- test
ggsave(str_6panel, filename = "fig/sims_plots/str_6panel.png", width = 7, height = 7)

# 6-panel boxplot: degree
plots <- vector(mode = "list", length = length(uniquesims))
insets <- vector(mode = "list", length = length(uniquesims))
for(i in 1:length(uniquesims)){
  usim <- unique(obs_stats_df$uniquesim)[i]
  letter <- letters[i]
  datobs <- obs_stats_df %>% filter(uniquesim == usim) %>% as.data.frame()
  
  # Get the order for the x axis (based on observed), and arrange both observed and simulated to adhere to this order.
  ord <- datobs %>%
    arrange(desc(degree)) %>%
    pull(ID1)
  datobs <- datobs %>%
    mutate(ID1 = factor(ID1, levels = ord))
  datperm <- stats_perm %>%
    filter(uniquesim == usim,
           shift %in% c(whichshift, NA)) %>% 
    mutate(ID1 = factor(ID1, levels = ord))
  
  # Make the plot
  p <- datperm %>%
    filter(!is.na(ID1)) %>% # XXX need to deal with NA values for ID1--they didn't have stats calculated for them
    ggplot()+
    geom_boxplot(aes(x = ID1, y = degree, col = type, fill = type), position = position_dodge(), outlier.size = 0.5)+
    theme_classic()+
    geom_point(data = datobs, aes(x = ID1, y = degree), col = "black", size = 2, pch = 1)+
    theme(axis.ticks.x = element_blank(),
          axis.text.x = element_blank(),
          axis.title = element_blank(),
          legend.position = "bottom",
          panel.background = element_rect(fill = "transparent"),
          plot.background = element_rect(fill = "transparent", color = NA)
    )+
    scale_fill_manual(name = "Randomization", values = permutationColors)+
    scale_color_manual(name = "Randomization", values = permutationColors)+
    labs(title = letter)+
    NULL
  plots[[i]] <- p
  
  # Now make the inset
  summ <- datperm %>%
    filter(shift %in% c(whichshift, NA)) %>%
    group_by(iteration, type) %>%
    summarize(mndeg = mean(degree)) %>%
    ungroup()
  obsval <- datobs %>%
    pull(degree) %>%
    mean()
  
  miniplot <- summ %>%
    ggplot(aes(x = mndeg))+
    geom_density(aes(col = type), linewidth = 1)+
    geom_vline(xintercept = obsval, lty = 2, linewidth = 0.5)+
    theme_classic()+
    coord_flip()+
    theme(axis.text.y = element_text(size = 6),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          legend.position = "none")+
    scale_color_manual(name = "Randomization", values = permutationColors)
  insets[[i]] <- miniplot
}

fulls <- map2(plots, insets, ~.x + theme(legend.position = "none") + patchwork::inset_element(.y, 0.63, 0.7, 1, 1.2, clip = FALSE, on_top = FALSE))

test <- ggpubr::ggarrange(plotlist = fulls, ncol = 2, nrow = 3#, 
                          #common.legend = TRUE, legend = "bottom"
)
deg_6panel <- test
ggsave(deg_6panel, filename = "fig/sims_plots/deg_6panel.png", width = 7, height = 7)

# Histograms: mean value vs. black
summ <- stats_perm %>%
  group_by(uniquesim, type, iteration, shift) %>%
  summarize(mndeg = mean(degree, na.rm = T),
            mnstr = mean(strength, na.rm = T)) %>%
  mutate(shiftprop = (2*shift)/days) %>%
  mutate(letters = case_when(uniquesim == "1_ns" ~ "a",
                             uniquesim == "1_s" ~ "b",
                             uniquesim == "2_ns" ~ "c",
                             uniquesim == "2_s" ~ "d",
                             uniquesim == "3_ns" ~ "e",
                             uniquesim == "3_s" ~ "f",
                             TRUE ~ NA))
obs_summ <- obs_stats_df %>%
  group_by(uniquesim) %>%
  summarize(mndeg = mean(degree, na.rm = T),
            mnstr = mean(strength, na.rm = T)) %>%
  mutate(letters = case_when(uniquesim == "1_ns" ~ "a",
                             uniquesim == "1_s" ~ "b",
                             uniquesim == "2_ns" ~ "c",
                             uniquesim == "2_s" ~ "d",
                             uniquesim == "3_ns" ~ "e",
                             uniquesim == "3_s" ~ "f",
                             TRUE ~ NA))

# Shift histograms
shifthistrs_str <- summ %>%
  filter(type == "Wrap-around") %>%
  ggplot(aes(x = mnstr, group = factor(shiftprop)))+
  geom_density(aes(col = shiftprop), linewidth = 1.2)+
  scale_colour_gradientn(colors = continuousColors)+
  facet_wrap(~letters, ncol = 2, nrow = 3, scales = "free")+
  theme_classic()+
  theme(legend.title = element_text("Shiftmax \n(prop. of total dur)"))+
  geom_density(data = summ %>% filter(type == "Shuffled"), col = permutationColors[2], linewidth = 1.2)+
  theme(legend.position = "bottom",
        strip.background = element_blank(),
        strip.text.x = element_text(hjust = 0, size = 12, face = "bold"),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title = element_text(size = 14),
        panel.spacing.x = unit(1, "lines")
        )+
  geom_vline(data = obs_summ, aes(xintercept = mnstr), linetype = 2)+
  ylab("")+
  xlab("Mean strength")+
  labs(color = "Shift range proportion")
shifthistrs_str
ggsave(shifthistrs_str, filename = "fig/sims_plots/shifhistrs_str.png", width = 6, height = 8)

shifthistrs_deg <- summ %>%
  filter(type == "Wrap-around") %>%
  ggplot(aes(x = mndeg, group = factor(shiftprop)))+
  geom_density(aes(col = shiftprop), linewidth = 1.2)+
  scale_colour_gradientn(colors = continuousColors)+
  facet_wrap(~letters, ncol = 2, nrow = 3, scales = "free")+
  theme_classic()+
  theme(legend.title = element_text("Shiftmax \n(prop. of total dur)"))+
  geom_density(data = summ %>% filter(type == "Shuffled"), col = permutationColors[2], linewidth = 1.2)+
  theme(legend.position = "bottom",
        strip.background = element_blank(),
        strip.text.x = element_text(hjust = 0, size = 12, face = "bold"),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title = element_text(size = 14)
  )+
  geom_vline(data = obs_summ, aes(xintercept = mndeg), linetype = 2)+
  ylab("Frequency")+xlab("Mean degree")+
  labs(color = "Shift range proportion")
shifthistrs_deg
ggsave(shifthistrs_deg, filename = "fig/sims_plots/shifhistrs_deg.png", width = 6, height = 8)

# Make these into a single graph: shiftprop vs. mean mean deg/mean mean str
summsumm <- summ %>%
  group_by(type, uniquesim, shiftprop) %>%
  summarize(mnmndeg = mean(mndeg, na.rm = T),
            mnmnstr = mean(mnstr, na.rm = T)) %>%
  mutate(scenario = substr(uniquesim, 1, 1),
         sns = factor(stringr::str_extract(uniquesim, "[a-z]+")),
         sociable = case_when(sns == "s" ~ "Sociable",
                              sns == "ns" ~ "Non-sociable"))
obs_summsumm <- obs_summ %>%
  mutate(scenario = substr(uniquesim, 1, 1),
         sns = factor(stringr::str_extract(uniquesim, "[a-z]+")),
         sociable = case_when(sns == "s" ~ "Sociable",
                              sns == "ns" ~ "Non-sociable"))
rand_forplot <- summsumm %>% filter(type == "Shuffled") %>% rename("mndeg" = mnmndeg, 
                                                                 "mnstr" = mnmnstr) %>%
  mutate(sociable = case_when(sns == "s" ~ "Sociable",
                              sns == "ns" ~ "Non-sociable"))
horizlines <- bind_rows(obs_summsumm %>% mutate(type = "observed"), rand_forplot)

shiftprop_mnmndeg <- summsumm %>%
  filter(type == "Wrap-around") %>%
  ggplot(aes(x = shiftprop, y = mnmndeg, col = sociable))+
  geom_point(size = 3)+
  geom_path(linewidth = 1)+
  facet_wrap(~scenario, scales = "free", ncol = 1)+
  theme_classic()+
  geom_hline(data = horizlines, aes(yintercept = mndeg, col = sociable, lty = type), linewidth = 1)+
  ylab("Mean mean degree (population)")+xlab("Shift range proportion")+
  scale_color_manual(values = snsColors)+
  scale_linetype_manual(name = "Reference \nlines", values = c(2, 1))+
  guides(linetype = "none")+
  theme(legend.position = "bottom",
        strip.background = element_blank(),
        strip.text = element_blank(),
        legend.title = element_blank(),
        axis.title = element_text(size = 14),
        legend.text = element_text(size = 10),
        axis.text = element_text(size = 12))
shiftprop_mnmndeg
ggsave(shiftprop_mnmndeg, file = "fig/sims_plots/shiftprop_mnmndeg.png", width = 4, height = 10)

shiftprop_mnmnstr <- summsumm %>%
  filter(type == "Wrap-around") %>%
  ggplot(aes(x = shiftprop, y = mnmnstr, col = sociable))+
  geom_point(size = 3)+
  geom_path(linewidth = 1)+
  facet_wrap(~scenario, scales = "free", ncol = 1)+
  theme_classic()+
  geom_hline(data = horizlines, aes(yintercept = mnstr, col = sociable, lty = type), linewidth = 1)+
  ylab("Mean mean strength (population)")+xlab("Shift range proportion")+
  scale_color_manual(values = snsColors)+
  scale_linetype_manual(name = "Reference \nlines", values = c(2, 1))+
  guides(linetype = "none")+
  theme(legend.position = "bottom",
        strip.background = element_blank(),
        strip.text = element_blank(),
        legend.title = element_blank(),
        axis.title = element_text(size = 14),
        legend.text = element_text(size = 10),
        axis.text = element_text(size = 12))
shiftprop_mnmnstr
ggsave(shiftprop_mnmnstr, file = "fig/sims_plots/shiftprop_mnmnstr.png", width = 4, height = 10)
