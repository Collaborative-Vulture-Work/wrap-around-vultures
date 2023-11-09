# Plots for the vulture workflow
library(tidyverse)
library(gespeR) # for rank order comparisons
load("data/vulture_permutations/vulture_stats_obs.Rda")
load("data/vulture_permutations/vulture_stats_perms.Rda")
load("data/vulture_permutations/vulture_stats_perms_2.Rda")
source("scripts/00.1_functions.R")

# Plots -------------------------------------------------------------------
# Get descending order for strength and degree
ord_str <- vulture_stats_obs %>%
  arrange(desc(str)) %>%
  pull(ID1)
ord_deg <- vulture_stats_obs %>%
  arrange(desc(deg)) %>%
  pull(ID1)

obs <- vulture_stats_obs %>%
  mutate(ID1 = factor(ID1, levels = ord_deg, ordered = T))
perm <- vulture_stats_perms %>%
  mutate(ID1 = factor(ID1, levels = ord_deg, ordered = T))
perm2 <- vulture_stats_perms_2 %>%
  mutate(ID1 = factor(ID1, levels = ord_deg, ordered = T))

deg_box_vultures <- perm %>%
  left_join(obs %>% select(ID1, deg), by = "ID1") %>%
  group_by(ID1) %>%
  mutate(deg.y = ifelse(row_number() == 1, deg.y, NA)) %>%
  ggplot(aes(x = ID1))+
  geom_boxplot(aes(y = deg.x, col = type, fill =type), 
               position = position_dodge(preserve = "single"), 
               outlier.size = 0.5)+
  geom_point(aes(y = deg.y), color = "black", pch = 1, size = 2)+
  theme_classic()+
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = "bottom")+
  ylab("Degree") + xlab("Ranked vultures")+
  scale_fill_manual(name = "Permutation type", values = c("#4f7fb3", permutationColors[2]))+
  scale_color_manual(name = "Permutation type", values = c("#4f7fb3", permutationColors[2]))
deg_box_vultures
ggsave(deg_box_vultures, filename = "fig/vulture_permutations_plots/deg_box_vultures.png", width = 9, height = 5)

inset_deg24 <- perm %>% group_by(type, iteration) %>% summarize(mndeg = mean(deg, na.rm = T)) %>%
  ggplot(aes(x = mndeg, col = type))+
  geom_density(linewidth = 1.5)+
  scale_color_manual(values = c("#4f7fb3", permutationColors[2]))+
  theme_classic()+
  theme(legend.position = "none")+
  coord_flip()+
  xlab("Mn. degree")+
  ylab("Frequency")+
  theme(axis.title = element_text(size = 24),
        axis.text = element_text(size = 22))
ggsave(inset_deg24, filename = "fig/vulture_permutations_plots/inset_deg24.png", width = 4, height = 4)

deg_box_vultures_2 <- perm2 %>%
  left_join(obs %>% select(ID1, deg), by = "ID1") %>%
  group_by(ID1) %>%
  mutate(deg.y = ifelse(row_number() == 1, deg.y, NA)) %>%
  ggplot(aes(x = ID1))+
  geom_boxplot(aes(y = deg.x, col = type, fill =type), 
               position = position_dodge(preserve = "single"), 
               outlier.size = 0.5)+
  geom_point(aes(y = deg.y), color = "black", pch = 1, size = 2)+
  theme_classic()+
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = "bottom")+
  ylab("Degree") + xlab("Ranked vultures")+
  scale_fill_manual(name = "Permutation type", values = c("#253d5a", permutationColors[2]))+
  scale_color_manual(name = "Permutation type", values = c("#253d5a", permutationColors[2]))
deg_box_vultures_2
ggsave(deg_box_vultures_2, filename = "fig/vulture_permutations_plots/deg_box_vultures_2.png", width = 9, height = 5)

inset_deg2 <- perm2 %>% group_by(type, iteration) %>% summarize(mndeg = mean(deg, na.rm = T)) %>%
  ggplot(aes(x = mndeg, col = type))+
  geom_density(linewidth = 1.5)+
  scale_color_manual(values = c("#253d5a", permutationColors[2]))+
  theme_classic()+
  theme(legend.position = "none")+
  coord_flip()+
  xlab("Mn. degree")+
  ylab("Frequency")+
  theme(axis.title = element_text(size = 24),
        axis.text = element_text(size = 22))
ggsave(inset_deg2, filename = "fig/vulture_permutations_plots/inset_deg2.png", width = 4, height = 4)

obs <- vulture_stats_obs %>%
  mutate(ID1 = factor(ID1, levels = ord_str, ordered = T))
perm <- vulture_stats_perms %>%
  mutate(ID1 = factor(ID1, levels = ord_str, ordered = T))
perm2 <- vulture_stats_perms_2 %>%
  mutate(ID1 = factor(ID1, levels = ord_str, ordered = T))

str_box_vultures <- perm %>%
  left_join(obs %>% select(ID1, str), by = "ID1") %>%
  group_by(ID1) %>%
  mutate(deg.y = ifelse(row_number() == 1, str.y, NA)) %>%
  ggplot(aes(x = ID1))+
  geom_boxplot(aes(y = str.x, col = type, fill =type), 
               position = position_dodge(preserve = "single"),
               outlier.size = 0.5)+
  geom_point(aes(y = str.y), color = "black", pch = 1, size = 2)+
  theme_classic()+
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = "bottom")+
  ylab("Strength") + xlab("Ranked vultures")+
  scale_fill_manual(name = "Permutation type", values = c("#4f7fb3", permutationColors[2]))+
  scale_color_manual(name = "Permutation type", values = c("#4f7fb3", permutationColors[2]))
str_box_vultures
ggsave(str_box_vultures, filename = "fig/vulture_permutations_plots/str_box_vultures.png", width = 9, height = 5)

inset_str24 <- perm %>% group_by(type, iteration) %>% summarize(mnstr = mean(str, na.rm = T)) %>%
  ggplot(aes(x = mnstr, col = type))+
  geom_density(linewidth = 1.5)+
  scale_color_manual(values = c("#4f7fb3", permutationColors[2]))+
  theme_classic()+
  theme(legend.position = "none")+
  coord_flip()+
  xlab("Mn. strength")+
  ylab("Frequency")+
  theme(axis.title = element_text(size = 24),
        axis.text = element_text(size = 22))
ggsave(inset_str24, filename = "fig/vulture_permutations_plots/inset_str24.png", width = 4, height = 4)

str_box_vultures_2 <- perm2 %>%
  left_join(obs %>% select(ID1, str), by = "ID1") %>%
  group_by(ID1) %>%
  mutate(deg.y = ifelse(row_number() == 1, str.y, NA)) %>%
  ggplot(aes(x = ID1))+
  geom_boxplot(aes(y = str.x, col = type, fill =type), 
               position = position_dodge(preserve = "single"),
               outlier.size = 0.5)+
  geom_point(aes(y = str.y), color = "black", pch = 1, size = 2)+
  theme_classic()+
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = "bottom")+
  ylab("Strength") + xlab("Ranked vultures")+
  scale_fill_manual(name = "Permutation type", values = c("#253d5a", permutationColors[2]))+
  scale_color_manual(name = "Permutation type", values = c("#253d5a", permutationColors[2]))
str_box_vultures_2
ggsave(str_box_vultures_2, filename = "fig/vulture_permutations_plots/str_box_vultures_2.png", width = 9, height = 5)

inset_str2 <- perm2 %>% group_by(type, iteration) %>% summarize(mnstr = mean(str, na.rm = T)) %>%
  ggplot(aes(x = mnstr, col = type))+
  geom_density(linewidth = 1.5)+
  scale_color_manual(values = c("#253d5a", permutationColors[2]))+
  theme_classic()+
  theme(legend.position = "none")+
  coord_flip()+
  xlab("Mn. strength")+
  ylab("Frequency")+
  theme(axis.title = element_text(size = 24),
        axis.text = element_text(size = 22))
ggsave(inset_str2, filename = "fig/vulture_permutations_plots/inset_str2.png", width = 4, height = 4)


box_vultures <- ggpubr::ggarrange(plotlist = list(deg_box_vultures + 
                                                    theme(axis.title.x = element_blank()),
                                                  str_box_vultures), 
                                  ncol = 1, common.legend = TRUE, legend = "bottom",
                                  labels = c("a", "b"))
box_vultures
ggsave(box_vultures, filename = "fig/vulture_permutations_plots/box_vultures.png",
       width = 7, height = 7)

# 10-day shift window for conveyor permutation. 1 season of data used total.
