# Let's make data for the three different scenarios
# Scenario 1: fixed home ranges
# Scenario 2: randomly-moving home ranges
# Scenario 3: directionally-moving home ranges
source("scripts/00.1_functions.R")
library(tidyverse)
library(viridis)
socLevels <- seq(from = 0, to = 1, by = 0.1)

# SIM 1 -------------------------------------------------------------------
r <- 0.01 # home range centers effectively not moving
baseAgentStep <- 7
HRStpSize <- baseAgentStep*r
HRStpStd <- HRStpSize*0.75 # leaving this here for now--could go back and change later if we want. 
hrk <- 0.01
hre <- 0.7

# sim1_ns <- simulateAgents(N = 30,
#                        Days = 50,
#                        DayLength = 50,
#                        Soc_Percep_Rng = 1000,
#                        PairedAgents = 0,
#                        PairStartDist = 0,
#                        Scl = 1000,
#                        seed = 9252023,
#                        EtaCRW = 0.7,
#                        StpSize_ind = baseAgentStep,
#                        StpStd_ind = 5,
#                        Kappa_ind = 4,
#                        ToPlot = 0,
#                        quiet = T,
#                        sim_3 = F,
#                        socialWeight = 0,
#                        HREtaCRW = 0.7,
#                        HRStpSize = HRStpSize,
#                        HRStpStd = HRStpStd,
#                        HRKappa_ind = hrk)
# save(sim1_ns, file = "data/simulations/sim1_ns.Rda")
load("data/simulations/sim1_ns.Rda")

hr <- sim1_ns$HRCent %>% as.data.frame() %>% mutate(indiv = 1:nrow(.)) %>% rename("X" = V1, "Y" = V2)

ggplot() + 
  geom_point(data = sim1_ns$XY, aes(x = X, y = Y, col = day))+
  geom_point(data = hr, aes(x = X, y = Y), pch = 19, size = 5)+
  facet_wrap(~indiv, scales = "free")+theme_minimal()+
  theme(legend.position = "none", text = element_text(size = 10))+
  scale_color_viridis()+
  ggtitle("Scenario 1, non-sociable")

indivs <- sample(unique(sim1_ns$XY$indiv), 10)
p_s1_ns <- sim1_ns$XY %>% 
  filter(indiv %in% indivs) %>%
  ggplot() +
  geom_path(data = sim1_ns$XY %>% filter(!indiv %in% indivs), 
            aes(x=  X, y = Y, group = indiv), 
            col = "black", linewidth = 0.1, alpha = 0.1)+
  geom_path(aes(x = X, y = Y, col = indiv), 
            linewidth = 1, alpha = 0.9)+
  theme(legend.position = "none", text = element_text(size = 10))+
  scale_color_manual(values = as.character(tencolors))+
  theme_minimal()+
  theme(legend.position = "none")+
  ggtitle("Scenario 1, non-sociable")
#geom_point(data = hr, aes(x = X, y = Y), col = "black", size = 3)
ggsave(p_s1_ns, file = "fig/trajectories/p_s1_ns.png", width = 6, height = 7)

# sim1_socLevels <- map(socLevels, ~{
#   sim <- simulateAgents(N = 30,
#                            Days = 50,
#                            DayLength = 50,
#                            Soc_Percep_Rng = 1000,
#                            PairedAgents = 0,
#                            PairStartDist = 0,
#                            Scl = 1000,
#                            seed = 9252023,
#                            EtaCRW = 0.7,
#                            StpSize_ind = baseAgentStep,
#                            StpStd_ind = 5,
#                            Kappa_ind = 4,
#                            ToPlot = 0,
#                            quiet = T,
#                            sim_3 = F,
#                            socialWeight = .x,
#                            HREtaCRW = 0.7,
#                            HRStpSize = HRStpSize,
#                            HRStpStd = HRStpStd,
#                            HRKappa_ind = hrk)
#   return(sim)
# })
# save(sim1_socLevels, file = "data/simulations/sim1_socLevels.Rda")

sim1_s <- simulateAgents(N = 30,
                          Days = 50,
                          DayLength = 50,
                          Soc_Percep_Rng = 1000,
                          PairedAgents = 0,
                          PairStartDist = 0,
                          Scl = 1000,
                          seed = 9252023,
                          EtaCRW = 0.7,
                          StpSize_ind = baseAgentStep,
                          StpStd_ind = 5,
                          Kappa_ind = 4,
                          ToPlot = 0,
                          quiet = T,
                          sim_3 = F,
                          socialWeight = 0.75,
                          HREtaCRW = 0.7,
                          HRStpSize = HRStpSize,
                          HRStpStd = HRStpStd,
                          HRKappa_ind = hrk)
save(sim1_s, file = "data/simulations/sim1_s.Rda")
load("data/simulations/sim1_s.Rda")

hr <- sim1_s$HRCent %>% as.data.frame() %>% mutate(indiv = 1:nrow(.)) %>% rename("X" = V1, "Y" = V2)

ggplot() + 
  geom_point(data = sim1_s$XY, aes(x = X, y = Y, col = day))+
  geom_point(data = hr, aes(x = X, y = Y), pch = 19, size = 5)+
  facet_wrap(~indiv, scales = "free")+theme_minimal()+
  theme(legend.position = "none", text = element_text(size = 10))+
  scale_color_viridis()+
  ggtitle("Scenario 1, sociable")

indivs <- sample(unique(sim1_s$XY$indiv), 10)
p_s1_s <- sim1_s$XY %>%
  filter(indiv %in% indivs) %>%
  ggplot()+
  geom_path(data = sim1_s$XY %>% filter(!indiv %in% indivs), 
            aes(x=  X, y = Y, group = indiv), 
            col = "black", linewidth = 0.1, alpha = 0.1)+
  geom_path(aes(x = X, y = Y, col = indiv),
            linewidth = 1, alpha = 0.9) +
  theme(legend.position = "none", text = element_text(size = 10))+
  scale_color_manual(values = as.character(tencolors))+
  theme_minimal()+
  theme(legend.position = "none")+
  ggtitle("Scenario 1, sociable")
#geom_point(data = hr, aes(x = X, y = Y), col = "black", size = 3)
ggsave(p_s1_s, file = "fig/trajectories/p_s1_s.png", width = 6, height = 7)

# SIM 2 -------------------------------------------------------------------
r <- 10 # home range steps are 10x the size of agent steps
baseAgentStep <- 7
HRStpSize <- baseAgentStep*r
HRStpStd <- HRStpSize*0.75 # leaving this here for now--could go back and change later if we want. 
hrk <- 0.01 # effectively k = 0, random direction for home range movement.
hre <- 0.7

# sim2_ns <- simulateAgents(N = 30,
#                           Days = 50,
#                           DayLength = 50,
#                           Soc_Percep_Rng = 1000,
#                           PairedAgents = 0,
#                           PairStartDist = 0,
#                           Scl = 1000,
#                           seed = 9252023,
#                           EtaCRW = 0.7,
#                           StpSize_ind = baseAgentStep,
#                           StpStd_ind = 5,
#                           Kappa_ind = 4,
#                           ToPlot = 0,
#                           quiet = T,
#                           sim_3 = T,
#                           socialWeight = 0,
#                           HREtaCRW = 0.7,
#                           HRStpSize = HRStpSize,
#                           HRStpStd = HRStpStd,
#                           HRKappa_ind = hrk)
# save(sim2_ns, file = "data/simulations/sim2_ns.Rda")
load("data/simulations/sim2_ns.Rda")

ggplot() + 
  geom_point(data = sim2_ns$XY, aes(x = X, y = Y, col = day))+
  geom_point(data = sim2_ns$HRCent, aes(x = X, y = Y, col = day), 
             pch = 19, size = 5)+
  facet_wrap(~indiv, scales = "free")+theme_minimal()+
  theme(legend.position = "none", text = element_text(size = 10))+
  scale_color_viridis()+
  ggtitle("Scenario 2, non-sociable")

indivs <- sample(unique(sim2_ns$XY$indiv), 10)
p_s2_ns <- sim2_ns$XY %>%
  filter(indiv %in% indivs) %>%
  ggplot()+
  geom_path(data = sim2_ns$XY %>% filter(!indiv %in% indivs), 
            aes(x=  X, y = Y, group = indiv), 
            col = "black", linewidth = 0.1, alpha = 0.1)+
  geom_path(aes(x = X, y = Y, col = indiv),
            linewidth = 1, alpha = 0.9) +
  theme(legend.position = "none", text = element_text(size = 10))+
  scale_color_manual(values = as.character(tencolors))+
  theme_minimal()+
  theme(legend.position = "none")+
  ggtitle("Scenario 2, non-sociable")
ggsave(p_s2_ns, file = "fig/trajectories/p_s2_ns.png", width = 6, height = 7)

# sim2_socLevels <- map(socLevels, ~{
#   sim <- simulateAgents(N = 30,
#                  Days = 50,
#                  DayLength = 50,
#                  Soc_Percep_Rng = 1000,
#                  PairedAgents = 0,
#                  PairStartDist = 0,
#                  Scl = 1000,
#                  seed = 9252023,
#                  EtaCRW = 0.7,
#                  StpSize_ind = baseAgentStep,
#                  StpStd_ind = 5,
#                  Kappa_ind = 4,
#                  ToPlot = 0,
#                  quiet = T,
#                  sim_3 = T,
#                  socialWeight = .x,
#                  HREtaCRW = 0.7,
#                  HRStpSize = HRStpSize,
#                  HRStpStd = HRStpStd,
#                  HRKappa_ind = hrk)
#   return(sim)
# })
# save(sim2_socLevels, file = "data/simulations/sim2_socLevels.Rda")

sim2_s <- simulateAgents(N = 30,
                          Days = 50,
                          DayLength = 50,
                          Soc_Percep_Rng = 1000,
                          PairedAgents = 0,
                          PairStartDist = 0,
                          Scl = 1000,
                          seed = 9252023,
                          EtaCRW = 0.7,
                          StpSize_ind = baseAgentStep,
                          StpStd_ind = 5,
                          Kappa_ind = 4,
                          ToPlot = 0,
                          quiet = T,
                          sim_3 = T,
                          socialWeight = 0.75,
                          HREtaCRW = 0.7,
                          HRStpSize = HRStpSize,
                          HRStpStd = HRStpStd,
                          HRKappa_ind = hrk)
save(sim2_s, file = "data/simulations/sim2_s.Rda")
load("data/simulations/sim2_s.Rda")

ggplot() + 
  geom_point(data = sim2_s$XY, aes(x = X, y = Y, col = day))+
  geom_point(data = sim2_s$HRCent, aes(x = X, y = Y, col = day), 
             pch = 19, size = 5)+
  facet_wrap(~indiv, scales = "free")+theme_minimal()+
  theme(legend.position = "none", text = element_text(size = 10))+
  scale_color_viridis()+
  ggtitle("Scenario 2, sociable")

indivs <- sample(unique(sim2_s$XY$indiv), 10)
p_s2_s <- sim2_s$XY %>%
  filter(indiv %in% indivs) %>%
  ggplot()+
  geom_path(data = sim2_s$XY %>% filter(!indiv %in% indivs), 
            aes(x=  X, y = Y, group = indiv), 
            col = "black", linewidth = 0.1, alpha = 0.1)+
  geom_path(aes(x = X, y = Y, col = indiv),
            linewidth = 1, alpha = 0.9) +
  theme(legend.position = "none", text = element_text(size = 10))+
  scale_color_manual(values = as.character(tencolors))+
  theme_minimal()+
  theme(legend.position = "none")+
  ggtitle("Scenario 2, sociable")
ggsave(p_s2_s, file = "fig/trajectories/p_s2_s.png", width = 6, height = 7)

# SIM 3 -------------------------------------------------------------------
r <- 10 # home range steps are 10x the size of agent steps
baseAgentStep <- 7
HRStpSize <- baseAgentStep*r
HRStpStd <- HRStpSize*0.75 # leaving this here for now--could go back and change later if we want. 
hrk <- 20 # k = 20, highly directional
hre <- 0.7

# sim3_ns <- simulateAgents(N = 30,
#                           Days = 50,
#                           DayLength = 50,
#                           Soc_Percep_Rng = 1000,
#                           PairedAgents = 0,
#                           PairStartDist = 0,
#                           Scl = 1000,
#                           seed = 9252023,
#                           EtaCRW = 0.7,
#                           StpSize_ind = baseAgentStep,
#                           StpStd_ind = 5,
#                           Kappa_ind = 4,
#                           ToPlot = 0,
#                           quiet = T,
#                           sim_3 = T,
#                           socialWeight = 0,
#                           HREtaCRW = 0.7,
#                           HRStpSize = HRStpSize,
#                           HRStpStd = HRStpStd,
#                           HRKappa_ind = hrk)
# save(sim3_ns, file = "data/simulations/sim3_ns.Rda")
load("data/simulations/sim3_ns.Rda")

ggplot() + 
  geom_point(data = sim3_ns$XY, aes(x = X, y = Y, col = day))+
  geom_point(data = sim3_ns$HRCent, aes(x = X, y = Y, col = day), 
             pch = 19, size = 5)+
  facet_wrap(~indiv, scales = "free")+theme_minimal()+
  theme(legend.position = "none", text = element_text(size = 10))+
  scale_color_viridis()+
  ggtitle("Scenario 3, non-sociable")

indivs <- sample(unique(sim3_ns$XY$indiv), 10)
p_s3_ns <- sim3_ns$XY %>%
  filter(indiv %in% indivs) %>%
  ggplot()+
  geom_path(data = sim3_ns$XY %>% filter(!indiv %in% indivs), 
            aes(x=  X, y = Y, group = indiv), 
            col = "black", linewidth = 0.1, alpha = 0.1)+
  geom_path(aes(x = X, y = Y, col = indiv),
            linewidth = 1, alpha = 0.9) +
  theme(legend.position = "none", text = element_text(size = 10))+
  scale_color_manual(values = as.character(tencolors))+
  theme_minimal()+
  theme(legend.position = "none")+
  ggtitle("Scenario 3, non-sociable")
ggsave(p_s3_ns, file = "fig/trajectories/p_s3_ns.png", width = 7, height = 6)

# sim3_socLevels <- map(socLevels, ~{
#   sim3 <- simulateAgents(N = 30,
#                            Days = 50,
#                            DayLength = 50,
#                            Soc_Percep_Rng = 1000,
#                            PairedAgents = 0,
#                            PairStartDist = 0,
#                            Scl = 1000,
#                            seed = 9252023,
#                            EtaCRW = 0.7,
#                            StpSize_ind = baseAgentStep,
#                            StpStd_ind = 5,
#                            Kappa_ind = 4,
#                            ToPlot = 0,
#                            quiet = T,
#                            sim_3 = T,
#                            socialWeight = .x,
#                            HREtaCRW = 0.7,
#                            HRStpSize = HRStpSize,
#                            HRStpStd = HRStpStd,
#                            HRKappa_ind = hrk)
#   return(sim)
# })
# save(sim3_socLevels, file = "data/simulations/sim3_socLevels.Rda")

sim3_s <- simulateAgents(N = 30,
                         Days = 50,
                         DayLength = 50,
                         Soc_Percep_Rng = 1000,
                         PairedAgents = 0,
                         PairStartDist = 0,
                         Scl = 1000,
                         seed = 9252023,
                         EtaCRW = 0.7,
                         StpSize_ind = baseAgentStep,
                         StpStd_ind = 5,
                         Kappa_ind = 4,
                         ToPlot = 0,
                         quiet = T,
                         sim_3 = T,
                         socialWeight = 0.75,
                         HREtaCRW = 0.7,
                         HRStpSize = HRStpSize,
                         HRStpStd = HRStpStd,
                         HRKappa_ind = hrk)
save(sim3_s, file = "data/simulations/sim3_s.Rda")
load("data/simulations/sim3_s.Rda")

ggplot() + 
  geom_point(data = sim3_s$XY, aes(x = X, y = Y, col = day))+
  #geom_point(data = sim3_s$HRCent, aes(x = X, y = Y, col = day), pch = 19, size = 5)+
  facet_wrap(~indiv, scales = "free")+theme_minimal()+
  theme(legend.position = "none", text = element_text(size = 10))+
  scale_color_viridis()+
  ggtitle("Scenario 3, sociable")

indivs <- sample(unique(sim3_s$XY$indiv), 10)
p_s3_s <- sim3_s$XY %>%
  filter(indiv %in% indivs) %>%
  ggplot()+
  geom_path(data = sim3_s$XY %>% filter(!indiv %in% indivs), 
            aes(x=  X, y = Y, group = indiv), 
            col = "black", linewidth = 0.1, alpha = 0.1)+
  geom_path(aes(x = X, y = Y, col = indiv),
            linewidth = 1, alpha = 0.9) +
  theme(legend.position = "none", text = element_text(size = 10))+
  scale_color_manual(values = as.character(tencolors))+
  theme_minimal()+
  theme(legend.position = "none")+
  ggtitle("Scenario 3, sociable")
ggsave(p_s3_s, file = "fig/trajectories/p_s3_s.png", width = 7, height = 6)

# Put all 6 plots together, removing their titles
a <- p_s1_ns + theme(title = element_blank())
b <- p_s1_s + theme(title = element_blank())
c <- p_s2_ns + theme(title = element_blank())
d <- p_s2_s + theme(title = element_blank())
e <- p_s3_ns + theme(title = element_blank())
f <- p_s3_s + theme(title = element_blank())
trajectories_patchwork <- ((a+b)/(c+d)/(e+f))+
  theme(text = element_text('mono', size = 15))
trajectories_patchwork
ggsave(trajectories_patchwork, filename = "fig/trajectories/trajectories_patchwork.png", width = 11, height = 16)

trajectories_patchwork_horizontal <- ((a+c+e)/(b+d+f))+
  theme(text = element_text('mono', size = 15))
trajectories_patchwork_horizontal
ggsave(trajectories_patchwork_horizontal, filename = "fig/trajectories/trajectories_patchwork_horizontal.png", width = 16, height = 11)
