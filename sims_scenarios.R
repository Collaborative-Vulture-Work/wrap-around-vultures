# Let's make data for the three different scenarios
# Scenario 1: fixed home ranges
# Scenario 2: randomly-moving home ranges
# Scenario 3: directionally-moving home ranges
source("scripts/functions.R")
library(tidyverse)


# SIM 1 -------------------------------------------------------------------
r <- 0.01 # home range centers effectively not moving
baseAgentStep <- 7
HRStpSize <- baseAgentStep*r
HRStpStd <- HRStpSize*0.75 # leaving this here for now--could go back and change later if we want. 
hrk <- 0.01 # kappa effectively zero--any jitter will be random
hre <- 0.7

sim1_ns <- simulateAgents(N = 5,
                       Days = 5,
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
                       socialWeight = 0,
                       HREtaCRW = 0.7,
                       HRStpSize = HRStpSize,
                       HRStpStd = HRStpStd,
                       HRKappa_ind = hrk)
save(sim1_ns, file = "data/simulations/sim1_ns.Rda")
load("data/simulations/sim1_ns.Rda")

ggplot() + 
  geom_point(data = sim1_ns$XY, aes(x = X, y = Y, col = day))+
  geom_point(data = sim1_ns$HRCent, aes(x = X, y = Y, col = day), 
             pch = 19, size = 5)+
  facet_wrap(~indiv, scales = "free")+theme_minimal()+
  theme(legend.position = "none", text = element_text(size = 10))+
  scale_color_viridis()+
  ggtitle("Scenario 1, non-sociable")

ggplot() +
  geom_point(data = sim1_ns$XY, aes(x = X, y = Y, col = indiv))+
  geom_point(data = sim1_ns$HRCent, aes(x = X, y = Y, col = indiv),
             pch = 19, size = 5)+
  theme(legend.position = "none", text = element_text(size = 10))+
  scale_color_viridis_d()+
  theme_minimal()+
  ggtitle("Scenario 1, non-sociable")

sim1_s <- simulateAgents(N = 5,
                          Days = 5,
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
                          socialWeight = 0.5,
                          HREtaCRW = 0.7,
                          HRStpSize = HRStpSize,
                          HRStpStd = HRStpStd,
                          HRKappa_ind = hrk)
save(sim1_s, file = "data/simulations/sim1_s.Rda")
load("data/simulations/sim1_s.Rda")

ggplot() + 
  geom_point(data = sim1_s$XY, aes(x = X, y = Y, col = day))+
  geom_point(data = sim1_s$HRCent, aes(x = X, y = Y, col = day), 
             pch = 19, size = 5)+
  facet_wrap(~indiv, scales = "free")+theme_minimal()+
  theme(legend.position = "none", text = element_text(size = 10))+
  scale_color_viridis()+
  ggtitle("Scenario 1, sociable")

ggplot() +
  geom_point(data = sim1_s$XY, aes(x = X, y = Y, col = indiv))+
  geom_point(data = sim1_s$HRCent, aes(x = X, y = Y, col = indiv),
             pch = 19, size = 5)+
  theme(legend.position = "none", text = element_text(size = 10))+
  scale_color_viridis_d()+
  theme_minimal()+
  ggtitle("Scenario 1, sociable")

