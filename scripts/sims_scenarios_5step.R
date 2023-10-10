# Re-running the three different scenarios, but with only a few spatial positions each day. 
# Scenario 1: fixed home ranges
# Scenario 2: randomly-moving home ranges
# Scenario 3: directionally-moving home ranges
source("scripts/functions.R")
library(tidyverse)
socLevels <- seq(from = 0, to = 1, by = 0.1)


# SIM 1 -------------------------------------------------------------------
r <- 0.01 # home range centers effectively not moving
baseAgentStep <- 7
HRStpSize <- baseAgentStep*r
HRStpStd <- HRStpSize*0.75 # leaving this here for now--could go back and change later if we want. 
hrk <- 0.01
hre <- 0.7

sim1_ns_5step <- simulateAgents(N = 30,
                          Days = 50,
                          DayLength = 5,
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
                          socialWeight = 0,
                          HREtaCRW = 0.7,
                          HRStpSize = HRStpSize,
                          HRStpStd = HRStpStd,
                          HRKappa_ind = hrk)
save(sim1_ns_5step, file = "data/simulations/five_steps_per_day/sim1_ns_5step.Rda")
load("data/simulations/five_steps_per_day/sim1_ns_5step.Rda")

sim1_socLevels_5step <- map(socLevels, ~{
  sim <- simulateAgents(N = 30,
                        Days = 50,
                        DayLength = 5,
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
                        socialWeight = .x,
                        HREtaCRW = 0.7,
                        HRStpSize = HRStpSize,
                        HRStpStd = HRStpStd,
                        HRKappa_ind = hrk)
  return(sim)
})
save(sim1_socLevels_5step, file = "data/simulations/five_steps_per_day/sim1_socLevels_5step.Rda")

sim1_s_5step <- simulateAgents(N = 30,
                          Days = 50,
                          DayLength = 5,
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
                          socialWeight = 0.5,
                          HREtaCRW = 0.7,
                          HRStpSize = HRStpSize,
                          HRStpStd = HRStpStd,
                          HRKappa_ind = hrk)
save(sim1_s_5step, file = "data/simulations/five_steps_per_day/sim1_s_5step.Rda")
load("data/simulations/five_steps_per_day/sim1_s_5step.Rda")


# SIM 2 -------------------------------------------------------------------
r <- 10 # home range steps are 10x the size of agent steps
baseAgentStep <- 7
HRStpSize <- baseAgentStep*r
HRStpStd <- HRStpSize*0.75 # leaving this here for now--could go back and change later if we want. 
hrk <- 0.01 # effectively k = 0, random direction for home range movement.
hre <- 0.7

sim2_ns_5step <- simulateAgents(N = 30,
                          Days = 50,
                          DayLength = 5,
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
save(sim2_ns_5step, file = "data/simulations/five_steps_per_day/sim2_ns_5step.Rda")
load("data/simulations/five_steps_per_day/sim2_ns_5step.Rda")

sim2_socLevels_5step <- map(socLevels, ~{
  sim <- simulateAgents(N = 30,
                        Days = 50,
                        DayLength = 5,
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
                        socialWeight = .x,
                        HREtaCRW = 0.7,
                        HRStpSize = HRStpSize,
                        HRStpStd = HRStpStd,
                        HRKappa_ind = hrk)
  return(sim)
})
save(sim2_socLevels_5step, file = "data/simulations/five_steps_per_day/sim2_socLevels_5step.Rda")

sim2_s_5step <- simulateAgents(N = 30,
                          Days = 50,
                          DayLength = 5,
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
save(sim2_s_5step, file = "data/simulations/five_steps_per_day/sim2_s_5step.Rda")
load("data/simulations/five_steps_per_day/sim2_s_5step.Rda")

# SIM 3 -------------------------------------------------------------------
r <- 10 # home range steps are 10x the size of agent steps
baseAgentStep <- 7
HRStpSize <- baseAgentStep*r
HRStpStd <- HRStpSize*0.75 # leaving this here for now--could go back and change later if we want. 
hrk <- 20 # k = 20, highly directional
hre <- 0.7

sim3_ns_5step <- simulateAgents(N = 30,
                          Days = 50,
                          DayLength = 5,
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
save(sim3_ns_5step, file = "data/simulations/five_steps_per_day/sim3_ns_5step.Rda")
load("data/simulations/five_steps_per_day/sim3_ns_5step.Rda")

sim3_socLevels_5step <- map(socLevels, ~{
  sim3 <- simulateAgents(N = 30,
                         Days = 50,
                         DayLength = 5,
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
                         socialWeight = .x,
                         HREtaCRW = 0.7,
                         HRStpSize = HRStpSize,
                         HRStpStd = HRStpStd,
                         HRKappa_ind = hrk)
  return(sim3)
})
save(sim3_socLevels_5step, file = "data/simulations/five_steps_per_day/sim3_socLevels_5step.Rda")

sim3_s_5step <- simulateAgents(N = 30,
                         Days = 50,
                         DayLength = 5,
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
save(sim3_s_5step, file = "data/simulations/five_steps_per_day/sim3_s_5step.Rda")
load("data/simulations/five_steps_per_day/sim3_s_5step.Rda")
