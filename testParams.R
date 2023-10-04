socialWeight <- 0
r <- 10
baseAgentStep <- 7
HRStpSize <- baseAgentStep*r
HRStpStd <- HRStpSize*0.75 # leaving this here for now--could go back and change later if we want. 
hrk <- 20
hre <- 0.7
sim <- simulateAgents(N = 5,
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
                      socialWeight = socialWeight,
                      HREtaCRW = hre,
                      HRStpSize = HRStpSize,
                      HRStpStd = HRStpStd,
                      HRKappa_ind = hrk)
xy <- sim$XY
hrcent <- sim$HRCent

ggplot() + 
  geom_point(data = xy, aes(x = X, y = Y, col = day))+
  geom_point(data = hrcent, aes(x = X, y = Y, col = day), 
             pch = 19, size = 5)+
  facet_wrap(~indiv, scales = "free")+theme_minimal()+
  theme(legend.position = "none", text = element_text(size = 10))+
  scale_color_viridis()+
  ggtitle(paste0("hrk = ", hrk, ", sw = ", socialWeight, ", ratio = ", r, ", hre = ", hre))

ggplot() +
  geom_point(data = xy, aes(x = X, y = Y, col = indiv))+
  geom_point(data = hrcent, aes(x = X, y = Y, col = indiv),
             pch = 19, size = 5)+
  theme(legend.position = "none", text = element_text(size = 10))+
  scale_color_viridis_d()+
  theme_minimal()+
  ggtitle(paste0("hrk = ", hrk, ", sw = ", socialWeight, ", ratio = ", r, ", hre = ", hre))
