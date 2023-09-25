source("scripts/functions.R") # pulls in all the functions (written by Orr and Ryan)
library(tidyverse) # for data wrangling and plotting
library(spatsoc) # to implement the trajectory randomization method as described in Orr's paper
library(viridis)

stats <- data.frame()
sims <- list()
baseHRStpSize <- 7
for(x in 1:50){
  HRStpSize <- baseHRStpSize * x
  HRStpStd <- HRStpSize * 0.05
  sim <- simulateAgents(N = 6,
                         Days = 6,
                         DayLength = 50,
                         Soc_Percep_Rng = 1000,
                         PairedAgents = 0,
                         PairStartDist = 0,
                         Scl = 1000,
                         seed = 9252023,
                         EtaCRW = 0.7,
                         StpSize_ind = 7,
                         StpStd_ind = 5,
                         Kappa_ind = 4,
                         ToPlot = 0,
                         quiet = F,
                         sim_3 = T,
                         socialWeight = 0,
                         HREtaCRW = 0.7,
                         HRStpSize = HRStpSize,
                         HRStpStd = HRStpStd,
                         HRKappa_ind = 4)
  sims <- c(sims, list(sim))
  tortuosityByIndiv <- get_tortuosity(sim$XY)
  mean_tortuosity <- colMeans(tortuosityByIndiv)
  stats <- rbind(stats, c(HRStpSize, mean_tortuosity))
}
colnames(stats) <- c("HRStpSize", "mean_tortuosity")
plot(stats$HRStpSize, stats$mean_tortuosity, "l")
# sims[[simNum]]$XY %>% ggplot(aes(x = X, y = Y, col = day))+geom_point(alpha = 0.7)+geom_point(data = sims[[simNum]]$HRCent, pch = 19, size = 5)+facet_wrap(~indiv, scales = "free")+theme_minimal()+theme(legend.position = "none")+scale_color_viridis()
