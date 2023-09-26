source("scripts/functions.R") # pulls in all the functions (written by Orr and Ryan)
library(tidyverse) # for data wrangling and plotting
library(spatsoc) # to implement the trajectory randomization method as described in Orr's paper
library(viridis)
statsBySocialWeight <- list()
simsBySocialWeight <- list()
socialWeight <- 0
for(y in 1:4){
  stats <- data.frame()
  baseHRStpSize <- 3
  sims <- list()
  for(x in 1:35){
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
                           quiet = T,
                           sim_3 = T,
                           socialWeight = socialWeight,
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
  statsBySocialWeight <- c(statsBySocialWeight, list(stats))
  simsBySocialWeight <- c(simsBySocialWeight, list(sims))
  socialWeight <- socialWeight + 0.25
}

plot(statsBySocialWeight[[1]]$HRStpSize, statsBySocialWeight[[1]]$mean_tortuosity, "l", col="green", xlab="HR Step Size", ylab="Mean Tortuosity")
lines(statsBySocialWeight[[2]]$HRStpSize, statsBySocialWeight[[2]]$mean_tortuosity, "l", col="red")
lines(statsBySocialWeight[[3]]$HRStpSize, statsBySocialWeight[[3]]$mean_tortuosity, "l", col="blue")
lines(statsBySocialWeight[[4]]$HRStpSize, statsBySocialWeight[[4]]$mean_tortuosity, "l", col="purple")
legend(legend=c("SocialWeight=0", "SocialWeight=0.25", "SocialWeight=0.5", "SocialWeight=0.75"), fill= c("green", "red", "blue", "purple"), "topright")
# sim$XY %>% ggplot(aes(x = X, y = Y, col = day))+geom_point(alpha = 0.7)+geom_point(data = sim$HRCent, pch = 19, size = 5)+facet_wrap(~indiv, scales = "free")+theme_minimal()+theme(legend.position = "none")+scale_color_viridis()
