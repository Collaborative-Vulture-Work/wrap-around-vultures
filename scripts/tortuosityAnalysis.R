source("scripts/functions.R") # pulls in all the functions (written by Orr and Ryan)
library("tidyverse")

statsBySocialWeight <- list() # list of stats by social weights (0, 0.25, 0.5, 0.75)
simsBySocialWeight <- list() # list of sims by social weights
socialWeight <- 0 # starting social weight
socialWeightIncrement <- 0.25 # amount to increment social weight by
HRSteps <- 35

for(y in 1:ceiling(1/socialWeightIncrement)){
  stats <- data.frame() # data frame to hold HR step size and mean tortuosity for the sims
  baseHRStpSize <- 3 # starting HRStpSize
  sims <- list() # list to hold sims
  for(x in 1:HRSteps){
    HRStpSize <- baseHRStpSize * x
    HRStpStd <- HRStpSize * 0.05
    print(paste0("Working on sim: ", x, " for weight: ", socialWeight))
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
    tortuosityByIndiv <- get_tortuosity(sim$XY) # get tortuosity per individual
    mean_tortuosity <- colMeans(tortuosityByIndiv)
    strength_tortuosity <- colSums(tortuosityByIndiv)
    stats <- rbind(stats, c(HRStpSize, mean_tortuosity, strength_tortuosity)) # add stats to list
  }
  colnames(stats) <- c("HRStpSize", "mean_tortuosity", "strength_tortuosity") # rename columns
  statsBySocialWeight <- c(statsBySocialWeight, list(stats)) # add stats and sims to list
  simsBySocialWeight <- c(simsBySocialWeight, list(sims))
  socialWeight <- socialWeight + socialWeightIncrement # increment social weight
}

plot(statsBySocialWeight[[1]]$HRStpSize, statsBySocialWeight[[1]]$mean_tortuosity, "l", col="green", xlab="HR Step Size", ylab="Mean Tortuosity")
lines(statsBySocialWeight[[2]]$HRStpSize, statsBySocialWeight[[2]]$mean_tortuosity, "l", col="red")
lines(statsBySocialWeight[[3]]$HRStpSize, statsBySocialWeight[[3]]$mean_tortuosity, "l", col="blue")
lines(statsBySocialWeight[[4]]$HRStpSize, statsBySocialWeight[[4]]$mean_tortuosity, "l", col="purple")
legend(legend=c("SocialWeight=0", "SocialWeight=0.25", "SocialWeight=0.5", "SocialWeight=0.75"), fill= c("green", "red", "blue", "purple"), "topright")

# sim$XY %>% ggplot(aes(x = X, y = Y, col = day))+geom_point(alpha = 0.7)+geom_point(data = sim$HRCent, pch = 19, size = 5)+facet_wrap(~indiv, scales = "free")+theme_minimal()+theme(legend.position = "none")+scale_color_viridis()
