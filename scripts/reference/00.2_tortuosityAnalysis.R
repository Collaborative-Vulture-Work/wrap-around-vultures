source("scripts/functions.R") # pulls in all the functions (written by Orr and Ryan)
library("tidyverse")
library("viridis")

baseAgentStep <- 7
userHRRatios <- seq(from = 1, to = 20, by = 2) # can't take this down to 0 because then the gamma distributions for step size make no sense.
socialWeights <- seq(from = 0, to = 1, by = 0.1)

# run the simulations -----------------------------------------------------
statsBySocialWeight_k0 <- list()
simsBySocialWeight_k0 <- list()
# Non-directional home range movement (kappa = 0.01)
for(y in 1:length(socialWeights)){
  stats <- data.frame()
  sims <- list()
  socialWeight <- socialWeights[y]
  for(ratio in userHRRatios){
    HRStpSize <- baseAgentStep*ratio
    HRStpStd <- HRStpSize*0.75 # setting the standard deviation to 5% of the mean.
    print(paste0("Working on HRRatio: ", ratio, " for weight: ", socialWeight))
    sim <- simulateAgents(N = 6,
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
                          HREtaCRW = 0,
                          HRStpSize = HRStpSize,
                          HRStpStd = HRStpStd,
                          HRKappa_ind = 0.01)
    sims <- c(sims, list(sim))
    tortuosityByIndiv <- get_tortuosity(sim$XY) # get tortuosity per individual
    mean_tortuosity <- colMeans(tortuosityByIndiv)
    strength_tortuosity <- 0
    stats <- rbind(stats, c(HRStpSize, ratio, socialWeight, mean_tortuosity)) # add stats to list
  }
  colnames(stats) <- c("HRStpSize", "ratio", "socialWeight", "mean_tortuosity") # rename columns
  statsBySocialWeight_k0 <- c(statsBySocialWeight_k0, list(stats)) # add stats and sims to list
  simsBySocialWeight_k0 <- c(simsBySocialWeight_k0, list(sims))
}

save(simsBySocialWeight_k0, file = "data/simsBySocialWeight_k0.Rda")
save(statsBySocialWeight_k0, file = "data/statsBySocialWeight_k0.Rda")

statsBySocialWeight_k4 <- list()
simsBySocialWeight_k4 <- list()
# Directional home range movement (Kappa = 4)
for(y in 1:length(socialWeights)){
  stats <- data.frame()
  sims <- list()
  socialWeight <- socialWeights[y]
  for(ratio in userHRRatios){
    HRStpSize <- baseAgentStep*ratio
    HrStpStd <- HRStpSize*0.75 # setting the standard deviation to 5% of the mean.
    print(paste0("Working on HRRatio: ", ratio, " for weight: ", socialWeight))
    sim <- simulateAgents(N = 6,
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
                          HREtaCRW = 0,
                          HRStpSize = HRStpSize,
                          HRStpStd = HRStpStd,
                          HRKappa_ind = 4)
    sims <- c(sims, list(sim))
    tortuosityByIndiv <- get_tortuosity(sim$XY) # get tortuosity per individual
    mean_tortuosity <- colMeans(tortuosityByIndiv)
    strength_tortuosity <- 0
    stats <- rbind(stats, c(HRStpSize, ratio, socialWeight, mean_tortuosity)) # add stats to list
  }
  colnames(stats) <- c("HRStpSize", "ratio", "socialWeight", "mean_tortuosity") # rename columns
  statsBySocialWeight_k4 <- c(statsBySocialWeight_k4, list(stats)) # add stats and sims to list
  simsBySocialWeight_k4 <- c(simsBySocialWeight_k4, list(sims))
}

save(simsBySocialWeight_k4, file = "data/simsBySocialWeight_k4.Rda")
save(statsBySocialWeight_k4, file = "data/statsBySocialWeight_k4.Rda")

# Plotting ----------------------------------------------------------------
load("data/simsBySocialWeight_k0.Rda")
load("data/simsBySocialWeight_k4.Rda")
load("data/statsBySocialWeight_k0.Rda")
load("data/statsBySocialWeight_k4.Rda")

# Combine the stats into one data frame
stats_k0 <-  statsBySocialWeight_k0 %>% purrr::list_rbind()
stats_k4 <-  statsBySocialWeight_k4 %>% purrr::list_rbind()

# Make graphs -------------------------------------------------------------
# Line graph
stats_k0 %>%
  ggplot(aes(x = ratio, y = mean_tortuosity, col = factor(socialWeight)))+
  geom_line(linewidth = 1.5)+
  scale_color_viridis_d(name = "social \nweight")+
  theme_classic()

stats_k4 %>%
  ggplot(aes(x = ratio, y = mean_tortuosity, col = factor(socialWeight)))+
  geom_line(linewidth = 1.5)+
  scale_color_viridis_d(name = "social \nweight")+
  theme_classic()

# Shared line plot: ratio vs. tortuosity, colored by socialWeight
r_tort_sw_line <- stats_k0 %>%
  mutate(kappa = 0) %>%
  bind_rows(stats_k4 %>% mutate(kappa = 4)) %>%
  ggplot(aes(x = ratio, y = mean_tortuosity, col = factor(socialWeight)))+
  geom_line(linewidth = 1.5)+
  scale_color_viridis_d(name = "social \nweight", option = "H")+
  theme_classic()+
  facet_wrap(~kappa)+
  geom_hline(aes(yintercept = 1), col = "blue")
ggsave(r_tort_sw_line, file = "fig/r_tort_sw_line.png", width = 7, height = 5)

# Shared line plot the other way: socialWeight vs. tortuosity, colored by ratio
sw_tort_r_line <- stats_k0 %>%
  mutate(kappa = 0) %>%
  bind_rows(stats_k4 %>% mutate(kappa = 4)) %>%
  ggplot(aes(x = socialWeight, y = mean_tortuosity, col = factor(ratio)))+
  geom_line(linewidth = 1.5)+
  scale_color_viridis_d(name = "ratio", option = "H")+
  theme_classic()+
  facet_wrap(~kappa)
ggsave(sw_tort_r_line, file = "fig/sw_tort_r_line.png", width = 7, height = 5)

# Heat map
heatmap_k0 <- stats_k0 %>%
  ggplot(aes(x = ratio, y = socialWeight, fill = mean_tortuosity))+
  geom_tile()+
  scale_fill_viridis_c()+
  ylab("Social Weight")+
  xlab("HR step / Agent step")
heatmap_k0
ggsave(heatmap_k0, filename = "fig/heatmap_k0.png", width = 6, height = 5)

heatmap_k4 <- stats_k4 %>%
  ggplot(aes(x = ratio, y = socialWeight, fill = mean_tortuosity))+
  geom_tile()+
  scale_fill_viridis_c()+
  ylab("Social Weight")+
  xlab("HR step / Agent step")
heatmap_k4
ggsave(heatmap_k4, filename = "fig/heatmap_k4.png", width = 6, height = 5)

heatmap_shared <- stats_k0 %>% mutate(kappa = 0) %>%
  bind_rows(stats_k4 %>% mutate(kappa = 4)) %>%
  ggplot(aes(x = ratio, y = socialWeight, fill = mean_tortuosity))+
  geom_tile()+
  scale_fill_viridis_c()+
  ylab("Social Weight")+
  xlab("HR step / Agent step")+
  facet_wrap(~kappa)+
  theme_minimal()
ggsave(heatmap_shared, filename = "fig/heatmap_shared.png", width = 9, height = 4)

# Un-nesting the list so we can plot the data
xy_k0 <- simsBySocialWeight_k0
names(xy_k0) <- socialWeights
xy_k0 <- xy_k0 %>% map(~ map(., "XY"))
xy_k0 <- map(xy_k0, ~map2(.x, .y = userHRRatios, ~{.x %>% mutate(ratio = .y)}) %>% purrr::list_rbind())
xys_k0 <- purrr::list_rbind(xy_k0, names_to = "socialWeight")
xys_k0$socialWeight <- as.numeric(xys_k0$socialWeight)
xys_k0$indiv <- as.factor(xys_k0$indiv)

hrc_k0 <- simsBySocialWeight_k0
names(hrc_k0) <- socialWeights
hrc_k0 <- hrc_k0 %>% map(~ map(., "HRCent"))
hrc_k0 <- map(hrc_k0, ~map2(.x, .y = userHRRatios, ~{.x %>% mutate(ratio = .y)}) %>% purrr::list_rbind())
hrcs_k0 <- purrr::list_rbind(hrc_k0, names_to = "socialWeight")
hrcs_k0$socialWeight <- as.numeric(hrcs_k0$socialWeight)
hrcs_k0$indiv <- as.factor(hrcs_k0$indiv)

xy_k4 <- simsBySocialWeight_k4
names(xy_k4) <- socialWeights
xy_k4 <- xy_k4 %>% map(~ map(., "XY"))
xy_k4 <- map(xy_k4, ~map2(.x, .y = userHRRatios, ~{.x %>% mutate(ratio = .y)}) %>% purrr::list_rbind())
xys_k4 <- purrr::list_rbind(xy_k4, names_to = "socialWeight")
xys_k4$socialWeight <- as.numeric(xys_k4$socialWeight)
xys_k4$indiv <- as.factor(xys_k4$indiv)

hrc_k4 <- simsBySocialWeight_k4
names(hrc_k4) <- socialWeights
hrc_k4 <- hrc_k4 %>% map(~ map(., "HRCent"))
hrc_k4 <- map(hrc_k4, ~map2(.x, .y = userHRRatios, ~{.x %>% mutate(ratio = .y)}) %>% purrr::list_rbind())
hrcs_k4 <- purrr::list_rbind(hrc_k4, names_to = "socialWeight")
hrcs_k4$socialWeight <- as.numeric(hrcs_k4$socialWeight)
hrcs_k4$indiv <- as.factor(hrcs_k4$indiv)

#Define a plotting function
plt <- function(r, sw, n = NULL, xy, hr){
  indivs <- levels(xy$indiv)
  if(!is.null(n)){
    random <- sample(indivs, size = n)
  }else{
    random <- indivs
  }
  dat <- xy %>% filter(ratio == r, socialWeight == sw, indiv %in% random)
  hrs <- hr %>% filter(ratio == r, socialWeight == sw, indiv %in% random)
  ggplot() + 
    geom_point(data = dat, aes(x = X, y = Y, col = day))+
    geom_point(data = hrs, aes(x = X, y = Y, col = day), 
                        pch = 19, size = 5)+
    facet_wrap(~indiv, scales = "free")+theme_minimal()+
    theme(legend.position = "none", text = element_text(size = 10))+
    scale_color_viridis()+
    ggtitle(paste("Ratio", r, ", social weight", sw))
}

# Investigate the k0 heatmap
heatmap_k0
plt(1, 0.6, xy = xys_k0, hr = hrcs_k0)

# Investigate the k4 heatmap
heatmap_k4
plt(1, 0, xy = xys_k4, hr = hrcs_k4)
plt(4, 0.2, 5, xy = xys_k4, hr = hrcs_k4)
plt(20, 0, 6, xy = xys_k4, hr = hrcs_k4)
plt(20, 0.7, 6, xy = xys_k4, hr = hrcs_k4)
plt(20, 0, 6, xy = xys_k0, hr = hrcs_k0)
plt(5, 0, 6, xy = xys_k0, hr = hrcs_k0)
plt(5, 0.7, 6, xy = xys_k0, hr = hrcs_k0)

# Trying to get the three scenarios ---------------------------------------
# SCENARIO 1: fixed home ranges
# SCENARIO 2: randomly-moving home ranges
# SCENARIO 3: directionally-moving home ranges
# ratio 20, kappa 4
plt(r = 20, sw = 0, n = 6, xy = xys_k4, hr = hrcs_k4)


