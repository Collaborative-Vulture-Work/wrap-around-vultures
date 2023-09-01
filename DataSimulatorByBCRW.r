## this code generates data for testing the method of social network randomizations ###
## the code generates a population of agents with given movement rules ###
## currently it is set on the paired agents scenario, see details below for switching between different scenarios or exploring the parameter spcae fo other values
## writen by Orr Spiegel Jan 2016. contact me at orr.spiegel@mail.huji.ac.il if you need further help ###
# Edited by Ryan Nguyen and Kaija Gahm
# XXXK are notations by Kaija to indicate confusion/questions.

#Required packages #######
library(CircStats)
library(spatstat)
library(sp)
library(R.matlab)

#### model parameters and protocol selection for users to choose #######
ToPlot=1#can be 0 (no plot) or 1 (create plots)- note this will slow the proccess a lot!!
Color_indv=c(3,5,6,7,8,2,3,5,6,7,8,2)#if ToPlot=01 this determines how many individfuals to plot (currently 12). The colors for plotted individuas: 2-=red 3=green 5= light green 6 purpule 7 yelow 8 grey 

# DayLength <- 50 # Number of time steps per day (currently it is limited to 59 since i considered it as minutes within an hour)
# DaysToSimulate <- 10 # How many days to simulate
# N_indv <- 6 # Number of individuals in the population 
# Social_Pecrt_rng <- 2000 #detection range (in meters) - indivduals within this range will be considered in the bias point in the relevan scenario of sociable agents. set to 0 if you want socially indiferent agents (or in the fixed pairs scenario) 
# # Additional parameters
# DriftHRCenters <- 0 # 1 or 0 for a drift of the center in space along the simulation (i.e. the changing environment scenarios), 0 is no drift.
# DriftStrength <- c(1,0) # If DriftHRCenters==1 this defines drift in m per day for X and Y values in the simulation  
# DriftasOnce <- 2 # If DriftHRCenters==1 this defines when does the drift occur. can be 0,1,2. if 0 it will drift daily; if 1 it will drift in the midle of the run if 2 it will drift from begining. 
# PropDriftIndiv <- 1 # Proportion of drifting individuals.
# PairedAgents <- 0; # If 1 then simulates the paired agent scenario where agents are paired in their intial location and show attraction to their pair only, indiferent to the others. 
# PairStartDist <- 200 # If PairedAgents=1 this sets the Upper limit of the distance of the pairs. For the second individual in each pair the location will be random within this range from the first mate. 
#set.seed(1234) #comment it for new randomizations start points
# Movement rules for agents -----------------------------------------------
# EtaCRW <- 0.7 # The weight of the CRW component in the BCRW used to model the Indiv movement
# StpSize_ind <- 7 # Mean step lengths of individuals;
# StpStd_ind <- 5 # Standard deviations of step lengths of individuals
# Kappa_ind <- 3  # Concentration parameters of von Mises directional distributions used for individuals' movement

simulateAgents <- function(N_indv = 6,
                           DaysToSimulate = 10, 
                           DayLength = 50, 
                           Social_Pecrt_rng = 2000,
                           DriftHRCenters = 0,
                           DriftStrength = c(1,0),
                           DriftasOnce = 2,
                           PropDriftIndiv = 1,
                           PairedAgents = 0,
                           PairStartDist = 200,
                           seed = NULL,
                           EtaCRW = 0.7,
                           StpSize_ind = 7,
                           StpStd_ind = 5,
                           Kappa_ind = 3,
                           ToPlot = 1){
  
  # Set the seed, if one is provided ----------------------------------------
  if(!is.null(seed)){
    set.seed(seed)
  }
  
  # If requested, plot the step size distributions --------------------------
  if (ToPlot==1){
    # 1. Density plot of step lengths
    plot(density(rgamma(10000, 
                        shape = StpSize_ind^2/StpStd_ind^2, 
                        scale = StpStd_ind^2/StpSize_ind)))  
    
    # 2. Rose diagram of step directions
    rose.diag(rvm(n=10000, 
                  mean = 0,
                  k = Kappa_ind),
              bins=72, 
              pch = 16, 
              cex = 1, 
              shrink = 1)
  }
  
  # Prepare variables for storing data --------------------------------------
  if(PairedAgents==1){ # If paired design, then no social perception range for other agents
    Social_Pecrt_rng <- 0
  } 
  # Calculate dimensions: time steps, rows
  N_tmStp <- DaysToSimulate*DayLength # Total number of time steps that will be simulated for each iteration
  N_Rows <- N_indv*N_tmStp
  
  # Create data frame
  XYind_log2 <- data.frame(
    indiv = as.factor(rep(seq(1:N_indv), length.out = N_Rows, each = N_tmStp)),
    step = rep(seq(1:N_tmStp), length.out = N_Rows),
    Day = rep(seq(1:DaysToSimulate), length.out = N_Rows, each = DayLength),
    StepInDay = rep(seq(1:DayLength), length.out = N_Rows, each = 1),
    burst = as.factor(rep(seq(1:(DaysToSimulate*N_indv)), 
                          length.out = N_Rows, each = DayLength)),
    x = NA,
    y = NA,
    pseudoSex = NA
  )
  
  # Set starting conditions -------------------------------------------------
  startIndx <- 1
  Phi_ind <- rep(0, N_indv) # Direction of the last step for the individuals
  XYind <- vector(mode = "list") # This list will store the matrices of locations of all individuals
  HRCnt <- matrix(data = NA, nrow = N_indv, ncol = 3) # Home range centers
  HRCnt[,3] <- rep(c(1,2), N_indv/2) # Since sex is listed in this parameter I update it to be pairs of diferent sexes for the paired design
  HRcenterDist <- rep(0, N_indv/2) # Distance between pairs of agents at the beginning
  
  #### Setting drift of HR center (bias point) for the drifting scenarios ####
  if (DriftasOnce == 0){ # 0 it will drift daily;
    Xdrift <- cumsum(rep(c(DriftStrength[1], rep(0, (DayLength-1))), DaysToSimulate)) # For each day i have the drift, then steps with no drift until the next day. I work with Cumsum since the drift grows 
    Ydrift <- cumsum(rep(c(DriftStrength[2], rep(0, (DayLength-1))), DaysToSimulate)) 
    print('using drift daily')
  } else if (DriftasOnce == 1){ # if 1 it will drift in the middle of the run  
    Xdrift <- rep(0, N_tmStp)
    Xdrift[N_tmStp/2] <- DriftStrength[1]*DaysToSimulate
    Xdrift <- cumsum(Xdrift)
    Ydrift <- rep(0,N_tmStp)
    Ydrift[N_tmStp/2] <- DriftStrength[2]*DaysToSimulate
    Ydrift <- cumsum(Ydrift) 
    print('using drift from middle step of the run')
  } else if (DriftasOnce == 2){ #if 2 it will drift from beginning.
    Xdrift <- rep(DriftStrength[1]*DaysToSimulate,N_tmStp)
    Ydrift <- rep(DriftStrength[2]*DaysToSimulate,N_tmStp)
    print('using drift from start')
  }else{
    print('check your params')
  }
  
  DrifByStep <- rbind(Xdrift, Ydrift)
  
  # Determining individuals to drift.
  DriftingYorN <- c(rep(1, PropDriftIndiv*N_indv),
                    rep(-1, N_indv-PropDriftIndiv*N_indv))
  DriftingYorN <- sample(DriftingYorN, N_indv)
  
  # Setting the HR centerDrift for this time step (cumulative from beginning)
  if(DriftHRCenters==1){
    CurDrift <- DrifByStep[,Curr_tmStp]
    names(CurDrift) <- NULL
  }else{
    CurDrift <- c(0,0)
  }
  
  # Generate landscapes -----------------------------------------------------
  #### generating landscape for this iteration #######      
  # N_FoodPatches <- 50 # How many patches - this is just to be consistent with the code. currently individuals do not respond to resources. 
  # Scl <- 2000 # The size of the area for simuation. not important since there is not boundry. just to be consistent with other codes.
  # PointsStrc <- rsyst(nx <- sqrt(N_FoodPatches)) # Generation of a uniform landscape
  # FoodItems <- SpatialPoints(coords(PointsStrc)*Scl-Scl/2) # These are the points on scale of 0 to 1 no -Scl/2 to Scl/2
  # if(ToPlot==1){
  #   plot(FoodItems, col='white', pch=1) 
  #   title('the simulated landscape')
  # } # The number of point will be more or less kappa*mu  
  
  #### a loop on individuals for initial conditions ####
  for (k in 1:N_indv) {
    # Placing agents in their initial positions 
    XYind[[k]] <- matrix(rep(NA, 2 * N_tmStp), ncol = 2) # This matrix will store the location of each individual
    XYind[[k]][1, ] <-  c(runif(n=1, min=-Scl/3, max=Scl/3 ), # X random initial location of each individual is uniformly distributed in the range of the observed ones
                          runif(n=1, min=-Scl/3, max=Scl/3 )) # Y random initial location of each individual is uniformly distributed in the range of the observed ones
    HRCnt[k,1:2] <- XYind[[k]][1, ] # Logging the HR center 
    
    if((PairedAgents == 1) & (k %% 2 == 0)){ # If this is a paired scenario then I switch every other individual to be a pair with nearby location to the previous one
      XYind[[k]][1, ] <- XYind[[k-1]][1, ]+c(runif(n=2, min=-PairStartDist/2, 
                                                   max=PairStartDist/2)) # Setting the start tlocation for the pair
      HRcenterDist[k/2] <- round(dist(rbind(XYind[[k-1]][1, ], c(XYind[[k]][1, ]))),
                                 digit = 1) # Storing the initial distance from this pair
      print(paste("pair:",k-1,"and",k, "distance",HRcenterDist[k/2] ))
    } # If paired design, then use nearby coordinate for every second individual
    if (ToPlot == 1){
      plot(SpatialPoints(coords = matrix(data = XYind[[k]][1, ], nrow = 1)), add = T,
           pch = HRCnt[k, c(3)]+23, col = HRCnt[k, c(3)], bg = HRCnt[k, c(3)], cex=1.5) 
      if((PairedAgents == 1) & (k %% 2 == 0)){ # Plot also line between pairs
        plot(SpatialLines(list(Lines(list(Line(rbind(XYind[[k-1]][1, ],
                                                     c(XYind[[k]][1, ])))), 
                                     ID=k/2) )), add=T) # This is the river 'line' 
      } # Plot also line between pairs
    } # Plot the line
  } # End loop on individuals 
  
  
  #### loop on time steps and individuals to run the simulation ####
  for(Curr_tmStp in 1:(N_tmStp-1)){
    ## loop on individuals ##
    for(Curr_indv in 1:N_indv){
      
      #### distance of this Curr_indv to all other individuals 
      Dist <- rep(NA,N_indv ) 
      for(ii in 1:N_indv){
        Dist[ii] <- dist(rbind(XYind[[ii]][Curr_tmStp, ],
                               c(XYind[[Curr_indv]][Curr_tmStp, ])))
      }
      Dist[Dist==0] <- NA # Getting rid of the distance to self
      
      ## start generation of indiv location at time k+1
      
      ##### selecting direction ##########
      # Calculating the direction to the initial location (bias point )+ now with drift for the current step
      BiasPoint <- (XYind[[Curr_indv]][1, ] + CurDrift*DriftingYorN[Curr_indv]) # This bias point is the origin+ the current cumulative bias
      if((PairedAgents==1) & (Curr_indv %% 2 == 1)){
        BiasPoint <- colMeans(rbind(BiasPoint, XYind[[Curr_indv+1]][Curr_tmStp, ]))
      } # Updating bias point as the mean of HR center and mate's last location
      if((PairedAgents==1) & (Curr_indv %% 2 == 0)){
        BiasPoint <- colMeans(rbind(BiasPoint, XYind[[Curr_indv-1]][Curr_tmStp-1, ]))
      } # Updating bias point as the mean of HR center and mate's previous location (before last step since he moved already)  
      
      if(min(Dist, na.rm=T) < Social_Pecrt_rng){ # If there is another individual in range,
        # if i want the mean of direction to the HR and nearest neighbor: 
        # BiasPoint <- colMeans(rbind(BiasPoint, XYind[[which.min(Dist)]][Curr_tmStp, ]))} # Then updated  this for the bias also
        BiasPoint <- XYind[[which.min(Dist)]][Curr_tmStp, ]
      } # Then updated  this for the bias also
      
      coo <- BiasPoint  - XYind[[Curr_indv]][Curr_tmStp, ] # Checking direction to the Bias point of this individual, change the second ccomp to have another bias
      # Set direction:
      mu <- Arg(coo[1] + (0+1i) * coo[2]) # XXXK what is this? Direction, I think.
      # Make sure direction is not negative:
      if(mu < 0){
        mu <- mu + 2 * pi  
      } 
      mu.av <- Arg(EtaCRW * exp(Phi_ind[Curr_indv] * (0+1i)) + (1 - EtaCRW) * exp(mu * (0+1i))) # Bias to initial location + CRW to find the von mises center for next step
      Phi_ind[Curr_indv] <- rvm(n=1, mean = mu.av, k = Kappa_ind)#choosing curr step direction from vonMises centered around the direction selected above 
      
      ##### Performing the step #########
      # Selection of step size for this indiv in this state from the specific gamma          
      step.len <- rgamma(1, shape = StpSize_ind^2/StpStd_ind^2, 
                         scale = StpStd_ind^2/StpSize_ind)
      step <- step.len * c(Re(exp((0+1i) * Phi_ind[Curr_indv])), 
                           Im(exp((0+1i) * Phi_ind[Curr_indv])))
      XYind[[Curr_indv]][Curr_tmStp + 1, ] <- XYind[[Curr_indv]][Curr_tmStp, ] + step # The indiv's next location
      
      ###### ploting the steps ########
      if(Curr_indv <= length(Color_indv) & ToPlot==1 ){
        # Plot only up to 6 indiv and only if ToPlot==1
        StepAsLine = SpatialLines(list(Lines(list(Line(XYind[[Curr_indv]][Curr_tmStp:(Curr_tmStp+1) , ])), 
                                             ID = Curr_tmStp) )) # Convert line to spatial object for plot
        plot(StepAsLine, add = T, col = Color_indv[Curr_indv], lwd = 4) # Plot the buffer
      } # Plot only up to 6 indiv
      
    } # End loop on invdividuals
    print(c("done with timestep", Curr_tmStp, "out of", N_tmStp))
    
  } # End loop on time steps
  
  #### after the nested loops -how many times out of the box? ####
  for(k in 1:N_indv){        
    endIndx <- startIndx + N_tmStp-1
    XYind_log2[c(startIndx:endIndx), c("x","y")] <- XYind[[k]]        
    XYind_log2[c(startIndx:endIndx), c("pseudoSex")] <- HRCnt[k,3] # Storing the sex of this agent based on the HR center it got was it a male1 or a female2 there?
    startIndx <- endIndx+1
  }        
  OutOfTheBox <- sum((XYind_log2$x<  -Scl/2) | 
                    (XYind_log2$x>   Scl/2) |
                    (XYind_log2$y<  -Scl/2) |
                    (XYind_log2$y>   Scl/2) ) /length(XYind_log2$x)
  print(paste("out of the box rate", round(OutOfTheBox, digit = 5)))
  
  # Create a list for output with three slots: hr centers, and xy coordinates
  Name1 <- paste("xyFromSimulationForSNanalysis", N_tmStp, N_indv, 100*EtaCRW, StpSize_ind, DriftHRCenters, ".rdata", sep = "_")
  out <- list("Name1" = Name1, "Name2" = Name2, "HRCntXY" = HRCnt, "XY" = XYind_log2)
  return(out)
}

simulateAgents()
save(list("HRCntXY" = HRCnt, "XY" = XYind_log2), file = Name1)
Name2 <- "xyFromSimulationForSNanalysis.mat"
save(list=ls(),file=Name1)
writeMat(con = Name2, XY = XYind_log2, HRCntXY = HRCnt) 





















