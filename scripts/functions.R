# FUNCTION DEFINITIONS
# To be used in the rest of the workflow for this project.
library(dplyr)
library(lubridate)
library(data.table)
library(spatsoc)
library(multidplyr)
#devtools::install_github("kaijagahm/vultureUtils")
# library(vultureUtils)

# 1. simulateAgents -------------------------------------------------------
## this code generates data for testing the method of social network randomizations ###
## the code generates a population of agents with given movement rules ###
## currently it is set on the paired agents scenario, see details below for switching between different scenarios or exploring the parameter spcae fo other values
## writen by Orr Spiegel Jan 2016. contact me at orr.spiegel@mail.huji.ac.il if you need further help ###
# Edited by Ryan Nguyen and Kaija Gahm
# XXXK are notations by Kaija to indicate confusion/questions.

#### model parameters and protocol selection for users to choose #######
ToPlot=1#can be 0 (no plot) or 1 (create plots)- note this will slow the proccess a lot!!
Color_indv=c(3,5,6,7,8,2,3,5,6,7,8,2)#if ToPlot=01 this determines how many individfuals to plot (currently 12). The colors for plotted individuas: 2-=red 3=green 5= light green 6 purpule 7 yelow 8 grey 

simulateAgents <- function(N = 6, # Number of individuals in the population 
                           Days = 10, # Number of days to simulate
                           DayLength = 50, # Number of time steps per day (currently it is limited to 59 since i considered it as minutes within an hour)
                           Soc_Percep_Rng = 2000, #detection range (in meters) - indivduals within this range will be considered in the bias point in the relevan scenario of sociable agents. set to 0 if you want socially indiferent agents (or in the fixed pairs scenario) 
                           DriftHRCenters = 0, # 1 or 0 for a drift of the center in space along the simulation (i.e. the changing environment scenarios), 0 is no drift.
                           DriftStrength = c(1,0), # If DriftHRCenters==1 this defines drift in m per day for X and Y values in the simulation 
                           DriftasOnce = 2, # If DriftHRCenters==1 this defines when does the drift occur. can be 0,1,2. if 0 it will drift daily; if 1 it will drift in the midle of the run if 2 it will drift from begining. 
                           PropDriftIndiv = 1, # Proportion of drifting individuals.
                           PairedAgents = 0, # If 1 then simulates the paired agent scenario where agents are paired in their intial location and show attraction to their pair only, indiferent to the others.
                           PairStartDist = 200, # If PairedAgents=1 this sets the Upper limit of the distance of the pairs. For the second individual in each pair the location will be random within this range from the first mate. 
                           Scl = 2000,
                           seed = NULL,
                           EtaCRW = 0.7, # The weight of the CRW component in the BCRW used to model the Indiv movement
                           StpSize_ind = 7, # Mean step lengths of individuals;
                           StpStd_ind = 5, # Standard deviations of step lengths of individuals 
                           Kappa_ind = 3, # Concentration parameters of von Mises directional distributions used for individuals' movement
                           ToPlot = 1,
                           quiet = F,
                           HRChangeRadius = 0, # Radius in which new HR center is to be selected from the next day
                           PairedHRMovement = 0, # Simulates BCRW with HR centers
                           HREtaCRW = 0.7, # parameters for HR BCRW
                           HRKappa_ind = 1, # controls how strongly vm distribution is centered on mu
                           HR_Social_Dist = 1000 # distance to bias toward another HR (soon to be removed)
                           ){
  
  # 1. Set the seed, if one is provided ----------------------------------------
  if(!is.null(seed)){
    set.seed(seed)
  }
  
  # 2. If requested, plot the step size distributions --------------------------
  if (ToPlot==1){
    # 1. Density plot of step lengths
    plot(density(rgamma(10000, 
                        shape = StpSize_ind^2/StpStd_ind^2, 
                        scale = StpStd_ind^2/StpSize_ind)))  
    
    # 2. Rose diagram of step directions
    CircStats::rose.diag(CircStats::rvm(n=10000, 
                                        mean = 0,
                                        k = Kappa_ind),
                         bins=72, 
                         pch = 16, 
                         cex = 1, 
                         shrink = 1)
  }
  
  # 3. Prepare variables for storing data --------------------------------------
  if(PairedAgents==1){ # If paired design, then no social perception range for other agents
    Soc_Percep_Rng <- 0
  } 
  # Calculate dimensions: time steps, rows
  N_timesteps <- Days*DayLength # Total number of time steps that will be simulated for each iteration
  N_Rows <- N*N_timesteps
  
  # Create data frame that will store the coordinates of each individual at each timestep
  XYind_log2 <- data.frame(
    indiv = as.factor(rep(seq(1:N), length.out = N_Rows, each = N_timesteps)),
    step = rep(seq(1:N_timesteps), length.out = N_Rows),
    Day = rep(seq(1:Days), length.out = N_Rows, each = DayLength),
    StepInDay = rep(seq(1:DayLength), length.out = N_Rows, each = 1),
    burst = as.factor(rep(seq(1:(Days*N)), 
                          length.out = N_Rows, each = DayLength)),
    x = NA, y = NA, pseudoSex = NA
  )
  
  # 4. Set starting conditions -------------------------------------------------
  startIndx <- 1
  Phi_ind <- rep(0, N) # Direction of the last step for the individuals
  XYind <- vector(mode = "list") # This list will store the matrices of locations of all individuals
  HRCent <- matrix(data = NA, nrow = N, ncol = 3) # Empty matrix that will store home range centers
  HRCent[,3] <- rep(c(1,2), length.out = nrow(HRCent)) # Assign sex 
  HRcenterDist <- rep(0, N/2) # Initial distance between pairs of agents
  
  # 5. Set drift of HR center (bias point) for drifting scenarios -----------
  # if (DriftasOnce == 0){ # 0 it will drift daily;
  #   Xdrift <- cumsum(rep(c(DriftStrength[1], rep(0, (DayLength-1))), Days)) # For each day i have the drift, then steps with no drift until the next day. I work with Cumsum since the drift grows 
  #   Ydrift <- cumsum(rep(c(DriftStrength[2], rep(0, (DayLength-1))), Days)) 
  #   print('using drift daily')
  # } else if (DriftasOnce == 1){ # if 1 it will drift in the middle of the run  
  #   Xdrift <- rep(0, N_timesteps)
  #   Xdrift[N_timesteps/2] <- DriftStrength[1]*Days
  #   Xdrift <- cumsum(Xdrift)
  #   Ydrift <- rep(0,N_timesteps)
  #   Ydrift[N_timesteps/2] <- DriftStrength[2]*Days
  #   Ydrift <- cumsum(Ydrift) 
  #   print('using drift from middle step of the run')
  # } else if (DriftasOnce == 2){ #if 2 it will drift from beginning.
  #   Xdrift <- rep(DriftStrength[1]*Days,N_timesteps)
  #   Ydrift <- rep(DriftStrength[2]*Days,N_timesteps)
  #   print('using drift from start')
  # }else{
  #   print('check your params')
  # }
  # 
  # DrifByStep <- rbind(Xdrift, Ydrift)
  # 
  # # Determining individuals to drift.
  # DriftingYorN <- c(rep(1, PropDriftIndiv*N),
  #                   rep(-1, N-PropDriftIndiv*N))
  # DriftingYorN <- sample(DriftingYorN, N)
  # 
  # # Setting the HR centerDrift for this time step (cumulative from beginning)
  # if(DriftHRCenters==1){
  #   CurDrift <- DrifByStep[,Curr_timestep]
  #   names(CurDrift) <- NULL
  # }else{
  #   CurDrift <- c(0,0)
  # }
  
  # 6. Set initial conditions for individuals ----------------------------------
  # Loop on individuals: set initial conditions
  for (k in 1:N) {
    # Place agents in their initial positions
    XYind[[k]] <- matrix(rep(NA, 2*N_timesteps), ncol = 2) # This matrix will store the location of each individual
    # Set random X and Y locations and log the HR center
    XYind[[k]][1, ] <-  c(runif(n=1, min=-Scl/3, max=Scl/3), 
                          runif(n=1, min=-Scl/3, max=Scl/3 )) 
    HRCent[k, 1:2] <- XYind[[k]][1, ]
    
    # If paired scenario: switch every other individual to be a pair with nearby location to the previous one
    if((PairedAgents == 1) & (k %% 2 == 0)){
      # Setting the start tlocation for the pair
      XYind[[k]][1, ] <- XYind[[k-1]][1, ]+c(runif(n=2, min=-PairStartDist/2, 
                                                   max=PairStartDist/2)) 
      # Storing the initial distance from this pair
      HRcenterDist[k/2] <- round(dist(rbind(XYind[[k-1]][1, ], c(XYind[[k]][1, ]))),
                                 digit = 1) 
      print(paste("pair:",k-1,"and",k, "distance",HRcenterDist[k/2] ))
    }
    # # If plotting, then make the plot: # XXX the plotting here doesn't work
    # if (ToPlot == 1){
    #   plot(sp::SpatialPoints(coords = matrix(data = XYind[[k]][1, ], nrow = 1)), add = T,
    #        pch = HRCent[k, 3]+23, col = HRCent[k, 3], bg = HRCent[k, 3], cex = 1.5) 
    #   if((PairedAgents == 1) & (k %% 2 == 0)){ # Plot also line between pairs
    #     plot(sp::SpatialLines(list(sp::Lines(list(sp::Line(rbind(XYind[[k-1]][1, ],
    #                                                              c(XYind[[k]][1, ])))), 
    #                                          ID=k/2) )), add=T) # This is the river 'line' 
    #   } # Plot also line between pairs
    # } # Plot the line
  } # End loop on individuals 
  
  # If the HR's are going to be changing (sim2 or sim3), create a list to store daily HR centers, and set the initial values from HRCent above.
  if(HRChangeRadius > 0){
    HRCentPerDay <- list()
    HRCentPerDay[[1]] <- HRCent # set initial HR centers
    dayCount <- 1
  }
  
  # If HRs will be moving in pairs... (should probably get rid of this)
  # if(PairedHRMovement > 0){
  #   HRPerTimestep <- list()
  #   HRPhi_ind <- rep(0, N) # Direction of the last step for the HRs
  #   for (k in 1:N){
  #     HRPerTimestep[[k]] <- matrix(rep(NA, 2 * N_timesteps), ncol = 2)
  #     HRPerTimestep[[k]][1, ] <- HRCent[k, 1:2] # set initial HR centers
  #   }
  # }
  
  # 7. Run the simulation ----------------------------------
  # Loop on time steps and individuals to run the simulation
  for(Curr_timestep in 1:(N_timesteps-1)){
    
    ### If it's a new day, move the HR center (sim2 and sim3) ###
    # XXX this should be an if statement for sim2 specifically, OR should add code for sim3 in this same loop.
    newDay <- Curr_timestep %% DayLength == 0 ## && and %% are for scalars not vectors, change every day
    if(HRChangeRadius > 0 && newDay){ # If this is the start of a new day... 
      # Increment the day count.
      dayCount <- dayCount + 1
      
      # Choose a random step (within a circle) for that individual's home range to take (angle and step length)
      randomAngles <- runif(N, min=0, 2 * pi) # sample angle from 0-2pi per individual
      randomLengths <- HRChangeRadius * sqrt(runif(N)) # https://stackoverflow.com/questions/5837572/generate-a-random-point-within-a-circle-uniformly #
      
      # Get random XY (based on the above angle and step length) to add to the existing HR to move it.
      randomX <- randomLengths * cos(randomAngles)
      randomY <- randomLengths * sin(randomAngles)
      randomXY <- cbind(randomX, randomY) 
      
      # Move the HR centers
      HRCentPerDay[[dayCount]] <- HRCentPerDay[[dayCount-1]]
      HRCentPerDay[[dayCount]][, 1:2] <- HRCentPerDay[[dayCount]][, 1:2] + randomXY # move HR centers
    }
 
    # Calculate social distance to other individuals
    for(Curr_indv in 1:N){
      # Compute distance of this individual to all other individuals
      Dist <- rep(NA, N) 
      for(Other_indv in 1:N){
        Dist[Other_indv] <- stats::dist(rbind(XYind[[Other_indv]][Curr_timestep,],
                                      c(XYind[[Curr_indv]][Curr_timestep,])))
      }
      Dist[Dist == 0] <- NA # Remove distance to self
      
      # if(PairedHRMovement > 0){
      # ## distance to other HR centers ## 
      #   
      #   HRDist <- rep(NA,N ) 
      #   for(ii in 1:N){
      #     HRDist[ii] <- stats::dist(rbind(HRPerTimestep[[ii]][Curr_timestep, ],
      #                                   c(HRPerTimestep[[Curr_indv]][Curr_timestep, ])))
      #   }
      #   HRDist[Dist==0] <- NA # Getting rid of the distance to self
      # }
      ## start generation of indiv location at time k+1
      
      ##### selecting direction ##########
      # Calculating the direction to the initial location (bias point )+ now with drift for the current step
      
      # Set individual bias points in different ways depending on method
      if(HRChangeRadius > 0)
        BiasPoint <- HRCentPerDay[[dayCount]][Curr_indv, 1:2] # if HR changes per day, sample by day
      # else if(PairedHRMovement > 0)
      #   BiasPoint <- HRPerTimestep[[Curr_indv]][Curr_timestep, ] # if HR changes by timestep, sample by timestep (soon to be removed)
      else
        BiasPoint <- (XYind[[Curr_indv]][1, ] + CurDrift*DriftingYorN[Curr_indv]) # This bias point is the origin+ the current cumulative bias
        # BiasPoint <- HRCentPerDay[[1]][Curr_indv, 1:2] above is the original code, though this might work
      
      if((PairedAgents == 1) & (Curr_indv %% 2 == 1)){
        # Updating bias point as the mean of HR center and mate's last location
        BiasPoint <- colMeans(rbind(BiasPoint, XYind[[Curr_indv+1]][Curr_timestep, ]))
      } 
      if((PairedAgents == 1) & (Curr_indv %% 2 == 0)){
        # Update bias point as the mean of HR center and mate's previous location (before last step since he moved already)  
        BiasPoint <- colMeans(rbind(BiasPoint, XYind[[Curr_indv-1]][Curr_timestep-1, ]))
      } 
      
      # If another individual is within social perception range...
      if(min(Dist, na.rm = T) < Soc_Percep_Rng){
        # if i want the mean of direction to the HR and nearest neighbor: 
        # BiasPoint <- colMeans(rbind(BiasPoint, XYind[[which.min(Dist)]][Curr_timestep, ]))} # Then updated  this for the bias also
        # Then bias toward that other individual.
        BiasPoint <- XYind[[which.min(Dist)]][Curr_timestep, ]
      }
      
      # Set direction to the chosen bias point
      coo <- BiasPoint - XYind[[Curr_indv]][Curr_timestep, ] 
      # Set direction:
      mu <- Arg(coo[1] + (0+1i) * coo[2]) # XXXK what is this? Direction, I think.
      # Make sure direction is not negative:
      if(mu < 0){
        mu <- mu + 2 * pi  
      } 
      mu.av <- Arg(EtaCRW * exp(Phi_ind[Curr_indv] * (0+1i)) + (1 - EtaCRW) * exp(mu * (0+1i))) # Bias to initial location + CRW to find the von mises center for next step
      Phi_ind[Curr_indv] <- CircStats::rvm(n=1, mean = mu.av, k = Kappa_ind)#choosing curr step direction from vonMises centered around the direction selected above 
      
      ##### Performing the step #########
      # Selection of step size for this indiv in this state from the specific gamma          
      step.len <- stats::rgamma(1, shape = StpSize_ind^2/StpStd_ind^2, 
                                scale = StpStd_ind^2/StpSize_ind)
      step <- step.len * c(Re(exp((0+1i) * Phi_ind[Curr_indv])), 
                           Im(exp((0+1i) * Phi_ind[Curr_indv])))
      XYind[[Curr_indv]][Curr_timestep + 1, ] <- XYind[[Curr_indv]][Curr_timestep, ] + step # The indiv's next location
      
      # Move HR according to vm (soon to be removed) #
      # if(PairedHRMovement > 0){
      #   if(min(HRDist, na.rm=T) < HR_Social_Dist)
      #     HRBiasPoint <- HRPerTimestep[[which.min(HRDist)]][Curr_timestep, ] # bias to closest HR in range
      #   else
      #     HRBiasPoint <- HRPerTimestep[[Curr_indv]][1, ] # otherwise bias to original HR position
      #   HRcoo <- HRBiasPoint - HRPerTimestep[[Curr_indv]][Curr_timestep, ] # get direction from HR current position to bias
      #   HRmu <- Arg(coo[1] + (0+1i) * coo[2])
      #   if(HRmu < 0){
      #     HRmu <- HRmu + 2 * pi  
      #   } 
      #   HRmu.av <- Arg(HREtaCRW * exp(HRPhi_ind[Curr_indv] * (0+1i)) + (1 - HREtaCRW) * exp(mu * (0+1i)))
      #   HRPhi_ind[Curr_indv] <- CircStats::rvm(n=1, mean = mu.av, k = HRKappa_ind)
      #   step.len <- stats::rgamma(1, shape = StpSize_ind^2/StpStd_ind^2,  # step sizes could be changed ?
      #                             scale = StpStd_ind^2/StpSize_ind)
      #   step <- step.len * c(Re(exp((0+1i) * HRPhi_ind[Curr_indv])), 
      #                        Im(exp((0+1i) * HRPhi_ind[Curr_indv])))
      #   HRPerTimestep[[Curr_indv]][Curr_timestep + 1, ] <- HRPerTimestep[[Curr_indv]][Curr_timestep, ] + step
      # }
      
      ###### ploting the steps ########
      if(Curr_indv <= length(Color_indv) & ToPlot==1 ){
        # Plot only up to 6 indiv and only if ToPlot==1
        StepAsLine <- sp::SpatialLines(list(sp::Lines(list(sp::Line(XYind[[Curr_indv]][Curr_timestep:(Curr_timestep+1) , ])), 
                                                      ID = Curr_timestep) )) # Convert line to spatial object for plot
        plot(StepAsLine, add = T, col = Color_indv[Curr_indv], lwd = 4) # Plot the buffer
      } # Plot only up to 6 indiv
      
    } # End loop on invdividuals
    if(quiet == F){
      print(c("done with timestep", Curr_timestep, "out of", N_timesteps))
    }
  } # End loop on time steps
  
  #### after the nested loops -how many times out of the box? ####
  for(k in 1:N){        
    endIndx <- startIndx + N_timesteps-1
    XYind_log2[c(startIndx:endIndx), c("x","y")] <- XYind[[k]]        
    XYind_log2[c(startIndx:endIndx), c("pseudoSex")] <- HRCent[k,3] # Storing the sex of this agent based on the HR center it got was it a male1 or a female2 there?
    startIndx <- endIndx+1
  }        
  OutOfTheBox <- sum((XYind_log2$x<  -Scl/2) | 
                       (XYind_log2$x>   Scl/2) |
                       (XYind_log2$y<  -Scl/2) |
                       (XYind_log2$y>   Scl/2) ) /length(XYind_log2$x)
  print(paste("out of the box rate", round(OutOfTheBox, digit = 5)))
  
  # Create a list for output with three slots: hr centers, and xy coordinates
  rName <- paste("sim", N_timesteps, N, 100*EtaCRW, StpSize_ind, DriftHRCenters, ".rdata", sep = "_")
  matlabName <- "xyFromSimulationForSNanalysis.mat" # keeping the old format for compatibility with Orr's old code
 
  # determine which form of HR centers to return #
  if(HRChangeRadius > 0)
    HRReturn <- HRCentPerDay
  else if(PairedHRMovement > 0)
    HRReturn <- HRPerTimestep
  else
    HRReturn <- HRCent
  
  out <- list("rName" = rName, "matlabName" = matlabName, "HRCent" = HRReturn, "XY" = XYind_log2)
  return(out)
}

#test <- simulateAgents(N = 5, Days = 10)
# save(list("HRCent" = HRCent, "XY" = XYind_log2), file = Name1)
# save(list=ls(),file=Name1)
# R.matlab::writeMat(con = Name2, XY = XYind_log2, HRCent = HRCent) 

# 2. fix_times ------------------------------------------------------------
# changes day and step to posix. This used to be load_data. You now have to do the loading by yourself. It's too weird to include the process of loading a .Rda file in a function because of the weird naming thing. See implementation of this in workflow.R
# The input data is the $XY portion of the list returned by simulateAgents().

# XXXK: need to talk to Orr about this sampling interval. Here is what was written in Ryan's code:
# SAMPLING_INTERVAL <- 10 # "minutes", from matlab code; 10 minutes per timestep with 50 timesteps gives about 8hrs of data
# KG 2023-09-01 For now I have added sampling_interval as a parameter in this function.
# XXXK

fix_times <- function(simulation_data, sampling_interval = 10){
  start_time <- as.POSIXct("2023-08-11 23:50")  # note simulation data starts on day 1 step 1 so the mindate will be 8-13 00:00
  simulation_data <- simulation_data %>%
    dplyr::mutate(datetime = start_time + lubridate::days(Day) + lubridate::minutes(StepInDay * sampling_interval)) %>%
    dplyr::select(indiv, x, y, datetime)
  return(simulation_data)
}

# 3. get_edgelist ---------------------------------------------------------
# gets network graph
get_edgelist <- function(data, idCol, dateCol){
  if(is.data.frame(data))
    data <- data.table::setDT(data)
  timegroup_data <- spatsoc::group_times(data, datetime = dateCol, threshold = "10 minutes") # could be 4 minutes; see Window variable in matlab code
  spatsoc::edge_dist(timegroup_data, threshold = 14, id = idCol, coords = c('x','y'), timegroup = "timegroup", returnDist = FALSE, fillNA = FALSE)
}

# 5 rotate_data_table ------------------------------------------------------------
# Note: this no longer actually takes a data_table, just keeping the name for consistency with previous.
# Unlike the previous function, this one requires that you separate the dates and times into separate columns beforehand.
rotate_data_table <- function(dataset, shiftMax, idCol = "indiv", dateCol = "date", timeCol = "time"){
  indivList <- dataset %>%
    group_by(.data[[idCol]]) %>%
    group_split(.keep = T)
  joined <- vector(mode = "list", length = length(indivList))
  for(indiv in 1:length(indivList)){
    x <- indivList[[indiv]]
    shift <- sample(-(shiftMax):shiftMax, size = 1)
    #cat(shift, "\n")
    # get all unique days that show up
    days <- sort(unique(x[[dateCol]]))
    
    # get min and max dates to shift around (the "poles" of the conveyor)
    selfMinDate <- min(days, na.rm = T)
    selfMaxDate <- max(days, na.rm = T)
    
    # create a total sequence of dates to select from
    daysFilled <- seq(lubridate::ymd(selfMinDate), lubridate::ymd(selfMaxDate), by = "day")
    # converting to numbers so we can use %%--which dates are the ones we started with?
    vec <- which(daysFilled %in% days)
    shiftedvec <- vec + shift # shift
    new <- (shiftedvec - min(vec)) %% (max(vec)-min(vec)+1)+1 # new dates as numbers
    shiftedDates <- daysFilled[new] # select those dates from the possibilities
    
    # Make a data frame to hold the old and new dates
    daysDF <- bind_cols({{dateCol}} := days, 
                        "newDate" = shiftedDates,
                        shift = shift)
    nw <- left_join(x, daysDF, by = dateCol)
    
    if(!is.null(timeCol)){
      nw$newdatetime <- lubridate::ymd_hms(paste(nw$newDate, nw[[timeCol]]))
    }
    joined[[indiv]] <- nw
  }
  out <- purrr::list_rbind(joined)
  return(out)
}

# 6. get_stats ------------------------------------------------------------
get_stats <- function(edgelist, data){
  associations <- edgelist %>%
    dplyr::count(ID1) %>%
    dplyr::rename("associations" = "n")
  
  degree <- edgelist %>%
    dplyr::group_by(ID1) %>%
    dplyr::summarise(degree = n_distinct(ID2), .groups = "drop")
  
  sri_per_edge <- calcSRI(dataset = data, edges = edgelist, idCol = "indiv", timegroupCol = "timegroup")
  
  mean_sri_and_strength <- sri_per_edge %>%
    dplyr::group_by(ID1) %>%
    dplyr::summarise(mean_sri = mean(sri),
                     strength = sum(sri, na.rm = T), .groups = "drop")
  
  stats <- dplyr::inner_join(associations, degree, by = dplyr::join_by(ID1)) %>%
    dplyr::inner_join(., mean_sri_and_strength, by=dplyr::join_by(ID1))
  return(stats)
}

# 7. mean_stats -----------------------------------------------------------
mean_stats <- function(stats){
  mean_stats <- stats %>%
    dplyr::summarise(mean_associations = mean(associations), 
                     mean_degree = mean(degree), 
                     mean_sri = mean(mean_sri), 
                     mean_strength =mean(strength),
                     .groups = "drop")
  return(mean_stats)
}


# 8. get_realization_data -------------------------------------------------
get_realization_data <- function(simulation_data, n, quiet = F){ #XXXK: need to generalize idCol and dateCol, but I don't know how to do that with data.table.
  realization_data <- data.frame()
  time_to_rotate <- Sys.time()
  for(x in 1:n){
    if(quiet == FALSE){
      print(paste("Working on realization", x))
    }
    
    rotated_data <- rotate_data_table(simulation_data, idCol = "indiv", dateCol = "datetime")
    rotated_edgelist <- get_edgelist(rotated_data, idCol = "indiv", dateCol = "datetime")
    stats <- get_stats(rotated_edgelist)
    average_stats <- mean_stats(stats)
    realization_data <- rbind(realization_data, average_stats)
  }
  print(Sys.time() - time_to_rotate)
  return(realization_data)
}

# calcSRI -----------------------------------------------------------------
# XXX This is pasted in from vultureUtils for now because I was having trouble installing it. Need to fix that.
calcSRI <- function(dataset, edges, idCol = "Nili_id", timegroupCol = "timegroup", dateCol = "datetime"){
  if(!(timegroupCol %in% names(dataset))){
    if(is.data.frame(dataset))
      dataset <- data.table::setDT(dataset)
    dataset <- spatsoc::group_times(dataset, datetime = dateCol, threshold = "10 minutes")
  }
  
  # setup for time warning
  # cat("\nComputing SRI... this may take a while if your dataset is large.\n")
  start <- Sys.time()
  
  # arg checks
  checkmate::assertSubset(timegroupCol, names(dataset))
  checkmate::assertSubset(idCol, names(dataset))
  checkmate::assertDataFrame(dataset)
  checkmate::assertDataFrame(edges)
  
  edges <- dplyr::as_tibble(edges)
  
  ## get individuals per timegroup as a list
  # Info about timegroups and individuals, for SRI calculation
  timegroupsList <- dataset %>%
    dplyr::select(tidyselect::all_of(c(timegroupCol, idCol))) %>%
    dplyr::mutate({{idCol}} := as.character(.data[[idCol]])) %>%
    dplyr::distinct() %>%
    dplyr::group_by(.data[[timegroupCol]]) %>%
    dplyr::group_split() %>%
    purrr::map(~.x[[idCol]])
  
  ## get unique set of timegroups
  timegroups <- unique(dataset[[timegroupCol]])
  
  ## get all unique pairs of individuals
  inds <- as.character(unique(dataset[[idCol]]))
  allPairs <- expand.grid(ID1 = as.character(inds), ID2 = as.character(inds), stringsAsFactors = F) %>%
    dplyr::filter(ID1 < ID2)
  
  # wide data
  datasetWide <- dataset %>%
    sf::st_drop_geometry() %>%
    dplyr::select(tidyselect::all_of(c(timegroupCol, idCol))) %>%
    dplyr::distinct() %>%
    dplyr::mutate(val = TRUE) %>%
    tidyr::pivot_wider(id_cols = tidyselect::all_of(timegroupCol), names_from = tidyselect::all_of(idCol),
                       values_from = "val", values_fill = FALSE)
  
  ## get SRI information
  dfSRI <- purrr::pmap_dfr(allPairs, ~{
    a <- .x
    b <- .y
    colA <- datasetWide[,a]
    colB <- datasetWide[,b]
    nBoth <- sum(colA & colB)
    x <- nrow(unique(edges[edges$ID1 %in% c(a, b) & edges$ID2 %in% c(a, b), timegroupCol]))
    yab <- nBoth - x
    sri <- x/(x+yab)
    if(is.infinite(sri)){
      sri <- 0
    }
    dfRow <- data.frame("ID1" = a, "ID2" = b, "sri" = sri)
    return(dfRow)
  })
  
  # complete the time message
  end <- Sys.time()
  duration <- difftime(end, start, units = "secs")
  cat(paste0("SRI computation completed in ", round(duration, 3), " seconds.\n"))
  return(dfSRI)
}


