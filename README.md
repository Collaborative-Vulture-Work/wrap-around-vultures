data/ - saved data used for analyses
docs/ - documents used to navigate original 2014 code
fig/ - figures for analyses
simdata/ - saved simulations for testing

All relevent scripts to this project can be found in scripts/

functions.r - Function definitions used for the workflow of the project
    simulateAgents - modified code from 2014 paper used to simulate agents using von mises distribution and timesteps
    fix_times - converts Day and Step columns from simulation to POSIXct for spatsoc network graph
    get_edgelist - gets edgelist of timegrouped interactions for data
    rotate_data_table - rotates dates per individual on data from fix_times
    get_stats - gets number of associations, degree, mean sri and strength from edgelist per individual
    mean_stats - gets the mean of previous stats
    calcSRI - calculates SRI values for individuals
    get_tortuosity - gets tortuosity per individual path, given raw XY data from simulateAgents

workflow.r - Workflow to create data used for analyses. Runs sociable and non-sociable simulations using simulateAgents and
             different scenarios. Then, the simulations are put through the conveyor method and randomization method to compare
             the difference in stats and generate graphs.

shiftMax_analysis.R - Script to determine shiftMax value for conveyor method for discernable difference between randomization.

socialAttraction.R - Script to compare randomization and conveyor method for different levels of social attraction

tortuosityAnalysis.R - Script used to determine step size for moving HR centers (bias points), based on tortuosity and social weight