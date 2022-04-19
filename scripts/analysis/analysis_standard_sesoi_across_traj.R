setwd("~/Desktop/ResearchTrajectory/")

##################################################
############# for confirmatory STANDARD SESOI data  
##################################################

# source functions and scripts for analysis
source("./scripts/analysis/prior_probs_for_analysis.R")
source("./scripts/analysis/functions_for_analysis.R")
load("./data/standard_sesoi_data_after_replication.RData")
load("./data/outcomes_standard_exploration.RData")

dat <- standard_sesoi_data_after_replication

dat$selection <- 
  ifelse(dat$p_value <= .05, 1, 0)

names(dat)[7] <- "hedges"

outcomes_standard_sesoi_across_traj <- compute_outcomes_across_trajectory_standard()

outcomes_standard_sesoi_across_traj <-
  outcomes_standard_sesoi_across_traj %>% 
  mutate(trajectory = "Standard-SESOI") %>% 
  select(trajectory, everything())

# save outcome table
# save(outcomes_standard_sesoi_across_traj,
#      file = "./data/outcomes_standard_sesoi_across_traj.RData")

