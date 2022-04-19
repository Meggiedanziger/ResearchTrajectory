setwd("~/Desktop/ResearchTrajectory/")

###################################################
# for confirmatory SESOI STANDARD data with 95 % CI 
###################################################

# source functions and scripts for analysis
source("./scripts/analysis/prior_probs_for_analysis.R")
source("./scripts/analysis/functions_for_analysis.R")
load("./data/sesoi_standard_data_after_replication.RData")
load("./data/outcomes_sesoi_exploration.RData")

dat <- sesoi_standard_data_after_replication

dat$SESOI <- as.numeric(dat$SESOI)

dat$selection <- 
  ifelse(dat$p_value <= .05, 1, 0)

names(dat)[7] <- "hedges"

outcomes_sesoi_standard_across_traj <- compute_outcomes_across_trajectory()

outcomes_sesoi_standard_across_traj <-
  outcomes_sesoi_standard_across_traj %>% 
  mutate(trajectory = "SESOI-Standard") %>% 
  select(trajectory, everything())

# save outcome table
# save(outcomes_sesoi_standard_across_traj,
#      file = "./data/outcomes_sesoi_standard_across_traj.RData")



