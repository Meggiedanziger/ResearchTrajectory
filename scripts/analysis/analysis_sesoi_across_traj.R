setwd("~/Desktop/ResearchTrajectory/")

##################################################
######### for confirmatory SESOI data with 95 % CI 
##################################################

# source functions and scripts for analysis
source("./scripts/analysis/prior_probs_for_analysis.R")
source("./scripts/analysis/functions_for_analysis.R")
load("./data/sesoi_data_after_replication.RData")
load("./data/outcomes_sesoi_exploration.RData")

dat <- sesoi_data_after_replication

dat$SESOI <- as.numeric(dat$SESOI)

dat$selection <- 
  ifelse(dat$p_value <= .05, 1, 0)

names(dat)[7] <- "hedges"

outcomes_sesoi_across_traj <- compute_outcomes_across_trajectory()

outcomes_sesoi_across_traj <-
  outcomes_sesoi_across_traj %>% 
  mutate(trajectory = "SESOI") %>% 
  select(trajectory, everything())

# save outcome table
# save(outcomes_sesoi_across_traj,
#      file = "./data/outcomes_sesoi_across_traj.RData")


###########################################################
######### for confirmatory SESOI data with 80 % and 50 % CI 
###########################################################

# source functions and scripts for analysis
source("./scripts/analysis/prior_probs_for_analysis.R")
source("./scripts/analysis/functions_for_analysis.R")
load("./data/sesoi_data_after_replication_varied_CI.RData")
load("./data/outcomes_sesoi_exploration_CI.RData")

dat <- sesoi_data_after_replication_varied_CI

dat$SESOI <- as.numeric(dat$SESOI)

dat$selection <- 
  ifelse(dat$p_value <= .05, 1, 0)

names(dat)[7] <- "hedges"

outcomes_sesoi_across_traj_varied_CI <- compute_outcomes_across_trajectory_CI()

outcomes_sesoi_across_traj_varied_CI <-
  outcomes_sesoi_across_traj_varied_CI %>% 
  mutate(trajectory = "SESOI") %>% 
  select(trajectory, everything())

# save outcome table
# save(outcomes_sesoi_across_traj_varied_CI,
#      file = "./data/outcomes_sesoi_across_traj_varied_CI.RData")


###########################################################
########### for confirmatory SESOI data with 80 % power for
########### confirmatory sample size ######################
###########################################################

# source functions and scripts for analysis
source("./scripts/analysis/prior_probs_for_analysis.R")
source("./scripts/analysis/functions_for_analysis.R")
load("./data/sesoi_data_after_replication_high_power.RData")
load("./data/outcomes_sesoi_exploration.RData")

outcomes_sesoi_exp <-
  outcomes_sesoi_exp %>% 
  filter(SESOI != 0.1)

dat <- sesoi_data_after_replication_high_power

dat$SESOI <- as.numeric(dat$SESOI)

dat$selection <- 
  ifelse(dat$p_value <= .05, 1, 0)

names(dat)[7] <- "hedges"

outcomes_sesoi_across_traj_high_power <- compute_outcomes_across_trajectory_high_power()

outcomes_sesoi_across_traj_high_power <-
  outcomes_sesoi_across_traj_high_power %>% 
  mutate(trajectory = "SESOI") %>% 
  select(trajectory, everything())

# save outcome table
# save(outcomes_sesoi_across_traj_high_power,
#      file = "./data/outcomes_sesoi_across_traj_high_power.RData")


