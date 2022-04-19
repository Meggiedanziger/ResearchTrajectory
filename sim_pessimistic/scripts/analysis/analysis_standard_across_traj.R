setwd("~/Desktop/ResearchTrajectory/sim_pessimistic")

##################################################
################### for confirmatory STANDARD data  
##################################################

# source functions and scripts for analysis
source("./scripts/analysis/prior_probs_for_analysis.R")
source("./scripts/analysis/functions_for_analysis.R")
load("./data/standard_data_after_replication_Carneiro.RData")
load("./data/outcomes_standard_exploration_Carneiro.RData")

dat_sig <- standard_data_after_replication

dat <-
  bind_rows(dat_sig, 
            dat_sig)

dat$SESOI <- 
  rep(c(0.5, 1.0),
      each = nrow(dat_sig))

dat$selection <- 
  ifelse(dat$p_value <= .05, 1, 0)

names(dat)[7] <- "hedges"

outcomes_standard_across_traj <- compute_outcomes_across_trajectory_standard()

outcomes_standard_across_traj <-
  outcomes_standard_across_traj %>% 
  mutate(trajectory = "Standard") %>% 
  select(trajectory, everything())

# save outcome table
# save(outcomes_standard_across_traj,
#      file = "./data/outcomes_standard_across_traj_Carneiro.RData")
