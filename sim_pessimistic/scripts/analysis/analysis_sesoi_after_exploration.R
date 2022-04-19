setwd("~/Desktop/ResearchTrajectory/sim_pessimistic")

##################################################
########## for exploratory SESOI data with 95 % CI 
##################################################

# source functions and scripts for analysis
source("./scripts/analysis/prior_probs_for_analysis.R")
source("./scripts/analysis/functions_for_analysis.R")
load("./data/exploratory_data_sesoi_Carneiro.RData")

dat <- exploratory_data_sesoi

dat$SESOI <- as.numeric(dat$SESOI)

# use function compute_outcomes_exploration() to compute FPR, FNR, PPV, NPV etc.
outcomes_sesoi_exp <- compute_outcomes_exploration()

outcomes_sesoi_exp <-
  outcomes_sesoi_exp %>% 
  mutate(trajectory = "SESOI") %>% 
  select(trajectory, everything())

# save(outcomes_sesoi_exp,
#      file = "./data/outcomes_sesoi_exploration_Carneiro.RData")


