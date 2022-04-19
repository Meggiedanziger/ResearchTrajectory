setwd("~/Desktop/ResearchTrajectory/")

##################################################
########## for exploratory SESOI data with 95 % CI 
##################################################

# source functions and scripts for analysis
source("./scripts/analysis/prior_probs_for_analysis.R")
source("./scripts/analysis/functions_for_analysis.R")
load("./data/exploratory_data_sesoi.RData")

dat <- exploratory_data_sesoi

dat$SESOI <- as.numeric(dat$SESOI)

# use function compute_outcomes_exploration() to compute FPR, FNR, PPV, NPV etc.
outcomes_sesoi_exp <- compute_outcomes_exploration()

outcomes_sesoi_exp <-
  outcomes_sesoi_exp %>% 
  mutate(trajectory = "SESOI") %>% 
  select(trajectory, everything())

# save(outcomes_sesoi_exp,
#      file = "./data/outcomes_sesoi_exploration.RData")


##################################################
# for exploratory SESOI data with 50 % and 80 % CI 
##################################################

load("./data/exploratory_data_sesoi_varied_CI.RData")

dat_ci <- exploratory_data_sesoi_varied_CI

class(dat_ci$SESOI)

dat_ci$SESOI <- as.numeric(dat_ci$SESOI)

# use function compute_outcomes_exploration_CI() to compute FPR, FNR, PPV, NPV etc.
outcomes_sesoi_exp_CI <- compute_outcomes_exploration_CI()

outcomes_sesoi_exp_CI <-
  outcomes_sesoi_exp_CI %>% 
  mutate(trajectory = "SESOI") %>% 
  select(trajectory, everything())

# save(outcomes_sesoi_exp_CI,
#      file = "./data/outcomes_sesoi_exploration_CI.RData")


