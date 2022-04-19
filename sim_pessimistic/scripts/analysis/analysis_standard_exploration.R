setwd("~/Desktop/ResearchTrajectory/sim_pessimistic")

# source functions and scripts for analysis
source("./scripts/analysis/prior_probs_for_analysis.R")
source("./scripts/analysis/functions_for_analysis.R")
load("./data/exploratory_data_sig_0.05_Carneiro.RData")

dat_sig <- exploratory_data_sig

dat <-
  bind_rows(dat_sig, 
            dat_sig)

dat$SESOI <- rep(c(0.5, 1.0),
                 each = nrow(dat_sig))

# use function compute_outcomes_exploration_sig() to compute FPR, FNR, PPV, NPV etc.
outcomes_standard_exp <- compute_outcomes_exploration_standard()

outcomes_standard_exp <-
  outcomes_standard_exp %>% 
  mutate(trajectory = "Standard") %>% 
  select(trajectory, everything())

# save(outcomes_standard_exp,
#      file = "./data/outcomes_standard_exploration_Carneiro.RData")





