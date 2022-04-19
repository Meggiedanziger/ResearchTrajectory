setwd("~/Desktop/ResearchTrajectory/")

# source functions and scripts for analysis
source("./scripts/analysis/functions_for_analysis.R")
load("./data/standard_data_after_replication.RData")

dat_sig <- standard_data_after_replication

dat <-
  bind_rows(dat_sig, 
            dat_sig,
            dat_sig,
            dat_sig,
            dat_sig)

dat$SESOI <- 
  rep(c(0.1, 0.3, 0.5, 0.7, 1.0),
      each = nrow(dat_sig))

dat$selection <- 
  ifelse(dat$p_value <= .05, 1, 0)

names(dat)[7]  <- "hedges"
names(dat)[12] <- "pval_threshold"

outcomes_standard_rep <- compute_outcomes_rep_standard()

outcomes_standard_rep <-
  outcomes_standard_rep %>%
  mutate(trajectory = "Standard") %>%
  select(trajectory, everything())

# save(outcomes_standard_rep,
#      file = "./data/outcomes_standard_confirmation.RData")

