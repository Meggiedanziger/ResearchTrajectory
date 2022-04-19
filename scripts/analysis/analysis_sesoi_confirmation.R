setwd("~/Desktop/ResearchTrajectory/")

##################################################
######### for confirmatory SESOI data with 95 % CI 
##################################################

# source functions and scripts for analysis
source("./scripts/analysis/functions_for_analysis.R")
load("./data/sesoi_data_after_replication.RData")

dat <- sesoi_data_after_replication

dat$SESOI <- as.numeric(dat$SESOI)

dat$selection <- 
  ifelse(dat$p_value <= .05, 1, 0)

names(dat)[7] <- "hedges"

outcomes_sesoi_rep <- compute_outcomes_rep()

outcomes_sesoi_rep <-
  outcomes_sesoi_rep %>% 
  mutate(trajectory = "SESOI") %>% 
  select(trajectory, everything())

# save(outcomes_sesoi_rep,
#      file = "./data/outcomes_sesoi_confirmation.RData")

###########################################################
######### for confirmatory SESOI data with 80 % and 50 % CI 
###########################################################

load("./data/sesoi_data_after_replication_varied_CI.RData")

dat <- sesoi_data_after_replication_varied_CI

dat$SESOI <- as.numeric(dat$SESOI)

dat$selection <- 
  ifelse(dat$p_value <= .05, 1, 0)

names(dat)[7] <- "hedges"

outcomes_sesoi_varied_CI_rep <- compute_outcomes_rep_CI()

outcomes_sesoi_varied_CI_rep <-
  outcomes_sesoi_varied_CI_rep %>% 
  mutate(trajectory = "SESOI") %>% 
  select(trajectory, everything())

# save(outcomes_sesoi_varied_CI_rep,
#      file = "./data/outcomes_sesoi_confirmation_varied_CI.RData")

###########################################################
########### for confirmatory SESOI data with 80 % power for
########### confirmatory sample size ######################
###########################################################

load("./data/sesoi_data_after_replication_high_power.RData")

dat <- sesoi_data_after_replication_high_power

dat$SESOI <- as.numeric(dat$SESOI)

dat$selection <- 
  ifelse(dat$p_value <= .05, 1, 0)

names(dat)[7] <- "hedges"

outcomes_sesoi_rep_high_power <- compute_outcomes_rep()

outcomes_sesoi_rep_high_power <-
  outcomes_sesoi_rep_high_power %>% 
  mutate(trajectory = "SESOI") %>% 
  select(trajectory, everything())

# save(outcomes_sesoi_rep_high_power,
#      file = "./data/outcomes_sesoi_confirmation_high_power.RData")







