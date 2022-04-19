setwd("~/Desktop/ResearchTrajectory/")

# read in outcome data of all trajectories

load("./data/outcomes_standard_across_traj.RData")
res_standard <- outcomes_standard_across_traj

load("./data/outcomes_sesoi_across_traj.RData")
res_sesoi <- outcomes_sesoi_across_traj

load("./data/outcomes_standard_sesoi_across_traj.RData")
res_standard_sesoi <- outcomes_standard_sesoi_across_traj

load("./data/outcomes_sesoi_standard_across_traj.RData")
res_sesoi_standard <- outcomes_sesoi_standard_across_traj



# combine data from all trajectories from both distributions
res <- 
  bind_rows(res_standard,
            res_standard_sesoi,
            res_sesoi, 
            res_sesoi_standard)

# save(res, file = "./data/all_outcomes.RData")



