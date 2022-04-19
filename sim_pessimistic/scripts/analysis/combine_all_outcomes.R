setwd("~/Desktop/ResearchTrajectory/sim_pessimistic")

# read in outcome data of all trajectories

load("data/outcomes_standard_across_traj_Carneiro.RData")  
res_standard <- outcomes_standard_across_traj

load("data/outcomes_sesoi_across_traj_Carneiro.RData")
res_sesoi <- outcomes_sesoi_across_traj

# combine data from all trajectories from both distributions
res_Carneiro <- 
  bind_rows(res_standard,
            res_sesoi)

# save(res_Carneiro,
#      file = "./data/all_outcomes_Carneiro.RData")



