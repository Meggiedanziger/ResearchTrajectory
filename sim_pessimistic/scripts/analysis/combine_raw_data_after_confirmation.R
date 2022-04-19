setwd("~/Desktop/ResearchTrajectory/sim_pessimistic")

library(tidyverse)

#################################################
################ SESOI TRAJECTORY ###############
#################################################

### with 95 % CI ###

# read in data from all SESOI trajectories
read_in_SESOI <- function(k) {
  
  SESOI_data <- read_csv(paste("./data/raw/confirmation_sesoi_method2_", k, sep = ""))
}

sets_equiv <- c("0.5", "1.0")

SESOI_traj_data <- list()

for (i in sets_equiv) {
  
  SESOI_traj_data[[i]] <- read_in_SESOI(i)
  print(i)
  
}

# add column SESOI with respective SESOI
SESOI_traj_data$`0.5`$SESOI <- 0.5
SESOI_traj_data$`1.0`$SESOI <- 1.0

# concatenate data of all SESOI trajectories
sesoi_data_after_replication <-
  bind_rows(SESOI_traj_data$`0.5`,
            SESOI_traj_data$`1.0`)

# save(sesoi_data_after_replication,
#      file = "./data/sesoi_data_after_replication_Carneiro.RData")


#################################################
############## STANDARD TRAJECTORY ##############
#################################################

standard_data <- 
  read_csv("./data/raw/confirmation_sig_p0.05_method1")

# add column pval_thres with respective p-value threshold
standard_data$pval_threshold <- 0.05

standard_data_after_replication <- standard_data

# save(standard_data_after_replication,
#      file = "./data/standard_data_after_replication_Carneiro.RData")








