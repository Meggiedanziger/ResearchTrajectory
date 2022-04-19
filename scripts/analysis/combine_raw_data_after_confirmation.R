setwd("~/Desktop/ResearchTrajectory/")

library(tidyverse)

#################################################
################ SESOI TRAJECTORY ###############
#################################################

### with 95 % CI ###

# read in data from all SESOI trajectories
read_in_SESOI <- function(k) {
  
  SESOI_data <- read_csv(paste("./data/raw/confirmation_sesoi_method2_", k, sep = ""))
}

sets_equiv <- c("0.1", "0.3", "0.5", "0.7", "1.0")

SESOI_traj_data <- list()

for (i in sets_equiv) {
  
  SESOI_traj_data[[i]] <- read_in_SESOI(i)
  print(i)
  
}

# add column SESOI with respective SESOI
SESOI_traj_data$`0.1`$SESOI <- 0.1
SESOI_traj_data$`0.3`$SESOI <- 0.3
SESOI_traj_data$`0.5`$SESOI <- 0.5
SESOI_traj_data$`0.7`$SESOI <- 0.7
SESOI_traj_data$`1.0`$SESOI <- 1.0

# concatenate data of all SESOI trajectories
sesoi_data_after_replication <-
  bind_rows(SESOI_traj_data$`0.1`,
            SESOI_traj_data$`0.3`,
            SESOI_traj_data$`0.5`,
            SESOI_traj_data$`0.7`,
            SESOI_traj_data$`1.0`)

# save(sesoi_data_after_replication,
#      file = "./data/sesoi_data_after_replication.RData")


### with 50 % CI ###

read_in_SESOI <- function(k) {
  
  SESOI_data <- read_csv(paste("./data/raw/confirmation_sesoi_method2_CI_50_", k, sep = ""))
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
SESOI_traj_data_con_50 <-
  bind_rows(SESOI_traj_data$`0.5`,
            SESOI_traj_data$`1.0`)

# add column CI 
SESOI_traj_data_con_50$CI <- 50


### with 80 % CI ###

read_in_SESOI <- function(k) {
  
  SESOI_data <- read_csv(paste("./data/raw/confirmation_sesoi_method2_CI_80_", k, sep = ""))
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
SESOI_traj_data_con_80 <-
  bind_rows(SESOI_traj_data$`0.5`,
            SESOI_traj_data$`1.0`)

# add column CI 
SESOI_traj_data_con_80$CI <- 80

# combine exploratory data sesoi with 50 % and 80 % CI in one data set
sesoi_data_after_replication_varied_CI <-
  bind_rows(SESOI_traj_data_con_80,
            SESOI_traj_data_con_50)

# save(sesoi_data_after_replication_varied_CI,
#      file = "./data/sesoi_data_after_replication_varied_CI.RData")


### with 80 % power for sample size calculation for confirmatory study ###

read_in_SESOI <- function(k) {
  
  SESOI_data <- read_csv(paste("./data/raw/confirmation_sesoi_method2_power0.8_", k, sep = ""))
}

sets_equiv <- c("0.3", "0.5", "0.7", "1.0")

SESOI_traj_data <- list()

for (i in sets_equiv) {
  
  SESOI_traj_data[[i]] <- read_in_SESOI(i)
  print(i)
  
}

# add column SESOI with respective SESOI
SESOI_traj_data$`0.3`$SESOI <- 0.3
SESOI_traj_data$`0.5`$SESOI <- 0.5
SESOI_traj_data$`0.7`$SESOI <- 0.7
SESOI_traj_data$`1.0`$SESOI <- 1.0

# concatenate data of all SESOI trajectories
sesoi_data_after_replication_high_power <-
  bind_rows(SESOI_traj_data$`0.3`,
            SESOI_traj_data$`0.5`,
            SESOI_traj_data$`0.7`,
            SESOI_traj_data$`1.0`)

# save(sesoi_data_after_replication_high_power,
#      file = "./data/sesoi_data_after_replication_high_power.RData")


#################################################
############## STANDARD TRAJECTORY ##############
#################################################

# read in data from all Standard trajectories
read_in_standard <- function(k) {
  
  standard_data <- 
    read_csv(paste("./data/raw/confirmation_sig_method1_p", k, sep = ""))
}

sets_sig <- c("0.05", "0.1")

standard_traj_data <- list()

for (i in sets_sig) {
  
  standard_traj_data[[i]] <- read_in_standard(i)
  print(i)
  
}

# add column pval_thres with respective p-value threshold
standard_traj_data$`0.05`$pval_thres <- 0.05
standard_traj_data$`0.1`$pval_thres  <- 0.1

standard_data_after_replication <-
  bind_rows(standard_traj_data$`0.05`,
            standard_traj_data$`0.1`)

# save(standard_data_after_replication,
#      file = "./data/standard_data_after_replication.RData")


#################################################
########### SESOI STANDARD TRAJECTORY ###########
#################################################

# read in data from all SESOI trajectories
read_in_SESOI <- function(k) {
  
  SESOI_data <- read_csv(paste("./data/raw/confirmation_sesoi_method1_", k, sep = ""))
}

sets_equiv <- c("0.1", "0.3", "0.5", "0.7", "1.0")

SESOI_traj_data <- list()

for (i in sets_equiv) {
  
  SESOI_traj_data[[i]] <- read_in_SESOI(i)
  print(i)
  
}

# add column SESOI with respective SESOI
SESOI_traj_data$`0.1`$SESOI <- 0.1
SESOI_traj_data$`0.3`$SESOI <- 0.3
SESOI_traj_data$`0.5`$SESOI <- 0.5
SESOI_traj_data$`0.7`$SESOI <- 0.7
SESOI_traj_data$`1.0`$SESOI <- 1.0

# concatenate data of all SESOI trajectories
sesoi_standard_data_after_replication <-
  bind_rows(SESOI_traj_data$`0.1`,
            SESOI_traj_data$`0.3`,
            SESOI_traj_data$`0.5`,
            SESOI_traj_data$`0.7`,
            SESOI_traj_data$`1.0`)

# save(sesoi_standard_data_after_replication,
#      file = "./data/sesoi_standard_data_after_replication.RData")


#################################################
########### STANDARD SESOI TRAJECTORY ###########
#################################################

# read in data from all SESOI trajectories
read_in_standard <- function(k, m) {
  
  standard_data <- 
    read_csv(paste("./data/raw/confirmation_sig_p", k, "_method2_", m, sep = ""))
}

sets_sig <- c("0.05", "0.1")

sets_equiv <- c("0.1", "0.3", "0.5", "0.7", "1.0")

standard_traj_data <-
  
  foreach(i = sets_sig) %do% {
    
    helper <- list()
    
    for (j in sets_equiv) {
      
      helper[[j]] <- read_in_standard(i, j)
      
    }
    
    standard_traj_data <- helper 
    
  }

# add column SESOI with respective SESOI for p-value threshold = 0.05
standard_traj_data[[1]]$`0.1`$SESOI <- 0.1
standard_traj_data[[1]]$`0.3`$SESOI <- 0.3
standard_traj_data[[1]]$`0.5`$SESOI <- 0.5
standard_traj_data[[1]]$`0.7`$SESOI <- 0.7
standard_traj_data[[1]]$`1.0`$SESOI <- 1.0

# concatenate data of all trajectories for p-value threshold = 0.05
standard_traj_data_0.05_con <-
  bind_rows(standard_traj_data[[1]]$`0.1`,
            standard_traj_data[[1]]$`0.3`,
            standard_traj_data[[1]]$`0.5`,
            standard_traj_data[[1]]$`0.7`,
            standard_traj_data[[1]]$`1.0`)

# add column SESOI with respective SESOI for p-value threshold = 0.1
standard_traj_data[[2]]$`0.1`$SESOI <- 0.1
standard_traj_data[[2]]$`0.3`$SESOI <- 0.3
standard_traj_data[[2]]$`0.5`$SESOI <- 0.5
standard_traj_data[[2]]$`0.7`$SESOI <- 0.7
standard_traj_data[[2]]$`1.0`$SESOI <- 1.0

# concatenate data of all trajectories for p-value threshold = 0.1
standard_traj_data_0.1_con <-
  bind_rows(standard_traj_data[[2]]$`0.1`,
            standard_traj_data[[2]]$`0.3`,
            standard_traj_data[[2]]$`0.5`,
            standard_traj_data[[2]]$`0.7`,
            standard_traj_data[[2]]$`1.0`)

# add column pval_thres with respective p-value threshold
standard_traj_data_0.05_con$pval_threshold <- 0.05
standard_traj_data_0.1_con$pval_threshold  <- 0.1

# concatenate data of all SESOI trajectories
standard_sesoi_data_after_replication <-
  bind_rows(standard_traj_data_0.05_con,
            standard_traj_data_0.1_con)

# save(standard_sesoi_data_after_replication,
#      file = "./data/standard_sesoi_data_after_replication.RData")



