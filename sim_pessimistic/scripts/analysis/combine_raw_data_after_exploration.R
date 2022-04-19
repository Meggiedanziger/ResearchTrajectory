setwd("~/Desktop/ResearchTrajectory/sim_pessimistic")

library(tidyverse)

#################################################
################ SESOI TRAJECTORY ###############
#################################################

### with 95 % CI ###

read_in_exploratory_data <- function(k) {
  
  exp_data <- read_csv(paste("./data/raw/exploratory_data_sesoi_", k, sep = ""))
}

sets_equiv <- c("0.5", "1.0")

exploratory_data <- list()

for (i in sets_equiv) {
  
  exploratory_data[[i]] <- read_in_exploratory_data(i)
  print(i)
  
}

sum(exploratory_data$`0.5`$selection == 1)
sum(exploratory_data$`1.0`$selection == 1)

exploratory_data_sesoi <-
  bind_rows(exploratory_data$`0.5`,
            exploratory_data$`1.0`)

exploratory_data_sesoi$SESOI <-
  rep(sets_equiv, each = 20000)

# save(exploratory_data_sesoi, 
#      file = "./data/exploratory_data_sesoi_Carneiro.RData")


#################################################
############## STANDRAD TRAJECTORY ##############
#################################################
  
exploratory_data_sig <- read_csv("./data/raw/exploratory_data_sig_0.05")

# save(exploratory_data_sig, 
#      file = "./data/exploratory_data_sig_0.05_Carneiro.RData")

