setwd("~/Desktop/ResearchTrajectory/")

library(tidyverse)

#################################################
################ SESOI TRAJECTORY ###############
#################################################

### with 95 % CI ###

read_in_exploratory_data <- function(k) {
  
  exp_data <- read_csv(paste("./data/raw/exploratory_data_sesoi_", k, sep = ""))
}

sets_equiv <- c("0.1", "0.3", "0.5", "0.7", "1.0")

exploratory_data <- list()

for (i in sets_equiv) {
  
  exploratory_data[[i]] <- read_in_exploratory_data(i)
  print(i)
  
}

sum(exploratory_data$`0.1`$selection == 1)
sum(exploratory_data$`0.3`$selection == 1)
sum(exploratory_data$`0.5`$selection == 1)
sum(exploratory_data$`0.7`$selection == 1)
sum(exploratory_data$`1.0`$selection == 1)

exploratory_data_sesoi <-
  bind_rows(exploratory_data$`0.1`,
            exploratory_data$`0.3`,
            exploratory_data$`0.5`,
            exploratory_data$`0.7`,
            exploratory_data$`1.0`)

exploratory_data_sesoi$SESOI <-
  rep(sets_equiv, each = 40000)

# save(exploratory_data_sesoi, 
#      file = "./data/exploratory_data_sesoi.RData")


### with 50 % CI ###

read_in_exploratory_data <- function(k) {
  
  exp_data <- read_csv(paste("./data/raw/exploratory_data_CI_50_sesoi_", k, sep = ""))
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
  rep(sets_equiv, each = 40000)

exploratory_data_sesoi$CI <- 50

exploratory_data_sesoi_CI_50 <- exploratory_data_sesoi


### with 80 % CI ###

read_in_exploratory_data <- function(k) {
  
  exp_data <- read_csv(paste("./data/raw/exploratory_data_CI_80_sesoi_", k, sep = ""))
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
  rep(sets_equiv, each = 40000)

exploratory_data_sesoi$CI <- 80

exploratory_data_sesoi_CI_80 <- exploratory_data_sesoi


# combine exploratory data sesoi with 50 % and 80 % CI in one data set

exploratory_data_sesoi_varied_CI <-
  bind_rows(exploratory_data_sesoi_CI_50,
            exploratory_data_sesoi_CI_80)

# save(exploratory_data_sesoi_varied_CI,
#      file = "./data/exploratory_data_sesoi_varied_CI.RData")


#################################################
############## STANDRAD TRAJECTORY ##############
#################################################

read_in_exploratory_data_sig <- function(k) {
  
  exp_data_sig <- read_csv(paste("./data/raw/exploratory_data_sig_", k, sep = ""))
}

sets_sig <- c("0.05", "0.1")

exploratory_data_sig <- list()

for (i in sets_sig) {
  
  exploratory_data_sig[[i]] <- read_in_exploratory_data_sig(i)
  print(i)
}

sum(exploratory_data_sig$`0.05`$selection == exploratory_data_sig$`0.1`$selection)

exploratory_data_sig <-
  bind_rows(exploratory_data_sig$`0.05`,
            exploratory_data_sig$`0.1`)

exploratory_data_sig$pval_threshold <-
  rep(sets_sig, each = 40000)

# save(exploratory_data_sig, 
#      file = "./data/exploratory_data_sig.RData")


