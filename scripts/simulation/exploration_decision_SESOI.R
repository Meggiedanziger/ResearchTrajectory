# setwd("~/Desktop/ResearchTrajectory/")

rm(list = ls())

# Here we simulate a preclinical research trajectory
# in the first stage an exploratory study is conducted with a limited number of animals
# in the second stage this experiment is replicated based on some information from the first experiment

# source additional scripts
source("./scripts/simulation/load_packages.R")
source("./scripts/simulation/load_ES_data.R")
source("./scripts/simulation/functions_for_sim.R")

# additional settings for parallel processing
registerDoMC(cores = 4)
registerDoParallel()
getDoParWorkers()

n_exp <- 10000 # number of experiments we run in 1st stage (exploration)
ES_true <- abs(meta$yi) # empirical effect sizes (ES)

# set seed to reproduce results
set.seed(4321)

# sample from ES distribution and show histograms of empirical and sampled ES
current_ES <- sample(ES_true, n_exp, replace = TRUE)
hist(ES_true, breaks = 200)
hist(current_ES, breaks = 200)


# here starts the actual simulation
# we test three initial sample sizes (robustness check)
samp_size_vector <- c(5, 7, 10, 15)

# we create a list of exploratory data
# function generate_study() taken from script functions_for_sim.R
list_exploratory_data <- 
  
  foreach(samp_size = samp_size_vector) %do% {
    
    exploratory_data <- list()
    
    for(i in 1:n_exp) {
      
      exploratory_data[[i]] <- generate_study(current_ES[i])
      
      exploratory_data[[i]] <-
        exploratory_data[[i]] %>% 
        mutate(study_id = i,
               ES_true = current_ES[i])
      
    }
    
    list_exploratory_data <- exploratory_data
  }


# we run a t-test and create a summary of results (CI and p-value)
# the confidence interval generated here is used to check whether CI covers SESOI
# function get_summary_study() taken from script functions_for_sim.R
exploratory_data_summary <- list()

plan(multicore)
for (i in 1:length(samp_size_vector)) {
  
  exploratory_data_summary[[i]] <- 
    future_map(list_exploratory_data[[i]], get_summary_study)
  
}

# decision to go on
# this decision depends on whether our SESOI is within the 95 % CI
# select studies for replication if SESOI lies within the 95 % CI
# in the function get_decision_equiv() you can change the value of SESOI
selection <- list()

for (i in 1:length(samp_size_vector)) {
  
  selection[[i]] <- future_map(exploratory_data_summary[[i]], 
                               get_decision_equiv, SESOI = 1.0)
  
}

# create a data frame which can be used in script confirmation.R
row_names <- NULL
col_names <- c("init_sample_size", "study_id", "t_value",
               "p_value", "hedges", "ci_low", "ci_high")

df <- as_tibble(matrix(unlist(exploratory_data_summary), 
                       nrow = n_exp*length(samp_size_vector), byrow = TRUE,
                       dimnames = list(c(row_names),
                                       c(col_names))))

col_name <- "selection"

df_equiv <- as_tibble(matrix(unlist(selection), 
                           nrow = n_exp*length(samp_size_vector), byrow = TRUE,
                           dimnames = list(c(row_names),
                                           c(col_name))))


dat <- bind_cols(df, df_equiv)

dat$ES_true <- rep(current_ES, 4)

# save data frame
# write.csv(dat, file = "./data/exploratory_data_sesoi_1.0")


