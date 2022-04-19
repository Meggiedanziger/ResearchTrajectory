setwd("~/Desktop/ResearchTrajectory/")

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


sum(current_ES > 0)

min(ES_true)
max(ES_true)
min(current_ES)
max(current_ES)

# here starts the actual simulation
# we test three initial sample sizes (robustness check)
samp_size_vector <- c(30)

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
# the p-value is the criterion by which we decide which studies move to confirmation
# function get_summary_study() taken from script functions_for_sim.R
exploratory_data_summary <- list()

plan(multicore)
for (i in 1:length(samp_size_vector)) {
  
  exploratory_data_summary[[i]] <- 
    future_map(list_exploratory_data[[i]], get_summary_study)
  
}

# decision to go on
# this decision depends on whether exploratory result is significant (p <= .05 / p <= .1) or not
# select studies for replication if p-value < .05 or < .1
# in the function get_decision_sig() you can change the value of pval_threshold
selection <- list()

for (i in 1:length(samp_size_vector)) {

  selection[[i]] <- 
    future_map(exploratory_data_summary[[i]],
               get_decision_sig,
               pval_threshold = 0.05)
  
}

# create a data frame which can be used in script confirmation.R
row_names <- NULL
col_names <- c("init_sample_size", "study_id", "t_value",
               "p_value", "hedges", "ci_low", "ci_high")

df <- as_tibble(matrix(unlist(exploratory_data_summary), 
                       nrow = n_exp * length(samp_size_vector), byrow = TRUE,
                       dimnames = list(c(row_names),
                                       c(col_names))))


col_name <- "selection"

df_sig <- as_tibble(matrix(unlist(selection), 
                           nrow = n_exp * length(samp_size_vector), byrow = TRUE,
                           dimnames = list(c(row_names),
                                           c(col_name))))



dat <- bind_cols(df, df_sig)

dat$ES_true <- rep(current_ES)

# save data frame
# save(dat,
#      file = "./data/exploratory_data_sig_30eu.RData")

hist(dat$hedges, breaks = 50)
