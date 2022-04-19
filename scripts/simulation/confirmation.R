setwd("~/Desktop/ResearchTrajectory/")

# source additional scripts

# choose this script if you want to select studies for replication by 
# p-value threshold 
# source("./scripts/simulation/exploration_decision_significance.R")

# choose this script if want to select studies for replication by 
# smallest effect size of interest (SESOI) 
# source("./scripts/simulation/exploration_decision_SESOI.R")

# view data frame created by scripts above
dat

# filter studies that were selected for replication (selection == 1)
data <-
  dat %>%
  group_by(init_sample_size, study_id) %>%
  filter(selection == 1)

# how many studies selected given initial sample size
selected <-
  data %>%
  group_by(init_sample_size) %>%
  summarize(selected = sum(selection == 1))

# now estimate sample size for replication study
# choose one of the methods for sample size estimation (code chunks)
# method 1: Exploratory effect size (probably the worst solution but good as a floor benchmark, 1 in function)
# method 2: SESOI. With this all experiments will have the same number of EU (2 as function parameter, requires to set a power parameter)

rep_sample_size <- NULL

for (i in 1:nrow(data)) {

  # for method 1
  rep_sample_size[i] <-
    ceiling(calc_sample_size(data = data[i, ], sample_size = data[i, ]$init_sample_size,
                             method = 1))
  
  # for method 2
  # rep_sample_size[i] <-
  #   ceiling(calc_sample_size(data = data[i, ], sample_size = data[i, ]$init_sample_size,
  #                            method = 2, SESOI = 1.0, power = .5))
}

# add column for replication sample size to data frame
data$rep_samp_size <- rep_sample_size

# how many effect sizes < 0
# these are excluded as we only continue with effect sizes in favor of treatment
negative_ES <-
  data %>%
  group_by(init_sample_size) %>%
  summarize(neg_ES = sum(hedges < 0))

percent_selected <-
  selected %>%
  mutate(neg_ES = negative_ES$neg_ES,
         selected_no_negatives = selected-neg_ES,
         per_selected = selected_no_negatives / 10000 * 100)

# to later add column with number of replication attempts
rep_attempts <-
  rep(c(percent_selected$selected_no_negatives[1]))

replication_data <- list()

rep_exp_no <- 0

# remove selected studies that had effect sizes < 0
select_experiments <- which(data$selection == 1)
select_experiments <- select_experiments[data$hedges[select_experiments] >= 0]

current_ES_rep <- data$ES_true

# run the code to perform replication study on studies selected for replication

set.seed(4321)

for(i in select_experiments) {
  
  rep_exp_no <- rep_exp_no + 1
  
  replication_data[[rep_exp_no]] <-
    generate_study(ES_true = current_ES_rep[i],
                   sample_size = rep_sample_size[i])
  
  replication_data[[rep_exp_no]] <-
    replication_data[[rep_exp_no]] %>% 
    mutate(study_id = rep_exp_no)
}

plan(multicore)
rep_data_summary <- 
  future_map(replication_data, get_summary_study_rep)
 
res_summary_rep <-
  data.frame(init_sample_size = data$init_sample_size[select_experiments],
             rep_no = c(1:rep_exp_no),
             rep_sample_size = rep_sample_size[select_experiments],
             t_value = unlist(map(rep_data_summary, "t_value")),
             p_value = unlist(map(rep_data_summary, "p_value")), #[seq(1, 2*rep_exp_no, 2)],
             hedge = unlist(map(rep_data_summary, "hedge")),
             ci_low = unlist(map(rep_data_summary, "ci_low")),
             ci_high = unlist(map(rep_data_summary, "ci_high")),
             ES_true = data$ES_true[select_experiments],
             rep_attempts = rep_attempts)

# save data frame
# ?: enter method 1 or 2 for sample size calculation approach
# ??: if SESOI was used: enter SESOI
# write.csv(res_summary_rep,
#           file = "./data/confirmation_sesoi_method?_??")

#or

#??: enter p-value threshold used
#?: enter method 1 or 2 for sample size calculation approach
#???: if combination of significance and method2: enter SESOI
# write.csv(res_summary_rep,
#           file = "./data/confirmation_sig_p??_method?_???")

