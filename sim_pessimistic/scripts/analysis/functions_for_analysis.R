
#################################################
################ SESOI TRAJECTORY ###############
#################################################

# function to compute outcomes after exploration in the SESOI trajectory

compute_outcomes_exploration <- function() {
  
  selected <-
    dat %>% 
    group_by(SESOI, init_sample_size) %>%
    filter(selection == 1) %>% 
    summarize(selected = n())
  
  selected_pos_ES <-
    dat %>% 
    group_by(SESOI, init_sample_size) %>%
    filter(selection == 1) %>% 
    filter(hedges >= 0) %>%
    summarize(selected_pos = n())
  
  not_selected <-
    dat %>% 
    group_by(SESOI, init_sample_size) %>%
    filter(hedges < 0 | selection == 0) %>% 
    summarize(not_selected = n())
  
  true_positives <-
    dat %>% 
    group_by(SESOI, init_sample_size) %>%
    filter(selection == 1) %>% 
    filter(hedges > 0) %>%
    filter(ES_true >= SESOI) %>% 
    summarize(true_pos = n())
  
  false_positives <-
    dat %>% 
    group_by(SESOI, init_sample_size) %>%
    filter(selection == 1) %>% 
    filter(hedges > 0) %>%
    filter(ES_true < SESOI) %>% 
    summarize(false_pos = n())
  
  true_negatives <-
    dat %>% 
    group_by(SESOI, init_sample_size) %>%
    filter(hedges <= 0 | selection == 0) %>% 
    filter(ES_true < SESOI) %>% 
    summarize(true_neg = n())
  
  false_negatives <-
    dat %>% 
    group_by(SESOI, init_sample_size) %>%
    filter(hedges <= 0 | selection == 0) %>% 
    filter(ES_true >= SESOI) %>% 
    summarize(false_neg = n())
  
  dat <-
    selected %>% 
    left_join(selected_pos_ES) %>% 
    left_join(not_selected) %>% 
    left_join(true_positives) %>% 
    left_join(false_positives) %>% 
    left_join(true_negatives) %>% 
    left_join(false_negatives)
  
  
  # add columns for positives and negatives to calculate outcomes
  dat$all_positives <- rep(c(mat[2, 1], 
                             mat[2, 1], 
                             mat[2, 2], 
                             mat[2, 2]))
  
  dat$all_negatives <- rep(c(mat[3, 1], 
                             mat[3, 1], 
                             mat[3, 2], 
                             mat[3, 2]))
  
  concatenate_outcomes <-
    dat %>% 
    mutate(sensitivity = true_pos/ all_positives,
           specificity = true_neg/ all_negatives,
           FNR = false_neg/ all_positives,
           FPR = false_pos/ all_negatives,
           FDR = false_pos/(false_pos + true_pos),
           FOR = false_neg/(false_neg + true_neg),
           prev = (true_pos + false_neg) / (all_positives + all_negatives))
  
  concatenate_outcomes <- 
    concatenate_outcomes %>% 
    mutate(PPV = (sensitivity * prev) / 
             (sensitivity * prev + (1 - specificity) * (1 - prev)))
  
  concatenate_outcomes <-
    concatenate_outcomes %>% 
    mutate(NPV = ((1 - prev) * specificity) / 
             ((1 - prev) * specificity + prev * (1 - sensitivity)))
  
  concatenate_outcomes <-
    concatenate_outcomes %>%
    mutate(dor = (true_pos * true_neg) / (false_pos * false_neg)) %>%
    mutate(log_dor = log(dor))
  
  return(concatenate_outcomes)
  
}


# function to compute outcomes for second stage only in the SESOI trajectory

compute_outcomes_rep <- function() {
  
  selected <-
    dat %>% 
    group_by(SESOI, init_sample_size) %>%
    filter(selection == 1) %>% 
    summarize(selected = n())
  
  selected_pos_ES <-
    dat %>% 
    group_by(SESOI, init_sample_size) %>%
    filter(selection == 1) %>% 
    filter(hedges >= 0) %>%
    summarize(selected_pos = n())
  
  not_selected <-
    dat %>% 
    group_by(SESOI, init_sample_size) %>%
    filter(hedges < 0 | selection == 0) %>% 
    summarize(not_selected = n())
  
  true_positives <-
    dat %>% 
    group_by(SESOI, init_sample_size) %>%
    filter(selection == 1) %>% 
    filter(hedges > 0) %>%
    filter(ES_true >= SESOI) %>% 
    summarize(true_pos = n())
  
  false_positives <-
    dat %>% 
    group_by(SESOI, init_sample_size) %>%
    filter(selection == 1) %>% 
    filter(hedges > 0) %>%
    filter(ES_true < SESOI) %>% 
    summarize(false_pos = n())
  
  true_negatives <-
    dat %>% 
    group_by(SESOI, init_sample_size) %>%
    filter(hedges <= 0 | selection == 0) %>% 
    filter(ES_true < SESOI) %>% 
    summarize(true_neg = n())
  
  false_negatives <-
    dat %>% 
    group_by(SESOI, init_sample_size) %>%
    filter(hedges <= 0 | selection == 0) %>% 
    filter(ES_true >= SESOI) %>% 
    summarize(false_neg = n())
  
  # compute all positives / negatives in sample of studies that proceeded to confirmation
  pos_and_neg <-
    dat %>% 
    group_by(SESOI, init_sample_size) %>% 
    summarize(all_positives = sum(ES_true >= SESOI),
              all_negatives = sum(ES_true < SESOI),
              all = all_positives + all_negatives)
  
  dat <-
    selected %>% 
    left_join(selected_pos_ES) %>% 
    left_join(not_selected) %>% 
    left_join(true_positives) %>% 
    left_join(false_positives) %>% 
    left_join(true_negatives) %>% 
    left_join(false_negatives) %>% 
    left_join(pos_and_neg)
  
  concatenate_outcomes <-
    dat %>% 
    mutate(sensitivity = true_pos/ all_positives,
           specificity = true_neg/ all_negatives,
           FNR = false_neg/ all_positives,
           FPR = false_pos/ all_negatives,
           FDR = false_pos/(false_pos + true_pos),
           FOR = false_neg/(false_neg + true_neg),
           prev = (true_pos + false_neg) / (all_positives + all_negatives))
  
  concatenate_outcomes <- 
    concatenate_outcomes %>% 
    mutate(PPV = (sensitivity * prev) / 
             (sensitivity * prev + (1 - specificity) * (1 - prev)))
  
  concatenate_outcomes <-
    concatenate_outcomes %>% 
    mutate(NPV = ((1 - prev) * specificity) / 
             ((1 - prev) * specificity + prev * (1 - sensitivity)))
  
  concatenate_outcomes <-
    concatenate_outcomes %>% 
    mutate(dor = (true_pos * true_neg) / (false_pos * false_neg)) %>% 
    mutate(log_dor = log(dor))
  
  return(concatenate_outcomes)
  
}


# function to compute outcomes across trajectory in the SESOI trajectory

compute_outcomes_across_trajectory <- function() {
  
  selected <-
    dat %>% 
    group_by(SESOI, init_sample_size) %>%
    filter(selection == 1) %>% 
    summarize(selected = n())
  
  selected_pos_ES <-
    dat %>% 
    group_by(SESOI, init_sample_size) %>%
    filter(selection == 1) %>% 
    filter(hedges >= 0) %>%
    summarize(selected_pos = n())
  
  not_selected <-
    dat %>% 
    group_by(SESOI, init_sample_size) %>%
    filter(hedges < 0 | selection == 0) %>% 
    summarize(not_selected = n())
  
  true_positives <-
    dat %>% 
    group_by(SESOI, init_sample_size) %>%
    filter(selection == 1) %>% 
    filter(hedges > 0) %>%
    filter(ES_true >= SESOI) %>% 
    summarize(true_pos = n())
  
  false_positives <-
    dat %>% 
    group_by(SESOI, init_sample_size) %>%
    filter(selection == 1) %>% 
    filter(hedges > 0) %>%
    filter(ES_true < SESOI) %>% 
    summarize(false_pos = n())
  
  true_negatives <-
    dat %>% 
    group_by(SESOI, init_sample_size) %>%
    filter(hedges <= 0 | selection == 0) %>% 
    filter(ES_true < SESOI) %>% 
    summarize(true_neg = n())
  
  false_negatives <-
    dat %>% 
    group_by(SESOI, init_sample_size) %>%
    filter(hedges <= 0 | selection == 0) %>% 
    filter(ES_true >= SESOI) %>% 
    summarize(false_neg = n())
  
  animal_numbers <-
    dat %>% 
    group_by(SESOI, init_sample_size) %>% 
    summarize(mean_N = mean(rep_sample_size),
              sd_N   = sd(rep_sample_size))
  
  median_effect <-
    dat %>% 
    group_by(SESOI, init_sample_size) %>% 
    summarize(median_effect = median(hedges))
  
  median_effect_success <-
    dat %>%
    group_by(SESOI, init_sample_size) %>% 
    filter(selection == 1) %>%
    summarize(median_effect_success = median(hedges))
  
  median_ES_true <-
    dat %>% 
    group_by(SESOI, init_sample_size) %>% 
    summarize(median_ES_true = median(ES_true))
  
  dat <-
    selected %>% 
    left_join(selected_pos_ES) %>% 
    left_join(not_selected) %>% 
    left_join(true_positives) %>% 
    left_join(false_positives) %>% 
    left_join(true_negatives) %>% 
    left_join(false_negatives)
  
  # add true negatives from exploration
  dat$true_neg  <- dat$true_neg + outcomes_sesoi_exp$true_neg
  
  # add false negatives from exploration
  dat$false_neg <- dat$false_neg + outcomes_sesoi_exp$false_neg
  
  # add columns for positives and negatives to calculate outcomes
  dat$all_positives <- rep(c(mat[2, 1], 
                             mat[2, 1], 
                             mat[2, 2], 
                             mat[2, 2]))
  
  dat$all_negatives <- rep(c(mat[3, 1], 
                             mat[3, 1], 
                             mat[3, 2], 
                             mat[3, 2]))
  
  concatenate_outcomes <-
    dat %>% 
    mutate(sensitivity = true_pos/ all_positives,
           specificity = true_neg/ all_negatives,
           FNR = false_neg/ all_positives,
           FPR = false_pos/ all_negatives,
           FDR = false_pos/(false_pos + true_pos),
           FOR = false_neg/(false_neg + true_neg),
           prev = (true_pos + false_neg) / (all_positives + all_negatives))
  
  concatenate_outcomes <- 
    concatenate_outcomes %>% 
    mutate(PPV = (sensitivity * prev) / 
             (sensitivity * prev + (1 - specificity) * (1 - prev)))
  
  concatenate_outcomes <-
    concatenate_outcomes %>% 
    mutate(NPV = ((1 - prev) * specificity) / 
             ((1 - prev) * specificity + prev * (1 - sensitivity)))
  
  concatenate_outcomes <-
    concatenate_outcomes %>%
    mutate(dor = (true_pos * true_neg) / (false_pos * false_neg)) %>%
    mutate(log_dor = log(dor))
  
  concatenate_outcomes <-
    concatenate_outcomes %>% 
    left_join(animal_numbers) %>% 
    left_join(median_effect) %>% 
    left_join(median_effect_success) %>% 
    left_join(median_ES_true)
  
  return(concatenate_outcomes)
  
}


#################################################
############## STANDRAD TRAJECTORY ##############
#################################################

# function to compute outcomes after exploration in the Standard trajectory

compute_outcomes_exploration_standard <- function() {
  
  selected <-
    dat %>% 
    group_by(SESOI, init_sample_size) %>%
    filter(selection == 1) %>% 
    summarize(selected = n())
  
  selected_pos_ES <-
    dat %>% 
    group_by(SESOI, init_sample_size) %>%
    filter(selection == 1) %>% 
    filter(hedges >= 0) %>%
    summarize(selected_pos = n())
  
  not_selected <-
    dat %>% 
    group_by(SESOI, init_sample_size) %>%
    filter(hedges < 0 | selection == 0) %>% 
    summarize(not_selected = n())
  
  true_positives <-
    dat %>% 
    group_by(SESOI, init_sample_size) %>%
    filter(selection == 1) %>% 
    filter(hedges > 0) %>%
    filter(ES_true >= SESOI) %>% 
    summarize(true_pos = n())
  
  false_positives <-
    dat %>% 
    group_by(SESOI, init_sample_size) %>%
    filter(selection == 1) %>% 
    filter(hedges > 0) %>%
    filter(ES_true < SESOI) %>% 
    summarize(false_pos = n())
  
  true_negatives <-
    dat %>% 
    group_by(SESOI, init_sample_size) %>%
    filter(hedges <= 0 | selection == 0) %>% 
    filter(ES_true < SESOI) %>% 
    summarize(true_neg = n())
  
  false_negatives <-
    dat %>% 
    group_by(SESOI, init_sample_size) %>%
    filter(hedges <= 0 | selection == 0) %>% 
    filter(ES_true >= SESOI) %>% 
    summarize(false_neg = n())
  
  dat <-
    selected %>% 
    left_join(selected_pos_ES) %>% 
    left_join(not_selected) %>% 
    left_join(true_positives) %>% 
    left_join(false_positives) %>% 
    left_join(true_negatives) %>% 
    left_join(false_negatives)
  
  # add columns for positives and negatives to calculate outcomes
  dat$all_positives <- rep(c(mat[2, 1],
                             mat[2, 1],
                             mat[2, 2],
                             mat[2, 2]))

  dat$all_negatives <- rep(c(mat[3, 1],
                             mat[3, 1],
                             mat[3, 2],
                             mat[3, 2]))

  concatenate_outcomes <-
    dat %>%
    mutate(sensitivity = true_pos/ all_positives,
           specificity = true_neg/ all_negatives,
           FNR = false_neg/ all_positives,
           FPR = false_pos/ all_negatives,
           FDR = false_pos/(false_pos + true_pos),
           FOR = false_neg/(false_neg + true_neg),
           prev = (true_pos + false_neg) / (all_positives + all_negatives))

  concatenate_outcomes <-
    concatenate_outcomes %>%
    mutate(PPV = (sensitivity * prev) /
             (sensitivity * prev + (1 - specificity) * (1 - prev)))

  concatenate_outcomes <-
    concatenate_outcomes %>%
    mutate(NPV = ((1 - prev) * specificity) /
             ((1 - prev) * specificity + prev * (1 - sensitivity)))
  
  concatenate_outcomes <-
    concatenate_outcomes %>% 
    mutate(dor = (true_pos * true_neg) / (false_pos * false_neg)) %>% 
    mutate(log_dor = log(dor))

  return(concatenate_outcomes)
  
}

# function to compute outcomes for second stage only in the Standard trajectory

compute_outcomes_rep_standard <- function() {
  
  selected <-
    dat %>% 
    group_by(pval_threshold, SESOI, init_sample_size) %>%
    filter(selection == 1) %>% 
    summarize(selected = n())
  
  selected_pos_ES <-
    dat %>% 
    group_by(pval_threshold, SESOI, init_sample_size) %>%
    filter(selection == 1) %>% 
    filter(hedges >= 0) %>%
    summarize(selected_pos = n())
  
  not_selected <-
    dat %>% 
    group_by(pval_threshold, SESOI, init_sample_size) %>%
    filter(hedges < 0 | selection == 0) %>% 
    summarize(not_selected = n())
  
  true_positives <-
    dat %>% 
    group_by(pval_threshold, SESOI, init_sample_size) %>%
    filter(selection == 1) %>% 
    filter(hedges > 0) %>%
    filter(ES_true >= SESOI) %>% 
    summarize(true_pos = n())
  
  false_positives <-
    dat %>% 
    group_by(pval_threshold, SESOI, init_sample_size) %>%
    filter(selection == 1) %>% 
    filter(hedges > 0) %>%
    filter(ES_true < SESOI) %>% 
    summarize(false_pos = n())
  
  true_negatives <-
    dat %>% 
    group_by(pval_threshold, SESOI, init_sample_size) %>%
    filter(hedges <= 0 | selection == 0) %>% 
    filter(ES_true < SESOI) %>% 
    summarize(true_neg = n())
  
  false_negatives <-
    dat %>% 
    group_by(pval_threshold, SESOI, init_sample_size) %>%
    filter(hedges <= 0 | selection == 0) %>% 
    filter(ES_true >= SESOI) %>% 
    summarize(false_neg = n())
  
  # compute all positives / negatives in sample of studies that proceeded to confirmation
  pos_and_neg <-
    dat %>% 
    group_by(pval_threshold, SESOI, init_sample_size) %>% 
    summarize(all_positives = sum(ES_true >= SESOI),
              all_negatives = sum(ES_true < SESOI),
              all = all_positives + all_negatives)
  
  dat <-
    selected %>% 
    left_join(selected_pos_ES) %>% 
    left_join(not_selected) %>% 
    left_join(true_positives) %>% 
    left_join(false_positives) %>% 
    left_join(true_negatives) %>% 
    left_join(false_negatives) %>% 
    left_join(pos_and_neg)
  
  concatenate_outcomes <-
    dat %>% 
    mutate(sensitivity = true_pos/ all_positives,
           specificity = true_neg/ all_negatives,
           FNR = false_neg/ all_positives,
           FPR = false_pos/ all_negatives,
           FDR = false_pos/(false_pos + true_pos),
           FOR = false_neg/(false_neg + true_neg),
           prev = (true_pos + false_neg) / (all_positives + all_negatives))
  
  concatenate_outcomes <- 
    concatenate_outcomes %>% 
    mutate(PPV = (sensitivity * prev) / 
             (sensitivity * prev + (1 - specificity) * (1 - prev)))
  
  concatenate_outcomes <-
    concatenate_outcomes %>% 
    mutate(NPV = ((1 - prev) * specificity) / 
             ((1 - prev) * specificity + prev * (1 - sensitivity)))
  
  concatenate_outcomes <-
    concatenate_outcomes %>% 
    mutate(dor = (true_pos * true_neg) / (false_pos * false_neg)) %>% 
    mutate(log_dor = log(dor))
  
  return(concatenate_outcomes)
  
}

# function to compute outcomes across trajectory in the Standard trajectory

compute_outcomes_across_trajectory_standard <- function() {
  
  selected <-
    dat %>% 
    group_by(pval_threshold, SESOI, init_sample_size) %>%
    filter(selection == 1) %>% 
    summarize(selected = n())
  
  selected_pos_ES <-
    dat %>% 
    group_by(pval_threshold, SESOI, init_sample_size) %>%
    filter(selection == 1) %>% 
    filter(hedges >= 0) %>%
    summarize(selected_pos = n())
  
  not_selected <-
    dat %>% 
    group_by(pval_threshold, SESOI, init_sample_size) %>%
    filter(hedges < 0 | selection == 0) %>% 
    summarize(not_selected = n())
  
  true_positives <-
    dat %>% 
    group_by(pval_threshold, SESOI, init_sample_size) %>%
    filter(selection == 1) %>% 
    filter(hedges > 0) %>%
    filter(ES_true >= SESOI) %>% 
    summarize(true_pos = n())
  
  false_positives <-
    dat %>% 
    group_by(pval_threshold, SESOI, init_sample_size) %>%
    filter(selection == 1) %>% 
    filter(hedges > 0) %>%
    filter(ES_true < SESOI) %>% 
    summarize(false_pos = n())
  
  true_negatives <-
    dat %>% 
    group_by(pval_threshold, SESOI, init_sample_size) %>%
    filter(hedges <= 0 | selection == 0) %>% 
    filter(ES_true < SESOI) %>% 
    summarize(true_neg = n())
  
  false_negatives <-
    dat %>% 
    group_by(pval_threshold, SESOI, init_sample_size) %>%
    filter(hedges <= 0 | selection == 0) %>% 
    filter(ES_true >= SESOI) %>% 
    summarize(false_neg = n())
  
  animal_numbers <-
    dat %>% 
    group_by(pval_threshold, SESOI, init_sample_size) %>% 
    summarize(mean_N = mean(rep_sample_size),
              sd_N   = sd(rep_sample_size))
  
  median_effect <-
    dat %>% 
    group_by(pval_threshold, SESOI, init_sample_size) %>% 
    summarize(median_effect = median(hedges))
  
  median_effect_success <-
    dat %>%
    group_by(pval_threshold, SESOI, init_sample_size) %>% 
    filter(selection == 1) %>%
    summarize(median_effect_success = median(hedges))
  
  median_ES_true <-
    dat %>% 
    group_by(pval_threshold, SESOI, init_sample_size) %>% 
    summarize(median_ES_true = median(ES_true))
  
  dat <-
    selected %>% 
    left_join(selected_pos_ES) %>% 
    left_join(not_selected) %>% 
    left_join(true_positives) %>% 
    left_join(false_positives) %>% 
    left_join(true_negatives) %>% 
    left_join(false_negatives)
  
  # add true negatives from exploration
  dat$true_neg  <- dat$true_neg + outcomes_standard_exp$true_neg
  
  # add false negatives from exploration
  dat$false_neg <- dat$false_neg + outcomes_standard_exp$false_neg
  
  # add columns for positives and negatives to calculate outcomes
  dat$all_positives <- rep(c(mat[2, 1], 
                             mat[2, 1], 
                             mat[2, 2], 
                             mat[2, 2]))
  
  dat$all_negatives <- rep(c(mat[3, 1], 
                             mat[3, 1], 
                             mat[3, 2], 
                             mat[3, 2]))
  
  concatenate_outcomes <-
    dat %>% 
    mutate(sensitivity = true_pos/ all_positives,
           specificity = true_neg/ all_negatives,
           FNR = false_neg/ all_positives,
           FPR = false_pos/ all_negatives,
           FDR = false_pos/(false_pos + true_pos),
           FOR = false_neg/(false_neg + true_neg),
           prev = (true_pos + false_neg) / (all_positives + all_negatives))
  
  concatenate_outcomes <- 
    concatenate_outcomes %>% 
    mutate(PPV = (sensitivity * prev) / 
             (sensitivity * prev + (1 - specificity) * (1 - prev)))
  
  concatenate_outcomes <-
    concatenate_outcomes %>% 
    mutate(NPV = ((1 - prev) * specificity) / 
             ((1 - prev) * specificity + prev * (1 - sensitivity)))
  
  concatenate_outcomes <-
    concatenate_outcomes %>%
    mutate(dor = (true_pos * true_neg) / (false_pos * false_neg)) %>%
    mutate(log_dor = log(dor))
  
  concatenate_outcomes <-
    concatenate_outcomes %>% 
    left_join(animal_numbers) %>% 
    left_join(median_effect) %>% 
    left_join(median_effect_success) %>% 
    left_join(median_ES_true)
  
  return(concatenate_outcomes)
  
}


