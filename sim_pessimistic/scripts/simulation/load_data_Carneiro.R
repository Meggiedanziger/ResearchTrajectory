# setwd("~/Desktop/ResearchTrajectory/sim_pessimistic")

source("./scripts/simulation/load_packages.R")

ES_data_Carneiro <- read.csv(file = "./input/ES_data_Carneiro.csv", 
                             sep = ";", dec = ",")


names(ES_data_Carneiro)[5] <- "control"
names(ES_data_Carneiro)[6] <- "treated"
names(ES_data_Carneiro)[15] <- "pooled_sd"
names(ES_data_Carneiro)[16] <- "ES_d"
names(ES_data_Carneiro)[19] <- "descriptive_term"

ES_data_Carneiro$stand_diff <- (ES_data_Carneiro$treated-ES_data_Carneiro$control) / ES_data_Carneiro$pooled_sd

res_descriptive <- tibble(ES_data_Carneiro$Result.Description, ES_data_Carneiro$ES_d)

ES_data_Carneiro <-
  ES_data_Carneiro %>% 
  select(control, treated, pooled_sd, ES_d, descriptive_term, stand_diff) %>% 
  drop_na()

ES_pos <- 
  ES_data_Carneiro %>% 
  filter(ES_d > 0)

ES_neg <- 
  ES_data_Carneiro %>% 
  filter(ES_d < 0)

ggplot(data = ES_data_Carneiro) +
  # geom_histogram(aes(x = ES_d)) +
  geom_density(data = ES_pos,
               aes(x = ES_d), color = "red") +
  geom_density(data = ES_neg,
               aes(x = ES_d), color = "green")
  

# save(ES_data_Carneiro, 
#      file = "~/Desktop/simulation/sim_supp/data/ES_data_Carneiro.RData")

# zeros <- which(ES_data_Carneiro$ES_d == 0)
# sum(ES_data_Carneiro$ES_d == 0)
# mean(ES_data_Carneiro$ES_d)
# median(ES_data_Carneiro$ES_d)
hist(ES_data_Carneiro$ES_d, breaks = 50)

