
source("./scripts/simulation/load_packages.R")

osf_retrieve_file("8cqwa") %>%
  osf_download("meta_effectsize.csv",
               path = "~/Desktop/ResearchTrajectory/input",
               conflicts = TRUE)

meta <- read.csv("./input/meta_effectsize.csv")

effect_sizes <- 
  ggplot(meta, aes(x = abs(yi))) + 
  geom_histogram(bins = 60,
                 color = "black", size = 0.3,
                 fill = "darkgrey") +
  labs(x = expression(paste("Hedge's G ", italic("(log scale)"))),
       y = "Number of effect sizes") +
  scale_x_continuous(trans = "pseudo_log",
                     breaks = c(0, 0.2, 0.5, 0.9, 1.5, 3, 7, 20)) + 
  theme_bw()
  

effect_sizes

# save(meta, file = "./data/original_ES_distribution.RData")

# sum(meta$yi > 10)
