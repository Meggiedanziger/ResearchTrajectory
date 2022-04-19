
load("all_outcomes.RData")

library(viridis)
library(RColorBrewer)

outcomes_standard <-
  res %>% 
  filter(trajectory == "Standard")

outcomes_standard$dor[3] <-
  (outcomes_standard$sensitivity[3] * 0.999) / 
 ( (1 - outcomes_standard$sensitivity[3]) * (1 - 0.999) )

outcomes_standard$log_dor[3] <- log(outcomes_standard$dor[3])

ggplot(data = outcomes_standard,
       aes(x = factor(SESOI),
           y = factor(init_sample_size),
           fill = log_dor)) +
  facet_wrap(~ pval_threshold) +
  geom_tile(alpha = 0.8) +
  scale_fill_viridis() +
  theme_classic()

ggplot(data = outcomes_standard,
       aes(x = factor(SESOI),
           y = log_dor,
           color = factor(pval_threshold))) +
  facet_wrap(~ init_sample_size) +
  geom_point(alpha = 0.5) +
  theme_bw()

# display.brewer.all(colorblindFriendly = TRUE)
# brewer.pal(n = 8, name = "Dark2")

outcomes_sesoi <-
  res %>% 
  filter(trajectory == "SESOI")


ggplot(data = outcomes_sesoi,
       aes(x = factor(SESOI),
           y = factor(init_sample_size),
           fill = log_dor)) +
  geom_tile(alpha = 0.8) +
  scale_fill_viridis() +
  theme_classic()

ggplot(data = outcomes_sesoi,
       aes(x = factor(SESOI),
           y = log_dor)) +
  facet_wrap(~ init_sample_size) +
  geom_point(size = 1) +
  theme_bw()


outcomes_standard <-
  outcomes_standard %>% 
  filter(pval_threshold == 0.05) %>% 
  filter(init_sample_size == 10)

outcomes_standard$dor[1] <-
  (outcomes_standard$sensitivity[1] * 0.999) / 
  ( (1 - outcomes_standard$sensitivity[1]) * (1 - 0.999) )

outcomes_standard$log_dor[1] <- log(outcomes_standard$dor[1])

outcomes_sesoi <-
  outcomes_sesoi %>% 
  filter(init_sample_size == 10)

outcomes_sesoi$SESOI <- as.numeric(outcomes_sesoi$SESOI)

plot_data <-
  bind_rows(outcomes_standard,
            outcomes_sesoi)


ggplot(data = plot_data,
       aes(x = factor(SESOI),
           y = log_dor,
           color = trajectory)) +
  geom_point(size = 1.7) +
  theme_bw()



