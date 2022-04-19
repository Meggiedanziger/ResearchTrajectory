setwd("~/Desktop/ResearchTrajectory/")

source("./scripts/simulation/load_packages.R")

# load data of both trajectories after replication
load("./data/standard_data_after_replication.RData")
load("./data/sesoi_data_after_replication.RData")

# rename data sets
data_standard <- 
  standard_data_after_replication

data_sesoi <- 
  sesoi_data_after_replication

# add column for outcome significant / not significant
# 2 == significant
# 1 == not significant
data_standard$H0 <- ifelse(data_standard$p_value <= .05, 1, 0)

# add column that codes decision criterion from exploratory stage to confirmatory stage
data_standard$decision_crit <- "significance"

# add column that codes approach to determine sample size for confirmatory study
data_standard$sampsize_approach <- "standard"

min(data_standard$ES_true)
max(data_standard$ES_true)

data_standard_10 <-
  data_standard %>% 
  filter(init_sample_size == 10) %>%
  filter(pval_thres == 0.05) %>% 
  filter(ES_true < 10)

plot_data_standard <-
  data_standard %>% 
  filter(init_sample_size == 10) %>%
  filter(pval_thres == 0.05) %>% 
  filter(ES_true < 10) %>% 
  mutate(breaks = findInterval(ES_true, seq(0, 10, 0.2))) %>% 
  group_by(H0, breaks) %>% 
  summarize(N = n()) %>% 
  mutate(
    pct = ifelse(H0 == 0, N / sum(N), 1 - N / sum(N)),
    breaks = seq(0, 10, 0.2)[breaks]
  )

detect_probs_standard <- 
  ggplot() +
  geom_segment(
    data = plot_data_standard, size = 3, show.legend = FALSE,
    aes(x = breaks, xend = breaks, 
        y = H0, yend = pct, 
        colour = factor(H0))) +
  stat_smooth(data = data_standard_10, 
              aes(y = H0, x = ES_true), 
              method = "glm", 
              method.args = list(family = "binomial"),
              color = "#595959") +
  scale_y_continuous(limits = c(-0.02, 1.02)) +
  # scale_x_continuous(trans = "pseudo_log",
  #                    breaks = c(0, 0.2, 0.5, 0.9, 1.5, 3 ,7 , 10)) +
  scale_color_manual(breaks = c("1",
                                "0"),
                     values = c("#CC6677",
                                "#6699CC")) +
  labs(x = " ",
       y = "ES detected  No = 0 / Yes = 1") +
  theme_bw(base_size = 12) +
  theme(axis.title.y = element_text(size = 9),
        axis.text.y = element_text(size = 7, colour = "black"),
        axis.text.x = element_text(size = 7, colour = "black"),
        legend.position = "none")


### FOR SESOI TRAJECTORY WITH SESOI 0.5 ###

# add column for outcome significant / not significant
# 2 == significant
# 1 == not significant
data_sesoi$H0 <- ifelse(data_sesoi$p_value <= .05, 1, 0)

# add column that codes decision criterion from exploratory stage to confirmatory stage
data_sesoi$decision_crit <- "equivalence"

# add column that codes approach to determine sample size for confirmatory study
data_sesoi$sampsize_approach <- "SESOI"

data_sesoi_10 <-
  data_sesoi %>% 
  filter(init_sample_size == 10) %>%
  filter(ES_true < 10)

plot_data_sesoi_0.5 <-
  data_sesoi %>% 
  filter(init_sample_size == 10) %>%
  filter(SESOI == 0.5) %>%
  filter(ES_true < 10) %>% 
  mutate(breaks = findInterval(ES_true, seq(0, 10, 0.2))) %>% 
  group_by(H0, breaks) %>% 
  summarize(N = n()) %>% 
  mutate(
    pct = ifelse(H0 == 0, N / sum(N), 1 - N / sum(N)),
    breaks = seq(0, 10, 0.2)[breaks]
  )

detect_probs_sesoi_0.5 <- 
  ggplot() +
  geom_segment(
    data = plot_data_sesoi_0.5, size = 3, show.legend = FALSE,
    aes(x = breaks, xend = breaks, 
        y = H0, yend = pct, 
        colour = factor(H0))) +
  stat_smooth(data = data_sesoi_10, 
              aes(y = H0, x = ES_true), 
              method = "glm", 
              method.args = list(family = "binomial"),
              color = "#595959") +
  geom_vline(xintercept = 0.5,
             lty = 2,
             color = "#595959") + 
  scale_y_continuous(limits = c(-0.02, 1.02)) +
  scale_color_manual(breaks = c("1",
                                "0"),
                     values = c("#CC6677",
                                "#6699CC")) +
  labs(x = expression(paste("Effect sizes after replication (Hedges' ", italic("g"), ") ")),
       y = " ") +
  theme_bw(base_size = 12) +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.x = element_text(size = 9),
        axis.text.x = element_text(size = 7, colour = "black"),
        legend.position = "none")


### FOR SESOI TRAJECTORY WITH SESOI 1.0 ###

plot_data_sesoi_1.0 <-
  data_sesoi %>% 
  filter(init_sample_size == 10) %>%
  filter(SESOI == 1.0) %>%
  filter(ES_true < 10) %>% 
  mutate(breaks = findInterval(ES_true, seq(0, 10, 0.2))) %>% 
  group_by(H0, breaks) %>% 
  summarize(N = n()) %>% 
  mutate(
    pct = ifelse(H0 == 0, N / sum(N), 1 - N / sum(N)),
    breaks = seq(0, 10, 0.2)[breaks]
  )

detect_probs_sesoi_1.0 <- 
  ggplot() +
  geom_segment(
    data = plot_data_sesoi_0.5, size = 3, show.legend = FALSE,
    aes(x = breaks, xend = breaks, 
        y = H0, yend = pct, 
        colour = factor(H0))) +
  stat_smooth(data = data_sesoi_10, 
              aes(y = H0, x = ES_true), 
              method = "glm", 
              method.args = list(family = "binomial"),
              color = "#595959") +
  geom_vline(xintercept = 1.0,
             lty = 2,
             color = "#595959") + 
  scale_y_continuous(limits = c(-0.02, 1.02)) +
  scale_color_manual(breaks = c("1",
                                "0"),
                     values = c("#CC6677",
                                "#6699CC")) +
  labs(x = " ",
       y = " ") +
  theme_bw(base_size = 12) +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_text(size = 7, colour = "black"),
        legend.position = "none")


ggdraw(xlim = c(0, 1.46),
       ylim = c(0, .51)) +
  draw_plot(detect_probs_standard,  0, 0, .5, .5) +
  draw_plot(detect_probs_sesoi_0.5, .5, 0, .46, .5) +
  draw_plot(detect_probs_sesoi_1.0, .96, 0, .46, .5) +
  draw_plot_label(c("a", "b", "c"),
                  c(0.045, .495, 0.955),
                  c(.5, .5, .5),
                  size = 9)
