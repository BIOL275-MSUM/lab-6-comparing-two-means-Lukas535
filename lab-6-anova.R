
# load packages -----------------------------------------------------------

library(tidyverse)


# read data ---------------------------------------------------------------

crabs <- read_csv("chap15q27FiddlerCrabFans.csv")
crabs

# QUESTION D --------------------------------------------------------------

# graph the distribution of body temperatures for each crab type

crabs %>% 
  filter(!is.na(crabType)) %>% 
  ggplot(aes(x = bodyTemperature)) +
  geom_histogram(
    aes(fill = crabType),
    bins = 8,
    alpha = 0.5,
    position = "identity"
  ) +
  facet_wrap( ~ crabType, ncol = 1)

# QUESTION E --------------------------------------------------------------

# ANOVA

aov_crab <-
  aov(bodyTemperature ~ crabType, data = crabs)

summary(aov_crab)
