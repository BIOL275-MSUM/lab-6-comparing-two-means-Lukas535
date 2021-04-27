
# load packages -----------------------------------------------------------

library(tidyverse)


# read data ---------------------------------------------------------------

fish <- read_csv("chap12q19ElectricFish.csv")


# put data in tidy format ------------------------------------------------

fish_long <- 
  pivot_longer(fish, speciesUpstream:speciesDownstream,
               names_to = "location",
               values_to = "species") %>% 
  mutate(location = str_remove(location, c("species"))) %>% 
  print()

# do stuff

fish_ttest <- t.test(species ~ location, data = fish_long)
fish_ttest$estimate


# Question A t-test -------------------------------------------------------

16.41667 - 14.58333
print(fish_ttest)

fish_mean_difference <-
  fish_long %>% 
  group_by(location) %>% 
  summarize(
    n = n(),
    mean = mean(species),
    sd = sd(species),
    sem = sd/sqrt(n),
    upper = mean + 1.96 * sem,
    lower = mean - 1.96 * sem
  ) %>% 
  print()

# Question B difference in means -----------------------------------------

fish_long %>% 
  ggplot(aes(x = location, y = species)) +
  geom_jitter(aes(color = location),
              shape = 16, size = 3,
              alpha = 0.3, width = 0.4) +
  geom_errorbar(aes(y = mean, ymax = upper, ymin = lower),
                data = fish_mean_difference,
                width = .1, size = .8) +
  geom_point(aes(y = mean),
             data = fish_mean_difference,
             size = 3) +
  scale_color_manual(values = c("red", "cyan4")) +
  theme_minimal() +
  guides(color = "none")

# Question C histograms --------------------------------------------------

fish_long %>% 
  ggplot(aes(x = species)) +
  geom_histogram(
    aes(fill = location),
    bins = 8,
    alpha = 0.5,
    position = "identity"
  ) +
  scale_fill_manual(values = c("red", "cyan2")) +
  theme_minimal() +
  facet_wrap( ~ location)
  
