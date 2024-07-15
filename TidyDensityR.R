#----------------------------------------#
# Exploring the TidiDensity package in R #
#----------------------------------------#

library(TidyDensity)
library(tidyverse)

# plot Normal distribution
set.seed(2024)
tidy_autoplot(tidy_normal(.n = 100, .num_sims = 6), .plot_type = "density")
tidy_autoplot(tidy_normal(.n = 100, .num_sims = 6), .plot_type = "quantile")
tidy_autoplot(tidy_normal(.n = 100, .num_sims = 6), .plot_type = "probability")
tidy_autoplot(tidy_normal(.n = 100, .num_sims = 6), .plot_type = "qq")

# Reference: https://www.spsanderson.com/TidyDensity/

# Random Walks
set.seed(2024)
tidy_normal(.num_sims = 25, .n = 100) |>
  tidy_random_walk(.value_type = "cum_sum") |>
  tidy_random_walk_autoplot()

# Random Walks with sampling
set.seed(2024)
tidy_normal(.num_sims = 25, .n = 100) |>
  tidy_random_walk(.value_type = "cum_sum", .sample = TRUE) |>
  tidy_random_walk_autoplot()

# Random Walks with sampling with replacement
set.seed(2024)
tidy_normal(.num_sims = 25, .n = 100) |>
  tidy_random_walk(.value_type = "cum_sum", .sample = TRUE, .replace = TRUE) |>
  tidy_random_walk_autoplot()

# Comparing different Random Walks

set.seed(2024)
df <- rbind(
  tidy_normal(.num_sims = 25, .n = 100) |>
    tidy_random_walk(.value_type = "cum_sum") |>
    mutate(type = "No_Sample"),
  tidy_normal(.num_sims = 25, .n = 100) |>
    tidy_random_walk(.value_type = "cum_sum", .sample = TRUE) |>
    mutate(type = "Sample_No_Replace"),
  tidy_normal(.num_sims = 25, .n = 100) |>
    tidy_random_walk(.value_type = "cum_sum", .sample = TRUE, .replace = TRUE) |>
    mutate(type = "Sample_Replace")
) |>
  select(sim_number, x, random_walk_value, type) |>
  mutate(
    low_ci = -1.96 * sqrt(x),
    hi_ci = 1.96 * sqrt(x)
  )
atb <- attributes(df)
df |>
  ggplot(aes(
    x = x, 
    y = random_walk_value, 
    group = sim_number, 
    color = factor(type))
  ) +
  geom_line(aes(alpha = 0.382)) +
  geom_line(aes(y = low_ci, group = sim_number), 
            linetype = "dashed", size = 0.6, color = "black") +
  geom_line(aes(y = hi_ci, group = sim_number), 
            linetype = "dashed", size = 0.6, color = "black") +
  theme_minimal() +
  theme(legend.position="none") +
  facet_wrap(~type) +
  labs(
    x = "Time",
    y = "Random Walk Value",
    title = "Random Walk with Different Sampling Methods",
    subtitle = paste0("Simulations: ", atb$all$.num_sims, 
                      " | Steps: ", atb$all$.n,
                      " | Distribution: ", atb$all$dist_with_params
    )
  )

# Reference: https://www.r-bloggers.com/2024/07/exploring-random-walks-with-tidydensity-in-r/