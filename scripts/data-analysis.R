# Set working directory to source file location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Clear environment
rm(list = ls())


# Load libraries ----------------------------------------------------------

library(tidyverse)


# Load data ---------------------------------------------------------------

load("../data/survey-responses.RData")
load("../data/investment-factors.RData")


investments <- survey_responses %>%
  select(
    identification_number,
    student_number,
    investment_decision_week_4,
    investment_decision_week_8
  ) %>%
  full_join(
    x = .,
    y = investment_factors,
    by = c("identification_number" = "id")
  ) %>%
  mutate(
    market_shock = ifelse(
      identification_number %% 2 == 0,
      "negative",
      "positive"
    ),
    .before = identification_number
  ) %>%
  # Drop unused investment factors
  drop_na(
    investment_decision_week_4,
    investment_decision_week_8
  ) %>%
  # Rename investment factor columns
  rename_with(
    .fn = ~ gsub(
      pattern = "round",
      replacement = "investment_factor_week",
      x = .x,
      fixed = TRUE
    ),
    .cols = starts_with("round")
  )


# Mean difference analysis ------------------------------------------------

mean_dd_matrix <- investments %>%
  select(
    market_shock,
    identification_number,
    investment_decision_week_4,
    investment_decision_week_8
  ) %>%
  group_by(market_shock) %>%
  summarise(
    week_4 = mean(investment_decision_week_4),
    week_8 = mean(investment_decision_week_8),
  ) %>%
  pivot_longer(
    cols = c(week_4, week_8),
    names_to = "investment_decision",
    values_to = "mean_investment_decision_value"
  ) %>%
  pivot_wider(
    names_from = market_shock,
    values_from = mean_investment_decision_value
  )

mean_dd_matrix


# The effect of a negative shock on the mean investment decision
diff(mean_dd_matrix$positive)
diff(mean_dd_matrix$negative)

diff(
  c(
    diff(mean_dd_matrix$positive),
    diff(mean_dd_matrix$negative)
  )
)

# Observation: The negative market shock decreases the mean investment decision
# by 0.26 (-0.26) euros more compared to the positive market shock. And with
# that, the average amount of money invested decreases more following a negative
# shock compared to a positive one.

# Alternatively, the positive market shock decreases the mean investment
# decision by 0.26 euros less compared to the negative market shock.


# Standard deviation difference in differences ----------------------------

sd_dd_matrix <- investments %>%
  select(
    market_shock,
    identification_number,
    investment_decision_week_4,
    investment_decision_week_8
  ) %>%
  group_by(market_shock) %>%
  summarise(
    week_4 = sd(investment_decision_week_4),
    week_8 = sd(investment_decision_week_8),
  ) %>%
  pivot_longer(
    cols = c(week_4, week_8),
    names_to = "investment_decision",
    values_to = "sd_investment_decision_value"
  ) %>%
  pivot_wider(
    names_from = market_shock,
    values_from = sd_investment_decision_value
  )

sd_dd_matrix


# The effect of a negative shock on the standard deviation investment decision
diff(sd_dd_matrix$positive)
diff(sd_dd_matrix$negative)

diff(
  c(
    diff(sd_dd_matrix$positive),
    diff(sd_dd_matrix$negative)
  )
)

# Observation: The negative market shock increases the standard deviation by
# 0.72 euros compared to the positive market shock. And with that, the
# variability in the investment decision distribution increases more following
# a negative market shock compared to the case of a positive market shock.


# Mann-Whitney U Test -----------------------------------------------------

# Null hypothesis (H0): Individuals react symmetrically to positive and negative
# shocks. Thus, the deviation in the investment decision values in the positive
# shock market and the inverse deviation in the investment decision values in
# the negative shock market are equal.

# Alternative hypothesis: Individuals react asymmetrically to positive and
# negative shocks. Thus, the deviation in the investment decision values in the
# positive shock market and the inverse deviation in the investment decision
# values in the negative shock market are not equal.


# Data transformation: absolute mean differences

investments_deviation <- investments %>%
  select(
    market_shock,
    identification_number,
    investment_decision_week_4,
    investment_decision_week_8
  ) %>%
  mutate(
    deviation = investment_decision_week_8 - investment_decision_week_4
  )

head(investments_deviation)

deviation_market_down <- investments_deviation %>%
  filter(market_shock == "negative") %>%
  pull(deviation)

deviation_market_down_inverse <- (-1) * deviation_market_down

deviation_market_up <- investments_deviation %>%
  filter(market_shock == "positive") %>%
  pull(deviation)


# Create data frame
deviation_market <- data.frame() %>%
  bind_rows(
    as.data.frame(deviation_market_down_inverse) %>%
      rename(value = deviation_market_down_inverse) %>%
      mutate(measure = "down_inverse", .before = everything()),
    as.data.frame(deviation_market_up) %>%
      rename(value = deviation_market_up) %>%
      mutate(measure = "up", .before = everything())
  )


investment_decisions_deviation_distribution_visualization <-
  function(x, title, subtitle = NULL, path, y_limits = NULL) {
    plot <-
      ggplot(mapping = aes(x = x)) +
      geom_histogram(binwidth = 1) +
      scale_y_continuous(breaks = scales::pretty_breaks(), limits = y_limits) +
      labs(
        title = title,
        subtitle = subtitle,
        x = "Investment Decision",
        y = "Frequency"
      ) +
      theme_minimal()
    
    ggsave(
      plot = plot,
      path,
      width = 16,
      height = 10,
      units = "cm",
      bg = "white"
    )
    
    print(plot)
    
    # Shut down current graphics device
    dev.off()
  }

investment_decisions_deviation_distribution_visualization(
  x = deviation_market_down_inverse,
  title = "Distribution of Inverse Deviations in Investment Decision",
  subtitle = "Negative Shock Market",
  path = "../figures/investment-decision-deviations-negative-market-distribution.png"
)

investment_decisions_deviation_distribution_visualization(
  x = deviation_market_up,
  title = "Distribution of Deviations in Investment Decision",
  subtitle = "Positive Shock Market",
  path = "../figures/investment-decision-deviations-positive-market-distribution.png"
)


ggplot(data = deviation_market, mapping = aes(x = value)) +
  geom_histogram(binwidth = 1) +
  scale_y_continuous(breaks = scales::pretty_breaks()) +
  facet_wrap(
    ~ measure,
    ncol = 1,
    labeller = labeller(
      measure = c(
        down_inverse = "Negative Shock (Inverse)",
        up = "Positive Shock"
      )
    )
  ) +
  labs(
    title = "Distributions of Deviations in Investment Decision",
    x = "Investment Decision",
    y = "Frequency"
  ) +
  theme_minimal()

ggsave(
  plot = last_plot(),
  filename = "../figures/investment-decision-deviations-distributions.png",
  width = 16,
  height = 10 * 1.5,
  units = "cm",
  bg = "white"
)

# Shut down current graphics device
dev.off()


wilcox.test(
  x = deviation_market_down_inverse,
  y = deviation_market_up,
  alternative = "two.sided",
  paired = FALSE,
  exact = FALSE,
  conf.level = 0.9
)

# Conclusion: Since p-value = 0.2491 > p-critical = 0.10 we fail to reject the
# null hypothesis. The change in the investment decisions following a positive
# market shock is not significantly different to the inverse change in the
# investment decisions following a negative market shock.


## Subgroup analysis (only observations with non-zero change) -------------

investments_deviation_nonzero_change <- investments_deviation %>%
  filter(deviation != 0)

investments_deviation_nonzero_change

nrow(investments_deviation_nonzero_change)

nrow(
  investments_deviation_nonzero_change %>%
    filter(market_shock == "positive")
)

nrow(
  investments_deviation_nonzero_change %>%
    filter(market_shock == "negative")
)

# Observation: 11 (out of 30) respondents changed their investment decision
# after the market shocks. Out of those 11 people, 6 experienced a negative
# and 5 a positive market shock.


investments_deviation_nonzero_change <-
  investments_deviation_nonzero_change %>%
  left_join(
    x = .,
    y = survey_responses %>%
      select(-investment_decision_week_4, -investment_decision_week_8),
    by = c("identification_number" = "identification_number"),
    keep = FALSE
  )

View(investments_deviation_nonzero_change)


deviation_nonzero_change_market_down <-
  investments_deviation_nonzero_change %>%
  filter(market_shock == "negative") %>%
  pull(deviation)

deviation_nonzero_change_market_down_inverse <-
  (-1) * deviation_nonzero_change_market_down

deviation_nonzero_change_market_up <-
  investments_deviation_nonzero_change %>%
  filter(market_shock == "positive") %>%
  pull(deviation)


wilcox.test(
  x = deviation_nonzero_change_market_down_inverse,
  y = deviation_nonzero_change_market_up,
  alternative = "two.sided",
  paired = FALSE,
  exact = FALSE,
  conf.level = 0.9
)

# Conclusion: Since p-value = 0.0814 < p-critical = 0.10 we reject the null
# hypothesis in favor of the alternative hypothesis. The change in the
# investment decisions following a positive market shock is significantly
# different to the inverse change in the investment decisions following a
# negative market shock at the 10 percent level.


# Subgroup analysis (only observations with trading experience) -----------

investments_deviation_trading_experience <- investments_deviation %>%
  left_join(
    x = .,
    y = survey_responses %>%
      select(-investment_decision_week_4, -investment_decision_week_8),
    by = c("identification_number" = "identification_number"),
    keep = FALSE
  ) %>%
  # Filter for trading experience
  filter(
    stock_ownership == "I trade occasionally" |
      stock_ownership == "I have traded frequently for more than 1 year" |
      stock_ownership == "I own 1-5 stocks" |
      stock_ownership == "I own 6-10 stocks" |
      stock_ownership == "I own more than 10 stocks" |
      trading_experience_cryptocurrency == "Yes, but I do not trade frequently" |
      trading_experience_cryptocurrency == "Yes, I trade frequently"
  )

nrow(investments_deviation_trading_experience)

# Observation: 15 out of 30 participants have had at least some trading
# experience


deviation_trading_experience_market_down <-
  investments_deviation_trading_experience %>%
  filter(market_shock == "negative") %>%
  pull(deviation)

deviation_trading_experience_market_down_inverse <-
  (-1) * deviation_trading_experience_market_down

deviation_trading_experience_market_up <-
  investments_deviation_trading_experience %>%
  filter(market_shock == "positive") %>%
  pull(deviation)


wilcox.test(
  x = deviation_trading_experience_market_down_inverse,
  y = deviation_trading_experience_market_up,
  alternative = "two.sided",
  paired = FALSE,
  exact = FALSE,
  conf.level = 0.9
)

# Conclusion: Since p-value = 0.4368 > p-critical = 0.10 we fail to reject the
# null hypothesis. The change in the investment decisions following a positive
# market shock is not significantly different to the inverse change in the
# investment decisions following a negative market shock.
