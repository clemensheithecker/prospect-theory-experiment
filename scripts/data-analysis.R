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


# Mean difference in differences ------------------------------------------

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
diff(
  c(
    diff(mean_dd_matrix$positive),
    diff(mean_dd_matrix$negative)
  )
)

# Observation: A negative market shock decreases the mean investment decision
# by 0.26 (-0.26) euros and with that the average amount of money invested.


# The effect of a positive shock on the mean investment decision
diff(
  c(
    diff(mean_dd_matrix$negative),
    diff(mean_dd_matrix$positive)
  )
)

# Observation: A positive market shock increases the mean investment decision
# by 0.26 euros and with that the average amount of money invested.


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
diff(
  c(
    diff(sd_dd_matrix$positive),
    diff(sd_dd_matrix$negative)
  )
)

# Observation: A negative market shock increases the standard deviation by 0.72
# euros and with that the variability in the investment decision distribution.


# The effect of a positive shock on the standard deviation investment decision
diff(
  c(
    diff(sd_dd_matrix$negative),
    diff(sd_dd_matrix$positive)
  )
)

# Observation: A positive market shock decreases the standard deviation by 0.72
# (-0.72) euros and with that the variability in the investment decision
# distribution.


# Mann-Whitney U Test -----------------------------------------------------

# Null hypothesis: Individuals react symmetrically to positive and negative
# shocks. Thus, the deviation of the investment decision values in the neutral
# market state and the investment decision values in the shock market state are
# equal.

# Alternative hypothesis: Individuals react asymmetrically to positive and
# negative shocks. Thus, deviation of the investment decision values in the
# neutral market state and the investment decision values in the shock market
# state are not equal.


# Data transformation: absolute mean differences

investments_deviation <- investments %>%
  select(
    market_shock,
    identification_number,
    investment_decision_week_4,
    investment_decision_week_8
  ) %>%
  mutate(
    deviation = case_when(
      market_shock == "positive" ~
        investment_decision_week_8 - investment_decision_week_4,
      market_shock == "negative" ~
        investment_decision_week_4 - investment_decision_week_8
    )
  )

head(investments_deviation)

deviation_market_down <- investments_deviation %>%
  filter(market_shock == "negative") %>%
  pull(deviation)

deviation_market_up <- investments_deviation %>%
  filter(market_shock == "positive") %>%
  pull(deviation)


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
  }

investment_decisions_deviation_distribution_visualization(
  x = deviation_market_down,
  title = "Distribution of Deviations in Investment Decision",
  subtitle = "Negative Shock Market",
  path = "../figures/investment-decision-deviations-negative-market-distribution.png"
)

investment_decisions_deviation_distribution_visualization(
  x = deviation_market_up,
  title = "Distribution of Deviations in Investment Decision",
  subtitle = "Positive Shock Market",
  path = "../figures/investment-decision-deviations-positive-market-distribution.png"
)


wilcox.test(
  x = deviation_market_down,
  y = deviation_market_up,
  alternative = "two.sided",
  paired = FALSE,
  exact = FALSE,
  conf.level = 0.95
)

# Conclusion: Since p-value = 0.2491 < p-critical 0.5 the null hypothesis is to
# be rejected. The change in the investment decisions following a positive
# market shock is significantly different to the change in the investment
# decisions following a negative market shock. This difference is significant at
# the 5% level.
