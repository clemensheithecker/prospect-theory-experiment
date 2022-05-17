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


# Who was the most successful investor? -----------------------------------

investor_success <- investments %>%
  mutate(
    investment_value_week_5 =
      investment_decision_week_4 * investment_factor_week_5,
    investment_value_week_9 =
      investment_decision_week_8 * investment_factor_week_9,
    capital_week_5 =
      investment_value_week_5 + 10 - investment_decision_week_4,
    capital_week_9 =
      investment_value_week_9 + 10 - investment_decision_week_8,
    capital_total = capital_week_5 + capital_week_9
  )

# Most successful investor by group (positive and negative shock)
investor_success_summary <- investor_success %>%
  select(
    market_shock,
    identification_number,
    student_number,
    # investment_decision_week_4,
    # investment_decision_week_8,
    # capital_week_5,
    # capital_week_9,
    capital_total
  ) %>%
  group_by(market_shock) %>%
  filter(capital_total == max(capital_total))

investor_success_summary


# Investment decision distribution visualization --------------------------

investment_decisions_distribution_visualization <-
  function(x, title, path, y_limits = NULL) {
    plot <-
      ggplot(mapping = aes(x = x)) +
      geom_histogram(binwidth = 1) +
      scale_x_continuous(
        breaks = seq(from = 0, to = 10, by = 1),
        limits = c(-0.5, 10.5)
      ) +
      scale_y_continuous(breaks = scales::pretty_breaks(), limits = y_limits) +
      labs(
        title = title,
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


# Neutral market state

investment_decisions_distribution_visualization(
  x = investments %>%
    select(
      market_shock,
      identification_number,
      investment_decision_week_4,
      investment_decision_week_8
    ) %>%
    pull(investment_decision_week_4),
  title = "Distribution of Investment Decisions in the Neutral Market",
  path = "../figures/investment-decisions-neutral-market-distribution.png"
)


# Positive market shock

investment_decisions_distribution_visualization(
  x = investments %>%
    select(
      market_shock,
      identification_number,
      investment_decision_week_4,
      investment_decision_week_8
    ) %>%
    filter(market_shock == "positive") %>%
    pull(investment_decision_week_8),
  title = "Distribution of Investment Decisions for the Positive Market Shock",
  path = "../figures/investment-decisions-positive-shock-distribution.png",
  y_limits = c(0, 10)
)


# Negative market shock

investment_decisions_distribution_visualization(
  x = investments %>%
    select(
      market_shock,
      identification_number,
      investment_decision_week_4,
      investment_decision_week_8
    ) %>%
    filter(market_shock == "negative") %>%
    pull(investment_decision_week_8),
  title = "Distribution of Investment Decisions for the Negative Market Shock",
  path = "../figures/investment-decisions-negative-shock-distribution.png",
  y_limits = c(0, 10)
)
