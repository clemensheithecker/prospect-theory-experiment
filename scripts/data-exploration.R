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
    # capital_week_5,
    # capital_week_9,
    capital_total
  ) %>%
  group_by(market_shock) %>%
  filter(capital_total == max(capital_total))

investor_success_summary
