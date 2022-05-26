# Set working directory to source file location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Clear environment
rm(list = ls())


# Load libraries ----------------------------------------------------------

library(gtsummary)

library(tidyverse)

library(xtable)


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


# How many observations are in the sample? --------------------------------

number_observations <- nrow(investments)

number_observations

# 30 observations in the sample of which half (15) were exposed to a negative
# and the other half (15) exposed to a positive shock


# Summary statistics to LaTeX ---------------------------------------------

summary_stats_to_latex <- function(df, file, title, label) {
  # Capture output from xtable
  output <- capture.output(print(
    xtable(
      df,
      type = "latex",
      caption = title,
      # Set the alignment of the columns
      align = c("l", "X", "r", "r", "r", "r"),
      # Set the number of digits
      digits = c(0, 0, 3, 3, 3, 3),
      # Set the format of the columns
      display = c("s", "s", "f", "f", "f", "f"),
      label = label
    ),
    type = "latex",
    include.rownames = FALSE,
    caption.placement = "top",
    booktabs = TRUE,
    tabular.environment = "tabularx",
    width = "\\textwidth",
    comment = FALSE
  ))
  
  caption_position <- which(startsWith(output, "\\caption{"))
  label_position <- which(startsWith(output, "\\label{"))
  
  caption <- output[caption_position]
  label <- output[label_position]
  
  tabularx_position <- which(startsWith(output, "\\begin{tabularx}"))
  
  output <- c(
    output[1:caption_position - 1],
    caption,
    label,
    output[tabularx_position],
    output[tabularx_position + 1:length(output)]
  )
  
  # Remove null values
  output <- output[!is.na(output)]
  
  # Remove unnecessary decimal points
  output <- gsub(".000", "", output)
  
  # Print table
  cat(output, sep = "\n")
  
  # Write to LaTeX file
  write(output, file = file)
}


# Summary statistics ------------------------------------------------------

investments_summary_stats <- investments %>%
  select(
    market_shock,
    identification_number,
    investment_decision_week_4,
    investment_decision_week_8
  ) %>%
  mutate(
    deviation_negative = ifelse(
      market_shock == "negative",
      investment_decision_week_8 - investment_decision_week_4,
      NA
    ),
    deviation_positive = ifelse(
      market_shock == "positive",
      investment_decision_week_8 - investment_decision_week_4,
      NA
    )
  ) %>%
  pivot_wider(
    names_from = market_shock,
    names_prefix = "investment_decision_week_8_",
    values_from = investment_decision_week_8
  ) %>%
  select(-identification_number) %>%
  # Calculate summary statistics
  summarise(
    across(
      everything(),
      list(
        Mean = ~ mean(., na.rm = TRUE),
        SD = ~ sd(., na.rm = TRUE),
        Min = ~ min(., na.rm = TRUE),
        Max = ~ max(., na.rm = TRUE)
      )
    )
  ) %>%
  # Reshape data frame from wide to long format
  pivot_longer(
    cols = everything(),
    names_to = "Variable",
    values_to = "Value"
  ) %>%
  # Separate "Variable" column into two separate columns "Variable" and
  # "Statistic"
  separate(
    Variable,
    into = c("Variable", "Statistic"),
    # Match the last occurance of the underscore
    sep = "_(?!.*_)"
  ) %>%
  # Convert "Variable" to a factor to preserve the ordering of the variables
  mutate(Variable = factor(Variable, levels = unique(Variable))) %>%
  # Reshape "Statistic" and "Value" columns from long to wide format
  pivot_wider(names_from = Statistic, values_from = Value) %>%
  # Reorder columns
  select(Variable, Mean, SD, Min, Max) %>%
  # Round all values to three decimal places
  mutate(across(Mean:Max, round, 3)) %>%
  # Rename and reorder variables
  mutate(
    Variable = factor(
      Variable,
      levels = c(
        "investment_decision_week_4",
        "investment_decision_week_8_negative",
        "investment_decision_week_8_positive",
        "deviation_negative",
        "deviation_positive"
      ),
      labels = c(
        "Investment Decision Week 4",
        "Investment Decision Week 8, Negative Shock Market",
        "Investment Decision Week 8, Positive Shock Market",
        "Investment Decision Deviation, Negative Shock Market",
        "Investment Decision Deviation, Positive Shock Market"
      )
    )
  ) %>%
  arrange(Variable)

investments_summary_stats

# At the end of week 4, individuals invested on average 8.7 euros out of their
# 10 euros. At the end of week 8, individuals who experienced a negative market
# shock invested on average 7.53 euros and individuals who experienced a
# positive marketed shock invested 7.19 out of their 10 euros.
# 
# etc.


summary_stats_to_latex(
  df = investments_summary_stats,
  file = "../reports/figures/investments_summary_stats.tex",
  title = paste0(
    "Investments Summary Statistics (", number_observations, " Observations)"
  ),
  label = "table:InvestmentsSummaryStats"
)


# Summary statistics of participants --------------------------------------

survey_responses %>%
  select(gender:trading_experience_cryptocurrency) %>%
  tbl_summary()
