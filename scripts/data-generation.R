# Set working directory to source file location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Clear environment
rm(list = ls())


# Load libraries ----------------------------------------------------------

library(tidyverse)

# A package to convert a data frame into an exportable format
library(flextable)

library(officer)


# Variables ---------------------------------------------------------------

# Number of participants (even number)
n <- 48

# Neutral distribution: mean
mean_neutral <- 1.2

# Neutral distribution: standard deviation
sd_neutral <- 0.1

# Positive shock distribution: mean
mean_up <- 1.6

# Positive shock distribution: standard deviation
sd_up <- sd_neutral


# Set seed ----------------------------------------------------------------

set.seed(8642)


# Generate prices ---------------------------------------------------------

generate_investment_factors_table <-
  function(n,
           mean_neutral,
           sd_neutral,
           mean_up,
           sd_up) {
    investment_factors <- data.frame(tibble(id = 1:n))
    
    # Create mirroring negative shock distribution
    mean_down = mean_neutral - (mean_up - mean_neutral)
    sd_down = sd_up
    
    # Neutral market
    for (round in 1:4) {
      round_name <- paste("round_", round, sep = "")
      
      investment_factors[round_name] <-
        round(
          rnorm(n, mean = mean_neutral, sd = sd_neutral),
          digits = 2
        )
    }
    
    # Market with volatility shock
    for (round in 5:9) {
      round_name <- paste("round_", round, sep = "")
      
      # Positive shock
      investment_factors[1:(n / 2), round_name] <-
        round(
          rnorm(n / 2, mean = mean_up, sd = sd_up),
          digits = 2
        )
      
      # Negative shock
      investment_factors[(n / 2 + 1):n, round_name] <-
        round(
          rnorm(n / 2, mean = mean_down, sd = sd_down),
          digits = 2
        )
    }
    
    return(investment_factors)
  }


investment_factors <- generate_investment_factors_table(
  n = n,
  mean_neutral = mean_neutral,
  sd_neutral = sd_neutral,
  mean_up = mean_up,
  sd_up = sd_up
)

head(investment_factors)


# Export table to word ----------------------------------------------------

investment_factors_word <- flextable(investment_factors) %>%
  add_header_row(
    values = c("", "Investment Factors"),
    colwidths = c(1, 9)
  ) %>%
  theme_vanilla %>%
  fontsize(size = 10) %>%
  vline(
    j = c('round_4', 'round_8'),
    border = officer::fp_border(),
    part = "all"
  ) %>%
  align(align = "center", part = "all") %>%
  set_table_properties(width = 1, layout = "autofit")

save_as_docx(
  investment_factors_word,
  path = "../documents/investment-factors.docx",
  pr_section = officer::prop_section(
    page_margins = officer::page_mar(
      bottom = 1,
      top = 1,
      right = 0.75,
      left = 0.75,
      header = 0,
      footer = 0,
      gutter = 0
    )
  )
)


# Save data as RData file -------------------------------------------------

save(investment_factors, file = "../data/investment-factors.RData")


# Distribution visualizations ---------------------------------------------

# Neutral market state

investment_factors_neutral <- as.vector(
  c(
    investment_factors$round_1,
    investment_factors$round_2,
    investment_factors$round_3,
    investment_factors$round_4
  )
)

investment_factors_neutral_distribution <-
  ggplot(mapping = aes(x = investment_factors_neutral)) +
    geom_histogram(bins = 8) +
    labs(
      title = "Distribution of Investment Factors in Neutral Market",
      x = "Investment Factor",
      y = "Frequency"
    ) +
    theme_minimal()

ggsave(
  plot = investment_factors_neutral_distribution,
  "../figures/investment-factors-neutral-market-distribution.png",
  width = 16,
  height = 10,
  units = "cm",
  bg = "white"
)


# Positive market shock

investment_factors_up <- as.vector(
  c(
    investment_factors[1:(n / 2), "round_5"],
    investment_factors[1:(n / 2), "round_6"],
    investment_factors[1:(n / 2), "round_7"],
    investment_factors[1:(n / 2), "round_8"],
    investment_factors[1:(n / 2), "round_9"]
  )
)

investment_factors_up_distribution <-
  ggplot(mapping = aes(x = investment_factors_up)) +
    geom_histogram(bins = 8) +
    labs(
      title = "Distribution of Investment Factors for Positive Market Shock",
      x = "Investment Factor",
      y = "Frequency"
    ) +
    theme_minimal()

ggsave(
  plot = investment_factors_up_distribution,
  "../figures/investment-factors-positive-shock-distribution.png",
  width = 16,
  height = 10,
  units = "cm",
  bg = "white"
)


# Negative market shock

investment_factors_down <- as.vector(
  c(
    investment_factors[(n / 2 + 1):n, "round_5"],
    investment_factors[(n / 2 + 1):n, "round_6"],
    investment_factors[(n / 2 + 1):n, "round_7"],
    investment_factors[(n / 2 + 1):n, "round_8"],
    investment_factors[(n / 2 + 1):n, "round_9"]
  )
)

investment_factors_down_distribution <-
  ggplot(mapping = aes(x = investment_factors_down)) +
    geom_histogram(bins = 8) +
    labs(
      title = "Distribution of Investment Factors for Negative Market Shock",
      x = "Investment Factor",
      y = "Frequency"
    ) +
    theme_minimal()

ggsave(
  plot = investment_factors_down_distribution,
  "../figures/investment-factors-negative-shock-distribution.png",
  width = 16,
  height = 10,
  units = "cm",
  bg = "white"
)
