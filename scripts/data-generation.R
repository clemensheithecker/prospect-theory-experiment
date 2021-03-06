# Set working directory to source file location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Clear environment
rm(list = ls())


# Load libraries ----------------------------------------------------------

# A package to convert a data frame into an exportable format
library(flextable)

library(officer)

library(tidyverse)


# Set seed ----------------------------------------------------------------

set.seed(8642)


# Generate investment factors ---------------------------------------------

generate_investment_factors_table <-
  function(n,
           mean_neutral,
           sd_neutral,
           mean_up,
           sd_up) {
    # Check if n is even or odd; if odd add 1 to make it even
    if ((n %% 2) != 0) {
      n <- n + 1
    }
    
    # Create empty data frame with n rows
    investment_factors <- data.frame(tibble(id = 1:n))
    
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
      
      # Positive shock (every odd number)
      
      # Draw positive shock values from positive shock normal distribution
      values_up <- round(
        rnorm(n / 2, mean = mean_up, sd = sd_up),
        digits = 2
      )
      
      # Index
      index_up <- 1
      
      for (odd_row in seq(from = 1, to = n, by = 2)) {
        investment_factors[odd_row, round_name] <- values_up[index_up]
        
        # Increment index
        index_up <- index_up + 1
      }
      
      
      # Negative shock (every even number)
      
      # Derive negative shock values mirroring positive shock values
      values_down <- mean_neutral - (values_up - mean_neutral)
      
      # Index
      index_down <- 1
      
      for (even_row in seq(from = 2, to = n, by = 2)) {
        investment_factors[even_row, round_name] <- values_down[index_down]
        
        # Increment index
        index_down <- index_down + 1
      }
    }
    
    # Check for any negative values (except for 1st column) and change them to 0
    investment_factors[, -1][investment_factors[, -1] < 0] <- 0
    
    return(investment_factors)
  }


investment_factors <- generate_investment_factors_table(
  # Number of participants (even number)
  n = 44,
  # Neutral distribution: mean
  mean_neutral = 1.2,
  # Neutral distribution: standard deviation
  sd_neutral = 0.1,
  # Positive shock distribution: mean
  mean_up = 1.8,
  # Positive shock distribution: standard deviation
  sd_up = 0.1
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


# Export dataframe --------------------------------------------------------

# Save as RData file
save(
  investment_factors,
  file = "../data/investment-factors.RData"
)

# Save as CSV file
write.csv(
  investment_factors,
  file = "../data/investment-factors.csv",
  row.names = FALSE
)


# Distribution visualizations ---------------------------------------------

dataframe_values_to_vector <- function(dataframe, columns, row_indices) {
  vector <- c()
  
  for (column in columns) {
    vector <- append(
      vector,
      dataframe[row_indices, column]
    )
  }
  
  return(vector)
}


# Neutral market state

investment_factors_neutral <- dataframe_values_to_vector(
  dataframe = investment_factors,
  columns = c("round_1", "round_2", "round_3", "round_4"),
  row_indices = 1:nrow(investment_factors)
)

investment_factors_neutral_distribution <-
  ggplot(mapping = aes(x = investment_factors_neutral)) +
    geom_histogram(binwidth = 0.1) +
    labs(
      title = "Distribution of Investment Factors",
      subtitle = "Neutral Market",
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

indices_up <- seq(from = 1, to = nrow(investment_factors), by = 2)

investment_factors_up <- dataframe_values_to_vector(
  dataframe = investment_factors,
  columns = c("round_5", "round_6", "round_7", "round_8", "round_9"),
  row_indices = indices_up
)

investment_factors_up_distribution <-
  ggplot(mapping = aes(x = investment_factors_up)) +
    geom_histogram(binwidth = 0.1) +
    labs(
      title = "Distribution of Investment Factors",
      subtitle = "Positive Shock Market",
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

indices_down <- seq(from = 2, to = nrow(investment_factors), by = 2)

investment_factors_down <- dataframe_values_to_vector(
  dataframe = investment_factors,
  columns = c("round_5", "round_6", "round_7", "round_8", "round_9"),
  row_indices = indices_down
)

investment_factors_down_distribution <-
  ggplot(mapping = aes(x = investment_factors_down)) +
    geom_histogram(binwidth = 0.1) +
    labs(
      title = "Distribution of Investment Factors",
      subtitle = "Negative Shock Market",
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


# Create data frame
investment_factors_distribution <- data.frame() %>%
  bind_rows(
    as.data.frame(investment_factors_neutral) %>%
      rename(value = investment_factors_neutral) %>%
      mutate(measure = "neutral", .before = everything()),
    as.data.frame(investment_factors_down) %>%
      rename(value = investment_factors_down) %>%
      mutate(measure = "down", .before = everything()),
    as.data.frame(investment_factors_up) %>%
      rename(value = investment_factors_up) %>%
      mutate(measure = "up", .before = everything())
  )

ggplot(
  data = investment_factors_distribution,
  mapping = aes(x = value)
) +
  geom_histogram(binwidth = 0.1) +
  facet_wrap(
    ~ measure,
    ncol = 1,
    labeller = labeller(
      measure = c(
        neutral = "Neutral Market State",
        down = "Negative Shock",
        up = "Positive Shock"
      )
    )
  ) +
  labs(
    title = "Distributions of Investment Factors",
    x = "Investment Factor",
    y = "Frequency"
  ) +
  theme_minimal()

ggsave(
  plot = last_plot(),
  file = "../figures/investment-factors-distributions.png",
  width = 16,
  height = 10 * 2.25,
  units = "cm",
  bg = "white"
)


# Validity check ----------------------------------------------------------

# mean_neutral = 1.2

all(1.2 - (investment_factors_up - 1.2) == investment_factors_down)
