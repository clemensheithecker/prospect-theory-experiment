# Set working directory to source file location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Clear environment
rm(list = ls())


# Load libraries ----------------------------------------------------------

library(tidyverse)

# A package to convert a data frame into an exportable format
library(flextable)


# Variables ---------------------------------------------------------------

# Number of rounds
rounds <- 6

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


# Create mirroring negative shock

# Negative shock distribution: mean
mean_down = mean_neutral - (mean_up - mean_neutral)

# Negative shock distribution: standard deviation
sd_down = sd_up


# Generate prices ---------------------------------------------------------

generate_price_table <-
  function(n,
           rounds,
           mean_neutral,
           sd_neutral,
           mean_up,
           sd_up,
           mean_down,
           sd_down) {
    prices <- data.frame(
      tibble(id = 1:n)
    )
    
    # Neutral
    for (round in 1:(rounds / 2)) {
      round_name <- paste("round_", round, sep = "")
      
      prices[round_name] = round(
        rnorm(n, mean = mean_neutral, sd = sd_neutral),
        digits = 2
      )
    }
    
    # Volatility shock
    for (round in (rounds / 2 + 1):(rounds + 1)) {
      round_name <- paste("round_", round, sep = "")
      
      # Up
      prices[1:(n / 2), round_name] = round(
        rnorm(n / 2, mean = mean_up, sd = sd_up),
        digits = 2
      )
      
      # Down
      prices[(n / 2 + 1):n, round_name] = round(
        rnorm(n / 2, mean = mean_down, sd = sd_down),
        digits = 2
      )
    }
    
    return(prices)
}


prices <- generate_price_table(
  n = n,
  rounds = rounds,
  mean_neutral = mean_neutral,
  sd_neutral = sd_neutral,
  mean_up = mean_up,
  sd_up = sd_up,
  mean_down = mean_down,
  sd_down = sd_down
)

str(prices)


# Export table to word ----------------------------------------------------

flextable <- flextable(data = prices) %>%
  add_header_row(
    values = c("", "Returns"),
    colwidths = c(1, 7)
  ) %>%
  theme_box %>%
  autofit %>%
  align(align = "center", part = "all")

flextable

save_as_docx(flextable, path = "../documents/prices.docx")


x# Distribution visualizations ---------------------------------------------

# Neutral

neutral <- as.vector(
  c(
    prices$round_1,
    prices$round_2,
    prices$round_3
  )
)

ggplot(mapping = aes(x = neutral)) +
  geom_histogram(bins = 8) +
  labs(
    title = "Distribution of Neutral Market State Returns",
    x = "Returns",
    y = "Frequency"
  ) +
  theme_minimal()

# Up

up <- as.vector(
  c(
    prices[1:(n / 2), "round_4"],
    prices[1:(n / 2), "round_5"],
    prices[1:(n / 2), "round_6"],
    prices[1:(n / 2), "round_7"]
  )
)

ggplot(mapping = aes(x = up)) +
  geom_histogram(bins = 8) +
  labs(
    title = "Distribution of Positive Market Shock Returns",
    x = "Returns",
    y = "Frequency"
  ) +
  theme_minimal()

# Down

down <- as.vector(
  c(
    prices[(n / 2 + 1):n, "round_4"],
    prices[(n / 2 + 1):n, "round_5"],
    prices[(n / 2 + 1):n, "round_6"],
    prices[(n / 2 + 1):n, "round_7"]
  )
)

ggplot(mapping = aes(x = down)) +
  geom_histogram(bins = 8) +
  labs(
    title = "Distribution of Negative Market Shock Returns",
    x = "Returns",
    y = "Frequency"
  ) +
  theme_minimal()
