# Set working directory to source file location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Clear environment
rm(list = ls())


# Load libraries ----------------------------------------------------------

# Clean data frame
library(janitor)

library(tidyverse)


# Load data ---------------------------------------------------------------

survey_responses_raw <- read_csv(
  file = "../data/survey-responses-raw.csv",
  # Specify the column types (f = factor, i = integer)
  col_types = "fiiiiiiiffffffff",
  # No name repair or checks, beyond basic existence of names
  name_repair = "minimal"
) %>%
  # Clean column names
  clean_names() %>%
  # Rename columns
  rename(
    identification_number_a = identification_number,
    student_number_a = student_number,
    investment_decision_week_4_a = investment_decision_week_4,
    identification_number_b = identification_number_2,
    student_number_b = student_number_2,
    investment_decision_week_4_b = investment_decision_week_4_2
  )

glimpse(survey_responses_raw)


# Clean data --------------------------------------------------------------

survey_responses_a <- survey_responses_raw %>%
  filter(copy == "A") %>%
  select(identification_number_a:investment_decision_week_4_a) %>%
  rename(
    identification_number = identification_number_a,
    student_number = student_number_a,
    investment_decision_week_4 = investment_decision_week_4_a
  ) %>%
  arrange(identification_number) %>%
  # only keep unique/distinct rows
  distinct()

head(survey_responses_a)


survey_responses_b <- survey_responses_raw %>%
  filter(copy == "B") %>%
  select(identification_number_b:trading_experience_cryptocurrency) %>%
  rename(
    identification_number = identification_number_b,
    student_number = student_number_b,
    investment_decision_week_4 = investment_decision_week_4_b
  ) %>%
  arrange(identification_number) %>%
  # only keep unique/distinct rows
  distinct()

head(survey_responses_b)


# Check if first three columns match from copy A and copy B
all(
  survey_responses_a == survey_responses_b %>% select(
    identification_number,
    student_number,
    investment_decision_week_4
  )
)

# Show rows that do not match
anti_join(
  survey_responses_a,
  survey_responses_b %>% select(
    identification_number,
    student_number,
    investment_decision_week_4
  )
)


# Export data -------------------------------------------------------------

survey_responses <- survey_responses_b

# Save as RData file
save(
  survey_responses,
  file = "../data/survey-responses.RData"
)

# Save as CSV file
write.csv(
  survey_responses,
  file = "../data/survey-responses.csv",
  row.names = FALSE
)
