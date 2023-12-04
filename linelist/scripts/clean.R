pacman::p_load(
  rio,
  here,
  dplyr,
  forcats,
  readr,
  styler
)

# Good if data is perfectly typed, else use load_csv
# surv_raw <- import(here('data','surveillance_linelist_20141201.csv'))

# load_csv allows to set types on import, which is recommended, rest with mutate

# Load data --------------------------------------
data <- read_csv(
  here("data", "surveillance_linelist_20141201.csv"),
  name_repair = "universal",
  col_types = cols(
    .default = col_guess(),
    onset.date = col_date(format = "%m/%d/%Y"),
    date.of.report = col_date(format = "%m/%d/%Y"),
    gender = col_factor(c("m", "f")),
    age.unit = col_factor(c("years", "months"))
  )
)
# Oldschool Method:
# surv_raw$gender <- as.factor(surv_raw$gender)

# Using Tidyverse

# Transform data --------------------------------------
cols_log = c("fever", "chills", "cough", "aches", "vomit", "epilink")

data <- data |>
  mutate(across(cols_log, ~ as.logical(.x == "yes")))

glimpse(data)


# Plot data --------------------------------------