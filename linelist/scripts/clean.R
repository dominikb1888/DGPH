pacman::p_load(
  rio,
  here,
  dplyr,
  forcats
  )

surv_raw <- import(here('data','surveillance_linelist_20141201.csv'))

# Oldschool Method:
# surv_raw$gender <- as.factor(surv_raw$gender)

# Using Tidyverse
surv_raw <- surv_raw %>%
 mutate(gender = recode_factor(as.factor(gender), m="male", f="female"))
  
 

View(surv_raw)

