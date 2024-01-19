pacman::p_load(
  rio,
  here,
  dplyr,
  epikit,
  forcats,
  readr,
  styler, 
  tidyverse,
  janitor
  )

# Good if data is perfectly typed, else use load_csv
# surv_raw <- import(here('data','surveillance_linelist_20141201.csv'))

# load_csv allows to set types on import, which is recommended, rest with mutate


# Load data --------------------------------------
# Some Column Types set during import, if no complex mutation needed


data <- read_csv(
          here("data","surveillance_linelist_20141201.csv"), 
          name_repair = "universal",
          col_types = cols(
            row_num = col_skip(),
            onset.date = col_date(format = "%m/%d/%Y"), 
            gender = col_factor(levels = c("m", "f")), 
            age = col_integer(), 
            age.unit = col_factor(levels = c("months","years")), 
            wt..kg. = col_integer(), 
            ht..cm. = col_integer(), 
            fever = col_character(), 
            date.of.report = col_date(format = "%m/%d/%Y")
          ),
        )
      
# Transform data --------------------------------------

# Columns to recast types after mutation
cols_log = c("fever", "chills", "cough", "aches", "vomit", "epilink")
cols_fac = c("district_res","district_det", "admin3pcod","case_def", "hospital")

data <- data |>
  
  ###
  clean_names() |>
  
  ###
  rename(date_onset = onset_date,
         date_report = date_of_report,
         district_res = adm3_name_res,
         district_det = adm3_name_det)  |>
  
  ###
  mutate(
    across(cols_log, ~ as.logical(.x == "yes")),
    
    wt_kg = ifelse(wt_kg < 0, NA, wt_kg),
    bmi = ifelse(bmi < 0, NA, bmi),
   
    hospital = recode(hospital,
      "Mitilary Hospital" = "Military Hospital",
      "Port Hopital" = "Port Hospital",
      "Port" = "Port Hospital",
      "St. Mark's Maternity Hospital (SMMH)" = "SMMH"),
 
    case_def = case_when(
      lab_confirmed == TRUE             ~ "Confirmed case",
      epilink == "yes" & fever == "yes" ~ "Suspect case",
      TRUE                              ~ "To investigate"),

    age_years = case_when(
      age_unit == 'months'   ~ age/12,
      age_unit == 'years'    ~ age, 
      age_unit == NA       ~ age ),
    
    adult = ifelse(age >= 18, TRUE, FALSE),
   
    # fix temperatures recorded in Fahrenheit
    temp = case_when(
      temp > 90 & temp < 120 ~ (temp - 32) * 5 / 9,
      TRUE                   ~ temp),
    
    # make age categories
    age_cat = age_categories(age_years,
                             lower = 0,
                             upper = 70,
                             by = 5),
    
    across(cols_fac, as.factor),
  ) |>
  
  ###
  distinct()

