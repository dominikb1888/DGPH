pacman::p_load(
  ggplot,
  ggpubr
)

source("scripts/clean.R")

# Group data -------------------------------------

ll_by_case_def <- data |> 
  group_by(gender, case_def) |>
  tally()

ggplot(ll_by_case_def, aes(x=gender, y=n, fill=case_def)) + geom_col()

age_classes <- data |> 
  group_by(
    age_class = ifelse(adult, "adult", "child")) |> 
  tally(sort = T)

ggplot(age_classes, aes(x=age_class, y=n)) + geom_col()

age_table <- data |>
  tabyl(age_cat, gender) |>
  adorn_totals(where="both") |>
  adorn_percentages(denominator = "col") |>
  adorn_pct_formatting() |>
  adorn_ns(position = "front") |>
  adorn_title(                                # adjust titles
    row_name = "Age Category",
    col_name = "Gender")



# Plot data --------------------------------------

pacman::p_load(gridExtra)
plot(tableGrob(age_table))
