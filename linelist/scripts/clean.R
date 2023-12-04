pacman::p_load(
  rio,
  here,
  dplyr,
  epikit,
  forcats,
  readr,
  styler, 
  tidyverse,
  janitor,       # to clean column names
  
  sf,            # to manage spatial data using a Simple Feature format
  tmap,          # to produce simple maps, works for both interactive and static maps
  OpenStreetMap, # to add OSM basemap in ggplot map
  spdep 
)

# Good if data is perfectly typed, else use load_csv
# surv_raw <- import(here('data','surveillance_linelist_20141201.csv'))

# load_csv allows to set types on import, which is recommended, rest with mutate


# Load data --------------------------------------
# Some Column Types set during import, if no complex mutation needed

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

# Transform data --------------------------------------

# Columns to recast types after mutation
cols_log = c("fever", "chills", "cough", "aches", "vomit", "epilink")
cols_fac = c("district_res","district_det", "admin3pcod","case_def")

data <- data |>
  
  ###
  clean_names() |>
  
  ###
  rename(date_onset = onset_date,
         date_report = date_of_report,
         district_res = adm3_name_res,
         district_det = adm3_name_det)  |>
  
  ###
  select(-row_num) |>
  
  ###
  mutate(
    across(cols_log, ~ as.logical(.x == "yes")),
    
    adult = ifelse(age >= 18, TRUE, FALSE),
     
    wt_kg = ifelse(wt_kg < 0, NA, wt_kg),
    
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
    
    # fix temperatures recorded in Farinheit
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


glimpse(data)


# Plot data --------------------------------------

# Create a mapplot based on location of cases
data_sf <- data |>
  drop_na(lat,lon) |>
  sf::st_as_sf(coords = c("lon", "lat"), crs = 4326)

sle_adm3 <- read_sf(here("data","gis","shp","sle_admbnda_adm3_1m_gov_ocha_20161017.shp")) |>
  janitor::clean_names() |> # standardize column names
  filter(admin2name %in% c("Western Area Urban", "Western Area Rural")) # filter to keep certain areas

sle_adm3_pop <- import(here("data", "gis", "population", "sle_admpop_adm3_2020_v2.csv")) |>
  janitor::clean_names()

sle_hf <- sf::read_sf(here("data", "gis", "shp", "healthsites.shp")) |> 
  janitor::clean_names() |>
  filter(amenity %in% c("hospital", "clinic", "doctors"))

tmap_mode("plot")

tm_shape(sle_adm3, bbox = c(-13.3, 8.43, -13.2, 8.5)) +     #
  tm_polygons(col = "#F7F7F7") +
  tm_borders(col = "#000000", lwd = 2) +
  tm_text("admin3name")+
tm_shape(data_sf) +
  tm_dots(size=0.08, col='blue', alpha = 0.5) +
  tm_layout(title = "Distribution of Ebola cases")   # give title to map


data_adm <- data_sf |>
  # join the administrative boundary file to the linelist, based on spatial intersection
  sf::st_join(sle_adm3, join = st_intersects) 
  # Keep the old column names and two new admin ones of interest
  select(names(data_sf), admin3name, admin3pcod.y)
  
case_adm3 <- data_adm |> 
  as_tibble() |>
  group_by(admin3pcod.y, admin3name) |>
  summarise(cases = n()) |>
  arrange(desc(cases))

case_adm3

#Closest Health Facility
data_sf_hf <- data_sf |>
  st_join(sle_hf, join = st_nearest_feature) |>
  select(case_id, osm_id, name, amenity) |>
  rename("nearest_clinic" = "name")

data_sf_hf


# Count cases by health facility
hf_catchment <- data_sf_hf %>%   # begin with linelist including nearest clinic data
  as.data.frame() %>%                # convert from shapefile to dataframe
  count(nearest_clinic,              # count rows by "name" (of clinic)
        name = "case_n") %>%         # assign new counts column as "case_n"
  arrange(desc(case_n))              # arrange in descending order

hf_catchment                         # print to console

tmap_mode("view")   # set tmap mode to interactive  

# plot the cases and clinic points 
tm_shape(data_sf_hf) +            # plot cases
  tm_dots(size=0.08,                  # cases colored by nearest clinic
          col='nearest_clinic') +    
  tm_shape(sle_hf) +                    # plot clinic facilities in large black dots
  tm_dots(size=0.3, col='black', alpha = 0.4) +      
  tm_text("name") +                   # overlay with name of facility
  tm_view(set.view = c(-13.2284, 8.4699, 13), # adjust zoom (center coords, zoom)
          set.zoom.limits = c(13,14))+
  tm_layout(title = "Cases, colored by nearest clinic")

