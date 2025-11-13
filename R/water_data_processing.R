library(readxl)
library(here)
library(janitor) # check that this package is installed

# Read the dataset without header or column names
raw_data_water <- read_xlsx(
  here::here("data/raw/JMP_WASH_HH_2025_by_country-2.xlsx"),
  sheet = "Water",
  skip = 3, 
  col_names = FALSE)

# Read the header of the dataset separately
header_sanitation <- read_xlsx(
  here::here("data/raw/JMP_WASH_HH_2025_by_country-2.xlsx"),
  sheet = "Water",
  n_max = 3,
  col_names = FALSE)

# Transform the header
header_water_trans <- t(header_water) |> # transpose for easy manipulation
  as_tibble() |> # convert to tibble to use functions of tidyverse
  mutate( # create auxiliary variables _complete to use in the for loop
    V1_complete = case_when(
      V1 == "DRINKING WATER" ~ NA, 
      str_detect(V1, "Prop") ~ NA,
      .default = V1
    ),
    V2_complete = V2
  ) |> 
  relocate(V1_complete, .after = V1) |> 
  relocate(V2_complete, .after = V2)

View(header_water_trans)

values_to_repeat <- c("RURAL", "URBAN", "TOTAL")

# number of rows to fill
n1 <- 4  
n2 <- 6

# Loops to repeat the values "RURAL", "URBAN", "TOTAL" where necessary
idx1 <- which(header_water_trans |> pull(V1) %in% values_to_repeat)
idx2 <- which(header_water_trans |> pull(V2) %in% values_to_repeat)

# Take the value in row i, column V1, and copy it into the next n1 rows in column V1_complete
for(i in idx1) {
  header_water_trans[(i+1):(i+n1), "V1_complete"] <- header_water_trans[i, "V1"]
}
# As idx1 = c(5, 10, 15), the loop will run three times:
# 1st time with i = 5
# 2nd time with i = 10
# 3rd time with i = 15

for(j in idx2) {
  header_water_trans[(j+1):(j+n2), "V2_complete"] <- header_water_trans[j, "V2"]
}

View(header_water_trans)

# create the column names by combining the three values 
column_names <- header_water_trans |> 
  unite(complete_names,
        V1_complete,
        V2_complete,
        V3, 
        sep = "_",
        na.rm = TRUE,
        remove = FALSE) |> 
  pull(complete_names)

# use the created column names for the dataset
colnames(raw_data_water) <- column_names

# clean the names of your dataset (remove spaces, manage duplicates)
data_water <- raw_data_water |> 
  clean_names(case = "snake")


# here you can select the variables of interest and filter countries  
processed_data_water <- data_water |> 
  select(country_area_or_territory,
         iso3,
         year,
         sdg_region,
         income_groupings,
         population_thousands,
         starts_with("rural"),
         starts_with("urban")) |> 
  filter(country_area_or_territory %in% c("Portugal", "Brazil", "Angola", "Cabo Verde", 
                                          "Guinea-Bissau", "Equatorial Guinea", "Mozambique", 
                                          "Sao Tome and Principe", "Timor-Leste"))

library(dplyr)
library(tidyr)
library(stringr)
library(forcats)

water_data_long <- water_data |>
  rename(
    country = country_area_or_territory,
    region = sdg_region,
    population = population_thousands,
    income = income_groupings
    ) |> 
  pivot_longer(
    cols = matches("^(rural|urban)_"),   # all columns that start with rural_ or urban_
    names_to = c("residence", "varname_short"),
    names_pattern = "^(rural|urban)_(.*)$",  # split at the first underscore
    values_to = "percent") |>
  mutate(
    varname_short = case_match(
      varname_short,
      "at_least_basic" ~ "wat_bas",
      "limited_more_than_30_mins" ~ "wat_lim",
      "unimproved" ~ "wat_unimp",
      "surface_water" ~ "wat_surf",
      "safely_managed" ~ "wat_safe",
      "accessible_on_premises" ~ "wat_accessible",
      "available_when_needed" ~ "wat_available",
      "free_from_contamination" ~ "wat_free",
      "piped" ~ "wat_piped",
      "non_piped" ~ "wat_nonpiped",
      .default = varname_short)) |> 
  mutate(
    residence = str_to_title(residence),
    varname_long = case_match(
      varname_short,
      "wat_bas" ~ "At least basic drinking water",
      "wat_lim" ~ "Limited drinking water (>30 mins)",
      "wat_unimp" ~ "Unimproved source",
      "wat_surf" ~ "Surface water",
      "wat_safe" ~ "Safely managed drinking water",
      "wat_accessible" ~ "Accessible on premises",
      "wat_available" ~ "Available when needed",
      "wat_free" ~ "Free from contamination",
      "wat_piped" ~ "Piped water",
      "wat_nonpiped" ~ "Non-piped water",
      .default = varname_short)) |> 
  mutate(
    varname_short = factor(
      varname_short,
      levels = c("wat_bas","wat_lim","wat_unimp","wat_surf", "wat_rate", "wat_safe", 
                 "wat_accessible", "wat_available", "wat_free", "wat_piped","wat_nonpiped")
      ),
    residence = factor(residence, levels = c("Rural","Urban"))
  )
