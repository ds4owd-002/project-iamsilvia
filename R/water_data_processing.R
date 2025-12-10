################################################################################
#
# This script uses the JMP WASH data and transforms the jmp_wld_water data set 
# from wide to long format, renames variables, and selects variables and 
# countries of interest to make it analysis-ready.
#
################################################################################


library(readxl)
library(here)
library(tidyverse)
library(janitor) # package with functions as clean_names(), snake_case

# Read the dataset without header or column names
water_raw <- read_xlsx(
  here::here("data/raw/JMP_WASH_HH_2025_by_country-2.xlsx"),
  sheet = "Water",
  skip = 3, 
  col_names = FALSE) #read all rows as data
# col_names is an argument inside read_xlsx(), not a standalone function

# Read the header of the dataset separately
header_water <- read_xlsx(
  here::here("data/raw/JMP_WASH_HH_2025_by_country-2.xlsx"),
  sheet = "Water",
  n_max = 3,
  col_names = FALSE)

# Transform the header
header_water_trans <- t(header_water) |> # transpose for easy manipulation
  as_tibble() |> # convert to tibble to use functions of tidyverse
  mutate( # create auxiliary variables _complete to use in the for loop
    V1_complete = case_when( #creates a new column V1_complete that
      V1 == "DRINKING WATER" ~ NA, #replaces "drinking water" to NA
      str_detect(V1, "Prop") ~ NA, #and "Prop" to NA
      .default = V1 #and keeps the rest 
    ),
    V2_complete = V2 #creates a new column V2_complete equal to V2
  ) |> 
  relocate(V1_complete, .after = V1) |> 
  relocate(V2_complete, .after = V2)

values_to_repeat <- c("RURAL", "URBAN", "TOTAL") #header categories labels to repeat

# number of rows to fill
n1 <- 4  #when we find a RURAL/URBAN/TOTAL in V1, copy it down 4 rows
n2 <- 6  #when we find a RURAL/URBAN/TOTAL in V2, copy it down 6 rows

# Loops to repeat the values "RURAL", "URBAN", "TOTAL" where necessary
idx1 <- which(header_water_trans |> pull(V1) %in% values_to_repeat) #extracts the V1 column as a vector. 
#Checks which rows have values equal to "RURAL", "URBAN", or "TOTAL". idx1 = the row numbers where V1 is RURAL/URBAN/TOTAL
idx2 <- which(header_water_trans |> pull(V2) %in% values_to_repeat)

# Take the value in row i, column V1, and copy it into the next n1 rows in column V1_complete
for(i in idx1) {
  header_water_trans[(i+1):(i+n1), "V1_complete"] <- header_water_trans[i, "V1"] #For each row i where V1 is RURAL/URBAN/TOTAL, 
  #copy that value into the next n1 rows in V1_complete.
}
# As idx1 = c(5, 10, 15), the loop will run three times:
# 1st time with i = 5
# 2nd time with i = 10
# 3rd time with i = 15

for(j in idx2) {
  header_water_trans[(j+1):(j+n2), "V2_complete"] <- header_water_trans[j, "V2"] #For each row j where V2 is RURAL/URBAN/TOTAL,
  #copy that value into the next n2 rows in V2_complete.
}

View(header_water_trans)

# create the column names by combining the three values 
column_names <- header_water_trans |> 
  unite(complete_names,
        V1_complete,
        V2_complete,
        V3, 
        sep = "_",
        na.rm = TRUE, #If one of the components is NA, ignore it instead of including NA in the string
        remove = FALSE) |> #Do NOT delete the original columns after uniting
  pull(complete_names)

# use the created column names for the dataset
colnames(water_raw) <- column_names #replaces all column names in the dataset water_raw
#The length of column_names must equal the number of columns in water_raw - CONFIRMED
#colnames - set column names in water_raw dataset

# clean the names of your dataset (remove spaces, manage duplicates)
  

#Transform dataset long on indicator
water_data <- water_raw |>
  clean_names(case = "snake") |> 
  select(country_area_or_territory,
         iso3,
         year,
         population_thousands,
         starts_with("rural"),
         starts_with("urban"), 
         starts_with("total"),
         income_groupings, 
         sdg_region)

water_data_long <- water_data |>   
  # 1) Rename some columns to clean/more conventional names
  rename( 
    country = country_area_or_territory,
    population = population_thousands,
    income_id = income_groupings
    ) |> 
  #2) Go from wide (rural_*, urban_*, total_*) to long
  pivot_longer(
    cols = matches("^(rural|urban|total)_"),   # all columns that start with rural_ or urban_
    names_to = c("residence", "varname_long"),
    names_pattern = "^(rural|urban|total)_(.*)$",  # Take column name that starts with rural/urban/total and split at the first underscore
    values_to = "percent") |> 
  # 3) Remove annual rate variables if present
  filter(!str_detect(varname_long, "^annual_rate_of_change")) |>
  # 4) Clean residence and create short codes for variables
  mutate(
    residence = str_to_title(residence),
    varname_short = case_match(
      varname_long,
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
      .default = varname_long),
    varname_long = case_match(
      varname_short,
      "wat_bas"        ~ "At least basic drinking water",
      "wat_lim"        ~ "Limited drinking water (>30 mins)",
      "wat_unimp"      ~ "Unimproved source",
      "wat_surf"       ~ "Surface water",
      "wat_safe"       ~ "Safely managed drinking water",
      "wat_accessible" ~ "Accessible on premises",
      "wat_available"  ~ "Available when needed",
      "wat_free"       ~ "Free from contamination",
      "wat_piped"      ~ "Piped water",
      "wat_nonpiped"   ~ "Non-piped water",
      .default         = varname_short),
    income_id_short = case_match(
      income_id,
      "High income"              ~ "HIC",
      "Upper middle income"      ~ "UMIC",
      "Lower middle income"      ~ "LMIC",
      "Low income"               ~ "LIC"
    ))|>
  relocate(varname_short, .before = varname_long) |> 
  relocate(income_id, .after = percent) |> 
  relocate(income_id_short, .before = income_id) |> 
  relocate(sdg_region, .after = income_id)


write_csv(water_data_long,
  file = here("data/processed/water_data_long.csv")
)
