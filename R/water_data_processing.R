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
header_water <- read_xlsx(
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

for(i in idx1) {
  header_water_trans[(i+1):(i+n1), "V1_complete"] <- header_water_trans[i, "V1"]
}

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
         year, 
         starts_with("rural"),
         starts_with("urban")) |> 
  filter(country_area_or_territory %in% c("Portugal", "Brazil"))

# save processed data
# Note that the folder `processed` has to be created before
write_csv(processed_data_water, 
          file = here::here("data/processed/water_data.csv"))
