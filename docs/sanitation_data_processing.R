install.packages("janitor")

library(readxl)
library(here)
library(janitor)
library(tidyverse)

raw_data_san <- read_xlsx(
  here::here("data/raw/JMP_WASH_HH_2025_by_country-2.xlsx"),
  sheet = "Sanitation",
  skip = 3,
  col_names = FALSE)

header_san <- read_xlsx(
  here::here("data/raw/JMP_WASH_HH_2025_by_country-2.xlsx"),
  sheet = "Sanitation",
  n_max = 3,
  col_names = FALSE)

header_san_trans <- t(header_san) |> 
  as_tibble() |> 
  mutate(
    V1_complete = case_when(
      V1 == "SANITATION" ~ NA,
      .default = V1
    ),
    V2_complete = case_when(
      str_detect(V2, "Prop") ~ NA,
      .default = V2
    )
  ) |> 
  relocate(V1_complete, .after = V1) |> 
  relocate(V2_complete, .after = V2)