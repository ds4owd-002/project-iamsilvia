CPLP Drinking Water Access Analysis (2000–2024)

Capstone Project – WASH Data Analytics

Overview

This project analyses the evolution of access to drinking water in Portuguese-speaking countries (CPLP) over the past 25 years, using global WASH estimates from the WHO/UNICEF Joint Monitoring Programme (JMP). 
The goal is to evaluate national progress toward Sustainable Development Goal 6 (SDG 6) and understand inequalities across rural and urban populations.

The project includes data cleaning, transformation, exploratory analysis, and visualisation using the tidyverse. 
The processed dataset is reshaped into a long, analysis-ready format for reproducible research.


Data Source

This project uses publicly available estimates from the:

WHO/UNICEF Joint Monitoring Programme (JMP)
Water Supply, Sanitation and Hygiene (WASH) dataset – 2025 update

The JMP provides harmonized national estimates for service levels including:

Safely managed

At least basic

Limited

Unimproved

Surface water

Variables Included

The raw dataset contains:

country

iso3

year

sdg_region

income_groupings

population_thousands

Service-level indicators for rural, urban, and total populations

The project focuses on drinking water (Water sheet).


Data Cleaning and Processing

Key steps (all performed in R):

Import raw Excel data

Extract and reconstruct multi-row header information

Clean variable names using janitor::clean_names()

Convert wide-format service-level columns to long tidy format

Recode variable labels to short and long definitions (wat_bas, wat_safe, etc.)

Restrict dataset to CPLP countries

Convert percent values to numeric

Save processed data into /data/processed/ as CSV

The final dataset contains:

| country | iso3 | year | residence | varname_short | varname_long | percent |

Methods

This project uses:

R (tidyverse) for data wrangling and visualization

ggplot2 for time-series and inequality plots

dplyr for filtering, grouping, and summarizing

tidyr for reshaping data

here for reproducible paths

readxl for Excel import

janitor for cleaning variable names

Analysis outputs include time-series plots, rural–urban comparisons, and summary tables.

Repository Structure
📦 project-folder
├── data
│   ├── raw
│   │   └── JMP_WASH_HH_2025_by_country-2.xlsx
│   └── processed
│       └── water_data_long.csv
├── scripts
│   ├── 01_data_cleaning.R
│   ├── 02_analysis.R
│   └── 03_figures.R
├── quarto
│   └── report.qmd
├── README.md
└── .gitignore
