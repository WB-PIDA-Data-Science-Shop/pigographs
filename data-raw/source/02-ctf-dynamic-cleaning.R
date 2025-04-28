## code to prepare `01-dynamic_ctf_clusters` dataset goes here

library(here)
library(haven)
library(readxl)
library(countrycode)
library(tidyverse)


# Load data
ctf_static <- read_dta(
              here("data-raw", "input", "cliar", "static_ctf_032525.dta")) |>
              filter(country_group == 0) |>
              filter(region != "North America")  # Exclude North America

ctf_dyn <- read_dta(
            here("data-raw", "input", "cliar", "dynamic_ctf_032525.dta")) |>
            filter(country_group == 0) |>
            filter(region != "North America")  # Exclude North America

db_variables <- read_xlsx(
                here("data-raw", "input", "cliar", "db_variables.xlsx"))

lending_class <- read_csv(
                  here("data-raw", "input", "wb", "client_countries.csv")) |>
                  select(country_code, lending_category)  # Keep only relevant columns


# Subset
# Perform a left join to merge lending_class into ctf_static
ctf_dyn_clients <- ctf_dyn |>
  left_join(lending_class, by = "country_code") |>  # Join based on country_code
  filter(country_code %in% lending_class$country_code) |>
  relocate(income_group, lending_category, .after = country_code)

# Objective: obtain cluster avgs between 2018 and 2022 by region

# Perform the left join
ctf_dyn_joined <- ctf_dyn_clients |>
  pivot_longer(cols = 7:last_col(), # Pivot all indicator columns
               names_to = "variable",
               values_to = "value") |>
  left_join(db_variables, by = "variable") |> # Join with variable metadata
  select(everything(), var_name, family_name) # Reorder columns for readability


# Avg data preparation
dynamic_ctf_clusters <- ctf_dyn_clients |>
  select(1:8, ends_with("_avg")
  ) |>
  rename(
    `Public HRM Institutions` = vars_hrm_avg,
    `Degree of Integrity` = vars_anticorruption_avg,
    `Energy and Enviroment Institutions` = vars_climate_avg,
    `Justice Institutions` = vars_leg_avg, ### Not relevant
    `Political Institutions` = vars_pol_avg,
    `Social Institutions` = vars_social_avg,
  ) |>
  pivot_longer(cols = 8:last_col(),
               names_to = "cluster",
               values_to = "value"
  ) |>
  select(-country_group)

# Export dataframe
usethis::use_data(dynamic_ctf_clusters, overwrite = TRUE)
