## code to prepare `04-appendix-indicator-clean` dataset goes here

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
  here("data-raw", "input", "cliar", "db_variables.xlsx")) |>
  mutate(family_name = str_to_lower(family_name),
         var_name = str_to_lower(var_name))

lending_class <- read_csv(
  here("data-raw", "input", "wb", "client_countries.csv")) |>
  select(country_code, lending_category)  # Keep only relevant columns

# Subset
# Perform a left join to merge lending_class into ctf_static
ctf_dyn_clients <- ctf_dyn |>
  left_join(lending_class, by = "country_code") |>  # Join based on country_code
  filter(country_code %in% lending_class$country_code) |>
  relocate(income_group, lending_category, .after = country_code)


clusters <- c(
  "public human resource management institutions",
  "degree of integrity",
  "energy and environment institutions",
  "political institutions",
  "social institutions",
  "justice institutions"
)

#Pivot long
ctf_dyn_long <- ctf_dyn_clients |>
  pivot_longer(
    cols      = 8:last_col(),
    names_to  = "variable",
    values_to = "value"
  ) |>
  left_join(db_variables, by = "variable") |>
  select(
    country_name, country_code, income_group,
    lending_category, region, year,
    variable, value, var_name, family_name
  ) |>
  # filter on membership rather than ==:
  filter(family_name %in% clusters)



indicators_dyn <- ctf_dyn_long |>
  filter(year %in% c(2016, 2018, 2020, 2022),
         !is.na(value),
         !is.na(var_name)
  )


indicator_cluster_distance <- bind_rows(
                      indicators_dyn |>
                        filter(family_name != "energy and environment institutions") |>
                        compute_indicator_diff(2018, 2022) |>
                        filter(!is.na(ctf_distance)),
                      indicators_dyn |>
                        filter(family_name == "energy and environment institutions") |>
                        compute_indicator_diff(2016, 2020) |>
                        filter(!is.na(ctf_distance))
                      ) |>
                      select(-starts_with("value_"))  # drop all cols that start with "value_"

# inspect
glimpse(indicator_cluster_distance)



usethis::use_data(indicator_cluster_distance, overwrite = TRUE)
