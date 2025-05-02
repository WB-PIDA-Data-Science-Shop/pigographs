## code to prepare `01-static_ctf_clusters` dataset goes here

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

db_variables <- read_xlsx(
                here("data-raw", "input", "cliar", "db_variables.xlsx"))

lending_class <- read_csv(
                  here("data-raw", "input", "wb", "client_countries.csv")) |>
                  select(country_code, lending_category)  # Keep only relevant columns

# Subsetting
# Perform a left join to merge lending_class into ctf_static
ctf_static_wide <- ctf_static %>%
  left_join(lending_class, by = "country_code") %>%  # Join based on country_code
  filter(country_code %in% lending_class$country_code) %>%   # Subset only matched countries
  relocate(income_group, lending_category, .after = country_code)

write_csv(
  ctf_static_wide,
  here(
    "data-raw",
    "output",
    "ctf_static_wide.csv.gz"
  )
)

# Pivot longer and merge with db_variables
static_ctf_clusters <- ctf_static_wide |>
  pivot_longer(
    cols = 7:last_col(),
    names_to = "variable",
    values_to = "value"
  ) |>
  left_join(
    db_variables |>
      group_by(variable)
  ) |>
  select(1:9, -country_group)


# Export dataframe
usethis::use_data(static_ctf_clusters, overwrite = TRUE)

