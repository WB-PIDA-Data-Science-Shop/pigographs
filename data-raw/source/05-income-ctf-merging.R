## code to prepare `income_ctf` dataset goes here


# Set-up

# Load necessary libraries
library(dplyr)
library(ggplot2)
library(tidyr)
library(patchwork)
library(here)
library(stringr)
library(scales)
library(haven)
library(readxl)
library(grid)    # for textGrob

# Data preparation
# Set up directories based on your project root
input_dir  <- here("data-raw", "input", "correlation")
output_dir <- here("figures")

# Load data files
ctf_country        <- read_dta(file.path(input_dir, "static_ctf_012225.dta"))
cliar_db_variables <- read_xlsx(here("data-raw", "input", "cliar", "db_variables.xlsx"))

# Identify CLIAR cluster variables
cliar_cluster_avg <- ctf_country |>
                      select(ends_with("_avg")) |>
                      colnames()

# Read and clean WDI data
wdi <- read_excel(
  file.path(input_dir,
            "P_Data_Extract_From_World_Development_Indicators_updated.xlsx"),
  sheet = "Data",
  na = ".."
) %>%
  filter(!is.na(`Country Code`)) %>%
  filter(`Series Name` %in% c(
    "Poverty headcount ratio at $2.15 a day (2017 PPP) (% of population)",
    "GDP per capita, PPP (constant 2021 international $)",
    "Unemployment, total (% of total labor force) (modeled ILO estimate)"
  )) %>%
  mutate(
    `2019_2023` = coalesce(
      `2023 [YR2023]`, `2022 [YR2022]`, `2021 [YR2021]`,
      `2020 [YR2020]`, `2019 [YR2019]`
    )
  ) %>%
  select(-matches("\\[YR\\d{4}\\]"), -`Series Code`) %>%
  pivot_wider(
    names_from  = `Series Name`,
    values_from = `2019_2023`
  )


# Standardize and filter CLIAR country data
ctf_country <- ctf_country |>
  mutate(region = str_to_title(trimws(as.character(region)))) |>
  filter(
    region != "North America",
    region != ""
    # income_group != "High income"
  )

# Read allowed countries (optional)
allowed_countries <- read_excel(file.path(input_dir, "client_countries.xlsx"))

# Define WDI variables of interest
wdi_vars <- c(
  "Poverty headcount ratio at $2.15 a day (2017 PPP) (% of population)",
  "GDP per capita, PPP (constant 2021 international $)"
)

# Convert to numeric
wdi <- wdi |>
  mutate(across(all_of(wdi_vars), ~ as.numeric(as.character(.))))

# CLIAR vars and labels
ctf_vars   <- c(
  "vars_hrm_avg", "vars_digital_avg", "vars_anticorruption_avg",
  "vars_transp_avg", "vars_leg_avg", "vars_mkt_avg"
)
ctf_labels <- c(
  vars_hrm_avg            = "Public Human Resource Management",
  vars_digital_avg        = "Digital and Data",
  vars_anticorruption_avg = "Degree of Integrity",
  vars_transp_avg         = "Transparency and Accountability",
  vars_leg_avg            = "Justice",
  vars_mkt_avg            = "Business Environment"
)

# Merge CLIAR + WDI
income_ctf <- ctf_country |>
  select(country_code, all_of(ctf_vars), region) |>
  left_join(wdi, by = c("country_code" = "Country Code")) |>
  rename_with(~ c(ctf_labels)[.], all_of(ctf_vars))

# Export dataframe
usethis::use_data(income_ctf, overwrite = TRUE)
