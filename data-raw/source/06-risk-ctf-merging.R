## code to prepare `risk_ctf` dataset goes here


# Set-up

# Load necessary libraries
library(dplyr)
library(tidyr)
library(here)
library(stringr)
library(haven)



devtools::load_all()

# Lazy load WDI data

income_ctf_to_merge <- income_ctf


# Load Country Risk Index and merge
risk_data <- read_excel(
                        file.path(input_dir, "country_risk_index.xlsx"),
                        sheet = "Table Data"
                        ) |>
                          select(`Country Name`, `Country Risk Index`) |>
                          rename(country_risk = `Country Risk Index`) |>
                          mutate(`Country Name` = trimws(`Country Name`))

risk_ctf <- income_ctf_to_merge |>
                    left_join(risk_data,
                              by = c("Country Name" = "Country Name")
                              )

# Export dataframe
usethis::use_data(risk_ctf, overwrite = TRUE)

