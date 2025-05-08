## code to prepare `gsps-management` dataset goes here

library(dplyr)
library(readr)
library(stringr)
library(tidyr)
library(janitor)
library(here)

# 2. read-in data ------------------------------------------------------------
gsps <- read_csv(
  here("data-raw", "input", "wb", "gsps.csv")
) |>
  clean_names()

gsps_management <- gsps |>
  filter(
    str_detect(section_org, "(?i)Management")
  )


# Export dataframe
usethis::use_data(gsps_management, overwrite = TRUE)

