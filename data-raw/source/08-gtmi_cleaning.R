## code to prepare `08-gtmi_cleaning` dataset goes here
library(readr)
library(dplyr)
library(countrycode)
library(here)

# URL for original dataset:
# https://datacatalogfiles.worldbank.org/ddh-published/0037889/DR0065450/WBG_GovTech%20Dataset_Dec2020.xlsx
# access date: 5/16/2025

dgss <- read_csv(
  here("data-raw", "input", "wb", "dgss.csv"),
  na = c("", "-", "NA")
)  |>
  janitor::clean_names()

# fix regions
dgss <- dgss |>
  mutate(
    # fix iso3c
    code_1 = case_when(
      code_1 == "ROM" ~ "ROU",
      code_1 == "KSV" ~ "XKX",
      code_1 == "ADO" ~ "AND",
      code_1 == "TMP" ~ "TLS",
      code_1 == "ZAR" ~ "COD",
      code_1 == "WBG" ~ "PSE",
      T ~ code_1
    ),
    region = countrycode(code_1, origin = "wb", destination = "region")
  )

dgss <- dgss %>%
  select(
    code = code_1,
    economy,
    level,
    population,
    gni,
    gnipc,
    region,
    ends_with("_sta"),
    ends_with("_yr")
  ) %>%
  select(
    code,
    economy,
    level,
    population,
    gni,
    gnipc,
    region,
    matches("tmis|cust|hrm|payr|e_prc|pims|dms")
  )

usethis::use_data(dgss, overwrite = TRUE)
