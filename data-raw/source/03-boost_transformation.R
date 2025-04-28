## code to prepare `budget_execution` dataset goes here

library(haven)
library(here)
library(tidyverse)
library(janitor)
library(readr)
library(readxl)
library(dplyr)



# Read data
# Load the "Approved" sheet
boost_raw_approved <- read_excel(
                      here("data-raw", "input", "boost", "BOOST_country_data.xlsx"), sheet = "Approved") |>
                      clean_names()

# Load the "Executed" sheet
boost_raw_executed <- read_excel(
                      here("data-raw", "input", "boost", "BOOST_country_data.xlsx"), sheet = "Executed") |>
                      clean_names()

boost_priorities <- read_excel(
                    here("data-raw", "input", "boost", "budget_priorities.xlsx"), sheet = "Sheet2") |>
                    clean_names()



### Approved outlook
# Country total
boost_raw_approved |> tabyl(variable) # 154 Genereal Education / Health

ap_completeness <- boost_raw_approved |>
                    tabyl(country, coverage) # Data completeness


### Approved outlook
# Country total
boost_raw_executed |> tabyl(variables) # 155 Genereal Education / Health


#1. Subset - Lines extraction

##1.1 Data transf


###...Approved

# Extract sp-in Totals for both ap-ex
# Data cleaning
boost_raw_approved <- boost_raw_approved |>
  select(-c(
    filter_by,
    code,
    avg,
    formulas,
    coverage,
    calendar,
    basis,
    source,
    column1
  ))

# Pivoting the dataframe longer
app_long <- boost_raw_approved |>
  pivot_longer(
    cols = all_of(starts_with("x")),
    names_to = "year",
    values_to = "value_country",
    values_drop_na = FALSE  # Keep NAs to asses data completeness
  ) |>
  transmute(
    country_code = alpha_code,
    country_name = country,
    region,
    income,
    year,
    budget_line = variable,
    value_country
  )


str(app_long)

# Education - Health subset - infrastructure - justice
app_bud <- app_long |>
  mutate(
    sector = case_when(
      str_detect(budget_line, "education") ~ "Education",
      str_detect(budget_line, "health") ~ "Health",
      str_detect(budget_line, "roads") ~ "Infrastructure",
      str_detect(budget_line, "judiciary") ~ "Justice",
      TRUE ~ "Other"
    )
)

app_bud |> tabyl(sector, year)

app_bud_sector <- app_bud |>
                  mutate(
                    approved_value = as.double(value_country)
                  ) |>
                  filter(
                    budget_line %in% boost_priorities$team_priorities
                  ) |>
                  group_by(
                    budget_line
                    ) |>
                  filter(
                    !is.na(approved_value)
                    )

app_balanced <- app_bud_sector |>
                  mutate(region = case_when(
                  region == "Latin America & caribbean" ~ "Latin America & Caribbean",
                  TRUE ~ region
                )) |>
                  group_by(
                    region, country_name, country_code, year,  approved_value, budget_line
                    ) |>
                filter(
                  !is.na(approved_value)
                  ) |>
                select(region, country_name, country_code, year,  approved_value, budget_line)
# overview
app_balanced |> tabyl(year, budget_line)



###...Excecuted


# Extract sp-in Totals for both ap-ex
# Data cleaning
boost_raw_executed <- boost_raw_executed |>
  select(-c(
    filter_by,
    source)
    )

# Pivoting the dataframe longer
ex_long <- boost_raw_executed |>
  pivot_longer(
    cols = all_of(starts_with("x")),
    names_to = "year",
    values_to = "value_country",
    values_drop_na = FALSE  # Keep NAs to asses data completeness
  ) |>
  transmute(
    country_name = country,
    budget_line = variables,
    year,
    value_country,
    budget_line
  )


str(ex_long)

# Sector subset
ex_bud <- ex_long |>
  mutate(
    sector = case_when(
      str_detect(budget_line, "education") ~ "Education",
      str_detect(budget_line, "education") ~ "Education",
      str_detect(budget_line, "health") ~ "Health",
      str_detect(budget_line, "roads") ~ "Infrastructure",
      str_detect(budget_line, "judiciary") ~ "Justice",
      TRUE ~ "Toatal"
    )
)


ex_balanced <- ex_bud |>
                mutate(
                    executed_value = as.double(value_country)
                  ) |>
                filter(
                  budget_line %in% boost_priorities$team_priorities
                ) |>
                group_by(
                  budget_line
                  ) |>
                filter(
                  !is.na(executed_value)
                  ) |>
                  arrange(sector, budget_line)


ex_balanced |> glimpse()


## 1.2 Data Wrangling


# Join data frames
boost_priority_bud <- left_join( ### Reverse joint because 2 values are required
  app_balanced,
  ex_balanced,
  by = c("country_name", "year", "budget_line")
)





## 1.3 Plot Completeness

# Ratio calculation
boost_execution <- boost_priority_bud  |>
  mutate(ex_ratio = (executed_value / approved_value) * 100) |>
  mutate(year = as.numeric(gsub("x", "", year)))


# Final quality test for duplicates
duplicates <- boost_execution |>
 group_by(region, country_name, country_code, year, budget_line, ex_ratio) |>
  filter(n() > 1) |>
  summarize(count = n(), .groups = 'drop') |>
  print()



# Add line type
boost_ex_add_type <- boost_execution |>
  mutate(
    sp_type = case_when(
      str_detect(budget_line, "Spending in allowances") ~ "allowances",
      str_detect(budget_line, " Spending: Capital expenditures in") ~ "capital",
      str_detect(budget_line, "Spending: Total Expenditures") ~ "total",
      str_detect(budget_line, "Spending in wages") ~ "wages",
      str_detect(budget_line, "Spending in Goods and services") ~ "goods and services",
      TRUE ~ "Other"
    )
  )




# Remove outliers
boost_ex_del_outliers <- boost_ex_add_type |>
   filter(between(ex_ratio, 0, 300))

ex_no_outliers <- boost_ex_del_outliers |> tabyl(budget_line, year)


# Last renaming
budget_execution <-  boost_ex_del_outliers |>
  mutate(
    region = recode(region,
                         "Europe & Central Asia" = "ECA",
                         "East Asia & Pacific" = "EAP",
                         "Latin America & Caribbean" = "LAC",
                         "Middle East & North Africa" = "MENA",
                         "South Asia" = "SAR",
                         "Sub-Saharan Africa" = "SSA")
  ) |>
  select(-value_country) |>
  rename(spending_type = sp_type,
         execution_ratio = ex_ratio)


# Export dataframe
usethis::use_data(budget_execution, overwrite = TRUE)





