
# set-up ------------------------------------------------------------------
library(haven)
library(dplyr)
library(stringi)
library(here)
library(tidyverse)
library(ggplot2)
library(janitor)
library(reshape2)
library(scales)
library(stringr)
library(readr)
library(readxl)
library(labelled)
library(cowplot)
library(openxlsx)
library(RColorBrewer)
library(countrycode)
library(extrafont)
library(devtools)
library(ggrepel)
library(forcats)
library(patchwork)

devtools::load_all()

theme_set(
  theme_minimal(base_family = "Segoe UI Semilight") +
    theme(
      axis.text.x = element_text(size = 10, hjust = .5),
      axis.text.y = element_text(size = 9),
      plot.title = element_text(size = 16, face = "bold"),
      plot.subtitle = element_text(size = 12),
      plot.background = element_blank(),
      plot.caption = element_text(hjust = 0, size = 10),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      legend.position = "top"
    )
)

## Absence of MENA and SA, in order to maintain colors
custom_colors <- c(
  "East Asia & Pacific" = "#a6cee3",
  "Europe & Central Asia" = "#1f78b4",
  "Latin America & Caribbean" = "#b2df8a",
  "Middle East & North Africa" = "#33a02c",
  "South Asia" = "#fb9a99",
  "Sub-Saharan Africa" = "#e31a1c"
)

ggsave_facet<- partial(
  ggplot2::ggsave,
  bg = "white",
  width = 20,
  height = 16
)

options(ggrepel.max.overlaps = Inf) ### For wrapping country codes



# data-load ---------------------------------------------------------------

year_subset <- dynamic_ctf_clusters |>
                filter(
                  year %in% c(2016, 2018, 2020, 2022),
                  !is.na(value)
                  )


# clusters-visualizations -------------------------------------------------

# FIGURE 9. Political cluster
political_diff <- compute_cluster_diff(year_subset,
                                       "Political Institutions",
                                       2018,
                                       2022) |>
  distinct(country_name,
           ctf_distance,
           lending_category,
           region,.keep_all = FALSE) |>
  filter(!is.na(ctf_distance))

# Plot
political_diff |>
  mutate(country_name = reorder(country_name, ctf_distance)) |>
  generate_diff_plot()

ggsave_facet(
  here("figures","09-political-cluster-change.png")
)

# FIGURE 10. Social cluster
social_diff <- compute_cluster_diff(year_subset,
                                    "Social Institutions",
                                    2018,
                                    2022) |>
  distinct(country_name,
           ctf_distance,
           lending_category,
           region,.keep_all = FALSE) |>
  filter(!is.na(ctf_distance))

# Plot
social_diff |>
  mutate(country_name = reorder(country_name, ctf_distance)) |>
  generate_diff_plot()

ggsave_facet(
  here("figures","10-social-cluster-change.png")
)


# FIGURE 11. HRMS cluster
hrm_diff <- compute_cluster_diff(year_subset,
                                 "Public HRM Institutions",
                                 2018,
                                 2022) |>
            distinct(country_name, # Ensure there are no duplicates
                     ctf_distance,
                     lending_category,
                     region,.keep_all = FALSE) |>
            filter(!is.na(ctf_distance))
# Plot
hrm_diff |>
  mutate(country_name = reorder(country_name, ctf_distance)) |>
  generate_diff_plot()

ggsave_facet(
  here("figures","11-hrm-cluster-change.png")
)

# FIGURE 12. Integrity cluster
integrity_diff <- compute_cluster_diff(year_subset,
                                       "Degree of Integrity",
                                       2018,
                                       2022) |>
                  distinct(country_name,
                           ctf_distance,
                           lending_category,
                           region,.keep_all = FALSE) |>
                  filter(!is.na(ctf_distance))

# Plot
integrity_diff |>
  mutate(country_name = reorder(country_name, ctf_distance)) |>
  generate_diff_plot()

ggsave_facet(
  here("figures","12-integrity-cluster-change.png")
)


# FIGURE 23. ANNEX Justice cluster
justice_diff <- compute_cluster_diff(year_subset,
                                    "Justice Institutions",
                                    2018,
                                    2022) |>
  distinct(country_name,
           ctf_distance,
           lending_category,
           region,.keep_all = FALSE) |>
  filter(!is.na(ctf_distance))

# Plot
justice_diff |>
  mutate(country_name = reorder(country_name, ctf_distance)) |>
  generate_diff_plot()

ggsave_facet(
  here("figures","23-annex-a-justice-cluster-change.png")
)

# FIGURE 23. ANNEX Enviroment cluster
env_diff <- compute_cluster_diff(year_subset,
                                     "Energy and Enviroment Institutions",
                                     2016,
                                     2020) |> # Change Time frame
            distinct(country_name,
                     ctf_distance,
                     lending_category,
                     region,.keep_all = FALSE) |>
            filter(!is.na(ctf_distance))

# Plot
env_diff |>
  mutate(country_name = reorder(country_name, ctf_distance)) |>
  generate_diff_plot()

ggsave_facet(
  here("figures","24-annex-b-enviromental-cluster-change.png")
)


### code-end
