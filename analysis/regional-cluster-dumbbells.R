
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
library(purrr)

devtools::load_all()

theme_set(
  theme_minimal() +
    theme(
      text = element_text(family = "Segoe UI Semibold"),
      axis.text.x = element_text(size = 14, hjust = .5),
      axis.text.y = element_text(size = 14),
      plot.title = element_text(size = 22, face = "bold"),
      plot.subtitle = element_text(size = 16),
      plot.background = element_blank(),
      plot.caption = element_text(hjust = 0, size = 12),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      legend.position = "none"
    )
)


ggsave_db <- partial(
  ggplot2::ggsave,
  bg = "white",
  width = 14,
  height = 9
)

options(ggrepel.max.overlaps = Inf)



# data-load ---------------------------------------------------------------

ctf_static_wide <- read_csv(
                  here("data-raw", "output", "ctf_static_wide.csv.gz"))


# regional-visualizations -------------------------------------------------


# FIGURE 5. POLITICAL
pol_data <- ctf_static_wide |>
  compute_regional_statistics("vars_pol_avg")

# Plot
pol_data |>
  generate_regional_minmax_plot("Political Institutions") +
  ggtitle(
    "Political Institutions",
    subtitle = "Regional Distributions and Average Trend"
  ) +
  labs(
    x = "Region",
    y = "CTF Average Score",
    shape = "Values",
    color = "Region",
    hjust = 0
  ) +
  coord_cartesian(ylim = c(0, 1)) +
  scale_color_brewer(palette = "Paired")

ggsave_db(
  here("figures","05-poli-arena-hrm-regional-dumbbells.png")
)


# FIGURE 5. SOCIAL
social_data <- ctf_static_wide |>
  compute_regional_statistics("vars_social_avg")

# Plot
social_data |>
  generate_regional_minmax_plot("Social Institutions") +
  ggtitle(
    "Social Institutions",
    subtitle = "Regional Distributions and Average Trend"
  ) +
  labs(
    x = "Region",
    y = "CTF Average Score",
    shape = "Values",
    color = "Region",
    hjust = 0
  ) +
  coord_cartesian(ylim = c(0, 1)) +
  scale_color_brewer(palette = "Paired")

ggsave_db(
  here("figures","05-social-arena-hrm-regional-dumbbells.png")
)



# FIGURE 6. HRM
hrm_data <- ctf_static_wide |>
  compute_regional_statistics("vars_hrm_avg")

# Plot
hrm_data |>
  generate_regional_minmax_plot("Personnel") +
  ggtitle(
    "Public HRM Institutions",
    subtitle = "Regional Distributions and Average Trend"
  ) +
  labs(
    x = "Region",
    y = "CTF Average Score",
    shape = "Values",
    color = "Region",
    hjust = 0
  ) +
  coord_cartesian(ylim = c(0, 1)) +
  scale_color_brewer(palette = "Paired")

ggsave_db(
  here("figures","06-center-of-gov-hrm-regional-dumbbells.png")
)

# FIGURE 6. DIGITAL
digital_data <- ctf_static_wide |>
  compute_regional_statistics("vars_digital_avg")

# Plot
digital_data |>
  generate_regional_minmax_plot("Digital") +
  ggtitle(
    "Digital and Data Institutions",
    subtitle = "Regional Distributions and Average Trend"
  ) +
  labs(
    x = "Region",
    y = "CTF Average Score",
    shape = "Values",
    color = "Region",
    hjust = 0
  ) +
  coord_cartesian(ylim = c(0, 1)) +
  scale_color_brewer(palette = "Paired")

ggsave_db(
  here("figures","06-center-of-gov-digital-regional-dumbbells.png")
)


# FIGURE 7. INTEGRITY
integrity_data <- ctf_static_wide |>
  compute_regional_statistics("vars_anticorruption_avg")

# Plot
integrity_data |>
  generate_regional_minmax_plot("Accountability") +
  ggtitle(
    "Degree of Integrity",
    subtitle = "Regional Distributions and Average Trend"
  ) +
  labs(
    x = "Region",
    y = "CTF Average Score",
    shape = "Values",
    color = "Region",
    hjust = 0
  ) +
  coord_cartesian(ylim = c(0, 1)) +
  scale_color_brewer(palette = "Paired")

ggsave_db(
  here("figures","07-integrity-regional-dumbbells.png")
)


# FIGURE 7. TRANSPARENCY
transp_data <- ctf_static_wide |>
  compute_regional_statistics("vars_transp_avg")

# Plot
transp_data |>
  generate_regional_minmax_plot("Transparency") +
  ggtitle(
    "Transparency & Accountability Institutions",
    subtitle = "Regional Distributions and Average Trend"
  ) +
  labs(
    x = "Region",
    y = "CTF Average Score",
    shape = "Values",
    color = "Region",
    hjust = 0
  ) +
  coord_cartesian(ylim = c(0, 1)) +
  scale_color_brewer(palette = "Paired")

ggsave_db(
  here("figures","07-transp-regional-dumbbells.png")
)


# FIGURE 8. JUSTICE
justice_data <- ctf_static_wide |>
  compute_regional_statistics("vars_leg_avg")

# Plot
justice_data |>
  generate_regional_minmax_plot("Justice") +
  ggtitle(
    "Justice Institutions",
    subtitle = "Regional Distributions and Average Trend"
  ) +
  labs(
    x = "Region",
    y = "CTF Average Score",
    shape = "Values",
    color = "Region",
    hjust = 0
  ) +
  coord_cartesian(ylim = c(0, 1)) +
  scale_color_brewer(palette = "Paired")

ggsave_db(
  here("figures","08-justice-regional-dumbbells.png")
)



# FIGURE 8. ENVIROMENT
climate_data <- ctf_static_wide |>
  compute_regional_statistics("vars_climate_avg")

# Plot
climate_data |>
  generate_regional_minmax_plot("Climate") +
  ggtitle(
    "Energy and Enviroment Institutions",
    subtitle = "Regional Distributions and Average Trend"
  ) +
  labs(
    x = "Region",
    y = "CTF Average Score",
    shape = "Values",
    color = "Region",
    hjust = 0
  ) +
  coord_cartesian(ylim = c(0, 1)) +
  scale_color_brewer(palette = "Paired")

ggsave_db(
  here("figures","08-enviroment-regional-dumbbells.png")
)


### code-end


