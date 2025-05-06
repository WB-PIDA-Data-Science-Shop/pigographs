

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

options(ggrepel.max.overlaps = Inf)

ggsave_bud <- partial(
  ggplot2::ggsave,
  bg = "white",
  width = 12,
  height = 6
)

# data-load ---------------------------------------------------------------

regional_budget_execution <- budget_execution


# analysis----------------------------------------------------------------

# 1. Education, capital expenditures
regional_budget_execution |>
  plot_budget_execution(
    exyear     = 2019,
    govsector  = "Education",
    budgetline = "Spending: Capital expenditures in education"
  ) +
  theme_minimal() +
  coord_flip() +
  theme(
    text = element_text(family = "Segoe UI Semibold"),
    axis.text.x = element_text(size = 12, hjust = 0.5),
    axis.text.y = element_text(size = 12, hjust = 0.5),
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 14),
    plot.background = element_blank(),
    plot.caption = element_text(hjust = 0, size = 10),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    legend.position = "none"
  ) +
  scale_color_brewer(palette = "Paired") +
  ylim(0, 200)

ggsave_bud(
  here("figures","15-service-delivery-education-budget-execution.png")
)

# 2. Health, capital expenditures
regional_budget_execution |>
  plot_budget_execution(
    exyear     = 2019,
    govsector  = "Health",
    budgetline = "Spending: Capital expenditures in health"
  ) +
  theme_minimal() +
  coord_flip() +
  theme(
    text = element_text(family = "Segoe UI Semibold"),
    axis.text.x = element_text(size = 12, hjust = 0.5),
    axis.text.y = element_text(size = 12, hjust = 0.5),
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 14),
    plot.background = element_blank(),
    plot.caption = element_text(hjust = 0, size = 10),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    legend.position = "none"
  ) +
  scale_color_brewer(palette = "Paired") +
  ylim(0, 200)

ggsave_bud(
  here("figures","15-service-delivery-health-budget-execution.png")
)


### code-end
