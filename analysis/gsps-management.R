
# Load libraries

library(dplyr)
library(readr)
library(ggplot2)
library(stringr)
library(tidyr)
library(janitor)
library(cowplot)
library(here)

devtools::load_all()

theme_set(
  theme_cowplot(
    font_size = 20
  )
)

options(
  ggplot2.discrete.colour= c(
    "#00ADE4", "#002345",
    "#ff8000", "#fcdf2d"
  )
)

# read-in data ------------------------------------------------------------


management_data <- gsps_management



# analysis -------------------------------------------------------------
# target setting
management_data |>
  filter(
    indicator == "Clear targets" &
      group == "Institution"
  ) |>
  ggplot(
    aes(x = mean, y = country, fill = country)
  ) +
  geom_boxplot(
    width = 0.25,
    outlier.shape = NA
  ) +
  geom_jitter(
    size = 1,
    alpha = 0.5,
    height = 0.25
  ) +
  scale_x_continuous(
    labels = scales::percent_format()
  ) +
  scale_fill_manual(
    name = "Country",
    values = c("#00ADE4", "#ff8000")
  ) +
  theme(
    legend.position = "none"
  ) +
  labs(
    x = "Share of Respondents",
    y = "Country"
  )

ggsave(
  here("figures", "19_mgmt_target.png"),
  bg = "white",
  height = 8,
  width = 12
)
