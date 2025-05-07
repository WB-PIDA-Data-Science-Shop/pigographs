# 1. set-up ---------------------------------------------------------------
library(dplyr)
library(readr)
library(ggplot2)
library(stringr)
library(tidyr)
library(janitor)
library(cowplot)
library(here)

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

# 2. read-in data ------------------------------------------------------------
gsps <- read_csv(
  here("data-raw", "input", "wb", "gsps.csv")
) |>
  clean_names()

gsps_management <- gsps |>
  filter(
    str_detect(section_org, "(?i)Management")
  )

# 3. analysis -------------------------------------------------------------
# target setting
gsps_management |>
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
