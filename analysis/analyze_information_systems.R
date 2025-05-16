# set-up ------------------------------------------------------------------
library(ggplot2)
library(dplyr)
library(tidyr)
library(here)
library(hrbrthemes)

theme_set(
  theme_ipsum_rc(
    grid = T,  base_size = 16,
    axis_title_size = 14, caption_size = 12
  )
)

# analysis ----------------------------------------------------------------
dgss %>%
  group_by(region) %>%
  summarise(
    across(
      c(ends_with("_sta")),
      ~ mean(. == 3, na.rm = TRUE)
    )
  ) %>%
  pivot_longer(
    cols = c(e_prc_sta, hrm_sta, payr_sta, pims_sta, tmis_sta, dms_sta)
  ) %>%
  mutate(
    # region = factor(
    #   region,
    #   levels = levels(region) |> rev()
    # ),
    name = recode(
      name,
      "e_prc_sta" = "Procurement",
      "hrm_sta" = "Human Resource Management",
      "payr_sta" = "Payroll",
      "pims_sta" = "Public Investment",
      "tmis_sta" = "Taxes",
      "dms_sta" = "Debt Management"
    )
  ) %>%
  filter(!is.na(region) & region != "North America") %>%
  mutate(
    name = factor(
      name,
      levels = c("Taxes", "Debt Management", "Payroll", "Human Resource Management", "Procurement", "Public Investment")
    )
  ) %>%
  ggplot() +
  geom_col(
    aes(
      value,
      region,
      fill = region
    )
  ) +
  scale_fill_brewer(palette = "Paired") +
  facet_wrap(vars(name), nrow = 5) +
  scale_x_continuous(labels = scales::percent_format()) +
  labs(
    x = "Percentage of Countries with Operational Information Systems",
    y = "Sector/Region"
  ) +
  theme(
    text = element_text(size = 20),
    strip.text.x = element_text(size = 18),
    legend.position = "none"
  )

ggsave(
  here("figures", "17-coverage_information_systems.png"),
  height = 10,
  width = 16
)
