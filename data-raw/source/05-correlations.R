# Set-up

# Load necessary libraries
library(dplyr)
library(ggplot2)
library(tidyr)
library(patchwork)
library(here)
library(stringr)
library(scales)
library(haven)
library(readxl)
library(grid)    # for textGrob

# Data preparation
# Set up directories based on your project root
input_dir  <- here("data-raw", "input", "correlation")
output_dir <- here("figures")

# Load data files
ctf_country        <- read_dta(file.path(input_dir, "static_ctf_012225.dta"))
cliar_db_variables <- read_xlsx(here("data-raw", "input", "cliar", "db_variables.xlsx"))

# Identify CLIAR cluster variables
cliar_cluster_avg <- ctf_country |>
                      select(ends_with("_avg")) |>
                      colnames()

# Read and clean WDI data
wdi <- read_excel(
  file.path(input_dir,
            "P_Data_Extract_From_World_Development_Indicators_updated.xlsx"),
  sheet = "Data",
  na = ".."
) %>%
  filter(!is.na(`Country Code`)) %>%
  filter(`Series Name` %in% c(
    "Poverty headcount ratio at $2.15 a day (2017 PPP) (% of population)",
    "GDP per capita, PPP (constant 2021 international $)",
    "Unemployment, total (% of total labor force) (modeled ILO estimate)"
  )) %>%
  mutate(
    `2019_2023` = coalesce(
      `2023 [YR2023]`, `2022 [YR2022]`, `2021 [YR2021]`,
      `2020 [YR2020]`, `2019 [YR2019]`
    )
  ) %>%
  select(-matches("\\[YR\\d{4}\\]"), -`Series Code`) %>%
  pivot_wider(
    names_from  = `Series Name`,
    values_from = `2019_2023`
  )


# Standardize and filter CLIAR country data
ctf_country <- ctf_country |>
  mutate(region = str_to_title(trimws(as.character(region)))) |>
  filter(
    region != "North America",
    region != ""
    # income_group != "High income"
  )

# Read allowed countries (optional)
allowed_countries <- read_excel(file.path(input_dir, "client_countries.xlsx"))

# Define WDI variables of interest
wdi_vars <- c(
  "Poverty headcount ratio at $2.15 a day (2017 PPP) (% of population)",
  "GDP per capita, PPP (constant 2021 international $)"
)

# Convert to numeric
wdi <- wdi |>
  mutate(across(all_of(wdi_vars), ~ as.numeric(as.character(.))))

# CLIAR vars and labels
ctf_vars   <- c(
  "vars_hrm_avg", "vars_digital_avg", "vars_anticorruption_avg",
  "vars_transp_avg", "vars_leg_avg", "vars_mkt_avg"
)
ctf_labels <- c(
  vars_hrm_avg            = "Public Human Resource Management",
  vars_digital_avg        = "Digital and Data",
  vars_anticorruption_avg = "Degree of Integrity",
  vars_transp_avg         = "Transparency and Accountability",
  vars_leg_avg            = "Justice",
  vars_mkt_avg            = "Business Environment"
)

# Merge CLIAR + WDI
merged_data <- ctf_country |>
  select(country_code, all_of(ctf_vars), region) |>
  left_join(wdi, by = c("country_code" = "Country Code")) |>
  rename_with(~ c(ctf_labels)[.], all_of(ctf_vars))

# Load Country Risk Index and merge
risk_data <- read_excel(
  file.path(input_dir, "country_risk_index.xlsx"), sheet = "Table Data"
) |>
  select(`Country Name`, `Country Risk Index`) |>
  rename(country_risk = `Country Risk Index`) |>
  mutate(`Country Name` = trimws(`Country Name`))

merged_data_risk <- merged_data |>
  left_join(risk_data, by = c("Country Name" = "Country Name"))



# ----------------------------------------------------------------------------
# Analysis A: WDI vs. CLIAR (CTF) with R² annotation
# ----------------------------------------------------------------------------
for (wdi_var in wdi_vars) {
  # Determine shared Y title
  overall_y_title <- if (
    wdi_var == "GDP per capita, PPP (constant 2021 international $)"
  ) {
    "log(GDP per capita, PPP (constant 2021 international $)) (circa 2022)"
  } else {
    paste(wdi_var, "(circa 2022)")
  }

  plot_list    <- list()
  country_lists <- list()

  for (ctf_key in names(ctf_labels)) {
    ctf_name  <- ctf_labels[[ctf_key]]
    plot_data <- merged_data %>% drop_na(all_of(c(ctf_name, wdi_var)))

    country_lists[[ctf_name]] <- sort(unique(plot_data$country_code))

    # Compute y_val
    plot_data <- plot_data %>% mutate(
      y_val = if (
        wdi_var == "GDP per capita, PPP (constant 2021 international $)"
      ) log(.data[[wdi_var]]) else .data[[wdi_var]]
    )

    # Fit quadratic model & extract R² via dynamic formula
    formula_str <- sprintf("y_val ~ poly(`%s`, 2)", ctf_name)
    fit <- lm(as.formula(formula_str), data = plot_data)
    r2  <- summary(fit)$r.squared
    label_r2 <- sprintf("R² = %.3f", r2)

    # Position for annotation
    x_pos <- min(plot_data[[ctf_name]], na.rm = TRUE)
    y_pos <- max(plot_data$y_val, na.rm = TRUE)

    # Build plot
    p <- ggplot(plot_data,
                aes(x = .data[[ctf_name]], y = y_val, color = region)) +
      geom_point(size = 2) +
      geom_smooth(
        method  = "lm",
        formula = y ~ poly(x, 2),
        se      = FALSE,
        color   = "black",
        linetype= "dashed"
      ) +
      annotate(
        "text", x = x_pos, y = y_pos, label = label_r2,
        hjust = 0, vjust = 1, size = 4
      ) +
      labs(x = paste(ctf_name, "(2019-2023)"), color = "Region") +
      scale_color_brewer(palette = "Paired") +
      theme_minimal() +
      theme(
        axis.title.y    = element_blank(),
        axis.text.x     = element_text(size = 14),
        axis.text.y     = element_text(size = 14),
        axis.title      = element_text(size = 16),
        plot.margin     = margin(10,10,15,10),
        legend.position = "bottom",
        legend.title    = element_text(face = "bold", size = 16),
        legend.text     = element_text(size = 14),
        panel.border    = element_rect(color = "black", fill = NA, size = 1)
      )

    plot_list[[ctf_name]] <- p
  }

  # Remove y-axis on even columns
  plot_list_shared <- lapply(seq_along(plot_list), function(i) {
    if (i %% 2 == 0) plot_list[[i]] +
      theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
    else plot_list[[i]]
  })

  # Combine into 3x2 panel
  combined_panel <- (plot_list_shared[[1]] | plot_list_shared[[2]]) /
    (plot_list_shared[[3]] | plot_list_shared[[4]]) /
    (plot_list_shared[[5]] | plot_list_shared[[6]]) +
    plot_layout(guides = "collect") &
    theme(
      legend.position = "bottom",
      legend.box      = "horizontal",
      legend.title    = element_text(face = "bold", size = 16),
      legend.text     = element_text(size = 16)
    )

  shared_y <- textGrob(overall_y_title, rot = 90,
                       gp = gpar(fontsize = 16, fontface = "bold"))
  final_plot <- wrap_elements(shared_y) + combined_panel +
    plot_layout(widths = c(0.1,1))

  # Save outputs
  safe_name <- gsub("[^A-Za-z0-9]", "", wdi_var)
  ggsave(
    file.path(output_dir, paste0("Repo_Correlation_", safe_name, ".png")),
    final_plot, width = 12, height = 12, dpi = 300, bg = "white"
  )
}



# ----------------------------------------------------------------------------
# Analysis B: Country Risk vs. CLIAR with R² annotation
# ----------------------------------------------------------------------------
ctf_clusters <- unname(ctf_labels)
plot_list    <- list()
country_lists <- list()

for (ctf_name in ctf_clusters) {
  plot_data <- merged_data_risk %>%
    drop_na(all_of(c("country_risk", ctf_name)))
  country_lists[[ctf_name]] <- sort(unique(plot_data$country_code))

  # Fit & R²
  formula_str <- sprintf("country_risk ~ poly(`%s`, 2)", ctf_name)
  fit <- lm(as.formula(formula_str), data = plot_data)
  r2  <- summary(fit)$r.squared
  label_r2 <- sprintf("R² = %.3f", r2)

  x_pos <- min(plot_data[[ctf_name]], na.rm = TRUE)
  y_pos <- max(plot_data$country_risk, na.rm = TRUE)

  p <- ggplot(plot_data,
              aes(x = .data[[ctf_name]], y = country_risk, color = region)) +
    geom_point(size = 2) +
    geom_smooth(
      method  = "lm",
      formula = y ~ poly(x, 2),
      se      = FALSE,
      color   = "black",
      linetype= "dashed"
    ) +
    annotate(
      "text", x = x_pos, y = y_pos, label = label_r2,
      hjust = 0, vjust = 1, size = 4
    ) +
    labs(x = paste(ctf_name, "(2019-2023)"), color = "Region") +
    scale_color_brewer(palette = "Paired") +
    scale_x_continuous(limits = c(0,1), breaks = seq(0,1,0.2)) +
    scale_y_continuous(breaks = scales::pretty_breaks(5), limits = c(20,NA)) +
    theme_minimal() +
    theme(
      axis.title.y    = element_blank(),
      axis.text.x     = element_text(size = 14),
      axis.text.y     = element_text(size = 14),
      axis.title      = element_text(size = 16),
      plot.margin     = margin(10,10,15,10),
      legend.position = "bottom",
      legend.title    = element_text(face = "bold", size = 16),
      legend.text     = element_text(size = 14),
      panel.border    = element_rect(color = "black", fill = NA, size = 1)
    )

  plot_list[[ctf_name]] <- p
}

# Remove y-axis on even columns
plot_list_shared <- lapply(seq_along(plot_list), function(i) {
  if (i %% 2 == 0) plot_list[[i]] +
    theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
  else plot_list[[i]]
})

combined_panel <- (plot_list_shared[[1]] | plot_list_shared[[2]]) /
  (plot_list_shared[[3]] | plot_list_shared[[4]]) /
  (plot_list_shared[[5]] | plot_list_shared[[6]]) +
  plot_layout(guides = "collect") &
  theme(
    legend.position = "bottom",
    legend.box      = "horizontal",
    legend.title    = element_text(face = "bold", size = 16),
    legend.text     = element_text(size = 16)
  )
shared_y <- textGrob("Country Risk Index (2025)", rot = 90,
                     gp = gpar(fontsize = 16, fontface = "bold"))
final_plot <- wrap_elements(shared_y) + combined_panel +
  plot_layout(widths = c(0.1,1))

ggsave(
  file.path(output_dir, "CountryRisk_vs_CTFClusters_repo.png"),
  final_plot, width = 12, height = 12, dpi = 300, bg = "white"
)


