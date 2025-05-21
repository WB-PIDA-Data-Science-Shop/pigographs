# ----------------------------------------------------------------------------
# 0. Setup & Data Loading
# ----------------------------------------------------------------------------
library(dplyr); library(ggplot2); library(tidyr); library(patchwork)
library(here);  library(stringr); library(scales); library(haven)
library(readxl); library(grid)
library(magrittr)
library(stargazer)

input_dir  <- here("data-raw","input","correlation")
output_dir <- here("figures")

# 0.1 CLIAR country (exclude NA & North America)
ctf_country <- read_dta(file.path(input_dir, "static_ctf_012225.dta")) |>
  mutate(region = str_to_title(trimws(as.character(region)))) |>
  filter(region != "North America", region != "")

# 0.2 WDI
wdi <- readxl::read_excel(
  file.path(input_dir, "P_Data_Extract_From_World_Development_Indicators_updated.xlsx"),
  sheet = "Data"
) %>%
  replace(. == "..", NA) %>%
  filter(!is.na(`Country Code`),
         `Series Name` %in% c(
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
  tidyr::pivot_wider(
    names_from  = `Series Name`,
    values_from = `2019_2023`
  ) %>%
  mutate(across(
    c(
      "Poverty headcount ratio at $2.15 a day (2017 PPP) (% of population)",
      "GDP per capita, PPP (constant 2021 international $)"
    ),
    as.numeric
  ))

# Define WDI variables of interest
wdi_vars <- c(
  "Poverty headcount ratio at $2.15 a day (2017 PPP) (% of population)",
  "GDP per capita, PPP (constant 2021 international $)"
)

# 0.3 Risk
risk_data <- read_excel(
  file.path(input_dir, "country_risk_index.xlsx"), sheet = "Table Data"
) |>
  select(`Country Name`, `Country Risk Index`) |>
  rename(country_risk = `Country Risk Index`) |>
  mutate(`Country Name` = trimws(`Country Name`))

# ----------------------------------------------------------------------------
# 1. Generic plotting function (with 2‐row legend)
# ----------------------------------------------------------------------------
run_ctf_plots <- function(country_df, ctf_vars, ctf_labels,
                          outcome_type = c("wdi","risk"), suffix) {
  outcome_type <- match.arg(outcome_type)

  # Merge in WDI + rename
  base <- country_df |>
    select(country_code, all_of(ctf_vars), region) |>
    left_join(wdi, by = c("country_code" = "Country Code")) |>
    rename_with(~ ctf_labels[.x], all_of(ctf_vars))

  # If risk, join risk_data and set up single‐outcome list
  if (outcome_type == "risk") {
    base <- base |> left_join(risk_data, by = c("Country Name"="Country Name"))
    outcome_list <- list(list(
      var       = "country_risk",
      y_label   = "Country Risk Index (2025)",
      transform = identity
    ))
  } else {
    outcome_list <- lapply(wdi_vars, function(v) {
      lab <- if (grepl("GDP per capita", v)) {
        "log(GDP per capita, PPP (constant 2021 international $))\n(circa 2022)"
      } else {
        paste0(str_wrap(v, width = 40), "\n(circa 2022)")
      }
      list(var = v, y_label = lab, transform = function(x) {
        if (grepl("GDP per capita", v)) log(x) else x
      })
    })
  }

  # Loop over each outcome
  for (out in outcome_list) {
    plots      <- list()
    country_ls <- list()

    for (cl in names(ctf_labels)) {
      disp <- ctf_labels[[cl]]
      tmp  <- base |>
        drop_na(all_of(c(disp, out$var))) |>
        mutate(y_val = out$transform(.data[[out$var]]))

      country_ls[[disp]] <- sort(unique(tmp$country_code))

      fit  <- lm(as.formula(sprintf("y_val ~ poly(`%s`,2)", disp)), data = tmp)
      r2   <- summary(fit)$r.squared
      r2l  <- sprintf("R² = %.3f", r2)
      x0   <- min(tmp[[disp]], na.rm=TRUE)
      y0   <- max(tmp$y_val, na.rm=TRUE)

      p <- ggplot(tmp, aes(x = .data[[disp]], y = y_val, color = region)) +
        geom_point(size=2) +
        geom_smooth(
          method  = "lm",
          formula = y ~ poly(x,2),
          se      = FALSE,
          color   = "black",
          linetype= "dashed"
        ) +
        annotate("text", x = x0, y = y0, label = r2l, hjust=0, vjust=1, size=4) +
        labs(x = paste0(disp, " (2019-2023)"), color = "Region") +
        scale_color_brewer(palette="Paired") +
        guides(color = guide_legend(nrow = 2, byrow = TRUE)) +
        theme_minimal(base_size=14) +
        theme(
          axis.title.y    = element_blank(),
          axis.title      = element_text(size=16),
          legend.position = "bottom",
          legend.title    = element_text(face="bold", size=14),
          legend.text     = element_text(size=12),
          panel.border    = element_rect(color="black", fill=NA)
        )

      plots[[disp]] <- p
    }

    # Suppress y‐axis on every 2nd plot
    plots_shared <- lapply(seq_along(plots), function(i) {
      if (i %% 2 == 0)
        plots[[i]] + theme(axis.text.y = element_blank(),
                           axis.ticks.y = element_blank())
      else
        plots[[i]]
    })

    # 2‐column grid + shared legend (2‐row ensured by guides above)
    combined <- wrap_plots(plots_shared, ncol=2) +
      plot_layout(guides="collect") &
      theme(legend.position="bottom", legend.box="horizontal")

    shared_y   <- textGrob(out$y_label, rot=90,
                           gp=gpar(fontsize=16, fontface="bold"))
    final_plot <- wrap_elements(shared_y) + combined +
      plot_layout(widths=c(0.1,1))

    # Filenames
    fn_base <- if (outcome_type=="risk") "CountryRisk_vs_CTF"
    else gsub("[^A-Za-z0-9]", "", out$var)
    file_png <- paste0(fn_base, "_", suffix, ".png")
    file_csv <- paste0(if (outcome_type=="risk") "CountryLists_CountryRisk_"
                       else "CountryLists_", fn_base, "_",
                       suffix, ".csv")

    ggsave(file.path(output_dir, file_png),
           final_plot,
           width  = 12,
           height = if (length(ctf_labels)>4) 12 else 8,
           dpi    = 300,
           bg     = "white")

    # Export country lists
    max_l <- max(sapply(country_ls, length))
    padded <- lapply(country_ls, function(v){ length(v)<-max_l; v })
    dfc <- as.data.frame(padded, stringsAsFactors=FALSE)
  }
}

# ----------------------------------------------------------------------------
# 2. Run all three cluster‐sets for both WDI & Risk
# ----------------------------------------------------------------------------

# (a) original 6 clusters → "main"
ctf_vars_6   <- c("vars_hrm_avg","vars_digital_avg","vars_anticorruption_avg",
                  "vars_transp_avg","vars_leg_avg","vars_mkt_avg")
ctf_labels_6 <- c(
  vars_hrm_avg            = "Public Human Resource Management",
  vars_digital_avg        = "Digital and Data",
  vars_anticorruption_avg = "Degree of Integrity",
  vars_transp_avg         = "Transparency and Accountability",
  vars_leg_avg            = "Justice",
  vars_mkt_avg            = "Business Environment"
)
run_ctf_plots(ctf_country,    ctf_vars_6, ctf_labels_6, "wdi",  "main")
run_ctf_plots(ctf_country,    ctf_vars_6, ctf_labels_6, "risk", "main")

# (b) first 4 w/o SSA → "annex_nossa"
ctf_country_nossa <- ctf_country |> filter(region!="Sub-Saharan Africa")
ctf_vars_4a       <- c("vars_hrm_avg","vars_digital_avg",
                       "vars_anticorruption_avg","vars_transp_avg")
ctf_labels_4a     <- c(
  vars_hrm_avg            = "Public Human Resource Management",
  vars_digital_avg        = "Digital and Data",
  vars_anticorruption_avg = "Degree of Integrity",
  vars_transp_avg         = "Transparency and Accountability"
)
run_ctf_plots(ctf_country_nossa, ctf_vars_4a, ctf_labels_4a, "wdi",  "annex_nossa")
run_ctf_plots(ctf_country_nossa, ctf_vars_4a, ctf_labels_4a, "risk", "annex_nossa")

# (c) other 4 incl SSA → "annex"
ctf_vars_4b   <- c("vars_pol_avg","vars_social_avg","vars_leg_avg","vars_climate_avg")
ctf_labels_4b <- c(
  vars_pol_avg     = "Political",
  vars_social_avg  = "Social",
  vars_leg_avg     = "Justice",
  vars_climate_avg = "Energy and Environment"
)
run_ctf_plots(ctf_country,    ctf_vars_4b, ctf_labels_4b, "wdi",  "annex")
run_ctf_plots(ctf_country,    ctf_vars_4b, ctf_labels_4b, "risk", "annex")

# ----------------------------------------------------------------------------
# 3. Regression on WDI & Risk
# ----------------------------------------------------------------------------
# Generate regression table in Annex 5
lm_risk_institutions <- lm(
  `Country Risk Index` ~ `Public Human Resource Management Institutions` +
    `Digital and Data Institutions` + `Transparency and Accountability Institutions` +
    `Degree of Integrity` + as.factor(region),
  data = risk_data |>
    rename(`Country Risk Index` = country_risk) |>
    inner_join(
      ctf_country |>
          filter(region != "North America" & region != "" & region != " ") |>
          select(all_of(ctf_vars_6), country_name, region) |>
          rename(
            `Public Human Resource Management Institutions` = vars_hrm_avg,
            `Digital and Data Institutions` = vars_digital_avg,
            `Transparency and Accountability Institutions` = vars_transp_avg,
            `Degree of Integrity` = vars_anticorruption_avg
          ),
      by = c("Country Name" = "country_name")
    ) |>
    mutate(
      across(
        where(is.numeric),
        \(col) as.vector(scale(col))
      )
    )
)

# export table
stargazer(
  lm_risk_institutions,
  omit  = "as.factor",
  type = "html",
  out = here(output_dir, "lm_risk_institutions.doc")
)
