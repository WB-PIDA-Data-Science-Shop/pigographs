# ----------------------------------------------------------------------------
# 0. Setup & Data Loading
# ----------------------------------------------------------------------------
library(dplyr); library(ggplot2); library(tidyr); library(patchwork)
library(here);  library(stringr); library(scales); library(haven)
library(readxl); library(grid)
library(magrittr)
library(stargazer)
library(cliaretl)

input_dir  <- "//egvpi/egvpi/data/pigo/input/correlation"
output_dir <- here("figures")

# 0.1 CLIAR country (exclude NA & North America)
ctf_country <- read_dta(file.path(input_dir, "static_ctf_032525.dta")) |>
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
                          outcome_type = c("wdi","risk"),
                          suffix,
                          highlight_country = "KEN") {

  outcome_type <- match.arg(outcome_type)

  # Merge in WDI + rename
  base <- country_df |>
    dplyr::select(country_code, dplyr::all_of(ctf_vars), region) |>
    dplyr::left_join(wdi, by = c("country_code" = "Country Code")) |>
    dplyr::rename_with(~ ctf_labels[.x], dplyr::all_of(ctf_vars))

  # If risk: join risk_data and set up outcome_list
  if (outcome_type == "risk") {
    base <- base |> dplyr::left_join(risk_data, by = c("Country Name"="Country Name"))
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
        paste0(stringr::str_wrap(v, width = 40), "\n(circa 2022)")
      }

      list(
        var     = v,
        y_label = lab,
        transform = function(x) if (grepl("GDP per capita", v)) log(x) else x
      )
    })
  }

  # Loop over each outcome
  for (out in outcome_list) {
    plots      <- list()
    country_ls <- list()

    for (cl in names(ctf_labels)) {

      disp <- ctf_labels[[cl]]

      tmp <- base |>
        tidyr::drop_na(dplyr::all_of(c(disp, out$var))) |>
        dplyr::mutate(y_val = out$transform(.data[[out$var]]))

      country_ls[[disp]] <- sort(unique(tmp$country_code))

      # Fit quadratic regression
      fit <- lm(as.formula(sprintf("y_val ~ poly(`%s`, 2)", disp)), data = tmp)
      fit_s <- summary(fit)
      coeffs <- coef(fit_s)

      # Extract coefficients
      b0 <- coeffs[1, 1]
      b1 <- coeffs[2, 1]
      b2 <- coeffs[3, 1]
      pval_b1 <- coeffs[2, 4]
      r2 <- fit_s$r.squared

      # Significance stars
      sig_star <- ifelse(pval_b1 < 0.001, "***",
                         ifelse(pval_b1 < 0.01, "**",
                                ifelse(pval_b1 < 0.05, "*",
                                       ifelse(pval_b1 < 0.1, ".", ""))))

      # Labels
      eq_label <- sprintf("y = %.3f + %.3f·x + %.3f·x²", b0, b1, b2)
      p_label  <- sprintf("p-value(slope) = %.2e%s", pval_b1, sig_star)
      r2_label <- sprintf("R² = %.3f", r2)

      reg_label <- paste(eq_label, p_label, r2_label, sep = "\n")

      # Add highlight flag
      tmp <- tmp |>
        dplyr::mutate(
          highlight_flag = dplyr::if_else(
            country_code == highlight_country,
            highlight_country,
            "Other"
          )
        )

      # Determine top-left annotation placement
      x0 <- min(tmp[[disp]], na.rm = TRUE)
      y0 <- max(tmp$y_val, na.rm = TRUE)


      # Build plot
      p <-
        ggplot2::ggplot(tmp, ggplot2::aes(
          x = .data[[disp]],
          y = y_val,
          color = highlight_flag
        )) +
        ggplot2::geom_point(size = 2) +
        ggplot2::geom_text(
          data = subset(tmp, highlight_flag == highlight_country),
          ggplot2::aes(label = country_code),
          vjust = -0.7,
          size = 3.5,
          color = "steelblue",
          fontface = "bold"
        ) +
        ggplot2::geom_smooth(
          method = "lm",
          formula = y ~ poly(x, 2),
          se = FALSE,
          color = "black",
          linetype = "dashed"
        ) +
        ggplot2::annotate(
          "label",
          x = x0,
          y = 135,
          label = reg_label,
          hjust = 0,
          vjust = 1,
          size = 3.8,
          fill = "white",
          alpha = 0.9,
          label.size = 0.3
        ) +
        ylim(c(0, 140)) +
        ggplot2::labs(
          x = paste0(disp, " (2019–2023)"),
          color = NULL
        ) +
        ggplot2::scale_color_manual(
          values = c(
            setNames("steelblue", highlight_country),
            Other = "gray80"
          ),
          guide = "none"
        ) +
        ggplot2::theme_bw(base_size = 14) +
        ggplot2::theme(
          axis.title.y    = ggplot2::element_blank(),
          axis.title      = ggplot2::element_text(size = 16),
          legend.position = "bottom",
          legend.title    = ggplot2::element_text(face = "bold", size = 14),
          legend.text     = ggplot2::element_text(size = 12),
          panel.border    = ggplot2::element_rect(color = "black", fill = NA)
        )

      plots[[disp]] <- p
    }

    # Remove y-axis from every 2nd plot
    plots_shared <- lapply(seq_along(plots), function(i) {
      if (i %% 2 == 0)
        plots[[i]] +
        ggplot2::theme(axis.text.y = ggplot2::element_blank(),
                       axis.ticks.y = ggplot2::element_blank())
      else
        plots[[i]]
    })

    # Combined layout
    combined <- patchwork::wrap_plots(plots_shared, ncol = 2) +
      patchwork::plot_layout(guides = "collect") &
      ggplot2::theme(legend.position = "bottom", legend.box = "horizontal")

    shared_y <- grid::textGrob(out$y_label, rot = 90,
                               gp = grid::gpar(fontsize = 16, fontface = "bold"))

    final_plot <- patchwork::wrap_elements(shared_y) + combined +
      patchwork::plot_layout(widths = c(0.1, 1))

    # Filenames
    fn_base <- if (outcome_type == "risk") "CountryRisk_vs_CTF"
    else gsub("[^A-Za-z0-9]", "", out$var)

    file_png <- paste0(fn_base, "_", suffix, ".png")
    file_csv <- paste0(
      if (outcome_type == "risk") "CountryLists_CountryRisk_" else "CountryLists_",
      fn_base, "_",
      suffix,
      ".csv"
    )

    ggsave(file.path(output_dir, file_png),
           final_plot,
           width  = 12,
           height = if (length(ctf_labels) > 4) 12 else 8,
           dpi    = 300,
           bg     = "white")

    # Export country lists
    max_l <- max(sapply(country_ls, length))
    padded <- lapply(country_ls, function(v){ length(v) <- max_l; v })
    dfc <- as.data.frame(padded, stringsAsFactors = FALSE)
  }
}


run_ctf_plots2 <- function(country_df, ctf_vars, ctf_labels,
                           outcome_type = c("wdi","risk"),
                           suffix,
                           highlight_country = "KEN") {

  outcome_type <- match.arg(outcome_type)

  # -----------------------------
  # Automatically detect numeric WDI variables
  # -----------------------------
  wdi_numeric_vars <- wdi |>
    dplyr::select(-`Country Name`, -`Country Code`) |>
    dplyr::select(where(is.numeric)) |>
    names()

  # -----------------------------
  # Merge in WDI + rename CTF vars
  # -----------------------------
  base <- country_df |>
    dplyr::select(country_code, all_of(ctf_vars), CountryName = country_name) |>
    dplyr::left_join(wdi, by = c("country_code" = "Country Code")) |>
    dplyr::rename_with(~ ctf_labels[.x], all_of(ctf_vars))

  # -----------------------------
  # Outcome list
  # -----------------------------
  if (outcome_type == "risk") {

    base <- base |>
      dplyr::left_join(risk_data, by = c("CountryName"="Country Name"))

    outcome_list <- list(list(
      var       = "country_risk",
      y_label   = "Country Risk Index (2025)",
      transform = identity
    ))

  } else {

    # WDI numeric outcomes
    outcome_list <- lapply(wdi_numeric_vars, function(v) {

      lab <- dplyr::case_when(
        grepl("gdp.per.capita", v, ignore.case = TRUE) ~
          "log(GDP per capita, PPP (constant 2021 international $))\n(circa 2022)",
        grepl("fdi", v, ignore.case = TRUE) & grepl("gdp", v, ignore.case = TRUE) ~
          "FDI Net Inflows as Share of GDP (2019–2023)",
        grepl("nbf", v, ignore.case = TRUE) | grepl("newbiz", v, ignore.case = TRUE) ~
          "New Businesses Registered per 1000 People (2019–2023)",
        TRUE ~ paste0(stringr::str_wrap(v, width = 40), "\n(circa 2022)")
      )

      transform_fn <- function(x) {
        if (grepl("GDP per capita", v, ignore.case = TRUE)) log(x) else x
      }

      list(var = v, y_label = lab, transform = transform_fn)
    })
  }

  # -----------------------------
  # Loop through outcomes
  # -----------------------------
  for (out in outcome_list) {

    plots      <- list()
    country_ls <- list()

    for (cl in names(ctf_labels)) {

      disp <- ctf_labels[[cl]]

      tmp <- base |>
        tidyr::drop_na(dplyr::all_of(c(disp, out$var))) |>
        dplyr::mutate(
          y_val = out$transform(.data[[out$var]]),
          highlight_flag = ifelse(country_code == highlight_country, highlight_country, "Other")
        )

      # Optional filtering for extreme FDI/rent values
      if ("fdishare_gdp" %in% names(tmp)) {
        tmp <- tmp |>
          dplyr::filter(fdishare_gdp >= 0 & fdishare_gdp <= 50 & rrentshare_gdp <= 20)
      }

      country_ls[[disp]] <- sort(unique(tmp$country_code))

      # -----------------------------
      # Fit quadratic model (raw polynomial for readable coefficients)
      # -----------------------------
      fit <- lm(as.formula(sprintf("y_val ~ poly(`%s`, 2, raw = TRUE)", disp)), data = tmp)
      fit_s <- summary(fit)
      coeffs <- coef(fit_s)

      b0 <- coeffs[1,1]
      b1 <- coeffs[2,1]
      b2 <- coeffs[3,1]
      pval_b1 <- coeffs[2,4]
      r2 <- fit_s$r.squared

      # Significance stars
      sig_star <- ifelse(pval_b1 < 0.001, "***",
                         ifelse(pval_b1 < 0.01, "**",
                                ifelse(pval_b1 < 0.05, "*",
                                       ifelse(pval_b1 < 0.1, ".", ""))))

      reg_label <- sprintf(
        "y = %.3f + %.3f·x + %.3f·x²\np-value(slope) = %.2e%s\nR² = %.3f",
        b0, b1, b2, pval_b1, sig_star, r2
      )

      # -----------------------------
      # Determine annotation coordinates
      # -----------------------------
      x0 <- min(tmp[[disp]], na.rm=TRUE)
      y_range <- range(tmp$y_val, na.rm=TRUE)
      y_max <- y_range[2] + 0.15 * diff(y_range)  # expand y-axis by 15% to fit label

      # -----------------------------
      # Build plot
      # -----------------------------
      p <- ggplot2::ggplot(tmp, ggplot2::aes(x = .data[[disp]], y = y_val, color = highlight_flag)) +
        ggplot2::geom_point(size = 2) +
        ggplot2::scale_color_manual(values = c(setNames("steelblue", highlight_country), Other = "gray80"),
                                    guide = "none") +
        ggplot2::geom_smooth(method = "lm", formula = y ~ poly(x,2,raw=TRUE),
                             se = FALSE, color = "black", linetype="dashed") +
        ggplot2::annotate("label", x = x0, y = y_max,
                          label = reg_label, hjust=0, vjust=1,
                          size = 3.8, fill="white", alpha=0.9, label.size = 0.3) +
        ggplot2::labs(x = paste0(disp, " (2019–2023)")) +
        ggplot2::expand_limits(y = y_max) +
        ggplot2::theme_bw(base_size = 14) +
        ggplot2::theme(
          axis.title.y    = ggplot2::element_blank(),
          axis.title      = ggplot2::element_text(size=16),
          legend.position = "none",
          panel.border    = ggplot2::element_rect(color="black", fill=NA),
          panel.grid      = ggplot2::element_line(color="grey85")
        )

      # Highlight country label
      tmp_h <- tmp |> dplyr::filter(country_code == highlight_country)
      if (nrow(tmp_h) > 0) {
        p <- p +
          ggplot2::geom_text(data = tmp_h,
                             ggplot2::aes(label = CountryName),
                             nudge_y = 0.02 * diff(y_range),
                             size = 4, fontface="bold", color="steelblue")
      }

      plots[[disp]] <- p
    }

    # -----------------------------
    # Remove y-axis from every 2nd plot
    # -----------------------------
    plots_shared <- lapply(seq_along(plots), function(i) {
      if (i %% 2 == 0)
        plots[[i]] +
        ggplot2::theme(axis.text.y = ggplot2::element_blank(),
                       axis.ticks.y = ggplot2::element_blank())
      else
        plots[[i]]
    })

    # -----------------------------
    # Combine with shared y-axis
    # -----------------------------
    combined <- patchwork::wrap_plots(plots_shared, ncol=2) +
      patchwork::plot_layout(guides="collect")

    shared_y <- grid::textGrob(out$y_label, rot=90,
                               gp=grid::gpar(fontsize=16, fontface="bold"))

    final_plot <- patchwork::wrap_elements(shared_y) + combined +
      patchwork::plot_layout(widths=c(0.1,1))

    # -----------------------------
    # Save PNG
    # -----------------------------
    fn_base <- if (outcome_type=="risk") "CountryRisk_vs_CTF"
    else gsub("[^A-Za-z0-9]", "", out$var)

    file_png <- paste0(fn_base, "_", suffix, ".png")

    ggsave(file.path(output_dir, file_png),
           final_plot,
           width  = 12,
           height = if(length(ctf_labels) > 4) 12 else 8,
           dpi    = 300,
           bg     = "white")

    # -----------------------------
    # Export country lists
    # -----------------------------
    max_l <- max(sapply(country_ls, length))
    padded <- lapply(country_ls, function(v){ length(v) <- max_l; v })
    dfc <- as.data.frame(padded, stringsAsFactors = FALSE)

  }
}



#### compute some plots for verena
corruption_list <- c("vdem_core_v2x_pubcorr", "vdem_core_v2x_execorr", "vdem_core_v2lgcrrpt",
                     "wjp_rol_2", "wjp_rol_6_2", "wjp_rol_4_8", "bs_bti_q12_1", "bs_bti_q15_1",
                     "vars_anticorruption_avg")

corruption_labels <- c(
  vdem_core_v2x_pubcorr   = "Public Sector Corruption",
  vdem_core_v2x_execorr   = "Executive Corruption",
  vdem_core_v2lgcrrpt     = "Legislative Corruption",
  wjp_rol_2               = "Absence of Corruption",
  wjp_rol_6_2             = "Administrative Corruption",
  wjp_rol_4_8             = "Constraints on Government Power",
  bs_bti_q12_1            = "Anti-Corruption Policy",
  bs_bti_q15_1            = "Integrity Framework",
  vars_anticorruption_avg = "Degree of Integrity"
)

run_ctf_plots(country_df = ctf_country,
              ctf_vars = corruption_list,
              ctf_labels = corruption_labels,
              outcome_type = "risk",
              suffix = "main",
              highlight_country = "KEN")

### pull in the FDI inflows as a share of GDP & new business information

fdi_dt <-
cliaretl::extract_data_from_api(dataset_id = "WB_WDI",
                                indicator_ids = "WB_WDI_BX_KLT_DINV_WD_GD_ZS",
                                source = "d360")

fdi_dt <- fdi_dt[[2]]

fdi_dt <-
  fdi_dt |>
  dplyr::select(OBS_VALUE, TIME_PERIOD, REF_AREA) |>
  rename(fdishare_gdp = "OBS_VALUE",
         year = "TIME_PERIOD",
         country_code = "REF_AREA") |>
  mutate(fdishare_gdp = as.numeric(fdishare_gdp),
         year = as.integer(year)) |>
  dplyr::filter(year >= 2019 & year <= 2023) |>
  group_by(country_code) |>
  summarise(fdishare_gdp = mean(fdishare_gdp, na.rm = TRUE))


### include indicator for new business formation
nbf_dt <-
  cliaretl::extract_data_from_api(dataset_id = "WB_WDI",
                                  indicator_ids = "WB_WDI_IC_BUS_NDNS_ZS",
                                  source = "d360")

nbf_dt <- nbf_dt[[2]]

nbf_dt <-
  nbf_dt |>
  dplyr::select(OBS_VALUE, TIME_PERIOD, REF_AREA) |>
  rename(nbfper1000 = "OBS_VALUE",
         year = "TIME_PERIOD",
         country_code = "REF_AREA") |>
  mutate(nbfper1000 = as.numeric(nbfper1000),
         year = as.integer(year)) |>
  dplyr::filter(year >= 2019 & year <= 2023) |>
  group_by(country_code) |>
  summarise(nbfper1000 = mean(nbfper1000, na.rm = TRUE))


wdi <-
  wdi |>
  rename(country_code = `Country Code`) |>
  full_join(fdi_dt, by = "country_code") |>
  full_join(nbf_dt, by = "country_code")

wdi <- wdi |> rename(`Country Code` = "country_code")

### ok lets include resource wealth and drop resource rich countries
rrent_dt <- cliaretl::extract_data_from_api(dataset_id = "WB_WDI",
                                            indicator_ids = "WB_WDI_NY_GDP_TOTL_RT_ZS",
                                            source = "d360")[[2]]

rrent_dt <-
  rrent_dt |>
  dplyr::select(OBS_VALUE, TIME_PERIOD, REF_AREA) |>
  rename(rrentshare_gdp = "OBS_VALUE",
         year = "TIME_PERIOD",
         country_code = "REF_AREA") |>
  mutate(rrentshare_gdp = as.numeric(rrentshare_gdp),
         year = as.integer(year)) |>
  dplyr::filter(year >= 2019 & year <= 2023) |>
  group_by(country_code) |>
  summarise(rrentshare_gdp = mean(rrentshare_gdp, na.rm = TRUE))

wdi <- wdi |> full_join(rrent_dt, by = c("Country Code" = "country_code"))


# wdi <- wdi |> dplyr::filter(rrentshare_gdp <= 20)

run_ctf_plots2(country_df = ctf_country,
              ctf_vars = corruption_list,
              ctf_labels = corruption_labels,
              outcome_type = "wdi",
              suffix = "main",
              highlight_country = "KEN")

### lets put together the raw data and documentation

raw_dt <-
  ctf_country |>
  dplyr::select(country_name, country_code, region, all_of(corruption_list)) |>
  full_join(wdi |>
              rename(country_code = `Country Name`,
                     country_name = `Country Code`) |>
              dplyr::select(country_code,
                            country_name,
                            fdishare_gdp,
                            nbfper1000,
                            rrentshare_gdp),
            by = c("country_code",
                   "country_name"))


vars <- intersect(db_variables_final$variable, colnames(raw_dt))

doc_dt <-
  bind_rows(tibble(variable = vars,
                   var_name = db_variables_final$var_name[db_variables_final$variable %in% vars],
                   description = db_variables_final$description[db_variables_final$variable %in% vars],
                   source = db_variables_final$source[db_variables_final$variable %in% vars]),
            tibble(variable = c("fdishare_gdp", "nbfper1000", "rrentshare_gdp"),
                   var_name = c("FDI Net Inflows as a Share of GDP", "New Businesses Registered per 1000 people", "Natural Resource Rents Share of GDP"),
                   description = c(" Foreign direct investment are the net inflows of investment to acquire a lasting management interest (10 percent or more of voting stock) in an enterprise operating in an economy other than that of the investor. It is the sum of equity capital, reinvestment of earnings, other long-term capital, and short-term capital as shown in the balance of payments. This series shows net inflows (new investment inflows less disinvestment) in the reporting economy from foreign investors, and is divided by GDP. (2019-2023)",
                                   "The number of newly registered firms with limited liability per 1,000 working-age people (ages 15-64) per calendar year. (2019 - 2023)",
                                   "Total natural resources rents are the sum of oil rents, natural gas rents, coal rents (hard and soft), mineral rents, and forest rents (2019-2021)"),
                   source = rep("WDI", 3)))


writexl::write_xlsx(list("data" = raw_dt,
                         "documentation" = doc_dt),
                    "figures/verena_kenya.xlsx")
























































































# # ----------------------------------------------------------------------------
# # 2. Run all three cluster‐sets for both WDI & Risk
# # ----------------------------------------------------------------------------
#
# # (a) original 6 clusters → "main"
# ctf_vars_6   <- c("vars_hrm_avg","vars_digital_avg","vars_anticorruption_avg",
#                   "vars_transp_avg","vars_leg_avg","vars_mkt_avg")
# ctf_labels_6 <- c(
#   vars_hrm_avg            = "Public Human Resource Management",
#   vars_digital_avg        = "Digital and Data",
#   vars_anticorruption_avg = "Degree of Integrity",
#   vars_transp_avg         = "Transparency and Accountability",
#   vars_leg_avg            = "Justice",
#   vars_mkt_avg            = "Business Environment"
# )
# run_ctf_plots(ctf_country,    ctf_vars_6, ctf_labels_6, "wdi",  "main")
# run_ctf_plots(ctf_country,    ctf_vars_6, ctf_labels_6, "risk", "main")
#
# # (b) first 4 w/o SSA → "annex_nossa"
# ctf_country_nossa <- ctf_country |> filter(region!="Sub-Saharan Africa")
# ctf_vars_4a       <- c("vars_hrm_avg","vars_digital_avg",
#                        "vars_anticorruption_avg","vars_transp_avg")
# ctf_labels_4a     <- c(
#   vars_hrm_avg            = "Public Human Resource Management",
#   vars_digital_avg        = "Digital and Data",
#   vars_anticorruption_avg = "Degree of Integrity",
#   vars_transp_avg         = "Transparency and Accountability"
# )
# run_ctf_plots(ctf_country_nossa, ctf_vars_4a, ctf_labels_4a, "wdi",  "annex_nossa")
# run_ctf_plots(ctf_country_nossa, ctf_vars_4a, ctf_labels_4a, "risk", "annex_nossa")
#
# # (c) other 4 incl SSA → "annex"
# ctf_vars_4b   <- c("vars_pol_avg","vars_social_avg","vars_leg_avg","vars_climate_avg")
# ctf_labels_4b <- c(
#   vars_pol_avg     = "Political",
#   vars_social_avg  = "Social",
#   vars_leg_avg     = "Justice",
#   vars_climate_avg = "Energy and Environment"
# )
# run_ctf_plots(ctf_country,    ctf_vars_4b, ctf_labels_4b, "wdi",  "annex")
# run_ctf_plots(ctf_country,    ctf_vars_4b, ctf_labels_4b, "risk", "annex")
#
# # ----------------------------------------------------------------------------
# # 3. Regression on WDI & Risk
# # ----------------------------------------------------------------------------
# # Generate regression table in Annex 5
# lm_risk_institutions <- lm(
#   `Country Risk Index` ~ `Public Human Resource Management Institutions` +
#     `Digital and Data Institutions` + `Transparency and Accountability Institutions` +
#     `Degree of Integrity` + as.factor(region),
#   data = risk_data |>
#     rename(`Country Risk Index` = country_risk) |>
#     inner_join(
#       ctf_country |>
#           filter(region != "North America" & region != "" & region != " ") |>
#           select(all_of(ctf_vars_6), country_name, region) |>
#           rename(
#             `Public Human Resource Management Institutions` = vars_hrm_avg,
#             `Digital and Data Institutions` = vars_digital_avg,
#             `Transparency and Accountability Institutions` = vars_transp_avg,
#             `Degree of Integrity` = vars_anticorruption_avg
#           ),
#       by = c("Country Name" = "country_name")
#     ) |>
#     mutate(
#       across(
#         where(is.numeric),
#         \(col) as.vector(scale(col))
#       )
#     )
# )
#
# # export table
# stargazer(
#   lm_risk_institutions,
#   omit  = "as.factor",
#   type = "html",
#   out = here(output_dir, "lm_risk_institutions.doc")
# )





