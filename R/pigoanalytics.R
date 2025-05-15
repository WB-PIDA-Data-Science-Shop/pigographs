#' A function to Compute Regional Min/Average/Max Statistics for a Given Cluster
#'
#' @param data A data frame containing the input data.
#' @param dimension The column name (as a string or symbol) representing the dimension to compute statistics for.
#' @return A data frame with computed regional statistics.
#'
#' @import dplyr rlang tidyr countrycode
#' @importFrom rlang ensym
#' @export

compute_regional_statistics <- function(data, dimension) {
  dimension <- rlang::ensym(dimension)

  # Define custom mappings for unmatched country names
  custom_mappings <- c(
    "Channel Islands" = "CHI",
    "Kosovo" = "XKX"
  )

  data |>
    select(country_name, !!dimension, region) |>
    group_by(region) |>
    mutate(
      region_av = ifelse(all(is.na(!!dimension)), NA, mean(!!dimension, na.rm = TRUE)),
      max_av = ifelse(all(is.na(!!dimension)), NA, max(!!dimension, na.rm = TRUE)),
      min_av = ifelse(all(is.na(!!dimension)), NA, min(!!dimension, na.rm = TRUE))
    ) |>
    ungroup() |>
    pivot_longer(
      cols = c(min_av, region_av, max_av),
      names_to = "type",
      values_to = "value"
    ) |>
    mutate(
      type = recode(type,
                    min_av = "Min",
                    region_av = "Average",
                    max_av = "Max")
    ) |>
    left_join(
      data |>
        group_by(region) |>
        summarise(
          min_value = ifelse(all(is.na(!!dimension)), NA, min(!!dimension, na.rm = TRUE)),
          max_value = ifelse(all(is.na(!!dimension)), NA, max(!!dimension, na.rm = TRUE)),
          .groups = "drop"
        ),
      by = "region"
    ) |>
    mutate(
      country_code = countrycode(
        country_name,
        "country.name",
        "iso3c",
        custom_match = custom_mappings
      )
    ) |>
    rename(
      country_dimension_av = !!dimension,
      region_long = region
    )
}


#' A function to wrap graph's text labels
#'
#' @param x object or variable that need to be wrapped
#' @param width number of characters
#'
#' @import stringr
#' @export
wrap_facet_titles <- function(x, width) {
  str_wrap(x, width = width)
}

#' A function to Generate a Min/Average/Max Labeled Plot by Region
#'
#' Creates a plot with labels for a specific dimension.
#'
#' @param data A data frame containing the input data.
#' @param dimension The column name (as a string or symbol) representing the dimension to filter and plot.
#' @return A ggplot object with the generated labels plot.
#'
#' @importFrom rlang !!
#' @importFrom dplyr filter mutate
#' @importFrom ggplot2 ggplot aes geom_point geom_segment scale_shape_manual theme element_text
#' @importFrom ggrepel geom_text_repel
#' @export
#'

generate_regional_minmax_plot <- function(data, dimension) {
  # Filter data for the specific dimension
  filtered_data <- data |>
    filter(dimension == !!dimension) |>
    mutate(region_w = wrap_facet_titles(region_long, width = 15))

  # Generate the plot
  ggplot(filtered_data, aes(x = region_w,
                            y = value,
                            shape = type,
                            color = region_w)) +
    geom_point(size = 8) +

    # Vertical segment from min to max values
    geom_segment(aes(x = region_w,
                     xend = region_w,
                     y = min_value,
                     yend = max_value),
                 linetype = "solid",
                 linewidth = 1.5) +

    # Add country code labels
    geom_text_repel(aes(x = region_w,
                        y = country_dimension_av,
                        label = country_code),
                    data = filtered_data |> filter(type == "Average"),
                    size = 4,
                    nudge_x = -0.3,
                    segment.color = NA,
                    box.padding = 0.15,
    ) +

    # Custom shapes for points
  scale_shape_manual(values = c("Min" = 19, "Average" = 24, "Max" = 19)) +

    # Adjust x-axis text to be shifted to the left
    theme(axis.text.x = element_text(hjust = 1))
}


#' A function to Compute Change in Cluster Values Between Two Years for Each Country
#'
#' Computes the difference in values for a specific cluster between two years.
#'
#' @param data A data frame containing the input data.
#' @param cluster_name The name of the cluster to filter.
#' @param from_year The starting year for comparison.
#' @param to_year The ending year for comparison.
#' @return A data frame with the computed differences.
#' @export
compute_cluster_diff <- function(data, cluster_name, from_year, to_year) {
  data |>
    filter(cluster == cluster_name,
           year %in% c(from_year, to_year),
           !is.na(value)) |>
    pivot_wider(
      id_cols = c(country_name, country_code, income_group, lending_category, region),
      names_from = year,
      values_from = value,
      names_prefix = "value_"
    ) |>
    mutate(ctf_distance = .data[[paste0("value_", to_year)]] - .data[[paste0("value_", from_year)]]) |>
    arrange(desc(ctf_distance))
}



#' A function to Plot Regional CTF Score Change by Country
#'
#' Creates a lollipop-style chart of CTF score changes for each country,
#' facetted by region with free y-axis scales. Each country’s change (
#' \code{ctf_distance}) is shown as a line from 0 plus a point, with
#' a reference horizontal line at 0. The chart is flipped for readability
#' of country names.
#'
#' @param data A data frame containing the input data.
#' @return A ggplot object showing the regional changes.
#' @export

generate_diff_plot <- function(data){
  data |>
    ggplot(aes(x = country_name, y = ctf_distance, color = as.factor(region))) +
    geom_segment(aes(xend = country_name, y = 0, yend = ctf_distance), linewidth = 1) +
    geom_point(size = 2, alpha = 0.6) +
    geom_hline(yintercept = 0,
               linetype = "solid",
               linewidth = .5,
               alpha = 0.75,
               color =  "lightgrey") +
    labs(
      x = "Country by Region",
      y = "Change in CTF Score"
    ) +
    facet_wrap(~region, scales = "free_y", nrow = 2) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(size = 18, hjust = .5),
      axis.text.y = element_text(size = 16),
      axis.title.y = element_text(size = 20, face = "bold"),
      axis.title.x = element_text(size = 20, face = "bold"),
      plot.background = element_blank(),
      plot.caption = element_text(hjust = 0, size = 10),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      legend.position = "none",
      strip.text = element_text(size = 20)
    ) +
    scale_color_brewer(palette = "Paired") +
    scale_y_continuous(limits = c(-0.4, 0.4), breaks = c(-0.4, 0, 0.4)) +
    coord_flip()
}




compute_indicator_diff <- function(data, from_year, to_year) {
  data |>
    filter(year %in% c(from_year, to_year)) |>
    pivot_wider(
      id_cols     = c(country_name, country_code, income_group,
                      lending_category, region, variable, var_name, family_name),
      names_from  = year,
      values_from = value,
      names_prefix= "value_",
      values_fill = NA_real_
    ) |>
      mutate(
        ctf_distance =
          .data[[paste0("value_", to_year)]] -
          .data[[paste0("value_", from_year)]]
    ) |>
    arrange(desc(ctf_distance))
}




plot_cluster_appendix <- function(data, cluster_ctf, year_label) {
  # make a file‐system–safe folder name:
  safe_cluster <- gsub("[^A-Za-z0-9]+", "_", cluster_ctf)
  out_dir     <- here("figures", "appendix", safe_cluster)

  # create the directory if needed:
  if (!dir.exists(out_dir)) {
    dir.create(out_dir, recursive = TRUE)
  }

  # subset to that cluster
  cluster_data <- data %>%
    filter(family_name == cluster_ctf)

  # one plot per var_name
  indicators <- unique(cluster_data$var_name)

  for (indicator in indicators) {
    # subset and reorder within each region
    plot_data <- cluster_data %>%
      filter(var_name == indicator) %>%
      group_by(region) %>%
      mutate(
        country_name = fct_reorder(country_name, ctf_distance, .desc = FALSE)
      ) %>%
      ungroup()

    # build the plot
    p <- ggplot(plot_data, aes(
      x     = country_name,
      y     = ctf_distance,
      color = as.factor(region)
    )) +
      geom_segment(aes(xend = country_name, y = 0, yend = ctf_distance),
                   linewidth = 1) +
      geom_point(size = 2, alpha = 0.6) +
      geom_hline(yintercept = 0,
                 linetype = "solid",
                 linewidth = 0.5,
                 alpha = 0.75) +
      labs(
        x        = "Country",
        y        = "Change in CTF Score",
        title    = paste0("Change between ", year_label, " for: ", indicator),
        subtitle = paste("Cluster:", cluster_ctf),
        caption = paste(
          "Source: World Bank CLIAR Dashboard. Analysis by PIGO authors",
          "Includes only IBRD/IDA & Blend countries. Observations shown only where data are available",
          sep = "\n"
        )
      ) +
      facet_wrap(~region, scales = "free_y", nrow = 2) +
      theme_minimal() +
      theme(
        axis.text.x     = element_text(size = 18, angle = 45, hjust = 0.5),
        axis.text.y     = element_text(size = 16, hjust = 1),
        axis.title      = element_text(size = 20, face = "bold"),
        plot.title      = element_text(size = 22, face = "bold", hjust = 0.5),
        plot.subtitle   = element_text(size = 16, face = "bold", hjust = 0.5),
        strip.text      = element_text(size = 18),
        legend.position = "none",
        panel.grid      = element_blank()
      ) +
      scale_color_brewer(palette = "Paired") +
      scale_y_continuous(limits = c(-1, 1), breaks = seq(-1, 1, by = 0.2)) +
      coord_flip()

    # Save under figures/<safe_cluster>/<safe_indicator>.png
    safe_ind <- gsub("[^A-Za-z0-9]+", "_", indicator)
    ggsave(
      filename = file.path(out_dir, paste0(safe_ind, "_indicator.png")),
      plot     = p,
      bg       = "white",
      width    = 20,
      height   = 16,
      dpi      = 300
    )
  }
}




#' Plot Budget Execution Ratios by Region
#'
#' @description
#' Filters a BOOST budget execution data frame by year, sector, and budget_line,
#' then returns a flipped execution‐ratio plot with reference bands.
#'
#' @param data
#'   A data.frame or tibble containing at least:
#'   `year` (numeric), `sector`, `budget_line`,
#'   `region`, `execution_ratio`, `country_code`.
#' @param exyear
#'   Year to filter (numeric or a string representing an integer).
#' @param govsector
#'   One value from the `sector` column (e.g. `"Health"`).
#' @param budgetline
#'   One value from the `budget_line` column
#'   (e.g. `"Spending: Capital expenditures in health"`).
#'
#' @return
#'   A ggplot2 object.
#' @importFrom dplyr filter
#' @importFrom ggplot2 ggplot aes geom_rect geom_hline geom_point labs theme_minimal coord_flip theme scale_color_brewer ylim
#' @importFrom ggrepel geom_text_repel
#' @export


plot_budget_execution <- function(data,
                                  exyear,
                                  govsector,
                                  budgetline) {
  # coerce exyear to numeric if needed
  if (is.character(exyear)) {
    exyear2 <- suppressWarnings(as.numeric(exyear))
    if (is.na(exyear2)) {
      stop("`exyear` must be numeric or a string coercible to numeric.")
    }
  } else {
    exyear2 <- exyear
  }

  df <- data %>%
    filter(
      year        == exyear2,
      sector      == govsector,
      budget_line == budgetline
    )

  if (nrow(df) == 0) {
    warning("No data for year=", exyear2,
            ", sector='", govsector,
            "', budget_line='", budgetline, "'.")
  }

  ggplot(df, aes(x = region, y = execution_ratio, color = region)) +
    geom_rect(
      aes(xmin = -Inf, xmax = Inf, ymin = 80, ymax = 120),
      fill = "#d0ece7", alpha = 0.5, inherit.aes = FALSE
    ) +
    geom_hline(
      yintercept = c(50, 150),
      linetype   = "dotted",
      linewidth  = 1.5,
      alpha      = 0.25,
      color      = "darkred"
    ) +
    geom_hline(
      yintercept = 100,
      linetype   = "solid",
      linewidth  = 2,
      alpha      = 0.75,
      color      = "#117a65"
    ) +
    geom_point(size = 2) +
    geom_text_repel(
      aes(label = country_code),
      segment.size = 0,
      size         = 4,
      hjust        = 1
    ) +
    labs(
      x     = "Region",
      y     = "Budget Execution Rate (%)"
    )
}




