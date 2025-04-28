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
      # region = recode(region,
      #                 "Europe & Central Asia" = "ECA",
      #                 "East Asia & Pacific" = "EAP",
      #                 "Latin America & Caribbean" = "LAC",
      #                 "Middle East & North Africa" = "MENA",
      #                 "South Asia" = "SAR",
      #                 "Sub-Saharan Africa" = "SSA"),
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

generate_labels_plot <- function(data, dimension) {
  # Filter data for the specific dimension
  filtered_data <- data |>
    filter(dimension == !!dimension) |>   # Use !! to inject the parameter dynamically
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
                    size = 4, # Font size ## More info https://ggplot2.tidyverse.org/reference/geom_text.html
                    nudge_x = -0.3, # Left aligned
                    segment.color = NA,   # Delete connectors or arrows
                    box.padding = 0.15,    # Min Space between tags
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


