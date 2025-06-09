
#' Compute Min / Average / Max statistics for a given dimension,
#' grouped by an arbitrary variable (e.g. region, income level).
#'
#' @param data         A data frame with at least country_name, the
#'                     dimension column, and the grouping column.
#' @param dimension    Unquoted column name of the indicator
#'                     you want to summarise (e.g. pfm_inst).
#' @param grouping_var Unquoted column name to group by
#'                     (default = region, but you can pass income_level, etc.).
#'
#' @return A tidy data frame with one row per country–type combination,
#'         where type ∈ {Min, Average, Max}.
#' @import dplyr tidyr rlang countrycode
#' @export
compute_group_statistics <- function(data,
                                     dimension,
                                     grouping_var = region) {

  # tidy-eval helpers
  dimension    <- rlang::ensym(dimension)
  grouping_var <- rlang::ensym(grouping_var)

  # custom ISO3 fixes
  custom_mappings <- c(
    "Channel Islands" = "CHI",
    "Kosovo"          = "XKX"
  )

  data |>
    dplyr::select(country_name, !!dimension, !!grouping_var) |>
    dplyr::group_by(!!grouping_var) |>
    dplyr::mutate(
      group_av = ifelse(all(is.na(!!dimension)), NA, mean(!!dimension, na.rm = TRUE)),
      max_av   = ifelse(all(is.na(!!dimension)), NA, max(!!dimension, na.rm = TRUE)),
      min_av   = ifelse(all(is.na(!!dimension)), NA, min(!!dimension, na.rm = TRUE))
    ) |>
    dplyr::ungroup() |>
    tidyr::pivot_longer(
      cols      = c(min_av, group_av, max_av),
      names_to  = "type",
      values_to = "value"
    ) |>
    dplyr::mutate(
      type = dplyr::recode(type,
                           min_av   = "Min",
                           group_av = "Average",
                           max_av   = "Max")
    ) |>
    # add overall min / max per group for later labelling if needed
    dplyr::left_join(
      data |>
        dplyr::group_by(!!grouping_var) |>
        dplyr::summarise(
          min_value = ifelse(all(is.na(!!dimension)), NA, min(!!dimension, na.rm = TRUE)),
          max_value = ifelse(all(is.na(!!dimension)), NA, max(!!dimension, na.rm = TRUE)),
          .groups   = "drop"
        ),
      by = rlang::as_name(grouping_var)
    ) |>
    dplyr::mutate(
      country_code = countrycode::countrycode(
        country_name,
        origin      = "country.name",
        destination = "iso3c",
        custom_match = custom_mappings
      )
    ) |>
    dplyr::rename(
      country_dimension_av = !!dimension,
      group_long           = !!grouping_var
    )
}


#' Plot Min / Average / Max statistics for one dimension,
#' grouped by an arbitrary variable (region, income_level, …).
#'
#' @param data          A data frame that already contains:
#'                      • `type`       – "Min", "Average", "Max"
#'                      • `value`      – numeric to plot
#'                      • `min_value`, `max_value`, `country_dimension_av`
#'                      and any grouping column you want to show on the x-axis.
#' @param grouping_var  Unquoted column name to use as the groups on the x-axis
#'                      (default = region_long, but you can pass income_level, etc.).
#'
#' @return A ggplot object.
#' @import ggplot2 dplyr rlang ggrepel
#' @export
generate_group_minmax_plot <- function(data,
                                       grouping_var = group_long,
                                       wrap_width   = 15) {

  # 1. Capture the grouping column, quoted or un-quoted
  grouping_var <- rlang::enquo(grouping_var)

  # 2. Filter & add a wrapped label
  filtered_data <- data |>
    dplyr::filter(!is.na(!!grouping_var)) |>
    dplyr::mutate(
      group_w = stringr::str_wrap(as.character(!!grouping_var),
                                  width = wrap_width)
    )

  # 3. Build the plot
  p <- ggplot2::ggplot(
    filtered_data,
    ggplot2::aes(x = group_w, y = value,
                 shape = type, colour = group_w)
  ) +
    ggplot2::geom_point(size = 8) +
    ggplot2::geom_segment(
      ggplot2::aes(x = group_w, xend = group_w,
                   y = min_value, yend = max_value),
      linetype = "solid", linewidth = 1.5
    ) +
    ggplot2::geom_hline(yintercept = khm_average_value,
                        linetype = "solid",
                        color = "grey",
                        alpha = 0.5,
                        linewidth = 4,
                        nudge_x = -0.3
    ) +
    ggrepel::geom_text_repel(
      data = filtered_data |> dplyr::filter(type == "Average"),
      ggplot2::aes(x = group_w, y = country_dimension_av,
                   label = country_code, colour = group_w),
      size = 4, nudge_x = -0.3, segment.color = NA,
      box.padding = 0.15, show.legend = FALSE
    ) +
    ggplot2::scale_shape_manual(
      values = c(Min = 19, Max = 19), drop = FALSE
    )

  invisible(p)
}


