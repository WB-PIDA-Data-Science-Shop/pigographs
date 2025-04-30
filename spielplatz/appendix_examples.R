


#
# plot_indicator_cluster <- function(data, indicator, cluster) {
#   data |>
#     filter(var_name    == indicator,
#            family_name == cluster) |>
#     ggplot(aes(x    = country_name,
#                y    = ctf_distance,
#                color = as.factor(region))) +
#       geom_segment(aes(xend = country_name, y = 0, yend = ctf_distance),
#                    linewidth = 1) +
#       geom_point(size = 2, alpha = 0.6) +
#       geom_hline(yintercept = 0,
#                  linetype    = "solid",
#                  linewidth   = 0.5,
#                  alpha       = 0.75,
#                  color       = "lightgrey") +
#       labs(
#         x     = "Country by Region",
#         y     = "Change in CTF Score",
#         title = paste("Indicator Differences for", indicator),
#         caption = "Source: CLIAR Dashboard"
#       ) +
#       facet_wrap(~region, scales = "free_y", nrow = 2) +
#       theme_minimal() +
#       theme(
#         axis.text.x      = element_text(size = 18, hjust = 0.5),
#         axis.text.y      = element_text(size = 16),
#         axis.title.y     = element_text(size = 20, face = "bold"),
#         axis.title.x     = element_text(size = 20, face = "bold"),
#         plot.title       = element_text(size = 22, face = "bold", hjust = 0.5),
#         plot.background  = element_blank(),
#         plot.caption     = element_text(hjust = 0, size = 10),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.border     = element_blank(),
#         legend.position  = "none",
#         strip.text       = element_text(size = 20)
#       ) +
#       scale_color_brewer(palette = "Paired") +
#       scale_y_continuous(limits = c(-1, 1), breaks = seq(-1, 1, by = 0.2)) +
#       coord_flip()
# }
#
# # 2. Build your combo table via unique()
# combos <- unique(
#   data[ , indicator, cluster ]
# )
#
# # 3. Loop through each pair and store plots
# all_plots <- list()
#
# for (i in seq_len(nrow(combos))) {
#   vn  <- combos$var_name[i]
#   fam <- combos$family_name[i]
#
#   p <- plot_indicator_cluster(data, vn, fam)
#
#   list_name <- paste0(
#     gsub("\\s+", "_", fam),
#     "__",
#     gsub("\\s+", "_", vn)
#   )
#   all_plots[[list_name]] <- p
# }
#
# # 4. (Optional) Print them all
# for (nm in names(all_plots)) {
#   print(all_plots[[nm]])
# }





# generate_indicator_diff_plots <- function(data, indicators) {
#   # Ensure `indicators` is a data frame with `var_name` and `family_name` columns
#   if (!all(c("var_name", "family_name") %in% colnames(indicators))) {
#     stop("The `indicators` data frame must contain `var_name` and `family_name` columns.")
#   }
#
#   # Loop through each indicator and generate plots
#   for (i in seq_len(nrow(indicators))) {
#     var_name <- indicators$var_name[i]
#     family_name <- indicators$family_name[i]
#
#     # Ensure `var_name` exists in the data
#     if (!var_name %in% colnames(data)) {
#       warning(paste("The column", var_name, "does not exist in the data. Skipping..."))
#       next
#     }
#
#     # Convert `var_name` to a symbol
#     indicator <- rlang::ensym(var_name)
#
#     # Generate the plot
#     indicator_plot <- data |>
#       ggplot(aes(x = country_name, y = !!indicator, color = as.factor(region))) +
#       geom_segment(aes(xend = country_name, y = 0, yend = !!indicator), linewidth = 1) +
#       geom_point(size = 2, alpha = 0.6) +
#       geom_hline(
#         yintercept = 0,
#         linetype = "solid",
#         linewidth = 0.5,
#         alpha = 0.75,
#         color = "lightgrey"
#       ) +
#       labs(
#         x = "Country by Region",
#         y = "Change in CTF Score",
#         title = paste("Indicator Differences for", var_name),
#         caption = "Source: CLIAR Dashboard"
#       ) +
#       facet_wrap(~region, scales = "free_y", nrow = 2) +
#       theme_minimal() +
#       theme(
#         axis.text.x = element_text(size = 18, hjust = 0.5),
#         axis.text.y = element_text(size = 16),
#         axis.title.y = element_text(size = 20, face = "bold"),
#         axis.title.x = element_text(size = 20, face = "bold"),
#         plot.title = element_text(size = 22, face = "bold", hjust = 0.5),
#         plot.background = element_blank(),
#         plot.caption = element_text(hjust = 0, size = 10),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.border = element_blank(),
#         legend.position = "none",
#         strip.text = element_text(size = 20)
#       ) +
#       scale_color_brewer(palette = "Paired") +
#       scale_y_continuous(limits = c(-1, 1), breaks = seq(-1, 1, by = 0.2)) +
#       coord_flip()
#
#     # Generate a safe filename and output directory
#     safe_var_name <- gsub("[^A-Za-z0-9]", "_", var_name)
#     safe_family_name <- gsub("[^A-Za-z0-9]", "_", family_name)
#     output_dir <- here("figures", safe_family_name)
#
#     # Ensure the output directory exists
#     if (!dir.exists(output_dir)) {
#       dir.create(output_dir, recursive = TRUE)
#     }
#
#     # Save the plot
#     ggsave(
#       filename = file.path(output_dir, paste0("over_time_indicator_", safe_var_name, ".png")),
#       plot = indicator_plot,
#       bg = "white",
#       width = 20,
#       height = 16
#     )
#
#     message(paste("Saved plot for", var_name, "in folder", family_name))
#   }
# }


# generate_indicator_diff_plot <- function(data, cluster){
#  indicator_plot <-  data |>
#     group_by(cluster) |>
#     ggplot(aes(x = country_name, y = ctf_distance, color = as.factor(region))) +
#     geom_segment(aes(xend = country_name, y = 0, yend = ctf_distance), linewidth = 1) +
#     geom_point(size = 2, alpha = 0.6) +
#     geom_hline(yintercept = 0,
#                linetype = "solid",
#                linewidth = .5,
#                alpha = 0.75,
#                color =  "lightgrey") +
#     labs(
#       x = "Country by Region",
#       y = "Change in CTF Score"
#     ) +
#     facet_wrap(~region, scales = "free_y", nrow = 2) +
#     theme_minimal() +
#     theme(
#       axis.text.x = element_text(size = 18, hjust = .5),
#       axis.text.y = element_text(size = 16),
#       axis.title.y = element_text(size = 20, face = "bold"),
#       axis.title.x = element_text(size = 20, face = "bold"),
#       plot.background = element_blank(),
#       plot.caption = element_text(hjust = 0, size = 10),
#       panel.grid.major = element_blank(),
#       panel.grid.minor = element_blank(),
#       panel.border = element_blank(),
#       legend.position = "none",
#       strip.text = element_text(size = 20)
#     ) +
#     scale_color_brewer(palette = "Paired") +
#     scale_y_continuous(limits = c(-1, 1), breaks = c(-1,1), by = 0.25) +
#     coord_flip()
#
#   # Save the plot with a unique filename based on the budget line
#   ggsave(filename = here("figures",as_string(cluster), paste0("over_time_indicator", gsub(" ", "_", ), ".png")),
#          plot = inticator_plot, bg = "white", width = 20, height = 16)
#
#
# }



# generate_bud_execution_plot <- function(data, budget_line, footnote) {
#   # Filter data by the specified budget line and year >= 2015
#   filtered_data <- data |>
#     filter(budget_line == budget_line) |>
#     filter(year >= 2015) |>
#     group_by(year, region) |>
#     summarise(
#       execution_ratio = mean(ex_ratio, na.rm = TRUE),
#       .groups = 'drop'
#     )
#
#   # Create the plot
#   pp <- ggplot(filtered_data, aes(x = year, y = execution_ratio, color = region, group = region)) +
#     # Highlight the 100% execution zone
#     geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 95, ymax = 105),
#               fill = "#ac82d7", alpha = 0.2, inherit.aes = FALSE) +
#     annotate("text",
#              x = Inf,
#              y = 115,
#              label = "100% Execution Zone",
#              hjust = 1.1,
#              vjust = 2,
#              color = "#ac82d7",
#              size = 3) +
#     # Line and points for the execution ratio by region
#     geom_line(linewidth = 1.5) +
#     geom_point(size = 3) +
#     labs(
#       title = paste(budget_line, "Execution Rate (%) by Region"),
#       subtitle = "Regional Averages (2015-2022)",
#       x = "Year",
#       y = "Regional Execution Rate (%)",
#       caption = footnote
#     ) +
#     theme_minimal() +
#     theme(
#       text = element_text(family = "Segoe UI Semibold"),
#       axis.text.x = element_text(size = 11, hjust = 0.5),
#       axis.text.y = element_text(size = 11, hjust = 0.5),
#       plot.title = element_text(size = 16, face = "bold"),
#       plot.subtitle = element_text(size = 14),
#       plot.background = element_blank(),
#       plot.caption = element_text(hjust = 0, size = 10),
#       panel.grid.major = element_blank(),
#       panel.grid.minor = element_blank(),
#       panel.border = element_blank(),
#       legend.position = "right"
#     ) +
#     scale_x_continuous(
#       breaks = function(x) seq(from = 2015, to = 2022, by = 1)
#     ) +
#     scale_color_brewer(palette = "Paired") +
#     ylim(50, 150) +
#     guides(color = guide_legend(title = "Region"))
#
#   # # Save the plot with a unique filename based on the budget line
#   # ggsave(filename = here("figs", "boost", "budget_predictability", paste0("region_execution_plot_", gsub(" ", "_", budget_line), ".png")),
#   #        plot = pp, bg = "white", width = 10, height = 6)

