
# set-up ------------------------------------------------------------------
library(haven)
library(dplyr)
library(stringi)
library(here)
library(tidyverse)
library(ggplot2)
library(janitor)
library(reshape2)
library(scales)
library(stringr)
library(readr)
library(readxl)
library(labelled)
library(cowplot)
library(openxlsx)
library(RColorBrewer)
library(countrycode)
library(extrafont)
library(devtools)
library(ggrepel)
library(forcats)
library(patchwork)
library(purrr)

devtools::load_all()

theme_set(
  theme_minimal(base_family = "Segoe UI Semilight") +
    theme(
      axis.text.x = element_text(size = 10, hjust = .5),
      axis.text.y = element_text(size = 9),
      plot.title = element_text(size = 16, face = "bold"),
      plot.subtitle = element_text(size = 12),
      plot.background = element_blank(),
      plot.caption = element_text(hjust = 0, size = 10),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      legend.position = "top"
    )
)



# data-load ---------------------------------------------------------------

indicator_distance <- indicator_cluster_distance #Package dataframe


# indicators-visualizations -------------------------------------------------

plot_cluster_appendix(indicator_distance,
                      "degree of integrity",
                      "2018-2022")


plot_cluster_appendix(indicator_distance,
                      "public human resource management institutions",
                      "2018-2022")


plot_cluster_appendix(indicator_distance,
                      "political institutions",
                      "2018-2022")


plot_cluster_appendix(indicator_distance,
                      "social institutions",
                      "2018-2022")


plot_cluster_appendix(indicator_distance,
                      "justice institutions",
                      "2018-2022")

plot_cluster_appendix(indicator_distance,
                      "energy and environment institutions",
                      "2016-2020" # Change time frame to 2016 - 2020
                      )


