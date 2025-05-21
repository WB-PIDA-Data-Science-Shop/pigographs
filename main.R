# Main R Script

# load libraries

# run this only rhe first time you are running the code
# renv::restore()

library(here)

# Defining all the folders
dirs <- c(
  here("data-raw", "source"), # Data Cleaning Scripts (optional)
  here("analysis") # Data Analysis scripts
)

# helper to source every .R/.r file in a directory
# this will run the scripts in the right order starting with data-raw and
# then the analysis scripts

source_all <- function(dir) {
  scripts <- list.files(
    path       = dir,
    pattern    = "\\.[Rr]$",
    full.names = TRUE
  )
  invisible(lapply(scripts, source))
}

# Source all the scripts listed
invisible(lapply(dirs, source_all))
