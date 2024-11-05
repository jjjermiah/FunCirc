# Define the required packages
required_packages <- c(
  "shiny",
  "shinydashboard",
  "shinyWidgets",
  "dplyr",
  "tidyr",
  "data.table",
  "DT",
  "ggplot2",
  "BoutrosLab.plotting.general",
  "janitor",
  "readxl",
  "rtracklayer",
  "tibble",
  "GenomicRanges",
  "ggrepel",
  "qs"
)

# Install missing packages
install_if_missing <- function(packages) {
  to_install <- packages[!packages %in% installed.packages()[, "Package"]]
  if (length(to_install) > 0) {
    install.packages(to_install)
  }
}

# Install only missing packages
install_if_missing(required_packages)

# Load all required packages
lapply(required_packages, library, character.only = TRUE)

# Ensure working directory is set to the FunCirc directory
# setwd("path/to/FunCirc") 

## Load the data
all_data <- qs::qread("data/all_data.qs")
