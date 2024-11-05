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
  "gtools",
  "gridExtra",
  "qs"
)

# Install missing packages using pak
install_if_missing <- function(packages) {
  if (!requireNamespace("pak", quietly = TRUE)) {
    install.packages("pak", repos = "https://cloud.r-project.org/")
  }
  pak::pkg_install(packages)
}

# Install only missing packages
install_if_missing(required_packages)

# Load all required packages
lapply(required_packages, library, character.only = TRUE)

# Ensure working directory is set to the FunCirc directory
# setwd("path/to/FunCirc") 

## Load the data
all_data <- qs::qread("data/all_data.qs")
