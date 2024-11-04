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
  "gridExtra"
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
