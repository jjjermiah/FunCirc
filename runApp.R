library(shiny)

source("dependencies.R")
# Load all required packages
lapply(required_packages, library, character.only = TRUE)

## Load the data
load("data/all_data.RData")

runApp("app.R")
