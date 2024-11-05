source("dependencies.R")


# Load all required packages with a progress bar
pb <- utils::txtProgressBar(
  min = 0,
  max = length(required_packages),
  style = 3,
  title = "Loading Packages"
)
for (i in seq_along(required_packages)) {
  require(
    required_packages[i],
    character.only = TRUE,
    warn.conflicts = FALSE,
    quietly = TRUE
  ) |>
    suppressPackageStartupMessages()

  setTxtProgressBar(pb, i)
}
close(pb)

# ## Load the data

all_data <- qs::qread("data/all_data.qs")

runApp("app.R")
