# Install packages needed by predict_on_demand.R in your normal R library.
# Run this only after disabling/renaming the broken renv folder if needed.
setwd("C:/Users/Christopher/Downloads/ci_files")
Sys.setenv(RENV_CONFIG_AUTOLOADER_ENABLED = "FALSE")
Sys.setenv(RENV_PROJECT = "NULL")
.libs <- .libPaths()
.libs <- .libs[!grepl("renv", .libs, ignore.case = TRUE)]
if (length(.libs) > 0) .libPaths(.libs)

packages <- c(
  "readxl", "dplyr", "tidyr", "stringr", "janitor", "purrr",
  "tibble", "zoo", "jsonlite", "Matrix", "xgboost", "lightgbm"
)

missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing) > 0) {
  install.packages(missing, repos = "https://cloud.r-project.org")
} else {
  message("All required packages are already installed.")
}
