# ============================================================
# R PACKAGE SETUP FOR STREAMLIT CLOUD / DEPLOYMENT
# Cebu City Dengue Forecasting App
#
# Purpose:
#   Installs only the R packages needed by predict_on_demand.R.
#   This version intentionally does NOT install sf/spdep to make deployment
#   faster and avoid GDAL-related failures.
# ============================================================

required_packages <- c(
  "readxl",
  "dplyr",
  "tidyr",
  "stringr",
  "janitor",
  "purrr",
  "xgboost",
  "lightgbm",
  "Matrix",
  "tibble",
  "zoo",
  "jsonlite"
)

user_lib <- Sys.getenv("R_LIBS_USER")

if (is.na(user_lib) || user_lib == "") {
  user_lib <- file.path(Sys.getenv("HOME"), "R", paste0("library-", paste(R.version$major, R.version$minor, sep = ".")))
  Sys.setenv(R_LIBS_USER = user_lib)
}

dir.create(user_lib, recursive = TRUE, showWarnings = FALSE)
.libPaths(c(user_lib, .libPaths()))

missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0) {
  install.packages(
    missing_packages,
    repos = "https://cloud.r-project.org",
    lib = user_lib,
    dependencies = TRUE
  )
}

still_missing <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(still_missing) > 0) {
  stop(
    paste0(
      "The following R packages could not be installed: ",
      paste(still_missing, collapse = ", ")
    )
  )
}

cat("R packages OK\n")
