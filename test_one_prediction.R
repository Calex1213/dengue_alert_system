# ============================================================
# TEST ONE PREDICTION
# This checks if predict_on_demand.R can produce JSON correctly.
# ============================================================

# ------------------------------------------------------------
# 1. Working directory and Rscript path
# ------------------------------------------------------------

setwd("C:/Users/Christopher/Downloads/ci_files")

R_SCRIPT_EXE <- "C:/Program Files/R/R-4.5.3/bin/Rscript.exe"

# ------------------------------------------------------------
# 2. Try to avoid broken renv/project library issues
# ------------------------------------------------------------

Sys.setenv(RENV_CONFIG_AUTOLOADER_ENABLED = "FALSE")
Sys.setenv(RENV_PROJECT = "NULL")

Sys.unsetenv("R_PROFILE_USER")
Sys.unsetenv("R_ENVIRON_USER")

.libPaths(.libPaths()[!grepl("renv", .libPaths(), ignore.case = TRUE)])

message("============================================================")
message("TEST ONE PREDICTION")
message("============================================================")
message("Working directory: ", getwd())
message("Rscript path: ", R_SCRIPT_EXE)
message("Library paths:")
print(.libPaths())
message("============================================================")

# ------------------------------------------------------------
# 3. Check files
# ------------------------------------------------------------

script_path <- file.path("r_scripts", "predict_on_demand.R")

if (!file.exists(script_path)) {
  stop(paste0(
    "Missing file: ", script_path, "\n",
    "Make sure predict_on_demand.R is inside the r_scripts folder."
  ))
}

if (!file.exists(R_SCRIPT_EXE)) {
  stop(paste0(
    "Rscript.exe was not found at: ", R_SCRIPT_EXE, "\n",
    "Check your R installation path."
  ))
}

# ------------------------------------------------------------
# 4. Run one prediction
# ------------------------------------------------------------

args <- c(
  "--vanilla",
  script_path,
  "--mode", "standard",
  "--level", "city",
  "--year", "2024",
  "--week", "52"
)

message("Running command:")
message(paste(shQuote(R_SCRIPT_EXE), paste(args, collapse = " ")))
message("============================================================")

raw_output <- system2(
  R_SCRIPT_EXE,
  args = args,
  stdout = TRUE,
  stderr = TRUE
)

message("Raw output:")
message("============================================================")
cat(paste(raw_output, collapse = "\n"))
message("\n============================================================")
message("TEST FINISHED")
message("============================================================")