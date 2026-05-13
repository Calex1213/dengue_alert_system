# ============================================================
# PRECOMPUTE FORECAST JSON FILES FOR STREAMLIT DEPLOYMENT
# Cebu City Dengue Early Warning System
#
# Run this locally from PowerShell:
#
# cd "C:\Users\Christopher\Downloads\ci_files"
# & "C:\Program Files\R\R-4.5.3\bin\Rscript.exe" --vanilla precompute_forecasts.R
#
# This script calls:
#   r_scripts/predict_on_demand.R
#
# It saves files to:
#   outputs/{mode}_{level}_{year}_w{week}.json
#
# Example:
#   outputs/standard_city_2024_w52.json
#   outputs/standard_barangay_2024_w52.json
#   outputs/environmental_only_city_2024_w52.json
#   outputs/environmental_only_barangay_2024_w52.json
# ============================================================


# ============================================================
# 1. FORCE LOCAL WORKING DIRECTORY AND CLEAN SESSION
# ============================================================

setwd("C:/Users/Christopher/Downloads/ci_files")

R_SCRIPT_EXE <- "C:/Program Files/R/R-4.5.3/bin/Rscript.exe"

Sys.setenv(RENV_CONFIG_AUTOLOADER_ENABLED = "FALSE")
Sys.setenv(RENV_PROJECT = "NULL")

Sys.unsetenv("R_PROFILE_USER")
Sys.unsetenv("R_ENVIRON_USER")

.libPaths(.libPaths()[!grepl("renv", .libPaths(), ignore.case = TRUE)])

message("============================================================")
message("PRECOMPUTE FORECASTS")
message("============================================================")
message("Working directory: ", getwd())
message("Rscript path: ", R_SCRIPT_EXE)
message("Library paths:")
print(.libPaths())
message("============================================================")


# ============================================================
# 2. SETTINGS YOU CAN EDIT
# ============================================================

MODES <- c("standard", "environmental_only")
LEVELS <- c("city", "barangay")

# Choose one:
#   "all"     = precompute every available year-week found in FINAL_DATASET.xlsx
#   "latest"  = precompute only the latest available year-week
#   "custom"  = precompute only CUSTOM_YEARS and CUSTOM_WEEKS below
PRECOMPUTE_SCOPE <- "custom"

# Used only if PRECOMPUTE_SCOPE <- "custom"
CUSTOM_YEARS <- 2023:2024
CUSTOM_WEEKS <- 1:52

# If TRUE, early weeks in the first year are removed because lag features may be unavailable.
DROP_EARLY_FIRST_YEAR_WEEKS <- TRUE
MIN_FIRST_YEAR_WEEK <- 30

# If TRUE, do not regenerate JSON files that already exist.
SKIP_EXISTING <- TRUE

DATASET_PATH <- file.path("data", "FINAL_DATASET.xlsx")
SCRIPT_PATH <- file.path("r_scripts", "predict_on_demand.R")
OUTPUT_DIR <- "outputs"
LOG_PATH <- file.path(OUTPUT_DIR, "precompute_log.csv")


# ============================================================
# 3. PACKAGE CHECKS
# ============================================================

required_packages <- c("readxl", "dplyr")

missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0) {
  stop(
    paste0(
      "Missing R package(s): ", paste(missing_packages, collapse = ", "), "\n",
      "Install them first using:\n",
      "install.packages(c(",
      paste(sprintf('\"%s\"', missing_packages), collapse = ", "),
      "))"
    )
  )
}


# ============================================================
# 4. BASIC FILE CHECKS
# ============================================================

if (!file.exists(R_SCRIPT_EXE)) {
  stop(paste0(
    "Rscript.exe was not found at: ", R_SCRIPT_EXE, "\n",
    "Check your R installation path."
  ))
}

if (!file.exists(DATASET_PATH)) {
  stop(paste0("Missing dataset: ", DATASET_PATH))
}

if (!file.exists(SCRIPT_PATH)) {
  stop(
    paste0(
      "Missing file: ", SCRIPT_PATH, "\n",
      "Make sure predict_on_demand.R is inside the r_scripts folder."
    )
  )
}

dir.create(OUTPUT_DIR, showWarnings = FALSE, recursive = TRUE)


# ============================================================
# 5. READ AVAILABLE YEAR-WEEK VALUES
# ============================================================

message("Reading available year-week values from: ", DATASET_PATH)

available_weeks <- readxl::read_excel(DATASET_PATH)

names(available_weeks) <- tolower(gsub(" ", "_", trimws(names(available_weeks))))

if (!("year" %in% names(available_weeks)) || !("week" %in% names(available_weeks))) {
  stop("FINAL_DATASET.xlsx must have year and week columns.")
}

available_weeks <- available_weeks |>
  dplyr::select(year, week) |>
  dplyr::filter(!is.na(year), !is.na(week)) |>
  dplyr::mutate(
    year = as.integer(year),
    week = as.integer(week)
  ) |>
  dplyr::distinct(year, week) |>
  dplyr::arrange(year, week)

if (DROP_EARLY_FIRST_YEAR_WEEKS && nrow(available_weeks) > 0) {
  first_year <- min(available_weeks$year, na.rm = TRUE)

  available_weeks <- available_weeks |>
    dplyr::filter(!(year == first_year & week < MIN_FIRST_YEAR_WEEK))
}

if (PRECOMPUTE_SCOPE == "latest") {
  weeks_to_run <- available_weeks |>
    dplyr::arrange(dplyr::desc(year), dplyr::desc(week)) |>
    dplyr::slice(1)
} else if (PRECOMPUTE_SCOPE == "custom") {
  weeks_to_run <- available_weeks |>
    dplyr::filter(year %in% CUSTOM_YEARS, week %in% CUSTOM_WEEKS) |>
    dplyr::arrange(year, week)
} else if (PRECOMPUTE_SCOPE == "all") {
  weeks_to_run <- available_weeks
} else {
  stop("PRECOMPUTE_SCOPE must be one of: 'all', 'latest', or 'custom'.")
}

if (nrow(weeks_to_run) == 0) {
  stop("No year-week combinations selected. Check PRECOMPUTE_SCOPE, CUSTOM_YEARS, and CUSTOM_WEEKS.")
}

message("Selected ", nrow(weeks_to_run), " origin week(s).")
message("Total JSON files expected: ", nrow(weeks_to_run) * length(MODES) * length(LEVELS))
message("============================================================")


# ============================================================
# 6. HELPER FUNCTIONS
# ============================================================

extract_json_object <- function(text) {
  if (length(text) == 0 || !nzchar(paste(text, collapse = "\n"))) {
    stop("No output was returned by predict_on_demand.R")
  }

  full_text <- paste(text, collapse = "\n")

  chars <- strsplit(full_text, "", fixed = TRUE)[[1]]
  start <- which(chars == "{")[1]

  if (is.na(start)) {
    stop(paste0("No JSON object found in output:\n", full_text))
  }

  depth <- 0L
  in_string <- FALSE
  escape <- FALSE

  for (i in seq.int(start, length(chars))) {
    ch <- chars[[i]]

    if (in_string) {
      if (escape) {
        escape <- FALSE
      } else if (ch == "\\") {
        escape <- TRUE
      } else if (ch == '"') {
        in_string <- FALSE
      }
    } else {
      if (ch == '"') {
        in_string <- TRUE
      } else if (ch == "{") {
        depth <- depth + 1L
      } else if (ch == "}") {
        depth <- depth - 1L

        if (depth == 0L) {
          return(substr(full_text, start, i))
        }
      }
    }
  }

  stop(paste0("Incomplete JSON object in output:\n", full_text))
}


make_output_file <- function(mode, level, year, week) {
  file.path(
    OUTPUT_DIR,
    paste0(mode, "_", level, "_", year, "_w", week, ".json")
  )
}


append_log <- function(record) {
  record_df <- as.data.frame(record, stringsAsFactors = FALSE)

  if (!file.exists(LOG_PATH)) {
    utils::write.csv(record_df, LOG_PATH, row.names = FALSE)
  } else {
    utils::write.table(
      record_df,
      LOG_PATH,
      sep = ",",
      row.names = FALSE,
      col.names = FALSE,
      append = TRUE
    )
  }
}


run_one_prediction <- function(mode, level, year, week, output_file) {
  args <- c(
    "--vanilla",
    SCRIPT_PATH,
    "--mode", mode,
    "--level", level,
    "--year", as.character(year),
    "--week", as.character(week)
  )

  message("Running command:")
  message(paste(shQuote(R_SCRIPT_EXE), paste(args, collapse = " ")))

  raw_output <- system2(
    R_SCRIPT_EXE,
    args = args,
    stdout = TRUE,
    stderr = TRUE
  )

  json_text <- extract_json_object(raw_output)

  writeLines(json_text, output_file, useBytes = TRUE)

  return(TRUE)
}


# ============================================================
# 7. RUN PREDICTIONS AND SAVE JSON FILES
# ============================================================

start_time <- Sys.time()

completed <- 0L
skipped <- 0L
failed <- 0L

planned <- nrow(weeks_to_run) * length(MODES) * length(LEVELS)

for (mode in MODES) {
  for (i in seq_len(nrow(weeks_to_run))) {
    year <- weeks_to_run$year[[i]]
    week <- weeks_to_run$week[[i]]

    for (level in LEVELS) {
      output_file <- make_output_file(mode, level, year, week)

      if (SKIP_EXISTING && file.exists(output_file)) {
        skipped <- skipped + 1L
        message("[SKIP] ", output_file)

        append_log(list(
          timestamp = as.character(Sys.time()),
          mode = mode,
          level = level,
          year = year,
          week = week,
          status = "skipped_existing",
          output_file = output_file,
          message = "Already exists"
        ))

        next
      }

      message("============================================================")
      message("[RUN] ", mode, " | ", level, " | ", year, " W", week)
      message("Output file: ", output_file)
      message("============================================================")

      one_start <- Sys.time()

      result <- tryCatch({
        run_one_prediction(
          mode = mode,
          level = level,
          year = year,
          week = week,
          output_file = output_file
        )

        list(ok = TRUE, message = "Saved")
      }, error = function(e) {
        list(ok = FALSE, message = conditionMessage(e))
      })

      elapsed_seconds <- round(
        as.numeric(difftime(Sys.time(), one_start, units = "secs")),
        2
      )

      if (isTRUE(result$ok)) {
        completed <- completed + 1L

        message("[OK] ", output_file, " (", elapsed_seconds, " sec)")

        append_log(list(
          timestamp = as.character(Sys.time()),
          mode = mode,
          level = level,
          year = year,
          week = week,
          status = "success",
          output_file = output_file,
          message = paste0("Elapsed seconds: ", elapsed_seconds)
        ))
      } else {
        failed <- failed + 1L

        message("[FAIL] ", output_file)
        message(result$message)

        append_log(list(
          timestamp = as.character(Sys.time()),
          mode = mode,
          level = level,
          year = year,
          week = week,
          status = "failed",
          output_file = output_file,
          message = result$message
        ))
      }

      message(
        "Progress: ", completed + skipped + failed, "/", planned,
        " | success=", completed,
        " | skipped=", skipped,
        " | failed=", failed
      )
    }
  }
}


# ============================================================
# 8. FINAL SUMMARY
# ============================================================

elapsed_total <- round(
  as.numeric(difftime(Sys.time(), start_time, units = "mins")),
  2
)

message("============================================================")
message("Precompute finished.")
message("Successful files: ", completed)
message("Skipped existing: ", skipped)
message("Failed files: ", failed)
message("Total planned: ", planned)
message("Total time: ", elapsed_total, " minutes")
message("Outputs folder: ", OUTPUT_DIR)
message("Log file: ", LOG_PATH)
message("============================================================")

if (failed > 0) {
  message("Some files failed. Check outputs/precompute_log.csv for details.")
  quit(status = 1)
}