# ============================================================
# ON-DEMAND PREDICTION BRIDGE FOR STREAMLIT APP
# Cebu City Dengue Early Warning System
#
# Run from the main DENGUE_FINAL_APP_V2 folder:
#   Rscript r_scripts/predict_on_demand.R --mode standard --level barangay --year 2024 --week 30
#
# Modes:
#   --mode standard | environmental_only
#   --level barangay | city
# ============================================================

# -------------------------
# 0. Quiet package loading
# -------------------------
# IMPORTANT FOR DEPLOYMENT:
# This script does NOT install packages at runtime.
# Streamlit Cloud should install R packages through r_packages_setup.R.
# The app intentionally avoids sf/spdep here for speed and deployment stability.

needed_packages <- c(
  "readxl", "dplyr", "tidyr", "stringr", "janitor", "purrr",
  "tibble", "zoo", "jsonlite", "Matrix", "xgboost", "lightgbm"
)

missing_packages <- needed_packages[
  !vapply(needed_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0) {
  stop(
    paste0(
      "Missing required R package(s): ",
      paste(missing_packages, collapse = ", "),
      ". Install these before running the app."
    )
  )
}

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(janitor)
  library(purrr)
  library(tibble)
  library(zoo)
  library(jsonlite)
  library(Matrix)
  library(xgboost)
  library(lightgbm)
})

has_lightgbm <- TRUE

# -------------------------
# 1. Constants
# -------------------------
DATA_PATH <- file.path("data", "FINAL_DATASET.xlsx")
SHAPE_ZIP_PATH <- file.path("data", "cebu_city_barangays.zip")
MODELS_DIR <- "models"
METADATA_DIR <- "model_metadata"
CACHE_DIR <- file.path("model_metadata", "runtime_cache")
dir.create(CACHE_DIR, showWarnings = FALSE, recursive = TRUE)

DISPLAY_HORIZONS <- c(0, 1, 2, 3, 4, 8, 12)
TRAIN_END_YEAR <- 2022
MAX_LAG_TO_TEST <- 20
ENV_ROLL_WINDOWS <- c(4, 8)
POP_DENSITY_UNIT <- "per_km2"
OUTBREAK_COUNT_THRESHOLD <- 2
LARGE_OUTBREAK_THRESHOLD <- 5
CITY_OUTBREAK_THRESHOLD_METHOD <- "train_quantile"
CITY_OUTBREAK_QUANTILE <- 0.75
CITY_LARGE_OUTBREAK_QUANTILE <- 0.90
CITY_FIXED_OUTBREAK_THRESHOLD_CASES <- 100
CITY_FIXED_LARGE_OUTBREAK_THRESHOLD_CASES <- 200
SOFT_ALPHA <- 0.50

area_text <- "
barangay,barangay_area_m2
Adlaon,10568289.49
Agsungot,3141145.937
Apas,1924622.783
Babag,9127305.301
Basak Pardo,2280016.225
Bacayan,1247466.647
Banilad,2361932.924
Basak San Nicolas,1482442.104
Binaliw,5160478.223
Bonbon,9335589.991
Budla-an (Pob.),5528415.047
Buhisan,7362349.884
Bulacao,4458308.598
Buot-Taup Pardo,6309902.571
Busay (Pob.),9073669.846
Calamba,461260.8167
Cambinocot,5404029.229
Capitol Site (Pob.),794266.9439
Carreta,944504.2462
Central (Pob.),295179.9903
Cogon Ramos (Pob.),295106.1074
Cogon Pardo,1534001.407
Day-as,106105.5372
Duljo (Pob.),403621.5959
Ermita (Pob.),379645.5962
Guadalupe,7364428.349
Guba,10821331.27
Hippodromo,430684.893
Inayawan,2753267.116
Kalubihan (Pob.),161379.8344
Kalunasan,1420118.552
Kamagayan (Pob.),117092.5693
Camputhaw (Pob.),1215505.455
Kasambagan,1832657.268
Kinasang-an Pardo,1509429.505
Labangon,1122041.116
Lahug (Pob.),4239264.538
Lorega (Lorega San Miguel),199630.1232
Lusaran,11205979.19
Luz,560240.7135
Mabini,5611102.651
Mabolo,1946308.241
Malubog,8555676.09
Mambaling,1249056.385
Pahina Central (Pob.),257598.4505
Pahina San Nicolas,75403.24046
Pamutan,11375185.98
Pardo (Pob.),2034673.842
Pari-an,97305.23716
Paril,3223325.36
Pasil,79951.38113
Pit-os,1653568.117
Pulangbato,5234284.92
Pung-ol-Sibugay,14616354.74
Punta Princesa,1276947.168
Quiot Pardo,761372.7629
Sambag I (Pob.),516257.1623
Sambag II (Pob.),447417.6939
San Antonio (Pob.),128340.3764
San Jose,2869390.311
San Nicolas Central,281010.9574
San Roque (Ciudad),455039.6854
Santa Cruz (Pob.),229062.3109
Sawang Calero (Pob.),242699.2115
Sinsin,8122060.059
Sirao,7704545.516
Suba Pob. (Suba San Nicolas),96693.99165
Sudlon I,3206182.493
Sapangdaku,8393868.225
T. Padilla,165106.4826
Tabunan,15861577.93
Tagbao,10534037.02
Talamban,4629709.911
Taptap,7570183.845
Tejero (Villa Gonzalo),538422.355
Tinago,229802.9203
Tisa,2436056.449
To-ong Pardo,6814618.803
Zapatera,335615.5369
Sudlon II,16116531.07
"


area_df <- read.csv(text = area_text, stringsAsFactors = FALSE) %>%
  mutate(barangay_clean = toupper(str_squish(barangay))) %>%
  select(barangay_clean, barangay_area_m2)

# -------------------------
# 2. Command-line args
# -------------------------
parse_args <- function() {
  args <- commandArgs(trailingOnly = TRUE)
  out <- list(mode = NULL, level = NULL, year = NULL, week = NULL)
  i <- 1
  while (i <= length(args)) {
    key <- args[[i]]
    val <- if (i + 1 <= length(args)) args[[i + 1]] else NA_character_
    if (key == "--mode") out$mode <- val
    if (key == "--level") out$level <- val
    if (key == "--year") out$year <- as.integer(val)
    if (key == "--week") out$week <- as.integer(val)
    i <- i + 2
  }
  if (is.null(out$mode) || !(out$mode %in% c("standard", "environmental_only"))) {
    stop("Missing/invalid --mode. Use standard or environmental_only.")
  }
  if (is.null(out$level) || !(out$level %in% c("barangay", "city"))) {
    stop("Missing/invalid --level. Use barangay or city.")
  }
  if (is.null(out$year) || is.na(out$year)) stop("Missing/invalid --year.")
  if (is.null(out$week) || is.na(out$week)) stop("Missing/invalid --week.")
  out
}

# -------------------------
# 3. Helpers
# -------------------------
sum_safe <- function(x) sum(as.numeric(x), na.rm = TRUE)
weighted_mean_safe <- function(x, w) {
  x <- as.numeric(x); w <- as.numeric(w)
  ok <- !is.na(x) & !is.na(w) & w > 0
  if (sum(ok) == 0) return(NA_real_)
  weighted.mean(x[ok], w[ok], na.rm = TRUE)
}

cases_to_loginc <- function(cases, estimated_population) {
  incidence <- (cases / estimated_population) * 10000
  incidence <- ifelse(is.na(incidence) | incidence < 0, 0, incidence)
  log1p(incidence)
}

loginc_to_cases <- function(pred_log_incidence, estimated_population) {
  pred_incidence <- expm1(pred_log_incidence)
  pred_cases <- (pred_incidence / 10000) * estimated_population
  pred_cases <- ifelse(is.na(pred_cases), 0, pred_cases)
  pmax(pred_cases, 0)
}

make_lags <- function(data, cols, lags, group_col = "barangay") {
  out <- data
  for (cc in cols) {
    if (cc %in% names(out)) {
      for (ll in lags) {
        new_name <- paste0(cc, "_lag", ll)
        out <- out %>%
          group_by(.data[[group_col]]) %>%
          arrange(year, week, .by_group = TRUE) %>%
          mutate(!!new_name := dplyr::lag(.data[[cc]], ll)) %>%
          ungroup()
      }
    }
  }
  out
}

make_rolls <- function(data, cols, windows, group_col = "barangay") {
  out <- data
  for (cc in cols) {
    if (cc %in% names(out)) {
      for (ww in windows) {
        new_name <- paste0(cc, "_rollmean_lag1_w", ww)
        out <- out %>%
          group_by(.data[[group_col]]) %>%
          arrange(year, week, .by_group = TRUE) %>%
          mutate(
            !!new_name := zoo::rollapplyr(
              dplyr::lag(.data[[cc]], 1),
              width = ww,
              FUN = function(z) mean(z, na.rm = TRUE),
              fill = NA_real_,
              partial = FALSE
            )
          ) %>%
          ungroup()
      }
    }
  }
  out
}

make_city_lags <- function(data, cols, lags) {
  out <- data
  for (cc in cols) {
    if (cc %in% names(out)) {
      for (ll in lags) {
        new_name <- paste0(cc, "_lag", ll)
        out <- out %>% arrange(year, week) %>% mutate(!!new_name := dplyr::lag(.data[[cc]], ll))
      }
    }
  }
  out
}

make_city_rolls <- function(data, cols, windows) {
  out <- data
  for (cc in cols) {
    if (cc %in% names(out)) {
      for (ww in windows) {
        new_name <- paste0(cc, "_rollmean_lag1_w", ww)
        out <- out %>%
          arrange(year, week) %>%
          mutate(
            !!new_name := zoo::rollapplyr(
              dplyr::lag(.data[[cc]], 1),
              width = ww,
              FUN = function(z) mean(z, na.rm = TRUE),
              fill = NA_real_,
              partial = FALSE
            )
          )
      }
    }
  }
  out
}

is_case_related_feature <- function(feature_names) {
  str_detect(
    feature_names,
    regex(
      paste(c("dengue", "case", "cases", "incidence", "target", "outbreak", "positive", "classifier", "probability"), collapse = "|"),
      ignore_case = TRUE
    )
  )
}

alert_from_probability <- function(prob) {
  case_when(
    is.na(prob) ~ "Unknown",
    prob < 0.30 ~ "Low",
    prob < 0.50 ~ "Watch",
    prob < 0.70 ~ "Moderate",
    prob < 0.85 ~ "High",
    TRUE ~ "Very High"
  )
}

estimated_probability_from_cases <- function(pred_cases, threshold) {
  ifelse(is.na(pred_cases) | is.na(threshold) | threshold <= 0, NA_real_, pmin(pmax(pred_cases / threshold, 0), 1))
}

alert_from_cases <- function(pred_cases, threshold) {
  ratio <- pred_cases / threshold
  case_when(
    is.na(ratio) ~ "Unknown",
    ratio < 0.50 ~ "Low",
    ratio < 0.80 ~ "Watch",
    ratio < 1.00 ~ "Moderate",
    ratio < 1.50 ~ "High",
    TRUE ~ "Very High"
  )
}

horizon_label <- function(h) {
  if (h == 0) return("Selected week")
  paste0(h, " week", ifelse(h == 1, "", "s"), " after")
}

# Build model.matrix from prediction rows, then align to training matrix columns.
# Missing engineered columns are filled with 0 instead of failing. This makes deployment
# more robust when optional neighbor-map features cannot be reconstructed on the server.
make_prediction_matrix <- function(pred_data, feature_cols, matrix_feature_names) {
  if (is.null(feature_cols) || length(feature_cols) == 0) {
    stop("Feature specification is empty.")
  }
  if (is.null(matrix_feature_names) || length(matrix_feature_names) == 0) {
    stop("Matrix feature specification is empty.")
  }

  for (cc in setdiff(feature_cols, names(pred_data))) {
    pred_data[[cc]] <- 0
  }

  x_raw <- pred_data %>% select(all_of(feature_cols))
  x_raw <- x_raw %>%
    mutate(across(where(is.character), as.factor)) %>%
    mutate(across(where(is.logical), as.integer))

  # Replace missing numeric feature values with 0. This avoids model-matrix failures
  # during true future forecasting when some optional lag/neighbor features are absent.
  x_raw <- x_raw %>% mutate(across(where(is.numeric), ~ ifelse(is.na(.x) | !is.finite(.x), 0, .x)))

  mm <- model.matrix(~ . - 1, data = x_raw)

  out <- matrix(0, nrow = nrow(mm), ncol = length(matrix_feature_names))
  colnames(out) <- matrix_feature_names
  common <- intersect(colnames(mm), matrix_feature_names)
  if (length(common) > 0) {
    out[, common] <- mm[, common, drop = FALSE]
  }
  out
}

get_metric_row <- function(mode, level, horizon) {
  stem <- case_when(
    mode == "standard" & level == "barangay" ~ "standard_barangay_metrics.csv",
    mode == "standard" & level == "city" ~ "standard_city_metrics.csv",
    mode == "environmental_only" & level == "barangay" ~ "environmental_barangay_metrics.csv",
    mode == "environmental_only" & level == "city" ~ "environmental_city_metrics.csv",
    TRUE ~ NA_character_
  )
  path <- file.path(METADATA_DIR, stem)
  if (!file.exists(path)) return(NULL)
  m <- suppressWarnings(read.csv(path, stringsAsFactors = FALSE))
  if ("Horizon_number" %in% names(m)) {
    row <- m[m$Horizon_number == horizon, , drop = FALSE]
  } else if ("forecast_horizon" %in% names(m)) {
    row <- m[m$forecast_horizon == horizon, , drop = FALSE]
  } else if ("Horizon" %in% names(m)) {
    row <- m[as.character(m$Horizon) == paste0("H+", horizon), , drop = FALSE]
  } else if ("forecast_horizon_label" %in% names(m)) {
    row <- m[as.character(m$forecast_horizon_label) == paste0("H+", horizon), , drop = FALSE]
  } else {
    row <- m[0, , drop = FALSE]
  }
  if (nrow(row) == 0) return(NULL)
  row[1, , drop = FALSE]
}

metric_value <- function(row, candidates, default = NA_real_) {
  if (is.null(row)) return(default)
  for (cc in candidates) {
    if (cc %in% names(row)) {
      val <- suppressWarnings(as.numeric(row[[cc]][1]))
      if (!is.na(val)) return(val)
    }
  }
  default
}

accuracy_from_metrics <- function(row) {
  rmse <- metric_value(row, c("RMSE_raw"))
  mean_actual <- metric_value(row, c("Mean_actual_raw"))
  if (is.na(rmse) || is.na(mean_actual) || mean_actual <= 0) return(NA_real_)
  max(0, min(100, 100 * (1 - rmse / mean_actual)))
}

mae_from_metrics <- function(row) {
  metric_value(row, c("MAE_raw", "High_week_MAE_raw", "Outbreak_week_MAE", "Outbreak_week_MAE_raw"), default = NA_real_)
}

# -------------------------
# 4. Data preparation
# -------------------------
read_base_data <- function() {
  if (!file.exists(DATA_PATH)) stop(paste0("DATA_PATH not found: ", DATA_PATH))
  raw_df <- read_excel(DATA_PATH) %>% clean_names()
  required_cols <- c("barangay", "year", "week", "pop_density", "dengue_cases")
  missing_cols <- setdiff(required_cols, names(raw_df))
  if (length(missing_cols) > 0) stop(paste0("Missing required columns: ", paste(missing_cols, collapse = ", ")))
  df <- raw_df %>%
    mutate(
      barangay = toupper(str_squish(as.character(barangay))),
      year = as.integer(year),
      week = as.integer(week),
      dengue_cases = as.numeric(dengue_cases),
      pop_density = as.numeric(pop_density)
    ) %>%
    left_join(area_df, by = c("barangay" = "barangay_clean"))
  if (any(is.na(df$barangay_area_m2))) {
    bad <- unique(df$barangay[is.na(df$barangay_area_m2)])
    stop(paste0("Some barangays did not match the area table: ", paste(bad, collapse = ", ")))
  }
  df %>%
    mutate(
      barangay_area_km2 = barangay_area_m2 / 1e6,
      estimated_population = case_when(
        POP_DENSITY_UNIT == "per_km2" ~ pop_density * barangay_area_km2,
        POP_DENSITY_UNIT == "per_m2" ~ pop_density * barangay_area_m2,
        TRUE ~ NA_real_
      ),
      estimated_population = ifelse(estimated_population <= 0 | is.na(estimated_population), NA_real_, estimated_population),
      dengue_incidence_10000 = (dengue_cases / estimated_population) * 10000,
      dengue_incidence_10000 = ifelse(is.na(dengue_incidence_10000) | dengue_incidence_10000 < 0, 0, dengue_incidence_10000),
      target_log_incidence = log1p(dengue_incidence_10000),
      outbreak_actual = as.integer(dengue_cases >= OUTBREAK_COUNT_THRESHOLD),
      large_outbreak_actual = as.integer(dengue_cases >= LARGE_OUTBREAK_THRESHOLD),
      week_sin = sin(2 * pi * week / 52),
      week_cos = cos(2 * pi * week / 52)
    ) %>%
    arrange(barangay, year, week)
}

get_neighbor_pairs <- function(df) {
  # Faster path: use cached neighbor pairs if available.
  cache_path <- file.path(CACHE_DIR, "neighbor_pairs.rds")
  if (file.exists(cache_path)) {
    obj <- tryCatch(readRDS(cache_path), error = function(e) NULL)
    if (is.data.frame(obj) && all(c("barangay", "neighbor") %in% names(obj))) {
      return(as_tibble(obj))
    }
  }

  # Deployment-safe fallback: do NOT require sf/spdep. If those packages are unavailable,
  # return an empty neighbor table and the model-matrix builder will fill missing
  # neighbor-derived model columns with 0.
  if (!requireNamespace("sf", quietly = TRUE) || !requireNamespace("spdep", quietly = TRUE)) {
    return(tibble(barangay = character(), neighbor = character()))
  }

  if (!file.exists(SHAPE_ZIP_PATH)) {
    return(tibble(barangay = character(), neighbor = character()))
  }

  shape_dir <- file.path(tempdir(), paste0("cebu_city_barangays_shape_", as.integer(Sys.time())))
  dir.create(shape_dir, showWarnings = FALSE, recursive = TRUE)

  ok <- tryCatch({
    unzip(SHAPE_ZIP_PATH, exdir = shape_dir)
    TRUE
  }, error = function(e) FALSE)

  if (!ok) return(tibble(barangay = character(), neighbor = character()))

  shp_files <- list.files(shape_dir, pattern = "\\.shp$", full.names = TRUE)
  if (length(shp_files) == 0) return(tibble(barangay = character(), neighbor = character()))

  out <- tryCatch({
    barangay_sf <- suppressWarnings(sf::st_read(shp_files[1], quiet = TRUE)) %>% clean_names()
    possible_name_cols <- c("barangay", "brgy", "name", "adm4_en", "adm4_name", "bgy_name", "barangay_n", "brgy_name")
    name_col <- possible_name_cols[possible_name_cols %in% names(barangay_sf)][1]
    if (is.na(name_col)) stop("Could not detect barangay name column in shapefile.")

    barangay_sf <- barangay_sf %>%
      mutate(barangay = toupper(str_squish(as.character(.data[[name_col]])))) %>%
      filter(barangay %in% unique(df$barangay)) %>%
      arrange(barangay)

    barangay_sf <- sf::st_make_valid(barangay_sf)
    nb <- spdep::poly2nb(barangay_sf, queen = TRUE)
    names(nb) <- barangay_sf$barangay

    tibble(
      barangay = rep(names(nb), lengths(nb)),
      neighbor = names(nb)[unlist(nb)]
    ) %>%
      filter(!is.na(neighbor))
  }, error = function(e) {
    tibble(barangay = character(), neighbor = character())
  })

  tryCatch(saveRDS(out, cache_path), error = function(e) invisible(NULL))
  out
}

prepare_standard_barangay <- function(df) {
  neighbor_pairs <- get_neighbor_pairs(df)
  env_candidates <- c("rainfall", "rh", "humidity", "relative_humidity", "temp_c", "temperature", "t_mean", "tmin", "tmax", "t_min", "t_max", "u_component_of_wind_10m", "v_component_of_wind_10m", "wind_speed_10m", "wind_speed", "flood_depth", "flood_duration", "flood_extent", "water_level")
  env_vars <- env_candidates[env_candidates %in% names(df)]
  static_candidates <- c("pop_density", "barangay_area_m2", "barangay_area_km2", "flood_risk_index", "barangay_classification", names(df)[str_detect(names(df), "^percent_|^x_percent_|annual_crop|brush|built|forest|crop|barren|fishpond|grassland|mangrove|water|landcover")])
  city_week <- df %>% group_by(year, week) %>% summarise(city_cases = sum(dengue_cases, na.rm = TRUE), city_incidence_mean = mean(dengue_incidence_10000, na.rm = TRUE), .groups = "drop") %>% arrange(year, week)
  for (ll in 1:MAX_LAG_TO_TEST) {
    city_week <- city_week %>% mutate(!!paste0("city_cases_lag", ll) := dplyr::lag(city_cases, ll), !!paste0("city_incidence_mean_lag", ll) := dplyr::lag(city_incidence_mean, ll))
  }
  city_lag_cols <- names(city_week)[str_detect(names(city_week), "^city_cases_lag|^city_incidence_mean_lag")]
  df2 <- df %>% left_join(city_week %>% select(year, week, all_of(city_lag_cols)), by = c("year", "week"))
  if (nrow(neighbor_pairs) > 0) {
    neighbor_activity <- df %>%
      select(year, week, neighbor = barangay, dengue_cases, dengue_incidence_10000) %>%
      inner_join(neighbor_pairs, by = "neighbor") %>%
      group_by(barangay, year, week) %>%
      summarise(
        neighbor_cases_mean = mean(dengue_cases, na.rm = TRUE),
        neighbor_incidence_mean = mean(dengue_incidence_10000, na.rm = TRUE),
        .groups = "drop"
      )
    df2 <- df2 %>% left_join(neighbor_activity, by = c("barangay", "year", "week"))
  } else {
    df2$neighbor_cases_mean <- 0
    df2$neighbor_incidence_mean <- 0
  }
  df2 <- df2 %>%
    mutate(
      neighbor_cases_mean = ifelse(is.na(neighbor_cases_mean), 0, neighbor_cases_mean),
      neighbor_incidence_mean = ifelse(is.na(neighbor_incidence_mean), 0, neighbor_incidence_mean)
    )
  df2 %>%
    make_lags(c("dengue_cases", "dengue_incidence_10000", "target_log_incidence"), 1:MAX_LAG_TO_TEST) %>%
    make_lags(env_vars, 1:MAX_LAG_TO_TEST) %>%
    make_lags(c("neighbor_cases_mean", "neighbor_incidence_mean"), 1:MAX_LAG_TO_TEST) %>%
    make_rolls(env_vars, ENV_ROLL_WINDOWS)
}

prepare_environmental_barangay <- function(df) {
  neighbor_pairs <- get_neighbor_pairs(df)
  env_candidates <- c("rainfall", "rh", "humidity", "relative_humidity", "temp_c", "temperature", "t_mean", "tmin", "tmax", "t_min", "t_max", "u_component_of_wind_10m", "v_component_of_wind_10m", "wind_speed_10m", "wind_speed", "flood_depth", "flood_duration", "flood_extent", "water_level")
  env_vars <- env_candidates[env_candidates %in% names(df)]
  if (length(env_vars) == 0) stop("No environmental variables found in dataset.")
  if (nrow(neighbor_pairs) > 0) {
    neighbor_env <- df %>%
      select(year, week, neighbor = barangay, all_of(env_vars)) %>%
      inner_join(neighbor_pairs, by = "neighbor") %>%
      group_by(barangay, year, week) %>%
      summarise(across(all_of(env_vars), ~ mean(.x, na.rm = TRUE), .names = "neighbor_env_{.col}_mean"), .groups = "drop")
  } else {
    neighbor_env <- tibble(barangay = character(), year = integer(), week = integer())
  }
  city_env <- df %>%
    group_by(year, week) %>%
    summarise(across(all_of(env_vars), ~ mean(.x, na.rm = TRUE), .names = "city_env_{.col}_mean"), .groups = "drop")
  df_env <- df %>% left_join(neighbor_env, by = c("barangay", "year", "week")) %>% left_join(city_env, by = c("year", "week"))
  all_env_base_vars <- names(df_env)[names(df_env) %in% c(env_vars, names(df_env)[str_detect(names(df_env), "^neighbor_env_")], names(city_env)[str_detect(names(city_env), "^city_env_")])]
  all_env_base_vars <- all_env_base_vars[!is_case_related_feature(all_env_base_vars)]
  df_env %>% make_lags(all_env_base_vars, 1:MAX_LAG_TO_TEST) %>% make_rolls(all_env_base_vars, ENV_ROLL_WINDOWS)
}

add_weeks_to_year_week <- function(year, week, horizon) {
  # App-facing forecast labels. Uses 52-week year convention to match app ISO-week display.
  out_year <- as.integer(year)
  out_week <- as.integer(week) + as.integer(horizon)
  while (out_week > 52) {
    out_week <- out_week - 52
    out_year <- out_year + 1L
  }
  while (out_week < 1) {
    out_week <- out_week + 52
    out_year <- out_year - 1L
  }
  list(year = out_year, week = out_week)
}

add_barangay_horizon_targets <- function(data, horizon) {
  data %>%
    group_by(barangay) %>%
    arrange(year, week, .by_group = TRUE) %>%
    mutate(
      origin_year = year,
      origin_week = week,
      forecast_horizon = horizon,
      forecast_horizon_label = paste0("H+", horizon),
      # TRUE FORECASTING MODE:
      # Do not require future rows or future actual cases. The direct horizon model
      # uses origin-week features, and the app computes the future target label.
      target_year_h = purrr::map2_int(year, week, ~ add_weeks_to_year_week(.x, .y, horizon)$year),
      target_week_h = purrr::map2_int(year, week, ~ add_weeks_to_year_week(.x, .y, horizon)$week),
      target_cases_h = NA_real_,
      target_population_h = estimated_population,
      target_log_incidence_h = NA_real_,
      target_outbreak_h = NA_integer_,
      target_large_outbreak_h = NA_integer_
    ) %>%
    ungroup()
}

prepare_city_base <- function(df) {
  env_candidates <- c("rainfall", "rh", "humidity", "relative_humidity", "temp_c", "temperature", "t_mean", "tmin", "tmax", "t_min", "t_max", "u_component_of_wind_10m", "v_component_of_wind_10m", "wind_speed_10m", "wind_speed", "flood_depth", "flood_duration", "flood_extent", "water_level")
  env_vars <- env_candidates[env_candidates %in% names(df)]
  static_candidates <- c("pop_density", "flood_risk_index", names(df)[str_detect(names(df), "^percent_|^x_percent_|annual_crop|brush|built|forest|crop|barren|fishpond|grassland|mangrove|water|landcover")])
  static_vars <- unique(static_candidates[static_candidates %in% names(df)])
  weighted_vars <- unique(c(env_vars, static_vars))
  barangay_week_clean <- df %>%
    group_by(barangay, year, week) %>%
    summarise(dengue_cases = sum_safe(dengue_cases), estimated_population = mean(estimated_population, na.rm = TRUE), barangay_area_m2 = mean(barangay_area_m2, na.rm = TRUE), barangay_area_km2 = mean(barangay_area_km2, na.rm = TRUE), across(all_of(weighted_vars), ~ mean(as.numeric(.x), na.rm = TRUE), .names = "{.col}"), .groups = "drop") %>%
    mutate(estimated_population = ifelse(is.nan(estimated_population), NA_real_, estimated_population))
  city_weighted_features <- barangay_week_clean %>%
    group_by(year, week) %>%
    summarise(across(all_of(weighted_vars), ~ weighted_mean_safe(.x, barangay_area_m2), .names = "city_area_weighted_{.col}"), .groups = "drop")
  city_week_df <- barangay_week_clean %>%
    group_by(year, week) %>%
    summarise(city_dengue_cases = sum_safe(dengue_cases), city_estimated_population = sum_safe(estimated_population), city_area_m2 = sum_safe(barangay_area_m2), city_area_km2 = sum_safe(barangay_area_km2), n_barangays_reported = n_distinct(barangay), .groups = "drop") %>%
    left_join(city_weighted_features, by = c("year", "week")) %>%
    mutate(city_area_m2 = median(city_area_m2, na.rm = TRUE), city_area_km2 = median(city_area_km2, na.rm = TRUE), city_pop_density = city_estimated_population / city_area_km2, city_incidence_10000 = (city_dengue_cases / city_estimated_population) * 10000, city_incidence_10000 = ifelse(is.na(city_incidence_10000) | city_incidence_10000 < 0, 0, city_incidence_10000), target_log_incidence_city = log1p(city_incidence_10000), week_sin = sin(2 * pi * week / 52), week_cos = cos(2 * pi * week / 52)) %>% arrange(year, week)
  if (CITY_OUTBREAK_THRESHOLD_METHOD == "train_quantile") {
    city_outbreak_threshold_cases_value <- as.numeric(quantile(city_week_df$city_dengue_cases[city_week_df$year <= TRAIN_END_YEAR], probs = CITY_OUTBREAK_QUANTILE, na.rm = TRUE))
    city_large_outbreak_threshold_cases_value <- as.numeric(quantile(city_week_df$city_dengue_cases[city_week_df$year <= TRAIN_END_YEAR], probs = CITY_LARGE_OUTBREAK_QUANTILE, na.rm = TRUE))
  } else {
    city_outbreak_threshold_cases_value <- CITY_FIXED_OUTBREAK_THRESHOLD_CASES
    city_large_outbreak_threshold_cases_value <- CITY_FIXED_LARGE_OUTBREAK_THRESHOLD_CASES
  }
  city_week_df %>% mutate(city_outbreak_threshold_cases = city_outbreak_threshold_cases_value, city_large_outbreak_threshold_cases = city_large_outbreak_threshold_cases_value, outbreak_actual_city = as.integer(city_dengue_cases >= city_outbreak_threshold_cases), large_outbreak_actual_city = as.integer(city_dengue_cases >= city_large_outbreak_threshold_cases))
}

prepare_standard_city <- function(city_week_df) {
  city_case_base_vars <- c("city_dengue_cases", "city_incidence_10000", "target_log_incidence_city")
  city_weighted_vars <- names(city_week_df)[str_detect(names(city_week_df), "^city_area_weighted_")]
  city_base_vars_for_lags <- unique(c(city_case_base_vars, city_weighted_vars, "city_pop_density"))
  city_week_df %>% make_city_lags(city_base_vars_for_lags, 1:MAX_LAG_TO_TEST) %>% make_city_rolls(city_weighted_vars, ENV_ROLL_WINDOWS)
}

prepare_environmental_city <- function(city_week_df) {
  city_env_base_vars <- names(city_week_df)[str_detect(names(city_week_df), "^city_area_weighted_") & !is_case_related_feature(names(city_week_df))]
  city_week_df %>% make_city_lags(city_env_base_vars, 1:MAX_LAG_TO_TEST) %>% make_city_rolls(city_env_base_vars, ENV_ROLL_WINDOWS)
}

add_city_horizon_targets <- function(data, horizon) {
  data %>%
    arrange(year, week) %>%
    mutate(
      origin_year = year,
      origin_week = week,
      target_year = purrr::map2_int(year, week, ~ add_weeks_to_year_week(.x, .y, horizon)$year),
      target_week = purrr::map2_int(year, week, ~ add_weeks_to_year_week(.x, .y, horizon)$week),
      target_cases = NA_real_,
      target_incidence_10000 = NA_real_,
      target_log_incidence = NA_real_,
      target_population = city_estimated_population,
      target_outbreak = NA_integer_,
      target_large_outbreak = NA_integer_,
      forecast_horizon = horizon,
      forecast_horizon_label = paste0("H+", horizon)
    )
}

# -------------------------
# 5. Prediction functions
# -------------------------
predict_xgb_regressor <- function(bundle, pred_rows, population_col, matrix_names = NULL) {
  feature_cols <- bundle$feature_cols
  matrix_feature_names <- if (!is.null(matrix_names)) matrix_names else bundle$matrix_feature_names
  mat <- make_prediction_matrix(pred_rows, feature_cols, matrix_feature_names)
  pred_log <- predict(bundle$model, mat)
  pred_cases <- loginc_to_cases(pred_log, pred_rows[[population_col]])
  list(pred_log = pred_log, pred_cases = pred_cases)
}

predict_xgb_classifier <- function(bundle, pred_rows, matrix_names = NULL) {
  feature_cols <- bundle$feature_cols
  matrix_feature_names <- if (!is.null(matrix_names)) matrix_names else bundle$matrix_feature_names
  mat <- make_prediction_matrix(pred_rows, feature_cols, matrix_feature_names)
  pmin(pmax(predict(bundle$model, mat), 0), 1)
}

predict_lgb_regressor <- function(bundle, pred_rows, population_col) {
  if (!has_lightgbm) stop("The lightgbm R package is required for this model but is not installed.")
  model_path <- bundle$model_path
  if (!file.exists(model_path)) {
    # Fallback if model_path was saved from a different working directory.
    model_path <- file.path(dirname(dirname(model_path)), basename(dirname(model_path)), basename(model_path))
  }
  if (!file.exists(model_path)) stop(paste0("LightGBM model file not found: ", bundle$model_path))
  model <- lightgbm::lgb.load(model_path)
  mat <- make_prediction_matrix(pred_rows, bundle$feature_cols, bundle$matrix_feature_names)
  pred_log <- predict(model, mat)
  pred_cases <- loginc_to_cases(pred_log, pred_rows[[population_col]])
  list(pred_log = pred_log, pred_cases = pred_cases)
}

predict_one_barangay_horizon <- function(data_engineered, mode, h, origin_year, origin_week) {
  model_dir <- if (mode == "standard") file.path(MODELS_DIR, "standard_barangay") else file.path(MODELS_DIR, "environmental_barangay")
  spec <- readRDS(file.path(model_dir, paste0("h", h, "_feature_spec.rds")))
  reg_bundle <- readRDS(file.path(model_dir, paste0("h", h, "_regressor.rds")))
  pred_data <- add_barangay_horizon_targets(data_engineered, h) %>%
    filter(origin_year == !!origin_year, origin_week == !!origin_week) %>%
    mutate(
      barangay = as.factor(barangay),
      barangay_classification = if ("barangay_classification" %in% names(.)) as.factor(barangay_classification) else factor("unknown")
    )
  if (nrow(pred_data) == 0) return(NULL)
  pred_data <- pred_data %>% filter(!is.na(target_population_h))
  if (nrow(pred_data) == 0) return(NULL)

  if (mode == "standard") {
    cls_bundle <- readRDS(file.path(model_dir, paste0("h", h, "_classifier.rds")))
    reg_pred <- predict_xgb_regressor(reg_bundle, pred_data, "target_population_h", spec$regressor_matrix_feature_names)
    prob <- predict_xgb_classifier(cls_bundle, pred_data, spec$classifier_matrix_feature_names)
    pred_cases <- pmax(reg_pred$pred_cases * (prob ^ SOFT_ALPHA), 0)
    alert <- alert_from_probability(prob)
    prob_type <- "classifier_probability"
  } else {
    reg_pred <- predict_xgb_regressor(reg_bundle, pred_data, "target_population_h", spec$matrix_feature_names)
    pred_cases <- pmax(reg_pred$pred_cases, 0)
    prob <- estimated_probability_from_cases(pred_cases, OUTBREAK_COUNT_THRESHOLD)
    alert <- alert_from_cases(pred_cases, OUTBREAK_COUNT_THRESHOLD)
    prob_type <- "estimated_from_predicted_cases"
  }

  metric_row <- get_metric_row(mode, "barangay", h)
  acc <- accuracy_from_metrics(metric_row)
  mae <- mae_from_metrics(metric_row)
  rows <- pred_data %>%
    transmute(
      barangay = as.character(barangay),
      predicted_cases = as.numeric(pred_cases),
      outbreak_probability = as.numeric(prob),
      estimated_outbreak_probability = if (prob_type == "estimated_from_predicted_cases") as.numeric(prob) else NA_real_,
      probability_type = prob_type,
      alert_level = as.character(alert),
      target_year = as.integer(target_year_h),
      target_week = as.integer(target_week_h),
      outbreak_threshold = OUTBREAK_COUNT_THRESHOLD,
      accuracy_percent = acc
    )
  list(
    horizon = h,
    label = horizon_label(h),
    target_year = as.integer(rows$target_year[1]),
    target_week = as.integer(rows$target_week[1]),
    accuracy_percent = acc,
    mae = mae,
    total_predicted_cases = sum(rows$predicted_cases, na.rm = TRUE),
    mean_outbreak_probability = mean(rows$outbreak_probability, na.rm = TRUE),
    barangay_predictions = rows
  )
}

predict_one_city_horizon <- function(data_engineered, mode, h, origin_year, origin_week) {
  model_dir <- if (mode == "standard") file.path(MODELS_DIR, "standard_city") else file.path(MODELS_DIR, "environmental_city")
  spec <- readRDS(file.path(model_dir, paste0("h", h, "_feature_spec.rds")))
  reg_bundle <- readRDS(file.path(model_dir, paste0("h", h, "_regressor.rds")))
  pred_data <- add_city_horizon_targets(data_engineered, h) %>%
    filter(origin_year == !!origin_year, origin_week == !!origin_week) %>%
    filter(!is.na(target_population))
  if (nrow(pred_data) == 0) return(NULL)
  reg_pred <- predict_lgb_regressor(reg_bundle, pred_data, "target_population")
  pred_cases <- pmax(reg_pred$pred_cases, 0)
  threshold <- pred_data$city_outbreak_threshold_cases[1]
  prob <- estimated_probability_from_cases(pred_cases[1], threshold)
  alert <- alert_from_cases(pred_cases[1], threshold)
  metric_row <- get_metric_row(mode, "city", h)
  acc <- accuracy_from_metrics(metric_row)
  mae <- mae_from_metrics(metric_row)
  list(
    horizon = h,
    label = horizon_label(h),
    target_year = as.integer(pred_data$target_year[1]),
    target_week = as.integer(pred_data$target_week[1]),
    predicted_cases = as.numeric(pred_cases[1]),
    total_predicted_cases = as.numeric(pred_cases[1]),
    outbreak_probability = as.numeric(prob),
    estimated_outbreak_probability = as.numeric(prob),
    probability_type = "estimated_from_predicted_cases",
    alert_level = alert,
    city_status = ifelse(pred_cases[1] >= threshold, "Above outbreak threshold", "Below outbreak threshold"),
    outbreak_threshold = as.numeric(threshold),
    accuracy_percent = acc,
    mae = mae
  )
}

# -------------------------
# 6. Main
# -------------------------
main <- function() {
  args <- parse_args()
  df <- read_base_data()

  if (args$level == "barangay") {
    engineered <- if (args$mode == "standard") prepare_standard_barangay(df) else prepare_environmental_barangay(df)
    horizons <- purrr::map(DISPLAY_HORIZONS, ~ predict_one_barangay_horizon(engineered, args$mode, .x, args$year, args$week))
  } else {
    city_base <- prepare_city_base(df)
    engineered <- if (args$mode == "standard") prepare_standard_city(city_base) else prepare_environmental_city(city_base)
    horizons <- purrr::map(DISPLAY_HORIZONS, ~ predict_one_city_horizon(engineered, args$mode, .x, args$year, args$week))
  }

  horizons <- horizons[!vapply(horizons, is.null, logical(1))]
  result <- list(
    mode = args$mode,
    level = args$level,
    origin_year = args$year,
    origin_week = args$week,
    display_horizons = DISPLAY_HORIZONS,
    horizons = horizons
  )
  cat(jsonlite::toJSON(result, dataframe = "rows", auto_unbox = TRUE, na = "null", digits = 6))
}

tryCatch(
  main(),
  error = function(e) {
    cat(jsonlite::toJSON(list(error = TRUE, message = conditionMessage(e)), auto_unbox = TRUE, na = "null"))
    quit(status = 1)
  }
)
