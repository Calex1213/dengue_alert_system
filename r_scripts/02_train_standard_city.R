# ============================================================
# CITY-WEEK STANDARD PURE LIGHTGBM REGRESSOR
# H+0 TO H+12 DIRECT FORECASTS
#
# Purpose:
#   Builds a city-wide weekly dengue forecasting model using the standard
#   city-week predictor set, including dengue autocorrelation, citywide
#   environmental lags, rolling means, and non-case static/seasonal predictors.
#
# This is the city-week counterpart of the winning barangay-week
# pure LightGBM regressor.
#
# Key rule:
#   This standard model DOES use dengue autocorrelation predictors, including
#   city dengue case, incidence, and log-incidence lags. Use this model when
#   recent dengue surveillance data are available.
#
# Standard city-week predictors include:
#   - city dengue case/incidence/log-incidence lags
#   - area-weighted city environmental variables
#   - environmental lags and rolling means
#   - non-case static / spatial variables, if available
#   - seasonality and city size/population descriptors
#
# Main outputs:
#   - city_week_pure_lightgbm_H0_TO_H12_metrics_*.csv
#   - city_week_pure_lightgbm_H0_TO_H12_predictions_*.csv
#   - city_week_pure_lightgbm_H0_TO_H12_selected_settings_*.csv
#   - city_week_pure_lightgbm_H0_TO_H12_feature_importance_*.csv
#   - city_week_pure_lightgbm_H0_TO_H12_compact_summary_*.csv
# ============================================================


# ============================================================
# 0. USER SETTINGS
# ============================================================


# ============================================================
# PROJECT-RELATIVE PATHS FOR FINAL APP V2
# ============================================================
args0 <- commandArgs(trailingOnly = FALSE)
file_arg <- args0[grepl("^--file=", args0)]
SCRIPT_DIR <- if (length(file_arg) > 0) {
  dirname(normalizePath(sub("^--file=", "", file_arg[1]), mustWork = FALSE))
} else {
  getwd()
}
PROJECT_DIR <- if (basename(SCRIPT_DIR) == "r_scripts") {
  normalizePath(file.path(SCRIPT_DIR, ".."), mustWork = FALSE)
} else {
  normalizePath(SCRIPT_DIR, mustWork = FALSE)
}
setwd(PROJECT_DIR)

DATA_PATH <- file.path(PROJECT_DIR, "data", "FINAL_DATASET.xlsx")
OUTPUT_DIR <- file.path(PROJECT_DIR, "outputs")
MODEL_ROOT_DIR <- file.path(PROJECT_DIR, "models")
METADATA_DIR <- file.path(PROJECT_DIR, "model_metadata")
dir.create(OUTPUT_DIR, showWarnings = FALSE, recursive = TRUE)
dir.create(MODEL_ROOT_DIR, showWarnings = FALSE, recursive = TRUE)
dir.create(METADATA_DIR, showWarnings = FALSE, recursive = TRUE)


TRAIN_END_YEAR <- 2022
TEST_YEARS <- c(2023, 2024)

VALIDATION_YEARS <- c(2021, 2022)
TRAIN_CORE_END_YEAR <- min(VALIDATION_YEARS) - 1

MAX_LAG_TO_TEST <- 20
LAG_KEEP_GRID <- c(1, 2, 3, 4, 5)
ENV_ROLL_WINDOWS <- c(4, 8)
FORECAST_HORIZONS <- 0:12
DISPLAY_HORIZONS <- c(0, 1, 2, 3, 4, 8, 12)

POP_DENSITY_UNIT <- "per_km2"
USE_PEAK_WEIGHTS <- TRUE
INCLUDE_STATIC_AND_SEASONAL_NON_CASE_FEATURES <- TRUE

CITY_OUTBREAK_THRESHOLD_METHOD <- "train_quantile"
CITY_OUTBREAK_QUANTILE <- 0.75
CITY_LARGE_OUTBREAK_QUANTILE <- 0.90
CITY_FIXED_OUTBREAK_THRESHOLD_CASES <- 100
CITY_FIXED_LARGE_OUTBREAK_THRESHOLD_CASES <- 200

OUTPUT_PREFIX <- "CITY_WEEK_PURE_LIGHTGBM_H0_TO_H12"
PLOT_DIR <- file.path(OUTPUT_DIR, paste0("dengue_city_week_plots_", OUTPUT_PREFIX))
MODEL_DIR <- file.path(MODEL_ROOT_DIR, "standard_city")
dir.create(MODEL_DIR, showWarnings = FALSE, recursive = TRUE)

SEED <- 123
set.seed(SEED)

dir.create(PLOT_DIR, showWarnings = FALSE, recursive = TRUE)
dir.create(OUTPUT_DIR, showWarnings = FALSE, recursive = TRUE)


# ============================================================
# 1. PACKAGES
# ============================================================

needed_packages <- c(
  "readxl", "dplyr", "tidyr", "stringr", "janitor", "purrr",
  "lightgbm", "Matrix", "tibble", "ggplot2", "scales", "zoo",
  "pROC", "readr"
)

installed <- rownames(installed.packages())
for (p in needed_packages) {
  if (!(p %in% installed)) install.packages(p, dependencies = TRUE)
}

library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(janitor)
library(purrr)
library(lightgbm)
library(Matrix)
library(tibble)
library(ggplot2)
library(scales)
library(zoo)
library(pROC)
library(readr)


# ============================================================
# 2. BARANGAY AREA TABLE
# ============================================================

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


# ============================================================
# 3. BASIC HELPERS
# ============================================================

weighted_mean_safe <- function(x, w) {
  x <- as.numeric(x)
  w <- as.numeric(w)
  ok <- !is.na(x) & !is.na(w) & w > 0
  if (sum(ok) == 0) return(NA_real_)
  weighted.mean(x[ok], w[ok], na.rm = TRUE)
}

sum_safe <- function(x) {
  sum(as.numeric(x), na.rm = TRUE)
}

safe_rmse <- function(actual, pred) sqrt(mean((actual - pred)^2, na.rm = TRUE))
safe_mae <- function(actual, pred) mean(abs(actual - pred), na.rm = TRUE)

safe_r2 <- function(actual, pred) {
  ss_res <- sum((actual - pred)^2, na.rm = TRUE)
  ss_tot <- sum((actual - mean(actual, na.rm = TRUE))^2, na.rm = TRUE)
  if (is.na(ss_tot) || ss_tot == 0) return(NA_real_)
  1 - ss_res / ss_tot
}

safe_cor <- function(actual, pred) suppressWarnings(cor(actual, pred, use = "complete.obs"))

city_loginc_to_cases <- function(pred_log_incidence, estimated_population) {
  pred_incidence <- expm1(pred_log_incidence)
  pred_cases <- (pred_incidence / 10000) * estimated_population
  pred_cases <- ifelse(is.na(pred_cases), 0, pred_cases)
  pmax(pred_cases, 0)
}

city_cases_to_loginc <- function(cases, estimated_population) {
  incidence <- (cases / estimated_population) * 10000
  incidence <- ifelse(is.na(incidence) | incidence < 0, 0, incidence)
  log1p(incidence)
}

classification_metrics_from_cases_city <- function(actual_cases, pred_cases, threshold_cases) {
  actual_class <- actual_cases >= threshold_cases
  pred_class <- pred_cases >= threshold_cases

  tp <- sum(actual_class & pred_class, na.rm = TRUE)
  tn <- sum(!actual_class & !pred_class, na.rm = TRUE)
  fp <- sum(!actual_class & pred_class, na.rm = TRUE)
  fn <- sum(actual_class & !pred_class, na.rm = TRUE)

  precision <- ifelse((tp + fp) == 0, NA_real_, tp / (tp + fp))
  recall <- ifelse((tp + fn) == 0, NA_real_, tp / (tp + fn))
  f1 <- ifelse(is.na(precision) | is.na(recall) | (precision + recall) == 0, NA_real_, 2 * precision * recall / (precision + recall))
  false_alarm_rate <- ifelse((fp + tn) == 0, NA_real_, fp / (fp + tn))
  specificity <- ifelse((tn + fp) == 0, NA_real_, tn / (tn + fp))

  tibble(
    Precision = precision,
    Recall = recall,
    Outbreak_F1_count_threshold = f1,
    False_alarm_rate = false_alarm_rate,
    Specificity = specificity,
    TP = tp,
    TN = tn,
    FP = fp,
    FN = fn
  )
}

make_alert_level_from_cases <- function(pred_cases, threshold_cases) {
  ratio <- pred_cases / threshold_cases
  case_when(
    is.na(ratio) ~ NA_character_,
    ratio < 0.50 ~ "Low",
    ratio < 0.80 ~ "Watch",
    ratio < 1.00 ~ "Moderate",
    ratio < 1.50 ~ "High",
    TRUE ~ "Very high"
  )
}

city_make_reg_weights <- function(cases) {
  if (!USE_PEAK_WEIGHTS) return(rep(1, length(cases)))

  q75 <- quantile(cases, 0.75, na.rm = TRUE)
  q90 <- quantile(cases, 0.90, na.rm = TRUE)
  q95 <- quantile(cases, 0.95, na.rm = TRUE)

  case_when(
    cases >= q95 ~ 8,
    cases >= q90 ~ 5,
    cases >= q75 ~ 3,
    cases > 0 ~ 1.5,
    TRUE ~ 1
  )
}

is_case_related_feature <- function(feature_names) {
  str_detect(
    feature_names,
    regex(
      paste(
        c(
          "dengue", "case", "cases", "incidence", "target", "outbreak",
          "positive", "classifier", "probability"
        ),
        collapse = "|"
      ),
      ignore_case = TRUE
    )
  )
}


# ============================================================
# 4. READ DATA AND CREATE BARANGAY BASE VARIABLES
# ============================================================

if (!file.exists(DATA_PATH)) {
  stop(paste0("DATA_PATH not found: ", DATA_PATH, "
Edit DATA_PATH at the top of this script."))
}

raw_df <- read_excel(DATA_PATH) %>% clean_names()

cat("
Columns found in FINAL_DATASET:
")
print(names(raw_df))

required_cols <- c("barangay", "year", "week", "pop_density", "dengue_cases")
missing_cols <- setdiff(required_cols, names(raw_df))
if (length(missing_cols) > 0) {
  stop(paste0("Missing required columns: ", paste(missing_cols, collapse = ", ")))
}

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
  cat("
Barangays without matched area:
")
  print(unique(df$barangay[is.na(df$barangay_area_m2)]))
  stop("Some barangays did not match the area table.")
}

df <- df %>%
  mutate(barangay_area_km2 = barangay_area_m2 / 1e6)

if (POP_DENSITY_UNIT == "per_km2") {
  df <- df %>% mutate(estimated_population = pop_density * barangay_area_km2)
} else if (POP_DENSITY_UNIT == "per_m2") {
  df <- df %>% mutate(estimated_population = pop_density * barangay_area_m2)
} else {
  stop("POP_DENSITY_UNIT must be either 'per_km2' or 'per_m2'.")
}

df <- df %>%
  mutate(
    estimated_population = ifelse(estimated_population <= 0 | is.na(estimated_population), NA_real_, estimated_population)
  ) %>%
  arrange(barangay, year, week)


# ============================================================
# 5. AGGREGATE BARANGAY-WEEK TO CITY-WEEK
# ============================================================

env_candidates <- c(
  "rainfall", "rh", "humidity", "relative_humidity",
  "temp_c", "temperature", "t_mean", "tmin", "tmax", "t_min", "t_max",
  "u_component_of_wind_10m", "v_component_of_wind_10m", "wind_speed_10m", "wind_speed",
  "flood_depth", "flood_duration", "flood_extent", "water_level"
)

env_vars <- env_candidates[env_candidates %in% names(df)]
if (length(env_vars) == 0) {
  stop("No environmental variables were found. Check column names after clean_names().")
}

static_candidates <- c(
  "pop_density",
  "flood_risk_index",
  names(df)[str_detect(names(df), "^percent_|^x_percent_|annual_crop|brush|built|forest|crop|barren|fishpond|grassland|mangrove|water|landcover")]
)

static_vars <- unique(static_candidates[static_candidates %in% names(df)])
weighted_vars <- unique(c(env_vars, static_vars))
weighted_vars <- weighted_vars[!is_case_related_feature(weighted_vars)]

cat("
Standard city-week variables area-weighted into city-week features:
")
print(weighted_vars)

barangay_week_clean <- df %>%
  group_by(barangay, year, week) %>%
  summarise(
    dengue_cases = sum_safe(dengue_cases),
    estimated_population = mean(estimated_population, na.rm = TRUE),
    barangay_area_m2 = mean(barangay_area_m2, na.rm = TRUE),
    barangay_area_km2 = mean(barangay_area_km2, na.rm = TRUE),
    across(all_of(weighted_vars), ~ mean(as.numeric(.x), na.rm = TRUE), .names = "{.col}"),
    .groups = "drop"
  ) %>%
  mutate(
    estimated_population = ifelse(is.nan(estimated_population), NA_real_, estimated_population)
  )

city_weighted_features <- barangay_week_clean %>%
  group_by(year, week) %>%
  summarise(
    across(
      all_of(weighted_vars),
      ~ weighted_mean_safe(.x, barangay_area_m2),
      .names = "city_area_weighted_{.col}"
    ),
    .groups = "drop"
  )

city_week_df <- barangay_week_clean %>%
  group_by(year, week) %>%
  summarise(
    city_dengue_cases = sum_safe(dengue_cases),
    city_estimated_population = sum_safe(estimated_population),
    city_area_m2 = sum_safe(barangay_area_m2),
    city_area_km2 = sum_safe(barangay_area_km2),
    n_barangays_reported = n_distinct(barangay),
    .groups = "drop"
  ) %>%
  left_join(city_weighted_features, by = c("year", "week")) %>%
  mutate(
    city_area_m2 = median(city_area_m2, na.rm = TRUE),
    city_area_km2 = median(city_area_km2, na.rm = TRUE),
    city_pop_density = city_estimated_population / city_area_km2,
    city_incidence_10000 = (city_dengue_cases / city_estimated_population) * 10000,
    city_incidence_10000 = ifelse(is.na(city_incidence_10000) | city_incidence_10000 < 0, 0, city_incidence_10000),
    target_log_incidence_city = log1p(city_incidence_10000),
    week_sin = sin(2 * pi * week / 52),
    week_cos = cos(2 * pi * week / 52)
  ) %>%
  arrange(year, week)

if (CITY_OUTBREAK_THRESHOLD_METHOD == "train_quantile") {
  city_outbreak_threshold_cases_value <- as.numeric(
    quantile(city_week_df$city_dengue_cases[city_week_df$year <= TRAIN_END_YEAR], probs = CITY_OUTBREAK_QUANTILE, na.rm = TRUE)
  )
  city_large_outbreak_threshold_cases_value <- as.numeric(
    quantile(city_week_df$city_dengue_cases[city_week_df$year <= TRAIN_END_YEAR], probs = CITY_LARGE_OUTBREAK_QUANTILE, na.rm = TRUE)
  )
} else {
  city_outbreak_threshold_cases_value <- CITY_FIXED_OUTBREAK_THRESHOLD_CASES
  city_large_outbreak_threshold_cases_value <- CITY_FIXED_LARGE_OUTBREAK_THRESHOLD_CASES
}

city_week_df <- city_week_df %>%
  mutate(
    city_outbreak_threshold_cases = city_outbreak_threshold_cases_value,
    city_large_outbreak_threshold_cases = city_large_outbreak_threshold_cases_value,
    outbreak_actual_city = as.integer(city_dengue_cases >= city_outbreak_threshold_cases),
    large_outbreak_actual_city = as.integer(city_dengue_cases >= city_large_outbreak_threshold_cases)
  )

cat("
City outbreak threshold cases:", city_outbreak_threshold_cases_value, "
")
cat("City large outbreak threshold cases:", city_large_outbreak_threshold_cases_value, "
")
cat("City-week rows:", nrow(city_week_df), "
")


# ============================================================
# 6. ENVIRONMENTAL-ONLY CITY FEATURE ENGINEERING
# ============================================================

make_city_lags <- function(data, cols, lags) {
  out <- data
  for (cc in cols) {
    if (cc %in% names(out)) {
      for (ll in lags) {
        new_name <- paste0(cc, "_lag", ll)
        out <- out %>%
          arrange(year, week) %>%
          mutate(!!new_name := dplyr::lag(.data[[cc]], ll))
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

select_best_city_lags_for_horizon <- function(data, base_vars, max_lag = 20, keep_per_var = 3, train_end_year = TRAIN_END_YEAR) {
  train_only <- data %>% filter(target_year <= train_end_year)
  selected <- c()
  reports <- list()

  for (v in base_vars) {
    lag_cols <- paste0(v, "_lag", 1:max_lag)
    lag_cols <- lag_cols[lag_cols %in% names(train_only)]
    if (length(lag_cols) == 0) next

    scores <- purrr::map_df(lag_cols, function(col) {
      ok <- !is.na(train_only[[col]]) &
        !is.na(train_only$target_log_incidence) &
        !is.na(train_only$target_cases) &
        !is.na(train_only$target_outbreak) &
        !is.na(train_only$target_large_outbreak)

      if (sum(ok) < 20 || sd(train_only[[col]][ok], na.rm = TRUE) == 0) {
        return(tibble(
          variable = v,
          feature = col,
          lag = as.integer(str_extract(col, "\\d+$")),
          cor_log_incidence = NA_real_,
          cor_cases = NA_real_,
          cor_outbreak = NA_real_,
          cor_large_outbreak = NA_real_,
          peak_lift = NA_real_,
          selection_score = NA_real_
        ))
      }

      x <- train_only[[col]][ok]
      y_log <- train_only$target_log_incidence[ok]
      y_cases <- train_only$target_cases[ok]
      y_out <- train_only$target_outbreak[ok]
      y_large <- train_only$target_large_outbreak[ok]

      cor_log <- suppressWarnings(abs(cor(x, y_log, use = "complete.obs")))
      cor_cases <- suppressWarnings(abs(cor(x, y_cases, use = "complete.obs")))
      cor_out <- suppressWarnings(abs(cor(x, y_out, use = "complete.obs")))
      cor_large <- suppressWarnings(abs(cor(x, y_large, use = "complete.obs")))

      peak_lift <- NA_real_
      if (sum(y_large == 1, na.rm = TRUE) >= 5 && sum(y_large == 0, na.rm = TRUE) >= 5) {
        peak_lift <- abs(mean(x[y_large == 1], na.rm = TRUE) - mean(x[y_large == 0], na.rm = TRUE)) /
          (sd(x, na.rm = TRUE) + 1e-9)
      }

      score <- 0.35 * ifelse(is.na(cor_log), 0, cor_log) +
        0.30 * ifelse(is.na(cor_cases), 0, cor_cases) +
        0.20 * ifelse(is.na(cor_out), 0, cor_out) +
        0.10 * ifelse(is.na(cor_large), 0, cor_large) +
        0.05 * ifelse(is.na(peak_lift), 0, min(peak_lift, 3) / 3)

      tibble(
        variable = v,
        feature = col,
        lag = as.integer(str_extract(col, "\\d+$")),
        cor_log_incidence = cor_log,
        cor_cases = cor_cases,
        cor_outbreak = cor_out,
        cor_large_outbreak = cor_large,
        peak_lift = peak_lift,
        selection_score = score
      )
    }) %>%
      arrange(desc(selection_score), lag)

    chosen <- scores %>%
      filter(!is.na(selection_score)) %>%
      slice_head(n = keep_per_var) %>%
      pull(feature)

    selected <- c(selected, chosen)
    reports[[v]] <- scores %>% mutate(selected = feature %in% chosen)
  }

  list(
    selected_features = unique(selected),
    lag_report = bind_rows(reports)
  )
}

# Standard city-week model: include dengue autocorrelation + environmental/static predictors.
city_case_base_vars <- c(
  "city_dengue_cases",
  "city_incidence_10000",
  "target_log_incidence_city"
)

city_weighted_vars <- names(city_week_df)[str_detect(names(city_week_df), "^city_area_weighted_")]

city_base_vars_for_lags <- unique(c(
  city_case_base_vars,
  city_weighted_vars,
  "city_pop_density"
))

city_week_lagged <- city_week_df %>%
  make_city_lags(city_base_vars_for_lags, 1:MAX_LAG_TO_TEST) %>%
  make_city_rolls(city_weighted_vars, ENV_ROLL_WINDOWS)

city_roll_feature_cols <- names(city_week_lagged)[str_detect(names(city_week_lagged), "_rollmean_lag1_w")]

city_static_feature_cols <- c(
  "week_sin",
  "week_cos",
  "city_estimated_population",
  "city_area_m2",
  "city_area_km2",
  "city_pop_density",
  "n_barangays_reported"
)
city_static_feature_cols <- city_static_feature_cols[city_static_feature_cols %in% names(city_week_lagged)]

cat("\nCity base variables for lagging/rolling in standard LightGBM model:\n")
print(city_base_vars_for_lags)
cat("\nNumber of city base variables:", length(city_base_vars_for_lags), "\n")


# ============================================================
# 7. MATRIX, LIGHTGBM MODEL, AND EVALUATION HELPERS
# ============================================================

make_city_matrix <- function(train_data, pred_data, features) {
  train_x_raw <- train_data %>% select(all_of(features))
  pred_x_raw <- pred_data %>% select(all_of(features))

  combined <- bind_rows(train_x_raw, pred_x_raw) %>%
    mutate(across(where(is.character), as.factor)) %>%
    mutate(across(where(is.logical), as.integer))

  mm <- model.matrix(~ . - 1, data = combined)

  train_mat <- mm[1:nrow(train_x_raw), , drop = FALSE]
  pred_mat <- mm[(nrow(train_x_raw) + 1):nrow(mm), , drop = FALSE]

  list(train = train_mat, pred = pred_mat, feature_names = colnames(mm))
}

make_city_horizon_dataset <- function(data, horizon) {
  shift_n <- horizon
  data %>%
    arrange(year, week) %>%
    mutate(
      origin_year = year,
      origin_week = week,
      target_year = dplyr::lead(year, shift_n),
      target_week = dplyr::lead(week, shift_n),
      target_cases = dplyr::lead(city_dengue_cases, shift_n),
      target_incidence_10000 = dplyr::lead(city_incidence_10000, shift_n),
      target_log_incidence = dplyr::lead(target_log_incidence_city, shift_n),
      target_population = dplyr::lead(city_estimated_population, shift_n),
      target_outbreak = dplyr::lead(outbreak_actual_city, shift_n),
      target_large_outbreak = dplyr::lead(large_outbreak_actual_city, shift_n)
    ) %>%
    filter(
      !is.na(target_year),
      !is.na(target_week),
      !is.na(target_cases),
      !is.na(target_population),
      !is.na(target_log_incidence),
      !is.na(target_outbreak),
      !is.na(target_large_outbreak)
    ) %>%
    mutate(
      forecast_horizon = horizon,
      forecast_horizon_label = paste0("H+", horizon)
    )
}

build_city_horizon_features <- function(horizon, keep_per_var, lag_train_end_year) {
  h_df <- make_city_horizon_dataset(city_week_lagged, horizon)

  city_lag_selection <- select_best_city_lags_for_horizon(
    data = h_df,
    base_vars = city_base_vars_for_lags,
    max_lag = MAX_LAG_TO_TEST,
    keep_per_var = keep_per_var,
    train_end_year = lag_train_end_year
  )

  selected_lag_features <- city_lag_selection$selected_features

  city_feature_cols <- unique(c(city_static_feature_cols, selected_lag_features, city_roll_feature_cols))

  city_feature_cols <- city_feature_cols[city_feature_cols %in% names(h_df)]
  city_feature_cols <- city_feature_cols[!str_detect(city_feature_cols, "geometry|geom|shape|objectid")]

  city_required_complete_cols <- unique(c(
    "target_cases", "target_incidence_10000", "target_log_incidence", "target_population",
    "target_outbreak", "target_large_outbreak",
    selected_lag_features,
    city_roll_feature_cols
  ))
  city_required_complete_cols <- city_required_complete_cols[city_required_complete_cols %in% names(h_df)]

  city_model_df <- h_df %>%
    arrange(year, week) %>%
    filter(if_all(all_of(city_required_complete_cols), ~ !is.na(.x)))

  list(
    data = city_model_df,
    features = city_feature_cols,
    selected_lag_features = selected_lag_features,
    lag_report = city_lag_selection$lag_report
  )
}

lgb_grid <- expand.grid(
  learning_rate = c(0.03, 0.05),
  num_leaves = c(7, 15, 31),
  min_data_in_leaf = c(5, 10, 20),
  feature_fraction = c(0.85),
  bagging_fraction = c(0.85),
  bagging_freq = c(1),
  nrounds = c(400, 500, 600),
  stringsAsFactors = FALSE
)

fit_city_lgb_regressor <- function(train_data, pred_data, features, params) {
  params <- as.list(params)
  mats <- make_city_matrix(train_data, pred_data, features)
  x_tr <- mats$train
  x_pr <- mats$pred

  dtrain <- lightgbm::lgb.Dataset(
    data = x_tr,
    label = train_data$target_log_incidence,
    weight = city_make_reg_weights(train_data$target_cases)
  )

  model <- lightgbm::lgb.train(
    params = list(
      objective = "regression",
      metric = "rmse",
      learning_rate = params$learning_rate,
      num_leaves = as.integer(params$num_leaves),
      min_data_in_leaf = as.integer(params$min_data_in_leaf),
      feature_fraction = params$feature_fraction,
      bagging_fraction = params$bagging_fraction,
      bagging_freq = as.integer(params$bagging_freq),
      verbosity = -1,
      seed = SEED,
      feature_pre_filter = FALSE
    ),
    data = dtrain,
    nrounds = as.integer(params$nrounds)
  )

  pred_log <- predict(model, x_pr)
  pred_cases <- city_loginc_to_cases(pred_log, pred_data$target_population)

  list(
    model = model,
    pred_log = pred_log,
    pred_cases = pmax(pred_cases, 0),
    feature_names = colnames(x_tr),
    nrounds = as.integer(params$nrounds)
  )
}

tune_city_lgbm_regressor <- function(train_core, validation, features, horizon_label) {
  purrr::pmap_df(lgb_grid, function(
    learning_rate, num_leaves, min_data_in_leaf, feature_fraction,
    bagging_fraction, bagging_freq, nrounds
  ) {
    params <- tibble(
      learning_rate = learning_rate,
      num_leaves = num_leaves,
      min_data_in_leaf = min_data_in_leaf,
      feature_fraction = feature_fraction,
      bagging_fraction = bagging_fraction,
      bagging_freq = bagging_freq,
      nrounds = nrounds
    )

    fit <- fit_city_lgb_regressor(
      train_data = train_core,
      pred_data = validation,
      features = features,
      params = params
    )

    metrics <- evaluate_city_regressor(
      model_name = "LightGBM tuning",
      horizon = as.integer(str_replace(horizon_label, "H\\+", "")),
      base_df = validation,
      pred_cases = fit$pred_cases
    )

    tibble(
      forecast_horizon_label = horizon_label,
      model_part = "regressor",
      learning_rate = learning_rate,
      num_leaves = num_leaves,
      min_data_in_leaf = min_data_in_leaf,
      feature_fraction = feature_fraction,
      bagging_fraction = bagging_fraction,
      bagging_freq = bagging_freq,
      nrounds = nrounds,
      validation_RMSE_raw = metrics$RMSE_raw,
      validation_MAE_raw = metrics$MAE_raw,
      validation_R2_raw = metrics$R2_raw,
      validation_Correlation_raw = metrics$Correlation_raw,
      validation_score = city_validation_score(metrics)
    )
  }) %>%
    arrange(validation_score, validation_RMSE_raw)
}

fit_final_city_lgbm_regressor <- function(train_data, pred_data, features, best_params) {
  fit_city_lgb_regressor(
    train_data = train_data,
    pred_data = pred_data,
    features = features,
    params = best_params
  )
}

evaluate_city_regressor <- function(model_name, horizon, base_df, pred_cases) {
  pred_cases <- pmax(pred_cases, 0)
  pred_log <- city_cases_to_loginc(pred_cases, base_df$target_population)

  eval_df <- base_df %>%
    mutate(
      actual_cases = target_cases,
      pred_cases = pred_cases,
      actual_log_incidence = target_log_incidence,
      pred_log_incidence = pred_log
    )

  threshold_cases_vec <- eval_df$city_outbreak_threshold_cases

  raw_reg <- tibble(
    Model = model_name,
    forecast_horizon = horizon,
    forecast_horizon_label = paste0("H+", horizon),
    RMSE_raw = safe_rmse(eval_df$actual_cases, eval_df$pred_cases),
    MAE_raw = safe_mae(eval_df$actual_cases, eval_df$pred_cases),
    R2_raw = safe_r2(eval_df$actual_cases, eval_df$pred_cases),
    Correlation_raw = safe_cor(eval_df$actual_cases, eval_df$pred_cases),
    Bias_raw = mean(eval_df$pred_cases - eval_df$actual_cases, na.rm = TRUE),
    Mean_pred_raw = mean(eval_df$pred_cases, na.rm = TRUE),
    Mean_actual_raw = mean(eval_df$actual_cases, na.rm = TRUE),
    RMSE_log_incidence = safe_rmse(eval_df$actual_log_incidence, eval_df$pred_log_incidence),
    MAE_log_incidence = safe_mae(eval_df$actual_log_incidence, eval_df$pred_log_incidence),
    R2_log_incidence = safe_r2(eval_df$actual_log_incidence, eval_df$pred_log_incidence),
    Correlation_log_incidence = safe_cor(eval_df$actual_log_incidence, eval_df$pred_log_incidence)
  )

  cls_cases <- classification_metrics_from_cases_city(
    eval_df$actual_cases,
    eval_df$pred_cases,
    threshold_cases_vec
  )

  high_week_threshold <- quantile(eval_df$actual_cases, 0.75, na.rm = TRUE)
  high_week_df <- eval_df %>% filter(actual_cases >= high_week_threshold)

  high_week_metrics <- if (nrow(high_week_df) > 0) {
    tibble(
      High_week_RMSE_raw = safe_rmse(high_week_df$actual_cases, high_week_df$pred_cases),
      High_week_MAE_raw = safe_mae(high_week_df$actual_cases, high_week_df$pred_cases),
      High_week_Bias_raw = mean(high_week_df$pred_cases - high_week_df$actual_cases, na.rm = TRUE),
      High_week_Underprediction_rate = mean(high_week_df$pred_cases < high_week_df$actual_cases, na.rm = TRUE)
    )
  } else {
    tibble(
      High_week_RMSE_raw = NA_real_,
      High_week_MAE_raw = NA_real_,
      High_week_Bias_raw = NA_real_,
      High_week_Underprediction_rate = NA_real_
    )
  }

  bind_cols(raw_reg, cls_cases, high_week_metrics)
}


# ============================================================
# 8. LAG-COUNT TUNING AND RUN ONE HORIZON
# ============================================================

tune_lag_count_for_lgb <- function(horizon) {
  default_lgb_params <- tibble(
    learning_rate = 0.03,
    num_leaves = 15,
    min_data_in_leaf = 10,
    feature_fraction = 0.85,
    bagging_fraction = 0.85,
    bagging_freq = 1,
    nrounds = 500
  )

  purrr::map_df(LAG_KEEP_GRID, function(k) {
    feat_obj <- build_city_horizon_features(
      horizon = horizon,
      keep_per_var = k,
      lag_train_end_year = TRAIN_CORE_END_YEAR
    )

    h_df <- feat_obj$data
    features <- feat_obj$features

    train_core <- h_df %>% filter(target_year <= TRAIN_CORE_END_YEAR)
    validation <- h_df %>% filter(target_year %in% VALIDATION_YEARS)

    if (nrow(train_core) == 0 || nrow(validation) == 0 || length(features) == 0) {
      return(tibble(
        forecast_horizon = horizon,
        forecast_horizon_label = paste0("H+", horizon),
        model_key = "lgb",
        keep_per_var = k,
        score = NA_real_,
        RMSE_raw = NA_real_,
        MAE_raw = NA_real_,
        R2_raw = NA_real_,
        feature_count = length(features)
      ))
    }

    fit <- fit_final_city_lgbm_regressor(
      train_data = train_core,
      pred_data = validation,
      features = features,
      best_params = default_lgb_params
    )

    metrics <- evaluate_city_regressor(
      model_name = paste0("Validation LightGBM lag keep ", k),
      horizon = horizon,
      base_df = validation,
      pred_cases = fit$pred_cases
    )

    tibble(
      forecast_horizon = horizon,
      forecast_horizon_label = paste0("H+", horizon),
      model_key = "lgb",
      keep_per_var = k,
      score = city_validation_score(metrics),
      RMSE_raw = metrics$RMSE_raw,
      MAE_raw = metrics$MAE_raw,
      R2_raw = metrics$R2_raw,
      feature_count = length(features)
    )
  }) %>%
    arrange(score)
}

run_one_city_horizon <- function(horizon) {
  set.seed(SEED + horizon)
  horizon_label <- paste0("H+", horizon)

  cat("\n============================================================\n")
  cat("CITY-WEEK STANDARD PURE LIGHTGBM REGRESSOR: ", horizon_label, "\n", sep = "")
  cat("============================================================\n")

  lag_tuning_results <- tune_lag_count_for_lgb(horizon)

  selected_keep <- lag_tuning_results %>%
    filter(!is.na(score)) %>%
    slice(1) %>%
    pull(keep_per_var)

  if (length(selected_keep) == 0 || is.na(selected_keep)) {
    selected_keep <- 3
  }

  cat("
Selected keep_per_var for city standard LightGBM ", horizon_label, ": ", selected_keep, "
", sep = "")

  feat_obj <- build_city_horizon_features(
    horizon = horizon,
    keep_per_var = selected_keep,
    lag_train_end_year = TRAIN_CORE_END_YEAR
  )

  h_df <- feat_obj$data
  features <- feat_obj$features

  train_core <- h_df %>% filter(target_year <= TRAIN_CORE_END_YEAR)
  validation <- h_df %>% filter(target_year %in% VALIDATION_YEARS)

  if (nrow(train_core) == 0 || nrow(validation) == 0 || length(features) == 0) {
    stop(paste0("Validation split failed for city standard model ", horizon_label))
  }

  reg_tuning_results <- tune_city_lgbm_regressor(train_core, validation, features, horizon_label)
  best_reg_params <- reg_tuning_results %>% slice(1)

  cat("\nBest city standard LightGBM regressor params (original grid) for ", horizon_label, ":\n", sep = "")
  print(best_reg_params)

  final_feat_obj <- build_city_horizon_features(
    horizon = horizon,
    keep_per_var = selected_keep,
    lag_train_end_year = TRAIN_END_YEAR
  )

  final_df <- final_feat_obj$data
  final_features <- final_feat_obj$features

  train_final <- final_df %>% filter(target_year <= TRAIN_END_YEAR)
  test_final <- final_df %>% filter(target_year %in% TEST_YEARS)

  if (nrow(train_final) == 0 || nrow(test_final) == 0 || length(final_features) == 0) {
    stop(paste0("Final train/test split failed for city standard model ", horizon_label))
  }

  final_reg <- fit_final_city_lgbm_regressor(
    train_data = train_final,
    pred_data = test_final,
    features = final_features,
    best_params = best_reg_params
  )

  final_metrics <- evaluate_city_regressor(
    model_name = "City-week Pure LightGBM regressor",
    horizon = horizon,
    base_df = test_final,
    pred_cases = final_reg$pred_cases
  )

  final_predictions <- test_final %>%
    select(
      origin_year,
      origin_week,
      target_year,
      target_week,
      forecast_horizon,
      forecast_horizon_label,
      city_dengue_cases,
      city_incidence_10000,
      target_cases,
      target_incidence_10000,
      target_log_incidence,
      target_population,
      city_outbreak_threshold_cases,
      city_large_outbreak_threshold_cases
    ) %>%
    mutate(
      actual_cases = target_cases,
      actual_incidence_10000 = target_incidence_10000,
      actual_log_incidence = target_log_incidence,
      actual_outbreak = actual_cases >= city_outbreak_threshold_cases,
      pure_lightgbm_cases = final_reg$pred_cases,
      predicted_cases = final_reg$pred_cases,
      pure_lightgbm_log_incidence = final_reg$pred_log,
      predicted_log_incidence = final_reg$pred_log,
      predicted_outbreak_from_cases = as.integer(predicted_cases >= city_outbreak_threshold_cases),
      alert_level = make_alert_level_from_cases(predicted_cases, city_outbreak_threshold_cases),
      error = predicted_cases - actual_cases,
      absolute_error = abs(error),
      display_in_app_default = forecast_horizon %in% DISPLAY_HORIZONS,
      model_type = "City-week Pure LightGBM regressor",
      uses_autocorrelation_features = TRUE,
      includes_citywide_environmental_features = TRUE
    )

  by_year <- final_predictions %>%
    group_by(target_year) %>%
    group_split() %>%
    purrr::map_df(function(d) {
      eval_base <- d %>%
        mutate(
          target_population = target_population,
          target_cases = actual_cases,
          target_log_incidence = actual_log_incidence,
          city_outbreak_threshold_cases = city_outbreak_threshold_cases
        )

      evaluate_city_regressor(
        model_name = paste0("City-week Pure LightGBM H+", horizon, " - target year ", unique(d$target_year)),
        horizon = horizon,
        base_df = eval_base,
        pred_cases = d$predicted_cases
      ) %>%
        mutate(target_year = unique(d$target_year), .after = forecast_horizon_label)
    })

  reg_importance <- tryCatch(
    {
      lightgbm::lgb.importance(model = final_reg$model) %>%
        as_tibble() %>%
        mutate(
          forecast_horizon = horizon,
          forecast_horizon_label = horizon_label,
          Component = "City-week Pure LightGBM regressor"
        )
    },
    error = function(e) {
      tibble(
        Feature = character(),
        Gain = numeric(),
        Cover = numeric(),
        Frequency = numeric(),
        forecast_horizon = integer(),
        forecast_horizon_label = character(),
        Component = character()
      )
    }
  )

  selected_settings <- tibble(
    forecast_horizon = horizon,
    forecast_horizon_label = horizon_label,
    final_feature_count = length(final_features),
    selected_keep_per_var = selected_keep,
    reg_learning_rate = best_reg_params$learning_rate,
    reg_num_leaves = best_reg_params$num_leaves,
    reg_min_data_in_leaf = best_reg_params$min_data_in_leaf,
    reg_feature_fraction = best_reg_params$feature_fraction,
    reg_bagging_fraction = best_reg_params$bagging_fraction,
    reg_bagging_freq = best_reg_params$bagging_freq,
    reg_nrounds = final_reg$nrounds
  )

  booster_path <- file.path(MODEL_DIR, paste0("h", horizon, "_regressor.txt"))
  lightgbm::lgb.save(final_reg$model, booster_path)

  reg_bundle <- list(
    model_path = booster_path,
    model_family = "standard_city_lightgbm",
    model_part = "regressor",
    horizon = horizon,
    horizon_label = horizon_label,
    feature_cols = final_features,
    matrix_feature_names = final_reg$feature_names,
    best_params = as.data.frame(best_reg_params),
    target = "target_log_incidence",
    uses_autocorrelation_features = TRUE,
    trained_on_years = paste0("<=", TRAIN_END_YEAR)
  )
  saveRDS(reg_bundle, file.path(MODEL_DIR, paste0("h", horizon, "_regressor.rds")))
  saveRDS(
    list(
      feature_cols = final_features,
      matrix_feature_names = final_reg$feature_names,
      horizon = horizon,
      horizon_label = horizon_label,
      uses_autocorrelation_features = TRUE
    ),
    file.path(MODEL_DIR, paste0("h", horizon, "_feature_spec.rds"))
  )

  write_csv(lag_tuning_results, file.path(OUTPUT_DIR, paste0("city_week_pure_lgb_lag_tuning_H", horizon, "_", OUTPUT_PREFIX, ".csv")))
  write_csv(reg_tuning_results, file.path(OUTPUT_DIR, paste0("city_week_pure_lgb_regressor_tuning_H", horizon, "_", OUTPUT_PREFIX, ".csv")))
  write_csv(final_feat_obj$lag_report, file.path(OUTPUT_DIR, paste0("city_week_pure_lgb_selected_lag_report_H", horizon, "_", OUTPUT_PREFIX, ".csv")))
  write_csv(tibble(feature = final_features), file.path(OUTPUT_DIR, paste0("city_week_pure_lgb_final_feature_list_H", horizon, "_", OUTPUT_PREFIX, ".csv")))
  write_csv(final_predictions, file.path(OUTPUT_DIR, paste0("city_week_pure_lgb_predictions_H", horizon, "_", OUTPUT_PREFIX, ".csv")))
  write_csv(final_metrics, file.path(OUTPUT_DIR, paste0("city_week_pure_lgb_metrics_H", horizon, "_", OUTPUT_PREFIX, ".csv")))

  cat("\nFinal city standard LightGBM ", horizon_label, " metrics:\n", sep = "")
  print(as.data.frame(final_metrics))

  list(
    metrics = final_metrics,
    predictions = final_predictions,
    by_year = by_year,
    settings = selected_settings,
    importance = reg_importance
  )
}


# ============================================================
# 9. RUN H+0 TO H+12
# ============================================================

city_horizon_results <- purrr::map(FORECAST_HORIZONS, run_one_city_horizon)

city_horizon_metrics <- purrr::map_df(city_horizon_results, "metrics") %>% arrange(forecast_horizon)
city_horizon_predictions <- purrr::map_df(city_horizon_results, "predictions") %>% arrange(forecast_horizon, target_year, target_week)
city_horizon_by_year <- purrr::map_df(city_horizon_results, "by_year") %>% arrange(forecast_horizon, target_year)
city_horizon_settings <- purrr::map_df(city_horizon_results, "settings") %>% arrange(forecast_horizon)
city_horizon_importance <- purrr::map_df(city_horizon_results, "importance") %>% arrange(forecast_horizon, desc(Gain))


# ============================================================
# 10. SAVE FINAL OUTPUTS
# ============================================================

write_csv(city_horizon_metrics, file.path(OUTPUT_DIR, paste0("city_week_pure_lightgbm_H0_TO_H12_metrics_", OUTPUT_PREFIX, ".csv")))
write_csv(city_horizon_predictions, file.path(OUTPUT_DIR, paste0("city_week_pure_lightgbm_H0_TO_H12_predictions_", OUTPUT_PREFIX, ".csv")))
write_csv(city_horizon_by_year, file.path(OUTPUT_DIR, paste0("city_week_pure_lightgbm_H0_TO_H12_by_year_", OUTPUT_PREFIX, ".csv")))
write_csv(city_horizon_settings, file.path(OUTPUT_DIR, paste0("city_week_pure_lightgbm_H0_TO_H12_selected_settings_", OUTPUT_PREFIX, ".csv")))
write_csv(city_horizon_importance, file.path(OUTPUT_DIR, paste0("city_week_pure_lightgbm_H0_TO_H12_feature_importance_", OUTPUT_PREFIX, ".csv")))

city_horizon_summary <- city_horizon_metrics %>%
  select(
    forecast_horizon,
    forecast_horizon_label,
    RMSE_raw,
    MAE_raw,
    R2_raw,
    Correlation_raw,
    Bias_raw,
    Mean_pred_raw,
    Mean_actual_raw,
    RMSE_log_incidence,
    MAE_log_incidence,
    R2_log_incidence,
    Correlation_log_incidence,
    Precision,
    Recall,
    Outbreak_F1_count_threshold,
    False_alarm_rate,
    Specificity,
    High_week_RMSE_raw,
    High_week_MAE_raw,
    High_week_Bias_raw,
    High_week_Underprediction_rate
  ) %>%
  arrange(forecast_horizon)

write_csv(city_horizon_summary, file.path(OUTPUT_DIR, paste0("city_week_pure_lightgbm_H0_TO_H12_compact_summary_", OUTPUT_PREFIX, ".csv")))

write_csv(city_horizon_metrics, file.path(METADATA_DIR, "standard_city_metrics.csv"))
write_csv(city_horizon_summary, file.path(METADATA_DIR, "standard_city_compact_summary.csv"))
write_csv(city_horizon_settings, file.path(METADATA_DIR, "standard_city_selected_settings.csv"))
write_csv(city_horizon_importance, file.path(METADATA_DIR, "standard_city_feature_importance.csv"))

cat("\n============================================================\n")
cat("CITY-WEEK PURE LIGHTGBM H+0 TO H+12 SUMMARY\n")
cat("============================================================\n")
print(as.data.frame(city_horizon_summary))

cat("\n============================================================\n")
cat("SELECTED SETTINGS BY HORIZON\n")
cat("============================================================\n")
print(as.data.frame(city_horizon_settings))

cat("\n============================================================\n")
cat("DONE: CITY-WEEK PURE LIGHTGBM H+0 TO H+12 COMPLETED\n")
cat("============================================================\n")

cat("\nApp interpretation:\n")
cat("Use predicted_cases or pure_lightgbm_cases as the final citywide standard case forecast.\n")
cat("This model intentionally has no classifier-derived outbreak_probability because it is a pure standard regressor.\n")
cat("Use alert_level as a case-threshold-derived alert category.\n")
cat("Use display_in_app_default to show selected week, +1, +2, +3, +4, +8, and +12 in the app.\n")
